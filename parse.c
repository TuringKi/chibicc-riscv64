#include "chibicc.h"

static Obj *locals;
static Obj *globals;
static Type *declarator(Token **rest, Token *tok, Type *ty);
static Node *declaration(Token **rest, Token *tok);
static Node *block_stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *assign(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *relational(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);
static Node *postfix(Token **rest, Token *tok);

static Node *new_node(NodeKind kind, Token *tok) {
  Node *node = calloc(1, sizeof(Node));
  node->kind = kind;
  node->tok = tok;
  return node;
}

static Obj *find_var(Token *tok) {
  for (Obj *var = locals; var; var = var->next) {
    if (strlen(var->name) == tok->len &&
        !strncmp(tok->loc, var->name, tok->len)) {
      return var;
    }
  }

  for (Obj *var = globals; var; var = var->next) {
    if (strlen(var->name) == tok->len &&
        !strncmp(tok->loc, var->name, tok->len)) {
      return var;
    }
  }

  return NULL;
}

static Node *new_variable(Obj *var, Token *tok) {
  Node *node = new_node(ND_VAR, tok);
  node->var = var;
  return node;
}

static Obj *new_var(char *name, Type *ty) {
  Obj *var = calloc(1, sizeof(Obj));
  var->name = name;
  var->ty = ty;
  return var;
}

static Obj *new_lvar(char *name, Type *ty) {
  Obj *var = new_var(name, ty);
  var->is_local = true;
  var->next = locals;
  locals = var;
  return var;
}

static Obj *new_gvar(char *name, Type *ty) {
  Obj *var = new_var(name, ty);
  var->next = globals;
  globals = var;
  return var;
}

static char *get_ident(Token *tok) {
  if (tok->kind != TK_IDENT) {
    error_tok(tok, "expected an identifier");
  }
  return strndup(tok->loc, tok->len);
}

static void create_param_lvars(Type *param) {
  if (param) {
    create_param_lvars(param->next);
    new_lvar(get_ident(param->name), param);
  }
}

static Type *declspec(Token **rest, Token *tok) {
  *rest = skip(tok, "int");
  return ty_int;
}
static Type *func_param(Token **rest, Token *tok, Type *ty) {
  Type head = {};
  Type *cur = &head;

  while (!equal(tok, ")")) {
    if (cur != &head) {
      tok = skip(tok, ",");
    }
    Type *basety = declspec(&tok, tok);
    Type *ty = declarator(&tok, tok, basety);
    cur = cur->next = copy_type(ty);
  }

  ty = func_type(ty);
  ty->params = head.next;
  *rest = tok->next;
  return ty;
}

static int get_number(Token *tok) {
  if (tok->kind != TK_NUM) {
    error_tok(tok, "expected a number");
  }
  return tok->val;
}

static Type *type_suffix(Token **rest, Token *tok, Type *ty) {
  if (equal(tok, "(")) {
    return func_param(rest, tok->next, ty);
  }

  if (equal(tok, "[")) {
    int sz = get_number(tok->next);
    tok = skip(tok->next->next, "]");
    ty = type_suffix(rest, tok, ty);
    return array_of(ty, sz);
  }

  *rest = tok;
  return ty;
}

static Type *declarator(Token **rest, Token *tok, Type *ty) {
  while (consume(&tok, tok, "*")) {
    ty = pointer_to(ty);
  }

  if (tok->kind != TK_IDENT) {
    error_tok(tok, "expected a variable name");
  }
  ty = type_suffix(rest, tok->next, ty);
  ty->name = tok;
  return ty;
}

static Node *new_unary(NodeKind kind, Node *expr, Token *tok) {
  Node *node = new_node(kind, tok);
  node->rhs = expr;
  return node;
}

static Node *new_binary(NodeKind kind, Node *lhs, Node *rhs, Token *tok) {

  Node *node = new_node(kind, tok);
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

static Node *new_num(int val, Token *tok) {
  Node *node = new_node(ND_NUM, tok);
  node->val = val;
  return node;
}

static Node *stmt(Token **rest, Token *tok) {
  if (equal(tok, "return")) {
    Node *node = new_node(ND_RETURN, tok);
    node->rhs = expr(&tok, tok->next);
    *rest = skip(tok, ";");
    return node;
  }

  if (equal(tok, "if")) {
    Node *node = new_node(ND_IF, tok);
    tok = skip(tok->next, "(");
    node->cond = expr(&tok, tok);
    tok = skip(tok, ")");
    node->then = stmt(&tok, tok);
    if (equal(tok, "else")) {
      node->els = stmt(&tok, tok->next);
    }
    *rest = tok;
    return node;
  }

  if (equal(tok, "for")) {
    Node *node = new_node(ND_FOR, tok);
    tok = skip(tok->next, "(");

    node->init = expr_stmt(&tok, tok);

    if (!equal(tok, ";")) {
      node->cond = expr(&tok, tok);
    }

    tok = skip(tok, ";");

    if (!equal(tok, ")")) {
      node->inc = expr(&tok, tok);
    }
    tok = skip(tok, ")");

    node->then = stmt(rest, tok);
    return node;
  }

  if (equal(tok, "while")) {
    Node *node = new_node(ND_FOR, tok);
    tok = skip(tok->next, "(");
    node->cond = expr(&tok, tok);
    tok = skip(tok, ")");
    node->then = stmt(rest, tok);
    return node;
  }

  if (equal(tok, "{")) {
    return block_stmt(rest, tok->next);
  }

  return expr_stmt(rest, tok);
}

static Node *declaration(Token **rest, Token *tok) {
  Type *basety = declspec(&tok, tok);

  Node head = {};
  Node *cur = &head;
  int i = 0;

  while (!equal(tok, ";")) {
    if (i++ > 0) {
      tok = skip(tok, ",");
    }

    Type *ty = declarator(&tok, tok, basety); // variable or variable of pointer
    Obj *var = new_lvar(get_ident(ty->name), ty);

    if (!equal(tok, "=")) {
      continue;
    }

    Node *lhs = new_variable(var, ty->name);
    Node *rhs = assign(&tok, tok->next);
    Node *node = new_binary(ND_ASSIGN, lhs, rhs, tok);
    cur = cur->next = new_unary(ND_EXPR_STMT, node, tok);
  }

  Node *node = new_node(ND_BLOCK, tok);
  node->body = head.next;
  *rest = tok->next;
  return node;
}

static Node *block_stmt(Token **rest, Token *tok) {
  Node *node = new_node(ND_BLOCK, tok);
  Node head = {};
  Node *cur = &head;

  while (!equal(tok, "}")) {
    if (equal(tok, "int")) {
      cur = cur->next = declaration(&tok, tok);
    } else {
      cur = cur->next = stmt(&tok, tok);
    }

    add_type(cur);
  }

  node->body = head.next;
  *rest = tok->next;
  return node;
}

// stmt = expr-stmt
static Node *expr_stmt(Token **rest, Token *tok) {
  if (equal(tok, ";")) {
    *rest = tok->next;
    return new_node(ND_BLOCK, tok);
  }
  Node *node = new_node(ND_EXPR_STMT, tok);
  node->rhs = expr(&tok, tok);
  *rest = skip(tok, ";");
  return node;
}

static Node *expr(Token **rest, Token *tok) { return assign(rest, tok); }

static Node *assign(Token **rest, Token *tok) {
  Node *node = equality(&tok, tok);

  if (equal(tok, "=")) {
    return new_binary(ND_ASSIGN, node, assign(rest, tok->next), tok);
  }
  *rest = tok;
  return node;
}

static Node *equality(Token **rest, Token *tok) {

  Node *node = relational(&tok, tok);

  for (;;) {
    Token *start = tok;
    if (equal(tok, "==")) {
      node = new_binary(ND_EQ, node, relational(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "!=")) {
      node = new_binary(ND_NE, node, relational(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

static Node *relational(Token **rest, Token *tok) {

  Node *node = add(&tok, tok);

  for (;;) {
    Token *start = tok;
    if (equal(tok, "<")) {
      node = new_binary(ND_LT, node, add(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "<=")) {
      node = new_binary(ND_LE, node, add(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, ">")) {
      node = new_binary(ND_LT, add(&tok, tok->next), node, start);
      continue;
    }

    if (equal(tok, ">=")) {
      node = new_binary(ND_LE, add(&tok, tok->next), node, start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

static Node *new_add(Node *lhs, Node *rhs, Token *tok) {
  add_type(lhs);
  add_type(rhs);

  if (is_integer(lhs->ty) && is_integer(rhs->ty)) {
    return new_binary(ND_ADD, lhs, rhs, tok);
  }

  if (lhs->ty->base && rhs->ty->base) {
    error_tok(tok, "invalid operands");
  }

  if (!lhs->ty->base && rhs->ty->base) {
    Node *tmp = lhs;
    lhs = rhs;
    rhs = tmp;
  }

  rhs = new_binary(ND_MUL, rhs, new_num(lhs->ty->base->size, tok), tok);
  return new_binary(ND_ADD, lhs, rhs, tok);
}

static Node *new_sub(Node *lhs, Node *rhs, Token *tok) {
  add_type(lhs);
  add_type(rhs);

  if (is_integer(lhs->ty) && is_integer(rhs->ty)) {
    return new_binary(ND_SUB, lhs, rhs, tok);
  }

  if (lhs->ty->base && is_integer(rhs->ty)) {
    rhs = new_binary(ND_MUL, rhs, new_num(lhs->ty->base->size, tok), tok);
    add_type(rhs);
    Node *node = new_binary(ND_SUB, lhs, rhs, tok);
    node->ty = lhs->ty;
    return node;
  }

  if (lhs->ty->base && rhs->ty->base) {
    Node *node = new_binary(ND_SUB, lhs, rhs, tok);
    node->ty = ty_int;
    return new_binary(ND_DIV, node, new_num(lhs->ty->base->size, tok), tok);
  }

  error_tok(tok, "invalid operands");
}

static Node *add(Token **rest, Token *tok) {

  Node *node = mul(&tok, tok);

  for (;;) {
    Token *start = tok;
    if (equal(tok, "+")) {
      node = new_add(node, mul(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "-")) {
      node = new_sub(node, mul(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

static Node *mul(Token **rest, Token *tok) {

  Node *node = unary(&tok, tok);

  for (;;) {
    Token *start = tok;
    if (equal(tok, "*")) {
      node = new_binary(ND_MUL, node, unary(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "/")) {
      node = new_binary(ND_DIV, node, unary(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

static Node *postfix(Token **rest, Token *tok) {

  Node *node = primary(&tok, tok);

  while (equal(tok, "[")) {
    Token *start = tok;
    Node *idx = expr(&tok, tok->next);
    tok = skip(tok, "]");
    node = new_unary(ND_DEREF, new_add(node, idx, start), start);
  }

  *rest = tok;
  return node;
}

static Node *unary(Token **rest, Token *tok) {

  if (equal(tok, "+")) {
    return unary(rest, tok->next);
  }

  if (equal(tok, "-")) {
    return new_unary(ND_NEG, unary(rest, tok->next), tok);
  }

  if (equal(tok, "&")) {
    return new_unary(ND_ADDR, unary(rest, tok->next), tok);
  }

  if (equal(tok, "*")) {
    return new_unary(ND_DEREF, unary(rest, tok->next), tok);
  }

  return postfix(rest, tok);
}

static Node *funccall(Token **rest, Token *tok) {
  Token *start = tok;
  tok = tok->next->next;

  Node head = {};
  Node *cur = &head;

  while (!equal(tok, ")")) {
    if (cur != &head) {
      tok = skip(tok, ",");
    }
    cur = cur->next = assign(&tok, tok);
  }

  *rest = skip(tok, ")");

  Node *node = new_node(ND_FUNCCAL, start);
  node->funcname = strndup(start->loc, start->len);
  node->args = head.next;
  return node;
}

static Node *primary(Token **rest, Token *tok) {
  if (equal(tok, "(")) {

    Node *node = expr(&tok, tok->next);
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "sizeof")) {
    Node *node = unary(rest, tok->next);
    add_type(node);
    return new_num(node->ty->size, tok);
  }

  if (tok->kind == TK_IDENT) {

    // Funcation call
    if (equal(tok->next, "(")) {
      return funccall(rest, tok);
    }

    // Variable
    Obj *var = find_var(tok);
    if (!var) {
      error_tok(tok, "undefined variable");
    }
    Node *node = new_variable(var, tok);
    *rest = tok->next;
    return node;
  }

  if (tok->kind == TK_NUM) {
    Node *node = new_num(tok->val, tok);
    *rest = tok->next;
    return node;
  }

  error_tok(tok, "expected an expression");
}

static Token *function(Token *tok, Type *basety) {
  Type *ty = declarator(&tok, tok, basety);

  locals = NULL;

  Obj *fn = new_gvar(get_ident(ty->name), ty);
  fn->is_function = true;

  create_param_lvars(ty->params);
  fn->params = locals;

  tok = skip(tok, "{");
  fn->body = block_stmt(&tok, tok);
  fn->locals = locals;
  return tok;
}

static Token *global_variables(Token *tok, Type *basety) {
  bool first = true;
  while (!consume(&tok, tok, ";")) {
    if (!first) {
      tok = skip(tok, ",");
    }
    first = false;
    Type *ty = declarator(&tok, tok, basety);
    new_gvar(get_ident(ty->name), ty);
  }
  return tok;
}

static bool is_function(Token *tok) {

  if (equal(tok, ";")) {
    return false;
  }

  Type dummy = {};
  Type *ty = declarator(&tok, tok, &dummy);
  return ty->kind == TY_FUNC;
}

Obj *parse(Token *tok) {
  globals = NULL;

  while (tok->kind != TK_EOF) {
    Type *basety = declspec(&tok, tok);
    if (is_function(tok)) {
      tok = function(tok, basety);
      continue;
    }

    tok = global_variables(tok, basety);
  }

  return globals;
}