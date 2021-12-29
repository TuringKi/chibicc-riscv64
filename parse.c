#include "chibicc.h"

typedef struct VarScope VarScope;
struct VarScope {
  VarScope *next;
  char *name;
  Obj *var;
  Type *type_def;
};

typedef struct TagScope TagScope;
struct TagScope {
  TagScope *next;
  char *name;
  Type *ty;
};

typedef struct Scope Scope;
struct Scope {
  Scope *next;
  VarScope *vars;
  TagScope *tags;
};

typedef struct {
  bool is_typedef;
} VarAttr;

static Obj *current_fn;
static Obj *locals;
static Obj *globals;
static Scope *scope = &(Scope){};
static bool is_typename(Token *tok);
static Type *declspec(Token **rest, Token *tok, VarAttr *attr);
static Type *declarator(Token **rest, Token *tok, Type *ty);
static Node *declaration(Token **rest, Token *tok, Type *basety);
static Node *block_stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *assign(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *relational(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Type *struct_decl(Token **rest, Token *tok);
static Type *union_decl(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *cast(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);
static Node *postfix(Token **rest, Token *tok);
static Token *parse_typedef(Token *tok, Type *basety);

static Node *new_node(NodeKind kind, Token *tok) {
  Node *node = calloc(1, sizeof(Node));
  node->kind = kind;
  node->tok = tok;
  return node;
}

static void enter_scope(void) {
  Scope *sc = calloc(1, sizeof(Scope));
  sc->next = scope;
  scope = sc;
}

static void leave_scope(void) { scope = scope->next; }

static VarScope *find_var(Token *tok) {
  for (Scope *scp = scope; scp; scp = scp->next) {
    for (VarScope *var = scp->vars; var; var = var->next) {
      if (equal(tok, var->name)) {
        return var;
      }
    }
  }

  return NULL;
}

static Type *find_tag(Token *tok) {
  for (Scope *sc = scope; sc; sc = sc->next)
    for (TagScope *sc2 = sc->tags; sc2; sc2 = sc2->next)
      if (equal(tok, sc2->name)) {
        return sc2->ty;
      }
  return NULL;
}

static Node *new_variable(Obj *var, Token *tok) {
  Node *node = new_node(ND_VAR, tok);
  node->var = var;
  return node;
}

static VarScope *push_scope(char *name) {
  VarScope *sc = calloc(1, sizeof(VarScope));
  sc->name = name;
  sc->next = scope->vars;
  scope->vars = sc;
  return sc;
}

static Node *new_long(int64_t val, Token *tok) {
  Node *node = new_node(ND_NUM, tok);
  node->val = val;
  node->ty = ty_long;
  return node;
}

static Obj *new_var(char *name, Type *ty) {
  Obj *var = calloc(1, sizeof(Obj));
  var->name = name;
  var->ty = ty;
  push_scope(name)->var = var;
  return var;
}

static Obj *new_lvar(char *name, Type *ty) {
  Obj *var = new_var(name, ty);
  var->is_local = true;
  var->next = locals;
  locals = var;
  return var;
}

Node *new_cast(Node *expr, Type *ty) {
  add_type(expr);

  Node *node = calloc(1, sizeof(Node));
  node->kind = ND_CAST;
  node->tok = expr->tok;
  node->rhs = expr;
  node->ty = copy_type(ty);
  return node;
}

static Obj *new_gvar(char *name, Type *ty) {
  Obj *var = new_var(name, ty);
  var->next = globals;
  globals = var;
  return var;
}

static Type *find_typedef(Token *tok) {
  if (tok->kind == TK_IDENT) {
    VarScope *sc = find_var(tok);
    if (sc) {
      return sc->type_def;
    }
  }
  return NULL;
}

static char *new_unique_name() {
  static int id = 0;
  return format(".L..%d", id++);
}

static Obj *new_anon_gvar(Type *ty) { return new_gvar(new_unique_name(), ty); }

static Obj *new_string_literal(char *p, Type *ty) {
  Obj *var = new_anon_gvar(ty);
  var->init_data = p;
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

static void push_tag_scope(Token *tok, Type *ty) {
  TagScope *sc = calloc(1, sizeof(TagScope));
  sc->name = strndup(tok->loc, tok->len);
  sc->ty = ty;
  sc->next = scope->tags;
  scope->tags = sc;
}

static Type *declspec(Token **rest, Token *tok, VarAttr *attr) {
  enum {
    VOID = 1 << 0,
    CHAR = 1 << 2,
    SHORT = 1 << 4,
    INT = 1 << 6,
    LONG = 1 << 8,
    OTHER = 1 << 10,
  };

  Type *ty = ty_int;
  int counter = 0;

  while (is_typename(tok)) {

    if (equal(tok, "typedef")) {
      if (!attr) {
        error_tok(tok,
                  "storage class specifier is not allowed in this context");
      }

      attr->is_typedef = true;
      tok = tok->next;
      continue;
    }

    Type *ty2 = find_typedef(tok);

    if (equal(tok, "struct") || equal(tok, "union") || ty2) {
      if (counter) {
        break;
      }
      if (equal(tok, "struct")) {
        ty = struct_decl(&tok, tok->next);
      } else if (equal(tok, "union")) {
        ty = union_decl(&tok, tok->next);
      } else {
        ty = ty2;
        tok = tok->next;
      }

      counter += OTHER;
      continue;
    }

    // Handle built-in types.
    if (equal(tok, "void"))
      counter += VOID;
    else if (equal(tok, "char"))
      counter += CHAR;
    else if (equal(tok, "short"))
      counter += SHORT;
    else if (equal(tok, "int"))
      counter += INT;
    else if (equal(tok, "long"))
      counter += LONG;
    else
      unreachable();

    switch (counter) {
    case VOID:
      ty = ty_void;
      break;
    case CHAR:
      ty = ty_char;
      break;
    case SHORT:
    case SHORT + INT:
      ty = ty_short;
      break;
    case INT:
      ty = ty_int;
      break;
    case LONG:
    case LONG + INT:
    case LONG + LONG:
    case LONG + LONG + INT:
      ty = ty_long;
      break;
    default:
      error_tok(tok, "invalid type");
    }

    tok = tok->next;
  }

  *rest = tok;
  return ty;
}

static Type *func_param(Token **rest, Token *tok, Type *ty) {
  Type head = {};
  Type *cur = &head;

  while (!equal(tok, ")")) {
    if (cur != &head) {
      tok = skip(tok, ",");
    }
    Type *basety = declspec(&tok, tok, NULL);
    Type *ty = declarator(&tok, tok, basety);
    cur = cur->next = copy_type(ty);
  }

  ty = func_type(ty);
  ty->params = head.next;
  *rest = tok->next;
  return ty;
}

static long get_number(Token *tok) {
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

  if (equal(tok, "(")) {
    Token *start = tok;
    Type dummy = {};
    declarator(&tok, start->next, &dummy);
    tok = skip(tok, ")");
    ty = type_suffix(rest, tok, ty);
    return declarator(&tok, start->next, ty);
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

static Node *new_num(int64_t val, Token *tok) {
  Node *node = new_node(ND_NUM, tok);
  node->val = val;
  return node;
}

static bool is_typename(Token *tok) {
  static char *kw[] = {
      "void", "char", "short", "int", "long", "struct", "union", "typedef",
  };

  for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++) {
    if (equal(tok, kw[i])) {
      return true;
    }
  }

  return find_typedef(tok);
}

static Node *stmt(Token **rest, Token *tok) {
  if (equal(tok, "return")) {
    Node *node = new_node(ND_RETURN, tok);
    Node *exp = expr(&tok, tok->next);
    *rest = skip(tok, ";");

    add_type(exp);
    node->rhs = new_cast(exp, current_fn->ty->return_ty);
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

static Type *abstract_declarator(Token **rest, Token *tok, Type *ty) {
  while (equal(tok, "*")) {
    ty = pointer_to(ty);
    tok = tok->next;
  }

  if (equal(tok, "(")) {
    Token *start = tok;
    Type dummy = {};
    abstract_declarator(&tok, start->next, &dummy);
    tok = skip(tok, ")");
    ty = type_suffix(rest, tok, ty);
    return abstract_declarator(&tok, start->next, ty);
  }

  return type_suffix(rest, tok, ty);
}

// type-name = declspec abstract-declarator
static Type *typename(Token **rest, Token *tok) {
  Type *ty = declspec(&tok, tok, NULL);
  return abstract_declarator(rest, tok, ty);
}

static Node *declaration(Token **rest, Token *tok, Type *basety) {
  Node head = {};
  Node *cur = &head;
  int i = 0;

  while (!equal(tok, ";")) {
    if (i++ > 0) {
      tok = skip(tok, ",");
    }

    Type *ty = declarator(&tok, tok, basety); // variable or variable of pointer
    if (ty->kind == TY_VOID) {
      error_tok(tok, "variable declared void");
    }
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

  enter_scope();

  while (!equal(tok, "}")) {
    if (is_typename(tok)) {
      VarAttr attr = {};
      Type *basety = declspec(&tok, tok, &attr);

      if (attr.is_typedef) {
        tok = parse_typedef(tok, basety);
        continue;
      }

      cur = cur->next = declaration(&tok, tok, basety);
    } else {
      cur = cur->next = stmt(&tok, tok);
    }
    add_type(cur);
  }

  leave_scope();

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

static Node *expr(Token **rest, Token *tok) {
  Node *node = assign(&tok, tok);

  if (equal(tok, ",")) {
    return new_binary(ND_COMMA, node, expr(rest, tok->next), tok);
  }

  *rest = tok;
  return node;
}

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

  rhs = new_binary(ND_MUL, rhs, new_long(lhs->ty->base->size, tok), tok);
  return new_binary(ND_ADD, lhs, rhs, tok);
}

static Node *new_sub(Node *lhs, Node *rhs, Token *tok) {
  add_type(lhs);
  add_type(rhs);

  if (is_integer(lhs->ty) && is_integer(rhs->ty)) {
    return new_binary(ND_SUB, lhs, rhs, tok);
  }

  if (lhs->ty->base && is_integer(rhs->ty)) {
    rhs = new_binary(ND_MUL, rhs, new_long(lhs->ty->base->size, tok), tok);
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

  Node *node = cast(&tok, tok);

  for (;;) {
    Token *start = tok;
    if (equal(tok, "*")) {
      node = new_binary(ND_MUL, node, cast(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "/")) {
      node = new_binary(ND_DIV, node, cast(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

static Node *cast(Token **rest, Token *tok) {
  if (equal(tok, "(") && is_typename(tok->next)) {
    Token *start = tok;
    Type *ty = typename(&tok, tok->next);
    tok = skip(tok, ")");
    Node *node = new_cast(cast(rest, tok), ty);
    node->tok = start;
    return node;
  }

  return unary(rest, tok);
}

static void struct_members(Token **rest, Token *tok, Type *ty) {
  Member head = {};
  Member *cur = &head;

  while (!equal(tok, "}")) {
    Type *basety = declspec(&tok, tok, NULL);
    int i = 0;

    while (!consume(&tok, tok, ";")) {
      if (i++)
        tok = skip(tok, ",");

      Member *mem = calloc(1, sizeof(Member));
      mem->ty = declarator(&tok, tok, basety);
      mem->name = mem->ty->name;
      cur = cur->next = mem;
    }
  }

  *rest = tok->next;
  ty->members = head.next;
}

static Type *struct_union_decl(Token **rest, Token *tok) {
  Token *tag = NULL;
  if (tok->kind == TK_IDENT) {
    tag = tok;
    tok = tok->next;
  }

  if (tag && !equal(tok, "{")) {
    Type *ty = find_tag(tag);
    if (!ty)
      error_tok(tag, "unknown struct type");
    *rest = tok;
    return ty;
  }

  // Construct a struct object.
  Type *ty = calloc(1, sizeof(Type));
  ty->kind = TY_STRUCT;
  struct_members(rest, tok->next, ty);
  ty->align = 1;

  if (tag) {
    push_tag_scope(tag, ty);
  }
  return ty;
}

static Type *struct_decl(Token **rest, Token *tok) {
  Type *ty = struct_union_decl(rest, tok);
  ty->kind = TY_STRUCT;
  // Assign offsets within the struct to members.
  int offset = 0;
  for (Member *mem = ty->members; mem; mem = mem->next) {
    offset = align_to(offset, mem->ty->align);
    mem->offset = offset;
    offset += mem->ty->size;
    if (ty->align < mem->ty->align) {
      ty->align = mem->ty->align;
    }
  }
  ty->size = align_to(offset, ty->align);

  return ty;
}

// union-decl = struct-union-decl
static Type *union_decl(Token **rest, Token *tok) {
  Type *ty = struct_union_decl(rest, tok);
  ty->kind = TY_UNION;

  // If union, we don't have to assign offsets because they
  // are already initialized to zero. We need to compute the
  // alignment and the size though.
  for (Member *mem = ty->members; mem; mem = mem->next) {
    if (ty->align < mem->ty->align) {
      ty->align = mem->ty->align;
    }
    if (ty->size < mem->ty->size) {
      ty->size = mem->ty->size;
    }
  }
  ty->size = align_to(ty->size, ty->align);
  return ty;
}

static Member *get_struct_member(Type *ty, Token *tok) {
  for (Member *mem = ty->members; mem; mem = mem->next)
    if (mem->name->len == tok->len &&
        !strncmp(mem->name->loc, tok->loc, tok->len))
      return mem;
  error_tok(tok, "no such member");
}

static Node *struct_ref(Node *lhs, Token *tok) {
  add_type(lhs);
  if (lhs->ty->kind != TY_STRUCT && lhs->ty->kind != TY_UNION) {
    error_tok(lhs->tok, "not a struct nor a union");
  }

  Node *node = new_unary(ND_MEMBER, lhs, tok);
  node->member = get_struct_member(lhs->ty, tok);
  return node;
}

static Node *postfix(Token **rest, Token *tok) {

  Node *node = primary(&tok, tok);

  for (;;) {
    if (equal(tok, "[")) {
      Token *start = tok;
      Node *idx = expr(&tok, tok->next);
      tok = skip(tok, "]");
      node = new_unary(ND_DEREF, new_add(node, idx, start), start);
      continue;
    }
    if (equal(tok, ".")) {
      node = struct_ref(node, tok->next);
      tok = tok->next->next;
      continue;
    }
    if (equal(tok, "->")) {
      // x->y is short for (*x).y
      node = new_unary(ND_DEREF, node, tok);
      node = struct_ref(node, tok->next);
      tok = tok->next->next;
      continue;
    }

    *rest = tok;
    return node;
  }
}

static Node *unary(Token **rest, Token *tok) {

  if (equal(tok, "+")) {
    return cast(rest, tok->next);
  }

  if (equal(tok, "-")) {
    return new_unary(ND_NEG, cast(rest, tok->next), tok);
  }

  if (equal(tok, "&")) {
    return new_unary(ND_ADDR, cast(rest, tok->next), tok);
  }

  if (equal(tok, "*")) {
    return new_unary(ND_DEREF, cast(rest, tok->next), tok);
  }

  return postfix(rest, tok);
}

static Node *funccall(Token **rest, Token *tok) {
  Token *start = tok;
  tok = tok->next->next;

  VarScope *sc = find_var(start);
  if (!sc) {
    error_tok(start, "implicit declaration of a function");
  }
  if (!sc->var || sc->var->ty->kind != TY_FUNC) {
    error_tok(start, "not a function");
  }

  Type *ty = sc->var->ty->return_ty;

  Node head = {};
  Node *cur = &head;

  while (!equal(tok, ")")) {
    if (cur != &head) {
      tok = skip(tok, ",");
    }
    cur = cur->next = assign(&tok, tok);
    add_type(cur);
  }

  *rest = skip(tok, ")");

  Node *node = new_node(ND_FUNCCAL, start);
  node->funcname = strndup(start->loc, start->len);
  node->ty = ty;
  node->args = head.next;
  return node;
}

static Node *primary(Token **rest, Token *tok) {
  Token *start = tok;

  if (equal(tok, "(") && equal(tok->next, "{")) {
    Node *node = new_node(ND_STMT_EXPR, tok);
    node->body = block_stmt(&tok, tok->next->next)->body;
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "(")) {

    Node *node = expr(&tok, tok->next);
    *rest = skip(tok, ")");
    return node;
  }
  if (equal(tok, "sizeof") && equal(tok->next, "(") &&
      is_typename(tok->next->next)) {
    Type *ty = typename(&tok, tok->next->next);
    *rest = skip(tok, ")");
    return new_num(ty->size, start);
  }

  if (equal(tok, "sizeof")) {
    Node *node = unary(rest, tok->next);
    add_type(node);
    return new_num(node->ty->size, tok);
  }

  if (tok->kind == TK_STR) {
    Obj *var = new_string_literal(tok->str, tok->ty);
    *rest = tok->next;
    return new_variable(var, tok);
  }

  if (tok->kind == TK_IDENT) {

    // Funcation call
    if (equal(tok->next, "(")) {
      return funccall(rest, tok);
    }

    // Variable
    VarScope *sc = find_var(tok);
    if (!sc || !sc->var) {
      error_tok(tok, "undefined variable");
    }
    Node *node = new_variable(sc->var, tok);
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

static Token *parse_typedef(Token *tok, Type *basety) {
  bool first = true;

  while (!consume(&tok, tok, ";")) {
    if (!first) {
      tok = skip(tok, ",");
    }
    first = false;

    Type *ty = declarator(&tok, tok, basety);
    push_scope(get_ident(ty->name))->type_def = ty;
  }
  return tok;
}

static Token *function(Token *tok, Type *basety) {
  Type *ty = declarator(&tok, tok, basety);

  locals = NULL;

  Obj *fn = new_gvar(get_ident(ty->name), ty);
  fn->is_function = true;
  fn->is_definition = !consume(&tok, tok, ";");

  if (!fn->is_definition) {
    return tok;
  }
  current_fn = fn;
  enter_scope();
  create_param_lvars(ty->params);
  fn->params = locals;

  tok = skip(tok, "{");
  fn->body = block_stmt(&tok, tok);
  fn->locals = locals;
  leave_scope();
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
    VarAttr attr = {};
    Type *basety = declspec(&tok, tok, &attr);

    // Typedef
    if (attr.is_typedef) {
      tok = parse_typedef(tok, basety);
      continue;
    }
    if (is_function(tok)) {
      tok = function(tok, basety);
      continue;
    }

    tok = global_variables(tok, basety);
  }

  return globals;
}