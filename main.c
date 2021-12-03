#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
  TK_PUNCT,
  TK_NUM,
  TK_EOF,
} TokenKind;

typedef struct Token Token;
struct Token {
  TokenKind kind;
  Token *next;
  int val;
  char *loc;
  int len;
};

static char *current_input;

static void error(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(-1);
}

static void verror_at(char *loc, char *fmt, va_list ap) {
  int pos = loc - current_input;
  if (pos < 0) {
    error("fatal error");
  }
  fprintf(stderr, "%s\n", current_input);
  fprintf(stderr, "%*s", pos, "");
  fprintf(stderr, "^ ");
  fprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(-1);
}

static void error_at(char *loc, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(loc, fmt, ap);
}

static void error_tok(Token *tok, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(tok->loc, fmt, ap);
}

static bool equal(Token *tok, char *op) {
  return memcmp(tok->loc, op, tok->len) == 0 && op[tok->len] == '\0';
}

static Token *skip(Token *tok, char *s) {
  if (!equal(tok, s)) {
    error_at(tok->loc, "expected '%s'", s);
  }
  return tok->next;
}

static int get_number(Token *tok) {
  if (tok->kind != TK_NUM) {
    error_tok(tok, "expected a number");
  }
  return tok->val;
}

static bool startwith(char *p, char *q) {
  return strncmp(p, q, strlen(q)) == 0;
}

static int read_punct(char *p) {
  if (startwith(p, "==") || startwith(p, ">=") || startwith(p, "!=") ||
      startwith(p, "<=")) {
    return 2;
  }

  return ispunct(*p) ? 1 : 0;
}

static Token *new_token(TokenKind kind, char *start, char *end) {
  Token *tok = calloc(1, sizeof(Token));
  if (tok == NULL) {
    error("creating token faild");
  }
  tok->kind = kind;
  tok->loc = start;
  tok->len = end - start;
  return tok;
}

// tokenize 函数将表达式解析为多个token。token的存储结构为链表。
static Token *tokenize() {
  char *p = current_input;
  Token head = {};
  Token *cur = &head;

  while (*p) {
    if (isspace(*p)) {
      p++;
      continue;
    }
    if (isdigit(*p)) {
      //注意下方的表达式要从右往左读，即先将返回的结果存储到next,然后再覆盖当前链表元素的指针值。
      cur = cur->next = new_token(TK_NUM, p, p);
      char *q = p;
      cur->val = strtol(p, &p, 10);
      cur->len = p - q;
      continue;
    }

    int punck_len = read_punct(p);
    if (punck_len) {
      cur = cur->next = new_token(TK_PUNCT, p, p + punck_len);
      p += cur->len;
      continue;
    }

    error_at(p, "invalid token");
  }

  cur = cur->next = new_token(TK_EOF, p, p);
  return head.next;
}

typedef enum {
  ND_ADD,
  ND_SUB,
  ND_MUL,
  ND_DIV,
  ND_NEG,
  ND_EQ,
  ND_NE,
  ND_LT,
  ND_LE,
  ND_NUM,
} NodeKind;

typedef struct Node Node;
struct Node {
  NodeKind kind;
  Node *lhs;
  Node *rhs;
  int val;
};

static Node *new_node(NodeKind kind) {
  Node *node = calloc(1, sizeof(Node));
  node->kind = kind;
  return node;
}

static Node *new_unary(NodeKind kind, Node *expr) {
  Node *node = new_node(kind);
  node->rhs = expr;
  return node;
}

static Node *new_binary(NodeKind kind, Node *lhs, Node *rhs) {

  Node *node = new_node(kind);
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

static Node *new_num(int val) {
  Node *node = new_node(ND_NUM);
  node->val = val;
  return node;
}

static Node *expr(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *relational(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);

static Node *expr(Token **rest, Token *tok) { return equality(rest, tok); }

static Node *equality(Token **rest, Token *tok) {

  Node *node = relational(&tok, tok);

  for (;;) {
    if (equal(tok, "==")) {
      node = new_binary(ND_EQ, node, relational(&tok, tok->next));
      continue;
    }

    if (equal(tok, "!=")) {
      node = new_binary(ND_NE, node, relational(&tok, tok->next));
      continue;
    }

    *rest = tok;
    return node;
  }
}

static Node *relational(Token **rest, Token *tok) {

  Node *node = add(&tok, tok);

  for (;;) {

    if (equal(tok, "<")) {
      node = new_binary(ND_LT, node, add(&tok, tok->next));
      continue;
    }

    if (equal(tok, "<=")) {
      node = new_binary(ND_LE, node, add(&tok, tok->next));
      continue;
    }

    if (equal(tok, ">")) {
      node = new_binary(ND_LT, add(&tok, tok->next), node);
      continue;
    }

    if (equal(tok, ">=")) {
      node = new_binary(ND_LE, add(&tok, tok->next), node);
      continue;
    }

    *rest = tok;
    return node;
  }
}

static Node *add(Token **rest, Token *tok) {

  Node *node = mul(&tok, tok);

  for (;;) {

    if (equal(tok, "+")) {
      node = new_binary(ND_ADD, node, mul(&tok, tok->next));
      continue;
    }

    if (equal(tok, "-")) {
      node = new_binary(ND_SUB, node, mul(&tok, tok->next));
      continue;
    }

    *rest = tok;
    return node;
  }
}

static Node *mul(Token **rest, Token *tok) {

  Node *node = unary(&tok, tok);

  for (;;) {

    if (equal(tok, "*")) {
      node = new_binary(ND_MUL, node, unary(&tok, tok->next));
      continue;
    }

    if (equal(tok, "/")) {
      node = new_binary(ND_DIV, node, unary(&tok, tok->next));
      continue;
    }

    *rest = tok;
    return node;
  }
}

static Node *unary(Token **rest, Token *tok) {

  if (equal(tok, "+")) {
    return unary(rest, tok->next);
  }

  if (equal(tok, "-")) {
    return new_unary(ND_NEG, unary(rest, tok->next));
  }

  return primary(rest, tok);
}

static Node *primary(Token **rest, Token *tok) {
  if (equal(tok, "(")) {

    Node *node = expr(&tok, tok->next);
    *rest = skip(tok, ")");
    return node;
  }

  if (tok->kind == TK_NUM) {
    Node *node = new_num(tok->val);
    *rest = tok->next;
    return node;
  }

  error_tok(tok, "expected an expression");
}

static int depth;

static void push(void) {
  printf("\t\taddi sp,sp,-8\n");
  printf("\t\tsd a0, 0(sp)\n");
  depth++;
}

static void pop(char *arg) {
  printf("\t\tld %s, 0(sp)\n", arg);
  printf("\t\taddi sp,sp,8\n");
  depth--;
}

static void gen_expr(Node *node) {

  switch (node->kind) {
  case ND_NEG:
    gen_expr(node->rhs);
    printf("\t\tsub a0, zero, a0\n");
    return;
  case ND_NUM:
    printf("\t\taddi a0, zero, %d\n", node->val);
    return;
  }

  gen_expr(node->rhs);
  push();
  gen_expr(node->lhs);
  pop("t0");

  switch (node->kind) {
  case ND_EQ:
    printf("\t\tsub t1, t0, a0\n");
    printf("\t\tsltiu a0, t1, 1\n");
    return;
  case ND_NE:
    printf("\t\tsub t1, t0, a0\n");
    printf("\t\tsltu a0, zero, t1\n");
    return;
  case ND_LE:
    printf("\t\tsub t1, a0, t0\n");
    printf("\t\tslt a0, zero, t1\n");
    printf("\t\txori a0, a0, 1\n");
    printf("\t\tandi a0, a0, 0xff\n");
    return;
  case ND_LT:
    printf("\t\tsub t1, t0, a0\n");
    printf("\t\tslt a0, zero, t1\n");
    return;

  case ND_ADD:
    printf("\t\tadd a0, a0, t0\n");
    return;
  case ND_SUB:
    printf("\t\tsub a0, a0, t0\n");
    return;
  case ND_MUL:
    printf("\t\tadd t1, zero, a0\n");
    printf("\t\tmulh a0, t1, t0\n");
    printf("\t\tmul a0, t1, t0\n");
    return;
  case ND_DIV:
    printf("\t\tdiv a0, a0, t0\n");
    return;
  }

  error("invalid expression");
}

int main(int argc, char **argv) {
  // stderr 是标准错误文件，它负责收集错误信息。一个经常使用到的方式为：./a.out
  // 2>error.log 或者 ./a.out 2>/dev/null 其中 2
  // 是标准错误输出的文件描述标号。前者将错误信息输出到error.log文件，后者将错误信息输出到
  // /dev/null，这是 linux 操作系统的空设备，输出到此设备意味着什么也不输出。
  if (argc != 2) {
    fprintf(stderr, "%s, invalid number of arguments\n", argv[0]);
  }
  current_input = argv[1];
  Token *tok = tokenize();
  Node *node = expr(&tok, tok);

  if (tok->kind != TK_EOF) {
    error_tok(tok, "extra token");
  }

  printf("\t.globl main\n");
  printf("main:\n");

  gen_expr(node);

  printf("\t\tret\n");

  assert(depth == 0);
  return 0;
}