#include "chibicc.h"

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

static void gen_addr(Node *node) {
  if (node->kind = ND_VAR) {
    int offset = (node->name - 'a' + 1) * 8;
    printf("\t\taddi a0, t2, -%d\n", offset);
    return;
  }

  error("not an lvalue");
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
  case ND_VAR:
    gen_addr(node);
    printf("\t\tld a0, 0(a0)\n");
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    push();
    gen_expr(node->rhs);
    pop("t0");
    printf("\t\tsd a0, 0(t0)\n");
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

static void gen_stmt(Node *node) {
  if (node->kind == ND_EXPR_STMT) {
    gen_expr(node->rhs);
    return;
  }

  error("invalid statement");
}

void codegen(Node *node) {

  printf("\t.globl main\n");
  printf("main:\n");

  printf("\t\tsd t2, 0(sp)\n");
  printf("\t\taddi t2, sp, 0\n");
  printf("\t\taddi sp, sp, -208\n");

  for (Node *n = node; n; n = n->next) {
    gen_stmt(n);
    assert(depth == 0);
  }
  printf("\t\taddi sp, sp, 208\n");
  printf("\t\tld t2, 0(sp)\n");

  printf("\t\tret\n");
}