#include "chibicc.h"

static int depth;

static int count(void) {
  static int i = 1;
  return i++;
}

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

static int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

static void gen_addr(Node *node) {
  if (node->kind = ND_VAR) {
    printf("\t\taddi a0, t2, -%d\n", node->var->offset);
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
  switch (node->kind) {
  case ND_IF: {
    int c = count();
    gen_expr(node->cond);
    printf("\t\tbeq zero, a0, .L.else.%d\n", c);
    gen_stmt(node->then);
    printf("\t\tjal zero, .L.end.%d\n", c);
    printf(".L.else.%d:\n", c);
    if (node->els) {
      gen_stmt(node->els);
    }
    printf(".L.end.%d:\n", c);
    return;
  }

  case ND_FOR: {
    int c = count();
    gen_stmt(node->init);
    printf(".L.begin.%d:\n", c);
    if (node->cond) {
      gen_expr(node->cond);
      printf("\t\tbeq zero, a0, .L.end.%d\n", c);
    }
    gen_stmt(node->then);
    if (node->inc) {
      gen_expr(node->inc);
    }
    printf("\t\tjal zero, .L.begin.%d\n", c);
    printf(".L.end.%d:\n", c);
    return;
  }

  case ND_BLOCK:
    for (Node *n = node->body; n; n = n->next) {
      gen_stmt(n);
    }
    return;
  case ND_RETURN:
    gen_expr(node->rhs);
    printf("\t\tjal zero, .L.return\n");
    return;
  case ND_EXPR_STMT:
    gen_expr(node->rhs);
    return;
  }

  error("invalid statement");
}

static void assign_lvar_offsets(Function *prog) {
  int offset = 0;
  for (Obj *var = prog->locals; var; var = var->next) {
    offset += 8;
    var->offset = -offset;
  }
  prog->stack_size = align_to(offset, 16);
}

void codegen(Function *prog) {

  assign_lvar_offsets(prog);

  printf("\t.globl main\n");
  printf("main:\n");

  printf("\t\tsd t2, 0(sp)\n");
  printf("\t\taddi t2, sp, 0\n");
  printf("\t\taddi sp, sp, -%d\n", prog->stack_size);

  gen_stmt(prog->body);

  printf(".L.return:\n");
  printf("\t\taddi sp, sp, %d\n", prog->stack_size);
  printf("\t\tld t2, 0(sp)\n");

  printf("\t\tret\n");
}