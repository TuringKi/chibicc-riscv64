#include "chibicc.h"

static int depth;
static char *argreg[] = {"a0", "a1", "a2", "a3", "a4", "a5"};
static Obj *cur_fn;

static void gen_expr();

static int count(void) {
  static int i = 1;
  return i++;
}

static void push(void) {
  printf("\t\taddi sp,sp,-8\n");
  printf("\t\tsd s1, 0(sp)\n");
  depth++;
}

static void pop(char *arg) {
  printf("\t\tld %s, 0(sp)\n", arg);
  printf("\t\taddi sp,sp,8\n");
  depth--;
}

static void load(Node *node) {
  Type *ty = node->ty;
  if (ty->kind == TY_ARRAY) {
    return;
  }
  if (!node->var) {
    if (ty->size == 1) {
      printf("\t\tlb s1, 0(s1)\n");
      return;
    }
    printf("\t\tld s1, 0(s1)\n");
    return;
  }
  if (node->var->is_local) {
    if (ty->size == 1) {
      printf("\t\tlb s1, 0(s1)\n");
      return;
    }
    printf("\t\tld s1, 0(s1)\n");
  } else {
    if (ty->size == 1) {
      printf("\t\tlb s1, %%lo(%s)(s1)\n", node->var->name);
      return;
    }
    printf("\t\tld s1, %%lo(%s)(s1)\n", node->var->name);
  }
}

static void store(Node *node) {
  Type *ty = node->ty;
  pop("t0");
  if (!node->var) {
    if (ty->size == 1) {
      printf("\t\tsb s1, 0(t0)\n");
      return;
    }
    printf("\t\tsd s1, 0(t0)\n");
    return;
  }
  if (node->var->is_local) {
    if (ty->size == 1) {
      printf("\t\tsb s1, 0(t0)\n");
      return;
    }
    printf("\t\tsd s1, 0(t0)\n");
  } else {
    if (ty->size == 1) {
      printf("\t\tsb s1, %%lo(%s)(t0)\n", node->var->name);
      return;
    }
    printf("\t\tsd s1, %%lo(%s)(t0)\n", node->var->name);
  }
}

static int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

static void gen_addr(Node *node) {
  switch (node->kind) {
  case ND_VAR:
    if (node->var->is_local) {
      printf("\t\taddi s1, fp, %d\n", node->var->offset);
      return;
    } else {
      printf("\t\tlui s1, %%hi(%s)\n", node->var->name);
      return;
    }

  case ND_DEREF:
    gen_expr(node->rhs);
    return;
  }

  error_tok(node->tok, "not an lvalue");
}

static void gen_expr(Node *node) {

  switch (node->kind) {
  case ND_NEG:
    gen_expr(node->rhs);
    printf("\t\tsub s1, zero, s1\n");
    return;
  case ND_NUM:
    printf("\t\taddi s1, zero, %d\n", node->val);
    return;
  case ND_DEREF:
    gen_expr(node->rhs);
    load(node);
    return;
  case ND_ADDR:
    gen_addr(node->rhs);
    return;
  case ND_VAR:
    gen_addr(node);
    load(node);
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    push();
    gen_expr(node->rhs);
    store(node->lhs);
    return;
  case ND_FUNCCAL: {
    int nargs = 0;

    for (Node *arg = node->args; arg; arg = arg->next) {
      gen_expr(arg);
      push();
      nargs++;
    }

    for (int i = nargs - 1; i >= 0; i--) {
      pop(argreg[i]);
    }

    // printf("\t\taddi s1, zero, 0\n");
    printf("\t\tcall %s\n", node->funcname);
    printf("\t\tadd s1, zero, a0\n");
    return;
  }
  }

  gen_expr(node->rhs);
  push();
  gen_expr(node->lhs);
  pop("t0");

  switch (node->kind) {
  case ND_EQ:
    printf("\t\tsub t1, t0, s1\n");
    printf("\t\tsltiu s1, t1, 1\n");
    return;
  case ND_NE:
    printf("\t\tsub t1, t0, s1\n");
    printf("\t\tsltu s1, zero, t1\n");
    return;
  case ND_LE:
    printf("\t\tsub t1, s1, t0\n");
    printf("\t\tslt s1, zero, t1\n");
    printf("\t\txori s1, s1, 1\n");
    printf("\t\tandi s1, s1, 0xff\n");
    return;
  case ND_LT:
    printf("\t\tsub t1, t0, s1\n");
    printf("\t\tslt s1, zero, t1\n");
    return;

  case ND_ADD:
    printf("\t\tadd s1, s1, t0\n");
    return;
  case ND_SUB:
    printf("\t\tsub s1, s1, t0\n");
    return;
  case ND_MUL:
    printf("\t\tadd t1, zero, s1\n");
    printf("\t\tmulh s1, t1, t0\n");
    printf("\t\tmul s1, t1, t0\n");
    return;
  case ND_DIV:
    printf("\t\tdiv s1, s1, t0\n");
    return;
  }

  error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node *node) {
  switch (node->kind) {
  case ND_IF: {
    int c = count();
    gen_expr(node->cond);
    printf("\t\tbeq zero, s1, .L.else.%d\n", c);
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
    if (node->init) {
      gen_stmt(node->init);
    }
    printf(".L.begin.%d:\n", c);
    if (node->cond) {
      gen_expr(node->cond);
      printf("\t\tbeq zero, s1, .L.end.%d\n", c);
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
    printf("\t\tjal zero, .L.return.%s\n", cur_fn->name);
    return;
  case ND_EXPR_STMT:
    gen_expr(node->rhs);
    return;
  }

  error_tok(node->tok, "invalid statement");
}

static void assign_lvar_offsets(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function) {
      continue;
    }
    int offset = 16; // 0~16:ra, fp
    for (Obj *var = fn->locals; var; var = var->next) {
      offset += var->ty->size;
      var->offset = -offset;
    }
    fn->stack_size = align_to(offset, 16);
  }
}

static void emit_data(Obj *prog) {

  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function) {
      continue;
    }

    printf(".data\n");
    printf(".globl %s\n", var->name);
    printf("%s:\n", var->name);

    if (var->init_data) {
      for (int i = 0; i < var->ty->size; i++) {
        printf("\t\t.byte %d\n", var->init_data[i]);
      }
    } else {
      printf("\t\t.zero %d\n", var->ty->size);
    }
  }
}

void emit_text(Obj *prog) {

  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function) {
      continue;
    }
    printf(".globl %s\n", fn->name);
    printf(".text\n");
    printf("%s:\n", fn->name);
    cur_fn = fn;

    printf("\t\tsd ra, -8(sp)\n");
    printf("\t\tsd fp, -16(sp)\n");
    printf("\t\taddi fp, sp, 0\n");
    printf("\t\taddi sp, sp, -%d\n", fn->stack_size);

    int i = 0;
    for (Obj *var = fn->params; var; var = var->next) {
      if (var->ty->size == 1) {
        printf("\t\tsb %s, %d(fp)\n", argreg[i++], (var->offset));
      } else {
        printf("\t\tsd %s, %d(fp)\n", argreg[i++], (var->offset));
      }
    }

    gen_stmt(fn->body);
    assert(depth == 0);

    printf(".L.return.%s:\n", fn->name);
    printf("\t\tadd a0, zero, s1\n");
    printf("\t\taddi sp, sp, %d\n", fn->stack_size);
    printf("\t\tld ra, -8(sp)\n");
    printf("\t\tld fp, -16(sp)\n");
    printf("\t\tjr ra\n");
  }
}

void codegen(Obj *prog) {
  assign_lvar_offsets(prog);
  emit_data(prog);
  emit_text(prog);
}