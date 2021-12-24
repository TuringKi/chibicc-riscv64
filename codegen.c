#include "chibicc.h"

static FILE *output_file;
static int depth;
static char *argreg[] = {"a0", "a1", "a2", "a3", "a4", "a5"};
static Obj *cur_fn;

static void gen_expr();
static void gen_stmt();

static void println(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
  va_end(ap);
  fprintf(output_file, "\n");
}

static int count(void) {
  static int i = 1;
  return i++;
}

static void push(void) {
  println("\t\taddi sp,sp,-8");
  println("\t\tsd s1, 0(sp)");
  depth++;
}

static void pop(char *arg) {
  println("\t\tld %s, 0(sp)", arg);
  println("\t\taddi sp,sp,8");
  depth--;
}

static void load(Node *node) {
  Type *ty = node->ty;
  if (ty->kind == TY_ARRAY) {
    return;
  }
  if (!node->var) {
    if (ty->size == 1) {
      println("\t\tlb s1, 0(s1)");
      return;
    }
    println("\t\tld s1, 0(s1)");
    return;
  }
  if (node->var->is_local) {
    if (ty->size == 1) {
      println("\t\tlb s1, 0(s1)");
      return;
    }
    println("\t\tld s1, 0(s1)");
  } else {
    if (ty->size == 1) {
      println("\t\tlb s1, %%lo(%s)(s1)", node->var->name);
      return;
    }
    println("\t\tld s1, %%lo(%s)(s1)", node->var->name);
  }
}

static void store(Node *node) {
  Type *ty = node->ty;
  pop("t0");
  if (!node->var) {
    if (ty->size == 1) {
      println("\t\tsb s1, 0(t0)");
      return;
    }
    println("\t\tsd s1, 0(t0)");
    return;
  }
  if (node->var->is_local) {
    if (ty->size == 1) {
      println("\t\tsb s1, 0(t0)");
      return;
    }
    println("\t\tsd s1, 0(t0)");
  } else {
    if (ty->size == 1) {
      println("\t\tsb s1, %%lo(%s)(t0)", node->var->name);
      return;
    }
    println("\t\tsd s1, %%lo(%s)(t0)", node->var->name);
  }
}

static int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

static void gen_addr(Node *node) {
  switch (node->kind) {
  case ND_VAR:
    if (node->var->is_local) {
      println("\t\taddi s1, fp, %d", node->var->offset);
      return;
    } else {
      println("\t\tlui s1, %%hi(%s)", node->var->name);
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
    println("\t\tsub s1, zero, s1");
    return;
  case ND_NUM:
    println("\t\taddi s1, zero, %d", node->val);
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
  case ND_STMT_EXPR:
    for (Node *n = node->body; n; n = n->next) {
      gen_stmt(n);
    }
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

    // println("\t\taddi s1, zero, 0");
    println("\t\tcall %s", node->funcname);
    println("\t\tadd s1, zero, a0");
    return;
  }
  }

  gen_expr(node->rhs);
  push();
  gen_expr(node->lhs);
  pop("t0");

  switch (node->kind) {
  case ND_EQ:
    println("\t\tsub t1, t0, s1");
    println("\t\tsltiu s1, t1, 1");
    return;
  case ND_NE:
    println("\t\tsub t1, t0, s1");
    println("\t\tsltu s1, zero, t1");
    return;
  case ND_LE:
    println("\t\tsub t1, s1, t0");
    println("\t\tslt s1, zero, t1");
    println("\t\txori s1, s1, 1");
    println("\t\tandi s1, s1, 0xff");
    return;
  case ND_LT:
    println("\t\tsub t1, t0, s1");
    println("\t\tslt s1, zero, t1");
    return;

  case ND_ADD:
    println("\t\tadd s1, s1, t0");
    return;
  case ND_SUB:
    println("\t\tsub s1, s1, t0");
    return;
  case ND_MUL:
    println("\t\tadd t1, zero, s1");
    println("\t\tmulh s1, t1, t0");
    println("\t\tmul s1, t1, t0");
    return;
  case ND_DIV:
    println("\t\tdiv s1, s1, t0");
    return;
  }

  error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node *node) {
  switch (node->kind) {
  case ND_IF: {
    int c = count();
    gen_expr(node->cond);
    println("\t\tbeq zero, s1, .L.else.%d", c);
    gen_stmt(node->then);
    println("\t\tjal zero, .L.end.%d", c);
    println(".L.else.%d:", c);
    if (node->els) {
      gen_stmt(node->els);
    }
    println(".L.end.%d:", c);
    return;
  }

  case ND_FOR: {
    int c = count();
    if (node->init) {
      gen_stmt(node->init);
    }
    println(".L.begin.%d:", c);
    if (node->cond) {
      gen_expr(node->cond);
      println("\t\tbeq zero, s1, .L.end.%d", c);
    }
    gen_stmt(node->then);
    if (node->inc) {
      gen_expr(node->inc);
    }
    println("\t\tjal zero, .L.begin.%d", c);
    println(".L.end.%d:", c);
    return;
  }

  case ND_BLOCK:
    for (Node *n = node->body; n; n = n->next) {
      gen_stmt(n);
    }
    return;
  case ND_RETURN:
    gen_expr(node->rhs);
    println("\t\tjal zero, .L.return.%s", cur_fn->name);
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

    println(".data");
    println(".globl %s", var->name);
    println("%s:", var->name);

    if (var->init_data) {
      for (int i = 0; i < var->ty->size; i++) {
        println("\t\t.byte %d", var->init_data[i]);
      }
    } else {
      println("\t\t.zero %d", var->ty->size);
    }
  }
}

void emit_text(Obj *prog) {

  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function) {
      continue;
    }
    println(".globl %s", fn->name);
    println(".text");
    println("%s:", fn->name);
    cur_fn = fn;

    println("\t\tsd ra, -8(sp)");
    println("\t\tsd fp, -16(sp)");
    println("\t\taddi fp, sp, 0");
    println("\t\taddi sp, sp, -%d", fn->stack_size);

    int i = 0;
    for (Obj *var = fn->params; var; var = var->next) {
      if (var->ty->size == 1) {
        println("\t\tsb %s, %d(fp)", argreg[i++], (var->offset));
      } else {
        println("\t\tsd %s, %d(fp)", argreg[i++], (var->offset));
      }
    }

    gen_stmt(fn->body);
    assert(depth == 0);

    println(".L.return.%s:", fn->name);
    println("\t\tadd a0, zero, s1");
    println("\t\taddi sp, sp, %d", fn->stack_size);
    println("\t\tld ra, -8(sp)");
    println("\t\tld fp, -16(sp)");
    println("\t\tjr ra");
  }
}

void codegen(Obj *prog, FILE *out) {
  output_file = out;
  assign_lvar_offsets(prog);
  emit_data(prog);
  emit_text(prog);
}