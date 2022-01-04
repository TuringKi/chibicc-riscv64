#include "chibicc.h"

static FILE *output_file;
static int depth;
static char *argreg[] = {"a0", "a1", "a2", "a3", "a4", "a5"};
static Obj *cur_fn;

static void gen_expr();
static void gen_stmt();
static void gen_addr(Node *node);

static void println(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
  va_end(ap);
  fprintf(output_file, "\n");
}

enum { I8, I16, I32, I64 };

static int getTypeId(Type *ty) {
  switch (ty->kind) {
  case TY_CHAR:
    return I8;
  case TY_SHORT:
    return I16;
  case TY_INT:
    return I32;
  }
  return I64;
}

static void cmp_zero(Type *ty) { println("\t\tsnez s1, s1"); }

static char i32i8[] = "\t\tlb s1, 0(%s)";
static char i32i16[] = "\t\tlh s1, 0(%s)";
static char i32i64[] = "\t\tld s1, 0(%s)";
static char *cast_table[][10] = {
    {NULL, NULL, NULL, i32i64},    // i8
    {i32i8, NULL, NULL, i32i64},   // i16
    {i32i8, i32i16, NULL, i32i64}, // i32
    {i32i8, i32i16, NULL, NULL},   // i64
};
static void cast(Type *from, Type *to, char *r) {
  if (to->kind == TY_VOID) {
    return;
  }
  if (to->kind == TY_BOOL) {
    cmp_zero(from);
    return;
  }
  int t1 = getTypeId(from);
  int t2 = getTypeId(to);
  if (cast_table[t1][t2]) {
    println(cast_table[t1][t2], r);
  }
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
  if (ty->kind == TY_ARRAY || ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    return;
  }
  if (!node->var) {
    if (ty->size == 1) {
      println("\t\tlb s1, 0(s1)");
      return;
    } else if (ty->size == 2) {
      println("\t\tlh s1, 0(s1)");
      return;
    } else if (ty->size == 4) {
      println("\t\tlw s1, 0(s1)");
      return;
    }
    println("\t\tld s1, 0(s1)");
    return;
  }
  if (node->var->is_local) {
    if (ty->size == 1) {
      println("\t\tlb s1, 0(s1)");
      return;
    } else if (ty->size == 2) {
      println("\t\tlh s1, 0(s1)");
      return;
    } else if (ty->size == 4) {
      println("\t\tlw s1, 0(s1)");
      return;
    }
    println("\t\tld s1, 0(s1)");
  } else {
    if (ty->size == 1) {
      println("\t\tlb s1, %%lo(%s)(s1)", node->var->name);
      return;
    } else if (ty->size == 2) {
      println("\t\tlh s1, %%lo(%s)(s1)", node->var->name);
      return;
    } else if (ty->size == 4) {
      println("\t\tlw s1, %%lo(%s)(s1)", node->var->name);
      return;
    }
    println("\t\tld s1, %%lo(%s)(s1)", node->var->name);
  }
}

static void store(Node *lhs, Node *rhs) {
  Type *ty = lhs->ty;
  pop("t0");

  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    println("\t\tadd a7, zero, s1");
    for (Member *member = ty->members; member; member = member->next) {
      println("\t\taddi t2, t0, %d", member->offset);
      if (member->ty->size == 1) {
        println("\t\tlb s1, %d(a7)", member->offset);
        println("\t\tsb s1, 0(t2)");
      } else if (member->ty->size == 2) {
        println("\t\tlh s1, %d(a7)", member->offset);
        println("\t\tsh s1, 0(t2)");
      } else if (member->ty->size == 4) {
        println("\t\tlw s1, %d(a7)", member->offset);
        println("\t\tsw s1, 0(t2)");
      } else {
        println("\t\tld s1, %d(a7)", member->offset);
        println("\t\tsd s1, 0(t2)");
      }
    }
    println("\t\tadd s1, zero, a7");
    return;
  }

  if (!lhs->var) {
    if (ty->size == 1) {
      println("\t\tsb s1, 0(t0)");
      return;
    } else if (ty->size == 2) {
      println("\t\tsh s1, 0(t0)");
      return;
    } else if (ty->size == 4) {
      println("\t\tsw s1, 0(t0)");
      return;
    }
    println("\t\tsd s1, 0(t0)");
    return;
  }
  if (lhs->var->is_local) {
    if (ty->size == 1) {
      println("\t\tsb s1, 0(t0)");
      return;
    } else if (ty->size == 2) {
      println("\t\tsh s1, 0(t0)");
      return;
    } else if (ty->size == 4) {
      println("\t\tsw s1, 0(t0)");
      return;
    }
    println("\t\tsd s1, 0(t0)");
  } else {
    if (ty->size == 1) {
      println("\t\tsb s1, %%lo(%s)(t0)", lhs->var->name);
      return;
    } else if (ty->size == 2) {
      println("\t\tsh s1, %%lo(%s)(t0)", lhs->var->name);
      return;
    } else if (ty->size == 4) {
      println("\t\tsw s1, %%lo(%s)(t0)", lhs->var->name);
      return;
    }
    println("\t\tsd s1, %%lo(%s)(t0)", lhs->var->name);
  }
}

int align_to(int n, int align) { return (n + align - 1) / align * align; }

static void gen_addr(Node *node) {
  switch (node->kind) {
  case ND_VAR:
    if (node->var->is_local) {
      println("\t\taddi s1, fp, %d", node->var->offset);
      return;
    } else {
      println("\t\tlui s1, %%hi(%s)", node->var->name);
      println("\t\taddi s1, s1, %%lo(%s)", node->var->name);
      return;
    }
  case ND_MEMBER:
    gen_addr(node->rhs);
    println("\t\taddi s1, s1, %d", node->member->offset);
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_addr(node->rhs);
    return;
  case ND_DEREF:
    gen_expr(node->rhs);
    return;
  }

  error_tok(node->tok, "not an lvalue");
}

static void gen_expr(Node *node) {
  println("\t\t.loc 1 %d", node->tok->line_no);

  switch (node->kind) {
  case ND_NEG:
    gen_expr(node->rhs);
    println("\t\tsub s1, zero, s1");
    return;
  case ND_NUM:
    println("\t\tli s1, %ld", node->val);
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_expr(node->rhs);
    return;
  case ND_CAST: {
    gen_expr(node->rhs);
    println("\t\taddi sp,sp,-8");

    println("\t\tsd s1, 0(sp)");

    cast(node->rhs->ty, node->ty, "sp");
    println("\t\taddi sp,sp,8");
  }
    return;

  case ND_LOGAND: {
    int c = count();
    gen_expr(node->lhs);
    println("\t\tbeqz s1, .L.false.%d", c);
    gen_expr(node->rhs);
    println("\t\tbeqz s1, .L.false.%d", c);
    println("\t\taddi s1,zero, 1");
    println("\t\tj .L.end.%d", c);
    println(".L.false.%d:", c);
    println("\t\taddi s1,zero, 0");
    println(".L.end.%d:", c);
    return;
  }
  case ND_LOGOR: {
    int c = count();
    gen_expr(node->lhs);
    println("\t\tbnez s1, .L.true.%d", c);
    gen_expr(node->rhs);
    println("\t\tbnez s1, .L.true.%d", c);
    println("\t\taddi s1,zero, 0");
    println("\t\tj .L.end.%d", c);
    println(".L.true.%d:", c);
    println("\t\taddi s1,zero, 1");
    println(".L.end.%d:", c);
    return;
  }

  case ND_NOT:
    gen_expr(node->rhs);
    println("\t\tseqz s1, s1");
    return;
  case ND_BITNOT:
    gen_expr(node->rhs);
    println("\t\nnot s1, s1");
    return;
  case ND_DEREF:
    gen_expr(node->rhs);
    load(node);
    return;
  case ND_ADDR:
    gen_addr(node->rhs);
    return;
  case ND_VAR:
  case ND_MEMBER:
    gen_addr(node);
    load(node);
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    push();
    gen_expr(node->rhs);
    store(node->lhs, node->rhs);
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
  case ND_MOD:
    if (node->lhs->ty->size == 8) {
      println("\t\trem s1, s1, t0");
    } else {
      println("\t\tremw s1, s1, t0");
    }
    return;
  case ND_BITAND:
    println("\t\tand s1, s1, t0");
    return;
  case ND_BITOR:
    println("\t\tor s1, s1, t0");
    return;
  case ND_BITXOR:
    println("\t\txor s1, s1, t0");
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
    println("\t\tmulw s1, s1, t0");
    return;
  case ND_DIV:
    println("\t\tdivw s1, s1, t0");
    return;
  }

  error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node *node) {
  println("\t\t.loc 1 %d", node->tok->line_no);

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
      println("\t\tbeq zero, s1, %s", node->brk_label);
    }
    gen_stmt(node->then);
    if (node->inc) {
      gen_expr(node->inc);
    }
    println("\t\tjal zero, .L.begin.%d", c);
    println("%s:", node->brk_label);
    return;
  }
  case ND_GOTO:
    println("\t\tj %s", node->unique_label);
    return;
  case ND_LABEL:
    println("%s:", node->unique_label);
    gen_stmt(node->lhs);
    return;
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
      offset = align_to(offset, var->ty->align);
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
    if (!fn->is_function || !fn->is_definition) {
      continue;
    }
    if (fn->is_static) {
      println(".local %s", fn->name);

    } else {
      println(".globl %s", fn->name);
    }
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
      } else if (var->ty->size == 2) {
        println("\t\tsh %s, %d(fp)", argreg[i++], (var->offset));
      } else if (var->ty->size == 4) {
        println("\t\tsw %s, %d(fp)", argreg[i++], (var->offset));
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