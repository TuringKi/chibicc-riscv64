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
  case ND_NULL_EXPR:
    return;
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

  case ND_MEMZERO: {
    int cnt_of_int64 = node->var->ty->size / 8;
    int rem64 = node->var->ty->size % 8;
    for (int i = 0; i < cnt_of_int64; i++) {
      println("\t\tsd zero, %d(fp)", node->var->offset + i * 8);
    }
    if (rem64 >= 4) {
      println("\t\tsw zero, %d(fp)", node->var->offset + cnt_of_int64 * 8);
      int rem32 = (rem64 - 4) % 4;
      if (rem32 >= 2) {
        println("\t\tsh zero, %d(fp)",
                node->var->offset + cnt_of_int64 * 8 + 4);
        if (rem32 == 3) {
          println("\t\tsb zero, %d(fp)",
                  node->var->offset + cnt_of_int64 * 8 + 6);
        }
      } else if (rem32 == 1) {
        println("\t\tsb zero, %d(fp)",
                node->var->offset + cnt_of_int64 * 8 + 4);
      }
    } else {
      if (rem64 >= 2) {
        println("\t\tsh zero, %d(fp)", node->var->offset + cnt_of_int64 * 8);
      }
      int rem16 = (rem64 - 2) % 2;
      for (int i = 0; i < rem16; i++) {
        println("\t\tsb zero, %d(fp)",
                node->var->offset + cnt_of_int64 * 8 + 2 + i);
      }
    }
  }
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
  case ND_COND: {
    int c = count();
    gen_expr(node->cond);
    println("\t\tbeqz s1, .L.else.%d", c);
    gen_expr(node->then);
    println("  j .L.end.%d", c);
    println(".L.else.%d:", c);
    gen_expr(node->els);
    println(".L.end.%d:", c);
    return;
  }
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
  case ND_SHL:
    if (node->ty->size == 8) {
      println("\t\tsll s1, s1, t0");

    } else {
      println("\t\tsllw s1, s1, t0");
    }
    return;
  case ND_SHR:
    if (node->ty->size == 8) {
      println("\t\tsraw s1, s1, t0");

    } else {
      println("\t\tsra s1, s1, t0");
    }

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
    println("%s:", node->cont_label);
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
    gen_stmt(node->rhs);
    return;

  case ND_SWITCH:
    gen_expr(node->cond);

    for (Node *n = node->case_next; n; n = n->case_next) {
      println("\t\taddi t1, s1, -%ld", n->val);
      println("\t\tbeqz t1, %s", n->label);
    }

    if (node->default_case) {
      println("\t\tj %s", node->default_case->label);
    }

    println("\t\tj %s", node->brk_label);
    gen_stmt(node->then);
    println("%s:", node->brk_label);
    return;
  case ND_CASE:
    println("%s:", node->label);
    gen_stmt(node->rhs);
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
      offset = align_to(offset, var->align);
      var->offset = -offset;
    }
    fn->stack_size = align_to(offset, 16);
  }
}

static int find_pow2(Token *tok, int val) {

  long p2 = 1;
  for (int i = 1; i <= 63; i++) {
    if (p2 << i == val) {
      return i;
    }
  }
  error_tok(tok, "requested alignment '%d' is not a positive power of 2", val);
  return -1;
}

static void emit_data(Obj *prog) {

  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function || !var->is_definition) {
      continue;
    }

    println("\t\t.globl %s", var->name);
    Token *tok = var->ty->name;
    if (tok && var->align > 1) {
      int align = find_pow2(tok, var->align);
      if (var->ty->kind == TY_ARRAY || var->ty->kind == TY_STRUCT ||
          var->ty->kind == TY_UNION) {
        if (align < 3) {
          align = 3;
        }
      }
      println("\t\t.align %d", align);
    } else {
      // println("\t\t.align 1");
    }
    if (var->init_data) {
      println(".data");
      println("%s:", var->name);

      Relocation *rel = var->rel;
      int pos = 0;
      while (pos < var->ty->size) {
        if (rel && rel->offset == pos) {
          println("\t\t.quad %s%+ld", rel->label, rel->addend);
          rel = rel->next;
          pos += 8;
        } else {
          println("\t\t.byte %d", var->init_data[pos++]);
        }
      }

      continue;
    }
    println(".bss");
    println("%s:", var->name);
    println("\t\t.zero %d", var->ty->size);
  }
}

void emit_text(Obj *prog) {

  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition) {
      continue;
    }
    if (fn->is_static) {
      println("\t\t.local %s", fn->name);

    } else {
      println("\t\t.globl %s", fn->name);
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