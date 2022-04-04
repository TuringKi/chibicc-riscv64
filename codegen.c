#include "chibicc.h"
#include <stdlib.h>

static FILE *output_file;
static int depth;
#define MAX_ARGREG 8
static char *argreg[] = {"a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"};
static char *argfreg[] = {"fa0", "fa1", "fa2", "fa3",
                          "fa4", "fa5", "fa6", "fa7"};
static Obj *cur_fn;
typedef struct ConstVal ConstVal;
struct ConstVal {
  int idx;
  double fval;
  TypeKind kind;
  ConstVal *next;
};
static ConstVal *cur_const_val;
static void gen_expr(Node *node);
static void gen_stmt(Node *node);
static void gen_addr(Node *node);

static void println(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
  va_end(ap);
  fprintf(output_file, "\n");
}

enum { I8, I16, I32, I64, U8, U16, U32, U64, F32, F64 };

static int getTypeId(TypeKind ty, bool is_unsigned) {
  switch (ty) {
  case TY_CHAR:
    return is_unsigned ? U8 : I8;
  case TY_SHORT:
    return is_unsigned ? U16 : I16;
  case TY_INT:
    return is_unsigned ? U32 : I32;
  case TY_LONG:
    return is_unsigned ? U64 : I64;
  case TY_FLOAT:
    return F32;
  case TY_DOUBLE:
    return F64;
  }
  return U64;
}

static void cmp_zero(TypeKind kind) {
  if (kind == TY_FLOAT) {
    println("\t\tfmv.w.x ft1, zero");
    println("\t\tfeq.s s1, fs1,ft1");
    println("\t\tseqz s1, s1");
  } else if (kind == TY_DOUBLE) {
    println("\t\tfmv.d.x ft1, zero");
    println("\t\tfeq.d s1, fs1,ft1");
    println("\t\tseqz s1, s1");
  } else {
    println("\t\tsnez s1, s1");
  }
}

static char i32i8[] = "\t\tlb s1, 0(sp)";
static char i32i16[] = "\t\tlh s1, 0(sp)";
static char i32u16[] = "\t\tlhu s1, 0(sp)";
static char i32i64[] = "\t\tld s1, 0(sp)";
static char i32u8[] = "\t\tlbu s1, 0(sp)";
static char i32i32[] = "\t\tlw s1, 0(sp)";
static char i32u32[] = "\t\tlwu s1, 0(sp)";
static char i32u64[] = "\t\tld s1, 0(sp)";

static char i32f32[] =
    "\t\tlw s1, 0(sp)\n\t\tfcvt.s.w fs1, s1\n\t\tfsw fs1, 0(sp)";
static char i32f64[] =
    "\t\tlw s1, 0(sp)\n\t\tfcvt.d.w fs1, s1\n\t\tfsd fs1, 0(sp)";
static char u32f32[] =
    "\t\tlw s1, 0(sp)\n\t\tfcvt.s.wu fs1, s1\n\t\tfsw fs1, 0(sp)";
static char u32f64[] =
    "\t\tlw s1, 0(sp)\n\t\tfcvt.d.wu fs1, s1\n\t\tfsd fs1, 0(sp)";

static char i64f32[] = "\t\tfcvt.s.l fs1, s1\n\t\tfsw fs1, 0(sp)";
static char i64f64[] = "\t\tfcvt.d.l fs1, s1\n\t\tfsd fs1, 0(sp)";
static char u64f32[] = "\t\tfcvt.s.lu fs1, s1\n\t\tfsw fs1, 0(sp)";
static char u64f64[] = "\t\tfcvt.d.lu fs1, s1\n\t\tfsd fs1, 0(sp)";

static char f32i8[] = "\t\tflw fs1, 0(sp)\n\t\tfcvt.w.s s1,fs1, rtz\n\t\tsb "
                      "s1, 0(sp)\n\t\tlb s1, 0(sp)";
static char f32u8[] = "\t\tflw fs1, 0(sp)\n\t\tfcvt.wu.s s1,fs1, rtz\n\t\tsb "
                      "s1, 0(sp)\n\t\tlbu s1, 0(sp)";
static char f32i16[] = "\t\tflw fs1, 0(sp)\n\t\tfcvt.w.s s1,fs1, rtz\n\t\tsh "
                       "s1, 0(sp)\n\t\tlh s1, 0(sp)";
static char f32u16[] = "\t\tflw fs1, 0(sp)\n\t\tfcvt.wu.s s1,fs1, rtz\n\t\tsh "
                       "s1, 0(sp)\n\t\tlhu s1, 0(sp)";
static char f32i32[] =
    "\t\tflw fs1, 0(sp)\n\t\tfcvt.w.s s1, fs1, rtz\n\t\tsw s1, 0(sp)";
static char f32u32[] =
    "\t\tflw fs1, 0(sp)\n\t\tfcvt.wu.s s1, fs1, rtz\n\t\tsw s1, 0(sp)";
static char f32i64[] =
    "\t\tflw fs1, 0(sp)\n\t\tfcvt.l.s s1, fs1, rtz\n\t\tsd s1, 0(sp)";
static char f32u64[] =
    "\t\tflw fs1, 0(sp)\n\t\tfcvt.lu.s s1, fs1, rtz\n\t\tsd s1, 0(sp)";

static char f64i8[] = "\t\tfld fs1, 0(sp)\n\t\tfcvt.w.d s1,fs1, rtz\n\t\tsb "
                      "s1, 0(sp)\n\t\tlb s1, 0(sp)";
static char f64u8[] = "\t\tfld fs1, 0(sp)\n\t\tfcvt.wu.d s1,fs1, rtz\n\t\tsb "
                      "s1, 0(sp)\n\t\tlbu s1, 0(sp)";
static char f64i16[] = "\t\tfld fs1, 0(sp)\n\t\tfcvt.w.d s1,fs1, rtz\n\t\tsh "
                       "s1, 0(sp)\n\t\tlh s1, 0(sp)";
static char f64u16[] = "\t\tfld fs1, 0(sp)\n\t\tfcvt.wu.d s1,fs1, rtz\n\t\tsh "
                       "s1, 0(sp)\n\t\tlhu s1, 0(sp)";
static char f64i32[] =
    "\t\tfld fs1, 0(sp)\n\t\tfcvt.w.d s1, fs1, rtz\n\t\tsw s1, 0(sp)";
static char f64u32[] =
    "\t\tfld fs1, 0(sp)\n\t\tfcvt.wu.d s1, fs1, rtz\n\t\tsw s1, 0(sp)";
static char f64i64[] =
    "\t\tfld fs1, 0(sp)\n\t\tfcvt.l.d s1, fs1, rtz\n\t\tsd s1, 0(sp)";
static char f64u64[] =
    "\t\tfld fs1, 0(sp)\n\t\tfcvt.lu.d s1, fs1, rtz\n\t\tsd s1, 0(sp)";

static char f32f64[] = "\t\tfcvt.d.s fs1, fs1";
static char f64f32[] = "\t\tfcvt.s.d fs1, fs1";

static char *cast_table[][10] = {
    // i8   i16     i32     i64     u8     u16     u32     u64     f32     f64
    {NULL, i32i16, i32i32, i32i64, i32u8, i32u16, i32u32, i32u64, i32f32,
     i32f64}, // i8
    {i32i8, NULL, i32i32, i32i64, i32u8, i32u16, i32u32, i32u64, i32f32,
     i32f64}, // i16
    {i32i8, i32i16, NULL, i32i64, i32u8, i32u16, i32u32, i32u64, i32f32,
     i32f64}, // i32
    {i32i8, i32i16, i32i32, NULL, i32u8, i32u16, i32u32, NULL, i64f32,
     i64f64}, // i64

    {i32i8, i32i16, i32i32, i32i64, NULL, i32u16, i32u32, i32u64, i32f32,
     i32f64}, // u8
    {i32i8, i32i16, i32i32, i32i64, i32u8, NULL, i32u32, i32u64, i32f32,
     i32f64}, // u16
    {i32i8, i32i16, i32i32, i32i64, i32u8, i32u16, NULL, i32u64, u32f32,
     u32f64}, // u32
    {i32i8, i32i16, i32i32, NULL, i32u8, i32u16, i32u32, NULL, u64f32,
     u64f64}, // u64

    {f32i8, f32i16, f32i32, f32i64, f32u8, f32u16, f32u32, f32u64, NULL,
     f32f64}, // f32
    {f64i8, f64i16, f64i32, f64i64, f64u8, f64u16, f64u32, f64u64, f64f32,
     NULL}, // f64
};
static void cast(Type *from, Type *to) {
  if (to->kind == TY_VOID) {
    return;
  }
  if (to->kind == TY_BOOL) {

    cmp_zero(from->kind);
    return;
  }
  int t1 = getTypeId(from->kind, from->is_unsigned);
  int t2 = getTypeId(to->kind, to->is_unsigned);
  if (cast_table[t1][t2]) {
    println(cast_table[t1][t2]);
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

static void pushf(void) {
  println("\t\taddi sp,sp,-8");
  println("\t\tfsd fs1, 0(sp)");
  depth++;
}

static void popf(char *arg) {
  println("\t\tfld %s, 0(sp)", arg);
  println("\t\taddi sp,sp,8");
  depth--;
}

static void load(Node *node) {
  Type *ty = node->ty;
  if (ty->kind == TY_ARRAY || ty->kind == TY_STRUCT || ty->kind == TY_UNION ||
      ty->kind == TY_FUNC) {
    return;
  }
  if (!node->var) {
    if (ty->size == 1) {
      if (ty->is_unsigned) {
        println("\t\tlbu s1, 0(s1)");
      } else {
        println("\t\tlb s1, 0(s1)");
      }
      return;
    } else if (ty->size == 2) {
      if (ty->is_unsigned) {
        println("\t\tlhu s1, 0(s1)");
      } else {
        println("\t\tlh s1, 0(s1)");
      }
      return;
    } else if (ty->size == 4) {
      if (ty->kind == TY_FLOAT) {
        println("\t\tflw fs1, 0(s1)");
        return;
      }
      println("\t\tlw s1, 0(s1)");
      return;
    }
    if (ty->kind == TY_DOUBLE) {
      println("\t\tfld fs1, 0(s1)");
      return;
    }
    println("\t\tld s1, 0(s1)");
    return;
  }
  if (node->var->is_local) {
    if (ty->size == 1) {
      if (ty->is_unsigned) {
        println("\t\tlbu s1, 0(s1)");
      } else {
        println("\t\tlb s1, 0(s1)");
      }
      return;
    } else if (ty->size == 2) {
      if (ty->is_unsigned) {
        println("\t\tlhu s1, 0(s1)");
      } else {
        println("\t\tlh s1, 0(s1)");
      }
      return;
    } else if (ty->size == 4) {
      if (ty->kind == TY_FLOAT) {
        println("\t\tflw fs1, 0(s1)");
        return;
      }
      println("\t\tlw s1, 0(s1)");
      return;
    }
    if (ty->kind == TY_DOUBLE) {
      println("\t\tfld fs1, 0(s1)");
    } else {
      println("\t\tld s1, 0(s1)");
    }
  } else {
    println("\t\tlui s1, %%hi(%s)", node->var->name);
    if (ty->size == 1) {
      if (ty->is_unsigned) {
        println("\t\tlbu s1, %%lo(%s)(s1)", node->var->name);
      } else {
        println("\t\tlb s1, %%lo(%s)(s1)", node->var->name);
      }
      return;
    } else if (ty->size == 2) {
      if (ty->is_unsigned) {
        println("\t\tlhu s1, %%lo(%s)(s1)", node->var->name);
      } else {
        println("\t\tlh s1, %%lo(%s)(s1)", node->var->name);
      }
      return;
    } else if (ty->size == 4) {
      if (ty->kind == TY_FLOAT) {
        println("\t\tflw fs1, %%lo(%s)(s1)", node->var->name);
      }
      println("\t\tlw s1, %%lo(%s)(s1)", node->var->name);
      return;
    }
    if (ty->kind == TY_DOUBLE) {
      println("\t\tfld fs1, %%lo(%s)(s1)", node->var->name);
    } else {
      println("\t\tld s1, %%lo(%s)(s1)", node->var->name);
    }
  }
}

static void store_struct(Type *ty, char *rg_val, char *rg_addr) {
  println("\t\tadd a7, zero, %s", rg_val);
  for (Member *member = ty->members; member; member = member->next) {
    println("\t\tli t2, %d", member->offset);
    println("\t\tadd t2, %s, t2", rg_addr);
    if (member->ty->size == 1) {
      if (member->ty->is_unsigned) {
        println("\t\tlbu s1, %d(a7)", member->offset);
      } else {
        println("\t\tlb s1, %d(a7)", member->offset);
      }
      println("\t\tsb s1, 0(t2)");
    } else if (member->ty->size == 2) {

      if (member->ty->is_unsigned) {
        println("\t\tlhu s1, %d(a7)", member->offset);
      } else {
        println("\t\tlh s1, %d(a7)", member->offset);
      }

      println("\t\tsh s1, 0(t2)");
    } else if (member->ty->size == 4) {
      if (member->ty->kind == TY_FLOAT) {
        println("\t\tflw fs1, %d(a7)", member->offset);
        println("\t\tfsw fs1, 0(t2)");
      } else {
        println("\t\tlw s1, %d(a7)", member->offset);
        println("\t\tsw s1, 0(t2)");
      }

    } else {
      if (member->ty->kind == TY_DOUBLE) {
        println("\t\tfld fs1, %d(a7)", member->offset);
        println("\t\tfsd fs1, 0(t2)");
      } else {
        println("\t\tld s1, %d(a7)", member->offset);
        println("\t\tsd s1, 0(t2)");
      }
    }
  }
  println("\t\tadd %s, zero, a7", rg_val);
  return;
}

static void store(Node *lhs, Node *rhs) {
  Type *ty = lhs->ty;
  pop("t0");

  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    store_struct(ty, "s1", "t0");
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
      if (ty->kind == TY_FLOAT) {
        println("\t\tfsw fs1, 0(t0)");
        return;
      }
      println("\t\tsw s1, 0(t0)");
      return;
    }
    if (ty->kind == TY_DOUBLE) {
      println("\t\tfsd fs1, 0(t0)");
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
      if (ty->kind == TY_FLOAT) {
        println("\t\tfsw fs1, 0(t0)");
        return;
      }
      println("\t\tsw s1, 0(t0)");
      return;
    }
    if (ty->kind == TY_DOUBLE) {
      println("\t\tfsd fs1, 0(t0)");
    } else {
      println("\t\tsd s1, 0(t0)");
    }

  } else {
    println("\t\tlui t0, %%hi(%s)", lhs->var->name);
    if (ty->size == 1) {
      println("\t\tsb s1, %%lo(%s)(t0)", lhs->var->name);
      return;
    } else if (ty->size == 2) {
      println("\t\tsh s1, %%lo(%s)(t0)", lhs->var->name);
      return;
    } else if (ty->size == 4) {
      if (ty->kind == TY_FLOAT) {
        println("\t\tfsw fs1, %%lo(%s)(t0)", lhs->var->name);
        return;
      }
      println("\t\tsw s1, %%lo(%s)(t0)", lhs->var->name);
      return;
    }
    if (ty->kind == TY_DOUBLE) {
      println("\t\tfsd fs1, %%lo(%s)(t0)", lhs->var->name);
    } else {
      println("\t\tsd s1, %%lo(%s)(t0)", lhs->var->name);
    }
  }
}

int align_to(int n, int align) { return (n + align - 1) / align * align; }

static void gen_addr(Node *node) {
  switch (node->kind) {
  case ND_VAR:
    if (node->var->is_local) {
      println("\t\tli s1, %d", node->var->offset);
      println("\t\tadd s1, fp, s1");
      return;
    } else {
      println("\t\tlui s1, %%hi(%s)", node->var->name);
      println("\t\taddi s1, s1, %%lo(%s)", node->var->name);
      return;
    }
  case ND_MEMBER:
    gen_addr(node->rhs);
    println("\t\tli t2, %d", node->member->offset);
    println("\t\tadd s1, s1, t2");
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_addr(node->rhs);
    return;
  case ND_DEREF:
    gen_expr(node->rhs);
    return;
  case ND_FUNCCAL:
    if (node->ret_buffer) {
      gen_expr(node);
      return;
    }
  }

  error_tok(node->tok, "not an lvalue");
}

static ConstVal *create_constval(double fval, TypeKind kind) {
  ConstVal *cval = calloc(1, sizeof(ConstVal));
  cval->idx = count();
  cval->fval = fval;
  cval->kind = kind;
  cval->next = NULL;
  cur_const_val = cur_const_val->next = cval;
  return cur_const_val;
}

static void push_struct(Node *arg) {
  int sz = align_to(arg->ty->size, 8);
  println("\t\taddi sp, sp, -%d", sz);
  for (int i = 0; i < sz / 8; i++) {
    println("\t\tld t2, %d(s1)", i * 8);
    println("\t\tsd t2, %d(sp)", i * 8);
  }
  depth += sz / 8;
}

static void push_args2(Node *args, bool first_pass, int *cnt_depth) {
  if (!args)
    return;
  push_args2(args->next, first_pass, cnt_depth);

  if ((first_pass && !args->pass_by_stack) ||
      (!first_pass && args->pass_by_stack)) {
    if (((args->ty->kind == TY_STRUCT || args->ty->kind == TY_UNION))) {
      if (args->ty->size <= 16) {
        return;
      }
    } else {
      return;
    }
  }

  switch (args->ty->kind) {
  case TY_STRUCT:
  case TY_UNION:
    if (args->ty->size > 16) {
      if (first_pass) {
        gen_expr(args);
        push_struct(args);
        *cnt_depth += align_to(args->ty->size, 8);
        args->param_stack_offset = *cnt_depth;
      } else {
        println("\t\tli t0, -%d", args->param_stack_offset);
        println("\t\tadd s1, s2, t0");
        push();
      }

    } else if (args->ty->size <= 16) {
      gen_expr(args);
      int nm = 0;

      for (Member *member = args->ty->members; member; member = member->next) {
        nm++;
      }
      if (nm == 2) {
        println("\t\taddi sp,sp,-16");
        depth += 2;
        int i = 0;
        for (Member *member = args->ty->members; member;
             member = member->next) {

          if (member->ty->kind == TY_FLOAT) {
            println("\t\tflw ft0, %d(s1)", member->offset);
            println("\t\tfsd ft0, %d(sp)", i * 8);
          } else if (member->ty->kind == TY_DOUBLE) {
            println("\t\tfld ft0, %d(s1)", member->offset);
            println("\t\tfsd ft0,  %d(sp)", i * 8);
          } else {
            println("\t\tld t0, %d(s1)", member->offset);
            println("\t\tsd t0,  %d(sp)", i * 8);
          }
          i++;
        }
      } else {
        push_struct(args);
      }
      if (first_pass && args->pass_by_stack) {
        *cnt_depth += align_to(args->ty->size, 8);
      }
    }
    break;
  case TY_FLOAT:
  case TY_DOUBLE:
    gen_expr(args);
    pushf();
    if (first_pass && args->pass_by_stack) {
      *cnt_depth += 8;
    }
    break;
  default:
    gen_expr(args);
    push();
    if (first_pass && args->pass_by_stack) {
      *cnt_depth += 8;
    }
  }
}
static int count_args_depth(Node *args) {
  int cnt;
  for (Node *arg = args; arg; arg = arg->next) {
    switch (arg->ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      if (args->ty->size <= 16) {
        cnt += align_to(arg->ty->size, 8);
      } else {
        cnt += 8;
      }
      break;
    default:
      cnt += 8;
    }
  }
  return cnt;
}

static int push_args(Node *node, int np, bool is_va_area) {

  int stack = 0, gp = 0, fp = 0, allp = 0, param_stack_offset = 0;

  if (node->ret_buffer && node->ty->size > 16) {
    gp++;
  }

  for (Node *arg = node->args; arg; arg = arg->next) {
    if (is_va_area && is_flonum(arg->ty)) {
      if (allp >= np && gp++ >= MAX_ARGREG) {
        arg->pass_by_stack = true;
        stack++;
      }

    } else {

      Type *ty = arg->ty;

      switch (ty->kind) {
      case TY_STRUCT:
      case TY_UNION: {
        if (ty->size > 16) {
          arg->pass_by_stack = true;
          stack += align_to(ty->size, 8) / 8;
          if (gp++ >= MAX_ARGREG) {
            stack++;
          }

        } else {

          int nm = 0;
          int cf = 0;
          for (Member *member = ty->members; member; member = member->next) {
            nm++;
            if (member->ty->kind == TY_FLOAT || member->ty->kind == TY_DOUBLE) {
              cf++;
            }
          }

          if (nm == 1) {
            Member *mem = ty->members;
            switch (mem->ty->kind) {
            case TY_STRUCT:
            case TY_UNION:
              error_tok(mem->name, "parameter not support struct's struct");
              break;
            case TY_FLOAT:
            case TY_DOUBLE:
              if (fp++ >= MAX_ARGREG && gp++ >= MAX_ARGREG) {
                mem->pass_by_stack = true;
                stack++;
              }
              break;
            default:
              if (gp++ >= MAX_ARGREG) {
                mem->pass_by_stack = true;
                stack++;
              }
            }
          } else {
            // if no floating, all use ax
            if (cf == 0) {
              if (ty->size <= 8) {
                if (gp++ >= MAX_ARGREG) {
                  arg->pass_by_stack = true;
                  stack++;
                }
              } else {
                if (gp + 2 >= MAX_ARGREG) {
                  arg->pass_by_stack = true;
                  stack += 2;
                  gp += 2;
                }
              }
            } else if (nm == 2) {
              if (cf == 2) { // tow floating
                if (fp++ >= MAX_ARGREG && gp++ >= MAX_ARGREG) {
                  arg->ty->members->pass_by_stack = true;
                  stack++;
                }
                if (fp++ >= MAX_ARGREG && gp++ >= MAX_ARGREG) {
                  arg->ty->members->next->pass_by_stack = true;
                  stack++;
                }
              } else {
                if (fp++ >= MAX_ARGREG && gp++ >= MAX_ARGREG) {
                  arg->ty->members->pass_by_stack = true;
                  stack++;
                }
                if (gp++ >= MAX_ARGREG) {
                  arg->pass_by_stack = true;
                  stack++;
                }
              }
            } else { // nm > 2
            }
          }
        }
        break;
      }

      case TY_FLOAT:
      case TY_DOUBLE:
        if (fp++ >= MAX_ARGREG && gp++ >= MAX_ARGREG) {
          arg->pass_by_stack = true;
          stack++;
        }
        break;
      default:
        if (gp++ >= MAX_ARGREG) {
          arg->pass_by_stack = true;
          stack++;
        }
      }

      allp++;
    }
  }
  if ((depth + stack) % 2 == 1) {
    println("\t\taddi sp,sp,-8");
    depth++;
    stack++;
  }
  int cnt_depth = 0;
  println("\t\tmv s2, sp");
  push_args2(node->args, true, &cnt_depth);
  push_args2(node->args, false, &cnt_depth);

  if (node->ret_buffer && node->ty->size > 16) {
    println("\t\tli t1, %d", node->ret_buffer->offset);
    println("\t\tadd s1, t1, fp");
    push();
  }

  return stack;
}

static void copy_ret_buffer(Obj *var) { Type *ty = var->ty; }

static void gen_expr(Node *node) {
  println("  .loc %d %d", node->tok->file->file_no, node->tok->line_no);

  switch (node->kind) {
  case ND_NULL_EXPR:
    return;
  case ND_NEG:
    gen_expr(node->rhs);
    if (node->ty->kind == TY_DOUBLE) {
      println("\t\tfneg.d fs1, fs1");
    } else if (node->ty->kind == TY_FLOAT) {
      println("\t\tfneg.s fs1, fs1");
    } else if (node->ty->kind == TY_LONG) {
      println("\t\tneg s1, s1");
    } else {
      println("\t\tnegw s1, s1");
    }

    return;
  case ND_NUM: {

    switch (node->ty->kind) {
    case TY_FLOAT: {
      ConstVal *cval = create_constval(node->fval, TY_FLOAT);
      println("\t\tlui t1, %%hi(.LC%d)", cval->idx);
      println("\t\tflw fs1, %%lo(.LC%d)(t1)", cval->idx);
      return;
    }
    case TY_DOUBLE: {
      ConstVal *cval = create_constval(node->fval, TY_DOUBLE);
      println("\t\tlui t1, %%hi(.LC%d)", cval->idx);
      println("\t\tfld fs1, %%lo(.LC%d)(t1)", cval->idx);
      return;
    }
    }

    println("\t\tli s1, %ld", node->val);
    return;
  }

  case ND_COMMA:
    gen_expr(node->lhs);
    gen_expr(node->rhs);
    return;

  case ND_MEMZERO: {
    int cnt_of_int64 = node->var->ty->size / 8;
    int rem64 = node->var->ty->size % 8;

    println("\t\tli t1, %d", node->var->offset);
    println("\t\tadd t1, t1, fp");
    for (int i = 0; i < cnt_of_int64; i++) {
      println("\t\tsd zero, %d(t1)", i * 8);
    }
    if (rem64 >= 4) {
      println("\t\tsw zero, %d(t1)", cnt_of_int64 * 8);
      int rem32 = (rem64 - 4) % 4;
      if (rem32 >= 2) {
        println("\t\tsh zero, %d(t1)", cnt_of_int64 * 8 + 4);
        if (rem32 == 3) {
          println("\t\tsb zero, %d(t1)", cnt_of_int64 * 8 + 6);
        }
      } else if (rem32 == 1) {
        println("\t\tsb zero, %d(t1)", cnt_of_int64 * 8 + 4);
      }
    } else {
      if (rem64 >= 2) {
        println("\t\tsh zero, %d(t1)", cnt_of_int64 * 8);
      }
      int rem16 = (rem64 - 2) % 2;
      for (int i = 0; i < rem16; i++) {
        println("\t\tsb zero, %d(t1)", cnt_of_int64 * 8 + 2 + i);
      }
    }
  }
    return;
  case ND_CAST: {
    gen_expr(node->rhs);
    println("\t\taddi sp,sp,-8");
    if (node->rhs->ty->kind == TY_FLOAT) {
      println("\t\tfsw fs1, 0(sp)");
    } else if (node->rhs->ty->kind == TY_DOUBLE) {
      println("\t\tfsd fs1, 0(sp)");
    } else {
      println("\t\tsd s1, 0(sp)");
    }

    cast(node->rhs->ty, node->ty);
    println("\t\taddi sp,sp,8");
  }
    return;

  case ND_LOGAND: {
    int c = count();
    gen_expr(node->lhs);
    if (node->lhs->ty->kind == TY_FLOAT) {
      println("\t\tfmv.w.x ft1, zero");
      println("\t\tfeq.s s1, fs1,ft1");
      println("\t\tbnez s1, .L.false.%d", c);
    } else if (node->lhs->ty->kind == TY_DOUBLE) {
      println("\t\tfmv.d.x ft1, zero");
      println("\t\tfeq.d s1, fs1,ft1");
      println("\t\tbnez s1, .L.false.%d", c);
    } else {
      println("\t\tbeqz s1, .L.false.%d", c);
    }
    //
    gen_expr(node->rhs);

    if (node->rhs->ty->kind == TY_FLOAT) {
      println("\t\tfmv.w.x ft1, zero");
      println("\t\tfeq.s s1, fs1,ft1");
      println("\t\tbnez s1, .L.false.%d", c);
    } else if (node->rhs->ty->kind == TY_DOUBLE) {
      println("\t\tfmv.d.x ft1, zero");
      println("\t\tfeq.d s1, fs1,ft1");
      println("\t\tbnez s1, .L.false.%d", c);
    } else {
      println("\t\tbeqz s1, .L.false.%d", c);
    }
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
    if (node->lhs->ty->kind == TY_FLOAT) {
      println("\t\tfmv.w.x ft1, zero");
      println("\t\tfeq.s s1, fs1,ft1");
      println("\t\tbeqz s1, .L.true.%d", c);
    } else if (node->lhs->ty->kind == TY_DOUBLE) {
      println("\t\tfmv.d.x ft1, zero");
      println("\t\tfeq.d s1, fs1,ft1");
      println("\t\tbeqz s1, .L.true.%d", c);
    } else {
      println("\t\tbnez s1, .L.true.%d", c);
    }

    gen_expr(node->rhs);
    if (node->rhs->ty->kind == TY_FLOAT) {
      println("\t\tfmv.w.x ft1, zero");
      println("\t\tfeq.s s1, fs1,ft1");
      println("\t\tbeqz s1, .L.true.%d", c);
    } else if (node->rhs->ty->kind == TY_DOUBLE) {
      println("\t\tfmv.d.x ft1, zero");
      println("\t\tfeq.d s1, fs1,ft1");
      println("\t\tbeqz s1, .L.true.%d", c);
    } else {
      println("\t\tbnez s1, .L.true.%d", c);
    }
    println("\t\taddi s1,zero, 0");
    println("\t\tj .L.end.%d", c);
    println(".L.true.%d:", c);
    println("\t\taddi s1,zero, 1");
    println(".L.end.%d:", c);
    return;
  }

  case ND_NOT:
    gen_expr(node->rhs);
    if (node->rhs->ty->kind == TY_FLOAT) {
      println("\t\tfmv.w.x ft1, zero");
      println("\t\tfeq.s s1, fs1,ft1");
    } else if (node->rhs->ty->kind == TY_DOUBLE) {
      println("\t\tfmv.d.x ft1, zero");
      println("\t\tfeq.d s1, fs1,ft1");
    } else {
      println("\t\tseqz s1, s1");
    }
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
    if (node->cond->ty->kind == TY_FLOAT) {
      println("\t\tfmv.w.x ft1, zero");
      println("\t\tfeq.s s1, fs1,ft1");
      println("\t\tbnez s1, .L.else.%d", c);
    } else if (node->cond->ty->kind == TY_DOUBLE) {
      println("\t\tfmv.d.x ft1, zero");
      println("\t\tfeq.d s1, fs1,ft1");
      println("\t\tbnez s1, .L.else.%d", c);
    } else {
      println("\t\tbeqz s1, .L.else.%d", c);
    }

    gen_expr(node->then);
    println("\t\tj .L.end.%d", c);
    println(".L.else.%d:", c);
    gen_expr(node->els);
    println(".L.end.%d:", c);
    return;
  }
  case ND_FUNCCAL: {

    bool is_va_area = false;
    int np = 0, up = 0;

    //  is_va_area = true;
    for (Type *var = node->func_ty->params; var; var = var->next) {
      np++;
    }
    for (Node *arg = node->args; arg; arg = arg->next) {
      up++;
    }
    if (up > np) {
      is_va_area = true;
    }

    int stack_args = push_args(node, np, is_va_area);

    gen_expr(node->rhs);

    int gp = 0, fp = 0, allp = 0;

    if (node->ret_buffer && node->ty->size > 16) {
      pop(argreg[gp++]);
    }

    for (Node *arg = node->args; arg; arg = arg->next) {

      if (is_va_area && is_flonum(arg->ty)) {
        if (allp >= np) {

          if (gp < MAX_ARGREG) {
            popf("ft1");
            println("\t\tfmv.x.d %s,ft1", argreg[gp++]);
          }
        }

      } else if (arg->ty->kind == TY_STRUCT || arg->ty->kind == TY_UNION) {
        if (arg->ty->size > 16) {

          if (gp < MAX_ARGREG) {
            pop(argreg[gp++]);
          }
          continue;
        }
        int nm = 0;
        int cf = 0;
        Type *ty = arg->ty;
        int sz = align_to(ty->size, 8) / 8;
        for (Member *member = ty->members; member; member = member->next) {
          nm++;
          if (member->ty->kind == TY_FLOAT || member->ty->kind == TY_DOUBLE) {
            cf++;
          }
        }

        if (nm == 1) {
          Member *mem = ty->members;
          switch (mem->ty->kind) {
          case TY_STRUCT:
          case TY_UNION:
            error_tok(mem->name, "parameter not support struct's struct");
            break;
          case TY_FLOAT:
          case TY_DOUBLE:
            if (fp < MAX_ARGREG) {
              popf(argfreg[fp++]);
            } else if (gp < MAX_ARGREG) {
              popf("ft1");
              println("\t\tfmv.x.d %s,ft1", argreg[gp++]);
              fp++;
            }
            break;
          default:
            if (gp < MAX_ARGREG) {
              pop(argreg[gp++]);
            }
          }
        } else if (nm == 2 && cf >= 1) {
          if (cf == 1) {
            if (is_flonum(ty->members->ty)) { // floating first
              if (fp < MAX_ARGREG) {
                popf(argfreg[fp++]);
              } else if (gp < MAX_ARGREG) {
                popf("ft1");
                println("\t\tfmv.x.d %s,ft1", argreg[gp++]);
                fp++;
              }
              if (gp < MAX_ARGREG) {
                pop(argreg[gp++]);
              }
            } else {
              if (gp < MAX_ARGREG) { // integer first
                pop(argreg[gp++]);
              }
              if (fp < MAX_ARGREG) {
                popf(argfreg[fp++]);
              } else if (gp < MAX_ARGREG) {
                popf("ft1");
                println("\t\tfmv.x.d %s,ft1", argreg[gp++]);
                fp++;
              }
            }
          } else {
            if (fp < MAX_ARGREG) {
              popf(argfreg[fp++]);
            } else if (gp < MAX_ARGREG) {
              popf("ft1");
              println("\t\tfmv.x.d %s,ft1", argreg[gp++]);
              fp++;
            }
            if (fp < MAX_ARGREG) {
              popf(argfreg[fp++]);
            } else if (gp < MAX_ARGREG) {
              popf("ft1");
              println("\t\tfmv.x.d %s,ft1", argreg[gp++]);
              fp++;
            }
          }

        } else {
          for (int i = 0; i < sz; i++) {
            if (gp < MAX_ARGREG) {
              pop(argreg[gp++]);
            }
          }
        }

      } else {
        if (is_flonum(arg->ty)) {
          if (fp < MAX_ARGREG) {
            popf(argfreg[fp++]);
          } else if (gp < MAX_ARGREG) {
            popf("ft1");
            println("\t\tfmv.x.d %s,ft1", argreg[gp++]);
            fp++;
          }

        } else if (gp < MAX_ARGREG) {
          pop(argreg[gp++]);
        }
      }

      allp++;
    }

    println("\t\tjalr s1");
    int tmp_stack_args = 8 * stack_args;
    println("\t\taddi sp,sp, %d", tmp_stack_args);
    depth -= stack_args;

    println("\t\taddi sp,sp,-8");
    if (node->ty->kind == TY_FLOAT) {
      println("\t\tfsw fa0, 0(sp)");
    } else if (node->ty->kind == TY_DOUBLE) {
      println("\t\tfsd fa0, 0(sp)");
    } else {
      println("\t\tsd a0, 0(sp)");
    }

    switch (node->ty->kind) {
    case TY_BOOL:
      println("\t\tsnez s1, a0");
      break;
    case TY_CHAR:
      if (node->ty->is_unsigned) {
        println("\t\tlbu s1, 0(sp)");
      } else {
        println("\t\tlb s1, 0(sp)");
      }
      break;
    case TY_SHORT:
      if (node->ty->is_unsigned) {
        println("\t\tlhu s1, 0(sp)");
      } else {
        println("\t\tlh s1, 0(sp)");
      }
      break;
    case TY_INT:
      println("\t\tlw s1, 0(sp)");
      break;
    case TY_FLOAT:
      println("\t\tflw fs1, 0(sp)");
      break;
    case TY_DOUBLE:
      println("\t\tfld fs1, 0(sp)");
      break;
    default:
      println("\t\tld s1, 0(sp)");
      break;
    }

    println("\t\taddi sp,sp,8");

    if (node->ret_buffer && node->ty->size <= 16) {
      copy_ret_buffer(node->ret_buffer);
      //   println("  lea %d(%%rbp), %%rax", node->ret_buffer->offset);
    }

    return;
  }
  }

  if (is_flonum(node->lhs->ty)) {
    gen_expr(node->rhs);
    pushf();
    gen_expr(node->lhs);
    popf("ft0");

    char *sz = (node->lhs->ty->kind == TY_FLOAT) ? "s" : "d";

    switch (node->kind) {
    case ND_EQ:
      println("\t\tfeq.%s s1, fs1, ft0", sz);
      return;
    case ND_NE:
      println("\t\tfeq.%s s1, fs1, ft0", sz);
      println("\t\tseqz s1, s1");
      return;
    case ND_LT:
      println("\t\tflt.%s s1, fs1, ft0", sz);
      return;
    case ND_LE:
      println("\t\tfle.%s s1, fs1, ft0", sz);
      return;
    case ND_ADD:
      println("\t\tfadd.%s fs1, fs1, ft0", sz);
      return;
    case ND_SUB:
      println("\t\tfsub.%s fs1, fs1, ft0", sz);
      return;
    case ND_MUL:
      println("\t\tfmul.%s fs1, fs1, ft0", sz);
      return;
    case ND_DIV:
      println("\t\tfdiv.%s fs1, fs1, ft0", sz);
      return;
    }

    error_tok(node->tok, "invalid expression");
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
    // println("\t\tsub t1, s1, t0");
    if (node->lhs->ty->is_unsigned) {
      println("\t\tsgtu s1, s1, t0");
    } else {
      println("\t\tsgt s1, s1, t0");
    }

    println("\t\txori s1, s1, 1");
    println("\t\tandi s1, s1, 0xff");
    return;
  case ND_MOD:
    if (node->lhs->ty->size == 8) {
      if (node->lhs->ty->is_unsigned) {
        println("\t\tremu s1, s1, t0");
      } else {
        println("\t\trem s1, s1, t0");
      }

    } else {
      if (node->lhs->ty->is_unsigned) {
        println("\t\tremuw s1, s1, t0");
      } else {
        println("\t\tremw s1, s1, t0");
      }
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
    // println("\t\tsub t1, t0, s1");
    if (node->lhs->ty->is_unsigned) {
      println("\t\tsgtu s1, t0, s1");
    } else {
      println("\t\tsgt s1, t0, s1");
    }
    return;
  case ND_ADD:
    println("\t\tadd s1, s1, t0");
    return;
  case ND_SUB:
    println("\t\tsub s1, s1, t0");
    return;
  case ND_MUL:
    if (node->ty->size == 8) {
      println("\t\tmul s1, s1, t0");
    } else {
      println("\t\tmulw s1, s1, t0");
    }

    return;
  case ND_DIV:
    if (node->ty->size == 8) {
      if (node->ty->is_unsigned) {
        println("\t\tdivu s1, s1, t0");
      } else {
        println("\t\tdiv s1, s1, t0");
      }

    } else {
      if (node->ty->is_unsigned) {
        println("\t\tdivuw s1, s1, t0");
      } else {
        println("\t\tdivw s1, s1, t0");
      }
    }

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
      if (node->ty->is_unsigned) {
        println("\t\tsrl s1, s1, t0");
      } else {
        println("\t\tsra s1, s1, t0");
      }

    } else {
      if (node->ty->is_unsigned) {
        println("\t\tsrlw s1, s1, t0");
      } else {
        println("\t\tsraw s1, s1, t0");
      }
    }

    return;
  }

  error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node *node) {
  println("  .loc %d %d", node->tok->file->file_no, node->tok->line_no);

  switch (node->kind) {
  case ND_IF: {
    int c = count();
    gen_expr(node->cond);
    if (node->cond->ty->kind == TY_FLOAT) {
      println("\t\tfmv.w.x ft1, zero");
      println("\t\tfeq.s s1, fs1,ft1");
      println("\t\tbnez s1, .L.else.%d", c);
    } else if (node->cond->ty->kind == TY_DOUBLE) {
      println("\t\tfmv.d.x ft1, zero");
      println("\t\tfeq.d s1, fs1,ft1");
      println("\t\tbnez s1, .L.else.%d", c);
    } else {
      println("\t\tbeqz s1, .L.else.%d", c);
    }
    gen_stmt(node->then);
    println("\t\tj .L.end.%d", c);
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
      if (node->cond->ty->kind == TY_FLOAT) {
        println("\t\tfmv.w.x ft1, zero");
        println("\t\tfeq.s s1, fs1,ft1");
        println("\t\tbnez s1,  %s", node->brk_label);
      } else if (node->cond->ty->kind == TY_DOUBLE) {
        println("\t\tfmv.d.x ft1, zero");
        println("\t\tfeq.d s1, fs1,ft1");
        println("\t\tbnez s1,  %s", node->brk_label);
      } else {
        println("\t\tbeqz s1, %s", node->brk_label);
      }
    }
    gen_stmt(node->then);
    println("%s:", node->cont_label);
    if (node->inc) {
      gen_expr(node->inc);
    }
    println("\t\tj .L.begin.%d", c);
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
  case ND_DO: {
    int c = count();
    println(".L.begin.%d:", c);
    gen_stmt(node->then);
    println("%s:", node->cont_label);
    gen_expr(node->cond);
    if (node->cond->ty->kind == TY_FLOAT) {
      println("\t\tfmv.w.x ft1, zero");
      println("\t\tfeq.s s1, fs1,ft1");
      println("\t\tbeqz s1, .L.begin.%d", c);
    } else if (node->cond->ty->kind == TY_DOUBLE) {
      println("\t\tfmv.d.x ft1, zero");
      println("\t\tfeq.d s1, fs1,ft1");
      println("\t\tbeqz s1, .L.begin.%d", c);
    } else {
      println("  bnez s1, .L.begin.%d", c);
    }

    println("%s:", node->brk_label);
    return;
  }
  case ND_SWITCH:
    gen_expr(node->cond);

    for (Node *n = node->case_next; n; n = n->case_next) {
      println("\t\tli t1, %ld", n->val);
      println("\t\tsub t1, s1, t1");
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
    if (node->rhs) {
      gen_expr(node->rhs);
    }

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
    int top = 0;
    int bottom = 24; // 16~24:ra, fp

    int gp = 0, fp = 0;

    for (Obj *var = fn->params; var; var = var->next) {
      if (is_flonum(var->ty)) {
        if (fp++ < MAX_ARGREG || (fp >= MAX_ARGREG && gp++ < MAX_ARGREG)) {
          continue;
        }
      } else {
        if (gp++ < MAX_ARGREG) {
          continue;
        }
      }
      if (top > 0) {
        top = align_to(top, 8);
      }
      var->offset = top;
      var->is_stack_param = true;
      top += var->ty->size;
    }

    for (Obj *var = fn->locals; var; var = var->next) {
      if (var->is_stack_param) {
        continue;
      }
      bottom += var->ty->size;
      bottom = align_to(bottom, var->align);
      var->offset = -bottom;
    }
    fn->stack_size = align_to(bottom, 16);
  }
}

static void emit_const(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
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
static void emit_constval(ConstVal *begin) {
  for (ConstVal *cval = begin; cval; cval = cval->next) {

    switch (cval->kind) {
    case TY_FLOAT:
      println(".LC%d:", cval->idx);
      float v = (float)cval->fval;
      println("\t\t.word\t%d", *(int *)(&v));
      break;
    case TY_DOUBLE:
      println(".LC%d:", cval->idx);
      println("\t\t.quad\t%ld", *(long *)(&cval->fval));
      break;
    }
  }
}
static void emit_data(Obj *prog) {

  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function || !var->is_definition) {
      continue;
    }
    if (var->is_static) {
      println("\t\t.local %s", var->name);
    } else {
      println("\t\t.globl %s", var->name);
    }
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

static void emit_normal_arg(Type *ty, int *gpi, int *fpi) {
  if (ty->size == 1) {
    println("\t\tsb %s, 0(t1)", argreg[(*gpi)++]);
  } else if (ty->size == 2) {
    println("\t\tsh %s, 0(t1)", argreg[(*gpi)++]);
  } else if (ty->size == 4) {
    if (ty->kind == TY_FLOAT) {
      if ((*fpi) < MAX_ARGREG) {
        println("\t\tfsw %s, 0(t1)", argfreg[(*fpi)++]);

      } else {
        println("\t\tmv s1, %s", argreg[(*gpi)++]);
        println("\t\tfmv.w.x fs1,s1");
        println("\t\tfsw fs1, 0(t1)");
      }
    } else {
      println("\t\tsw %s, 0(t1)", argreg[(*gpi)++]);
    }

  } else {
    if (ty->kind == TY_DOUBLE) {
      if ((*fpi) < MAX_ARGREG) {
        println("\t\tfsd %s, 0(t1)", argfreg[(*fpi)++]);
      } else {
        println("\t\tmv s1, %s", argreg[(*gpi)++]);
        println("\t\tfmv.d.x fs1,s1");
        println("\t\tfsd fs1, 0(t1)");
      }

    } else {
      println("\t\tsd %s, 0(t1)", argreg[(*gpi)++]);
    }
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

    println("\t\tsd ra, -16(sp)");
    println("\t\tsd fp, -24(sp)");
    println("\t\taddi fp, sp, 0");
    println("\t\tli t1, %d", fn->stack_size);
    println("\t\tsub sp, sp, t1");

    if (fn->va_area) {
      int np = 0;
      for (Obj *var = fn->params; var; var = var->next) {
        np++;
      }
      int offset = fn->va_area->offset;

      println("\t\tli t1, %d", offset);
      println("\t\tadd t1, t1, fp");
      offset = 0;

      for (int i = np; i < MAX_ARGREG; i++) {
        println("\t\tsd a%d, %d(t1)", i, offset);
        offset += 8;
      }
    }

    int gpi = 0, fpi = 0;
    for (Obj *var = fn->params; var; var = var->next) {
      if (var->is_stack_param) {
        continue;
      }

      println("\t\tli t1, %d", var->offset);
      println("\t\tadd t1, t1, fp");

      if (var->ty->kind == TY_STRUCT || var->ty->kind == TY_UNION) {

        Type *ty = var->ty;
        int nm = 0;
        int cf = 0;
        for (Member *member = ty->members; member; member = member->next) {
          nm++;
          if (member->ty->kind == TY_FLOAT || member->ty->kind == TY_DOUBLE) {
            cf++;
          }
        }
        int sz = align_to(ty->size, 8);
        if (sz > 16) {
          println("\t\tmv s2, %s", argreg[gpi++]);
          for (int i = 0; i < sz / 8; i++) {
            println("\t\tld t2, %d(s2)", i * 8);
            println("\t\tsd t2, %d(t1)", i * 8);
          }
        } else {
          if (nm == 1) {
            emit_normal_arg(ty->members->ty, &gpi, &fpi);
          } else if (nm == 2 && cf >= 1) {

            emit_normal_arg(ty->members->ty, &gpi, &fpi);
            println("\t\taddi t1, t1, %d", ty->members->next->offset);
            emit_normal_arg(ty->members->next->ty, &gpi, &fpi);
          } else {
            for (int i = 0; i < sz / 8; i++) {
              println("\t\tmv t2, %s", argreg[gpi++]);
              println("\t\tsd t2, %d(t1)", i * 8);
            }
          }
        }

        continue;
      }

      emit_normal_arg(var->ty, &gpi, &fpi);
    }

    gen_stmt(fn->body);
    assert(depth == 0);

    println(".L.return.%s:", fn->name);
    println("\t\tmv a0, s1");

    if (fn->ty->return_ty->kind == TY_FLOAT) {
      println("\t\taddi sp, sp, -8");
      println("\t\tfsw fs1, 0(sp)");
      println("\t\tflw fa0, 0(sp)");
      println("\t\taddi sp, sp, 8");

    } else if (fn->ty->return_ty->kind == TY_DOUBLE) {
      println("\t\taddi sp, sp, -8");
      println("\t\tfsd fs1, 0(sp)");
      println("\t\tfld fa0, 0(sp)");
      println("\t\taddi sp, sp, 8");
    }

    println("\t\tli t1, %d", fn->stack_size);
    println("\t\tadd sp, sp, t1");
    println("\t\tld ra, -16(sp)");
    println("\t\tld fp, -24(sp)");
    println("\t\tjr ra");
  }
}

void codegen(Obj *prog, FILE *out) {
  ConstVal cval = {};
  cur_const_val = &cval;
  output_file = out;

  File **files = get_input_files();
  for (int i = 0; files[i]; i++) {
    println("  .file %d \"%s\"", files[i]->file_no, files[i]->name);
  }

  assign_lvar_offsets(prog);
  emit_data(prog);
  emit_text(prog);
  emit_constval(&cval);
}