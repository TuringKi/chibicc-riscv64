#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#define MAX(x, y) ((x) < (y) ? (y) : (x))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

typedef struct Type Type;
typedef struct Node Node;
typedef struct Member Member;
typedef struct Relocation Relocation;

char *format(char *fmt, ...);

typedef enum {
  TK_IDENT,
  TK_PUNCT,
  TK_KEYWORD,
  TK_NUM,
  TK_STR,
  TK_EOF,
} TokenKind;

typedef struct Token Token;
struct Token {
  TokenKind kind;
  Token *next;
  int64_t val;
  char *loc;
  Type *ty;
  char *str;
  int line_no; // Line number
  int len;
};

void error(char *fmt, ...);
void verror_at(int line_no, char *loc, char *fmt, va_list ap);
void error_at(char *loc, char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);

bool equal(Token *tok, char *op);
Token *skip(Token *tok, char *s);
bool consume(Token **rest, Token *tok, char *str);
Token *tokenize_file(char *input);

#define unreachable() error("internal error at %s:%d", __FILE__, __LINE__)

typedef struct Obj Obj;
struct Obj {
  Obj *next;
  char *name;
  Type *ty;
  bool is_local;
  bool is_definition;
  bool is_function;
  bool is_static;
  int offset;
  Obj *params;
  Node *body;
  Obj *locals;
  char *init_data;
  Relocation *rel;
  int stack_size;
};

typedef struct Relocation Relocation;
struct Relocation {
  Relocation *next;
  int offset;
  char *label;
  long addend;
};

typedef enum {
  ND_NULL_EXPR,
  ND_ADD,
  ND_ADDR,
  ND_ASSIGN,
  ND_BITAND,
  ND_BITNOT,
  ND_BITOR,
  ND_BITXOR,
  ND_BLOCK,
  ND_CASE,
  ND_CAST,
  ND_COMMA,
  ND_DEREF,
  ND_DIV,
  ND_EQ,
  ND_SHL,
  ND_SHR,
  ND_COND,
  ND_EXPR_STMT,
  ND_FOR,
  ND_FUNCCAL,
  ND_GOTO,
  ND_IF,
  ND_LABEL,
  ND_LE,
  ND_LOGAND,
  ND_LOGOR,
  ND_LT,
  ND_MEMBER,
  ND_MOD,
  ND_MUL,
  ND_NE,
  ND_NEG,
  ND_NOT,
  ND_NUM,
  ND_RETURN,
  ND_STMT_EXPR,
  ND_SUB,
  ND_SWITCH,
  ND_VAR,
  ND_MEMZERO
} NodeKind;

struct Node {
  NodeKind kind;
  Node *next;
  Type *ty;
  Token *tok;

  Node *lhs;
  Node *rhs;

  Node *body;
  Member *member;

  Node *cond;
  Node *then;
  Node *els;

  Node *init;
  Node *inc;
  char *brk_label;
  char *cont_label;

  char *label;
  char *unique_label;
  Node *goto_next;

  Node *case_next;
  Node *default_case;

  Obj *var;
  int64_t val;
  Type *func_ty;
  char *funcname;
  Node *args;
};

Node *new_cast(Node *expr, Type *ty);
Obj *parse(Token *tok);

typedef enum {
  TY_BOOL,
  TY_VOID,
  TY_CHAR,
  TY_INT,
  TY_SHORT,
  TY_LONG,
  TY_PTR,
  TY_STRUCT,
  TY_ENUM,
  TY_ARRAY,
  TY_FUNC,
  TY_UNION,
} TypeKind;

struct Type {
  TypeKind kind;
  int size;
  int align;

  Type *base;
  Token *name;
  int array_len;
  Member *members;
  Type *return_ty;
  Type *params;
  Type *next;
};

struct Member {
  Member *next;
  Type *ty;
  Token *name;
  int idx;
  int offset;
};
extern Type *ty_void;
extern Type *ty_bool;
extern Type *ty_int;
extern Type *ty_short;
extern Type *ty_char;
extern Type *ty_long;

bool is_integer(Type *ty);
Type *pointer_to(Type *base);
Type *copy_type(Type *ty);
Type *struct_type(void);
void add_type(Node *node);
Type *array_of(Type *base, int size);
Type *func_type(Type *return_ty);
Type *enum_type(void);
void codegen(Obj *prog, FILE *out);
int align_to(int n, int align);
