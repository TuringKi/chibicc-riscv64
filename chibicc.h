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

typedef struct Type Type;
typedef struct Node Node;
typedef struct Member Member;

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
  int stack_size;
};

typedef enum {
  ND_ADD,
  ND_SUB,
  ND_MUL,
  ND_DIV,
  ND_NEG,
  ND_EQ,
  ND_NOT,
  ND_BITNOT,
  ND_BITAND,
  ND_BITOR,
  ND_BITXOR,
  ND_LOGAND,
  ND_LOGOR,
  ND_MOD,
  ND_NE,
  ND_LT,
  ND_LE,
  ND_ASSIGN,
  ND_MEMBER,
  ND_COMMA,
  ND_ADDR,
  ND_DEREF,
  ND_RETURN,
  ND_IF,
  ND_FOR,
  ND_BLOCK,
  ND_FUNCCAL,
  ND_EXPR_STMT,
  ND_STMT_EXPR,
  ND_VAR,
  ND_NUM,
  ND_CAST
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
void add_type(Node *node);
Type *array_of(Type *base, int size);
Type *func_type(Type *return_ty);
Type *enum_type(void);
void codegen(Obj *prog, FILE *out);
int align_to(int n, int align);
