#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <libgen.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#define MAX(x, y) ((x) < (y) ? (y) : (x))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

typedef struct Type Type;
typedef struct Node Node;
typedef struct Member Member;
typedef struct Relocation Relocation;
typedef struct Hideset Hideset;

typedef struct {
  char **data;
  int capacity;
  int len;
} StringArray;

void strarray_push(StringArray *arr, char *s);

char *format(char *fmt, ...);

typedef enum {
  TK_IDENT,
  TK_PUNCT,
  TK_KEYWORD,
  TK_NUM,
  TK_STR,
  TK_EOF,
} TokenKind;

typedef struct {
  char *name;
  int file_no;
  char *contents;
} File;

typedef struct Token Token;
struct Token {
  bool at_bol;
  char *loc;
  char *str;
  double fval;
  File *file;
  Hideset *hideset;
  int len;
  int line_no;
  int64_t val;
  Token *next;
  TokenKind kind;
  Type *ty;
  bool has_space;
  Token *origin;
};

void error(char *fmt, ...);
void verror_at(char *filename, char *input, int line_no, char *loc, char *fmt,
               va_list ap);
void error_at(char *loc, char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);
void warn_tok(Token *tok, char *fmt, ...);

bool equal(Token *tok, char *op);
Token *skip(Token *tok, char *s);
bool consume(Token **rest, Token *tok, char *str);
void convert_keywords(Token *tok);
File **get_input_files(void);
File *new_file(char *name, int file_no, char *contents);
Token *tokenize(File *file);
Token *tokenize_file(char *input);

#define unreachable() error("internal error at %s:%d", __FILE__, __LINE__)

Token *preprocess(Token *tok);

typedef struct Obj Obj;
struct Obj {
  Obj *next;
  char *name;
  Type *ty;
  bool is_local;
  int align;
  bool is_definition;
  bool is_function;
  bool is_static;
  Obj *va_area;

  int offset;
  Obj *params;
  Node *body;
  Obj *locals;
  char *init_data;
  Relocation *rel;
  int stack_size;
  bool is_stack_param;
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
  ND_DO,
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
  bool pass_by_stack;
  char *brk_label;
  char *cont_label;
  char *label;
  char *unique_label;
  double fval;
  int param_stack_offset;
  int64_t val;
  Member *member;
  Node *args;
  Node *body;
  Node *case_next;
  Node *cond;
  Node *default_case;
  Node *els;
  Node *goto_next;
  Node *inc;
  Node *init;
  Node *lhs;
  Node *next;
  Node *rhs;
  Node *then;
  NodeKind kind;
  Obj *ret_buffer;
  Obj *var;
  Token *tok;
  Type *func_ty;
  Type *ty;
};

Node *new_cast(Node *expr, Type *ty);
int64_t const_expr(Token **rest, Token *tok);
Obj *parse(Token *tok);

typedef enum {
  TY_BOOL,
  TY_VOID,
  TY_CHAR,
  TY_INT,
  TY_SHORT,
  TY_LONG,
  TY_FLOAT,
  TY_DOUBLE,
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
  bool is_unsigned;

  Type *base;
  Token *name;
  Token *name_pos;
  int array_len;
  bool is_flexible;
  bool is_variadic;
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
  int align;
  bool pass_by_stack;
};
extern Type *ty_void;
extern Type *ty_bool;
extern Type *ty_int;
extern Type *ty_short;
extern Type *ty_char;
extern Type *ty_long;

extern Type *ty_uchar;
extern Type *ty_ushort;
extern Type *ty_uint;
extern Type *ty_ulong;

extern Type *ty_float;
extern Type *ty_double;

bool is_flonum(Type *ty);
bool is_integer(Type *ty);
bool is_numeric(Type *ty);
Type *pointer_to(Type *base);
Type *copy_type(Type *ty);
Type *struct_type(void);
void add_type(Node *node);
Type *array_of(Type *base, int size);
Type *func_type(Type *return_ty);
Type *enum_type(void);
void codegen(Obj *prog, FILE *out);
int align_to(int n, int align);

extern char *base_file;
extern StringArray include_paths;
