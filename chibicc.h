#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Type Type;
typedef struct Node Node;
typedef enum {
  TK_IDENT,
  TK_PUNCT,
  TK_KEYWORD,
  TK_NUM,
  TK_EOF,
} TokenKind;

typedef struct Token Token;
struct Token {
  TokenKind kind;
  Token *next;
  int val;
  char *loc;
  int len;
};

void error(char *fmt, ...);
void verror_at(char *loc, char *fmt, va_list ap);
void error_at(char *loc, char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);

bool equal(Token *tok, char *op);
Token *skip(Token *tok, char *s);
bool consume(Token **rest, Token *tok, char *str);
Token *tokenize(char *input);

typedef struct Obj Obj;
struct Obj {
  Obj *next;
  char *name;
  Type *ty;
  int offset;
};

typedef enum {
  ND_ADD,
  ND_SUB,
  ND_MUL,
  ND_DIV,
  ND_NEG,
  ND_EQ,
  ND_NE,
  ND_LT,
  ND_LE,
  ND_ASSIGN,
  ND_ADDR,
  ND_DEREF,
  ND_RETURN,
  ND_IF,
  ND_FOR,
  ND_BLOCK,
  ND_FUNCCAL,
  ND_EXPR_STMT,
  ND_VAR,
  ND_NUM,
} NodeKind;

struct Node {
  NodeKind kind;
  Node *next;
  Type *ty;
  Token *tok;

  Node *lhs;
  Node *rhs;

  Node *body;

  Node *cond;
  Node *then;
  Node *els;

  Node *init;
  Node *inc;

  Obj *var;
  int val;
  char *funcname;
  Node *args;
};

typedef struct Function Function;
struct Function {
  Node *body;
  Obj *locals;
  int stack_size;
};

Function *parse(Token *tok);

typedef enum {
  TY_INT,
  TY_PTR,
} TypeKind;

struct Type {
  TypeKind kind;
  Type *base;
  Token *name;
};

extern Type *ty_int;

bool is_integer(Type *ty);
Type *pointer_to(Type *base);
void add_type(Node *node);

void codegen(Function *node);