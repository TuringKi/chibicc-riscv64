#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

typedef struct Obj Obj;
struct Obj {
  Obj *next;
  char *name;
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
  ND_RETURN,
  ND_BLOCK,
  ND_EXPR_STMT,
  ND_VAR,
  ND_NUM,
} NodeKind;

typedef struct Node Node;
struct Node {
  NodeKind kind;
  Node *next;
  Node *lhs;
  Node *rhs;
  Node *body;
  Obj *var;
  int val;
};

typedef struct Function Function;
struct Function {
  Node *body;
  Obj *locals;
  int stack_size;
};

void error(char *fmt, ...);
void verror_at(char *loc, char *fmt, va_list ap);
void error_at(char *loc, char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);

bool equal(Token *tok, char *op);
Token *skip(Token *tok, char *s);
Token *tokenize(char *input);

Function *parse(Token *tok);
void codegen(Function *node);