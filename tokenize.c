#include "chibicc.h"

static char *current_input;

void error(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(-1);
}

void verror_at(char *loc, char *fmt, va_list ap) {
  int pos = loc - current_input;
  if (pos < 0) {
    error("fatal error");
  }
  fprintf(stderr, "%s\n", current_input);
  fprintf(stderr, "%*s", pos, "");
  fprintf(stderr, "^ ");
  fprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(-1);
}

void error_at(char *loc, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(loc, fmt, ap);
}

void error_tok(Token *tok, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(tok->loc, fmt, ap);
}

bool equal(Token *tok, char *op) {
  return memcmp(tok->loc, op, tok->len) == 0 && op[tok->len] == '\0';
}

Token *skip(Token *tok, char *s) {
  if (!equal(tok, s)) {
    error_at(tok->loc, "expected '%s'", s);
  }
  return tok->next;
}

static int get_number(Token *tok) {
  if (tok->kind != TK_NUM) {
    error_tok(tok, "expected a number");
  }
  return tok->val;
}

static bool startwith(char *p, char *q) {
  return strncmp(p, q, strlen(q)) == 0;
}

static int read_punct(char *p) {
  if (startwith(p, "==") || startwith(p, ">=") || startwith(p, "!=") ||
      startwith(p, "<=")) {
    return 2;
  }

  return ispunct(*p) ? 1 : 0;
}

static Token *new_token(TokenKind kind, char *start, char *end) {
  Token *tok = calloc(1, sizeof(Token));
  if (tok == NULL) {
    error("creating token faild");
  }
  tok->kind = kind;
  tok->loc = start;
  tok->len = end - start;
  return tok;
}

// tokenize 函数将表达式解析为多个token。token的存储结构为链表。
Token *tokenize(char *p) {
  current_input = p;
  Token head = {};
  Token *cur = &head;

  while (*p) {
    if (isspace(*p)) {
      p++;
      continue;
    }
    if (isdigit(*p)) {
      //注意下方的表达式要从右往左读，即先将返回的结果存储到next,然后再覆盖当前链表元素的指针值。
      cur = cur->next = new_token(TK_NUM, p, p);
      char *q = p;
      cur->val = strtol(p, &p, 10);
      cur->len = p - q;
      continue;
    }

    int punck_len = read_punct(p);
    if (punck_len) {
      cur = cur->next = new_token(TK_PUNCT, p, p + punck_len);
      p += cur->len;
      continue;
    }

    error_at(p, "invalid token");
  }

  cur = cur->next = new_token(TK_EOF, p, p);
  return head.next;
}