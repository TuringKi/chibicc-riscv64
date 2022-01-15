#include "chibicc.h"
#include <ctype.h>

static char *current_input;
static char *current_filename;

static Token *new_token(TokenKind kind, char *start, char *end);

void error(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(-1);
}

void verror_at(int line_no, char *loc, char *fmt, va_list ap) {

  char *line = loc;
  while (current_input < line && line[-1] != '\n') {
    line--;
  }

  char *end = loc;
  while (*end != '\n') {
    end++;
  }

  int indent = fprintf(stderr, "%s:%d:", current_filename, line_no);
  fprintf(stderr, "%.*s\n", (int)(end - line), line);

  int pos = loc - line + indent;

  fprintf(stderr, "%*s", pos, "");
  fprintf(stderr, "^ ");
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
}

void error_at(char *loc, char *fmt, ...) {
  int line_no = 1;
  for (char *p = current_input; p < loc; p++) {
    if (*p == '\n') {
      line_no++;
    }
  }

  va_list ap;
  va_start(ap, fmt);
  verror_at(line_no, loc, fmt, ap);
  exit(-1);
}

void error_tok(Token *tok, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(tok->line_no, tok->loc, fmt, ap);
  exit(-1);
}

bool equal(Token *tok, char *op) {
  return memcmp(tok->loc, op, tok->len) == 0 && op[tok->len] == '\0';
}

bool consume(Token **rest, Token *tok, char *str) {
  if (equal(tok, str)) {
    *rest = tok->next;
    return true;
  }
  *rest = tok;
  return false;
}

Token *skip(Token *tok, char *s) {
  if (!equal(tok, s)) {
    error_tok(tok, "expected '%s'", s);
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

static bool is_ident1(char c) {
  return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
}

static bool is_ident2(char c) { return is_ident1(c) || ('0' <= c && c <= '9'); }

static int read_punct(char *p) {
  static char *kw[] = {"<<=", ">>=", "...", "==", "!=", "<=", ">=", "->",
                       "+=",  "-=",  "*=",  "/=", "++", "--", "%=", "&=",
                       "|=",  "^=",  "&&",  "||", "<<", ">>"};

  for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++) {
    if (startwith(p, kw[i])) {
      return strlen(kw[i]);
    }
  }

  return ispunct(*p) ? 1 : 0;
}

static bool is_keyword(Token *tok) {
  static char *kw[] = {
      "return",       "if",        "else",     "for",      "while",
      "int",          "sizeof",    "char",     "struct",   "union",
      "short",        "long",      "void",     "typedef",  "_Bool",
      "enum",         "static",    "goto",     "break",    "continue",
      "switch",       "case",      "default",  "extern",   "_Alignof",
      "_Alignas",     "do",        "signed",   "unsigned", "const",
      "volatile",     "auto",      "register", "restrict", "__restrict",
      "__restrict__", "_Noreturn", "float",    "double",
  };

  for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++) {
    if (equal(tok, kw[i])) {
      return true;
    }
  }
  return false;
}

static Token *read_int_literal(char *start) {
  char *p = start;

  int base = 10;
  if (!strncasecmp(p, "0x", 2) && isxdigit(p[2])) {
    p += 2;
    base = 16;
  } else if (!strncasecmp(p, "0b", 2) && (p[2] == '0' || p[2] == '1')) {
    p += 2;
    base = 2;
  } else if (*p == '0') {
    base = 8;
  }

  int64_t val = strtoul(p, &p, base);

  // Read U, L or LL suffixes.
  bool l = false;
  bool u = false;

  if (startwith(p, "LLU") || startwith(p, "LLu") || startwith(p, "llU") ||
      startwith(p, "llu") || startwith(p, "ULL") || startwith(p, "Ull") ||
      startwith(p, "uLL") || startwith(p, "ull")) {
    p += 3;
    l = u = true;
  } else if (!strncasecmp(p, "lu", 2) || !strncasecmp(p, "ul", 2)) {
    p += 2;
    l = u = true;
  } else if (startwith(p, "LL") || startwith(p, "ll")) {
    p += 2;
    l = true;
  } else if (*p == 'L' || *p == 'l') {
    p++;
    l = true;
  } else if (*p == 'U' || *p == 'u') {
    p++;
    u = true;
  }

  Type *ty;
  if (base == 10) {
    if (l && u)
      ty = ty_ulong;
    else if (l)
      ty = ty_long;
    else if (u)
      ty = (val >> 32) ? ty_ulong : ty_uint;
    else
      ty = (val >> 31) ? ty_long : ty_int;
  } else {
    if (l && u)
      ty = ty_ulong;
    else if (l)
      ty = (val >> 63) ? ty_ulong : ty_long;
    else if (u)
      ty = (val >> 32) ? ty_ulong : ty_uint;
    else if (val >> 63)
      ty = ty_ulong;
    else if (val >> 32)
      ty = ty_long;
    else if (val >> 31)
      ty = ty_uint;
    else
      ty = ty_int;
  }

  Token *tok = new_token(TK_NUM, start, p);
  tok->val = val;
  tok->ty = ty;
  return tok;
}

static Token *read_number(char *start) {

  Token *tok = read_int_literal(start);
  if (!strchr(".eEfF", start[tok->len])) {
    return tok;
  }

  char *end;
  double val = strtod(start, &end);

  Type *ty;
  if (*end == 'f' || *end == 'F') {
    ty = ty_float;
    end++;
  } else if (*end == 'l' || *end == 'L') {
    ty = ty_double;
    end++;
  } else {
    ty = ty_double;
  }

  tok = new_token(TK_NUM, start, end);
  tok->fval = val;
  tok->ty = ty;
  return tok;
}

static int convert_keywords(Token *tok) {
  for (Token *t = tok; t->kind != TK_EOF; t = t->next) {
    if (is_keyword(t)) {
      t->kind = TK_KEYWORD;
    }
  }
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

static int from_hex(char c) {
  if ('0' <= c && c <= '9')
    return c - '0';
  if ('a' <= c && c <= 'f')
    return c - 'a' + 10;
  return c - 'A' + 10;
}

static int read_escaped_char(char **new_pos, char *p) {

  if ('0' <= *p && *p <= '7') {
    // Read an octal number.
    int c = *p++ - '0';
    if ('0' <= *p && *p <= '7') {
      c = (c << 3) + (*p++ - '0');
      if ('0' <= *p && *p <= '7')
        c = (c << 3) + (*p++ - '0');
    }
    *new_pos = p;
    return c;
  }

  if (*p == 'x') {
    // Read a hexadecimal number.
    p++;
    if (!isxdigit(*p))
      error_at(p, "invalid hex escape sequence");

    int c = 0;
    for (; isxdigit(*p); p++)
      c = (c << 4) + from_hex(*p);
    *new_pos = p;
    return c;
  }

  *new_pos = p + 1;

  switch (*p) {
  case 'a':
    return '\a';
  case 'b':
    return '\b';
  case 't':
    return '\t';
  case 'n':
    return '\n';
  case 'v':
    return '\v';
  case 'f':
    return '\f';
  case 'r':
    return '\r';
  case 'e':
    return 27;
  default:
    return *p;
  }
}

static char *string_literal_end(char *p) {
  char *start = p;
  for (; *p != '"'; p++) {
    if (*p == '\n' || *p == '\0') {
      error_at(start, "unclosed string literal");
    }
    if (*p == '\\') {
      p++;
    }
  }
  return p;
}

static Token *read_char_literal(char *start) {
  char *p = start + 1;
  if (*p == '\0') {
    error_at(start, "unclosed char literal");
  }

  char c;
  if (*p == '\\') {
    c = read_escaped_char(&p, p + 1);

  } else {
    c = *p++;
  }

  char *end = strchr(p, '\'');
  if (!end) {
    error_at(p, "unclosed char literal");
  }

  Token *tok = new_token(TK_NUM, start, end + 1);
  tok->val = c;
  tok->ty = ty_int;
  return tok;
}

static Token *read_string_literal(char *start) {
  char *end = string_literal_end(start + 1);
  char *buf = calloc(1, end - start);
  int len = 0;
  for (char *p = start + 1; p < end;) {
    if (*p == '\\') {
      buf[len++] = read_escaped_char(&p, p + 1);
    } else {
      buf[len++] = *p++;
    }
  }
  Token *tok = new_token(TK_STR, start, end + 1);
  tok->ty = array_of(ty_char, len + 1);
  tok->str = buf;
  return tok;
}

// Initialize line info for all tokens.
static void add_line_numbers(Token *tok) {
  char *p = current_input;
  int n = 1;

  do {
    if (p == tok->loc) {
      tok->line_no = n;
      tok = tok->next;
    }
    if (*p == '\n')
      n++;
  } while (*p++);
}

// tokenize 函数将表达式解析为多个token。token的存储结构为链表。
Token *tokenize(char *filename, char *p) {
  current_filename = filename;
  current_input = p;
  Token head = {};
  Token *cur = &head;

  while (*p) {

    if (startwith(p, "//")) {
      p += 2;
      while (*p != '\n') {
        p++;
      }
      continue;
    }

    // Skip block comments.
    if (startwith(p, "/*")) {
      char *q = strstr(p + 2, "*/");
      if (!q) {
        error_at(p, "unclosed block comment");
      }
      p = q + 2;
      continue;
    }
    if (isspace(*p)) {
      p++;
      continue;
    }

    if (*p == '"') {
      cur = cur->next = read_string_literal(p);
      p += cur->len;
      continue;
    }

    if (*p == '\'') {
      cur = cur->next = read_char_literal(p);
      p += cur->len;
      continue;
    }

    if (isdigit(*p) || (*p == '.' && isdigit(p[1]))) {
      cur = cur->next = read_number(p);
      p += cur->len;
      continue;
    }

    if (is_ident1(*p)) {
      char *start = p;
      do {
        p++;
      } while (is_ident2(*p));
      cur = cur->next = new_token(TK_IDENT, start, p);
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
  add_line_numbers(head.next);
  convert_keywords(head.next);
  return head.next;
}

static char *read_file(char *path) {
  FILE *fp;

  if (strcmp(path, "-") == 0) {
    fp = stdin;
  } else {
    fp = fopen(path, "r");
    if (!fp) {
      error("cannot open: %s:%s", path, strerror(errno));
    }
  }

  char *buf;
  size_t buflen;
  FILE *out = open_memstream(&buf, &buflen);

  for (;;) {
    char buf2[4096];
    int n = fread(buf2, 1, sizeof(buf2), fp);
    if (n == 0) {
      break;
    }
    fwrite(buf2, 1, n, out);
  }

  if (fp != stdin) {
    fclose(fp);
  }

  fflush(out);
  if (buflen == 0 || buf[buflen - 1] != '\n') {
    fputc('\n', out);
  }
  fputc('\0', out);
  fclose(out);
  return buf;
}

Token *tokenize_file(char *path) { return tokenize(path, read_file(path)); }