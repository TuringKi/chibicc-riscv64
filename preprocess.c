#include "chibicc.h"

typedef struct Macro Macro;
struct Macro {
  Macro *next;
  char *name;
  Token *body;
  bool deleted;
};

typedef struct CondIncl CondIncl;
struct CondIncl {
  CondIncl *next;
  enum { IN_THEN, IN_ELIF, IN_ELSE } ctx;
  Token *tok;
  bool included;
};

typedef struct Hideset Hideset;
struct Hideset {
  Hideset *next;
  char *name;
};

static Macro *macros;
static CondIncl *cond_incl;

static Token *preprocess2(Token *tok);
static bool is_hash(Token *tok) { return tok->at_bol && equal(tok, "#"); }

static Token *copy_token(Token *tok) {
  Token *t = calloc(1, sizeof(Token));
  *t = *tok;
  t->next = NULL;
  return t;
}

static Token *new_eof(Token *tok) {
  Token *t = copy_token(tok);
  t->kind = TK_EOF;
  t->len = 0;
  return t;
}

static Token *skip_line(Token *tok) {
  if (tok->at_bol)
    return tok;
  warn_tok(tok, "extra token");
  while (tok->at_bol) {
    tok = tok->next;
  }
  return tok;
}

static Hideset *new_hideset(char *name) {
  Hideset *hs = calloc(1, sizeof(Hideset));
  hs->name = name;
  return hs;
}

static Hideset *hideset_union(Hideset *hs1, Hideset *hs2) {
  Hideset head = {};
  Hideset *cur = &head;

  for (; hs1; hs1 = hs1->next)
    cur = cur->next = new_hideset(hs1->name);
  cur->next = hs2;
  return head.next;
}

static bool hideset_contains(Hideset *hs, char *s, int len) {
  for (; hs; hs = hs->next)
    if (strlen(hs->name) == len && !strncmp(hs->name, s, len))
      return true;
  return false;
}

static Token *add_hideset(Token *tok, Hideset *hs) {
  Token head = {};
  Token *cur = &head;

  for (; tok; tok = tok->next) {
    Token *t = copy_token(tok);
    t->hideset = hideset_union(t->hideset, hs);
    cur = cur->next = t;
  }
  return head.next;
}

static Token *append(Token *tok1, Token *tok2) {
  if (tok1->kind == TK_EOF) {
    return tok2;
  }

  Token head = {};
  Token *cur = &head;

  for (; tok1->kind != TK_EOF; tok1 = tok1->next) {
    cur = cur->next = copy_token(tok1);
  }
  cur->next = tok2;
  return head.next;
}

static Token *skip_cond_incl2(Token *tok) {
  while (tok->kind != TK_EOF) {
    if (is_hash(tok) && (equal(tok->next, "if") || equal(tok->next, "ifdef") ||
                         equal(tok->next, "ifndef"))) {
      tok = skip_cond_incl2(tok->next->next);
      continue;
    }
    if (is_hash(tok) && equal(tok->next, "endif"))
      return tok->next->next;
    tok = tok->next;
  }
  return tok;
}

// Skip until next `#else` or `#endif`.
// Nested `#if` and `#endif` are skipped.
static Token *skip_cond_incl(Token *tok) {
  while (tok->kind != TK_EOF) {
    if (is_hash(tok) && (equal(tok->next, "if") || equal(tok->next, "ifdef") ||
                         equal(tok->next, "ifndef"))) {
      tok = skip_cond_incl2(tok->next->next);
      continue;
    }

    if (is_hash(tok) &&
        (equal(tok->next, "else") || equal(tok->next, "endif") ||
         equal(tok->next, "elif"))) {
      break;
    }
    tok = tok->next;
  }
  return tok;
}

static Token *copy_line(Token **rest, Token *tok) {
  Token head = {};
  Token *cur = &head;

  for (; !tok->at_bol; tok = tok->next) {
    cur = cur->next = copy_token(tok);
  }

  cur->next = new_eof(tok);
  *rest = tok;
  return head.next;
}

// Read and evaluate a constant expression.
static long eval_const_expr(Token **rest, Token *tok) {
  Token *start = tok;
  Token *expr = copy_line(rest, tok->next);
  expr = preprocess2(expr);
  if (expr->kind == TK_EOF) {
    error_tok(start, "no expression");
  }

  Token *rest2;
  long val = const_expr(&rest2, expr);
  if (rest2->kind != TK_EOF) {
    error_tok(rest2, "extra token");
  }
  return val;
}

static CondIncl *push_cond_incl(Token *tok, bool included) {
  CondIncl *ci = calloc(1, sizeof(CondIncl));
  ci->next = cond_incl;
  ci->tok = tok;
  ci->ctx = IN_THEN;
  cond_incl = ci;
  ci->included = included;
  return ci;
}

static Macro *find_macro(Token *tok) {
  if (tok->kind != TK_IDENT) {
    return NULL;
  }

  for (Macro *m = macros; m; m = m->next) {
    if (strlen(m->name) == tok->len && !strncmp(m->name, tok->loc, tok->len)) {
      return m->deleted ? NULL : m;
    }
  }

  return NULL;
}

static Macro *add_macro(char *name, Token *body) {
  Macro *m = calloc(1, sizeof(Macro));
  m->next = macros;
  m->name = name;
  m->body = body;
  macros = m;
  return m;
}

static bool expand_macro(Token **rest, Token *tok) {

  if (hideset_contains(tok->hideset, tok->loc, tok->len))
    return false;

  Macro *m = find_macro(tok);
  if (!m) {
    return false;
  }
  Hideset *hs = hideset_union(tok->hideset, new_hideset(m->name));
  Token *body = add_hideset(m->body, hs);
  *rest = append(body, tok->next);
  return true;
}

static Token *preprocess2(Token *tok) {
  Token head = {};
  Token *cur = &head;

  while (tok->kind != TK_EOF) {
    if (expand_macro(&tok, tok)) {
      continue;
    }
    if (!is_hash(tok)) {
      cur = cur->next = tok;
      tok = tok->next;
      continue;
    }
    Token *start = tok;
    tok = tok->next;

    if (equal(tok, "include")) {
      tok = tok->next;

      if (tok->kind != TK_STR) {
        error_tok(tok, "expected a filename");
      }

      char *path;
      if (tok->str[0] == '/') {
        path = tok->str;

      } else {
        path = format("%s/%s", dirname(strdup(tok->file->name)), tok->str);
      }
      Token *tok2 = tokenize_file(path);
      if (!tok2) {
        error_tok(tok, "%s", strerror(errno));
      }
      tok = skip_line(tok->next);
      tok = append(tok2, tok->next);
      continue;
    }

    if (equal(tok, "define")) {
      tok = tok->next;
      if (tok->kind != TK_IDENT) {
        error_tok(tok, "macro name must be an identifier");
      }
      char *name = strndup(tok->loc, tok->len);
      add_macro(name, copy_line(&tok, tok->next));
      continue;
    }

    if (equal(tok, "undef")) {
      tok = tok->next;
      if (tok->kind != TK_IDENT) {
        error_tok(tok, "macro name must be an identifier");
      }
      char *name = strndup(tok->loc, tok->len);
      tok = skip_line(tok->next);

      Macro *m = add_macro(name, NULL);
      m->deleted = true;
      continue;
    }

    if (equal(tok, "if")) {
      long val = eval_const_expr(&tok, tok);
      push_cond_incl(start, val);
      if (!val) {
        tok = skip_cond_incl(tok);
      }
      continue;
    }

    if (equal(tok, "ifdef")) {
      bool defined = find_macro(tok->next);
      push_cond_incl(tok, defined);
      tok = skip_line(tok->next->next);
      if (!defined)
        tok = skip_cond_incl(tok);
      continue;
    }

    if (equal(tok, "ifndef")) {
      bool defined = find_macro(tok->next);
      push_cond_incl(tok, !defined);
      tok = skip_line(tok->next->next);
      if (defined)
        tok = skip_cond_incl(tok);
      continue;
    }

    if (equal(tok, "else")) {
      if (!cond_incl || cond_incl->ctx == IN_ELSE) {
        error_tok(start, "stray #else");
      }
      cond_incl->ctx = IN_ELSE;
      tok = skip_line(tok->next);

      if (cond_incl->included) {
        tok = skip_cond_incl(tok);
      }
      continue;
    }

    if (equal(tok, "elif")) {
      if (!cond_incl || cond_incl->ctx == IN_ELSE)
        error_tok(start, "stray #elif");
      cond_incl->ctx = IN_ELIF;

      if (!cond_incl->included && eval_const_expr(&tok, tok)) {
        cond_incl->included = true;

      } else {
        tok = skip_cond_incl(tok);
      }
      continue;
    }

    if (equal(tok, "endif")) {
      if (!cond_incl) {
        error_tok(start, "stray #endif");
      }
      cond_incl = cond_incl->next;
      tok = skip_line(tok->next);
      continue;
    }

    if (tok->at_bol) {
      continue;
    }

    error_tok(tok, "invalid preprocessor directive");
  }

  cur->next = tok;
  return head.next;
}

// Entry point function of the preprocessor.
Token *preprocess(Token *tok) {
  tok = preprocess2(tok);
  if (cond_incl)
    error_tok(cond_incl->tok, "unterminated conditional directive");
  convert_keywords(tok);
  return tok;
}
