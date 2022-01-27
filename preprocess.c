#include "chibicc.h"

typedef struct MacroParam MacroParam;
struct MacroParam {
  MacroParam *next;
  char *name;
};

typedef struct MacroArg MacroArg;
struct MacroArg {
  MacroArg *next;
  char *name;
  Token *tok;
};

typedef Token *macro_handler_fn(Token *);

typedef struct Macro Macro;
struct Macro {
  Macro *next;
  char *name;
  Token *body;
  bool deleted;
  MacroParam *params;
  bool is_variadic;
  bool is_objlike;
  macro_handler_fn *handler;
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
static Macro *find_macro(Token *tok);
static bool is_hash(Token *tok) { return tok->at_bol && equal(tok, "#"); }

static bool file_exists(char *path) {
  struct stat st;
  return !stat(path, &st);
}

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

static Hideset *hideset_intersection(Hideset *hs1, Hideset *hs2) {
  Hideset head = {};
  Hideset *cur = &head;

  for (; hs1; hs1 = hs1->next) {
    if (hideset_contains(hs2, hs1->name, strlen(hs1->name))) {
      cur = cur->next = new_hideset(hs1->name);
    }
  }

  return head.next;
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

static char *quote_string(char *str) {
  int bufsize = 3;
  for (int i = 0; str[i]; i++) {
    if (str[i] == '\\' || str[i] == '"') {
      bufsize++;
    }
    bufsize++;
  }

  char *buf = calloc(1, bufsize);
  char *p = buf;
  *p++ = '"';
  for (int i = 0; str[i]; i++) {
    if (str[i] == '\\' || str[i] == '"') {
      *p++ = '\\';
    }
    *p++ = str[i];
  }
  *p++ = '"';
  *p++ = '\0';
  return buf;
}

static Token *new_str_token(char *str, Token *tmpl) {
  char *buf = quote_string(str);
  return tokenize(new_file(tmpl->file->name, tmpl->file->file_no, buf));
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

static Token *new_num_token(int val, Token *tmpl) {
  char *buf = format("%d\n", val);
  return tokenize(new_file(tmpl->file->name, tmpl->file->file_no, buf));
}

static Token *read_const_expr(Token **rest, Token *tok) {
  tok = copy_line(rest, tok);

  Token head = {};
  Token *cur = &head;

  while (tok->kind != TK_EOF) {

    if (equal(tok, "defined")) {
      Token *start = tok;
      bool has_paren = consume(&tok, tok->next, "(");

      if (tok->kind != TK_IDENT) {
        error_tok(start, "macro name must be an identifier");
      }
      Macro *m = find_macro(tok);
      tok = tok->next;

      if (has_paren) {
        tok = skip(tok, ")");
      }

      cur = cur->next = new_num_token(m ? 1 : 0, start);
      continue;
    }

    cur = cur->next = tok;
    tok = tok->next;
  }

  cur->next = tok;
  return head.next;
}

static long eval_const_expr(Token **rest, Token *tok) {
  Token *start = tok;
  Token *expr = read_const_expr(rest, tok->next);
  expr = preprocess2(expr);
  if (expr->kind == TK_EOF) {
    error_tok(start, "no expression");
  }

  for (Token *t = expr; t->kind != TK_EOF; t = t->next) {
    if (t->kind == TK_IDENT) {
      Token *next = t->next;
      *t = *new_num_token(0, t);
      t->next = next;
    }
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

static Macro *add_macro(char *name, bool is_objlike, Token *body) {
  Macro *m = calloc(1, sizeof(Macro));
  m->next = macros;
  m->name = name;
  m->is_objlike = is_objlike;
  m->body = body;
  macros = m;
  return m;
}

static MacroParam *read_macro_params(Token **rest, Token *tok,
                                     bool *is_variadic) {
  MacroParam head = {};
  MacroParam *cur = &head;

  while (!equal(tok, ")")) {
    if (cur != &head) {
      tok = skip(tok, ",");
    }

    if (equal(tok, "...")) {
      *is_variadic = true;
      *rest = skip(tok->next, ")");
      return head.next;
    }

    if (tok->kind != TK_IDENT) {
      error_tok(tok, "expected an identifier");
    }
    MacroParam *m = calloc(1, sizeof(MacroParam));
    m->name = strndup(tok->loc, tok->len);
    cur = cur->next = m;
    tok = tok->next;
  }
  *rest = tok->next;
  return head.next;
}

static void read_macro_definition(Token **rest, Token *tok) {
  if (tok->kind != TK_IDENT)
    error_tok(tok, "macro name must be an identifier");
  char *name = strndup(tok->loc, tok->len);
  tok = tok->next;

  if (!tok->has_space && equal(tok, "(")) {
    bool is_variadic = false;
    MacroParam *params = read_macro_params(&tok, tok->next, &is_variadic);
    Macro *m = add_macro(name, false, copy_line(rest, tok));
    m->params = params;
    m->is_variadic = is_variadic;
  } else {
    add_macro(name, true, copy_line(rest, tok));
  }
}

static MacroArg *read_macro_arg_one(Token **rest, Token *tok, bool read_rest) {
  Token head = {};
  Token *cur = &head;
  int level = 0;

  for (;;) {
    if (level == 0 && equal(tok, ")")) {
      break;
    }
    if (level == 0 && !read_rest && equal(tok, ",")) {
      break;
    }
    if (tok->kind == TK_EOF) {
      error_tok(tok, "premature end of input");
    }
    if (equal(tok, "(")) {
      level++;

    } else if (equal(tok, ")")) {
      level--;
    }

    cur = cur->next = copy_token(tok);
    tok = tok->next;
  }

  cur->next = new_eof(tok);

  MacroArg *arg = calloc(1, sizeof(MacroArg));
  arg->tok = head.next;
  *rest = tok;
  return arg;
}

static char *search_include_paths(char *filename) {
  if (filename[0] == '/')
    return filename;

  for (int i = 0; i < include_paths.len; i++) {
    char *path = format("%s/%s", include_paths.data[i], filename);
    if (file_exists(path))
      return path;
  }
  return NULL;
}

static MacroArg *read_macro_args(Token **rest, Token *tok, MacroParam *params,
                                 bool is_variadic) {
  Token *start = tok;
  tok = tok->next->next;

  MacroArg head = {};
  MacroArg *cur = &head;

  MacroParam *pp = params;
  for (; pp; pp = pp->next) {
    if (cur != &head) {
      tok = skip(tok, ",");
    }
    cur = cur->next = read_macro_arg_one(&tok, tok, false);
    cur->name = pp->name;
  }
  if (is_variadic) {
    MacroArg *arg;
    if (equal(tok, ")")) {
      arg = calloc(1, sizeof(MacroArg));
      arg->tok = new_eof(tok);
    } else {
      if (pp != params) {
        tok = skip(tok, ",");
      }
      arg = read_macro_arg_one(&tok, tok, true);
    }
    arg->name = "__VA_ARGS__";
    cur = cur->next = arg;
  } else if (pp) {
    error_tok(start, "too many arguments");
  }
  skip(tok, ")");
  *rest = tok;
  return head.next;
}

static MacroArg *find_arg(MacroArg *args, Token *tok) {
  for (MacroArg *ap = args; ap; ap = ap->next)
    if (tok->len == strlen(ap->name) && !strncmp(tok->loc, ap->name, tok->len))
      return ap;
  return NULL;
}

static char *join_tokens(Token *tok, Token *end) {
  int len = 1;
  for (Token *t = tok; t != end && t->kind != TK_EOF; t = t->next) {
    if (t != tok && t->has_space) {
      len++;
    }
    len += t->len;
  }

  char *buf = calloc(1, len);

  int pos = 0;
  for (Token *t = tok; t != end && t->kind != TK_EOF; t = t->next) {
    if (t != tok && t->has_space) {
      buf[pos++] = ' ';
    }
    strncpy(buf + pos, t->loc, t->len);
    pos += t->len;
  }
  buf[pos] = '\0';
  return buf;
}

static Token *stringize(Token *hash, Token *arg) {
  char *s = join_tokens(arg, NULL);
  return new_str_token(s, hash);
}

static Token *paste(Token *lhs, Token *rhs) {
  char *buf = format("%.*s%.*s", lhs->len, lhs->loc, rhs->len, rhs->loc);
  Token *tok = tokenize(new_file(lhs->file->name, lhs->file->file_no, buf));
  if (tok->next->kind != TK_EOF) {
    error_tok(lhs, "pasting forms '%s', an invalid token", buf);
  }
  return tok;
}

static Token *subst(Token *tok, MacroArg *args) {
  Token head = {};
  Token *cur = &head;

  while (tok->kind != TK_EOF) {

    if (equal(tok, "#")) {
      MacroArg *arg = find_arg(args, tok->next);
      if (!arg) {
        error_tok(tok->next, "'#' is not followed by a macro parameter");
      }
      cur = cur->next = stringize(tok, arg->tok);
      tok = tok->next->next;
      continue;
    }

    if (equal(tok, "##")) {
      if (cur == &head) {
        error_tok(tok, "'##' cannot appear at start of macro expansion");
      }

      if (tok->next->kind == TK_EOF) {
        error_tok(tok, "'##' cannot appear at end of macro expansion");
      }

      MacroArg *arg = find_arg(args, tok->next);
      if (arg) {
        if (arg->tok->kind != TK_EOF) {
          *cur = *paste(cur, arg->tok);
          for (Token *t = arg->tok->next; t->kind != TK_EOF; t = t->next)
            cur = cur->next = copy_token(t);
        }
        tok = tok->next->next;
        continue;
      }

      *cur = *paste(cur, tok->next);
      tok = tok->next->next;
      continue;
    }

    MacroArg *arg = find_arg(args, tok);

    if (arg && equal(tok->next, "##")) {
      Token *rhs = tok->next->next;

      if (arg->tok->kind == TK_EOF) {
        MacroArg *arg2 = find_arg(args, rhs);
        if (arg2) {
          for (Token *t = arg2->tok; t->kind != TK_EOF; t = t->next)
            cur = cur->next = copy_token(t);
        } else {
          cur = cur->next = copy_token(rhs);
        }
        tok = rhs->next;
        continue;
      }

      for (Token *t = arg->tok; t->kind != TK_EOF; t = t->next)
        cur = cur->next = copy_token(t);
      tok = tok->next;
      continue;
    }

    if (arg) {
      Token *t = preprocess2(arg->tok);
      t->at_bol = tok->at_bol;
      t->has_space = tok->has_space;
      for (; t->kind != TK_EOF; t = t->next)
        cur = cur->next = copy_token(t);
      tok = tok->next;
      continue;
    }

    cur = cur->next = copy_token(tok);
    tok = tok->next;
    continue;
  }

  cur->next = tok;
  return head.next;
}

static bool expand_macro(Token **rest, Token *tok) {

  if (hideset_contains(tok->hideset, tok->loc, tok->len))
    return false;

  Macro *m = find_macro(tok);
  if (!m) {
    return false;
  }

  if (m->handler) {
    *rest = m->handler(tok);
    (*rest)->next = tok->next;
    return true;
  }

  if (m->is_objlike) {
    Hideset *hs = hideset_union(tok->hideset, new_hideset(m->name));
    Token *body = add_hideset(m->body, hs);
    for (Token *t = body; t->kind != TK_EOF; t = t->next) {
      t->origin = tok;
    }
    *rest = append(body, tok->next);
    (*rest)->at_bol = tok->at_bol;
    (*rest)->has_space = tok->has_space;
    return true;
  }

  if (!equal(tok->next, "("))
    return false;

  Token *macro_token = tok;
  MacroArg *args = read_macro_args(&tok, tok, m->params, m->is_variadic);
  Token *rparen = tok;

  Hideset *hs = hideset_intersection(macro_token->hideset, rparen->hideset);
  hs = hideset_union(hs, new_hideset(m->name));

  Token *body = subst(m->body, args);
  body = add_hideset(body, hs);
  for (Token *t = body; t->kind != TK_EOF; t = t->next) {
    t->origin = macro_token;
  }
  *rest = append(body, tok->next);
  (*rest)->at_bol = macro_token->at_bol;
  (*rest)->has_space = macro_token->has_space;
  return true;
}

static char *read_include_filename(Token **rest, Token *tok, bool *is_dquote) {
  // Pattern 1: #include "foo.h"
  if (tok->kind == TK_STR) {

    *is_dquote = true;
    *rest = skip_line(tok->next);
    return strndup(tok->loc + 1, tok->len - 2);
  }

  // Pattern 2: #include <foo.h>
  if (equal(tok, "<")) {

    Token *start = tok;

    for (; !equal(tok, ">"); tok = tok->next)
      if (tok->at_bol || tok->kind == TK_EOF)
        error_tok(tok, "expected '>'");

    *is_dquote = false;
    *rest = skip_line(tok->next);
    return join_tokens(start->next, tok);
  }

  // Pattern 3: #include FOO

  if (tok->kind == TK_IDENT) {
    Token *tok2 = preprocess2(copy_line(rest, tok));
    return read_include_filename(&tok2, tok2, is_dquote);
  }

  error_tok(tok, "expected a filename");
}

static Token *include_file(Token *tok, char *path, Token *filename_tok) {
  Token *tok2 = tokenize_file(path);
  if (!tok2) {
    error_tok(filename_tok, "%s: cannot open file: %s", path, strerror(errno));
  }
  return append(tok2, tok);
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
      bool is_dquote;
      char *filename = read_include_filename(&tok, tok->next, &is_dquote);

      if (filename[0] != '/' && is_dquote) {
        char *path =
            format("%s/%s", dirname(strdup(start->file->name)), filename);
        if (file_exists(path)) {
          tok = include_file(tok, path, start->next->next);
          continue;
        }
      }

      char *path = search_include_paths(filename);
      tok = include_file(tok, path ? path : filename, start->next->next);

      continue;
    }

    if (equal(tok, "define")) {
      read_macro_definition(&tok, tok->next);
      continue;
    }

    if (equal(tok, "undef")) {
      tok = tok->next;
      if (tok->kind != TK_IDENT) {
        error_tok(tok, "macro name must be an identifier");
      }
      char *name = strndup(tok->loc, tok->len);
      tok = skip_line(tok->next);

      Macro *m = add_macro(name, true, NULL);
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

    if (equal(tok, "error")) {
      error_tok(tok, "error");
    }

    if (tok->at_bol) {
      continue;
    }

    error_tok(tok, "invalid preprocessor directive");
  }

  cur->next = tok;
  return head.next;
}

static void define_macro(char *name, char *buf) {
  Token *tok = tokenize(new_file("<built-in>", 1, buf));
  add_macro(name, true, tok);
}

static Macro *add_builtin(char *name, macro_handler_fn *fn) {
  Macro *m = add_macro(name, true, NULL);
  m->handler = fn;
  return m;
}

static Token *file_macro(Token *tmpl) {
  while (tmpl->origin) {
    tmpl = tmpl->origin;
  }
  return new_str_token(tmpl->file->name, tmpl);
}

static Token *line_macro(Token *tmpl) {
  while (tmpl->origin) {
    tmpl = tmpl->origin;
  }
  return new_num_token(tmpl->line_no, tmpl);
}

static void init_macros(void) {
  // Define predefined macros
  define_macro("_LP64", "1");
  define_macro("__C99_MACRO_WITH_VA_ARGS", "1");
  define_macro("__ELF__", "1");
  define_macro("__LP64__", "1");
  define_macro("__SIZEOF_DOUBLE__", "8");
  define_macro("__SIZEOF_FLOAT__", "4");
  define_macro("__SIZEOF_INT__", "4");
  define_macro("__SIZEOF_LONG_DOUBLE__", "8");
  define_macro("__SIZEOF_LONG_LONG__", "8");
  define_macro("__SIZEOF_LONG__", "8");
  define_macro("__SIZEOF_POINTER__", "8");
  define_macro("__SIZEOF_PTRDIFF_T__", "8");
  define_macro("__SIZEOF_SHORT__", "2");
  define_macro("__SIZEOF_SIZE_T__", "8");
  define_macro("__SIZE_TYPE__", "unsigned long");
  define_macro("__STDC_HOSTED__", "1");
  define_macro("__STDC_NO_ATOMICS__", "1");
  define_macro("__STDC_NO_COMPLEX__", "1");
  define_macro("__STDC_NO_THREADS__", "1");
  define_macro("__STDC_NO_VLA__", "1");
  define_macro("__STDC_VERSION__", "201112L");
  define_macro("__STDC__", "1");
  define_macro("__USER_LABEL_PREFIX__", "");
  define_macro("__alignof__", "_Alignof");
  define_macro("__riscv64", "1");
  define_macro("__riscv64__", "1");
  define_macro("__chibicc__", "1");
  define_macro("__const__", "const");
  define_macro("__gnu_linux__", "1");
  define_macro("__inline__", "inline");
  define_macro("__linux", "1");
  define_macro("__linux__", "1");
  define_macro("__signed__", "signed");
  define_macro("__typeof__", "typeof");
  define_macro("__unix", "1");
  define_macro("__unix__", "1");
  define_macro("__volatile__", "volatile");
  define_macro("linux", "1");
  define_macro("unix", "1");

  add_builtin("__FILE__", file_macro);
  add_builtin("__LINE__", line_macro);
}

static void join_adjacent_string_literals(Token *tok1) {
  while (tok1->kind != TK_EOF) {
    if (tok1->kind != TK_STR || tok1->next->kind != TK_STR) {
      tok1 = tok1->next;
      continue;
    }

    Token *tok2 = tok1->next;
    while (tok2->kind == TK_STR)
      tok2 = tok2->next;

    int len = tok1->ty->array_len;
    for (Token *t = tok1->next; t != tok2; t = t->next)
      len = len + t->ty->array_len - 1;

    char *buf = calloc(tok1->ty->base->size, len);

    int i = 0;
    for (Token *t = tok1; t != tok2; t = t->next) {
      memcpy(buf + i, t->str, t->ty->size);
      i = i + t->ty->size - t->ty->base->size;
    }

    *tok1 = *copy_token(tok1);
    tok1->ty = array_of(tok1->ty->base, len);
    tok1->str = buf;
    tok1->next = tok2;
    tok1 = tok2;
  }
}

Token *preprocess(Token *tok) {
  init_macros();

  tok = preprocess2(tok);
  if (cond_incl)
    error_tok(cond_incl->tok, "unterminated conditional directive");
  convert_keywords(tok);
  join_adjacent_string_literals(tok);
  return tok;
}
