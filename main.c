#include "chibicc.h"

char *base_file;

static char *opt_o;
static bool opt_E;

static char *input_path;

static void usage(int status) {
  fprintf(stderr, "chibicc [ -o <path> ] <file>\n");
  exit(status);
}

static void parse_args(int argc, char **argv) {
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--help"))
      usage(0);

    if (!strcmp(argv[i], "-o")) {
      if (!argv[++i])
        usage(1);
      opt_o = argv[i];
      continue;
    }

    if (!strncmp(argv[i], "-o", 2)) {
      opt_o = argv[i] + 2;
      continue;
    }

    if (!strcmp(argv[i], "-E")) {
      opt_E = true;
      continue;
    }

    if (argv[i][0] == '-' && argv[i][1] != '\0')
      error("unknown argument: %s", argv[i]);

    input_path = argv[i];
  }

  if (!input_path)
    error("no input files");
}

static FILE *open_file(char *path) {
  if (!path || strcmp(path, "-") == 0)
    return stdout;

  FILE *out = fopen(path, "w");
  if (!out)
    error("cannot open output file: %s: %s", path, strerror(errno));
  return out;
}

static void print_tokens(Token *tok) {
  FILE *out = open_file(opt_o ? opt_o : "-");

  int line = 1;
  for (; tok->kind != TK_EOF; tok = tok->next) {
    if (line > 1 && tok->at_bol) {
      fprintf(out, "\n");
    }
    fprintf(out, " %.*s", tok->len, tok->loc);
    line++;
  }
  fprintf(out, "\n");
}

int main(int argc, char **argv) {
  parse_args(argc, argv);

  // Tokenize and parse.
  Token *tok = tokenize_file(input_path);
  if (!tok) {
    error("%s: %s", base_file, strerror(errno));
  }
  tok = preprocess(tok);
  if (opt_E) {
    print_tokens(tok);
    return 0;
  }
  Obj *prog = parse(tok);

  // Traverse the AST to emit assembly.
  FILE *out = open_file(opt_o);
  codegen(prog, out);
  return 0;
}
