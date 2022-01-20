#include "chibicc.h"

static char *opt_o;

static char *input_path;

static void usage(char *binname, int status) {
  fprintf(stderr, "%s [ -o <path> ] <file>\n", binname);
  exit(status);
}

static void parse_args(int argc, char **argv) {
  if (argc == 0) {
    exit(-1);
  }
  char *binname = argv[0];
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--help")) {
      usage(binname, 0);
    }

    if (!strcmp(argv[i], "-o")) {
      if (!argv[++i]) {
        usage(binname, 1);
      }
      opt_o = argv[i];
      continue;
    }

    if (!strncmp(argv[i], "-o", 2)) {
      opt_o = argv[i] + 2;
      continue;
    }

    if (argv[i][0] == '-' && argv[i][1] != '\0') {
      error("unknown argument: %s", argv[i]);
    }

    input_path = argv[i];
  }

  if (!input_path) {
    error("no input files");
  }
}

static FILE *open_file(char *path) {
  if (!path || strcmp(path, "-") == 0) {
    return stdout;
  }

  FILE *out = fopen(path, "w");
  if (!out) {
    error("cannot open output: %s: %s", path, strerror(errno));
  }
  return out;
}

int main(int argc, char **argv) {

  parse_args(argc, argv);

  Token *tok = tokenize_file(input_path);
  Obj *prog = parse(tok);

  FILE *outfile = open_file(opt_o);
  fprintf(outfile, ".file 1 \"%s\"\n", input_path);

  codegen(prog, outfile);
  return 0;
}