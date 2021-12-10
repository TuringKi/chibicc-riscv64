#include "chibicc.h"

int main(int argc, char **argv) {
  // stderr 是标准错误文件，它负责收集错误信息。一个经常使用到的方式为：./a.out
  // 2>error.log 或者 ./a.out 2>/dev/null 其中 2
  // 是标准错误输出的文件描述标号。前者将错误信息输出到error.log文件，后者将错误信息输出到
  // /dev/null，这是 linux 操作系统的空设备，输出到此设备意味着什么也不输出。
  if (argc != 2) {
    fprintf(stderr, "%s, invalid number of arguments\n", argv[0]);
  }
  Token *tok = tokenize(argv[1]);

  Function *prog = parse(tok);

  codegen(prog);

  return 0;
}