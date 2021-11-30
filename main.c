#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
  // stderr 是标准错误文件，它负责收集错误信息。一个经常使用到的方式为：./a.out
  // 2>error.log 或者 ./a.out 2>/dev/null 其中 2
  // 是标准错误输出的文件描述标号。前者将错误信息输出到error.log文件，后者将错误信息输出到
  // /dev/null，这是 linux 操作系统的空设备，输出到此设备意味着什么也不输出。
  if (argc != 2) {
    fprintf(stderr, "%s, invalid number of arguments\n", argv[0]);
  }

  char *p = argv[1];

  printf("\t.globl main\n");
  printf("main:\n");
  // TODO:这里我们要考虑相加的数是否超过了立即数的位数。按照RISC-V的标准，I-Type
  //指令的立即数只能有12位。
  printf("\t\taddi a0, zero, %ld\n", strtol(p, &p, 10));

  while (*p) {
    if (*p == '+') {
      p++;
      printf("\t\taddi a0, a0, %ld\n", strtol(p, &p, 10));
      continue;
    }

    if (*p == '-') {
      p++;
      printf("\t\taddi t0, zero, %ld\n", strtol(p, &p, 10));
      printf("\t\tsub a0, a0, t0\n");
      continue;
    }

    fprintf(stderr, "unexpected character: '%c'\n", *p);
    return -1;
  }
  printf("\t\tret\n");
  return 0;
}