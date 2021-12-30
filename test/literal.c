#include "test.h"

int main() {
  ASSERT(97, 'a');
  ASSERT(10, '\n');
  ASSERT(121, '\x79');

  printf("OK\n");
  return 0;
}
