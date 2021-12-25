CFLAGS=-std=c11 -g -fno-common
GCC=riscv64-unknown-linux-gnu-gcc
QEMU=qemu-riscv64

SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)


riscv64-chibicc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(OBJS): chibicc.h



test/%.exe: riscv64-chibicc test/%.c
	$(GCC) -o- -E -P -C test/$*.c | ./riscv64-chibicc -o test/$*.s -
	$(GCC) -o $@ test/$*.s -xc test/common

test: $(TESTS)
	for i in $^; do echo $$i; qemu-riscv64 -L /home/mx/usr/sysroot/ ./$$i || exit 1; echo; done
	test/driver.sh

clean:
	rm -f riscv64-chibicc *.o *~ tmp* *.s test/*.s test/*.exe
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

.PHONY: test clean