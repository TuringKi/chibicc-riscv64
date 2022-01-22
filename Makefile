CFLAGS=-std=c11 -g -fno-common
GCC=riscv64-unknown-linux-gnu-gcc
QEMU=/data/usr/bin/qemu-riscv64
QEMU_RUN=$(QEMU) -L /data/usr/sysroot/ 

SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)
OBJS_S=$(SRCS:.c=.s)

TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)


#Stage 1:
riscv64-chibicc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(OBJS): chibicc.h



test/macro.exe: riscv64-chibicc test/macro.c
	./riscv64-chibicc -o test/macro.s test/macro.c
	$(GCC) -o $@ test/macro.s -xc test/common

test/%.exe: riscv64-chibicc test/%.c
	$(GCC) -o- -E -P -C test/$*.c | ./riscv64-chibicc -o test/$*.s -
	$(GCC) -o $@ test/$*.s -xc test/common

test: $(TESTS)
	for i in $^; do echo $$i; qemu-riscv64 -L /data/usr/sysroot/ ./$$i || exit 1; echo; done
	test/driver.sh ./riscv64-chibicc



test-all: test test-stage2

# Stage 2

stage2/riscv64-chibicc: $(OBJS_S:%=stage2/%)
	$(GCC) $(CFLAGS) -o  $@ -g $^
#$(GCC) $(CFLAGS) -o stage2/$*.o  -g -c stage2/$*.s
stage2/%.s: riscv64-chibicc self.py %.c
	mkdir -p stage2/test
	./self.py chibicc.h $*.c > stage2/$*.c
	./riscv64-chibicc -o stage2/$*.s stage2/$*.c




stage2/test/macro.exe: stage2/riscv64-chibicc test/macro.c
	mkdir -p stage2/test
	$(QEMU_RUN) ./stage2/riscv64-chibicc -o stage2/test/macro.s test/macro.c
	$(GCC) -o $@ stage2/test/macro.s -xc test/common

stage2/test/%.exe: stage2/riscv64-chibicc test/%.c
	mkdir -p stage2/test
	$(GCC) -o- -E -P -C test/$*.c | $(QEMU_RUN) ./stage2/riscv64-chibicc -o stage2/test/$*.s -
	$(GCC) -o $@ stage2/test/$*.s -xc test/common

test-stage2: $(TESTS:test/%=stage2/test/%)
	for i in $^; do echo $$i; qemu-riscv64 -L /data/usr/sysroot/ ./$$i || exit 1; echo; done
	test/driver.sh "qemu-riscv64  -L /data/usr/sysroot ./stage2/riscv64-chibicc"



clean:
	rm -rf riscv64-chibicc *.o *~ tmp* *.s test/*.s test/*.exe stage2
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

.PHONY: test clean test-stage2
