CFLAGS=-std=c11 -g -fno-common
GCC=riscv64-unknown-linux-gnu-gcc
QEMU=/data/usr/bin/qemu-riscv64
SYSROOT=/data/usr/sysroot/
QEMU_RUN=$(QEMU) -L $(SYSROOT) 

SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)
OBJS_S=$(SRCS:.c=.s)

TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)


#Stage 1:
riscv64-chibicc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(OBJS): chibicc.h



test/%.exe: riscv64-chibicc test/%.c
	./riscv64-chibicc -Iinclude  -Itest  -o  test/$*.s test/$*.c
	$(GCC) -o $@ test/$*.s -xc test/common

test: $(TESTS)
	for i in $^; do echo $$i; qemu-riscv64 -L $(SYSROOT) ./$$i || exit 1; echo; done
	test/driver.sh ./riscv64-chibicc



test-all: test test-stage2

# Stage 2

stage2/riscv64-chibicc: $(OBJS_S:%=stage2/%)
	$(GCC) $(CFLAGS) -o  $@ -g $^
#$(GCC) $(CFLAGS) -o stage2/$*.o  -g -c stage2/$*.s
stage2/%.s: riscv64-chibicc  %.c
	mkdir -p stage2/test
	./riscv64-chibicc -I$(SYSROOT)/usr/include  -o stage2/$*.s $*.c


stage2/test/%.exe: stage2/riscv64-chibicc test/%.c
	mkdir -p stage2/test
	$(QEMU_RUN) ./stage2/riscv64-chibicc -Iinclude -Itest -o stage2/test/$*.s test/$*.c
	$(GCC) -o $@ stage2/test/$*.s -xc test/common

test-stage2: $(TESTS:test/%=stage2/test/%)
	for i in $^; do echo $$i; qemu-riscv64 -L $(SYSROOT) ./$$i || exit 1; echo; done
	test/driver.sh "qemu-riscv64  -L $(SYSROOT) ./stage2/riscv64-chibicc"



clean:
	rm -rf riscv64-chibicc *.o *~ tmp* *.s test/*.s test/*.exe stage2
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

.PHONY: test clean test-stage2
