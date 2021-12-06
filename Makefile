CFLAGS=-std=c11 -g -fno-common
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

riscv64-chibicc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(OBJS): chibicc.h


test: riscv64-chibicc
	./test.sh

clean:
	rm -f riscv64-chibicc *.o *~ tmp* *.s

.PHONY: test clean