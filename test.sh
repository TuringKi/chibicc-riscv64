#!/bin/bash
GCC=riscv64-unknown-linux-gnu-gcc
GCCHost=gcc
C=./riscv64-chibicc
QEMU=qemu-riscv64
assert() {
    expected="$1"
    input="$2"
    $GCCHost main.c -o $C
    $C "$input" > tmp.s || exit
    $GCC -static -o tmp tmp.s
    $QEMU ./tmp
    actual="$?"

    if [ "$actual" = "$expected" ]; then
        echo "$input => $actual"
    else
        echo "$input => $expected expected, but got $actual"
        exit -1
    fi
}

assert 0 0
assert 42 42
assert 21 '5+20-4'
assert 99 '10     - 2    + 2-    10+    99'
assert 99 '10-     - 2    + 2-    10+    99'
echo OK