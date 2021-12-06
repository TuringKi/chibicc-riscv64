#!/bin/bash
GCC=riscv64-unknown-linux-gnu-gcc
GCCHost=gcc
C=./riscv64-chibicc
QEMU=qemu-riscv64
assert() {
    expected="$1"
    input="$2"
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

assert 3 'a=3; a;'
assert 8 'a=3; z=5; a+z;'
assert 6 'a=b=3; a+b;'
assert 0 '0;'
assert 42 '42;'
assert 0 '0==1;'
assert 1 '42==42;'
assert 1 '0!=1;'
assert 0 '42!=42;'

assert 1 '0<1;'
assert 0 '1<1;'
assert 0 '2<1;'
assert 1 '0<=1;'
assert 1 '1<=1;'
assert 0 '2<=1;'

assert 1 '1>0;'
assert 0 '1>1;'
assert 0 '1>2;'
assert 1 '1>=0;'
assert 1 '1>=1;'
assert 0 '1>=2;'
assert 0 '0==1;'
assert 1 '12==12;'
assert 1 '1!=2;'
assert 0 '2!=2;'
assert 0 '2<2;'
assert 1 '1<2;'
assert 1 '101>22;'
assert 1 '101>=22;'
assert 1 '22>=22;'
assert 0 '2>=22;'
assert 1 '2<=22;'
assert 1 '22<=22;'
assert 21 '5+20-4;'
assert 99 '10     - 2    + 2-    10+    99;'
assert 18 '10*2-2;'
assert 9 '10- 2/2;'
assert 99 '1*99;'
assert 109 '10+99;'
assert 17 '10*      2    + 2-    10/    2;'
assert 16 '10*      2    + (2-    10)/    2;'
assert 14 '10*      2    + (-2-    10)/    2;'
assert 3 '1; 2; 3;'
echo OK