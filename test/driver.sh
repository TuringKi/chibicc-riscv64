#!/bin/bash
C=$1

tmp=`mktemp -d /tmp/chibicc-test-XXXXXX`
trap 'rm -rf $tmp' INT TERM HUP EXIT
echo > $tmp/empty.c

check() {
    if [ $? -eq 0 ]; then
        echo "testing $1 ... passed"
    else
        echo "testing $1 ... failed"
        exit 1
    fi
}

# -o
rm -f $tmp/out
$C -o $tmp/out $tmp/empty.c
[ -f $tmp/out ]
check -o

echo foo > $tmp/out
cat $tmp/out | $C -E - | grep -q foo
check -E

echo foo > $tmp/out1
cat $tmp/out1 | $C -E -o $tmp/out2 -
cat $tmp/out2 | grep -q foo
check '-E and -o'

# -I
mkdir $tmp/dir
echo foo > $tmp/dir/i-option-test
cat $tmp/dir/i-option-test | $C -I$tmp/dir -E - | grep -q foo
check -I

# --help
$C --help 2>&1 | grep -q chibicc
check --help

echo OK
