#include "test.h"
#include <stdarg.h>

int ret3(void) {
  return 3;
  return 5;
}

int add2(int x, int y) { return x + y; }

int sub2(int x, int y) { return x - y; }

int add6(int a, int b, int c, int d, int e, int f) {
  return a + b + c + d + e + f;
}

int addx(int *x, int y) { return *x + y; }

int sub_char(char a, char b, char c) { return a - b - c; }

int fib(int x) {
  if (x <= 1)
    return 1;
  return fib(x - 1) + fib(x - 2);
}

int sub_long(long a, long b, long c) { return a - b - c; }
int sub_short(short a, short b, short c) { return a - b - c; }

int g1;

int *g1_ptr(void) { return &g1; }
char int_to_char(int x) { return x; }

int div_long(long a, long b) { return a / b; }

_Bool bool_fn_add(_Bool x) { return x + 1; }
_Bool bool_fn_sub(_Bool x) { return x - 1; }

static int static_fn(void) { return 3; }

int param_decay(int x[]) { return x[0]; }

int counter() {
  static int i;
  static int j = 1 + 1;
  return i++ + j++;
}

void ret_none() { return; }

_Bool true_fn();
_Bool false_fn();
char char_fn();
short short_fn();
int add_all(int n, ...);

int add_all(int n, ...);
int sprintf(char *buf, char *fmt, ...);
int vsprintf(char *buf, char *fmt, void *ap);

unsigned char uchar_fn();
unsigned short ushort_fn();

char schar_fn();
short sshort_fn();

double add_double(double x, double y);
float add_float(float x, float y);

char *fmt(char *buf, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vsprintf(buf, fmt, ap);
}

float add_float3(float x, float y, float z) { return x + y + z; }

double add_double3(double x, double y, double z) { return x + y + z; }
double add_double_int(double x, double y, int z) { return x + y + z; }

int (*fnptr(int (*fn)(int n, ...)))(int, ...) { return fn; }

int param_decay2(int x()) { return x(); }

char *func_fn(void) { return __func__; }

char *function_fn(void) { return __FUNCTION__; }

int add10_int(int x1, int x2, int x3, int x4, int x5, int x6, int x7, int x8,
              int x9, int x10);
float add10_float(float x1, float x2, float x3, float x4, float x5, float x6,
                  float x7, float x8, float x9, float x10);
double add10_double(double x1, double x2, double x3, double x4, double x5,
                    double x6, double x7, double x8, double x9, double x10);

int many_args1_(int a, int b, int c, int d, int e, int f, int g, int h, int i,
                int j, int k, int l, int m);

int many_args1(int a, int b, int c, int d, int e, int f, int g, int h, int i,
               int j, int k, int l, int m) {
  printf("%d,", a);
  printf("%d,", b);
  printf("%d,", c);
  printf("%d,", d);
  printf("%d,", e);
  printf("%d,", f);
  printf("%d,", g);
  printf("%d,", h);
  printf("%d,", i);
  printf("%d,", j);
  printf("%d,", k);
  printf("%d,", l);
  printf("%d\n", m);
  return g / h;
}

double many_args2(double a, double b, double c, double d, double e, double f,
                  double g, double h, double i, double j) {
  printf("%lf,", a);
  printf("%lf,", b);
  printf("%lf,", c);
  printf("%lf,", d);
  printf("%lf,", e);
  printf("%lf,", f);
  printf("%lf,", g);
  printf("%lf,", h);
  printf("%lf,", i);
  printf("%lf\n", j);
  return i / j;
}
int many_args3_(int a, double b, int c, int d, double e, int f, double g, int h,
                double i, double j, double k, double l, double m, int n, int o,
                double p, double, int);
int many_args3(int a, double b, int c, int d, double e, int f, double g, int h,
               double i, double j, double k, double l, double m, int n, int o,
               double p, double z, int zz) {
  printf("%d,", a);
  printf("%lf,", b);
  printf("%d,", c);
  printf("%d,", d);
  printf("%lf,", e);
  printf("%d,", f);
  printf("%lf,", g);
  printf("%d,", h);
  printf("%lf,", i);
  printf("%lf,", j);
  printf("%lf,", k);
  printf("%lf,", l);
  printf("%lf,", m);
  printf("%d,", n);
  printf("%d,", o);
  printf("%lf,", p);
  printf("%lf,", z);
  printf("%d\n,", zz);
  return o / p;
}

typedef struct {
  int a, b;
  short c;
  char d;
} Ty4;
typedef struct {
  int a;
  float b;
  double c;
} Ty5;
typedef struct {
  unsigned char a[3];
} Ty6;
typedef struct {
  long a, b, c;
} Ty7;
typedef struct {
  float a;
  double b;
} Ty8;

int struct_test5(Ty5 x, int n);
int struct_test4(Ty4 x, int n);
int struct_test6(Ty6 x, int n);
int struct_test7(Ty7 x, int n);
int struct_test8(Ty8 x, int n);

int struct_test4(Ty4 x, int n) {

  printf(">%d,", x.a);
  printf("%d,", x.b);
  printf("%d,", x.c);
  printf("%d<, %d\n", x.d, n);

  switch (n) {
  case 0:
    return x.a;
  case 1:
    return x.b;
  case 2:
    return x.c;
  default:
    return x.d;
  }
}

int struct_test5(Ty5 x, int n) {
  printf(">%d,", x.a);
  printf("%f,", x.b);
  printf("%lf<, %d\n,", x.c, n);

  switch (n) {
  case 0:
    return x.a;
  case 1:
    return x.b;
  default:
    return x.c;
  }
}

int struct_test6(Ty6 x, int n) { return x.a[n]; }

int struct_test7(Ty7 x, int n) {
  printf("%d\n", n);
  printf(">%ld,", x.a);
  printf("%ld,", x.b);
  printf("%ld<, %d\n,", x.c, n);
  switch (n) {
  case 0:
    return x.a;
  case 1:
    return x.b;
  default:
    return x.c;
  }
}

int struct_test8(Ty8 x, int n) {
  printf("%d\n", n);
  printf(">%f,", x.a);
  printf("%lf<, %d\n,", x.b, n);
  switch (n) {
  case 0:
    return x.a;
  case 1:
    return x.b;
  }
}

int struct_test14(Ty4 x, int n) {
  printf(">%d,", x.a);
  printf("%d,", x.b);
  printf("%d,", x.c);
  printf("%d<, %d\n", x.d, n);

  switch (n) {
  case 0:
    return x.a;
  case 1:
    return x.b;
  case 2:
    return x.c;
  default:
    return x.d;
  }
}

int struct_test15(Ty5 x, int n) {
  switch (n) {
  case 0:
    return x.a;
  case 1:
    return x.b;
  default:
    return x.c;
  }
}

int main() {
  ASSERT(3, ret3());
  ASSERT(8, add2(3, 5));
  ASSERT(2, sub2(5, 3));
  ASSERT(21, add6(1, 2, 3, 4, 5, 6));
  ASSERT(66, add6(1, 2, add6(3, 4, 5, 6, 7, 8), 9, 10, 11));
  ASSERT(136, add6(1, 2, add6(3, add6(4, 5, 6, 7, 8, 9), 10, 11, 12, 13), 14,
                   15, 16));

  ASSERT(7, add2(3, 4));
  ASSERT(1, sub2(4, 3));
  ASSERT(55, fib(9));

  ASSERT(1, ({ sub_char(7, 3, 3); }));

  ASSERT(1, sub_long(7, 3, 3));
  ASSERT(1, sub_short(7, 3, 3));

  ASSERT(5, int_to_char(261));
  ASSERT(-5, div_long(-10, 2));
  ASSERT(1, bool_fn_add(3));
  ASSERT(0, bool_fn_sub(3));
  ASSERT(1, bool_fn_add(-3));
  ASSERT(0, bool_fn_sub(-3));
  ASSERT(1, bool_fn_add(0));
  ASSERT(1, bool_fn_sub(0));

  ASSERT(2, counter());
  ASSERT(4, counter());
  ASSERT(6, counter());

  ASSERT(1, true_fn());
  ASSERT(0, false_fn());
  ASSERT(3, char_fn());
  ASSERT(5, short_fn());

  ASSERT(6, add_all(3, 1, 2, 3));
  ASSERT(5, add_all(4, 1, 2, 3, -1));

  {
    char buf[100];
    fmt(buf, "%d %d %d %s", 1, 14, 64, "mx");
    printf("%s\n", buf);
  }

  ASSERT(0, ({
           char buf[100];
           sprintf(buf, "%d %d %s", 1, 2, "foo");
           strcmp("1 2 foo", buf);
         }));

  ASSERT(0, ({
           char buf[100];
           fmt(buf, "%d %d %s", 1, 2, "foo");
           strcmp("1 2 foo", buf);
         }));

  ASSERT(251, uchar_fn());
  ASSERT(65528, ushort_fn());
  ASSERT(-5, schar_fn());
  ASSERT(-8, sshort_fn());

  ASSERT(6, add_float(2.3, 3.8));
  ASSERT(6, add_double(2.3, 3.8));

  ASSERT(7, add_float3(2.5, 2.5, 2.5));
  ASSERT(7, add_double3(2.5, 2.5, 2.5));
  ASSERT(8, add_double_int(2.5, 2.5, 3));

  ASSERT(0, ({
           char buf[100];
           sprintf(buf, "%.1f", (float)3.5);
           strcmp(buf, "3.5");
         }));
  ASSERT(0, ({
           char buf[100];
           fmt(buf, "%.1f", (float)3.5);
           strcmp(buf, "3.5");
         }));

  ASSERT(5, (add2)(2, 3));
  ASSERT(5, (&add2)(2, 3));
  ASSERT(7, ({
           int (*fn)(int, int) = add2;
           fn(2, 5);
         }));
  ASSERT(6, fnptr(add_all)(3, 1, 2, 3));

  ASSERT(3, param_decay2(ret3));

  ASSERT(5, sizeof(__func__));
  ASSERT(0, strcmp("main", __func__));
  ASSERT(0, strcmp("func_fn", func_fn()));

  ASSERT(0, strcmp("main", __FUNCTION__));
  ASSERT(0, strcmp("function_fn", function_fn()));

  ASSERT(55, add10_int(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
  ASSERT(55, add10_float(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
  ASSERT(55, add10_double(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));

  ASSERT(0, ({
           char buf[200];
           sprintf(buf,
                   "%d %.1f %.1f %.1f %d %d %.1f %d %d %d %d %.1f %d %d %.1f "
                   "%.1f %.1f %.1f %d",
                   1, 1.0, 1.0, 1.0, 1, 1, 1.0, 1, 1, 1, 1, 1.0, 1, 1, 1.0, 1.0,
                   1.0, 1.0, 1);
           strcmp("1 1.0 1.0 1.0 1 1 1.0 1 1 1 1 1.0 1 1 1.0 1.0 1.0 1.0 1",
                  buf);
         }));

  ASSERT(4, many_args1_(1, 2, 3, 4, 5, 6, 40, 10, 12, 13, 14, 15, 16));
  ASSERT(4, many_args1(1, 2, 3, 4, 5, 6, 40, 10, 12, 13, 14, 15, 16));
  ASSERT(4, many_args2(1, 2, 3, 4, 5, 6, 7, 8, 40, 10));
  ASSERT(8, many_args3_(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 80, 10,
                        43, 42));

  ASSERT(8, many_args3(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 80, 10,
                       43, 42));

  ASSERT(10, ({
           Ty4 x = {10, 20, 30, 40};
           struct_test4(x, 0);
         }));
  ASSERT(20, ({
           Ty4 x = {10, 20, 30, 40};
           struct_test4(x, 1);
         }));
  ASSERT(30, ({
           Ty4 x = {10, 20, 30, 40};
           struct_test4(x, 2);
         }));
  ASSERT(40, ({
           Ty4 x = {10, 20, 30, 40};
           struct_test4(x, 3);
         }));

  ASSERT(10, ({
           Ty5 x = {10, 20, 30};
           struct_test5(x, 0);
         }));
  ASSERT(20, ({
           Ty5 x = {10, 20, 30};
           struct_test5(x, 1);
         }));
  ASSERT(30, ({
           Ty5 x = {10, 20, 30};
           struct_test5(x, 2);
         }));

  ASSERT(10, ({
           Ty6 x = {10, 20, 30};
           struct_test6(x, 0);
         }));
  ASSERT(20, ({
           Ty6 x = {10, 20, 30};
           struct_test6(x, 1);
         }));
  ASSERT(30, ({
           Ty6 x = {10, 20, 30};
           struct_test6(x, 2);
         }));

  ASSERT(10, ({
           Ty7 x = {10, 20, 30};
           struct_test7(x, 0);
         }));
  ASSERT(20, ({
           Ty7 x = {10, 20, 30};
           struct_test7(x, 1);
         }));
  ASSERT(30, ({
           Ty7 x = {10, 20, 30};
           struct_test7(x, 2);
         }));

  ASSERT(10, ({
           Ty8 x = {10, 20};
           struct_test8(x, 0);
         }));
  ASSERT(20, ({
           Ty8 x = {10, 20};
           struct_test8(x, 1);
         }));

  ASSERT(20, ({
           Ty4 x = {10, 20, 30, 40};
           struct_test14(x, 1);
         }));
  ASSERT(10, ({
           Ty4 x = {10, 20, 30, 40};
           struct_test14(x, 0);
         }));

  ASSERT(30, ({
           Ty4 x = {10, 20, 30, 40};
           struct_test14(x, 2);
         }));
  ASSERT(40, ({
           Ty4 x = {10, 20, 30, 40};
           struct_test14(x, 3);
         }));

  ASSERT(10, ({
           Ty5 x = {10, 20, 30};
           struct_test15(x, 0);
         }));
  ASSERT(20, ({
           Ty5 x = {10, 20, 30};
           struct_test15(x, 1);
         }));
  ASSERT(30, ({
           Ty5 x = {10, 20, 30};
           struct_test15(x, 2);
         }));

  printf("OK\n");
  return 0;
}
