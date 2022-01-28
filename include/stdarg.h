#ifndef __STDARG_H
#define __STDARG_H
#define va_list void *
#define va_start(ap, last)                                                     \
  do {                                                                         \
    ap = __va_area__;                                                          \
  } while (0)

#define va_end(ap)

#define va_arg(ap, type)                                                       \
  ({                                                                           \
    type x = *(type *)(ap);                                                    \
    ap += 8;                                                                   \
    x;                                                                         \
  })

#define __GNUC_VA_LIST 1
typedef va_list __gnuc_va_list;

#endif
