#ifndef SE2_PRINT_H
#define SE2_PRINT_H

#ifdef USING_R
#  include <R_ext/Print.h>

#  define se2_printf Rprintf
#  define se2_warn REprintf
#  define se2_puts(...) se2_printf(__VA_ARGS__); se2_printf("\n")

#else
#  define se2_printf printf
#  define se2_warn(...) fprintf(stderr, __VA_ARGS__);
#  define se2_puts puts
#endif

#endif
