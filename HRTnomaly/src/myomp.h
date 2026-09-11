#ifndef __MYOMP_H__
#define __MYOMP_H__

#if defined _OPENMP
  #if (_OPENMP > 200800)
    #ifdef __clang__
    #define __VOPENMP 0
    #else
    #include <omp.h>
    #define __VOPENMP 1
    #endif
  #endif
#else
  #define __VOPENMP 0
#endif

#endif
