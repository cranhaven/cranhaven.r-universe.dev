#ifndef WMEM_H
#define WMEM_H

#include "simple-mem-stack.h"
#ifdef _OPENMP
#include <omp.h>
#endif

namespace wmem {
inline int thread_num() noexcept {
#ifdef _OPENMP
  return omp_get_thread_num();
#else
  return 0L;
#endif
}

/**
 * sets the working up for some maximum number of threads. This is not thread
 * and possibly invalids all requested memory
 */
void setup_working_memory(const size_t);

/// returns the simple_mem_stack for a given thread
ghqCpp::simple_mem_stack<double> &mem_stack(const size_t);

/// returns the simple_mem_stack for this thread
inline ghqCpp::simple_mem_stack<double> &mem_stack(){
  return mem_stack(thread_num());
}

} // namespace wmem

#endif
