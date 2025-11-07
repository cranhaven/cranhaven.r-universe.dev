#include "wmem.h"
#include <vector>

namespace wmem {
std::vector<ghqCpp::simple_mem_stack<double> > mem_stacks =
  std::vector<ghqCpp::simple_mem_stack<double> >(1);

void setup_working_memory(const size_t n_threads){
  mem_stacks.resize(std::max<size_t>(1, n_threads));
}

ghqCpp::simple_mem_stack<double> &mem_stack(const size_t idx){
  return mem_stacks[idx];
}

} // namespace wmem
