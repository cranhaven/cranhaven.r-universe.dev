// src/solvers/jv_core.cpp
// The two instantiations of detail::jv_core() the pure C++ solvers link against;
// the algorithm is in jv_core_impl.h.

#include "jv_core_impl.h"

namespace lap {
namespace detail {

template JvCoreResult jv_core<CostMatrix>(const CostMatrix&, const JvCoreOpts&);
template JvCoreResult jv_core<LazyCostMatrix>(const LazyCostMatrix&, const JvCoreOpts&);

}  // namespace detail
}  // namespace lap
