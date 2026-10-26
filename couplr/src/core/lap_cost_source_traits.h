// src/core/lap_cost_source_traits.h
// Type trait distinguishing cost-source types that expose a raw flat `mask`
// array (CostMatrix) from ones that only expose at()/allowed() (LazyCostMatrix).
// A handful of solver hot loops bypass the accessor methods and index `.mask`
// directly for speed; templating those on cost-source type needs an
// `if constexpr` branch, since LazyCostMatrix has no such array to index.
#pragma once

#include "lap_types.h"
#include <type_traits>

namespace lap {

template <typename T>
struct supports_raw_mask : std::false_type {};

template <>
struct supports_raw_mask<CostMatrix> : std::true_type {};

}  // namespace lap
