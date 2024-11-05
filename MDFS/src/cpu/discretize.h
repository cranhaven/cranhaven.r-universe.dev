#ifndef DISCRETIZE_H
#define DISCRETIZE_H

#include <cstddef>
#include <cstdint>
#include <vector>

void discretize(
    uint32_t seed,
    uint32_t discretization_index,
    uint32_t feature_id,
    std::size_t divisions,
    std::size_t object_count,
    const double* in_data,
    const std::vector<double>& sorted_in_data, // Note: why different type?
    uint8_t* out_data,
    double range
);

#endif
