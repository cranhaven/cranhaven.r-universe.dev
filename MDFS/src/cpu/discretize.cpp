#include "discretize.h"

#include <random>

void discretize(
    uint32_t seed,
    uint32_t discretization_index,
    uint32_t feature_id,
    std::size_t divisions,
    std::size_t object_count,
    const double* in_data,
    const std::vector<double>& sorted_in_data,
    uint8_t* out_data,
    double range
) {
    double* thresholds = new double[divisions];

    // brackets to limit scope
    {
        double sum = 0.0f;
        // brackets to limit scope
        {
            std::mt19937 seed_random_generator0(seed);
            std::mt19937 seed_random_generator1(seed_random_generator0() ^ discretization_index);
            std::mt19937 random_generator(seed_random_generator1() ^ feature_id);

            // E(X) = (a + b) / 2 = (1 - range + 1 + range) / 2 = 1
            std::uniform_real_distribution<double> uniform_range(1.0f - range, 1.0f + range);

            for (std::size_t d = 0; d < divisions; ++d) {
                thresholds[d] = uniform_range(random_generator);
                sum += thresholds[d];
            }

            sum += uniform_range(random_generator);
        }

        std::size_t done = 0;
        const double length_step = static_cast<double>(object_count) / sum;

        // thresholds are converted from an arbitrary space into real values (via indices)
        // d - iterates over divisions (of a variable in a discretization)
        for (std::size_t d = 0; d < divisions; ++d) {
            done += std::lround(thresholds[d] * length_step);

            // Note: Check when will this happen, maybe could be skipped
            if (done >= object_count) {
                done = object_count - 1;
            }

            thresholds[d] = sorted_in_data[done];
        }
    }

    // o - iterates over objects
    for (std::size_t o = 0; o < object_count; ++o) {
        out_data[o] = 0;

        // out_data[o] (starting with 0) is incremented every time in_data[o] is above a threshold
        // divisions is a small number (<=15), no reason to use binsearch, hence linear
        // d - iterates over divisions (per object o)
        for (std::size_t d = 0; d < divisions; ++d) {
            out_data[o] += in_data[o] > thresholds[d];
        }
    }

    delete[] thresholds;
}
