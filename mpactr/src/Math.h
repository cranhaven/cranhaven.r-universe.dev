//
// Created by Gregory Johnson on 8/22/25.
//

#ifndef MPACTR_MATH_H
#define MPACTR_MATH_H
#include <numeric>
#include <vector>

class VectorMath {
public:
    static double Mean(const std::vector<double>& data) {
        return std::accumulate(data.begin(), data.end(), 0.0) / static_cast<double>(data.size());
    }
    static double StandardDeviation(const std::vector<double>& data) {
        const double mean = Mean(data);
        const auto size = static_cast<double>(data.size());
        double summationDist = 0;
        for (const auto& val : data) {
            summationDist += pow(std::abs(val - mean), 2);
        }
        return std::sqrt(summationDist/(size - 1));
    }
    static double CoefficientOfVarianceCalculation(const std::vector<double>& data) {
        const double mean = Mean(data);
        if (mean == 0) return -1;
        const double standardDeviation = StandardDeviation(data);
        return standardDeviation/mean;
    }

};
#endif //MPACTR_MATH_H
