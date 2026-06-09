#ifndef SPATIALOPTIMIZER_H_
#define SPATIALOPTIMIZER_H_

#include <vector>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <RcppArmadillo.h>

// Spatial locality optimizer
class SpatialOptimizer {
public:
    // Compute spatial ordering using Morton codes (Z-order curve)
    static std::vector<size_t> computeSpatialOrder(const std::vector<std::vector<double>>& points);
    
    // Compute spatial ordering for Rcpp matrices
    static std::vector<size_t> computeSpatialOrder(const Rcpp::NumericMatrix& coords);
    
    // Create batches of spatially coherent points
    static std::vector<std::vector<size_t>> createSpatialBatches(
        const std::vector<size_t>& spatial_order, 
        size_t batch_size = 64);
    
private:
    // Morton encoding for 3D coordinates
    static uint64_t morton3D(uint32_t x, uint32_t y, uint32_t z);
    static uint32_t expandBits(uint32_t v);
    
    // Normalize coordinates to integer grid
    static void normalizeCoordinates(const std::vector<std::vector<double>>& points,
                                   std::vector<uint32_t>& x_norm,
                                   std::vector<uint32_t>& y_norm,
                                   std::vector<uint32_t>& z_norm);
};

#endif /* SPATIALOPTIMIZER_H_ */
