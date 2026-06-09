#include "SpatialOptimizer.h"
#include <RcppArmadillo.h>
#include <limits>

std::vector<size_t> SpatialOptimizer::computeSpatialOrder(const std::vector<std::vector<double>>& points) {
    if (points.empty()) return {};
    
    size_t n_points = points.size();
    std::vector<uint32_t> x_norm(n_points), y_norm(n_points), z_norm(n_points);
    
    normalizeCoordinates(points, x_norm, y_norm, z_norm);
    
    // Create pairs of (morton_code, original_index)
    std::vector<std::pair<uint64_t, size_t>> morton_pairs;
    morton_pairs.reserve(n_points);
    
    for (size_t i = 0; i < n_points; ++i) {
        uint64_t morton = morton3D(x_norm[i], y_norm[i], z_norm[i]);
        morton_pairs.emplace_back(morton, i);
    }
    
    // Sort by Morton code
    std::sort(morton_pairs.begin(), morton_pairs.end());
    
    // Extract the spatial order
    std::vector<size_t> spatial_order;
    spatial_order.reserve(n_points);
    
    for (const auto& pair : morton_pairs) {
        spatial_order.push_back(pair.second);
    }
    
    return spatial_order;
}

std::vector<size_t> SpatialOptimizer::computeSpatialOrder(const Rcpp::NumericMatrix& coords) {
    size_t n_points = coords.nrow();
    std::vector<std::vector<double>> points(n_points, std::vector<double>(3));
    
    for (size_t i = 0; i < n_points; ++i) {
        points[i][0] = coords(i, 0);
        points[i][1] = coords(i, 1);
        points[i][2] = coords(i, 2);
    }
    
    return computeSpatialOrder(points);
}

std::vector<std::vector<size_t>> SpatialOptimizer::createSpatialBatches(
    const std::vector<size_t>& spatial_order, size_t batch_size) {
    
    std::vector<std::vector<size_t>> batches;
    
    for (size_t i = 0; i < spatial_order.size(); i += batch_size) {
        size_t end = std::min(i + batch_size, spatial_order.size());
        batches.emplace_back(spatial_order.begin() + i, spatial_order.begin() + end);
    }
    
    return batches;
}

void SpatialOptimizer::normalizeCoordinates(const std::vector<std::vector<double>>& points,
                                          std::vector<uint32_t>& x_norm,
                                          std::vector<uint32_t>& y_norm,
                                          std::vector<uint32_t>& z_norm) {
    if (points.empty()) return;
    
    // Find bounding box
    double min_x = std::numeric_limits<double>::max();
    double min_y = std::numeric_limits<double>::max();
    double min_z = std::numeric_limits<double>::max();
    double max_x = std::numeric_limits<double>::lowest();
    double max_y = std::numeric_limits<double>::lowest();
    double max_z = std::numeric_limits<double>::lowest();
    
    for (const auto& point : points) {
        min_x = std::min(min_x, point[0]);
        min_y = std::min(min_y, point[1]);
        min_z = std::min(min_z, point[2]);
        max_x = std::max(max_x, point[0]);
        max_y = std::max(max_y, point[1]);
        max_z = std::max(max_z, point[2]);
    }
    
    // Normalize to [0, 2^21] range (to fit in Morton encoding)
    double scale_x = (max_x > min_x) ? ((1 << 21) - 1) / (max_x - min_x) : 1.0;
    double scale_y = (max_y > min_y) ? ((1 << 21) - 1) / (max_y - min_y) : 1.0;
    double scale_z = (max_z > min_z) ? ((1 << 21) - 1) / (max_z - min_z) : 1.0;
    
    for (size_t i = 0; i < points.size(); ++i) {
        x_norm[i] = static_cast<uint32_t>((points[i][0] - min_x) * scale_x);
        y_norm[i] = static_cast<uint32_t>((points[i][1] - min_y) * scale_y);
        z_norm[i] = static_cast<uint32_t>((points[i][2] - min_z) * scale_z);
    }
}

uint32_t SpatialOptimizer::expandBits(uint32_t v) {
    v = (v | (v << 16)) & 0x030000FF;
    v = (v | (v <<  8)) & 0x0300F00F;
    v = (v | (v <<  4)) & 0x030C30C3;
    v = (v | (v <<  2)) & 0x09249249;
    return v;
}

uint64_t SpatialOptimizer::morton3D(uint32_t x, uint32_t y, uint32_t z) {
    uint64_t answer = 0;
    answer |= expandBits(x) | (expandBits(y) << 1) | (expandBits(z) << 2);
    return answer;
}
