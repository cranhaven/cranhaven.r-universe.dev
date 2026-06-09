#include "ShadowCalcOptimized.h"
#include <algorithm>
#include <cmath>

// Thread-local shadow cache implementation
thread_local ThreadLocalShadowCache ShadowCalcOptimized::tl_shadow_cache;

bool ThreadLocalShadowCache::getCachedResult(const ShadowCacheKey& key, bool& result) {
    std::lock_guard<std::mutex> lock(cache_mutex);
    auto it = cache.find(key);
    if (it != cache.end()) {
        result = it->second;
        return true;
    }
    return false;
}

void ThreadLocalShadowCache::setCachedResult(const ShadowCacheKey& key, bool result) {
    std::lock_guard<std::mutex> lock(cache_mutex);
    if (cache.size() >= MAX_CACHE_SIZE) {
        // Simple LRU: clear half the cache
        auto it = cache.begin();
        std::advance(it, cache.size() / 2);
        cache.erase(cache.begin(), it);
    }
    cache[key] = result;
}

void ThreadLocalShadowCache::clear() {
    std::lock_guard<std::mutex> lock(cache_mutex);
    cache.clear();
}

// SOLPOS cache implementation
bool SolposCache::getCachedSolpos(const SolposKey& key, posdata& result) {
    std::lock_guard<std::mutex> lock(cache_mutex);
    auto it = cache.find(key);
    if (it != cache.end()) {
        result = it->second;
        return true;
    }
    return false;
}

void SolposCache::setCachedSolpos(const SolposKey& key, const posdata& result) {
    std::lock_guard<std::mutex> lock(cache_mutex);
    if (cache.size() >= MAX_CACHE_SIZE) {
        // Simple LRU: clear half the cache
        auto it = cache.begin();
        std::advance(it, cache.size() / 2);
        cache.erase(cache.begin(), it);
    }
    cache[key] = result;
}

void SolposCache::clear() {
    std::lock_guard<std::mutex> lock(cache_mutex);
    cache.clear();
}

// Enhanced shadow calculator implementation
ShadowCalcOptimized::ShadowCalcOptimized(AbstractPointCloud& shadowCloud, double voxelSize) 
    : ShadowCalc(shadowCloud, voxelSize), cache_grid_size(voxelSize * 2.0) {
}

ShadowCalcOptimized::~ShadowCalcOptimized() {
}

ShadowCacheKey ShadowCalcOptimized::createCacheKey(const posdata& solposData, 
                                                 const std::vector<double>& point) {
    ShadowCacheKey key;
    // Spatial grid (round to cache_grid_size)
    key.x_grid = static_cast<int>(std::round(point[0] / cache_grid_size));
    key.y_grid = static_cast<int>(std::round(point[1] / cache_grid_size));
    key.z_grid = static_cast<int>(std::round(point[2] / cache_grid_size));
    
    // Temporal grid (round to 15-minute intervals)
    key.day = solposData.daynum;
    key.hour = solposData.hour;
    key.minute = (solposData.minute / 15) * 15; // Round to 15-min intervals
    
    return key;
}

bool ShadowCalcOptimized::computeShadowCached(const posdata& solposData, 
                                            const std::vector<double>& point) {
    // Create cache key
    ShadowCacheKey cache_key = createCacheKey(solposData, point);
    
    // Check cache first
    bool cached_result;
    if (tl_shadow_cache.getCachedResult(cache_key, cached_result)) {
        return cached_result;
    }
    
    // Compute shadow using parent class method
    bool result = ShadowCalc::computeShadow(solposData, point);
    
    // Cache the result
    tl_shadow_cache.setCachedResult(cache_key, result);
    
    return result;
}

std::vector<bool> ShadowCalcOptimized::computeShadowBatch(const posdata& solposData, 
                                                        const std::vector<std::vector<double>>& points) {
    std::vector<bool> results(points.size());
    
    // Process batch with better cache locality
    for (size_t i = 0; i < points.size(); ++i) {
        results[i] = computeShadowCached(solposData, points[i]);
    }
    
    return results;
}

void ShadowCalcOptimized::clearCaches() {
    tl_shadow_cache.clear();
    SolposCache::getInstance().clear();
}
