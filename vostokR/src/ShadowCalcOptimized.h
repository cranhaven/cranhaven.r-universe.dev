#ifndef SHADOWCALCOPTIMIZED_H_
#define SHADOWCALCOPTIMIZED_H_

#include "ShadowCalc.h"
#include <unordered_map>
#include <mutex>
#include <thread>
#include <cstdint>

// Spatial-temporal key for shadow caching
struct ShadowCacheKey {
    int x_grid, y_grid, z_grid;  // Spatial grid
    int day, hour, minute;        // Temporal grid
    
    bool operator==(const ShadowCacheKey& other) const {
        return x_grid == other.x_grid && y_grid == other.y_grid && 
               z_grid == other.z_grid && day == other.day && 
               hour == other.hour && minute == other.minute;
    }
};

// Hash function for ShadowCacheKey
struct ShadowCacheKeyHash {
    std::size_t operator()(const ShadowCacheKey& k) const {
        return std::hash<int>()(k.x_grid) ^ 
               (std::hash<int>()(k.y_grid) << 1) ^ 
               (std::hash<int>()(k.z_grid) << 2) ^
               (std::hash<int>()(k.day) << 3) ^
               (std::hash<int>()(k.hour) << 4) ^
               (std::hash<int>()(k.minute) << 5);
    }
};

// Thread-local shadow cache
class ThreadLocalShadowCache {
public:
    static const size_t MAX_CACHE_SIZE = 10000;
    
    bool getCachedResult(const ShadowCacheKey& key, bool& result);
    void setCachedResult(const ShadowCacheKey& key, bool result);
    void clear();
    
private:
    std::unordered_map<ShadowCacheKey, bool, ShadowCacheKeyHash> cache;
    mutable std::mutex cache_mutex;
};

// SOLPOS result cache
struct SolposKey {
    int day, hour, minute;
    double lat, lon;
    
    bool operator==(const SolposKey& other) const {
        return day == other.day && hour == other.hour && 
               minute == other.minute && lat == other.lat && lon == other.lon;
    }
};

struct SolposKeyHash {
    std::size_t operator()(const SolposKey& k) const {
        return std::hash<int>()(k.day) ^ 
               (std::hash<int>()(k.hour) << 1) ^ 
               (std::hash<int>()(k.minute) << 2) ^
               (std::hash<double>()(k.lat) << 3) ^
               (std::hash<double>()(k.lon) << 4);
    }
};

class SolposCache {
public:
    static SolposCache& getInstance() {
        static SolposCache instance;
        return instance;
    }
    
    bool getCachedSolpos(const SolposKey& key, posdata& result);
    void setCachedSolpos(const SolposKey& key, const posdata& result);
    void clear();
    
private:
    std::unordered_map<SolposKey, posdata, SolposKeyHash> cache;
    mutable std::mutex cache_mutex;
    static const size_t MAX_CACHE_SIZE = 1000;
};

// Enhanced shadow calculator with optimizations
class ShadowCalcOptimized : public ShadowCalc {
public:
    ShadowCalcOptimized(AbstractPointCloud& shadowCloud, double voxelSize);
    virtual ~ShadowCalcOptimized();
    
    // Batch shadow computation
    std::vector<bool> computeShadowBatch(const posdata& solposData, 
                                        const std::vector<std::vector<double>>& points);
    
    // Thread-safe shadow computation with caching
    bool computeShadowCached(const posdata& solposData, 
                           const std::vector<double>& point);
    
    // Clear all caches
    void clearCaches();
    
private:
    ShadowCacheKey createCacheKey(const posdata& solposData, 
                                 const std::vector<double>& point);
    
    // Thread-local caches
    static thread_local ThreadLocalShadowCache tl_shadow_cache;
    
    // Cache grid resolution (meters)
    double cache_grid_size;
};

#endif /* SHADOWCALCOPTIMIZED_H_ */
