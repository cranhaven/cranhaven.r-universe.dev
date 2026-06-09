#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#ifdef _OPENMP
#include <omp.h>
#endif

#include "IrradianceCalc.h"
#include "ShadowCalc.h"
#include "ShadowCalcOptimized.h"
#include "SpatialOptimizer.h"
#include "solpos/solpos00.h"
#include "Vec3d.h"
#include "pointCloud/LidrPointCloud.h"
#include <vector>
#include <algorithm>

using namespace Rcpp;

// OpenMP thread control functions
// [[Rcpp::export]]
void set_vostokr_threads(int n_threads) {
#ifdef _OPENMP
    if (n_threads > 0) {
        omp_set_num_threads(n_threads);
    }
#endif
}

// [[Rcpp::export]]
int get_vostokr_threads() {
#ifdef _OPENMP
    return omp_get_max_threads();
#else
    return 1;
#endif
}

// [[Rcpp::export]]
int get_actual_vostokr_threads() {
#ifdef _OPENMP
    int actual_threads = 1;
    #pragma omp parallel
    {
        #pragma omp single
        actual_threads = omp_get_num_threads();
    }
    return actual_threads;
#else
    return 1;
#endif
}

// [[Rcpp::export]]
int get_vostokr_max_threads() {
#ifdef _OPENMP
    return omp_get_num_procs();
#else
    return 1;
#endif
}

// Performance monitoring and cache control
// [[Rcpp::export]]
void clear_vostokr_caches() {
    SolposCache::getInstance().clear();
    // Note: Thread-local caches are cleared automatically
}

// [[Rcpp::export]]
List get_vostokr_performance_info() {
    List info;
    info["openmp_enabled"] = 
#ifdef _OPENMP
        true;
#else
        false;
#endif
    info["max_threads"] = get_vostokr_max_threads();
    info["current_threads"] = get_vostokr_threads();
    return info;
}

// [[Rcpp::export]]
NumericVector calculate_solar_potential_cpp(NumericMatrix coords,
                                         NumericMatrix normals,
                                         int year,
                                         int day_start,
                                         int day_end,
                                         int day_step,
                                         int minute_step,
                                         double min_sun_angle,
                                         double voxel_size,
                                         double lat,
                                         double lon,
                                         double timezone) {
    
    int n_points = coords.nrow();
    NumericVector solar_potential(n_points, 0.0);
    
    // Create point cloud wrapper around the input data
    LidrPointCloud pointCloud(coords, normals);
    
    // Initialize optimized shadow calculator with caching
    ShadowCalcOptimized shadow_calc(pointCloud, voxel_size);
    
    // Compute spatial ordering for cache coherence
    std::vector<size_t> spatial_order = SpatialOptimizer::computeSpatialOrder(coords);
    
    // Create spatial batches for processing
    const size_t BATCH_SIZE = 64; // Optimized batch size
    std::vector<std::vector<size_t>> spatial_batches = 
        SpatialOptimizer::createSpatialBatches(spatial_order, BATCH_SIZE);
    
    // SOLPOS cache for temporal coherence
    SolposCache& solpos_cache = SolposCache::getInstance();
    
    // Hierarchical parallelization: parallel over time, then space
#ifdef _OPENMP
    int num_threads = omp_get_max_threads();
    #pragma omp parallel num_threads(num_threads)
#endif
    {
        // Each thread gets its own SOLPOS struct
        posdata thread_solpos;
        S_init(&thread_solpos);
        thread_solpos.year = year;
        thread_solpos.latitude  = static_cast<float>(lat);
        thread_solpos.longitude = static_cast<float>(lon);
        thread_solpos.timezone  = static_cast<float>(timezone);
        
        // Parallel over days
    #ifdef _OPENMP
        #pragma omp for schedule(dynamic)
    #endif
        for(int day = day_start; day <= day_end; day += day_step) {
            thread_solpos.daynum = day;
            
            // Get sunrise and sunset times
            thread_solpos.hour = 12;
            thread_solpos.minute = 0;
            thread_solpos.second = 0;
            S_decode(S_solpos(&thread_solpos), &thread_solpos);
            
            int sunrise_minute = static_cast<int>(thread_solpos.sretr);
            int sunset_minute  = static_cast<int>(thread_solpos.ssetr);
            
            // Loop through minutes between sunrise and sunset
            for(int current_minute = sunrise_minute; 
                current_minute < sunset_minute; 
                current_minute += minute_step) {
                
                thread_solpos.hour = current_minute / 60;
                thread_solpos.minute = current_minute - thread_solpos.hour * 60;
                thread_solpos.second = 0;
                
                // Check SOLPOS cache first
                SolposKey solpos_key = {day, thread_solpos.hour, thread_solpos.minute, lat, lon};
                if (!solpos_cache.getCachedSolpos(solpos_key, thread_solpos)) {
                    // Calculate sun position if not cached
                    S_decode(S_solpos(&thread_solpos), &thread_solpos);
                    solpos_cache.setCachedSolpos(solpos_key, thread_solpos);
                }
                
                if(thread_solpos.elevref >= min_sun_angle) {
                    // Create irradiance calculator for current time
                    IrradianceCalc irr_calc(thread_solpos);
                    
                    // Process spatial batches for better cache locality
                    for (const auto& batch : spatial_batches) {
                        // Prepare batch data
                        std::vector<std::vector<double>> batch_points;
                        batch_points.reserve(batch.size());
                        
                        for (size_t idx : batch) {
                            std::vector<double> point(6);
                            for (int j = 0; j < 3; j++) {
                                point[j] = coords(idx, j);
                                point[j+3] = normals(idx, j);
                            }
                            batch_points.push_back(point);
                        }
                        
                        // Compute shadows for entire batch
                        std::vector<bool> illuminated_batch = 
                            shadow_calc.computeShadowBatch(thread_solpos, batch_points);
                        
                        // Calculate irradiance and accumulate results
                        for (size_t b = 0; b < batch.size(); ++b) {
                            size_t point_idx = batch[b];
                            
                            double irr = irr_calc.getIrradiance(batch_points[b], illuminated_batch[b]);
                            float sunny_hours = static_cast<float>(minute_step) / 60.0f;
                            
                            // Thread-safe accumulation
#ifdef _OPENMP
                            #pragma omp atomic
#endif
                            solar_potential[point_idx] += irr * sunny_hours * day_step;
                        }
                    }
                }
            }
        }
    }
    
    return solar_potential;
}
