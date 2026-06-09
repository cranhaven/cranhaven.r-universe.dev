#define ARMA_DONT_USE_OPENMP
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include "../IrradianceCalc.h"
#include "../ShadowCalc.h"
#include "../solpos/solpos00.h"
#include "../Vec3d.h"
#include "../pointCloud/LidrPointCloud.h"
#include <vector>

using namespace Rcpp;

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
    NumericVector solar_potential(n_points);
    
    // Initialize SOLPOS
    posdata solpos;
    S_init(&solpos);
    
    solpos.year = year;
    solpos.latitude = lat;
    solpos.longitude = lon;
    solpos.timezone = timezone;
    
    // Create point cloud wrapper around the input data
    LidrPointCloud pointCloud(coords, normals);
    
    // Initialize shadow calculator with octree
    ShadowCalc shadow_calc(pointCloud, voxel_size);
    
    // Loop through days
    for(int day = day_start; day <= day_end; day += day_step) {
        solpos.daynum = day;
        
        // Get sunrise and sunset times
        solpos.hour = 12;
        solpos.minute = 0;
        solpos.second = 0;
        S_decode(S_solpos(&solpos), &solpos);
        
        int sunrise_minute = solpos.sretr;
        int sunset_minute = solpos.ssetr;
        
        // Loop through minutes between sunrise and sunset
        for(int current_minute = sunrise_minute; 
            current_minute < sunset_minute; 
            current_minute += minute_step) {
            
            solpos.hour = current_minute / 60;
            solpos.minute = current_minute - solpos.hour * 60;
            solpos.second = 0;
            
            // Calculate sun position
            S_decode(S_solpos(&solpos), &solpos);
            
            if(solpos.elevref >= min_sun_angle) {
                // Create irradiance calculator for current time
                IrradianceCalc irr_calc(solpos);
                
                // Process each point
                #pragma omp parallel for
                for(int i = 0; i < n_points; i++) {
                    std::vector<double> point(6);
                    for (int j = 0; j < 3; j++) {
                        point[j] = coords(i,j);
                        point[j+3] = normals(i,j);
                    }
                    
                    // Check if point is illuminated
                    bool illuminated = shadow_calc.computeShadow(solpos, point);
                    
                    // Calculate irradiance for current timestep
                    double irr = irr_calc.getIrradiance(point, illuminated);
                    
                    // Calculate sunny hours in current timestep
                    float sunny_hours = (float)(minute_step) / 60.0;
                    
                    // Add to total
                    #pragma omp atomic
                    solar_potential[i] += irr * sunny_hours * day_step;
                }
            }
        }
    }
    
    return solar_potential;
}
