#ifndef METHODS_HPP
#define METHODS_HPP

#include "classes.h"   // for Raster, HoughCenters, HoughCircle
#include <vector>

using std::vector;

// Core Hough-related functions
Raster getCounts(vector<vector<double> >& slice, double pixel_size);

vector<HoughCenters> treeHough(vector<vector<double> >& cppCloud,
                               double h1 = 1, double h2 = 3, double hstep = 0.5,
                               double radius = 0.25, double pixel = 0.025,
                               double density = 0.1, unsigned int votes = 3);

HoughCenters getSingleCenter(Raster* raster,
                             double max_radius = 0.25,
                             double min_den = 0.1,
                             unsigned int min_votes = 3);

vector<HoughCenters> getCenters(Raster* raster,
                                double max_radius = 0.25,
                                double min_den = 0.1,
                                unsigned int min_votes = 3);

#endif // METHODS_HPP
