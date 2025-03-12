// TRegion.h
// By Pavlo Mozharovskyi
// Last changed 13.04.2017
// Header to the main gate to computation of the Tukey region

#include "TukeyRegion.h"

bool TRegion(TMatrix X, int intDepth, int algStart, int numStart,
             vector<unsigned long long>* facets, int* numRidges);
bool TRegionCmb(TMatrix X, int intDepth, vector<unsigned long long>* facets);
bool TRegionBruteForce(TMatrix X, int intDepth,
                       vector<unsigned long long>* facets);
bool TRegionCmb2D(TMatrix X, int intDepth, vector<unsigned long long>* facets);
bool TRegionCheckDepth(TMatrix X, int intDepth, int algRegion, int algStart,
                       int numStart, vector<unsigned long long>* facets,
                       TPoint* innerPoint, int* numRidges);
bool TRegion2D(TMatrix X, int intDepth, int algStart, int numStart,
               vector<unsigned long long>* facets);
