// TkRegions.h
// By Pavlo Mozharovskyi
// Last changed 13.04.2017
// Header to the main gate to computation of the Tukey region

#include "TukeyRegion.h"

bool TRegionTau(TMatrix X, int intDepth, int algStart, int numStart,
                vector<unsigned long long>* facets,
                queue<TVariables*>* ridges);

List TukeyRegionTau(NumericMatrix data, int depth,
                    String method,
                    bool trgFacets,
                    bool checkInnerPoint,
                    bool retHalfspaces,
                    bool retHalfspacesNR,
                    bool retInnerPoint,
                    bool retVertices,
                    bool retFacets,
                    bool retVolume,
                    bool retBarycenter,
                    queue<TVariables*>* ridges,
                    IntegerMatrix halfspaces,
                    NumericVector innerPoint,
                    int verbosity);
