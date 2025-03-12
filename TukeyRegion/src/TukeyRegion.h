// TukeyRegion.h
// By Pavlo Mozharovskyi
// Last changed 02.01.2018
// Header to C++-callable functions for the Tukey region and its elements

#pragma once

#include <Rcpp.h>
using namespace Rcpp;

#include <vector>
#include <algorithm>
#include <boost/dynamic_bitset.hpp>
#include <queue>
using namespace std;

extern "C"{
#include "libqhull.h"
#include "qhull_a.h"
//#include "glpk.h"
}

#include "common.h"
#include "hypermatrix.h"
#include "bstree.h"
#include "TRegion.h"
#include "TkRegions.h"
#include "TConvexBody.h"

#include <math.h>
#include <cstdlib>
#include <string>

#include "qhAdapter.h"
#include <boost/random.hpp>
