/*
 *  constants.h
 *  ApproxMap
 *
 *  Created by yonghui on 4/17/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef CONSTANTS_HEADER
#define CONSTANTS_HEADER

#include <string>

using namespace std;
const double PROB_HOEFFDING_CUT_OFF = 0.000001e0;
const double ZERO_MINUS = -0.0001e0;
const double ESTIMATION_BEFORE_CLUSTERING = 0.01e0;
const double ZERO_PLUS = 0.0001e0;
const double Missing_Threshold = 0.30e0; // a marker will be removed if more than 40% of its genotype calls are missing
const double COMBINE_BINS_TH = 0.1e0;
const string HALDANE = "haldane";
const string KOSAMBI = "kosambi";
const double kMaskThreshold = 0.75e0;
const double kMinMaskThreshold = 0.75e0;
const double kMaskDecrement = 0.02e0;
enum ObjFunc{OBJF_ML, OBJF_COUNT, OBJF_CM};
const int kMaxErrorDectionRounds = 20;
const int kMaxMissingEstRounds = 10;
const bool kMSTVerbose = false;
const int kBadDetMaxNum = 8;
#endif
