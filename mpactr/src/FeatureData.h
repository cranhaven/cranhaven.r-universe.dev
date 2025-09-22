//
// Created by Gregory Johnson on 8/22/25.
//

#ifndef MPACTR_FEATUREDATA_H
#define MPACTR_FEATUREDATA_H
#include <vector>
#include <string>
#include "IntensityPeaks.h"
struct FeatureData {
    IntensityPeaks metaData;
    std::string compoundName;
};
#endif //MPACTR_FEATUREDATA_H
