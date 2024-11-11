
#ifndef EXPECTILE_H
#define EXPECTILE_H

#include <math.h>
#include <algorithm>
#include "distribution.h"

class CExpectile: public CDistribution
{

public:

    CExpectile(double dAlpha);

    virtual ~CExpectile();

    erboostRESULT ComputeWorkingResponse(double *adY,
                                   double *adMisc,
                                   double *adOffset,
                                   double *adF, 
                                   double *adZ, 
                                   double *adWeight,
                                   bool *afInBag,
                                   unsigned long nTrain);

    erboostRESULT InitF(double *adY, 
                    double *adMisc,
                    double *adOffset,
                    double *adWeight,
                    double &dInitF, 
                    unsigned long cLength);

    erboostRESULT FitBestConstant(double *adY,
                              double *adMisc,
                              double *adOffset,
                              double *adW,
                              double *adF,
                              double *adZ,
                              unsigned long *aiNodeAssign,
                              unsigned long nTrain,
                              VEC_P_NODETERMINAL vecpTermNodes,
                              unsigned long cTermNodes,
                              unsigned long cMinObsInNode,
                              bool *afInBag,
                              double *adFadj);

    double Deviance(double *adY,
                    double *adMisc,
                    double *adOffset,
                    double *adWeight,
                    double *adF,
                    unsigned long cLength);

    double BagImprovement(double *adY,
                          double *adMisc,
                          double *adOffset,
                          double *adWeight,
                          double *adF,
                          double *adFadj,
                          bool *afInBag,
                          double dStepSize,
                          unsigned long nTrain);

private:

    double dAlpha;   
};

#endif // EXPECTILE_H



