#ifndef ELASTICNET_H
#define ELASTICNET_H

#include "Binary.h"
#include "Linear.h"

void fEBDeltaML_EN(double *DeltaML, int *Action, double *AlphaRoot, int *anyToDelete,
                   int *Used, int * Unused, double * S_out, double * Q_out, double *Alpha,
                   double *a_lmabda,double *b_Alpha, int m, int mBar);
#endif
