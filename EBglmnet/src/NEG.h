#ifndef NEG_H
#define NEG_H

#include "Binary.h"
#include "Linear.h"

void fEBDeltaML_NEG(double *DeltaML, int *Action, double *AlphaRoot, int *anyToDelete,
                     int *Used, int * Unused, double * S_out, double * Q_out, double *Alpha,
                     double *a_gamma, double *b_gamma, int m, int mBar);

#endif
