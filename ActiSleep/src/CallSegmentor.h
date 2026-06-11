
#include "Constants.h"
#include "Segment.h"
#include "Observations.h"
#include "Segmentor.h"



void CallSegmentorPoisson(int *Size, int *KMax, int *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood);
void CallSegmentorExponential(int *Size, int *KMax, double *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood);
void CallSegmentorBinNeg(int *Size, int *KMax, double *theta, int *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood);
void CallSegmentorNormal(int *Size, int *KMax, double *Data, double *SegBounds, int *DataComp,  int *Breakpoints, double *Parameters, double *Likelihood);
void CallSegmentorVariance(int *Size, int *KMax, double *mu, double *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood);
void CallSegmentorPoissonKeep(int *Size, int *KMax, int *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood,double *Cost, int *Pos);
void CallSegmentorExponentialKeep(int *Size, int *KMax, double *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood,double *Cost, int *Pos);
void CallSegmentorBinNegKeep(int *Size, int *KMax, double *theta, int *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood,double *Cost, int *Pos);
void CallSegmentorNormalKeep(int *Size, int *KMax, double *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood,double *Cost, int *Pos);
void CallSegmentorVarianceKeep(int *Size, int *KMax, double *mu, double *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood,double *Cost, int *Pos);
