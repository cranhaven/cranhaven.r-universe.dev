#include "Observations.h"
#include "Segmentor.h"
#include "Trinome.h"
#include "Poisson.h"
#include "Exp.h"
#include "BinNegative.h"
#include "Variance.h"
#include "Segment.h"
#include "MyVector.h"


 void CallSegmentorPoisson(int *Size, int *KMax, int *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood)
  {
    int K= *KMax;
    int n = *Size;
    MyVector<int> MyData(n,0);
    MyVector<int> MyDataComp(n,0);
    for (int i=0; i<n; i++)
		{
			MyData[i] = Data[i];
			MyDataComp[i] = DataComp[i];
		}
    Observations<int> LesObservations(MyData, MyDataComp, true);
    Poisson MBg(0, 0, 0);
    Poisson MBgam(0, 0, 0);
    double min = SegBounds[0];
    double max = SegBounds[1];
    Segment Sp;
    if (min == max)
    	Sp = Segment(LesObservations.MinData, LesObservations.MaxData);
    else
    	Sp = Segment(min, max);
    MultiSegment S(Sp);
    Segmentor<Poisson, Poisson,  int> TheSegmentor(LesObservations, K, MBg, MBgam, &S);
    for (int k=0; k<K; k++)
    {
			MyVector<int> Temp=GetBreakpoints(k+1, n, TheSegmentor.GetM());
			MyVector<double> TempParam=GetParameters(k+1, n, TheSegmentor.GetM(), TheSegmentor.GetPar());
			for (int i=0; i<(k+1); i++)
			{
				Breakpoints[k*K+i] = Temp[i+1];
				Parameters[k*K+i] = TempParam[i];
			}
	 		Likelihood[k] = TheSegmentor.GetC()[k][n-1];
    }
  return;
}

 void CallSegmentorExponential(int *Size, int *KMax, double *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood)
  {
    int K= *KMax;
    int n = *Size;
    MyVector<double> MyData(n,0);
    MyVector<int> MyDataComp(n,0);
    for (int i=0; i<n; i++)
		{
			MyData[i] = Data[i];
			MyDataComp[i] = DataComp[i];
		}
    Observations<double> LesObservations(MyData, MyDataComp);
    Exponential MBg(0, 0, 0);
    Exponential MBgam(0, 0, 0);
    double MSeg = max(1.01,1.01/LesObservations.MinData);
    double mSeg = 1.01/LesObservations.MaxData;
    double min = SegBounds[0];
    double max = SegBounds[1];
    Segment Sp;
    if (min == max)
    	Sp = Segment(mSeg, MSeg);
    else
    	Sp = Segment(min, max);
    MultiSegment S(Sp);
    Segmentor<Exponential, Exponential,  double> TheSegmentor(LesObservations, K, MBg, MBgam, &S);
    for (int k=0; k<K; k++)
    {
			MyVector<int> Temp=GetBreakpoints(k+1, n, TheSegmentor.GetM());
			MyVector<double> TempParam=GetParameters(k+1, n, TheSegmentor.GetM(), TheSegmentor.GetPar());
			for (int i=0; i<(k+1); i++)
			{
				Breakpoints[k*K+i] = Temp[i+1];
				Parameters[k*K+i] = TempParam[i];
			}
	 		Likelihood[k] = TheSegmentor.GetC()[k][n-1];
    }
  return;
}


 void CallSegmentorPoissonKeep(int *Size, int *KMax, int *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood, double *Cost, int *Pos)
  {
    int K= *KMax;
    int n = *Size;
    MyVector<int> MyData(n,0);
    MyVector<int> MyDataComp(n,0);
    for (int i=0; i<n; i++)
		{
			MyData[i] = Data[i];
			MyDataComp[i] = DataComp[i];
		}
    Observations<int> LesObservations(MyData, MyDataComp, true);
    Poisson MBg(0, 0, 0);
    Poisson MBgam(0, 0, 0);
    double min = SegBounds[0];
    double max = SegBounds[1];
    Segment Sp;
    if (min == max)
    	Sp = Segment(LesObservations.MinData, LesObservations.MaxData);
    else
    	Sp = Segment(min, max);
    MultiSegment S(Sp);
    Segmentor<Poisson, Poisson,  int> TheSegmentor(LesObservations, K, MBg, MBgam, &S);
    double **C = TheSegmentor.GetC();
    int **Po = TheSegmentor.GetM();
    for (int k=0; k<K; k++)
    {
			MyVector<int> Temp=GetBreakpoints(k+1, n, TheSegmentor.GetM());
			MyVector<double> TempParam=GetParameters(k+1, n, TheSegmentor.GetM(), TheSegmentor.GetPar());
			for (int i=0; i<(k+1); i++)
			{
				Breakpoints[k*K+i] = Temp[i+1];
				Parameters[k*K+i] = TempParam[i];
			}
	 for (int j=0; j<n; j++)
	 {
				Cost[k*n+j] = C[k][j];
				Pos[k*n+j] = Po[k][j];
	 }
	 		Likelihood[k] = TheSegmentor.GetC()[k][n-1];
    }
  return;
}

 void CallSegmentorExponentialKeep(int *Size, int *KMax, double *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood, double *Cost, int *Pos)
  {
    int K= *KMax;
    int n = *Size;
    MyVector<double> MyData(n,0);
    MyVector<int> MyDataComp(n,0);
    for (int i=0; i<n; i++)
		{
			MyData[i] = Data[i];
			MyDataComp[i] = DataComp[i];
		}
    Observations<double> LesObservations(MyData, MyDataComp);
    Exponential MBg(0, 0, 0);
    Exponential MBgam(0, 0, 0);
    double MSeg = max(1.01,1.01/LesObservations.MinData);
    double mSeg = 1.01/LesObservations.MaxData;
    double min = SegBounds[0];
    double max = SegBounds[1];
    Segment Sp;
    if (min == max)
    	Sp = Segment(mSeg, MSeg);
    else
    	Sp = Segment(min, max);
    MultiSegment S(Sp);
    Segmentor<Exponential, Exponential,  double> TheSegmentor(LesObservations, K, MBg, MBgam, &S);
    double **C = TheSegmentor.GetC();
    int **Po = TheSegmentor.GetM();
    for (int k=0; k<K; k++)
    {
			MyVector<int> Temp=GetBreakpoints(k+1, n, TheSegmentor.GetM());
			MyVector<double> TempParam=GetParameters(k+1, n, TheSegmentor.GetM(), TheSegmentor.GetPar());
			for (int i=0; i<(k+1); i++)
			{
				Breakpoints[k*K+i] = Temp[i+1];
				Parameters[k*K+i] = TempParam[i];
			}
	 for (int j=0; j<n; j++)
	 {
				Cost[k*n+j] = C[k][j];
				Pos[k*n+j] = Po[k][j];
	 }
	 		Likelihood[k] = TheSegmentor.GetC()[k][n-1];
    }
  return;
}



 void CallSegmentorBinNeg(int *Size, int *KMax, double *theta, int *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood)
{
    int K= *KMax;
    int n = *Size;
    double Theta = *theta;
    bool b = true;
    MyVector<int> MyData(n,0);
    MyVector<int> MyDataComp(n,0);
    for (int i=0; i<n; i++)
		{
			MyData[i] = Data[i];
			MyDataComp[i] = DataComp[i];
		}
    Observations<int> LesObservations(MyData, MyDataComp, true);
    BinNegative MBg(0, 0, 0, b);
    BinNegative MBgam(0, 0, Theta, b);
    double min = SegBounds[0];
    double max = SegBounds[1];
    Segment Sp;
    if (min == max)
    	Sp = Segment(0, 1);
    else
    	Sp = Segment(min, max);
    MultiSegment S(Sp);
    Segmentor<BinNegative, BinNegative,  int> TheSegmentor(LesObservations, K, MBg, MBgam, &S);
    for (int k=0; k<K; k++)
    {
         MyVector<int> Temp=GetBreakpoints(k+1, n, TheSegmentor.GetM());
         MyVector<double> TempParam=GetParameters(k+1, n, TheSegmentor.GetM(), TheSegmentor.GetPar());
         for (int i=0; i<(k+1); i++)
         {
	      Breakpoints[k*K+i] = Temp[i+1];
	      Parameters[k*K+i] = TempParam[i];
	 }
	 Likelihood[k] = TheSegmentor.GetC()[k][n-1];
    }
  return;
}

 void CallSegmentorBinNegKeep(int *Size, int *KMax, double *theta, int *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood, double *Cost, int *Pos)
{
    int K= *KMax;
    int n = *Size;
    double Theta = *theta;
    bool b = true;
    MyVector<int> MyData(n,0);
    MyVector<int> MyDataComp(n,0);
    for (int i=0; i<n; i++)
		{
			MyData[i] = Data[i];
			MyDataComp[i] = DataComp[i];
		}
    Observations<int> LesObservations(MyData, MyDataComp, true);
    BinNegative MBg(0, 0, 0,b);
    BinNegative MBgam(0, 0, Theta,b);
    double min = SegBounds[0];
    double max = SegBounds[1];
    Segment Sp;
    if (min == max)
    	Sp = Segment(0, 1);
    else
    	Sp = Segment(min, max);
    MultiSegment S(Sp);
    Segmentor<BinNegative, BinNegative,  int> TheSegmentor(LesObservations, K, MBg, MBgam, &S);
    double **C = TheSegmentor.GetC();
    int **Po = TheSegmentor.GetM();
    for (int k=0; k<K; k++)
    {
         MyVector<int> Temp=GetBreakpoints(k+1, n, TheSegmentor.GetM());
         MyVector<double> TempParam=GetParameters(k+1, n, TheSegmentor.GetM(), TheSegmentor.GetPar());
         for (int i=0; i<(k+1); i++)
         {
	      Breakpoints[k*K+i] = Temp[i+1];
	      Parameters[k*K+i] = TempParam[i];
	 }
	 for (int j=0; j<n; j++)
	 {
				Cost[k*n+j] = C[k][j];
				Pos[k*n+j] = Po[k][j];
	 }
	 Likelihood[k] = TheSegmentor.GetC()[k][n-1];
    }
  return;
}

void CallSegmentorNormal(int *Size, int *KMax, double *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood)
{
    int K= *KMax;
    int n = *Size;
    MyVector<double> MyData(n,0);
    MyVector<int> MyDataComp(n,0);
    for (int i=0; i<n; i++)
    {
			MyData[i] = Data[i];
			MyDataComp[i] = DataComp[i];
		}
    Observations<double> LesObservations(MyData, MyDataComp);
    Trinome MBg(0, 0, 0);
    Trinome MBgam(0, 0, 0);
    double min = SegBounds[0];
    double max = SegBounds[1];
    Segment Sp;
    if (min == max)
    	Sp = Segment(LesObservations.MinData, LesObservations.MaxData);
    else
    	Sp = Segment(min, max);
    MultiSegment S(Sp);
    Segmentor<Trinome, Trinome,  double> TheSegmentor(LesObservations, K, MBg, MBgam, &S);
    for (int k=0; k<K; k++)
    {
            MyVector<int> Temp=GetBreakpoints(k+1, n, TheSegmentor.GetM());
            MyVector<double> TempParam=GetParameters(k+1, n, TheSegmentor.GetM(), TheSegmentor.GetPar());
            for (int i=0; i<(k+1); i++)
            {
	      Breakpoints[k*K+i] = Temp[i+1];
	      Parameters[k*K+i] = TempParam[i];
	    }
	    Likelihood[k] = TheSegmentor.GetC()[k][n-1];
    }
}

void CallSegmentorNormalKeep(int *Size, int *KMax, double *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood, double *Cost, int* Pos)
{
    int K= *KMax;
    int n = *Size;
    MyVector<double> MyData(n,0);
    MyVector<int> MyDataComp(n,0);
    for (int i=0; i<n; i++)
    {
			MyData[i] = Data[i];
			MyDataComp[i] = DataComp[i];
		}
    Observations<double> LesObservations(MyData, MyDataComp);
    Trinome MBg(0, 0, 0);
    Trinome MBgam(0, 0, 0);
    double min = SegBounds[0];
    double max = SegBounds[1];
    Segment Sp;
    if (min == max)
    	Sp = Segment(LesObservations.MinData, LesObservations.MaxData);
    else
    	Sp = Segment(min, max);
    MultiSegment S(Sp);
    Segmentor<Trinome, Trinome,  double> TheSegmentor(LesObservations, K, MBg, MBgam, &S);
    double **C = TheSegmentor.GetC();
    int **Po = TheSegmentor.GetM();
    for (int k=0; k<K; k++)
    {
            MyVector<int> Temp=GetBreakpoints(k+1, n, TheSegmentor.GetM());
            MyVector<double> TempParam=GetParameters(k+1, n, TheSegmentor.GetM(), TheSegmentor.GetPar());
            for (int i=0; i<(k+1); i++)
            {
	      Breakpoints[k*K+i] = Temp[i+1];
	      Parameters[k*K+i] = TempParam[i];
	    }
	 for (int j=0; j<n; j++)
	 {
				Cost[k*n+j] = C[k][j];
				Pos[k*n+j] = Po[k][j];
	 }
	    Likelihood[k] = TheSegmentor.GetC()[k][n-1];
    }
}

void CallSegmentorVariance(int *Size, int *KMax, double *Mmu, double *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood)
{
    int K= *KMax;
    int n = *Size;
    double mu = *Mmu;
    MyVector<double> MyData(n,0);
    MyVector<int> MyDataComp(n,0);
    for (int i=0; i<n; i++)
    {
			MyData[i] = Data[i];
			MyDataComp[i] = DataComp[i];
		}
    Observations<double> LesObservations(MyData, MyDataComp);
    Variance MVg(0, 0, 0,0);
    Variance MVgam(0, 0, 0, mu);
    double maax = std::max((LesObservations.MaxData-mu)*(LesObservations.MaxData-mu),(LesObservations.MinData-mu)*(LesObservations.MinData-mu));
    double min = SegBounds[0];
    double max = SegBounds[1];
    Segment Sp;
    if (min == max)
    	Sp = Segment(0, maax);
    else
    	Sp = Segment(min, max);
    MultiSegment S(Sp);
    Segmentor<Variance, Variance,  double> TheSegmentor(LesObservations, K, MVg, MVgam, &S);
    for (int k=0; k<K; k++)
    {
            MyVector<int> Temp=GetBreakpoints(k+1, n, TheSegmentor.GetM());
            MyVector<double> TempParam=GetParameters(k+1, n, TheSegmentor.GetM(), TheSegmentor.GetPar());
            for (int i=0; i<(k+1); i++)
            {
	      Breakpoints[k*K+i] = Temp[i+1];
	      Parameters[k*K+i] = TempParam[i];
	    }
	    Likelihood[k] = TheSegmentor.GetC()[k][n-1];
    }
}

void CallSegmentorVarianceKeep(int *Size, int *KMax, double *Mmu, double *Data, double *SegBounds, int *DataComp, int *Breakpoints, double *Parameters, double *Likelihood, double *Cost, int *Pos)
{
    int K= *KMax;
    int n = *Size;
    double mu = *Mmu;
    MyVector<double> MyData(n,0);
    MyVector<int> MyDataComp(n,0);
    for (int i=0; i<n; i++)
    {
			MyData[i] = Data[i];
			MyDataComp[i] = DataComp[i];
		}
    Observations<double> LesObservations(MyData, MyDataComp);
    Variance MVg(0, 0, 0,0);
    Variance MVgam(0, 0, 0, mu);
    double maax = std::max((LesObservations.MaxData-mu)*(LesObservations.MaxData-mu),(LesObservations.MinData-mu)*(LesObservations.MinData-mu));
    double min = SegBounds[0];
    double max = SegBounds[1];
    Segment Sp;
    if (min == max)
    	Sp = Segment(0, maax);
    else
    	Sp = Segment(min, max);
    MultiSegment S(Sp);
    Segmentor<Variance, Variance,  double> TheSegmentor(LesObservations, K, MVg, MVgam, &S);
    double **C = TheSegmentor.GetC();
    int **Po = TheSegmentor.GetM();
    for (int k=0; k<K; k++)
    {
            MyVector<int> Temp=GetBreakpoints(k+1, n, TheSegmentor.GetM());
            MyVector<double> TempParam=GetParameters(k+1, n, TheSegmentor.GetM(), TheSegmentor.GetPar());
            for (int i=0; i<(k+1); i++)
            {
	      Breakpoints[k*K+i] = Temp[i+1];
	      Parameters[k*K+i] = TempParam[i];
	    }
	 for (int j=0; j<n; j++)
	 {
				Cost[k*n+j] = C[k][j];
				Pos[k*n+j] = Po[k][j];
	 }
	    Likelihood[k] = TheSegmentor.GetC()[k][n-1];
    }
}
