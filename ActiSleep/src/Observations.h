/*
 *  Observations.h
 *  Segments
 *
 *  Created by Michel Koskas on 10/01/11.
 *  Copyright 2011 INRA, INA. All rights reserved.
 *
 */


#ifndef _Observations_h_
#define _Observations_h_


#define Separateur '\t'
#define FinDeLigne '\n'

#include <iostream>
#include <fstream>
#include <stdlib.h>

#include "GeneralFunctions.h"
#include "Constants.h"
#include "MyVector.h"


template<typename T>
class Observations
{
public:
  MyVector<T> y;
  MyVector<int> l;
  T MinData;
  T MaxData;
  double Mean;
  double Var;
  Observations();
  Observations(MyVector<T> &v, MyVector<int> &ml);
  Observations(MyVector<int> &v, MyVector<int> &ml, bool t=true);
  void MeanVarSubsection(int start, int end, double* m, double* v);
  void ComputeMinMax();
  void ComputeMeanVar();
};

template<typename T>
void Observations<T>::ComputeMinMax()
{
	if (y.size() == 0)
	{
		MinData = PLUS_INFINITY;
		MaxData = MINUS_INFINITY;
		return;
	}
  MinData = y[0];
  MaxData = y[0];
  int n = y.size();
  for (int i = 0; i < n; i++)
  {
    if (y[i] < MinData)
      MinData = y[i];
    if (y[i] > MaxData)
      MaxData = y[i];
  }
}

template<typename T>
void Observations<T>::MeanVarSubsection(int start, int end, double* m, double* v)
{
	if (y.size() == 0 || end<start)
	{
		*m = PLUS_INFINITY;
		*v = 0;
		return;
	}
  //int length = end-start;
  int length = 0;
  T Sum = 0;
  double Square = 0;
  for (int i = start; i < end; i++)
  {
  	length += l[i];
    Sum += y[i]*l[i];
  }
  *m = ((double) Sum)/((double) length);
  for (int i=start; i<end; i++)
    Square += l[i]*(y[i]-*m)*(y[i]-*m);
  *v = Square /(length-1);
}

template<typename T>
void Observations<T>::ComputeMeanVar()
{
	if (y.size() == 0)
	{
		Mean = PLUS_INFINITY;
		Var = 0;
		return;
	}
  Mean = 0;
  Var = 0;
  int tl = 0;
  int n = y.size();
  for (int i = 0; i < n; i++)
  {
  	tl += l[i];
    Mean += y[i];
  }  
  Mean /= tl;
  for (int i=0; i<n; i++)
    Var += l[i]*(y[i]-Mean)*(y[i]-Mean);
  Var /= (tl-1);
}



template<typename T>
Observations<T>::Observations()
{
}

template<typename T>
Observations<T>::Observations(MyVector<T> &v, MyVector<int> &ml)
{
  y = v;
  l = ml;
  ComputeMinMax();
  ComputeMeanVar();
}
template<typename T>
Observations<T>::Observations(MyVector<int> &v, MyVector<int> &ml, bool t)
{
  y.clear();
  l.clear();
  for (int i=0; i<v.size(); i++)
  {
    y.push_back(v[i]);
    l.push_back(ml[i]);
  }  
  ComputeMinMax();
  ComputeMeanVar();
}



#endif
