/*
 *  GeneralFunctions.cpp
 *  Segments
 *
 *  Created by Michel Koskas on 22/08/11.
 *  Copyright 2011 INRA, INA. All rights reserved.
 *
 */

#include <fstream>
#include <iostream>
#include <string>
#include <stdlib.h>
#include "GeneralFunctions.h"
#include "MyVector.h"


// This function says whether the character x is a number or not
bool IsDigit(char &x)
{
  if (x < '0')
    return false;
  if (x > '9')
    return false;
  return true;
}

MyVector<int> IntersectLists(const MyVector<int> &A, const MyVector<int> &B)
{
	MyVector<int> Res;
	MyVector<int>::const_iterator IA = A.begin(), IB = B.begin();
	while ((IA != A.end()) && (IB != B.end()))
		if (*IA < *IB)
			IA++;
		else if (*IB < *IA)
			IB++;
		else
		{
			Res.push_back(*IA);
			IA++;
			IB++;
		}
	return Res;
}

MyVector<int> GetBreakpoints(int k, int n, int** M)
{
  MyVector<int> TheBreakpoints;
  if (k>1)
    {
      int Prec = M[k-1][n-1];
      TheBreakpoints.push_back(Prec+1);
      if (k>2)
	for (int i=(k-2); i>=1; i--)
	  {
	    TheBreakpoints.push_back(M[i][Prec]+1);
	    Prec = M[i][Prec];
	  }
    }
  TheBreakpoints.push_back(0);
  TheBreakpoints.reverse();
  TheBreakpoints.push_back(n);
  TheBreakpoints.sort();
  return TheBreakpoints;
}

MyVector<double> GetParameters(int k, int n, int** M, double** Par)
{
  MyVector<double> Parameters;
  Parameters.push_back(Par[k-1][n-1]);
  if (k>1)
    {
      int Prec = M[k-1][n-1];
      if (k>2)
	    for (int i=(k-2); i>=1; i--)
	      {
	        Parameters.push_back(Par[i][Prec]);
	        Prec = M[i][Prec];
	      }
      Parameters.push_back(Par[0][Prec]);
    }
  Parameters.reverse();
  return Parameters;
}


