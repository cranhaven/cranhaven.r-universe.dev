// functions.h 
// Auxillary Functions Used By MCMC.cpp

#ifndef FUNCTIONS_H
#define FUNCTIONS_H

#include "List.h"
#include <cstdlib>
#include <iostream>
#include "stdio.h"
#include "stdlib.h"
#include <time.h>
#include <string>
#include <fstream>
#include <sstream>
#include "matrix.h"
#include "Rmath.h"

typedef QSMatrix<double> Matrix;

static const double PI = 3.141592653589793;

template<class Item>
struct Array2D
{
	char** names;
	Item** data;

	int m;
	int n;
};

extern void swap(int* s, int a, int b);

extern double factorial(double n);

extern double nCk(double n, double k);

extern void randPerm(int* vec, int n);

extern void rsample(int* sample, int* vec, int k, int n);

extern void quicksort(int* vec, int len);

extern Array2D<int> findR(int r, int n);

extern List<List<double>*> readDataFile(char* filename);

extern double* getWeightings(Array2D<int> array, int n);

extern List<int> atoiList(char* char_array);

extern int rand_int(int n);

extern double roundDouble(double doValue, int nPrecision);

#endif
