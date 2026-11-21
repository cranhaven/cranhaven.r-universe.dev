/*
 * KernelHermite.cpp
 *
 *  Created on: Jul 9, 2019
 *      Author: Christoph Koesner
 */

#include <Rcpp.h>
#include "KernelHermite.h"
#include <cmath>

using namespace std;

/**
 * implementation of the hermite kernel
 */
KernelHermite::KernelHermite(){
	d = 3;
	sigma = 1;
}
// fixed biy where these were the wrong way around
KernelHermite::KernelHermite(double sigma2,double d2){
	d = d2;
	sigma = sigma2;
}

double KernelHermite::calc(double a, double b) {
	double sum = 0;
	int i = 0;
	for (i = 0; i <= d; i++) {
		sum += exp(-(a * a + b * b) / (2 * sigma * sigma))
				* hermitePolynomial(a / sigma, i)
				* hermitePolynomial(b / sigma, i) / pow(2, i) / fact(i);
	}
	return sum;
}
/**
 * realization of the hermite polynomial by its recursive definition.
 */
double KernelHermite::hermitePolynomial(double x, int d) {
	double ret_val;
	if (d < 0) {
		return 0;
	}
	switch (d) {
	case 0:
		ret_val = 1;
		break;
	case 1:
		ret_val = 2 * x;
		break;
	case 2:
		ret_val = 4 * x * x - 2;
		break;
	case 3:
		ret_val = 8 * x * x * x - 12 * x;
		break;
	default:
		ret_val = 2 * x * hermitePolynomial(x, d - 1)
				- 2 * (d - 1) * hermitePolynomial(x, d - 2);
	}
	return ret_val;
}
/**
 * implementation of the factorial
 */
int KernelHermite::fact(int n){
	int i = 1, fact = 1;
	for(i = 2; i <= n; i++){
		fact = fact * i;
	}
	return fact;
}

