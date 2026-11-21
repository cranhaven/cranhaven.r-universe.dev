/*
 * KernelGauss.cpp
 *
 *  Created on: Jul 9, 2019
 *      Author: Christoph Koesner
 */

#include <Rcpp.h>
#include "KernelGauss.h"
#include <cmath>

using namespace std;

KernelGauss::KernelGauss(){
	sigma = 1;
}


KernelGauss::KernelGauss(double sigma2){
	sigma = sigma2;
}

double KernelGauss::calc(double a, double b){
	return exp(- (a-b)*(a-b) / (2*sigma*sigma));
}
