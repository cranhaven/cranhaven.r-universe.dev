/*
 * KernelHermite.h
 *
 *  Created on: Jul 9, 2019
 *      Author: Christoph Koesner
 */

#ifndef SRC_KERNELHERMITE_H_
#define SRC_KERNELHERMITE_H_

#include "Kernel.h"


/**
* implementation of the hermite kernel
*/
class KernelHermite : public Kernel{
private:
  double sigma;
  int d;
  double hermitePolynomial(double x, int d);
  int fact(int n);

public:
  KernelHermite();
  KernelHermite(double, double);
  double calc(double, double);
};

#endif /* SRC_KERNELHERMITE_H_ */

