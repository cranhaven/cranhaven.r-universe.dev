/*
 * KernelGauss.h
 *
 *  Created on: Jul 9, 2019
 *      Author: Christoph Koesner
 */

#ifndef SRC_KERNELGAUSS_H_
#define SRC_KERNELGAUSS_H_

#include "Kernel.h"


class KernelGauss : public Kernel{
public:
	KernelGauss();
	KernelGauss(double);
	double calc(double, double);
private:
  double sigma;
};

#endif /* SRC_KERNELGAUSS_H_ */
