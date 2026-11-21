/*
 * Kernel.h
 *
 *  Created on: Jul 9, 2019
 *      Author: Christoph Koesner
 */

#ifndef SRC_KERNEL_H_
#define SRC_KERNEL_H_

/**
* abstract class for kernel method
*/
class Kernel{
public:
  virtual double calc(double, double) = 0;
  virtual ~Kernel();
};

inline Kernel::~Kernel(){}

#endif /* SRC_KERNEL_H_ */
