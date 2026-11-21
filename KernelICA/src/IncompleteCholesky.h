/*
 * hermite_low_rank.h
 *
 *  Created on: Mar 9, 2020
 *      Author: Christoph Koesner
 */

#ifndef SRC_HERMITE_LOW_RANK_H_
#define SRC_HERMITE_LOW_RANK_H_

#include <RcppEigen.h>
#include "Kernel.h"

struct choleskyReturnVal{
	Eigen::MatrixXd L;
	Eigen::PermutationMatrix<Eigen::Dynamic, Eigen::Dynamic> perm;
};

template<typename Derived>
choleskyReturnVal choleskyLowRank(const Eigen::MatrixBase<Derived>&,
		Kernel&, double);

#endif /* SRC_HERMITE_LOW_RANK_H_ */
