#ifndef _telefit_COVS_H
#define _telefit_COVS_H

#include <RcppArmadillo.h>

void maternCov( arma::mat & cov, const arma::mat & d, double scale, double range,
				double smoothness, double nugget );

void maternArray( arma::vec & d, double scale, double range,
			      double smoothness, double nugget );

#endif
