//
// Created by madleina on 28.11.23.
//

#include "TPriorMultivariateNormalMixedModel.h"

namespace stattools::prior::impl::mvn_mix {
void handleEMMaximizationOneIteration_updateSigma(size_t k, size_t D, const std::vector<arma::mat> &sumXpiMinusMuSquare,
												  const TDataVector<double, size_t> &Weights,
												  arma::mat &EM_update_Sigma_temp) {
	// add to sum (x_pi - mu_k)(x_pi - mu_k)^T * weights_ki
	for (size_t d = 0; d < D; d++) {
		for (size_t e = 0; e < D; e++) { EM_update_Sigma_temp(d, e) += sumXpiMinusMuSquare[k](d, e) * Weights[k]; }
	}
}

void updateEMParametersOneIteration_Sigma(arma::mat &Sigma, size_t D, size_t NumParametersBelow,
										  arma::mat &EM_update_Sigma_temp, double EM_update_Weights_temp) {
	Sigma.zeros(D, D);
	for (size_t d = 0; d < D; d++) {
		for (size_t e = 0; e < D; e++) {
			Sigma(d, e) = EM_update_Sigma_temp(d, e) / ((double)NumParametersBelow * EM_update_Weights_temp);
		}
	}
}
} // namespace stattools::prior::impl::mvn_mix
