//
// Created by madleina on 28.11.23.
//

#include "TPriorMultivariateNormal.h"
#include "coretools/Math/mathConstants.h"

namespace stattools::prior::impl::mvn {

DimMVN::DimMVN(size_t Dim) {
	D                = Dim;
	minusDdiv2Log2Pi = -(double)D / 2. * log(two_pi);
	sqrt2piD         = sqrt(pow(two_pi, (double)D));
}

[[nodiscard]] size_t numberOfElementsInTriangularMatrix_Diagonal0(size_t D) {
	// this is for a triangular matrix where the diagonal is also zero
	return (D - 1) * D / 2;
}

[[nodiscard]] size_t indexMrs(size_t r, size_t s) {
	// this is for a triangular matrix where the diagonal is ALSO zero
	assert(r > 0);
	return r * (r - 1) / 2 + s;
}

std::shared_ptr<coretools::TNamesStrings> generateDimNamesMrs(const std::shared_ptr<coretools::TNamesEmpty> &DimName) {
	// DimName corresponds to the row-/colnames of the matrix -> now we want every combination of it (for lower
	// triangular matrix)
	std::vector<std::string> namesMrs;
	for (size_t r = 1; r < DimName->size(); r++) {
		for (size_t s = 0; s < r; s++) { namesMrs.push_back((*DimName)[r] + "_" + (*DimName)[s]); }
	}
	assert(namesMrs.size() == numberOfElementsInTriangularMatrix_Diagonal0(DimName->size()));
	return std::make_shared<coretools::TNamesStrings>(namesMrs);
}

double calcLLRatioUpdateM(size_t N, size_t D, double OldM, double NewM, double Sum) {
	return (double)N * (double)D * log(OldM / NewM) + 0.5 * (1. / (OldM * OldM) - 1. / (NewM * NewM)) * Sum;
}

void normalizeSumMLEMu(std::vector<double> &Sums, double TotalNumElements) {
	for (double &Sum : Sums) { Sum /= TotalNumElements; }
}

void normalizeSumMLESigma(arma::mat &Sums, double TotalNumElements) {
	for (size_t j = 0; j < Sums.n_rows; j++) {
		for (size_t k = 0; k < Sums.n_cols; k++) { Sums(j, k) /= TotalNumElements; }
	}
}

arma::mat calculateMFromSigma(const arma::mat &Sigma) {
	arma::mat invSigma;
	arma::mat SigmaNoisy = Sigma;
	SigmaNoisy.diag() += 0.00000001;
	if (!arma::inv_sympd(invSigma, SigmaNoisy)) {
		// if it still doesn't work -> throw. Could increase noise to diagonal
		DEVERROR("could not take inverse of Sigma!");
	}
	arma::mat M;
	if (!arma::chol(M, invSigma, "lower")) {
		// failed to do Cholesky decomposition, throw error
		coretools::cout << "Sigma: " << std::endl;
		coretools::cout << Sigma << std::endl;
		coretools::cout << "invSigma: " << std::endl;
		coretools::cout << invSigma << std::endl;
		DEVERROR("could not do Cholesky decomposition of Sigma!");
	}
	return M;
}

std::pair<size_t, size_t> getCoordinatesFromLinearIndex_Mrs(size_t Index) {
	size_t K   = std::ceil((1. + sqrt(1. + 8. * ((double)Index + 1.))) / 2.);
	size_t row = K - 1;
	size_t col = Index - row * (row - 1) / 2;
	return {row, col};
}

} // namespace stattools::prior::impl::mvn
