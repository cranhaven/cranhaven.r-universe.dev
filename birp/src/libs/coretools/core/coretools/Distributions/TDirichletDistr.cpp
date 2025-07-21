#include "TDirichletDistr.h"
namespace coretools::probdist {

Positive TDirichletDistr::density(TConstView<ZeroOpenOneClosed> x) const noexcept {
	// calculates density of a dirichlet distribution
	return calcProduct(x, _alphas) / _beta;
}

double TDirichletDistr::logDensity(TConstView<ZeroOpenOneClosed> x) const noexcept {
	// calculates log density of a dirichlet distribution
	return -_betaLog + calcLogProduct(x, _alphas);
}

void TDirichletDistr::fillRandom(std::vector<ZeroOpenOneClosed> &result) { fillRandom(_alphas, result); }
}
