#ifndef DISTRIBUTIONS_TDIRICHLETDISTR_H_
#define DISTRIBUTIONS_TDIRICHLETDISTR_H_

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Math/mathFunctions.h"
#include "coretools/Types/commonWeakTypes.h"

namespace coretools::probdist {

class TDirichletDistr {
private:
	std::vector<StrictlyPositive> _alphas;
	double _beta;
	double _betaLog;

	static bool checkSumX(TConstView<ZeroOpenOneClosed> x) noexcept {
		// x sum to 1
		double sum = 0.;
		for (auto &val : x) { sum += val; }
		if (std::abs(1. - sum) > 0.0000001) { // allow for numeric imprecision
			return false;
		}
		return true;
	}

	static double calcBetaFunction(TConstView<StrictlyPositive> alpha) {
		size_t K                = alpha.size();
		double prodGamma_alphaK = 1.;
		double sum_alphaK       = 0.;
		for (size_t k = 0; k < K; k++) {
			prodGamma_alphaK *= tgamma(alpha[k]);
			sum_alphaK += alpha[k];
		}
		return prodGamma_alphaK / tgamma(sum_alphaK);
	}

	static double calcProduct(TConstView<ZeroOpenOneClosed> x, TConstView<StrictlyPositive> alpha) {
		assert(checkSumX(x));
		assert(x.size() == alpha.size());

		// calculates prod_k x_k^(alpha_k - 1)
		size_t K    = alpha.size();
		double prod = 1.;
		for (size_t k = 0; k < K; k++) { prod *= pow(x[k], alpha[k] - 1.); }
		return prod;
	}

	static double calcLogProduct(TConstView<ZeroOpenOneClosed> x, TConstView<StrictlyPositive> alpha) {
		assert(checkSumX(x));
		assert(x.size() == alpha.size());

		// calculates log(prod_k x_k^(alpha_k - 1)) = sum_k (alpha_k - 1) * log(x_k)
		size_t K   = alpha.size();
		double sum = 0.;
		for (size_t k = 0; k < K; k++) { sum += (alpha[k] - 1.) * log(x[k]); }
		return sum;
	}

	static double calcLogBetaFunction(TConstView<StrictlyPositive> alpha) {
		size_t K                 = alpha.size();
		double sumLnGamma_alphaK = 0.;
		double sum_alphaK        = 0.;
		for (size_t k = 0; k < K; k++) {
			sumLnGamma_alphaK += gammaLog(alpha[k]);
			sum_alphaK += alpha[k];
		}
		return sumLnGamma_alphaK - gammaLog(sum_alphaK);
	}

public:
	TDirichletDistr() = default;
	TDirichletDistr(TConstView<StrictlyPositive> alphas) { set(alphas); }
	TDirichletDistr(std::vector<StrictlyPositive> alphas)
	    : _alphas(std::move(alphas)), _beta(calcBetaFunction(_alphas)), _betaLog(calcLogBetaFunction(_alphas)) {}

	void set(std::vector<StrictlyPositive> alphas) {
		_alphas  = std::move(alphas);
		_beta    = calcBetaFunction(_alphas);
		_betaLog = calcLogBetaFunction(_alphas);
	}

	void set(TConstView<StrictlyPositive> alphas) {
		_alphas.assign(alphas.begin(), alphas.end());
		_beta    = calcBetaFunction(_alphas);
		_betaLog = calcLogBetaFunction(_alphas);
	}

	static constexpr std::string_view name = "dirichlet";
	static constexpr bool isDiscrete() { return false; };
	static constexpr bool isMultiVariate() { return true; };

	// static function for external use
	static Positive density(TConstView<ZeroOpenOneClosed> x, TConstView<StrictlyPositive> alphas) noexcept {
		// calculates density of a dirichlet distribution
		return calcProduct(x, alphas) / calcBetaFunction(alphas);
	}

	static double logDensity(TConstView<ZeroOpenOneClosed> x, TConstView<StrictlyPositive> alphas) noexcept {
		// calculates log density of a dirichlet distribution
		return -calcLogBetaFunction(alphas) + calcLogProduct(x, alphas);
	}

	static void fillRandom(TConstView<StrictlyPositive> alphas, std::vector<ZeroOpenOneClosed> &result) {
		instances::randomGenerator().fillDirichletRandom(alphas, result);
	}

	// member functions
	Positive density(TConstView<ZeroOpenOneClosed> x) const noexcept;
	double logDensity(TConstView<ZeroOpenOneClosed> x) const noexcept;
	void fillRandom(std::vector<ZeroOpenOneClosed> &result);
	size_t size() const noexcept { return _alphas.size(); }
	double beta() const noexcept { return _beta; };
	const std::vector<StrictlyPositive> &alphas() const noexcept { return _alphas; }
	double betaLog() const noexcept { return _betaLog; };
	double calcProduct(TConstView<ZeroOpenOneClosed> x) const noexcept { return calcProduct(x, _alphas); }
	double calcLogProduct(TConstView<ZeroOpenOneClosed> x) const noexcept { return calcLogProduct(x, _alphas); }
};
} // namespace coretools::probdist
#endif
