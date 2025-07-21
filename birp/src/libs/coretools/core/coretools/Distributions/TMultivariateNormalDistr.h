#ifndef DISTRIBUTIONS_TMULTIVARIATENORMALDISTR_H_
#define DISTRIBUTIONS_TMULTIVARIATENORMALDISTR_H_

#include "coretools/arma_include.h"
#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Main/TError.h"
#include "coretools/Types/commonWeakTypes.h"

namespace coretools::probdist {

template<typename TypeX, bool AllowDegenerate = false> class TMultivariateNormalDistr {
	// TypeX is the type of the input data -> typically std::vector<double>, but can also be arma::vec or array
	// AllowDegenerate: if true, then also for degenerate covariance matrices, a density will be calculated using
	//   the disintegration theorem (https://en.wikipedia.org/wiki/Multivariate_normal_distribution#Degenerate_case)
private:
	std::vector<double> _mu;
	arma::mat _Sigma;

	// temporary variables
	arma::mat _invSigma;
	arma::mat _L;
	double _denom    = 0.0;
	double _denomLog = 0.0;

	static bool _checkDimensions(size_t k, TConstView<double> Mu, const arma::mat &Sigma) {
		return k == Mu.size() && k == Sigma.n_rows && k == Sigma.n_cols;
	}

	static arma::mat _calcInvSigma(const arma::mat &Sigma) {
		if constexpr (AllowDegenerate) {
			return arma::pinv(Sigma); // pseudo-inverse (generalized inverse)
		} else {
			return arma::inv_sympd(Sigma);
		}
	}

	static double _calcDetSigma(const arma::mat &Sigma) {
		if constexpr (AllowDegenerate) {
			// calculate pseudo-determinant: product of all non-zero eigenvalues
			const arma::vec eigVals = arma::svd(Sigma);
			double prod             = 1.0;
			for (size_t i = 0; i < eigVals.size(); i++) {
				const auto absVal = abs(eigVals[i]);
				if (absVal >= std::numeric_limits<double>::epsilon()) { prod *= absVal; }
			}
			return prod;
		} else {
			return arma::det(Sigma);
		}
	}

	static double _calcMahalanobis(const TypeX &x, TConstView<double> Mu, const arma::mat &InvSigma) {
		double tot = 0.0;
		for (size_t j = 0; j < x.size(); j++) {
			double s = 0.0;
			for (size_t k = 0; k < x.size(); k++) { s += (x[k] - Mu[k]) * InvSigma(k, j); }
			tot += s * (x[j] - Mu[j]);
		}
		return tot;
	}

	static double _calcDenomDensity(size_t k, const arma::mat &Sigma) {
		return std::sqrt(std::pow(two_pi, k) * _calcDetSigma(Sigma));
	}

	static double _calcDenomLogDensity(size_t k, const arma::mat &Sigma) {
		return 0.5 * ((double)k * log_two_pi + log(_calcDetSigma(Sigma)));
	}

	static TypeX _sample(size_t K, TConstView<double> Mu, const arma::mat &L) {
		// according to Numerical Recipes 3rd edition, pp.378
		// L is the Cholesky decomposition of Sigma into a left triangular matrix L times its transpose Sigma = LL^T

		// Get a vector y of independent random univariate normal deviates with mean = 0 and var = 1.
		std::vector<double> y(K);
		for (size_t k = 0; k < K; k++) { y[k] = coretools::instances::randomGenerator().getNormalRandom(0, 1); }

		// To get a multivariate normal deviate x, calculate x = Ly + mu
		TypeX x(K);
		for (size_t k = 0; k < K; k++) {
			// calculate Ly (matrix multiplication)
			double sum = 0.;
			for (size_t c = 0; c <= k; c++) { sum += L(k, c) * y[c]; }

			// calculate x[k] = Ly + Mus[k]
			x[k] = sum + Mu[k];
		}
		return x;
	}

public:
	TMultivariateNormalDistr() {
		_Sigma.eye(1, 1);
		_mu.resize(1, 0.0);
		set(_mu, _Sigma);
	};

	TMultivariateNormalDistr(TConstView<double> Mu, const arma::mat &Sigma) { set(Mu, Sigma); }

	template<bool AddNoiseDiag = false> void set(TConstView<double> Mu, const arma::mat &Sigma) {
		DEBUG_ASSERT(_checkDimensions(Mu.size(), Mu, Sigma));
		_mu.assign(Mu.begin(), Mu.end());
		_Sigma = Sigma;

		if constexpr (AddNoiseDiag) { _Sigma.diag() += 1e-10; }

		_invSigma = _calcInvSigma(_Sigma);
		_L        = arma::chol(_Sigma, "lower");
		_denom    = _calcDenomDensity(Mu.size(), _Sigma);
		_denomLog = _calcDenomLogDensity(Mu.size(), _Sigma);
	}

	static constexpr std::string_view name = "MVN";
	static constexpr bool isDiscrete() { return false; };
	static constexpr bool isMultiVariate() { return true; };
	static constexpr std::pair<double, double> support() {
		return std::make_pair(std::numeric_limits<double>::lowest(), std::numeric_limits<double>::max());
	}

	// static function for external use
	static Positive density(const TypeX &x, TConstView<double> Mu, const arma::mat &Sigma) noexcept(noDebug) {
		// calculates density of a multivariate normal distribution
		const auto k = x.size();
		DEBUG_ASSERT(_checkDimensions(k, Mu, Sigma));
		const double nom   = std::exp(-0.5 * _calcMahalanobis(x, Mu, _calcInvSigma(Sigma)));
		const double denom = _calcDenomDensity(k, Sigma);

		return nom / denom;
	}

	static double logDensity(const TypeX &x, TConstView<double> Mu, const arma::mat &Sigma) noexcept(noDebug) {
		// calculates log density of a multivariate normal distribution
		const auto k = x.size();
		DEBUG_ASSERT(_checkDimensions(k, Mu, Sigma));
		const double nom   = -0.5 * _calcMahalanobis(x, Mu, _calcInvSigma(Sigma));
		const double denom = _calcDenomLogDensity(k, Sigma);

		return nom - denom;
	}

	static TypeX sample(TConstView<double> Mu, const arma::mat &Sigma) {
		// according to Numerical Recipes 3rd edition, pp.378
		const auto K = Mu.size();
		DEBUG_ASSERT(_checkDimensions(K, Mu, Sigma));

		// Do Cholesky decomposition to factor Sigma into a left triangular matrix L times its transpose Sigma = LL^T
		const arma::mat L = arma::chol(Sigma, "lower");
		return _sample(K, Mu, L);
	}

	// member functions
	[[nodiscard]] Positive density(const TypeX &x) const noexcept(noDebug) {
		DEBUG_ASSERT(x.size() == _mu.size());
		const double nom = std::exp(-0.5 * _calcMahalanobis(x, _mu, _invSigma));

		return nom / _denom;
	};
	[[nodiscard]] double logDensity(const TypeX &x) const noexcept(noDebug) {
		DEBUG_ASSERT(x.size() == _mu.size());
		const double nom = -0.5 * _calcMahalanobis(x, _mu, _invSigma);
		return nom - _denomLog;
	};

	[[nodiscard]] TypeX sample() const { return _sample(_mu.size(), _mu, _L); };
};
} // namespace coretools::probdist
#endif
