//
// Created by madleina on 29.04.22.
//

#ifndef TPRIORGAMMA_H
#define TPRIORGAMMA_H

#include "coretools/Distributions/TGammaDistr.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TPriorBase.h"

//-------------------------------------------
// TGammaFixed
//-------------------------------------------

namespace stattools::prior {

template<typename Derived, typename Type, size_t NumDim> class TGammaFixed : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(TypesAreStrictlyPositiveFloatingPoints<Type>());

private:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	coretools::StrictlyPositive _alpha = coretools::StrictlyPositive::min();
	coretools::StrictlyPositive _beta  = coretools::StrictlyPositive::min();

	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) {
			(*Data)[i] = coretools::instances::randomGenerator().getGammaRand(_alpha, _beta);
		}
	};

public:
	~TGammaFixed() override = default;

	std::string name() const override { return "gamma"; }

	void setFixedPriorParameters(std::string_view Params) override {
		// gamma(alpha, beta)
		coretools::str::convertString(Params, "Parameters of " + name() + " distribution are alpha, beta. ", _alpha,
									  _beta);
	};

	auto alpha() const { return _alpha; }
	auto beta() const { return _beta; }

	double getDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TGammaDistr::density((double)(Type)Data[Index], _alpha, _beta);
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TGammaDistr::logDensity((double)(Type)Data[Index], _alpha, _beta);
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		const auto newX = (double)(Type)Data[Index];
		const auto oldX = (double)Data[Index].oldValue();
		return (_alpha - 1.) * log(newX / oldX) + _beta * (oldX - newX);
	};
};

//-------------------------------------------
// TGammaInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecAlpha, typename SpecBeta>
class TGammaInferred : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(
		TypesAreStrictlyPositiveFloatingPoints<Type, typename SpecAlpha::value_type, typename SpecBeta::value_type>());

private:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;
	using BoxType        = TGammaInferred<Derived, Type, NumDim, SpecAlpha, SpecBeta>;
	using TypeParamAlpha = TParameter<SpecAlpha, BoxType>;
	using TypeParamBeta  = TParameter<SpecBeta, BoxType>;

	TypeParamAlpha *_alpha = nullptr;
	TypeParamBeta *_beta   = nullptr;

protected:
	auto _setAlphaBetaToMixedTypeLogMomentEstimates() {
		// MLE does not have closed form
		// Consistent closed-form estimators of alpha and beta are derived from the likelihood
		// of the generalized gamma distribution
		// These estimators are not strictly maximum likelihood estimators, but are instead referred to as mixed type
		// log-moment estimators. They have however similar efficiency as the maximum likelihood estimators.
		// equations from Wikipedia
		double sum        = 0.;
		double sumOfLogs  = 0.;
		double sumOfXLogs = 0.;
		double N          = 0;
		for (const auto &storage : this->_storageBelow) {
			sum += storage->sum();
			sumOfLogs += storage->sumOfLogs();
			sumOfXLogs += storage->customSum([](double x) { return x * log(x); });
			N += storage->size();
		}

		const double denom = N * sumOfXLogs - sumOfLogs * sum;
		return std::make_pair((N * sum) / denom, (N * N) / denom);
	}

	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) {
			double val = coretools::instances::randomGenerator().getGammaRand(_alpha->value(), _beta->value());
			while (val <= (double)Type::min()) { // prevent errors due to interval violation
				val = coretools::instances::randomGenerator().getGammaRand(_alpha->value(), _beta->value());
			}
			(*Data)[i] = val;
		}
	};

public:
	TGammaInferred(TypeParamAlpha *Alpha, TypeParamBeta *Beta) : _alpha(Alpha), _beta(Beta) {
		this->addPriorParameter({_alpha, _beta});
	};
	~TGammaInferred() override = default;

	std::string name() const override { return "gamma"; }

	void initialize() override {
		_alpha->initStorage(this);
		_beta->initStorage(this);
	};

	auto alpha() const { return _alpha->value(); }
	auto beta() const { return _beta->value(); }

	double getDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TGammaDistr::density((Type)Data[Index], _alpha->value(), _beta->value());
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TGammaDistr::logDensity((Type)Data[Index], _alpha->value(), _beta->value());
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		const auto newX = (double)(Type)Data[Index];
		const auto oldX = (double)Data[Index].oldValue();
		return (_alpha->value() - 1.) * log(newX / oldX) + _beta->value() * (oldX - newX);
	};

	[[nodiscard]] auto calculateLLRatio(TypeParamAlpha *, size_t) const {
		const double v1 = ((double)_alpha->value() - (double)_alpha->oldValue()) * log(_beta->value());
		const double v2 = coretools::gammaLog(_alpha->oldValue()) - coretools::gammaLog(_alpha->value());
		const double v3 = (double)_alpha->value() - (double)_alpha->oldValue();
		auto f          = [v1, v2, v3](Storage *Data) { return Data->size() * (v1 + v2) + v3 * Data->sumOfLogs(); };
		return f;
	}

	[[nodiscard]] auto calculateLLRatio(TypeParamBeta *, size_t) const {
		const double v1 = _alpha->value() * log((double)_beta->value() / (double)_beta->oldValue());
		const double v2 = (double)_beta->oldValue() - (double)_beta->value();
		auto f          = [v1, v2](Storage *Data) { return v1 * Data->size() + v2 * Data->sum(); };
		return f;
	}

	void updateTempVals(TypeParamAlpha *, size_t, bool) { /* empty - there are no tmp values */
	}
	void updateTempVals(TypeParamBeta *, size_t, bool) { /* empty - there are no tmp values */
	}

	void guessInitialValues() override {
		auto [alpha, beta] = _setAlphaBetaToMixedTypeLogMomentEstimates();
		_alpha->set(alpha);
		_beta->set(beta);
	}
};

} // end namespace stattools::prior

#endif // TPRIORGAMMA_H
