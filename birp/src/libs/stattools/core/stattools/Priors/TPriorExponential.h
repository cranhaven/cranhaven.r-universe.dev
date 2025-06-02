//
// Created by caduffm on 3/15/22.
//

#ifndef TPRIOREXPONENTIAL_H
#define TPRIOREXPONENTIAL_H

#include "coretools/Distributions/TExponentialDistr.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TPriorBase.h"

//-------------------------------------------
// TExponentialFixed
//-------------------------------------------

namespace stattools::prior {

template<typename Derived, typename Type, size_t NumDim, bool TruncatedTop = false, bool TruncatedBottom = false>
class TExponentialFixed : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(TypesArePositiveFloatingPoints<Type>() || TypesAreStrictlyPositiveFloatingPoints<Type>() ||
				  (TruncatedTop && !TruncatedBottom &&
				   (TypesAreStrictlyPositiveWithVariableMax<Type>() || TypesAreZeroOpenOneClosed<Type>())) ||
				  (TruncatedTop && TruncatedBottom && TypesAreMinMaxVariable<Type>()));

private:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	coretools::StrictlyPositive _lambda = coretools::StrictlyPositive::min();

	mutable double _max         = 0.0;
	mutable double _CDF_max     = coretools::probdist::TExponentialDistr::cumulativeDensity(_max, _lambda);
	mutable double _log_CDF_max = log(_CDF_max);

	double _getRandomValue() const {
		if constexpr (TruncatedTop) {
			return coretools::instances::randomGenerator().getExponentialRandomTruncated(_lambda, 0.0, Type::max());
		}
		return coretools::instances::randomGenerator().getExponentialRandom(_lambda);
	}

	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) {
			if constexpr (TypesAreStrictlyPositiveFloatingPoints<Type>() ||
						  TypesAreStrictlyPositiveWithVariableMax<Type>()) {
				// do not simulate 0.0
				double val = _getRandomValue();
				while (val == 0.0) { val = _getRandomValue(); }
				(*Data)[i] = val;
			} else {
				(*Data)[i] = _getRandomValue();
			}
		}
	};

	void _setCDFMax() const {
		if (Type::max() != _max) { // max has changed
			_max         = Type::max();
			_CDF_max     = coretools::probdist::TExponentialDistr::cumulativeDensity(_max, _lambda);
			_log_CDF_max = log(_CDF_max);
		}
	}

public:
	~TExponentialFixed() override = default;

	std::string name() const override { return "exponential"; }

	void setFixedPriorParameters(std::string_view Params) override {
		// exponential(lambda)
		coretools::str::convertString(Params, "Parameter of " + name() + " distribution is lambda. ", _lambda);
		_setCDFMax();
	};

	auto lambda() const { return _lambda; }

	double getDensity(const Storage &Data, size_t Index) const override {
		const auto dens = coretools::probdist::TExponentialDistr::density((double)(Type)Data[Index], _lambda);
		if constexpr (TruncatedTop) {
			_setCDFMax();
			return dens / _CDF_max;
		}
		return dens;
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		const auto dens = coretools::probdist::TExponentialDistr::logDensity((double)(Type)Data[Index], _lambda);
		if constexpr (TruncatedTop) {
			_setCDFMax();
			return dens - _log_CDF_max;
		}
		return dens;
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		// same for truncated and non-truncated
		return _lambda * ((double)Data[Index].oldValue() - (double)(Type)Data[Index]);
	};
};

//-------------------------------------------
// TExponentialInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecLambda>
class TExponentialInferred : public TStochasticBase<Derived, Type, NumDim> {
	static_assert((TypesArePositiveFloatingPoints<Type>() || TypesAreStrictlyPositiveFloatingPoints<Type>()) &&
				  TypesAreStrictlyPositiveFloatingPoints<typename SpecLambda::value_type>());

private:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;
	using BoxType         = TExponentialInferred<Derived, Type, NumDim, SpecLambda>;
	using TypeParamLambda = TParameter<SpecLambda, BoxType>;

	TypeParamLambda *_lambda = nullptr;

protected:
	void _setLambdaToMLE() {
		double sum              = 0.;
		double totalNumElements = 0.;
		for (const auto &storage : this->_storageBelow) {
			sum += storage->sum();
			totalNumElements += storage->size();
		}
		if (sum == 0.) { // avoid division by zero
			sum = 0.01;
		}
		_lambda->set(totalNumElements / sum);
	}

	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) {
			(*Data)[i] = coretools::instances::randomGenerator().getExponentialRandom(_lambda->value());
		}
	};

public:
	TExponentialInferred(TypeParamLambda *Lambda) : _lambda(Lambda) { this->addPriorParameter(_lambda); };
	~TExponentialInferred() override = default;

	std::string name() const override { return "exponential"; }

	void initialize() override { _lambda->initStorage(this); };

	auto lambda() const { return _lambda->value(); }

	double getDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TExponentialDistr::density((double)(Type)Data[Index], _lambda->value());
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TExponentialDistr::logDensity((double)(Type)Data[Index], _lambda->value());
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		return _lambda->value() * ((double)Data[Index].oldValue() - (double)(Type)Data[Index]);
	};

	[[nodiscard]] auto calculateLLRatio(TypeParamLambda *, size_t) const {
		double v1 = log(_lambda->value() / _lambda->oldValue());
		double v2 = (double)_lambda->oldValue() - (double)_lambda->value();
		auto f    = [v1, v2](Storage *Data) { return v1 * Data->size() + v2 * Data->sum(); };
		return f;
	}

	void updateTempVals(TypeParamLambda *, size_t, bool) { /* empty - there are no tmp values */
	}

	void guessInitialValues() override { _setLambdaToMLE(); }
};

} // end namespace stattools::prior

#endif // TPRIOREXPONENTIAL_H
