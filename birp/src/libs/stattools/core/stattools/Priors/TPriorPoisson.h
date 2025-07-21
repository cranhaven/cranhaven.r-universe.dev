//
// Created by madleina on 29.04.22.
//

#ifndef TPRIORPOISSON_H
#define TPRIORPOISSON_H

#include "coretools/Distributions/TPoissonDistr.h"
#include "coretools/Math/mathFunctions.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TPriorBase.h"

//-------------------------------------------
// TPoissonFixed
//-------------------------------------------

namespace stattools::prior {

template<typename Derived, typename Type, size_t NumDim> class TPoissonFixed : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(TypesAreUnsignedInteger<Type>());

private:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	coretools::StrictlyPositive _lambda = coretools::StrictlyPositive::min();
	double _log_lambda                  = 0.0;

	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) {
			(*Data)[i] = coretools::instances::randomGenerator().getPoissonRandom(_lambda);
		}
	};

public:
	~TPoissonFixed() override = default;

	std::string name() const override { return "poisson"; }

	void setFixedPriorParameters(std::string_view Params) override {
		// poisson(lambda)
		coretools::str::convertString(Params, "Parameter of " + name() + " distribution is lambda. ", _lambda);
		_log_lambda = log(_lambda);
	};

	auto lambda() const { return _lambda; }

	double getDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TPoissonDistr::density((double)(Type)Data[Index], _lambda);
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TPoissonDistr::logDensity((double)(Type)Data[Index], _lambda);
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		const size_t newK = (Type)Data[Index];
		const size_t oldK = Data[Index].oldValue();
		const size_t max  = std::max(newK, oldK);
		const double diff = (double)newK - (double)oldK;
		double fac        = coretools::TFallingFactorial::fallingFactorialLog(max, (size_t)std::fabs(diff));
		if (newK == max) { fac *= -1.0; }
		return diff * _log_lambda + fac;
	};
};

//-------------------------------------------
// TPoissonInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecLambda>
class TPoissonInferred : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(TypesAreStrictlyPositiveFloatingPoints<typename SpecLambda::value_type>());

private:
	using BoxType = TPoissonInferred<Derived, Type, NumDim, SpecLambda>;
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;
	using TypeParamLambda = TParameter<SpecLambda, BoxType>;

	TypeParamLambda *_lambda = nullptr;
	double _log_lambda       = 0.0;

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
		_lambda->set(sum / totalNumElements);
	}

	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) {
			(*Data)[i] = coretools::instances::randomGenerator().getPoissonRandom(_lambda->value());
		}
	};

public:
	TPoissonInferred(TypeParamLambda *Lambda) : _lambda(Lambda) { this->addPriorParameter(_lambda); };
	~TPoissonInferred() override = default;

	std::string name() const override { return "poisson"; }

	void initialize() override { _lambda->initStorage(this); };

	auto lambda() const { return _lambda->value(); }

	double getDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TPoissonDistr::density((Type)Data[Index], _lambda->value());
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TPoissonDistr::logDensity((Type)Data[Index], _lambda->value());
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		const size_t newK = (Type)Data[Index];
		const size_t oldK = Data[Index].oldValue();
		const size_t max  = std::max(newK, oldK);
		const double diff = (double)newK - (double)oldK;
		double fac        = coretools::TFallingFactorial::fallingFactorialLog(max, (size_t)std::fabs(diff));
		if (newK == max) { fac *= -1.0; }
		return diff * _log_lambda + fac;
	};

	[[nodiscard]] auto calculateLLRatio(TypeParamLambda *, size_t) const {
		double v1 = log(_lambda->value() / _lambda->oldValue());
		double v2 = (double)_lambda->oldValue() - (double)_lambda->value();
		auto f    = [v1, v2](Storage *Data) { return v1 * Data->sum() + v2 * Data->size(); };
		return f;
	}

	void updateTempVals(TypeParamLambda *, size_t, bool Accepted) {
		if (Accepted) { _log_lambda = log(_lambda->value()); }
	}

	void guessInitialValues() override { _setLambdaToMLE(); }
};

} // end namespace stattools::prior

#endif // TPRIORPOISSON_H
