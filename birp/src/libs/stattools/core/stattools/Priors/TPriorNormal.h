//
// Created by caduffm on 3/15/22.
//

#ifndef TPRIORNORMAL_H
#define TPRIORNORMAL_H

#include "coretools/Distributions/TNormalDistr.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TPriorBase.h"

//-------------------------------------------
// TNormalFixed
//-------------------------------------------

namespace stattools::prior {

template<typename Derived, typename Type, size_t NumDim> class TNormalFixed : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(TypesAreUnboundedFloatingPoints<Type>());

private:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	double _mean                        = 0.0;
	coretools::StrictlyPositive _var    = coretools::StrictlyPositive::min();
	coretools::StrictlyPositive _sd     = coretools::StrictlyPositive::min();
	coretools::StrictlyPositive _twoVar = 2. * _var;

	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) {
			(*Data)[i] = coretools::instances::randomGenerator().getNormalRandom(_mean, sqrt(_var));
		}
	};

public:
	~TNormalFixed() override = default;

	[[nodiscard]] std::string name() const override { return "normal"; }

	void setFixedPriorParameters(std::string_view Params) override {
		// normal(mean,var)
		coretools::str::convertString(Params, "Parameters of " + name() + " distribution are mean, var. ", _mean, _var);
		_twoVar = 2.0 * _var;
		_sd     = sqrt(_var);
	};

	auto mean() const { return _mean; }

	auto var() const { return _var; }

	double getDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TNormalDistr::density((Type)Data[Index], _mean, _sd);
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TNormalDistr::logDensity((Type)Data[Index], _mean, _sd);
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		double tmp     = ((Type)Data[Index] - _mean);
		double tmp_old = (Data[Index].oldValue() - _mean);
		return (tmp_old * tmp_old - tmp * tmp) / _twoVar;
	};
};

//-------------------------------------------
// TNormalInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecMean, typename SpecVar>
class TNormalInferred : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(TypesAreUnboundedFloatingPoints<Type, typename SpecMean::value_type>() &&
				  TypesAreStrictlyPositiveFloatingPoints<typename SpecVar::value_type>());

private:
	using BoxType = TNormalInferred<Derived, Type, NumDim, SpecMean, SpecVar>;
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;
	using TypeParamMean = TParameter<SpecMean, BoxType>;
	using TypeParamVar  = TParameter<SpecVar, BoxType>;

	TypeParamMean *_mean = nullptr;
	TypeParamVar *_var   = nullptr;
	double _twoVar       = 0.0;
	double _sd           = 0.0;

protected:
	void _updateTempVals() {
		// need to be updated whenever var has changed
		_twoVar = 2. * _var->value();
		_sd     = sqrt(_var->value());
	};

	void _setMeanToMLE() {
		// calculate MLE of mean
		double innerSum         = 0.;
		double totalNumElements = 0;
		for (const auto &storage : this->_storageBelow) {
			innerSum += storage->sum();
			totalNumElements += storage->size();
		}
		double mean = innerSum / totalNumElements;

		// now set mean to MLE
		_mean->set(mean);
	}

	void _setVarToMLE() {
		// calculate MLE of var
		double innerSum         = 0.;
		double totalNumElements = 0;
		for (const auto &storage : this->_storageBelow) {
			innerSum += storage->sumOfNormalizedSquares(_mean->value());
			totalNumElements += storage->size();
		}

		// now set var to MLE
		_var->set(innerSum / totalNumElements);
	}

	void _simulateUnderPrior(Storage *Data) override {
		_updateTempVals();
		for (size_t i = 0; i < Data->size(); ++i) {
			(*Data)[i] = coretools::instances::randomGenerator().getNormalRandom(_mean->value(), _sd);
		}
	};

public:
	TNormalInferred(TypeParamMean *Mean, TypeParamVar *Var) : _mean(Mean), _var(Var) {
		this->addPriorParameter({_mean, _var});
	};
	~TNormalInferred() override = default;

	[[nodiscard]] std::string name() const override { return "normal"; }

	void initialize() override {
		_mean->initStorage(this);
		_var->initStorage(this);
	}

	auto mean() const { return _mean->value(); }

	auto var() const { return _var->value(); }

	double getDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TNormalDistr::density((Type)Data[Index], _mean->value(), _sd);
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TNormalDistr::logDensity((Type)Data[Index], _mean->value(), _sd);
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		double tmp     = ((Type)Data[Index] - _mean->value());
		double tmp_old = (Data[Index].oldValue() - _mean->value());
		return (tmp_old * tmp_old - tmp * tmp) / _twoVar;
	};

	[[nodiscard]] auto calculateLLRatio(TypeParamMean *, size_t) const {
		auto f = [mu = _mean->value(), oldMu = _mean->oldValue(), twoVar = _twoVar](Storage *Data) {
			double sum =
				Data->customSum([mu, oldMu](auto v) { return (v - oldMu) * (v - oldMu) - (v - mu) * (v - mu); });
			return sum / twoVar;
		};
		return f;
	}

	[[nodiscard]] auto calculateLLRatio(TypeParamVar *, size_t) const {
		double v1 = log(_var->oldValue() / _var->value());
		double v2 = 0.5 * (1. / _var->oldValue() - 1. / _var->value());

		auto f = [v1, v2, mu = _mean->value()](Storage *Data) {
			double sum = Data->sumOfNormalizedSquares(mu);
			return 0.5 * Data->size() * v1 + v2 * sum;
		};
		return f;
	}

	void updateTempVals(TypeParamMean *, size_t, bool) { /* empty - no temporary values are changed */
	}
	void updateTempVals(TypeParamVar *, size_t, bool Accepted) {
		if (Accepted) { _updateTempVals(); }
	}

	void guessInitialValues() override {
		_setMeanToMLE();
		_setVarToMLE();
	}
};

} // end namespace stattools::prior

#endif // TPRIORNORMAL_H
