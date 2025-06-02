//
// Created by caduffm on 3/15/22.
//

#ifndef TPRIORBINOMIAL_H
#define TPRIORBINOMIAL_H

#include "coretools/Distributions/TBinomialDistr.h"
#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Math/mathFunctions.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TPriorBase.h"

namespace stattools::prior {

//-------------------------------------------
// TBinomialFixed
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim> class TBinomialFixed : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(TypesAreUnsignedInteger<Type>() && (NumDim == 1 || NumDim == 2));

protected:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	coretools::ZeroOneOpen _p = coretools::ZeroOneOpen::min();
	double _log_p             = log(_p);
	double _log_q             = log(1. - _p);

	void _simulateUnderPrior(Storage *Data) override {
		using coretools::instances::randomGenerator;
		using coretools::P;
		for (auto i = Data->beginSlice(1, 0); !i.end(); ++i) {
			(*Data)[i.cur()]     = static_cast<Type>(randomGenerator().template getRand<size_t>(
				0, Type::max() + 1)); // draw random n: uniform between 0 and max
			(*Data)[i.cur() + 1] = randomGenerator().getBinomialRand(P(_p), (Type)(*Data)[i]);
		}
	};

	void _checkSizeStorageBelow() const {
		// check if size of parameter matches
		// either 1-dimensional with 2 elements, n and k
		// or 2-dimensional, where the number of columns is 2 (n and k)
		for (const auto &storage : this->_storageBelow) {
			if (storage->numDim() == 1 && storage->size() != 2) {
				DEVERROR("Parameter with 1 dimension must have a total size of 2 (not", storage->size(),
						 "), where the first element is n and the second element is k!");
			} else if (storage->numDim() == 2 && storage->dimensions()[1] != 2) {
				DEVERROR("Parameter with 2 dimensions must have exactly 2 columns (not", storage->dimensions()[1],
						 "), where the first element is n and the second element is k!");
			}
		}
	}

public:
	TBinomialFixed()           = default;
	~TBinomialFixed() override = default;

	[[nodiscard]] std::string name() const override { return "binomial"; }

	void setFixedPriorParameters(std::string_view Params) override {
		coretools::str::convertString(Params, "Parameter of " + name() + " distribution is p. ", _p);
		_log_p = log(_p);
		_log_q = log(1. - _p);

		_checkSizeStorageBelow();
	};

	auto p() const { return _p; }

	double getDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TBinomialDistr::density((Type)Data[Index], (Type)Data[Index + 1], coretools::P(_p));
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TBinomialDistr::logDensity((Type)Data[Index], (Type)Data[Index + 1], coretools::P(_p));
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		using coretools::chooseLog;
		uint64_t n    = (Type)Data[Index];
		uint64_t newK = (Type)Data[Index + 1];
		uint64_t oldK = Data[Index + 1].oldValue();
		return chooseLog(n, newK) - chooseLog(n, oldK) +
			   (static_cast<int>(newK) - static_cast<int>(oldK)) * _log_p +
			   (static_cast<int>(oldK) - static_cast<int>(newK)) * _log_q;
	};

	double getSumLogPriorDensity(const Storage &Data) const override {
		double sum = 0.;
		for (auto i = Data.beginSlice(1, 0); !i.end(); ++i) { sum += getLogDensity(Data, i.cur()); }
		return sum;
	};
};

//-------------------------------------------
// TBinomialInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecP>
class TBinomialInferred : public TBinomialFixed<Derived, Type, NumDim> {
	static_assert(TypesAreUnsignedInteger<Type>() && (NumDim == 1 || NumDim == 2) &&
				  TypesAreZeroOneOpenFloatingPoints<typename SpecP::value_type>());

private:
	using typename TBinomialFixed<Derived, Type, NumDim>::Storage;
	using typename TBinomialFixed<Derived, Type, NumDim>::UpdatedStorage;
	using BoxType       = TBinomialInferred<Derived, Type, NumDim, SpecP>;
	using TypeParamP    = TParameter<SpecP, BoxType>;
	TypeParamP *_paramP = nullptr;

protected:
	void _setPToMLE() {
		double sumK = 0.;
		double sumN = 0.;
		for (const auto &storage : this->_storageBelow) {
			for (auto i = storage->beginSlice(1, 0); !i.end(); ++i) {
				sumN += (Type)(*storage)[i.cur()];
				sumK += (Type)(*storage)[i.cur() + 1];
			}
		}

		// avoid p=0 and p=1 (to respect boundaries of p)
		if (sumK == 0.) {
			sumK++;
		} else if (sumK == sumN) {
			sumK--;
		}
		_paramP->set(sumK / sumN);
	}

	void _simulateUnderPrior(Storage *Data) override {
		updateTempVals(_paramP, 0, true);
		TBinomialFixed<Derived, Type, NumDim>::_simulateUnderPrior(Data);
	};

public:
	explicit TBinomialInferred(TypeParamP *P) : TBinomialFixed<Derived, Type, NumDim>(), _paramP(P) {
		this->addPriorParameter(_paramP);
	};
	~TBinomialInferred() override = default;

	void setFixedPriorParameters(std::string_view Params) override {
		if (!Params.empty()) {
			UERROR("Can not set prior parameter values for ", this->name(), " distribution: they are inferred.");
		}
	}

	void initialize() override {
		_paramP->initStorage(this);
		this->_checkSizeStorageBelow();
	};

	auto calculateLLRatio(TypeParamP *, size_t) const {
		double v1 = log((double)_paramP->value() / (double)_paramP->oldValue());
		double v2 = log((1. - _paramP->value()) / (1. - _paramP->oldValue()));
		auto f    = [v1, v2](Storage *Data) {
			double sumK       = 0.;
			double sumNMinusK = 0.;
			for (auto i = Data->beginSlice(1, 0); !i.end(); ++i) {
				const auto n = value((*Data)[i.cur()]);
				const auto k = value((*Data)[i.cur() + 1]);
				sumK += k;
				sumNMinusK += n - k;
			}
			return v1 * sumK + v2 * sumNMinusK;
		};
		return f;
	}

	void updateTempVals(TypeParamP *, size_t, bool Accepted) {
		if (Accepted) {
			this->_p     = _paramP->value();
			this->_log_p = log(_paramP->value());
			this->_log_q = log(1. - _paramP->value());
		}
	}

	void guessInitialValues() override { _setPToMLE(); }
};

} // end namespace stattools::prior

#endif // TPRIORBINOMIAL_H
