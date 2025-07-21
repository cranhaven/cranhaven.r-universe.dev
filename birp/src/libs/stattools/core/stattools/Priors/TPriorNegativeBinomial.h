//
// Created by madleina on 02.05.22.
//

#ifndef TPRIORNEGATIVEBINOMIAL_H
#define TPRIORNEGATIVEBINOMIAL_H

#include "coretools/Main/TParameters.h"
#include "coretools/Distributions/TNegativeBinomialDistr.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TPriorBase.h"
#include "stattools/Priors/TPriorBinomial.h"

namespace stattools::prior {

//-------------------------------------------
// TNegativeBinomialFixed
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim>
class TNegativeBinomialFixed : public TBinomialFixed<Derived, Type, NumDim> {
	// models the number of failures which occur in a sequence of iid Bernoulli trials
	// before a specified (non-random) target number of successes is reached.
	// Data below is 2-dimensional:
	// -> the first column corresponds to n (the number of successes until experiment is stopped)
	// -> the second column corresponds to x (the number of failures)
	// n is fixed, but x is a parameter that can change (just like the k from the binomial distribution)
	// Note: same implementation as in R, but differs from description of Wikipedia (switched success & failure)
protected:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	// for simulation: simulate random n. Do not want to take numeric limits of Type as maximum,
	// since x can be larger than n -> will be cut off / result in different distribution
	size_t _max_n_simulation = 0;

	void _simulateUnderPrior(Storage *Data) override {
		using namespace coretools::instances;
		coretools::TRange range = Data->get1DSlice(0, {0, 0}); // first column (= n)
		for (size_t i = range.begin; i < range.end; i += range.increment) {
			(*Data)[i]     = static_cast<Type>(randomGenerator().getRand<size_t>(
				0, _max_n_simulation + 1)); // draw random n: uniform between 0 and max
			(*Data)[i + 1] = randomGenerator().getNegativeBinomialRand(coretools::P(this->_p.get()), (Type)(*Data)[i]);
		}
	};

public:
	TNegativeBinomialFixed() { _max_n_simulation = coretools::instances::parameters().get("max_n_simulation", 50); }

	~TNegativeBinomialFixed() override = default;

	[[nodiscard]] std::string name() const override { return "negativeBinomial"; }

	double getDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TNegativeBinomialDistr::density((double)(Type)Data[Index], (Type)Data[Index + 1],
																	coretools::P(this->_p));
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TNegativeBinomialDistr::logDensity((double)(Type)Data[Index], (Type)Data[Index + 1],
																	   coretools::P(this->p()));
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		const double n    = (Type)Data[Index];
		const size_t newX = (Type)Data[Index + 1];
		const size_t oldX = Data[Index + 1].oldValue();
		// gammaLog(0) yields Inf -> prevent this
		if (n == 0.0 && newX == 0 && oldX > 0) { return std::numeric_limits<double>::max(); }
		if (n == 0.0 && newX > 0 && oldX == 0) { return std::numeric_limits<double>::lowest(); }
		if (n == 0.0) { return 0.0; }

		const size_t max  = std::max(newX, oldX);
		const double diff = (double)newX - (double)oldX;
		double fac        = coretools::TFallingFactorial::fallingFactorialLog((size_t)max, (size_t)std::fabs(diff));
		if (newX == max) { fac *= -1.0; }
		return coretools::gammaLog((double)newX + n) - coretools::gammaLog((double)oldX + n) + fac +
			   diff * this->_log_q;
	};
};

//-------------------------------------------
// TNegativeBinomialInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecP>
class TNegativeBinomialInferred : public TNegativeBinomialFixed<Derived, Type, NumDim> {
	static_assert(TypesAreZeroOneOpenFloatingPoints<typename SpecP::value_type>());

private:
	using typename TNegativeBinomialFixed<Derived, Type, NumDim>::Storage;
	using typename TNegativeBinomialFixed<Derived, Type, NumDim>::UpdatedStorage;
	using BoxType    = TNegativeBinomialInferred<Derived, Type, NumDim, SpecP>;
	using TypeParamP = TParameter<SpecP, BoxType>;

	TypeParamP *_paramP = nullptr;

protected:
	void _setPToMLE() {
		double sumN      = 0.;
		double sumNPlusX = 0.;
		for (const auto &storage : this->_storageBelow) {
			for (auto i = storage->beginSlice(1, 0); !i.end(); ++i) {
				sumN += (Type)(*storage)[i.cur()];
				sumNPlusX += (double)(Type)(*storage)[i.cur()] + (double)(Type)(*storage)[i.cur() + 1];
			}
		}

		// avoid p=0 and p=1 (to respect boundaries of p)
		if (sumN == 0.) {
			sumN++;
		} else if (sumN == sumNPlusX) {
			sumN--;
		}
		_paramP->set(sumN / sumNPlusX);
	}

	void _simulateUnderPrior(Storage *Data) override {
		updateTempVals(_paramP, 0, true);
		TNegativeBinomialFixed<Derived, Type, NumDim>::_simulateUnderPrior(Data);
	};

public:
	TNegativeBinomialInferred(TypeParamP *P) : _paramP(P) { this->addPriorParameter(_paramP); };
	~TNegativeBinomialInferred() override = default;

	void setFixedPriorParameters(std::string_view Params) override {
		if (!Params.empty()) {
			throw coretools::TUserError("Can not set prior parameter values for ", this->name(), " distribution: they are inferred.");
		}
	}

	void initialize() override {
		_paramP->initStorage(this);
		// check if dimensions of parameter below are as expected
		this->_checkSizeStorageBelow();
	};

	auto calculateLLRatio(TypeParamP *, size_t) const {
		double v1 = log((double)_paramP->value() / (double)_paramP->oldValue());
		double v2 = log((1. - _paramP->value()) / (1. - _paramP->oldValue()));
		auto f    = [v1, v2](Storage *Data) {
			double sumN = 0.;
			double sumX = 0.;
			for (auto i = Data->beginSlice(1, 0); !i.end(); ++i) {
				sumN += value((*Data)[i.cur()]);
				sumX += value((*Data)[i.cur() + 1]);
			}
			return v1 * sumN + v2 * sumX;
		};
		return f;
	}

	void updateTempVals(TypeParamP *, size_t, bool Accepted) {
		// this function is called if the update of p is accepted
		if (Accepted) {
			// need to be updated whenever p has changed
			this->_p     = _paramP->value();
			this->_log_p = log(_paramP->value());
			this->_log_q = log(1. - _paramP->value());
		}
	}

	void guessInitialValues() override { _setPToMLE(); }
};

} // end namespace stattools::prior

#endif // TPRIORNEGATIVEBINOMIAL_H
