//
// Created by madleina on 16.10.23.
//

#ifndef STATTOOLS_TPRIORTEMPLATE_H
#define STATTOOLS_TPRIORTEMPLATE_H

#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TPriorBase.h"

//-------------------------------------------
// TPRIORTEMPLATE
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecMyPriorParameter>
class TPriorTemplate : public stattools::prior::TStochasticBase<Derived, Type, NumDim> {
private:
	using typename stattools::prior::TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename stattools::prior::TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;
	using BoxType   = TPriorTemplate<Derived, Type, NumDim, SpecMyPriorParameter>;
	using TypeParam = stattools::TParameter<SpecMyPriorParameter, BoxType>;

	TypeParam *_myPriorParameter = nullptr;

protected:
	void _simulateUnderPrior(Storage *Data) override {
		// Fill Data with values randomly generated from this distribution
		// Example: for a Bernoulli distribution, this is
		for (size_t i = 0; i < Data->size(); ++i) {
			(*Data)[i] = coretools::instances::randomGenerator().getBernoulliRand(_myPriorParameter->value());
		}
	}

public:
	TPriorTemplate(TypeParam *MyPriorParameter) : _myPriorParameter(MyPriorParameter) {
		this->addPriorParameter({_myPriorParameter});
	}

	~TPriorTemplate() override = default;

	[[nodiscard]] std::string name() const override { return "template"; }

	void initialize() override { _myPriorParameter->initStorage(this); }

	double getDensity(const Storage &Data, size_t Index) const override {
		// Calculate P(Data[Index] | theta)
		// where theta are all parameters of this prior
		// Example: for a Bernoulli distribution, this is
		if ((Type)Data[Index]) { return _myPriorParameter->value(); }
		return 1.0 - _myPriorParameter->value();
	}

	double getLogDensity(const Storage &Data, size_t Index) const override {
		// Calculate log(P(Data[Index] | theta))
		// where theta are all parameters of this prior
		// Example: for a Bernoulli distribution, this is
		if ((Type)Data[Index]) { return log(_myPriorParameter->value()); }
		return log(1.0 - _myPriorParameter->value());
	}

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		// Calculate log(P(Data[Index]' | theta)) - log(P(Data[Index] | theta))
		// where theta are all parameters of this prior
		// and Data[Index]' is the new, proposed value of the parameter Data
		// This corresponds to the log prior ratio of the Hastings ratio and is used when updating Data[Index]
		// Example: for a Bernoulli distribution, this is
		const double x     = (double)(Type)Data[Index];
		const double old_x = (double)Data[Index].oldValue();
		return (x - old_x) * log(_myPriorParameter->value()) + (old_x - x) * log(1.0 - _myPriorParameter->value());
	}

	[[nodiscard]] auto calculateLLRatio(TypeParam *, size_t) const {
		// Function that calculates the log likelihood ratio (part of the Hastings ratio)
		// Example: for a Bernoulli distribution, this is
		double v1 = log((double)_myPriorParameter->value() / (double)_myPriorParameter->oldValue());
		double v2 = log((1. - _myPriorParameter->value()) / (1. - _myPriorParameter->oldValue()));
		auto f    = [v1, v2](Storage *Data) {
			size_t counter1 = Data->numNonZero();
			return v1 * (double)counter1 + v2 * (double)(Data->size() - counter1);
		};
		return f;
	}

	void updateTempVals(TypeParam *, size_t, bool /*Accepted*/) { /* empty - there are no tmp values */ }

	void guessInitialValues() override {
		// Set all prior parameters to some guess of the initial values (before MCMC starts)
		// can be obtained by MLE, method of moments or some other initial estimation
		_myPriorParameter->set(0.5);
	}
};

#endif // STATTOOLS_TPRIORTEMPLATE_H
