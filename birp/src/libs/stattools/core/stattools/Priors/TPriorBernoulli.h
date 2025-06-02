//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORBERNOUILLI_H
#define TPRIORBERNOUILLI_H

#include "coretools/Types/commonWeakTypes.h"
#include "stattools/EM/TEM.h"
#include "stattools/EM/TEMPriorIndependent.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TPriorBase.h"

namespace stattools::prior {

//-------------------------------------------
// TBernoulliFixed
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim>
class TBernoulliFixed : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(TypesAreBool<Type>());

private:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	coretools::ZeroOneOpen _pi             = coretools::ZeroOneOpen::min();
	coretools::ZeroOneOpen _1MinPi         = coretools::ZeroOneOpen::max();
	coretools::StrictlyNegative _logPi     = std::log(_pi);
	coretools::StrictlyNegative _log1MinPi = std::log(_1MinPi);

	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) {
			(*Data)[i] = coretools::instances::randomGenerator().getBinomialRand(coretools::P(_pi), 1);
		}
	};

public:
	TBernoulliFixed()           = default;
	~TBernoulliFixed() override = default;

	[[nodiscard]] std::string name() const override { return "bernoulli"; }

	void setFixedPriorParameters(std::string_view Params) override {
		coretools::str::convertString(Params, "Parameter of " + name() + " distribution is pi. ", _pi);
		_1MinPi    = 1. - _pi;
		_logPi     = std::log(_pi);
		_log1MinPi = std::log(1. - _pi);
	};

	auto pi() const { return _pi; }

	double getDensity(const Storage &Data, size_t Index) const override { return (Type)Data[Index] ? _pi : _1MinPi; };

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return (Type)Data[Index] ? _logPi : _log1MinPi;
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		return ((double)(Type)Data[Index] - (double)Data[Index].oldValue()) * _logPi +
			   ((double)Data[Index].oldValue() - (double)(Type)Data[Index]) * _log1MinPi;
	};
};

//-------------------------------------------
// TBernoulliInferredBase
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecPi, typename BoxType>
class TBernoulliInferredBase : public TStochasticBase<Derived, Type, NumDim>,
							   public TEMPriorIndependent_base<double, size_t, size_t> {
	static_assert(TypesAreBool<Type>() && TypesAreZeroOneOpenFloatingPoints<typename SpecPi::value_type>());

protected:
	using TypeParamPi = TParameter<SpecPi, BoxType>;
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	TypeParamPi *_pi = nullptr;

	double _1MinPi    = 0.0;
	double _logPi     = 0.0;
	double _log1MinPi = 0.0;
	double _EMSum     = 0.0;
	size_t _EMTotal   = 0;
	bool _ranEM       = false;

	void _updateTempVals() {
		// need to be updated whenever pi has changed
		_1MinPi    = 1. - _pi->value();
		_logPi     = log(_pi->value());
		_log1MinPi = log(1. - _pi->value());
	};

	void _setPiToMLE() {
		double counter1         = 0;
		double numElementsTotal = 0;
		for (const auto &storage : this->_storageBelow) {
			counter1 += storage->numNonZero();
			numElementsTotal += storage->size();
		}

		// set pi (but prevent pi == 0 and pi == 1)
		if (counter1 == 0) {
			_pi->set(1. / (numElementsTotal + 1));
		} else if (counter1 == numElementsTotal) {
			_pi->set((numElementsTotal) / (numElementsTotal + 1));
		} else {
			_pi->set(counter1 / numElementsTotal);
		}
	}

	void _runEMEstimationOrStatePosteriors(TLatentVariable<double, size_t, size_t> &latentVariable, bool RunEM) {
		// only allow for 1 parameter (no shared priors)
		// reason: EM. z below passes pointer to itself to this prior, which then runs EM. If there are multiple z,
		// we would need to find a way to have pointers to both and run the EM on them simultaneously -> not so easy
		if (this->_storageBelow.size() != 1) {
			DEVERROR("Can not run EM estimation of prior Bernoulli prior for ", this->_storageBelow.size(),
					 " parameters. Currently only implemented for 1 parameter.");
		}

		TEM<double, size_t, size_t> em(*this, latentVariable, 500, 0.0001, true);
		em.report();

		// get total length
		size_t length                 = this->_storageBelow[0]->size();
		std::vector<size_t> vecLength = {length};

		if (RunEM) { em.runEM(vecLength); } // else only estimate state posteriors!

		// to initialize z
		em.estimateStatePosteriors(vecLength);

		_ranEM = true;
	}

	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) {
			(*Data)[i] = coretools::instances::randomGenerator().getBinomialRand(coretools::P(_pi->value()), 1);
		}
	};

	// protected constructor: only accessible by derived classes
	explicit TBernoulliInferredBase(TypeParamPi *Pi) : TEMPriorIndependent_base<double, size_t, size_t>(2), _pi(Pi) {
		this->addPriorParameter(_pi);
	};

public:
	~TBernoulliInferredBase() override = default;

	auto pi() const { return _pi->value(); }

	double getDensity(const Storage &Data, size_t Index) const override {
		return (Type)Data[Index] ? (double)_pi->value() : _1MinPi;
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return (Type)Data[Index] ? _logPi : _log1MinPi;
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		return ((double)(Type)Data[Index] - (double)Data[Index].oldValue()) * _logPi +
			   ((double)Data[Index].oldValue() - (double)(Type)Data[Index]) * _log1MinPi;
	};

	void guessInitialValues() override {
		if (!_ranEM) { // only set pi to MLE if it was not set with EM previously
			_setPiToMLE();
		}
	}

	[[nodiscard]] auto calculateLLRatio(TypeParamPi *, size_t) const {
		double v1 = log((double)_pi->value() / (double)_pi->oldValue());
		double v2 = log((1. - _pi->value()) / (1. - _pi->oldValue()));
		auto f    = [v1, v2](Storage *Data) {
			size_t counter1 = Data->numNonZero();
			return v1 * (double)counter1 + v2 * (double)(Data->size() - counter1);
		};
		return f;
	}

	void updateTempVals(TypeParamPi *, size_t, bool Accepted) {
		if (Accepted) { this->_updateTempVals(); }
	}

	std::vector<double> getParameters() const override { return {_pi->value()}; }
	bool setParameters(coretools::TConstView<double> Params) override {
		if (Params[0] <= 0.0 || Params[1] >= 1.0) { return false; }
		_pi->set(Params[0]);
		return true;
	};

	void runEMEstimation(TLatentVariable<double, size_t, size_t> &latentVariable) override {
		_runEMEstimationOrStatePosteriors(latentVariable, true);
	}

	void estimateStatePosteriors(TLatentVariable<double, size_t, size_t> &latentVariable) override {
		_runEMEstimationOrStatePosteriors(latentVariable, false);
	}

	double operator()(size_t, size_t State) const override { return State ? (double)_pi->value() : 1. - _pi->value(); }

	void handleEMParameterInitialization(size_t, size_t State) override {
		if (State == 1) { _EMSum++; }
		_EMTotal++;
	}

	void finalizeEMParameterInitialization() override {
		// set pi (but prevent pi == 0 and pi == 1)
		double pi;
		if (_EMSum == 0.) {
			pi = 1. / _EMTotal;
		} else if (_EMSum == _EMTotal) {
			pi = (_EMTotal - 1.) / _EMTotal;
		} else {
			pi = _EMSum / _EMTotal;
		}
		pi = std::min(0.999, pi);
		pi = std::max(0.001, pi);
		_pi->set(pi);
	}

	void prepareEMParameterEstimationInitial() override { prepareEMParameterEstimationOneIteration(); }

	void prepareEMParameterEstimationOneIteration() override {
		_EMSum   = 0.;
		_EMTotal = 0;
	}

	void handleEMParameterEstimationOneIteration(size_t, const TDataVector<double, size_t> &weights) override {
		_EMSum += weights[1];
		_EMTotal++;
	}

	void finalizeEMParameterEstimationOneIteration() override { finalizeEMParameterInitialization(); }

	void switchPriorClassificationAfterEM() override {
		// classification of components by EM is random, but sometimes we have certain restrictions (e.g. normal mixed
		// model with 2 components, shared mean but different variances) we then might have to switch the EM labels and
		// all associated prior parameters

		// set to opposite
		_pi->set(1. - _pi->value());
		_updateTempVals();
	}
};

//-------------------------------------------
// TBernoulliInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecPi>
class TBernoulliInferred
	: public TBernoulliInferredBase<Derived, Type, NumDim, SpecPi, TBernoulliInferred<Derived, Type, NumDim, SpecPi>> {

private:
	using BoxType = TBernoulliInferred<Derived, Type, NumDim, SpecPi>;
	using Base    = TBernoulliInferredBase<Derived, Type, NumDim, SpecPi, BoxType>;
	using typename Base::Storage;
	using typename Base::TypeParamPi;
	using typename Base::UpdatedStorage;

protected:
	using TBernoulliInferredBase<Derived, Type, NumDim, SpecPi, BoxType>::_pi;

public:
	explicit TBernoulliInferred(TypeParamPi *Pi) : TBernoulliInferredBase<Derived, Type, NumDim, SpecPi, BoxType>(Pi){};
	~TBernoulliInferred() override = default;

	void initialize() override {
		_pi->initStorage(this);
	};

	[[nodiscard]] std::string name() const override { return "bernoulli"; }
};

//-------------------------------------------
// TBernoulliPerElementInferred
// Note: Remove this class after implementing
// per-dimension prior parameters!
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecPi>
class TBernoulliPerElementInferred : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(TypesAreBool<Type>() && TypesAreZeroOneOpenFloatingPoints<typename SpecPi::value_type>());

private:
	using BoxType = TBernoulliPerElementInferred<Derived, Type, NumDim, SpecPi>;
	using Base    = TStochasticBase<Derived, Type, NumDim>;
	using typename Base::Storage;
	using typename Base::UpdatedStorage;
	using TypeParamPi = TParameter<SpecPi, BoxType>;

	TypeParamPi *_pi = nullptr;

	std::vector<double> _1MinPi;
	std::vector<double> _logPi;
	std::vector<double> _log1MinPi;

	void _updateTempVals(size_t i) {
		// need to be updated whenever pi has changed
		_1MinPi[i]    = 1. - _pi->value(i);
		_logPi[i]     = log(_pi->value(i));
		_log1MinPi[i] = log(1. - _pi->value(i));
	};

	void _setPiToMLE() {
		for (size_t i = 0; i < _pi->size(); ++i) {
			double counter1 = 0;
			for (const auto &storage : this->_storageBelow) { counter1 += (Type)(*storage)[i]; }
			double N = (double)this->_storageBelow.size();
			// set pi (but prevent pi == 0 and pi == 1)
			if (counter1 == 0) {
				_pi->set(i, 0.001);
			} else if (counter1 == N) {
				_pi->set(i, 0.999);
			} else {
				_pi->set(i, counter1 / N);
			}
		}
	}

	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) {
			(*Data)[i] = coretools::instances::randomGenerator().getBinomialRand(coretools::P(_pi->value(i)), 1);
		}
	};

public:
	explicit TBernoulliPerElementInferred(TypeParamPi *Pi) : _pi(Pi) { this->addPriorParameter(_pi); };
	~TBernoulliPerElementInferred() override = default;

	void initialize() override {
		impl::checkSameDimStorageBelow(this->_storageBelow, name());

		const auto &storage = this->_storageBelow.front();
		_pi->initStorage(this, storage->dimensions(), storage->getDimensionNames());

		_1MinPi.resize(_pi->size());
		_logPi.resize(_pi->size());
		_log1MinPi.resize(_pi->size());
	};

	[[nodiscard]] std::string name() const override { return "bernoulliPerElement"; }

	double getDensity(const Storage &Data, size_t Index) const override {
		return (Type)Data[Index] ? (double)_pi->value(Index) : _1MinPi[Index];
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return (Type)Data[Index] ? _logPi[Index] : _log1MinPi[Index];
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		return ((double)(Type)Data[Index] - (double)Data[Index].oldValue()) * _logPi[Index] +
			   ((double)Data[Index].oldValue() - (double)(Type)Data[Index]) * _log1MinPi[Index];
	};

	void guessInitialValues() override { _setPiToMLE(); }

	[[nodiscard]] auto calculateLLRatio(TypeParamPi *, size_t Index) const {
		double v1 = log((double)_pi->value(Index) / (double)_pi->oldValue(Index));
		double v2 = log((1. - _pi->value(Index)) / (1. - _pi->oldValue(Index)));
		auto f    = [v1, v2, Index](Storage *Data) {
			if ((Type)(*Data)[Index]) { return v1; } // 1
			return v2;                               // 0
		};
		return f;
	}

	void updateTempVals(TypeParamPi *, size_t Index, bool Accepted) {
		if (Accepted) { _updateTempVals(Index); }
	}
};

} // end namespace stattools::prior

#endif // TPRIORBERNOUILLI_H
