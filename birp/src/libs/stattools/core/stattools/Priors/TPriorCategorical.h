//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORCATEGORICAL_H
#define TPRIORCATEGORICAL_H

#include "stattools/EM/TEM.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TPriorBase.h"

namespace stattools::prior {

//-------------------------------------------
// TCategoricalInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecPis>
class TCategoricalInferred : public TStochasticBase<Derived, Type, NumDim>,
							 public TEMPriorIndependent_base<double, size_t, size_t> {
	static_assert((TypesAreUnsignedIntWithVariableMax<Type>() || TypesAreUnsignedInteger<Type>()) &&
				  TypesAreZeroOpenOneClosedFloatingPoints<typename SpecPis::value_type>());
	static_assert(SpecPis::constraint::constraint == Constraints::sumOne);

	// Note: K corresponds to Max+1 of Type
	// Therefore, it can not be Type, since it violates its interval. It therefore has a separate type TypeK.
private:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;
	using BoxType      = TCategoricalInferred<Derived, Type, NumDim, SpecPis>;
	using TypeParamPis = TParameter<SpecPis, BoxType>;

	TypeParamPis *_pis = nullptr;
	size_t _K          = 0;

	std::vector<double> _EMSums;
	size_t _EMTotal = 0;
	bool _ranEM     = false;

protected:
	void _setPisToMLE() {
		std::vector<double> counter(_K, 0);
		double numElementsTotal = 0;
		for (const auto &storage : this->_storageBelow) {
			auto tmp = storage->countLevels(_K);
			coretools::pairwiseSum(counter, tmp);
			numElementsTotal += storage->size();
		}

		for (size_t k = 0; k < _K; k++) {
			double val = counter[k] / numElementsTotal;
			if (val == 0.) { // not allowed because of log
				val = 0.00001;
			}
			_pis->set(k, val);
		}
		_pis->normalize(_pis->getFull());
	}

	void _runEMEstimationOrStatePosteriors(TLatentVariable<double, size_t, size_t> &latentVariable, bool RunEM) {
		// only allow for 1 parameter (no shared priors)
		// reason: EM. z below passes pointer to itself to this prior, which then runs EM. If there are multiple z,
		// we would need to find a way to have pointers to both and run the EM on them simultaneously -> not so easy
		if (this->_storageBelow.size() > 1) {
			DEVERROR("Can not run EM estimation of prior ", name(), " for ", this->_storageBelow.size(),
					 " parameters. Currently only implemented for 1 parameter.");
		}

		TEM<double, size_t, size_t> em(*this, latentVariable, 500, 0.0001, true);
		em.report();

		// get total length
		size_t length                 = this->_storageBelow[0]->size();
		std::vector<size_t> vecLength = {length};

		if (RunEM) { em.runEM(vecLength); }

		// to initialize z
		em.estimateStatePosteriors(vecLength);

		_ranEM = true;
	}

	void _simulateUnderPrior(Storage *Data) override {
		_pis->normalize(_pis->getFull());

		// calculate cumulative pi's
		std::vector<double> cumPis(_K);
		cumPis[0] = _pis->value(0);       // first
		for (size_t k = 1; k < _K; k++) { // all others
			cumPis[k] = cumPis[k - 1] + _pis->value(k);
		}

		for (size_t i = 0; i < Data->size(); ++i) {
			(*Data)[i] = coretools::instances::randomGenerator().pickOne(cumPis);
		}
	};

public:
	TCategoricalInferred(TypeParamPis *Pis, size_t K)
		: TEMPriorIndependent_base<double, size_t, size_t>(), _pis(Pis), _K(K) {
		this->_numStates = _K;

		this->addPriorParameter(_pis);
	};
	~TCategoricalInferred() override = default;

	[[nodiscard]] std::string name() const override { return "categorical"; }

	void initialize() override {
		// pi will have indices as dimension names (results in pi_1, pi_2, pi_3, ...)
		auto dimNames = std::make_shared<coretools::TNamesIndices>();
		dimNames->setOffset(_pis->getDefinition().offSetDimensionNames());
		_pis->initStorage(this, {_K}, {dimNames});

		if (_pis->hasFixedInitialValue()) { _pis->normalize(_pis->getFull()); }

		// check if max of Type below = K-1
		if (Type::max() != _K - 1) {
			DEVERROR("K-1 (", _K - 1, ") differs from maximum value of Type (", Type::max(), ").");
		}
	};

	auto pi(size_t k) const { return _pis->value(k); }

	size_t ixMaxPi() const { return _pis->ix_max_element(); }

	double getDensity(const Storage &Data, size_t Index) const override { return _pis->value((Type)Data[Index]); };

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return log(_pis->value((Type)Data[Index]));
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		return log((double)_pis->value((Type)Data[Index]) / (double)_pis->value(Data[Index].oldValue()));
	};

	[[nodiscard]] auto calculateLLRatio(TypeParamPis *, size_t k) const {
		assert(std::fabs(_pis->sum() - 1.0) < 1e-05);

		double r = log((double)_pis->value(k) / (double)_pis->oldValue(k));
		auto f   = [r, k](Storage *Data) {
			auto c = Data->countLevel(k);
			return r * c;
		};
		return f;
	}

	void updateTempVals(TypeParamPis *, size_t, bool){/* empty: no tmp variables */};

	void guessInitialValues() override {
		if (!_ranEM) { _setPisToMLE(); }
	}

	std::vector<double> getParameters() const override {
		std::vector<double> pis(_pis->size());
		for (size_t i = 0; i < _pis->size(); ++i) { pis[i] = _pis->value(i); }
		return pis;
	};

	bool setParameters(coretools::TConstView<double> Params) override {
		for (size_t i = 0; i < _pis->size(); ++i) {
			if (Params[i] <= 0.0 || Params[i] >= 1.0) { return false; }
		}
		for (size_t i = 0; i < _pis->size(); ++i) { _pis->set(i, Params[i]); }
		_pis->normalize(_pis->getFull());
		return true;
	};

	void runEMEstimation(TLatentVariable<double, size_t, size_t> &latentVariable) override {
		_runEMEstimationOrStatePosteriors(latentVariable, true);
	}

	void estimateStatePosteriors(TLatentVariable<double, size_t, size_t> &latentVariable) override {
		_runEMEstimationOrStatePosteriors(latentVariable, false);
	}

	double operator()(size_t, size_t State) const override { return _pis->value(State); };

	void handleEMParameterInitialization(size_t, size_t State) override {
		_EMSums[State]++;
		_EMTotal++;
	}

	void finalizeEMParameterInitialization() override { finalizeEMParameterEstimationOneIteration(); }

	void prepareEMParameterEstimationInitial() override {
		_EMSums.resize(this->_numStates, 0.);
		_EMTotal = 0;
	}

	void prepareEMParameterEstimationOneIteration() override {
		std::fill(_EMSums.begin(), _EMSums.end(), 0.);
		_EMTotal = 0;
	}

	void handleEMParameterEstimationOneIteration(size_t, const TDataVector<double, size_t> &weights) override {
		coretools::pairwiseSum(_EMSums, weights);
		_EMTotal++;
	}

	void finalizeEMParameterEstimationOneIteration() override {
		for (size_t k = 0; k < _K; k++) {
			if (_EMSums[k] == 0) { _EMSums[k] = 1; }
			_pis->set(k, _EMSums[k] / _EMTotal);
		}
		_pis->normalize(_pis->getFull());
	}

	void finalizeEMParameterEstimationFinal() override { _EMSums.clear(); }
};

} // end namespace stattools::prior

#endif // TPRIORCATEGORICAL_H
