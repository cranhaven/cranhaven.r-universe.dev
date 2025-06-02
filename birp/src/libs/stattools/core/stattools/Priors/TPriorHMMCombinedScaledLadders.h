//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORHMMCOMBINEDSCALEDLADDERS_H
#define TPRIORHMMCOMBINEDSCALEDLADDERS_H

#include "stattools/HMMDistances/TCombinedStatesTransitionMatrices.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/HMMDistances/TOptimizeTransMatNelderMead.h"
#include "stattools/Priors/TPriorHMMBase.h"
#include "stattools/HMMDistances/TTransitionMatrixDist.h"
#include "stattools/HMMDistances/TTransitionMatrixHMMDistances.h"
#include "stattools/HMMDistances/TOptimizeTransMatNelderMead.h"

namespace stattools::prior {

//-------------------------------------------
// THMMCombinedScaledLadderBase
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecKappas, typename SpecNus, typename SpecMus,
		 template<typename, typename> typename TransitionMatrixType>
class THMMCombinedScaledLadderInferred
	: public THMMPrior<Derived, Type, NumDim, TCombinedStatesTransitionMatrices<double, size_t, size_t>> {
	static_assert(TypesAreUnsignedIntWithVariableMax<Type>() &&
				  TypesAreStrictlyPositiveFloatingPoints<typename SpecKappas::value_type, typename SpecNus::value_type,
														 typename SpecMus::value_type>());
	static_assert(!SpecKappas::parallelize); // not thread safe!
	static_assert(!SpecNus::parallelize);    // not thread safe!
	static_assert(!SpecMus::parallelize);    // not thread safe!

private:
	using BoxType =
		THMMCombinedScaledLadderInferred<Derived, Type, NumDim, SpecKappas, SpecNus, SpecMus, TransitionMatrixType>;
	using TypeParamKappas          = TParameter<SpecKappas, BoxType>;
	using TypeParamNus             = TParameter<SpecNus, BoxType>;
	using TypeParamMus             = TParameter<SpecMus, BoxType>;
	using TransitionMatrixTypeFull = TransitionMatrixType<double, size_t>;

	TypeParamKappas *_kappa = nullptr;
	TypeParamNus *_nu       = nullptr;
	TypeParamMus *_mu       = nullptr;

	std::vector<size_t> _numStates;
	size_t _numChains = 0;

	std::vector<bool> _withMu;
	std::vector<size_t> _chainToMuMap;
	size_t _numParametersTotal = 0;

	using Base = THMMPrior<Derived, Type, NumDim, TCombinedStatesTransitionMatrices<double, size_t, size_t>>;
	using Base::_transitionMatrix;

protected:
	using typename Base::Storage;
	using typename Base::UpdatedStorage;

	void _storeEMEstimatesInMCMCParameters(const std::vector<double> &Values) override {
		// fill MCMC parameters based on values from Baum-Welch
		assert(Values.size() == _numParametersTotal);
		size_t i = 0;
		for (size_t c = 0; c < _numChains; c++, i += 3) {
			_kappa->set(c, Values[i]);
			_nu->set(c, Values[i + 1]);
			if (_withMu[c]) { _mu->set(_chainToMuMap[c], Values[i + 2]); }
		}
	}

	auto _fixTransitionMatricesDuringEM() const {
		size_t sum = _kappa->hasFixedInitialValue() + _nu->hasFixedInitialValue() + _mu->hasFixedInitialValue();
		if (sum == 0) {
			return std::vector<bool>(_numChains, false);
		} else if (sum == 3) {
			return std::vector<bool>(_numChains, true);
		} else {
			UERROR("Error in initialization of ", name(),
				   " prior: either specify initial values for kappa, nu and mu, or for none of them.");
		}
	}

	void _setDefaultValues() {
		std::vector<double> defaultVals = {0.0001, 0.99, 0.99};
		for (size_t c = 0; c < _numChains; c++) {
			if (!_kappa->hasFixedInitialValue()) { _kappa->set(c, defaultVals[0]); }
			if (!_nu->hasFixedInitialValue()) { _nu->set(c, defaultVals[1]); }
			if (_withMu[c] && !_mu->hasFixedInitialValue()) { _mu->set(_chainToMuMap[c], defaultVals[2]); }
		}
	}

	void _setValuesBeforeEM() {
		_setDefaultValues();

		// assemble initial values for parameters in EM
		std::vector<double> initialValues(_numParametersTotal, 1.0);
		size_t i = 0;
		for (size_t c = 0; c < _numChains; c++, i += 3) {
			initialValues[i]     = _kappa->value(c);
			initialValues[i + 1] = _nu->value(c);
			if (_withMu[c]) { initialValues[i + 2] = _mu->value(_chainToMuMap[c]); }
		}

		_transitionMatrix.setValuesEM(initialValues);
	}

	void _runEMEstimationOrStatePosteriors(TLatentVariable<double, size_t, size_t> &latentVariable, bool RunEM) {
		// assemble initial values for parameters in EM
		_setValuesBeforeEM();

		// only allow for 1 parameter (no shared priors)
		// reason: EM. z below passes pointer to itself to this prior, which then runs EM. If there are multiple z,
		// we would need to find a way to have pointers to both and run the EM on them simultaneously -> not so easy
		if (this->_storageBelow.size() > 1) {
			DEVERROR("Can not run EM estimation of prior ", name(), " for ", this->_storageBelow.size(),
					 " parameters. Currently only implemented for 1 parameter.");
		}

		// fix transition matrix with EM only if transition matrix parameters shouldn't be initialized
		auto fixTransitionMatricesDuringEM = _fixTransitionMatricesDuringEM();
		_transitionMatrix.fixTransitionMatricesDuringEM(fixTransitionMatricesDuringEM, getParams());
		if (RunEM) {
			_transitionMatrix.runEMEstimation(latentVariable);
			// write last values of EM into HMM parameters
			auto values = _transitionMatrix.getFinalParameterEstimatesEM();
			_storeEMEstimatesInMCMCParameters(values);
		} else {
			_transitionMatrix.estimateStatePosteriors(latentVariable);
		}
		this->_ranEM = true;
	}

	auto _constructTransitionMatrices(const std::vector<size_t> &NumStates,
									  const std::vector<size_t> &AttractorIndex = {}) const {
		std::vector<std::shared_ptr<TTransitionMatrixDistancesOptimizerBase<double, size_t, size_t>>> optimizers(
			NumStates.size());
		for (size_t c = 0; c < NumStates.size(); c++) {
			auto o = std::make_shared<TOptimizeTransMatNelderMead<double, size_t, size_t, TransitionMatrixTypeFull>>(
				NumStates[c]);
			if constexpr (std::is_same_v<TransitionMatrixTypeFull,
										 stattools::TTransitionMatrixScaledLadderAttractorShift<double, size_t>> ||
						  std::is_same_v<TransitionMatrixTypeFull,
										 stattools::TTransitionMatrixScaledLadderAttractorShift2<double, size_t>>) {
				assert(NumStates.size() == AttractorIndex.size());
				o->setAttractorIx(AttractorIndex[c]);
			}
			optimizers[c] = o;
		}
		return optimizers;
	}

public:
	THMMCombinedScaledLadderInferred(TypeParamKappas *Kappas, TypeParamNus *Nus, TypeParamMus *Mus,
									 const std::vector<size_t> &NumStates, coretools::TDistancesBinnedBase *Distances,
									 const std::vector<size_t> &AttractorIndex = {})
		: Base(Distances, _constructTransitionMatrices(NumStates, AttractorIndex)), _kappa(Kappas), _nu(Nus), _mu(Mus),
		  _numStates(NumStates), _numChains(NumStates.size()) {

		// per chain, we need 1) the number of states and 2) a kappa, a nu and, if numStates > 3, we also need a mu
		_withMu.resize(_numChains, false);
		_chainToMuMap.resize(_numChains, 0);
		_numParametersTotal = 3 * _numChains;

		size_t i = 0;
		for (size_t c = 0; c < _numChains; ++c) {
			if (_numStates[c] > 3) {
				_withMu[c]       = true;
				_chainToMuMap[c] = i;
				++i;
			}
		}

		this->addPriorParameter({_kappa, _nu, _mu});
	};
	~THMMCombinedScaledLadderInferred() override = default;

	std::string name() const override { return "hmm_combinedScaledLadder"; }

	void initialize() override {
		// initialize storage for all kappas, mus and nus
		_kappa->initStorage(this, {_numChains}, {std::make_shared<coretools::TNamesIndices>(_numChains)});
		_nu->initStorage(this, {_numChains}, {std::make_shared<coretools::TNamesIndices>(_numChains)});
		auto names_mu = std::make_shared<coretools::TNamesStrings>();
		size_t numMu  = 0;
		for (size_t i = 0; i < _numChains; ++i) {
			if (_withMu[i]) {
				names_mu->addName({coretools::str::toString(i)});
				++numMu;
			}
		}
		_mu->initStorage(this, {numMu}, {names_mu});

		_setDefaultValues();
		this->updateTau(getParams());

		// make sure that max of parameters below is numStates-1
		if (Type::max() != _transitionMatrix.numStates() - 1) {
			DEVERROR("numStates-1 (", _transitionMatrix.numStates() - 1, ") differs from maximum value of Type (",
					 Type::max(), ").");
		}
	}

	std::vector<double> getParams() const override {
		std::vector<double> values(_numParametersTotal);
		size_t i = 0;
		for (size_t c = 0; c < _numChains; c++, i += 3) {
			values[i]     = _kappa->value(c);
			values[i + 1] = _nu->value(c);
			if (_withMu[c]) {
				values[i + 2] = _mu->value(_chainToMuMap[c]);
			} else {
				values[i + 2] = 1.0;
			}
		}
		return values;
	}

	auto kappasMusNus() const { return getParams(); }

	double dist1_transitionProbability(size_t From, size_t To) const {
		return _transitionMatrix.dist1_transitionProbability(From, To);
	}

	double stationary(size_t State) const { return _transitionMatrix.stationary(State); }

	void runEMEstimation(TLatentVariable<double, size_t, size_t> &latentVariable) override {
		_runEMEstimationOrStatePosteriors(latentVariable, true);
	}

	void estimateStatePosteriors(TLatentVariable<double, size_t, size_t> &latentVariable) override {
		_runEMEstimationOrStatePosteriors(latentVariable, false);
	}

	void guessInitialValues() override {
		// if z are known: no need to do EM
		// assemble initial values for parameters in EM
		_setValuesBeforeEM();

		if (!this->_ranEM) { // only initialize prior parameters if we didn't run EM before
			auto fixTransitionMatricesDuringEM = _fixTransitionMatricesDuringEM();
			_transitionMatrix.fixTransitionMatricesDuringEM(fixTransitionMatricesDuringEM, getParams());

			// if at least one is estimated -> estimate initial prior parameters
			bool estimate = false;
			for (const auto &chainIsFix : fixTransitionMatricesDuringEM) {
				if (!chainIsFix) {
					estimate = true;
					break;
				}
			}
			if (estimate) {
				this->guessInitialValues_HiddenStateKnown();
			} else {
				// don't initialize prior parameters, just leave it as it is
			}
		}
	}

	void switchPriorClassificationAfterEM() override {
		// classification of components by EM is random, but sometimes we have certain restrictions (e.g. normal
		// mixed model with 2 components, shared mean but different variances) we then might have to switch the EM
		// labels and all associated prior parameters

		// stays empty -> transition matrices are symmetric
	}
};
} // end namespace stattools::prior

#endif // TPRIORHMMCOMBINEDSCALEDLADDERS_H
