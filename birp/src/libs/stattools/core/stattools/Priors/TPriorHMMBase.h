//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORHMM_H
#define TPRIORHMM_H

#include "coretools/Main/TLog.h"
#include "coretools/Main/TParameters.h"
#include "coretools/Distances/TDistances.h"
#include "stattools/Priors/TPriorBase.h"

namespace stattools::prior {

//-------------------------------------------
// THMMPrior
//-------------------------------------------
template<typename Derived, typename Type, size_t NumDim, typename TypeTransitionMatrix>
class THMMPrior : public TStochasticBase<Derived, Type, NumDim> {
	// base class for HMM priors
protected:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	TypeTransitionMatrix _transitionMatrix;
	bool _ranEM = false;

	template<typename T> auto _getDensityForSpecificZ(const T &Data, size_t i, const Type &Value_i) const {
		return calcPOfValueGivenPreviousForSpecificZ(Data, i, Value_i) *
			   calcPOfNextGivenValueForSpecificZ(Data, i, Value_i);
	};

	void _guessInitialValues(bool FixTransitionMatricesDuringEM) {
		// if z are known: no need to do EM
		if (!_ranEM && !FixTransitionMatricesDuringEM) { // only initialize prior parameters if we didn't run EM before
			guessInitialValues_HiddenStateKnown();
		} // else don't initialize prior parameters, just leave it as it is
	}

	virtual void _storeEMEstimatesInMCMCParameters(const std::vector<double> &Values) = 0;

	void _simulateUnderPrior(Storage *Data) override {
		simulateDistances();
		simulateStates(Data);
	}

public:
	template<typename T>
	THMMPrior(coretools::TDistancesBinnedBase *Distances, const T &TransitionMatrix)
		: _transitionMatrix(Distances, TransitionMatrix){};

	~THMMPrior() override = default;

	virtual std::vector<double> getParams() const = 0;

	template<typename T> auto calcPOfValueGivenPrevious(const T &Data, size_t i) const {
		if (i == 0) { return _transitionMatrix(i, 0, (Type)Data[i]); }
		return _transitionMatrix(i, (Type)Data[i - 1], (Type)Data[i]);
	};

	template<typename T> auto calcPOfNextGivenValue(const T &Data, size_t i) const {
		if (i == Data.size() - 1) { return 1.; }
		return _transitionMatrix(i + 1, (Type)Data[i], (Type)Data[i + 1]);
	};

	auto calcPOfOldValueGivenPrevious(const UpdatedStorage &Data, size_t i) const {
		if (i == 0) { return _transitionMatrix(i, 0, Data[i].oldValue()); }
		return _transitionMatrix(i, (Type)Data[i - 1], Data[i].oldValue());
	};

	auto calcPOfNextGivenOldValue(const UpdatedStorage &Data, size_t i) const {
		if (i == (Data.size() - 1)) { return 1.; }
		return _transitionMatrix(i + 1, Data[i].oldValue(), (Type)Data[i + 1]);
	};

	template<typename T> auto calcLogPOfValueGivenPrevious(const T &Data, size_t i) const {
		const auto v = calcPOfValueGivenPrevious(Data, i);
		return (v == 0.0) ? std::numeric_limits<double>::lowest() : log(v);
	};

	template<typename T> auto calcLogPOfNextGivenValue(const T &Data, size_t i) const {
		const auto v = calcPOfNextGivenValue(Data, i);
		return (v == 0.0) ? std::numeric_limits<double>::lowest() : log(v);
	};

	auto calcLogPOfOldValueGivenPrevious(const UpdatedStorage &Data, size_t i) const {
		const auto v = calcPOfOldValueGivenPrevious(Data, i);
		return (v == 0.0) ? std::numeric_limits<double>::lowest() : log(v);
	};

	auto calcLogPOfNextGivenOldValue(const UpdatedStorage &Data, size_t i) const {
		const auto v = calcPOfNextGivenOldValue(Data, i);
		return (v == 0.0) ? std::numeric_limits<double>::lowest() : log(v);
	};

	template<typename T>
	auto calcPOfValueGivenPreviousForSpecificZ(const T &Data, size_t i, const Type &Value_i) const {
		if (i == 0) { return _transitionMatrix(i, 0, Value_i); }
		return _transitionMatrix(i, (Type)Data[i - 1], Value_i);
	};

	template<typename T> auto calcPOfNextGivenValueForSpecificZ(const T &Data, size_t i, const Type &Value_i) const {
		if (i == (Data.size() - 1)) { return 1.; }
		return _transitionMatrix(i + 1, Value_i, (Type)Data[i + 1]);
	};

	double getDensity(const Storage &Data, size_t Index) const override {
		return calcPOfValueGivenPrevious(Data, Index) * calcPOfNextGivenValue(Data, Index);
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return calcLogPOfValueGivenPrevious(Data, Index) + calcLogPOfNextGivenValue(Data, Index);
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		return calcLogPOfValueGivenPrevious(Data, Index) + calcLogPOfNextGivenValue(Data, Index) -
			   calcLogPOfOldValueGivenPrevious(Data, Index) - calcLogPOfNextGivenOldValue(Data, Index);
	};

	double getSumLogPriorDensity(const Storage &Data) const override {
		double sum = 0.;
		for (size_t i = 0; i < Data.size(); i++) { sum += calcLogPOfValueGivenPrevious(Data, i); }
		return sum;
	}

	bool updateTau(const std::vector<double> &Values) {
		resetTau(); // old = current

		// generate new transition matrices from updated parameters
		return _transitionMatrix.fillProbabilities(Values);
	};

	template<typename T> [[nodiscard]] auto calculateLLRatio(T *, const coretools::TRange&) {
		// Note: if one HMM parameter is updated (single or in pairs), all Tau's must be re-calculated
		// we want to do that only once per update, not for both indices of a pair
		// -> define calculateLLRatio using coretools::TRange -> only called once
		bool valid = this->updateTau(getParams());

		auto f = [&tau = _transitionMatrix, valid = valid](Storage *Data) {
			if (!valid) { return std::numeric_limits<double>::lowest(); }
			double LL = 0.0;
			LL += tau.logTransitionProbability(0, 0, value((*Data)[0])) -
				  tau.logOldTransitionProbability(0, 0, value((*Data)[0]));
			// all other
			for (size_t i = 1; i < Data->size(); ++i) {
				LL += tau.logTransitionProbability(i, value((*Data)[i - 1]), value((*Data)[i])) -
					  tau.logOldTransitionProbability(i, value((*Data)[i - 1]), value((*Data)[i]));
			}
			return LL;
		};
		return f;
	}

	template<class T> void updateTempVals(T *, const coretools::TRange&, bool Accepted) {
		// we want to reset only once per update, not for both indices of a pair
		// -> define updateTempVals using coretools::TRange -> only called once
		if (!Accepted) { this->resetTau(); }
	}

	void resetTau() { _transitionMatrix.swapNewOldTau(); };

	void guessInitialValues_HiddenStateKnown() {
		// estimate the transition matrix if hidden state was known: no EM required
		// get chunk ends from distances
		std::vector<size_t> chunkEnds = _transitionMatrix.distances()->template getChunkEnds<size_t>();

		// prepare storage
		_transitionMatrix.prepareEMParameterEstimationInitial();
		_transitionMatrix.prepareEMParameterEstimationOneIterationTransitionProbabilities();

		size_t first = 0;
		size_t previousState;

		// go over all parameters
		for (auto &storage : this->_storageBelow) {
			// go over all chunks
			for (size_t last : chunkEnds) {
				// go over all elements in one chunk
				previousState = (Type)(*storage)[0];
				for (size_t i = first + 1; i < last; ++i) {
					auto state = static_cast<size_t>((Type)(*storage)[i]);
					_transitionMatrix.handleEMParameterInitializationTransitionProbabilities(i, previousState, state);
					previousState = state;
				}
				// update first of next
				first = last;
			}
		}
		_transitionMatrix.finalizeEMParameterInitializationTransitionProbabilities();

		// store final estimate of HMM parameters
		auto values = _transitionMatrix.getFinalParameterEstimatesEM();
		_storeEMEstimatesInMCMCParameters(values);
	};

	void simulateDistances() {
		// simulate distances (same for all parameters)
		_transitionMatrix.simulateDistances(this->_storageBelow[0]->size());
		updateTau(getParams());
	}

	void simulateStates(Storage *Data) { _transitionMatrix.simulateStates(*Data); }
};

template<typename Derived, typename Type, size_t NumDim, typename TypeTransitionMatrix>
class THMMPriorStandard : public THMMPrior<Derived, Type, NumDim, TypeTransitionMatrix> {
	// base class for standard HMM priors: without combined states
protected:
	using typename THMMPrior<Derived, Type, NumDim, TypeTransitionMatrix>::Storage;
	using typename THMMPrior<Derived, Type, NumDim, TypeTransitionMatrix>::UpdatedStorage;
	using THMMPrior<Derived, Type, NumDim, TypeTransitionMatrix>::_transitionMatrix;
	using THMMPrior<Derived, Type, NumDim, TypeTransitionMatrix>::_ranEM;

	virtual void _setValuesBeforeEM(){};
	bool _fixTransitionMatricesDuringEM() const {
		// checks if all prior parameters are fix or all are estimated
		// throws if only part is fixed: difficult to handle for certain priors
		const auto params = this->getAllPriorParameters();
		bool allFix =
			std::all_of(params.begin(), params.end(), [](auto param) { return param->hasFixedInitialValue(); });
		bool anyFix =
			std::any_of(params.begin(), params.end(), [](auto param) { return param->hasFixedInitialValue(); });

		if (!allFix && anyFix) {
			throw coretools::TUserError("Error in initialization of ", this->name(),
				   " prior: either specify initial values for both all prior parameters, or for none of them. Don't "
				   "know how to handle only one!");
		}
		return allFix;
	}

	void _runEMEstimationOrStatePosteriors(TLatentVariable<double, size_t, size_t> &latentVariable,
										   bool FixTransitionMatricesDuringEM, bool RunEM, std::string_view Name) {
		// only allow for 1 parameter (no shared priors)
		// reason: EM. z below passes pointer to itself to this prior, which then runs EM. If there are multiple z,
		// we would need to find a way to have pointers to both and run the EM on them simultaneously -> not so easy
		if (this->_storageBelow.size() > 1) {
			throw coretools::TDevError("Can not run EM estimation of prior ", Name, " for ", this->_storageBelow.size(),
					 "parameters. Currently only implemented for 1 parameter.");
		}

		// fix transition matrix with EM only if transition matrix parameters shouldn't be initialized
		if (FixTransitionMatricesDuringEM) { _transitionMatrix.fixTransitionMatricesDuringEM(this->getParams()); }

		if (RunEM) {
			_transitionMatrix.runEMEstimation(latentVariable);
			// write last values of EM into HMM parameters
			auto values = _transitionMatrix.getFinalParameterEstimatesEM();
			this->_storeEMEstimatesInMCMCParameters(values);
		} else {
			_transitionMatrix.estimateStatePosteriors(latentVariable);
		}
		_ranEM = true;
	}

public:
	template<typename T>
	THMMPriorStandard(coretools::TDistancesBinnedBase *Distances, const T &TransitionMatrix)
		: THMMPrior<Derived, Type, NumDim, TypeTransitionMatrix>(Distances, TransitionMatrix){};
	~THMMPriorStandard() override = default;

	void runEMEstimation(TLatentVariable<double, size_t, size_t> &latentVariable) override {
		_setValuesBeforeEM();
		_runEMEstimationOrStatePosteriors(latentVariable, _fixTransitionMatricesDuringEM(), true, this->name());
	}

	void estimateStatePosteriors(TLatentVariable<double, size_t, size_t> &latentVariable) override {
		_runEMEstimationOrStatePosteriors(latentVariable, _fixTransitionMatricesDuringEM(), false, this->name());
	}

	void guessInitialValues() override {
		// if z are known: no need to do EM
		_setValuesBeforeEM();
		this->_guessInitialValues(_fixTransitionMatricesDuringEM());
	}
};

// extra standalone functions
namespace impl {

template<typename HMMPriorType, typename StorageType, typename Type>
void simulateFixedFractionZ1(HMMPriorType &HMMPrior, const StorageType &Storage) {
	// simulate distances and states
	HMMPrior.simulateDistances();
	HMMPrior.simulateStates(Storage);

	// should we simulate until a specific fraction of parameterBelow are 1?
	using namespace coretools::instances;
	if (parameters().exists("simulateFracZ1")) {
		auto goalStationary      = parameters().get<double>("simulateFracZ1");
		double absDiffStationary = parameters().get("absDiffSimulateFracZ1", goalStationary / 10.);
		size_t maxIterStationary = parameters().get("maxIterSimulateFracZ1", 1000);
		double fracZ1            = Storage->mean();
		size_t i                 = 1;
		while (std::fabs(fracZ1 - goalStationary) > absDiffStationary) {
			// simulate again
			HMMPrior.simulateStates(Storage);
			fracZ1 = Storage->mean();
			if (i > maxIterStationary) {
				logfile().list("Reached maximum number of iterations in simulating a fraction of ", goalStationary,
							   " z = 1");
				break;
			}
			i++;
		}
	}
}

} // end namespace impl

} // end namespace stattools::prior

#endif // TPRIORHMM_H
