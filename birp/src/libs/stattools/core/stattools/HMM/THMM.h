/*
 * THMM.h
 *
 *  Created on: Jan 19, 2021
 *      Author: phaentu
 */

#ifndef THMM_H_
#define THMM_H_

#include "stattools/EM/TEM.h"
#include "stattools/HMM/TLatentVariableBeta.h"

namespace stattools {

//-------------------------------------
// THMM
//-------------------------------------
template<typename PrecisionType, typename NumStatesType, typename LengthType>
class THMM : public TEM_base<PrecisionType, NumStatesType, LengthType> {
private:
	THMMForwardBackward<PrecisionType, NumStatesType, LengthType> _forwardBackward;
	TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> &_EMPrior;
	bool _curForwardIsAtBeginningOfAChunk; // variable used to handle very first of a chunk differently inside handles.

	TLatentVariableBeta<PrecisionType, NumStatesType, LengthType> _latVarBeta;

	using TEM_base<PrecisionType, NumStatesType, LengthType>::_latentVariable;
	using TEM_base<PrecisionType, NumStatesType, LengthType>::_writeLog;

protected:
	void _writeStatePosteriors(THMM<PrecisionType, NumStatesType, LengthType> &HMM,
							   const std::vector<LengthType> &ChunkEnds, size_t NumReplicates) {
		// for debugging: write state posteriors after initialization
		using namespace coretools::instances;
		if (parameters().exists("writeStatePosteriorsSmartInitialization")) {
			std::string filename = parameters().get("writeStatePosteriorsSmartInitialization");
			HMM.estimateStatePosteriors(ChunkEnds, NumReplicates, filename);
		}
	}

	void _initializeEMParameters(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates,
								 TEMPrior_base<PrecisionType, NumStatesType, LengthType> &) override {
		using namespace coretools::instances;
		this->_latentVariable.initializeEMParameters();
		if (!this->_EMEstimatePrior) { return; }
		if (_EMPrior.initializationType() == TypeTransMatInitialization::smart) {
			// do another HMM
			using namespace coretools::instances;
			if (this->_writeLog) {
				logfile().startIndent("Running an initial Baum-Welch on the mean posterior state of "
									  "the latent variable (smart initialization):");
			}

			// run a HMM on the mean posterior states
			if (parameters().exists("smartInitTraceFile")) {
				_latVarBeta.reportToFile(parameters().get("smartInitTraceFile"));
			}
			if (this->_writeLog) { _latVarBeta.report(); }

			_latVarBeta.initialize(_EMPrior.numStates(), ChunkEnds.back(), &this->_latentVariable);
			if (_latVarBeta.allEmissionsAreEqual()) {
				logfile().warning("All mean posterior states for emission probabilities are exactly equal! Can not run "
								  "smart initialization, will skip this step.");
				_EMPrior.setInitialization(TypeTransMatInitialization::defaultValues);
				return;
			}

			// run HMM
			size_t maxIterations          = parameters().get("numIterSmartInitialization", 1000);
			double minDeltaLL             = parameters().get("minDeltaLLSmartInitialization", 0.5);
			std::string filenameLLTrace   = parameters().get("nameLLTraceSmartInitalization", "");
			bool estimateTransitionMatrix = true;
			_EMPrior.setInitialization(TypeTransMatInitialization::defaultValues);
			THMM<PrecisionType, NumStatesType, LengthType> hmm(_EMPrior, _latVarBeta, maxIterations, minDeltaLL,
															   estimateTransitionMatrix);
			if (this->_writeLog) { hmm.report(); }
			hmm.runEM(ChunkEnds, 1, filenameLLTrace);

			// estimate state posteriors of smoothed latent variable
			_writeStatePosteriors(hmm, ChunkEnds, _latVarBeta.numRep());

			// report to logfile
			if (this->_writeLog) {
				logfile().startIndent("Finished initial Baum-Welch on the mean posterior state of "
									  "the latent variable (smart initialization).");
				logfile().startIndent("Final estimates of phi and kappa of smart latent variable: ", _latVarBeta.phi(),
									  ", ", _latVarBeta.kappa(), ".");
				logfile().endIndent();
				logfile().endIndent();
			}

			// make sure initialization is not called again
			_EMPrior.setInitialization(TypeTransMatInitialization::none);
		} else {
			_EMPrior.initializeEMParameters(_latentVariable, ChunkEnds, NumReplicates);
		}
	}

	// standard HMM functions
	PrecisionType _runForward(LengthType First, LengthType Last, size_t Rep,
							  THMMForwardAlpha<PrecisionType, NumStatesType, LengthType> &Alpha) {
		if (_writeLog) { coretools::instances::logfile().listFlushTime("Running forward-algorithm ..."); }

		// ensure capacity
		int dist = Last - First;
		if (dist < 1) { throw coretools::TDevError("distance between First and Last is < 1!"); }

		// calculate first alpha
		THMMEmission<PrecisionType, NumStatesType> emission(_EMPrior.numStates());
		_latentVariable.calculateEmissionProbabilities(First, Rep, emission);
		Alpha.startCalculationsForwards(Rep, _EMPrior, emission);

		// loop across others
		for (LengthType i = First + 1; i < Last; ++i) {
			_latentVariable.calculateEmissionProbabilities(i, Rep, emission);
			Alpha.moveCalculationsForward(i, Rep, _EMPrior, emission);
		}

		// done
		if (_writeLog) { coretools::instances::logfile().doneTime(); }

		// return LL
		return Alpha.LL();
	};

	void _runBackward(LengthType First, LengthType Last, size_t Rep,
					  THMMBackwardBeta<PrecisionType, NumStatesType, LengthType> &Beta) {
		if (_writeLog) { coretools::instances::logfile().listFlushTime("Running backward-algorithm ..."); }

		// calculate last beta
		THMMEmission<PrecisionType, NumStatesType> emission(_EMPrior.numStates());
		Beta.startCalculationsBackwards(_EMPrior);

		// loop across others
		// index runs from Last to 1 to be able to use unsigned
		for (LengthType i = Last - 1; i > First; --i) {
			// calculate emission at next (index i points to next data)
			_latentVariable.calculateEmissionProbabilities(i, Rep, emission);

			// iterate backward
			Beta.moveCalculationsBackward(i, Rep, _EMPrior, emission);
		}

		// done
		if (_writeLog) { coretools::instances::logfile().doneTime(); }
	};

	void _runForward(LengthType First, size_t Rep, void (THMM::*handle)(LengthType, size_t)) {
		if (_writeLog) {
			coretools::instances::logfile().listFlushTime("Running forward-algorithm (posterior decoding) ...");
		}

		// calculate first alpha
		THMMEmission<PrecisionType, NumStatesType> emission(_EMPrior.numStates());
		_latentVariable.calculateEmissionProbabilities(First, Rep, emission);
		_forwardBackward.startCalculationsForwards(Rep, _EMPrior, emission);
		_curForwardIsAtBeginningOfAChunk = true;
		(this->*handle)(First, Rep);

		// loop across others
		_curForwardIsAtBeginningOfAChunk = false;
		LengthType Last                  = First + _forwardBackward.length();
		for (LengthType i = First + 1; i < Last; ++i) {
			// move
			_latentVariable.calculateEmissionProbabilities(i, Rep, emission);
			_forwardBackward.moveCalculationsForward(i, Rep, _EMPrior, emission);
			(this->*handle)(i, Rep);
		}

		if (_writeLog) { coretools::instances::logfile().doneTime(); }
	};

	// function to run EM
	PrecisionType _runEMOneIteration_OneChunk_OneRep(
		size_t Chunk, LengthType First, LengthType Last, size_t Rep,
		void (TEM_base<PrecisionType, NumStatesType, LengthType>::*handle)(LengthType, size_t)) override {

		// run backward algorithm and store beta
		LengthType length = Last - First;
		_forwardBackward.beta().resize(_EMPrior.numStates(), length);
		_EMPrior.startNewChunk(Chunk, Rep);
		_runBackward(First, Last, Rep, _forwardBackward.beta());

		// run forward algorithm with handle
		_runForward(First, Rep, handle);
		return _forwardBackward.alpha().LL();
	}

	// function to calculate likelihood
	PrecisionType _calculateLL_OneChunk_OneRep(size_t Chunk, LengthType First, LengthType Last, size_t Rep) override {
		_EMPrior.startNewChunk(Chunk, Rep);
		return _runForward(First, Last, Rep, _forwardBackward.alpha());
	};

	// handle functions
	void _handleEMParameterEstimationOneIteration(LengthType Index, size_t Rep) override {
		if (this->_EMEstimatePrior) {
			if (_curForwardIsAtBeginningOfAChunk && _forwardBackward.gammaCalculated()) {
				_EMPrior.handleEMParameterEstimationOneIterationInitialDistribution(Index, Rep,
																					_forwardBackward.gamma());
			} else if (_forwardBackward.xiCalculated()) {
				_EMPrior.handleEMParameterEstimationOneIterationTransitionProbabilities(Index, Rep,
																						_forwardBackward.xi());
			}
		}
		_latentVariable.handleEMParameterEstimationOneIteration(Index, Rep, _forwardBackward.gamma());
	};

	void _handleStatePosteriorEstimation(LengthType Index, size_t Rep) override {
		_latentVariable.handleStatePosteriorEstimation(Index, Rep, _forwardBackward.gamma());
	};

	void _handleStatePosteriorEstimationAndWriting(LengthType Index, size_t Rep) override {
		_latentVariable.handleStatePosteriorEstimationAndWriting(Index, Rep, _forwardBackward.gamma(), this->_out);
	};

public:
	THMM(TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> &EMPrior,
		 TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatentVariable, bool WriteLog = false)
		: TEM_base<PrecisionType, NumStatesType, LengthType>(LatentVariable, WriteLog), _EMPrior(EMPrior) {
		_curForwardIsAtBeginningOfAChunk = false;
	};

	THMM(TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> &EMPrior,
		 TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatentVariable,
		 uint16_t BaumWelchMaxNumIterations, double BaumWelchMinDeltaLL, bool BaumWelchEstimateTransitionMatrix)
		: TEM_base<PrecisionType, NumStatesType, LengthType>(LatentVariable, BaumWelchMaxNumIterations,
															 BaumWelchMinDeltaLL, BaumWelchEstimateTransitionMatrix),
		  _EMPrior(EMPrior) {
		_curForwardIsAtBeginningOfAChunk = false;
	};

	THMM(TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> &EMPrior,
		 TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatentVariable,
		 uint16_t BaumWelchMaxNumIterations, double BaumWelchMinDeltaLL, bool BaumWelchEstimateTransitionMatrix,
		 uint16_t EMNumIterationsOnSubset, coretools::Probability FracSubset)
		: TEM_base<PrecisionType, NumStatesType, LengthType>(LatentVariable, BaumWelchMaxNumIterations,
															 BaumWelchMinDeltaLL, BaumWelchEstimateTransitionMatrix,
															 EMNumIterationsOnSubset, FracSubset),
		  _EMPrior(EMPrior) {
		_curForwardIsAtBeginningOfAChunk = false;
	};

	virtual ~THMM() = default;

	PrecisionType runEM(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates = 1,
						std::string_view Filename = "") {
		_forwardBackward.enableGammaCalculation();
		if (this->_EMEstimatePrior) { _forwardBackward.enableXiCalculation(); }
		return this->_runEM(ChunkEnds, NumReplicates, Filename, _EMPrior);
	}

	PrecisionType runSQUAREM(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates = 1,
							 std::string_view Filename = "") {
		_forwardBackward.enableGammaCalculation();
		if (this->_EMEstimatePrior) { _forwardBackward.enableXiCalculation(); }
		return this->_runSQUAREM(ChunkEnds, NumReplicates, Filename, _EMPrior);
	}

	PrecisionType calculateLL(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates = 1) {
		// we don't gamma and xi for this
		return this->_calculateLL(ChunkEnds, NumReplicates, _EMPrior);
	}

	PrecisionType estimateStatePosteriors(const std::vector<LengthType> &ChunkEnds, bool CalculateXi,
										  size_t NumReplicates = 1) {
		_forwardBackward.enableGammaCalculation();
		if (CalculateXi) { _forwardBackward.enableXiCalculation(); }
		return this->_estimateStatePosteriors(ChunkEnds, NumReplicates, _EMPrior);
	}

	PrecisionType estimateStatePosteriors(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates,
										  std::string_view Filename) {
		_forwardBackward.enableGammaCalculation();
		return this->_estimateStatePosteriors(ChunkEnds, NumReplicates, Filename, _EMPrior);
	}

	PrecisionType estimateStatePosteriors(const std::vector<LengthType> &ChunkEnds, std::string_view Filename) {
		return estimateStatePosteriors(ChunkEnds, 1, Filename);
	}

	// getters of TLatentVariableBeta
	const TLatentVariableBeta<PrecisionType, NumStatesType, LengthType> &getLatVarBeta() const { return _latVarBeta; };
	TLatentVariableBeta<PrecisionType, NumStatesType, LengthType> &getLatVarBeta() { return _latVarBeta; };
};

}; // end namespace stattools

#endif /* THMM_H_ */
