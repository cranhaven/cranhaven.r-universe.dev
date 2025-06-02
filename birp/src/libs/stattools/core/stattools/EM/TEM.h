//
// Created by madleina on 12.01.21.
//

#ifndef TEM_H
#define TEM_H

#include "coretools/Main/TLog.h"
#include "coretools/Main/TParameters.h"
#include "stattools/EM/TEMVars.h"
#include "stattools/EM/TLatentVariable.h"
#include "stattools/EM/TSQUAREM.h"
#include "stattools/HMM/THMMVars.h"

namespace stattools {

//-------------------------------------
// TEM_base
// a pure virtual base class
//-------------------------------------

template<typename PrecisionType, typename NumStatesType, typename LengthType> class TEM_base {
protected:
	// latent variable
	TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &_latentVariable;

	// logfile
	bool _writeLog;

	// file for writing
	coretools::TOutputFile _out;

	// settings
	size_t _EMNumIterations     = 100;
	PrecisionType _EMMinDeltaLL = 0.0001;
	bool _EMEstimatePrior       = true;

	// run (first) EM iterations on subset of data?
	bool _runEMOnSubset             = false;
	size_t _EMNumIterationsOnSubset = 0;
	coretools::Probability _fracOfSubset{1.0};
	size_t _totalSizeOfSubset = 0;

	// store delta LL
	PrecisionType _deltaLL = 0.0;

	// Auxiliary functions
	void _initFromParameters() {
		using coretools::P;
		using coretools::instances::parameters;
		// reading estimation parameters
		_EMMinDeltaLL            = parameters().get("deltaLL", 0.0001);
		_EMNumIterations         = parameters().get("iterations", 100);
		_EMNumIterationsOnSubset = parameters().get("iterationsSubset", 0);
		if (_EMNumIterationsOnSubset > 0) {
			_runEMOnSubset           = true;
			_EMNumIterationsOnSubset = std::min(_EMNumIterationsOnSubset, _EMNumIterations);
			_fracOfSubset            = parameters().get("fracSubset", P(0.1));
		}
	};

	void _reportParameters() {
		// reading estimation parameters
		coretools::instances::logfile().startIndent("EM parameters:");
		coretools::instances::logfile().list("Will run the EM algorithm until deltaLL < ", _EMMinDeltaLL,
											 ". (argument 'deltaLL')");
		coretools::instances::logfile().list("Will run at max ", _EMNumIterations,
											 " EM iterations. (argument 'iterations')");
		if (_runEMOnSubset) {
			coretools::instances::logfile().list("Will run at first ", _EMNumIterationsOnSubset,
												 " EM iterations on a subset of ", _fracOfSubset * 100.0,
												 "% of the data (arguments 'iterationsSubset' and 'fracSubset')");
		}
		coretools::instances::logfile().endIndent();
	};

	size_t _getTotalSizeOfSubset(const std::vector<LengthType> &ChunkEnds) {
		// total size of data corresponds to last element in ChunkEnds vector
		return std::round(_fracOfSubset * ChunkEnds.back());
	}

	// initialize EM parameters prior to EM
	void _prepareEMParameterEstimationInitial(TEMPrior_base<PrecisionType, NumStatesType, LengthType> &EMPrior) {
		_latentVariable.prepareEMParameterEstimationInitial();
		if (_EMEstimatePrior) { EMPrior.prepareEMParameterEstimationInitial(); }
	}

	virtual void _initializeEMParameters(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates,
										 TEMPrior_base<PrecisionType, NumStatesType, LengthType> &EMPrior) {
		_latentVariable.initializeEMParameters();
		if (_EMEstimatePrior) { EMPrior.initializeEMParameters(_latentVariable, ChunkEnds, NumReplicates); }
	}

	// functions to run EM
	virtual PrecisionType _runEMOneIteration_OneChunk_OneRep(size_t Chunk, LengthType First, LengthType Last,
															 size_t Rep,
															 void (TEM_base::*handle)(LengthType, size_t)) = 0;

	PrecisionType _runEMOneIteration_OneChunk(size_t Chunk, LengthType First, LengthType Last, size_t NumReplicates,
											  void (TEM_base::*handle)(LengthType, size_t)) {
		double LL = 0.0;
		for (size_t r = 0; r < NumReplicates; r++) {
			LL += _runEMOneIteration_OneChunk_OneRep(Chunk, First, Last, r, handle);
		}
		return LL;
	};

	PrecisionType _runEMOneIteration(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates,
									 void (TEM_base::*handle)(LengthType, size_t)) {
		using namespace coretools::instances;

		// loop across chunks when running EM
		LengthType first           = 0;
		PrecisionType LL           = 0.0;
		size_t currentSizeOfSubset = 0;
		for (size_t j = 0; j < ChunkEnds.size(); ++j) {
			LengthType last   = ChunkEnds[j];
			LengthType length = last - first;
			bool restricted   = false;
			if (_runEMOnSubset && currentSizeOfSubset + length > _totalSizeOfSubset) {
				length     = _totalSizeOfSubset - currentSizeOfSubset; // restrict length to subset size
				last       = first + length;
				restricted = true;
			}

			// report chunk
			if (ChunkEnds.size() > 1 && _writeLog) {
				logfile().startIndent("Running chunk ", j + 1, " of ", ChunkEnds.size(), ":");
				if (restricted) {
					logfile().list("Chunk is longer than size of subset, restricting it to ", length, " with indexes [",
								   first, ", ", last, ").");
				} else {
					logfile().list("Chunk is of length ", length, " with indexes [", first, ", ", last, ").");
				}
			}

			// run EM on current chunk
			LL += _runEMOneIteration_OneChunk(j, first, last, NumReplicates, handle);

			// update first of next
			first = last;

			// end of chunk
			if (ChunkEnds.size() > 1 && _writeLog) { coretools::instances::logfile().endIndent(); }
			if (_runEMOnSubset) {
				currentSizeOfSubset += length;
				if (currentSizeOfSubset == _totalSizeOfSubset) { break; } // finished subset
			}
		}
		return LL;
	};

	// function to calculate LL
	virtual PrecisionType _calculateLL_OneChunk_OneRep(size_t Chunk, LengthType First, LengthType Last, size_t Rep) = 0;

	PrecisionType _calculateLL_OneChunk(size_t Chunk, LengthType First, LengthType Last, size_t NumReplicates) {
		double LL = 0.0;
		for (size_t r = 0; r < NumReplicates; r++) { LL += _calculateLL_OneChunk_OneRep(Chunk, First, Last, r); }
		return LL;
	};

	// prepare EM
	void _prepareEMParameterEstimationOneIteration(TEMPrior_base<PrecisionType, NumStatesType, LengthType> &EMPrior) {
		_latentVariable.prepareEMParameterEstimationOneIteration();
		if (_EMEstimatePrior) { EMPrior.prepareEMParameterEstimationOneIteration(); }
	};

	// handle functions
	virtual void _handleEMParameterEstimationOneIteration(LengthType Index, size_t Rep)  = 0;
	virtual void _handleStatePosteriorEstimation(LengthType Index, size_t Rep)           = 0;
	virtual void _handleStatePosteriorEstimationAndWriting(LengthType Index, size_t Rep) = 0;

	// finalize iteration and EM
	void _finalizeEMParameterEstimationOneIteration(TEMPrior_base<PrecisionType, NumStatesType, LengthType> &EMPrior) {
		if (_EMEstimatePrior) { EMPrior.finalizeEMParameterEstimationOneIteration(); }
		_latentVariable.finalizeEMParameterEstimationOneIteration();
	};

	void _finalizeEMParameterEstimationFinal(TEMPrior_base<PrecisionType, NumStatesType, LengthType> &EMPrior) {
		_latentVariable.finalizeEMParameterEstimationFinal();
		if (_EMEstimatePrior) { EMPrior.finalizeEMParameterEstimationFinal(); }
	};

	void _reportEMParameters(TEMPrior_base<PrecisionType, NumStatesType, LengthType> &EMPrior) {
		if (_EMEstimatePrior) { EMPrior.reportEMParameters(); }
		_latentVariable.reportEMParameters();
	};

	void _prepareEM(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates, std::string_view FilenameLLTrace,
					TEMPrior_base<PrecisionType, NumStatesType, LengthType> &EMPrior,
					coretools::TOutputFile &FileLLTrace) {
		// use EM to infer parameters of the model
		if (_writeLog) {
			coretools::instances::logfile().startNumbering(
				"Running EM iterations to estimate hierarchical parameters:");
		}

		// write LL trace?
		if (!FilenameLLTrace.empty()) { FileLLTrace.open(FilenameLLTrace); }

		// prepare initial EM steps: initialize storage
		_prepareEMParameterEstimationInitial(EMPrior);

		// initialize EM parameters to some values
		_initializeEMParameters(ChunkEnds, NumReplicates, EMPrior);

		// run first EM iterations on a subset of the data only?
		if (_EMNumIterationsOnSubset > 0) {
			_runEMOnSubset     = true;
			_totalSizeOfSubset = _getTotalSizeOfSubset(ChunkEnds);
		} else {
			_totalSizeOfSubset = ChunkEnds.back();
		}
		_latentVariable.setSizeOfSubset(_totalSizeOfSubset);
	}

	void _stopRunningEMOnSubset(const std::vector<LengthType> &ChunkEnds) {
		_runEMOnSubset     = false;
		_totalSizeOfSubset = ChunkEnds.back();
		_latentVariable.setSizeOfSubset(_totalSizeOfSubset);
	}

	PrecisionType _runOneIteration_EM(size_t &iteration, const std::vector<LengthType> &ChunkEnds, size_t NumReplicates,
									  TEMPrior_base<PrecisionType, NumStatesType, LengthType> &EMPrior,
									  coretools::TOutputFile &FileLLTrace, double PreviousLL,
									  bool FirstIterAfterFinishingSubset) {
		using namespace coretools::instances;

		if (_writeLog) {
			std::string l = "EM iteration " + coretools::str::toString(iteration + 1);
			if (_runEMOnSubset) { l.append(" (running on subset)"); }
			logfile().numberWithIndent(l + ":");
		}

		// prepare parameter updates
		_prepareEMParameterEstimationOneIteration(EMPrior);

		// run one EM iteration
		PrecisionType LL =
			_runEMOneIteration(ChunkEnds, NumReplicates, &TEM_base::_handleEMParameterEstimationOneIteration);

		// update parameters
		coretools::TTimer start;
		if (_writeLog) { logfile().startIndent("Updating parameters ..."); }
		_finalizeEMParameterEstimationOneIteration(EMPrior);
		if (_writeLog) {
			logfile().list("Done in ", start.formattedTime(), "!");
			logfile().endIndent();
			_reportEMParameters(EMPrior);
		}

		// report
		if (FileLLTrace.isOpen()) { FileLLTrace.writeln(LL); }
		if (_writeLog) { logfile().list("Current LL = ", LL, "."); }
		_deltaLL = LL - PreviousLL;
		if (_writeLog && iteration > 0 && !FirstIterAfterFinishingSubset) {
			logfile().conclude("Delta LL = ", _deltaLL, ".");
		}

		++iteration;

		return LL;
	}

	bool _EMHasConverged(size_t iteration, bool FirstIterAfterFinishingSubset) {
		if (iteration > 1 && !_runEMOnSubset && !FirstIterAfterFinishingSubset) {
			// only allow for termination based on convergence one iteration and after finishing subset iterations
			// TODO: ask LatentVariable for LL from normalization!
			// -> in some cases, emission probabilities are so small that you want to normalize them with their max;
			//    but then, LL is not comparable over EM iterations -> LatentVariable should keep track of
			//    normalization LL this should be accounted for here!
			if (_deltaLL < _EMMinDeltaLL) {
				if (_writeLog) {
					coretools::instances::logfile().conclude("EM has converged!");
					coretools::instances::logfile().endIndent();
				}
				return true;
			}
		}
		return false;
	}

	virtual PrecisionType _runEM(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates,
								 std::string_view FilenameLLTrace,
								 TEMPrior_base<PrecisionType, NumStatesType, LengthType> &EMPrior) {
		using namespace coretools::instances;

		coretools::TOutputFile fileLLTrace;
		_prepareEM(ChunkEnds, NumReplicates, FilenameLLTrace, EMPrior, fileLLTrace);

		// tmp variables for EM
		PrecisionType previousLL = 0.0;
		size_t iteration         = 0;

		// run iterations
		bool firstIterAfterFinishingSubset = false;
		for (; iteration < _EMNumIterations;) {
			// stop running EM on subset?
			if (iteration == _EMNumIterationsOnSubset) {
				_stopRunningEMOnSubset(ChunkEnds);
				previousLL = 0.0; // reset previous LL: can not compare subset LL with LL of all the data
				firstIterAfterFinishingSubset = true;
			}

			previousLL = _runOneIteration_EM(iteration, ChunkEnds, NumReplicates, EMPrior, fileLLTrace, previousLL,
											 firstIterAfterFinishingSubset);

			// check if we break
			if (_EMHasConverged(iteration, firstIterAfterFinishingSubset)) { break; }

			// end of iteration
			if (_writeLog) { logfile().endIndent(); }
			firstIterAfterFinishingSubset = false;
		}

		// report end
		if (iteration == _EMNumIterations && _writeLog) { logfile().conclude("Reached max number of iterations."); }

		// clean EM
		_finalizeEMParameterEstimationFinal(EMPrior);

		// end of EM
		fileLLTrace.close();
		if (_writeLog) {
			logfile().endNumbering();
			logfile().endIndent();
		}

		return previousLL;
	};

	virtual PrecisionType _runSQUAREM(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates,
									  std::string_view FilenameLLTrace,
									  TEMPrior_base<PrecisionType, NumStatesType, LengthType> &EMPrior) {
		using namespace coretools::instances;

		coretools::TOutputFile fileLLTrace;
		_prepareEM(ChunkEnds, NumReplicates, FilenameLLTrace, EMPrior, fileLLTrace);

		// tmp variables for EM
		PrecisionType previousLL = 0.0;
		size_t iteration         = 0;
		size_t cycle             = 0;

		// run iterations
		bool firstIterAfterFinishingSubset = false;
		for (;; ++cycle) { // inifinite loop
						   // stop running EM on subset?
			if (_runEMOnSubset && iteration >= _EMNumIterationsOnSubset) {
				_stopRunningEMOnSubset(ChunkEnds);
				previousLL = 0.0; // reset previous LL: can not compare subset LL with LL of all the data
				firstIterAfterFinishingSubset = true;
			}

			if (_writeLog) { logfile().startIndent("SQUAREM cycle " + coretools::str::toString(cycle + 1)); }

			// store theta_0
			TThetaSQUAREM<PrecisionType, NumStatesType, LengthType> theta_0(_latentVariable, EMPrior);

			// run full EM iteration on theta_0
			previousLL = _runOneIteration_EM(iteration, ChunkEnds, NumReplicates, EMPrior, fileLLTrace, previousLL,
											 firstIterAfterFinishingSubset);
			if (_EMHasConverged(iteration, firstIterAfterFinishingSubset)) { break; }
			if (_writeLog) { logfile().endIndent(); }

			TThetaSQUAREM<PrecisionType, NumStatesType, LengthType> theta_1(_latentVariable, EMPrior);

			// run full EM iteration on theta_1
			previousLL = _runOneIteration_EM(iteration, ChunkEnds, NumReplicates, EMPrior, fileLLTrace, previousLL,
											 firstIterAfterFinishingSubset);
			if (_EMHasConverged(iteration, firstIterAfterFinishingSubset)) { break; }
			if (_writeLog) { logfile().endIndent(); }

			// store theta_2
			TThetaSQUAREM<PrecisionType, NumStatesType, LengthType> theta_2(_latentVariable, EMPrior);
			const double LL_theta_2 = _calculateLL(ChunkEnds, NumReplicates, EMPrior);

			// create new theta_sq
			TThetaSQUAREM<PrecisionType, NumStatesType, LengthType> theta_sq(theta_0, theta_1, theta_2);
			bool valid = theta_sq.update(_latentVariable, EMPrior);
			if (!valid) { // reject because proposed values are not valid
				logfile().list("Rejecting SQUAREM jump: Non-valid parameters.");
				theta_2.update(_latentVariable, EMPrior);
			} else {
				const double LL_theta_sq = _calculateLL(ChunkEnds, NumReplicates, EMPrior);

				if (LL_theta_sq > LL_theta_2) { // accept -> run another EM iteration based on theta_sq
					logfile().list("Accepted SQUAREM jump: LL is better (", LL_theta_sq, " > ", LL_theta_2, ").");
					previousLL = _runOneIteration_EM(iteration, ChunkEnds, NumReplicates, EMPrior, fileLLTrace,
													 previousLL, firstIterAfterFinishingSubset);
					if (_writeLog) { logfile().endIndent(); }
				} else { // reject
					logfile().list("Rejecting SQUAREM jump: LL is worse (", LL_theta_sq, " < ", LL_theta_2, ").");
					theta_2.update(_latentVariable, EMPrior);
				}
			}

			// check if we break
			if (iteration >= _EMNumIterations) { break; }

			// end of iteration
			if (_writeLog) { logfile().endIndent(); }
			firstIterAfterFinishingSubset = false;
		}

		// report end
		if (iteration >= _EMNumIterations && _writeLog) { logfile().conclude("Reached max number of iterations."); }

		// clean EM
		_finalizeEMParameterEstimationFinal(EMPrior);

		// end of EM
		fileLLTrace.close();
		if (_writeLog) {
			logfile().endNumbering();
			logfile().endIndent();
			logfile().endIndent();
		}

		return previousLL;
	};

	virtual PrecisionType _calculateLL(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates,
									   TEMPrior_base<PrecisionType, NumStatesType, LengthType> &EMPrior) {
		if (_writeLog) { coretools::instances::logfile().startIndent("Calculating LL ..."); }

		// prepare
		_latentVariable.prepareLLCalculation(EMPrior.numStates());

		// loop across chunks
		LengthType first = 0;
		PrecisionType LL = 0.0;
		for (size_t j = 0; j < ChunkEnds.size(); ++j) {
			LengthType last = ChunkEnds[j];
			// report chunk
			if (ChunkEnds.size() > 1 && _writeLog) {
				coretools::instances::logfile().startIndent("Running chunk " + coretools::str::toString(j + 1) +
															" of " + coretools::str::toString(ChunkEnds.size()) + ":");
			}

			// calculate LL
			LL += _calculateLL_OneChunk(j, first, last, NumReplicates);

			// update first of next
			first = last;

			// end of chunk
			if (ChunkEnds.size() > 1 && _writeLog) { coretools::instances::logfile().endIndent(); }
		}

		// finalize
		_latentVariable.finalizeLLCalculation();

		// report
		if (_writeLog) {
			coretools::instances::logfile().endIndent();
			coretools::instances::logfile().conclude("LL = ", LL);
		}

		// return
		return LL;
	};

	PrecisionType _estimateStatePosteriors(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates,
										   TEMPrior_base<PrecisionType, NumStatesType, LengthType> &EMPrior) {
		if (_writeLog) { coretools::instances::logfile().startIndent("Estimating State Posteriors:"); }

		// estimating state posteriors on subset does not make sense, make sure this is turned off
		_runEMOnSubset     = false;
		_totalSizeOfSubset = ChunkEnds.back();
		_latentVariable.setSizeOfSubset(_totalSizeOfSubset);

		// prepare
		_latentVariable.prepareStatePosteriorEstimation(EMPrior.numStates());

		// run EM
		PrecisionType LL = _runEMOneIteration(ChunkEnds, NumReplicates, &TEM_base::_handleStatePosteriorEstimation);

		// finalize
		_latentVariable.finalizeStatePosteriorEstimation();

		// report
		if (_writeLog) {
			coretools::instances::logfile().list("Current LL = ", LL, ".");
			coretools::instances::logfile().endIndent();
		}

		return LL;
	};

	virtual PrecisionType _estimateStatePosteriors(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates,
												   std::string_view Filename,
												   TEMPrior_base<PrecisionType, NumStatesType, LengthType> &EMPrior) {
		if (_writeLog) {
			coretools::instances::logfile().startIndent("Estimating State Posteriors:");
			coretools::instances::logfile().list("Writing state posteriors to '", Filename, "'.");
		}

		// estimating state posteriors on subset does not make sense, make sure this is turned off
		_runEMOnSubset     = false;
		_totalSizeOfSubset = ChunkEnds.back();
		_latentVariable.setSizeOfSubset(_totalSizeOfSubset);

		// assemble header
		std::vector<std::string> header;
		_latentVariable.prepareStatePosteriorEstimationAndWriting(EMPrior.numStates(), header);

		// open file
		_out.open(Filename);
		_out.writeHeader(header);

		// run forward-backward
		PrecisionType LL =
			_runEMOneIteration(ChunkEnds, NumReplicates, &TEM_base::_handleStatePosteriorEstimationAndWriting);

		// finalize
		_latentVariable.finalizeStatePosteriorEstimationAndWriting();

		// report
		if (_writeLog) {
			coretools::instances::logfile().list("Current LL = ", LL, ".");
			coretools::instances::logfile().endIndent();
		}

		// close file
		_out.close();

		return LL;
	};

public:
	TEM_base(TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatentVariable, bool WriteLog = false)
		: _latentVariable(LatentVariable) {
		_writeLog = WriteLog;

		_initFromParameters();
		if (_writeLog) { _reportParameters(); }

		// other settings (potentially overwritten by derived classes)
		_EMEstimatePrior = true;
	};

	TEM_base(TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatentVariable,
			 size_t EMMaxNumIterations, double EMMinDeltaLL, bool EMEstimatePrior)
		: _latentVariable(LatentVariable) {
		_writeLog = false;

		_EMNumIterations = EMMaxNumIterations;
		_EMMinDeltaLL    = EMMinDeltaLL;
		_EMEstimatePrior = EMEstimatePrior;
	};

	TEM_base(TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatentVariable,
			 size_t EMMaxNumIterations, double EMMinDeltaLL, bool EMEstimatePrior, size_t EMNumIterationsOnSubset,
			 coretools::Probability FracSubset)
		: _latentVariable(LatentVariable) {
		_writeLog = false;

		_EMNumIterations = EMMaxNumIterations;
		_EMMinDeltaLL    = EMMinDeltaLL;
		_EMEstimatePrior = EMEstimatePrior;

		_EMNumIterationsOnSubset = EMNumIterationsOnSubset;
		_EMNumIterationsOnSubset = std::min(_EMNumIterationsOnSubset, _EMNumIterations);
		_runEMOnSubset           = (_EMNumIterationsOnSubset > 0);
		_fracOfSubset            = FracSubset;
	};

	virtual ~TEM_base() = default;

	void setEstimatePrior(bool EMEstimatePrior) { _EMEstimatePrior = EMEstimatePrior; };

	void makeSilent() { _writeLog = false; };

	void report() { _writeLog = true; };

	PrecisionType getDeltaLL() const { return _deltaLL; }
};

//-------------------------------------
// TEM
//-------------------------------------
template<typename PrecisionType, typename NumStatesType, typename LengthType>
class TEM : public TEM_base<PrecisionType, NumStatesType, LengthType> {
private:
	TEMWeights<PrecisionType, NumStatesType, LengthType> _weights;
	TEMPriorIndependent_baseWithRep<PrecisionType, NumStatesType, LengthType> &_EMPrior;

	using TEM_base<PrecisionType, NumStatesType, LengthType>::_latentVariable;
	using TEM_base<PrecisionType, NumStatesType, LengthType>::_writeLog;

protected:
	PrecisionType _runEMOneIteration_OneChunk_OneRep(
		LengthType /*Index*/, LengthType First, LengthType Last, size_t Rep,
		void (TEM_base<PrecisionType, NumStatesType, LengthType>::*handle)(LengthType, size_t)) override {

		if (_writeLog) { coretools::instances::logfile().listFlushTime("Running EM ..."); }
		// calculate first alpha
		THMMEmission<PrecisionType, NumStatesType> emission(_EMPrior.numStates());
		_latentVariable.calculateEmissionProbabilities(First, Rep, emission);
		_weights.startCalculationsForwards(Rep, _EMPrior, emission);
		(this->*handle)(First, Rep);

		// loop across others
		for (LengthType i = First + 1; i < Last; ++i) {
			// move
			_latentVariable.calculateEmissionProbabilities(i, Rep, emission);
			_weights.moveCalculationsForward(i, Rep, _EMPrior, emission);
			(this->*handle)(i, Rep);
		}

		if (_writeLog) { coretools::instances::logfile().doneTime(); }

		return _weights.LL();
	};

	// function to calculate likelihood
	PrecisionType _calculateLL_OneChunk_OneRep(LengthType /*Index*/, LengthType First, LengthType Last,
											   size_t Rep) override {
		if (_writeLog) { coretools::instances::logfile().listFlushTime("Running EM ..."); }

		THMMEmission<PrecisionType, NumStatesType> emission(_EMPrior.numStates());
		_latentVariable.calculateEmissionProbabilities(First, Rep, emission);
		_weights.startCalculationsForwards(Rep, _EMPrior, emission);

		// loop across others
		for (LengthType i = First + 1; i < Last; ++i) {
			// move
			_latentVariable.calculateEmissionProbabilities(i, Rep, emission);
			_weights.moveCalculationsForward(i, Rep, _EMPrior, emission);
		}

		if (_writeLog) { coretools::instances::logfile().doneTime(); }

		return _weights.LL();
	};

	// handle functions
	void _handleStatePosteriorEstimation(LengthType Index, size_t Rep) override {
		_latentVariable.handleStatePosteriorEstimation(Index, Rep, _weights.weights());
	};

	void _handleStatePosteriorEstimationAndWriting(LengthType Index, size_t Rep) override {
		_latentVariable.handleStatePosteriorEstimationAndWriting(Index, Rep, _weights.weights(), this->_out);
	};

	void _handleEMParameterEstimationOneIteration(LengthType Index, size_t Rep) override {
		if (this->_EMEstimatePrior) {
			_EMPrior.handleEMParameterEstimationOneIteration(Index, Rep, _weights.weights());
		}
		_latentVariable.handleEMParameterEstimationOneIteration(Index, Rep, _weights.weights());
	};

public:
	TEM(TEMPriorIndependent_baseWithRep<PrecisionType, NumStatesType, LengthType> &EMPrior,
		TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatentVariable, bool WriteLog = true)
		: TEM_base<PrecisionType, NumStatesType, LengthType>(LatentVariable, WriteLog), _EMPrior(EMPrior){};

	TEM(TEMPriorIndependent_baseWithRep<PrecisionType, NumStatesType, LengthType> &EMPrior,
		TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatentVariable, size_t EMMaxNumIterations,
		double EMMinDeltaLL, bool EMEstimatePrior)
		: TEM_base<PrecisionType, NumStatesType, LengthType>(LatentVariable, EMMaxNumIterations, EMMinDeltaLL,
															 EMEstimatePrior),
		  _EMPrior(EMPrior){};

	TEM(TEMPriorIndependent_baseWithRep<PrecisionType, NumStatesType, LengthType> &EMPrior,
		TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatentVariable, size_t EMMaxNumIterations,
		double EMMinDeltaLL, bool EMEstimatePrior, size_t EMNumIterationsOnSubset, coretools::Probability FracSubset)
		: TEM_base<PrecisionType, NumStatesType, LengthType>(LatentVariable, EMMaxNumIterations, EMMinDeltaLL,
															 EMEstimatePrior, EMNumIterationsOnSubset, FracSubset),
		  _EMPrior(EMPrior){};

	PrecisionType runEM(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates = 1,
						std::string_view FilenameLLTrace = "") {
		return this->_runEM(ChunkEnds, NumReplicates, FilenameLLTrace, _EMPrior);
	}

	PrecisionType runSQUAREM(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates = 1,
							 std::string_view FilenameLLTrace = "") {
		return this->_runSQUAREM(ChunkEnds, NumReplicates, FilenameLLTrace, _EMPrior);
	}

	PrecisionType calculateLL(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates = 1) {
		return this->_calculateLL(ChunkEnds, NumReplicates);
	}

	PrecisionType estimateStatePosteriors(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates = 1) {
		return this->_estimateStatePosteriors(ChunkEnds, NumReplicates, _EMPrior);
	}

	PrecisionType estimateStatePosteriors(const std::vector<LengthType> &ChunkEnds, size_t NumReplicates,
										  std::string_view Filename) {
		return this->_estimateStatePosteriors(ChunkEnds, NumReplicates, Filename, _EMPrior);
	}

	PrecisionType estimateStatePosteriors(const std::vector<LengthType> &ChunkEnds, std::string_view Filename) {
		return estimateStatePosteriors(ChunkEnds, 1, Filename);
	}
};

}; // end namespace stattools

#endif // TEM_H
