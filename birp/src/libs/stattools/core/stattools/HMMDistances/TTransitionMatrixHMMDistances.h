//
// Created by madleina on 31.07.20.
//

#ifndef TTRANSITIONMATRIXEXPONENTIAL_H
#define TTRANSITIONMATRIXEXPONENTIAL_H

#include <cmath>
#include <memory>
#include <vector>
#include "coretools/arma_include.h"
#include "coretools/Files/TOutputFile.h"
#include "coretools/Distances/TDistances.h"
#include "stattools/HMM/THMM.h"

namespace stattools {

//-------------------------------------------
// TTransitionMatrixDistancesOptimizerBase
//-------------------------------------------

template<typename PrecisionType, typename NumStatesType, typename LengthType>
class TTransitionMatrixDistancesOptimizerBase {
private:
	using VecTau = std::vector<arma::mat>;

protected:
	bool _reportLog = false;

	virtual bool _fillTransitionProbabilitiesFromStartValues(VecTau &Tau)                               = 0;
	virtual bool _fillTransitionProbabilities(coretools::TConstView<PrecisionType> Values, VecTau &Tau) = 0;
	virtual bool _fillStationaryDistributionFromStartValues(VecTau &Tau)                                = 0;
	virtual bool _fillStationaryDistribution(coretools::TConstView<PrecisionType> Values, VecTau &Tau)  = 0;

public:
	virtual ~TTransitionMatrixDistancesOptimizerBase() = default;

	virtual std::vector<PrecisionType> setValuesEM(coretools::TConstView<PrecisionType> Values, VecTau &Tau) = 0;

	void report() { _reportLog = true; }
	bool doReport() const { return _reportLog; }

	bool fillProbabilitiesFromStartValues(VecTau &Tau) {
		if (!_fillTransitionProbabilitiesFromStartValues(Tau)) { return false; }
		if (!_fillStationaryDistributionFromStartValues(Tau)) { return false; }
		return true;
	}

	bool fillProbabilities(coretools::TConstView<PrecisionType> Values, VecTau &Tau) {
		if (!_fillTransitionProbabilities(Values, Tau)) { return false; }
		if (!_fillStationaryDistribution(Values, Tau)) { return false; }
		return true;
	}

	virtual std::vector<PrecisionType> maximizeQFunction(VecTau &Tau, VecTau &Xi,
														 const std::vector<std::vector<PrecisionType>> &Gamma,
														 bool FirstIteration, bool LastIteration) = 0;

	virtual size_t numParameters() const                            = 0;
	virtual size_t numStates() const                                = 0;
	virtual void fillHeaderEMReportFile(std::vector<std::string> &) = 0;
};

//-------------------------------------------
// TTransitionMatrixDistances
//-------------------------------------------

template<typename PrecisionType, typename NumStatesType, typename LengthType>
class TTransitionMatrixDistances : public TTransitionMatrix_base<PrecisionType, NumStatesType, LengthType> {
	// base class for HMMs where the transition matrix depends on a distance between two neighbouring points

	// this class stores two transition matrices
	// for MCMC: these are old and new transition matrices
	// for EM: old transition matrix is used to store EM sums for M-Step
private:
	// some typedefs
	using Base       = TTransitionMatrix_base<PrecisionType, NumStatesType, LengthType>;
	using VecTau     = std::vector<arma::mat>;
	using TOptimizer = TTransitionMatrixDistancesOptimizerBase<PrecisionType, NumStatesType, LengthType>;

protected:
	// _tau_allDist is of size numDistanceGroups
	// _tau_allDist[0] = stationary probabilities
	// _tau_allDist[1] = transition probabilities for distance group 1 -> calculated in specific way
	// _tau_allDist[2:end] = transition probabilities for all other distance groups, calculated as
	//                            _tau_allDist[i] = _tau_allDist[i-1] * _tau_allDist[i-1]

	// number of states
	using Base::_numStates;

	// distances
	coretools::TDistancesBinnedBase *_distances = nullptr;

	// optimizer
	std::shared_ptr<TOptimizer> _optimizer;

	// transition matrices, old and new
	VecTau _tau_allDist;
	VecTau _tau_allDist_old;

	// settings for EM
	bool _report                      = false;
	size_t _baumWelchMaxNumIterations = 500;
	double _baumWelchMinDeltaLL       = 0.0001;

	// temporary variables for EM
	std::vector<std::vector<PrecisionType>> _EM_colSums;
	bool _firstIteration                = true;
	bool _fixTransitionMatricesDuringEM = false;

	// report
	std::vector<PrecisionType> _curParameterEstimatesEM;
	coretools::TOutputFile _fileReportEM;

	void _resizeDistances() {
		if (!_distances) { DEVERROR("distances are not initialized!"); }
		_tau_allDist.resize(_distances->numDistanceGroups(), arma::mat(_numStates, _numStates, arma::fill::zeros));
		_tau_allDist_old.resize(_distances->numDistanceGroups(), arma::mat(_numStates, _numStates, arma::fill::zeros));
	}

	void _runEMEstimationOrStatePosteriors(TLatentVariable<double, size_t, size_t> &LatentVariable, bool RunEM) {
		_optimizer->report();
		_firstIteration = true;

		// update transition matrices in EM?
		const bool estimateTransitionMatrix = true;
		THMM<PrecisionType, NumStatesType, LengthType> hmm(*this, LatentVariable, _baumWelchMaxNumIterations,
														   _baumWelchMinDeltaLL, estimateTransitionMatrix);
		hmm.report();

		// get chunk ends from distances
		std::vector<LengthType> chunkEnds = _distances->getChunkEnds<LengthType>();

		if (RunEM) {
			hmm.runEM(chunkEnds);
			hmm.estimateStatePosteriors(chunkEnds, false); // initialize z only
		} else {
			hmm.estimateStatePosteriors(chunkEnds, false); // initialize z only
		}
	};

	[[nodiscard]] bool _checkRowSumTransitionMatrices() const {
		// returns false if any row of any transition matrix does not sum to 1 (allow for +- absError difference)
		double absError = 0.0001;

		for (size_t d = 1; d < _distances->numDistanceGroups(); d++) {
			for (NumStatesType i = 0; i < _numStates; i++) { // rows
				double sum = arma::sum(_tau_allDist[d].row(i));
				if (std::fabs(1. - sum) > absError) { return false; }
			}
		}
		return true;
	};

	void _openEMReportFile(std::string_view Filename, const std::vector<std::string> &Header) {
		// open file and write header
		_fileReportEM.open(Filename, Header);
	}

	void _writeToEMReportFile() {
		if (_fileReportEM.isOpen()) { // only write if developer opened file
			writeToEMReportFile(_fileReportEM);
			_fileReportEM.endln();
		}
	}

public:
	TTransitionMatrixDistances(coretools::TDistancesBinnedBase *Distances,
							   const std::shared_ptr<TOptimizer> &Optimizer)
		: TTransitionMatrix_base<PrecisionType, NumStatesType, LengthType>(Optimizer->numStates()),
		  _distances(Distances), _optimizer(Optimizer) {

		_resizeDistances();
	};

	TTransitionMatrixDistances(coretools::TDistancesBinnedBase *Distances,
							   const std::shared_ptr<TOptimizer> &Optimizer, std::string_view Filename)
		: TTransitionMatrix_base<PrecisionType, NumStatesType, LengthType>(Optimizer->numStates()),
		  _distances(Distances), _optimizer(Optimizer) {

		_resizeDistances();

		// open EM report file
		std::vector<std::string> header;
		_optimizer->fillHeaderEMReportFile(header);
		_openEMReportFile(Filename, header);
	};

	void setBaumWelchSettings(uint16_t BaumWelchMaxNumIterations, double BaumWelchMinDeltaLL) {
		_baumWelchMaxNumIterations = BaumWelchMaxNumIterations;
		_baumWelchMinDeltaLL       = BaumWelchMinDeltaLL;
	}

	void report() {
		_optimizer->report();
		_report = true;
	};

	std::vector<PrecisionType> getParameters() const override { return _curParameterEstimatesEM; }

	bool setParameters(coretools::TConstView<PrecisionType> Params) override { return fillProbabilities(Params); };

	void setValuesEM(coretools::TConstView<PrecisionType> Values) {
		_curParameterEstimatesEM = _optimizer->setValuesEM(Values, _tau_allDist);
		this->setInitialization(TypeTransMatInitialization::none);
	}

	void noInitializationBeforeEM() { this->setInitialization(TypeTransMatInitialization::none); }

	void fixTransitionMatricesDuringEM() { _fixTransitionMatricesDuringEM = true; }
	void fixTransitionMatricesDuringEM(coretools::TConstView<PrecisionType> Values) {
		setValuesEM(Values);
		_fixTransitionMatricesDuringEM = true;
	};

	bool fillProbabilities(coretools::TConstView<PrecisionType> Values) {
		return _optimizer->fillProbabilities(Values, _tau_allDist);
	};

	const arma::mat &operator[](size_t Index) const {
		assert(Index < _tau_allDist.size());
		return _tau_allDist[Index];
	};

	arma::mat &operator[](size_t Index) {
		assert(Index < _tau_allDist.size());
		return _tau_allDist[Index];
	};

	PrecisionType operator()(LengthType Index, NumStatesType From, NumStatesType To) const override {
		size_t distGroup = (*_distances)[Index];
		return _tau_allDist[distGroup](From, To);
	};

	PrecisionType transitionProbability(LengthType Index, NumStatesType From, NumStatesType To) const {
		// same as operator(), just sometimes nicer to read (i.e. for derived classes)
		return operator()(Index, From, To);
	};

	PrecisionType logTransitionProbability(LengthType Index, NumStatesType From, NumStatesType To) const {
		const auto v  = transitionProbability(Index, From, To);
		const auto lv = log(v);
		return lv;
	};

	PrecisionType oldTransitionProbability(LengthType Index, NumStatesType From, NumStatesType To) const {
		size_t distGroup = (*_distances)[Index];
		return _tau_allDist_old[distGroup](From, To);
	};

	PrecisionType logOldTransitionProbability(LengthType Index, NumStatesType From, NumStatesType To) const {
		const auto v  = oldTransitionProbability(Index, From, To);
		const auto lv = log(v);
		return lv;
	};

	PrecisionType dist1_transitionProbability(NumStatesType From, NumStatesType To) const {
		return _tau_allDist[1](From, To); // first distance group
	};

	PrecisionType stationary(NumStatesType State) const override { return _tau_allDist[0](State, State); };

	void swapNewOldTau() { _tau_allDist.swap(_tau_allDist_old); };

	std::vector<PrecisionType> getFinalParameterEstimatesEM() const { return _curParameterEstimatesEM; };
	[[nodiscard]] size_t numDistanceGroups() const { return _distances->numDistanceGroups(); };
	[[nodiscard]] coretools::TDistancesBinnedBase *distances() const { return _distances; };
	[[nodiscard]] virtual size_t numParameters() const { return _optimizer->numParameters(); };

	void initializeEMParameters(const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatentVariable,
								const std::vector<LengthType> &ChunkEnds) override {
		// write to file before EM starts
		_writeToEMReportFile();

		if (this->initializationType() == TypeTransMatInitialization::ML) {
			Base::initializeEMParameters(LatentVariable, ChunkEnds);
		} else if (this->initializationType() == TypeTransMatInitialization::defaultValues) {
			_optimizer->fillProbabilitiesFromStartValues(_tau_allDist);
		}
	}

	void fillProbabilitiesFromStartValues() {
		if (this->initializationType() == TypeTransMatInitialization::defaultValues) {
			_optimizer->fillProbabilitiesFromStartValues(_tau_allDist);
		}
	}

	void handleEMParameterInitializationTransitionProbabilities(LengthType Index, NumStatesType PreviousState,
																NumStatesType CurrentState) override {
		// initialize transition matrices prior to EM, assuming z are set to some initial values -> count
		// transitions abuse _transitionMatrices_old to count transitions
		if (this->initializationType() == TypeTransMatInitialization::ML) {
			size_t distGroup = _distances->operator[](Index);
			if (distGroup == 0) {
				DEVERROR("distGroup is 0. This should never happen, as distGroup=0 is at beginning of chunk!");
			}

			// count transition z_{t-1} -> z_t
			_tau_allDist_old[distGroup](PreviousState, CurrentState)++;
			_EM_colSums[distGroup][PreviousState]++;
		}
	};

	void finalizeEMParameterInitializationTransitionProbabilities() override {
		if (this->initializationType() == TypeTransMatInitialization::ML) {
			finalizeEMParameterEstimationOneIterationTransitionProbabilities();
		}
	};

	void runEMEstimation(TLatentVariable<PrecisionType, NumStatesType, LengthType> &LatentVariable) {
		_runEMEstimationOrStatePosteriors(LatentVariable, true);
	};

	void estimateStatePosteriors(TLatentVariable<PrecisionType, NumStatesType, LengthType> &LatentVariable) {
		_runEMEstimationOrStatePosteriors(LatentVariable, false);
	};

	void prepareEMParameterEstimationInitial() override {
		_EM_colSums.resize(_distances->numDistanceGroups(), std::vector<PrecisionType>(_numStates, 0.));
	};

	void prepareEMParameterEstimationOneIterationTransitionProbabilities() override {
		for (size_t d = 1; d < _distances->numDistanceGroups(); d++) {
			std::fill(_EM_colSums[d].begin(), _EM_colSums[d].end(), 0.);
			_tau_allDist_old[d].zeros();
		}
	};

	void handleEMParameterEstimationOneIterationTransitionProbabilities(
		LengthType Index, const THMMPosteriorXi<PrecisionType, NumStatesType, LengthType> &xi) override {
		// we abuse _transitionMatrices_old to store EM sums
		size_t distGroup = _distances->operator[](Index);
		assert(distGroup != 0);

		arma::mat &sum = _tau_allDist_old[distGroup];
		for (NumStatesType i = 0; i < _numStates; i++) {     // z_{t-1}
			for (NumStatesType j = 0; j < _numStates; j++) { // z_t
				sum(i, j) += xi(i, j);
				_EM_colSums[distGroup][i] += xi(i, j);
			}
		}
	};

	void finalizeEMParameterEstimationOneIterationTransitionProbabilities() override {
		if (!_fixTransitionMatricesDuringEM) {
			_curParameterEstimatesEM =
				_optimizer->maximizeQFunction(_tau_allDist, _tau_allDist_old, _EM_colSums, _firstIteration, false);
		}
		// switch bool
		if (_firstIteration) { _firstIteration = !_firstIteration; }

		assert(_checkRowSumTransitionMatrices());

		// write to file
		_writeToEMReportFile();
	};

	void writeToEMReportFile(coretools::TOutputFile &File) const {
		for (auto p : _curParameterEstimatesEM) { File << p; }
	};

	void fillHeaderEMReportFile(std::vector<std::string> &Header) { _optimizer->fillHeaderEMReportFile(Header); }

	void reportEMParameters() override {
		if (_report) {
			coretools::instances::logfile().list("transition matrix parameters = ", _curParameterEstimatesEM);
		}
	};

	void simulateDistances(int Length) {
		// simulate random distances - only if user has not filled them!
		if (_distances->size() == 0) { _distances->simulate(Length); }
		_resizeDistances();
	};

	NumStatesType sampleNextState(LengthType Index, NumStatesType State) const override {
		size_t distGroup      = (*_distances)[Index];
		auto transitionMatrix = operator[](distGroup);

		double random   = coretools::instances::randomGenerator().getRand();
		NumStatesType s = 0;
		double cumul    = transitionMatrix(State, s);

		while (cumul <= random) {
			++s;
			cumul += transitionMatrix(State, s);
		}

		return s;
	};

	void printLLSurface(TLatentVariable<PrecisionType, NumStatesType, LengthType> &LatentVariable,
						const std::array<std::vector<PrecisionType>, 2> Grid) {
		std::vector<std::vector<PrecisionType>> LL(Grid[0].size(), std::vector<PrecisionType>(Grid[1].size()));
		for (size_t i = 0; i < Grid[0].size(); i++) {
			for (size_t j = 0; j < Grid[1].size(); j++) {
				double v1 = Grid[0][i];
				double v2 = Grid[1][j];

				_optimizer->fillProbabilities({v1, v2}, _tau_allDist);

				THMM<PrecisionType, NumStatesType, LengthType> hmm(*this, LatentVariable, 500, 0.0001, true);
				std::vector<LengthType> chunkEnds = _distances->getChunkEnds<LengthType>();

				LL[i][j] = hmm.calculateLL(chunkEnds);
			}
		}

		// print to file
		coretools::TOutputFile file("LLSurface.txt");
		for (size_t i = 0; i < Grid[0].size(); i++) { file.writeln(LL[i]); }
		file.close();
	};
};

} // end namespace stattools

#endif // TTRANSITIONMATRIXEXPONENTIAL_H
