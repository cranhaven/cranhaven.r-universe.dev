//
// Created by madleina on 10.03.23.
//

#ifndef TLATENTVARIABLEBETA_H
#define TLATENTVARIABLEBETA_H

#include "coretools/Files/TInputFile.h"
#include "coretools/Files/TOutputFile.h"
#include "coretools/Main/TParameters.h"
#include "coretools/Math/TMatrix.h"
#include "coretools/Math/mathFunctions.h"
#include "coretools/algorithms.h"
#include "stattools/EM/TLatentVariable.h"
#include "stattools/MLEInference/TNelderMead.h"
#include <queue>

//-------------------------------------
// TLatentVariableBeta
//-------------------------------------

namespace stattools {
template<typename PrecisionType, typename NumStatesType, typename LengthType>
class TLatentVariableBeta : public TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> {
protected:
	// number of states, total size and number of replicates
	size_t _numStates = 0;
	size_t _size      = 0;
	size_t _numRep    = 0;

	// store the mean posterior states of other latent variable (normalized such that it is in interval [0,1])
	coretools::TMatrix<PrecisionType> _logNormMeanPosState;
	coretools::TMatrix<PrecisionType> _logOneMinusNormMeanPosState;

	// alpha and beta of beta distribution (length = numStates)
	std::vector<PrecisionType> _alphas;
	std::vector<PrecisionType> _betas;

	// parameters of emission: phi and kappa and some noise (delta)
	coretools::Probability _phi{0.5};
	coretools::StrictlyPositive _kappa = 10.0;
	coretools::Probability _delta;

	// report
	bool _report = false;
	coretools::TOutputFile _fileReportEM;

	// store sums for Q function
	std::vector<PrecisionType> _sumGamma;
	std::vector<PrecisionType> _sumGammaLogP;
	std::vector<PrecisionType> _sumGammaLogOneMinusP;

	// settings for Nelder Mead
	size_t _iter                              = 0;
	TSimplex<2> _simplexOfPreviousEMIteration = {};
	bool _fixPhi                              = false;
	bool _fixKappa                            = false;

	// smoothing
	size_t _halfWindowSize = 0;
	coretools::TOutputFile _fileMeanPosState;

	PrecisionType _calculateMode(NumStatesType State) const {
		double normState = (_numStates == 1) ? State : (double)State / (double)(_numStates - 1.0);
		return 0.5 * _phi.complement() + normState * _phi;
	}

	std::pair<PrecisionType, PrecisionType> _calculateAlphaBeta(NumStatesType State) const {
		const auto mode  = _calculateMode(State);
		const auto alpha = 1.0 + _kappa * mode;
		const auto beta  = 1.0 + _kappa * (1.0 - mode);
		return {alpha, beta};
	}

	bool _setPhiKappa(coretools::TConstView<PrecisionType> Result) {
		if (!_fixPhi) { _phi = coretools::P(_delta + coretools::logistic(Result[0]) * (1.0 - _delta)); }
		if (!_fixKappa) {
			const auto k = exp(Result[1]);
			if (k > 100000.0) { return false; } // some upper limit: beta distributions can go crazy
			_kappa = k;
		}
		return true;
	}

	void reportEMParameters() override {
		if (_report) { coretools::instances::logfile().list("phi, kappa = [", _phi, ", ", _kappa, "]"); }
	}

	void _updateAlphaBeta() {
		for (size_t j = 0; j < _numStates; ++j) { std::tie(_alphas[j], _betas[j]) = _calculateAlphaBeta(j); }
	}

	void _storeMeanPosState(double MeanPosState, size_t rep, size_t i) {
		// squeeze into interval [0,1]
		if (_numStates > 1) { MeanPosState /= ((double)_numStates - 1.0); }

		// make sure this is never exactly zero or one: problematic for Beta distribution
		MeanPosState                         = std::max(1e-10, MeanPosState);
		MeanPosState                         = std::min(1.0 - 1e-10, MeanPosState);
		_logNormMeanPosState(rep, i)         = log(MeanPosState);
		_logOneMinusNormMeanPosState(rep, i) = log(1.0 - MeanPosState);

		if (_fileMeanPosState.isOpen()) {
			_fileMeanPosState << MeanPosState;
			if (i == _size - 1) { _fileMeanPosState.endln(); } // last index: new line
		}
	}

	void _fillMeanPosStates(const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> *Other) {
		assert(_halfWindowSize == 0);
		for (size_t rep = 0; rep < _numRep; ++rep) {
			for (size_t i = 0; i < _size; ++i) {
				double meanPosState = Other->getPosteriorMeanState(i, rep, _numStates);
				_storeMeanPosState(meanPosState, rep, i);
			}
		}
	}

	void _fillMeanPosStatesSmooth(const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> *Other) {
		size_t window = 2 * _halfWindowSize + 1;

		// prepare storage
		TDataVector<PrecisionType, NumStatesType> emission(_numStates);
		std::vector<double> log_emission(_numStates, 0.0);
		std::vector<double> sum_log_emission(_numStates, 0.0);

		for (size_t rep = 0; rep < _numRep; ++rep) {
			std::queue<std::vector<double>> fifo;
			for (size_t i = 0; i < _size + _halfWindowSize; ++i) {
				if (i < _size) { // if i is within size: add data point -> fill emission probabilities and take log
					Other->calculateEmissionProbabilities(i, rep, emission);
					for (size_t k = 0; k < _numStates; ++k) {
						const auto log_e = log(emission[k]);
						log_emission[k]  = log_e;
						sum_log_emission[k] += log_e;
					}
					fifo.push(log_emission); // add to queue
				}

				if (i >= _halfWindowSize) { // we are on the right of the first halfWindowSize positions
					if (i >= window) {
						// we moved the window -> remove data point on the left
						for (size_t k = 0; k < _numStates; ++k) { sum_log_emission[k] -= fifo.front()[k]; }
						fifo.pop();
					}

					// remove log and normalize emissions
					auto normalized_emissions = sum_log_emission;
					coretools::normalizeLogSumExp(normalized_emissions);

					// calculate weighted mean across states
					double meanPosState = 0.0;
					for (size_t k = 1; k < _numStates; ++k) { meanPosState += (double)k * normalized_emissions[k]; }

					// store: make sure to shift index by halfWindowSize
					_storeMeanPosState(meanPosState, rep, i - _halfWindowSize);
				}
			}
		}
	}

	void _fillMeanPosStatesFromFile(std::string_view FileNameMeanPosStates) {
		// read file
		coretools::TInputFile file(FileNameMeanPosStates, coretools::FileType::NoHeader);
		for (; !file.empty(); file.popFront()) {
			auto line = file.front();
			if (line.size() != _size) {
				throw coretools::TUserError("Number of elements of input file ", FileNameMeanPosStates, " (", line.size(),
					   ") does not match expected number size (", _size, ")!");
			}
			for (size_t i = 0; i < line.size(); ++i) {
				_storeMeanPosState(coretools::str::fromString<double>(line[i]), file.curLine(), i);
			}
		}
	}

	PrecisionType _calcBetaConstant(size_t j) const {
		return coretools::gammaLog(_alphas[j] + _betas[j]) - coretools::gammaLog(_alphas[j]) -
			   coretools::gammaLog(_betas[j]);
	}

	PrecisionType _calcMinusQ(coretools::TConstView<PrecisionType> Values) {
		// Values = {logit(phi), log(kappa)}
		using namespace coretools;
		if (!_setPhiKappa(Values)) { return std::numeric_limits<double>::max(); }
		_updateAlphaBeta();

		double sum = 0.0;
		for (size_t j = 0; j < _numStates; ++j) {
			const double tmp1 = _numRep * _sumGamma[j] * _calcBetaConstant(j);
			const double tmp2 = (_alphas[j] - 1.) * _sumGammaLogP[j];
			const double tmp3 = (_betas[j] - 1.) * _sumGammaLogOneMinusP[j];
			sum += tmp1 + tmp2 + tmp3;
		}
		return -sum; // return -sum, because we want to maximize Q, but Nelder-Mead minimize
	};

	void _writeToEMFile() {
		if (_fileReportEM.isOpen()) { _fileReportEM << _phi << _kappa << coretools::endl; }
	}

public:
	TLatentVariableBeta() = default;
	TLatentVariableBeta(size_t NumStates, size_t Size, size_t NumRep)
		: TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType>() {
		initialize(NumStates, Size, NumRep);
	}

	TLatentVariableBeta(size_t NumStates, size_t Size,
						const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> *OtherLatentVariable)
		: TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType>() {
		initialize(NumStates, Size, OtherLatentVariable);
	}

	TLatentVariableBeta(size_t NumStates, size_t Size, size_t NumRep, std::string_view FileNameMeanPosStates)
		: TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType>() {
		initialize(NumStates, Size, NumRep, FileNameMeanPosStates);
	}

	~TLatentVariableBeta() override = default;

	void initialize(size_t NumStates, size_t Size, size_t NumRep) {
		using namespace coretools::instances;

		_numStates = NumStates;
		_size      = Size;
		_numRep    = NumRep;

		// check if valid
		if (_numStates == 0) { throw coretools::TDevError("Number of states can not be zero!"); }
		if (_numRep == 0) { throw coretools::TDevError("Number of replicates can not be zero!"); }

		// get initial values for phi, kappa and delta from command line
		_phi   = parameters().get("initialPhiSmartInitialization", _phi);
		_kappa = parameters().get("initialKappaSmartInitialization", _kappa);
		_delta = parameters().get("delta_SmartInitialization", coretools::P(1e-03));
		if (parameters().exists("fixPhiSmartInitialization")) { _fixPhi = true; }
		if (parameters().exists("fixKappaSmartInitialization")) { _fixKappa = true; }

		// smoothing?
		_halfWindowSize = parameters().get("halfWindowSizeSmartInitialization", _halfWindowSize);

		// write?
		if (parameters().exists("writePosteriorMeanStateSmartInitialization")) {
			_fileMeanPosState.open(parameters().get("writePosteriorMeanStateSmartInitialization"));
		}

		// set initial simplex for Nelder Mead
		std::array<double, 2> init    = {coretools::logit(_phi), log(_kappa)};
		_simplexOfPreviousEMIteration = TSimplex<2>(init, 0.1);

		// fill alphas and betas distributions
		_alphas.resize(_numStates);
		_betas.resize(_numStates);
		_updateAlphaBeta();

		// prepare storage
		_logNormMeanPosState.zeros(_numRep, _size);
		_logOneMinusNormMeanPosState.zeros(_numRep, _size);

		_writeToEMFile();
	}

	void initialize(size_t NumStates, size_t Size,
					const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> *OtherLatentVariable) {
		initialize(NumStates, Size, OtherLatentVariable->numRep());

		// calculate mean posterior states
		if (_halfWindowSize > 0) {
			_fillMeanPosStatesSmooth(OtherLatentVariable);
		} else {
			_fillMeanPosStates(OtherLatentVariable);
		}
	}

	void initialize(size_t NumStates, size_t Size, size_t NumRep, std::string_view FileNameMeanPosStates) {
		initialize(NumStates, Size, NumRep);

		// fill mean posterior states from file
		_fillMeanPosStatesFromFile(FileNameMeanPosStates);
	}

	void report() { _report = true; }

	void reportToFile(std::string_view Filename) { _fileReportEM.open(Filename, {"phi", "kappa"}); }

	std::vector<PrecisionType> getParameters() const override { return {_phi, _kappa}; };

	bool setParameters(coretools::TConstView<PrecisionType> Params) override {
		if (!coretools::ZeroOneOpen::isInsideInterval(Params[0])) { return false; }              // phi: (0,1)
		if (!coretools::StrictlyPositive::isInsideInterval(Params[1]) || Params[1] > 100000.0) { // kappa: (0, 100000)
			return false;
		}

		if (!_fixPhi) { _phi = coretools::Probability(Params[0]); }
		if (!_fixKappa) { _kappa = Params[1]; }
		_updateAlphaBeta();

		return true;
	};

	// function to calculate emission probabilities
	void calculateEmissionProbabilities(LengthType Index, size_t Replicate,
										TDataVector<PrecisionType, NumStatesType> &Emission) const override {
		for (size_t j = 0; j < _numStates; ++j) {
			// take exp of log density: more stable gamma functions
			const double logDens_1 = _calcBetaConstant(j);
			const double logDens_2 = (_alphas[j] - 1.) * _logNormMeanPosState(Replicate, Index);
			const double logDens_3 = (_betas[j] - 1.) * _logOneMinusNormMeanPosState(Replicate, Index);
			const double logDens   = logDens_1 + logDens_2 + logDens_3;
			Emission[j]            = exp(logDens);
		}
	};

	void prepareEMParameterEstimationInitial() override {
		_sumGamma.resize(_numStates, 0.0);
		_sumGammaLogP.resize(_numStates, 0.0);
		_sumGammaLogOneMinusP.resize(_numStates, 0.0);
	};

	void prepareEMParameterEstimationOneIteration() override {
		std::fill(_sumGamma.begin(), _sumGamma.end(), 0.0);
		std::fill(_sumGammaLogP.begin(), _sumGammaLogP.end(), 0.0);
		std::fill(_sumGammaLogOneMinusP.begin(), _sumGammaLogOneMinusP.end(), 0.0);
	}

	void handleEMParameterEstimationOneIteration(LengthType Index, size_t Replicate,
												 const TDataVector<PrecisionType, NumStatesType> &Weights) override {
		// sum up gamma
		for (NumStatesType j = 0; j < _numStates; ++j) {
			_sumGamma[j] += Weights[j];
			_sumGammaLogP[j] += Weights[j] * _logNormMeanPosState(Replicate, Index);
			_sumGammaLogOneMinusP[j] += Weights[j] * _logOneMinusNormMeanPosState(Replicate, Index);
		}
	};

	void finalizeEMParameterEstimationOneIteration() override {
		// start Nelder-Mead with simplex of previous iteration
		auto ptr = &TLatentVariableBeta<PrecisionType, NumStatesType, LengthType>::_calcMinusQ;
		TNelderMead<2> nelderMead(*this, ptr);
		if (_iter == 0) {
			// set low tolerance and maxNumFunctionEvaluations, as this is the first iteration
			// and EM will change values in next iteration anyways
			nelderMead.setFractionalConvergenceTolerance(10e-5);
			nelderMead.setMaxNumFunctionEvaluations(1000);
		} else {
			nelderMead.dynamicallyAdjustTolerance(_simplexOfPreviousEMIteration[0].value(), 0.001, 100);
		}
		nelderMead.minimize(_simplexOfPreviousEMIteration);

		// store simplex to start next EM iteration with this
		_simplexOfPreviousEMIteration = nelderMead.getCurrentSimplex();

		// store final phi and kappa
		_setPhiKappa(nelderMead.coordinates());

		// report to file?
		_writeToEMFile();

		++_iter;
	};

	void finalizeEMParameterEstimationFinal() override {
		_sumGamma.clear();
		_sumGammaLogP.clear();
		_sumGammaLogOneMinusP.clear();
	};

	void setInitialPhi(double Phi) { _phi = coretools::P(Phi); }
	void setInitialKappa(double Kappa) { _kappa = Kappa; }
	void setHalfWindowSize(size_t HalfWindowSize) { _halfWindowSize = HalfWindowSize; }

	PrecisionType phi() const { return _phi; }
	PrecisionType kappa() const { return _kappa; }

	bool allEmissionsAreEqual() const {
		const auto first = _logNormMeanPosState(0, 0);
		for (size_t i = 0; i < _logNormMeanPosState.n_rows; ++i) {
			for (size_t j = 0; j < _logNormMeanPosState.n_cols; ++j) {
				if (std::fabs(_logNormMeanPosState(i, j) - first) > 1e-5) { return false; }
			}
		}
		return true;
	}

	void simulate(const std::vector<size_t> &Z, std::string_view Filename) {
		// simulate under model (Beta distribution)
		assert(Z.size() == _size);

		coretools::TOutputFile file(Filename);
		for (size_t r = 0; r < _numRep; ++r) {
			for (size_t i = 0; i < _size; ++i) {
				const auto z     = Z[i];
				const auto alpha = _alphas[z];
				const auto beta  = _betas[z];
				file << coretools::instances::randomGenerator().getBetaRandom(alpha, beta);
			}
			file << coretools::endl;
		}
	}
};

} // namespace stattools

#endif // TLATENTVARIABLEBETA_H
