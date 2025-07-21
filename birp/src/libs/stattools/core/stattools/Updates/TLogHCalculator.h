//
// Created by caduffm on 12/19/22.
//

#ifndef TLOGHCALCULATOR_H
#define TLOGHCALCULATOR_H

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Math/TSumLog.h"
#include "coretools/Storage/TStorage.h"
#include <vector>

//--------------------------------------
// Functions
//--------------------------------------
bool evalLogH(double LogH);

namespace impl {

template<bool FuncCalculatesLogH, class Object, class Function>
double sumLogH(size_t Start, size_t End, Object &Obj, Function &FuncCalcHastingsRatio,
			   const coretools::TRange &IxParameters, double PriorLogH) {
	if constexpr (FuncCalculatesLogH) {
		// already in log -> just add
		double sum = 0.0;
		for (size_t i = Start; i < End; ++i) { sum += (Obj.*FuncCalcHastingsRatio)(i, IxParameters); }
		return sum + PriorLogH;
	} else {
		// not in log -> use coretools::TSumLog with dynamic binning (N = 0)
		coretools::TSumLog<0> sum;
		for (size_t i = Start; i < End; ++i) { sum.add((Obj.*FuncCalcHastingsRatio)(i, IxParameters)); }
		return sum.getSum() + PriorLogH;
	}
}

template<bool FuncCalculatesLogH, class Object, class Function>
bool evaluateRegular(size_t N, Object &Obj, Function &FuncCalcHastingsRatio, const coretools::TRange &IxParameters,
					 double LogPriorRatio) {
	const double logH = sumLogH<FuncCalculatesLogH>(0, N, Obj, FuncCalcHastingsRatio, IxParameters, LogPriorRatio);
	return evalLogH(logH);
}

} // namespace impl

//--------------------------------------
// TLogHCalculatorRegular
//--------------------------------------

class TLogHCalculatorRegular {
	// evaluate logH based on all elements, no blocks
private:
	// total size
	size_t _N;

public:
	TLogHCalculatorRegular()  = default;
	~TLogHCalculatorRegular() = default;

	void initialize(size_t /*NumLogHCalculators*/, double /*RelativeBlockSize*/, double /*LogProbReject*/,
					double /*LogProbAccept*/);
	void setSizeData(size_t N);

	void startTrackingBlockLogH(size_t /*NumIterations*/);
	void startEvaluatingBlockLogH();

	template<bool FuncCalculatesLogH, class Object, class Function>
	bool acceptUpdate(Object &Obj, Function &FuncCalcHastingsRatio, const coretools::TRange &IxParameters,
					  double LogPriorRatio) {
		return impl::evaluateRegular<FuncCalculatesLogH>(_N, Obj, FuncCalcHastingsRatio, IxParameters, LogPriorRatio);
	}
	bool acceptUpdate(double LogH) const;
};

//--------------------------------------
// TTotalLogH
//--------------------------------------

class TTotalLogH {
private:
	// store logH (for all iterations) and counters
	std::vector<double> _logH;
	double _y  = 0.0;
	double _y2 = 0.0;
	size_t _n  = 0;

public:
	void reserve(size_t GuessNumIterations);

	void clear();

	void add(double LogH);

	double operator[](size_t i);
	double y() const;
	double y2() const;
	size_t n() const;
};

//--------------------------------------
// TLogHCorrelation
//--------------------------------------

class TLogHCorrelation {
private:
	size_t _numBlocks = 0;

	TTotalLogH _totalLogH;
	coretools::TMultiDimensionalStorage<double, 2> _blockLogH;

	double _getSummedBlockLogH(size_t Iteration, const std::vector<size_t> &BlockIxs);

	std::tuple<double, double, double> _mergeBlocks(const std::vector<size_t> &BlockIxs);

	double _getNominator(double x, double xy);
	double _getXDenominator(double x, double x2);
	double _getYDenominator();

	double _calculateR(const std::tuple<double, double, double> &X);

	double _calculateSlope(const std::tuple<double, double, double> &X);
	double _calculateIntercept(const std::tuple<double, double, double> &X, double slope);

	double _calculateSigma(double Intercept, double Slope, const std::vector<size_t> &BlockIxs);

public:
	void reserve(size_t GuessNumIterations, size_t NumBlocks);
	void finalize();
	void clear();
	void addTotalLogH(double LogH);
	void addBlockLogH(double LogH);

	double calculateR(size_t Block);
	double calculateR(const std::vector<size_t> &Blocks);
	size_t getBlockIxWithHighestR();

	std::vector<size_t> getBlockOrder();

	std::tuple<double, double, double> fitLinearModel(const std::vector<size_t> &BlockIxs);
};

//--------------------------------------
// TLogHCalculatorBlocks
//--------------------------------------

class TLogHCalculatorBlocks {
protected:
	// total size and number of blocks
	size_t _N                 = 0;
	size_t _numBlocks         = 0;
	double _relativeBlockSize = 0.0;

	// store blockEnds and blockOrder
	std::vector<size_t> _blockEnds;
	std::vector<size_t> _blockOrder;

	// track correlation?
	bool _trackBlockLogH = false;

	// use blocks?
	bool _evaluateBlockLogH = false;

	// store logH (only temporarily)
	TLogHCorrelation _correlation;

	// store coefficients of linear model
	coretools::TMultiDimensionalStorage<double, 2> _coeff;

	// probabilities for early reject/accept
	double _log_prob_reject = 0.0;
	double _log_prob_accept = 0.0;

	void _setBlockEnds(double RelativeBlockSize);

	void _fitLinearModelLogH();

	void _clearLogH();

	std::tuple<size_t, size_t, double> _startBlock(size_t BlockIndex, double LogPriorRatio) const;

	template<bool FuncCalculatesLogH, class Object, class Function>
	bool _calculateLogHRegular_trackBlockLogH(Object &Obj, Function &FuncCalcHastingsRatio,
											  const coretools::TRange &IxParameters, double LogPriorRatio) {
		// calculate logH based on all elements, but keep track of block logH
		double totalLogH = 0.0;
		for (size_t b = 0; b < _numBlocks; ++b) {
			const auto [start, end, priorLogH] = _startBlock(b, LogPriorRatio);

			const double blockLogH =
				impl::sumLogH<FuncCalculatesLogH>(start, end, Obj, FuncCalcHastingsRatio, IxParameters, priorLogH);

			_correlation.addBlockLogH(blockLogH);
			totalLogH += blockLogH;
		}
		_correlation.addTotalLogH(totalLogH);

		return evalLogH(totalLogH);
	}

	static double _calculateLogProbPredLargerR(double r, double Pred_totalLogH, double Sigma) ;

	template<bool FuncCalculatesLogH, class Object, class Function>
	bool _calculateLogHInBlocks(Object &Obj, Function &FuncCalcHastingsRatio, const coretools::TRange &IxParameters,
								double LogPriorRatio, double r) const {
		// calculate logH successively per block and stop if prediction is good
		double logH = 0.0;
		for (size_t b = 0; b < _numBlocks; ++b) {
			const size_t block           = _blockOrder[b];
			auto [start, end, priorLogH] = _startBlock(block, LogPriorRatio);
			logH += impl::sumLogH<FuncCalculatesLogH>(start, end, Obj, FuncCalcHastingsRatio, IxParameters, priorLogH);

			// no need to do prediction for last block
			if (b == _numBlocks - 1) { break; }
			// predicted totalLogH = intercept + slope * logH
			const double pred_totalLogH            = _coeff(b, 0) + _coeff(b, 1) * logH;
			const double log_P_prediction_larger_r = _calculateLogProbPredLargerR(r, pred_totalLogH, _coeff(b, 2));
			if (log_P_prediction_larger_r < _log_prob_reject) { // super bad
				return false;
			} else if (log_P_prediction_larger_r > _log_prob_accept) { // super good
				return true;
			}
		}
		return evalLogH(logH);
	}

	template<bool FuncCalculatesLogH, class Object, class Function>
	bool _calculateLogHInBlocks(Object &Obj, Function &FuncCalcHastingsRatio, const coretools::TRange &IxParameters,
								double LogPriorRatio) const {
		// calculate logH successively per block and stop if prediction is good
		const double r = log(coretools::instances::randomGenerator().getRand());
		return _calculateLogHInBlocks<FuncCalculatesLogH>(Obj, FuncCalcHastingsRatio, IxParameters, LogPriorRatio, r);
	}

public:
	TLogHCalculatorBlocks()  = default;
	~TLogHCalculatorBlocks() = default;

	void initialize(double RelativeBlockSize, double LogProbReject, double LogProbAccept);
	void setSizeData(size_t N);

	void startTrackingBlockLogH(size_t NumIterations);
	void startEvaluatingBlockLogH();

	template<bool FuncCalculatesLogH, class Object, class Function>
	bool acceptUpdate(Object &Obj, Function &FuncCalcHastingsRatio, const coretools::TRange &IxParameters,
					  double LogPriorRatio) {
		if (_evaluateBlockLogH) {
			return _calculateLogHInBlocks<FuncCalculatesLogH>(Obj, FuncCalcHastingsRatio, IxParameters, LogPriorRatio);
		} else if (_trackBlockLogH) {
			return _calculateLogHRegular_trackBlockLogH<FuncCalculatesLogH>(Obj, FuncCalcHastingsRatio, IxParameters,
																			LogPriorRatio);
		} else {
			return impl::evaluateRegular<FuncCalculatesLogH>(_N, Obj, FuncCalcHastingsRatio, IxParameters,
															 LogPriorRatio);
		}
	}
};

//--------------------------------------
// TMultiLogHCalculatorBlocks
//--------------------------------------

class TMultiLogHCalculatorBlocks {
	// simple wrapper around TLogHCalculator
	// manages multiple instances of TLogHCalculator in case of shared/unique proposal kernels
private:
	std::vector<TLogHCalculatorBlocks> _logHCalculators;

	bool _shared = false;

public:
	TMultiLogHCalculatorBlocks() = default;
	~TMultiLogHCalculatorBlocks() = default;

	void initialize(size_t NumLogHCalculators, double RelativeBlockSize, double LogProbReject, double LogProbAccept);

	void setSizeData(size_t N);
	void startTrackingBlockLogH(size_t NumIterations);
	void startEvaluatingBlockLogH();

	template<bool FuncCalculatesLogH, class Object, class Function>
	bool acceptUpdate(Object &Obj, Function &FuncCalcHastingsRatio, const coretools::TRange &IxParameters,
					  double LogPriorRatio) {
		if (_shared) {
			return _logHCalculators[0].acceptUpdate<FuncCalculatesLogH>(Obj, FuncCalcHastingsRatio, IxParameters,
																		LogPriorRatio);
		}

		// use index of first parameter that is being updated
		// --> e.g. update pair -> logH prediction is based on first one
		const size_t i = IxParameters.begin;
		assert(i < _logHCalculators.size());
		return _logHCalculators[i].acceptUpdate<FuncCalculatesLogH>(Obj, FuncCalcHastingsRatio, IxParameters,
																	LogPriorRatio);
	}

	bool acceptUpdate(double LogH) const;
	bool isShared() const;
};

#endif // TLOGHCALCULATOR_H
