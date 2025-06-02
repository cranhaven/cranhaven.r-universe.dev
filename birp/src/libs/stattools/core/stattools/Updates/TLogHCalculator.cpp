//
// Created by madleina on 21.12.22.
//

#include "TLogHCalculator.h"
#include "coretools/Math/TAccept.h"
#include "coretools/Distributions/TNormalDistr.h"

//--------------------------------------
// Functions
//--------------------------------------

bool evalLogH(double LogH) {
	if (!std::isfinite(LogH)) { return false; } // not finite -> always reject
	if (LogH >= 0.0) { return true; }           // Hastings ratio >= 1 -> log Hastings ratio >= 0 -> always accept
	return coretools::TAccept::accept(LogH);    // accept/reject based on random number
}

//--------------------------------------
// TLogHCalculatorRegular
//--------------------------------------
void TLogHCalculatorRegular::initialize(size_t /*NumLogHCalculators*/, double /*RelativeBlockSize*/,
										double /*LogProbReject*/, double /*LogProbAccept*/) {}
void TLogHCalculatorRegular::setSizeData(size_t N) { _N = N; }

void TLogHCalculatorRegular::startTrackingBlockLogH(size_t /*NumIterations*/) {}
void TLogHCalculatorRegular::startEvaluatingBlockLogH() {}

bool TLogHCalculatorRegular::acceptUpdate(double LogH) const { return evalLogH(LogH); }

//--------------------------------------
// TTotalLogH
//--------------------------------------

void TTotalLogH::reserve(size_t GuessNumIterations) { _logH.reserve(GuessNumIterations); }

void TTotalLogH::clear() {
	_logH.clear();
	_y  = 0.0;
	_y2 = 0.0;
	_n  = 0;
}

void TTotalLogH::add(double LogH) {
	_logH.emplace_back(LogH);
	_y += LogH;
	_y2 += LogH * LogH;
	++_n;
}

double TTotalLogH::operator[](size_t i) { return _logH[i]; }
double TTotalLogH::y() const { return _y; }
double TTotalLogH::y2() const { return _y2; }
size_t TTotalLogH::n() const { return _n; }

//--------------------------------------
// TLogHCorrelation
//--------------------------------------

double TLogHCorrelation::_getSummedBlockLogH(size_t Iteration, const std::vector<size_t> &BlockIxs) {
	// sum logH over all relevant blocks for that iteration
	double sum = 0.0;
	for (auto ix : BlockIxs) { sum += _blockLogH(Iteration, ix); }
	return sum;
}

std::tuple<double, double, double> TLogHCorrelation::_mergeBlocks(const std::vector<size_t> &BlockIxs) {
	double x  = 0.0;
	double xy = 0.0;
	double x2 = 0.0;
	for (size_t i = 0; i < _totalLogH.n(); i++) {
		double sum = _getSummedBlockLogH(i, BlockIxs);
		x += sum;
		xy += sum * _totalLogH[i];
		x2 += sum * sum;
	}
	return {x, xy, x2};
}

double TLogHCorrelation::_getNominator(double x, double xy) { return (double)_totalLogH.n() * xy - x * _totalLogH.y(); }
double TLogHCorrelation::_getXDenominator(double x, double x2) { return (double)_totalLogH.n() * x2 - x * x; }
double TLogHCorrelation::_getYDenominator() {
	return (double)_totalLogH.n() * _totalLogH.y2() - _totalLogH.y() * _totalLogH.y();
}

double TLogHCorrelation::_calculateR(const std::tuple<double, double, double> &X) {
	const auto [x, xy, x2] = X;

	double nom   = _getNominator(x, xy);
	double denom = sqrt(_getXDenominator(x, x2)) * sqrt(_getYDenominator());
	return nom / denom;
}

double TLogHCorrelation::_calculateSlope(const std::tuple<double, double, double> &X) {
	const auto [x, xy, x2] = X;
	return _getNominator(x, xy) / _getXDenominator(x, x2);
}

double TLogHCorrelation::_calculateIntercept(const std::tuple<double, double, double> &X, double slope) {
	const auto [x, xy, x2] = X;
	double inv             = 1.0 / (double)_totalLogH.n();
	const double y_bar     = inv * _totalLogH.y();
	const double x_bar     = inv * x;
	return y_bar - slope * x_bar;
}

double TLogHCorrelation::_calculateSigma(double Intercept, double Slope, const std::vector<size_t> &BlockIxs) {
	double sum = 0.0;
	for (size_t i = 0; i < _totalLogH.n(); i++) {
		const double v = _totalLogH[i] - Intercept - Slope * _getSummedBlockLogH(i, BlockIxs);
		sum += v * v;
	}
	const double frac = 1.0 / ((double)_totalLogH.n() - 2.0); // = 1 / (n - p) where p = 2 = slope and intercept
	return sqrt(frac * sum);
}

size_t TLogHCorrelation::getBlockIxWithHighestR() {
	size_t maxIx = 0;
	double maxR  = -10000;
	for (size_t b = 0; b < _numBlocks; ++b) {
		const double r = calculateR(b);
		if (r > maxR) {
			maxIx = b;
			maxR  = r;
		}
	}
	return maxIx;
}

void TLogHCorrelation::reserve(size_t GuessNumIterations, size_t NumBlocks) {
	_numBlocks = NumBlocks;
	_totalLogH.reserve(GuessNumIterations);
	_blockLogH.prepareFillData(GuessNumIterations, {NumBlocks});
}

void TLogHCorrelation::finalize() { _blockLogH.finalizeFillData(); }

void TLogHCorrelation::clear() {
	_blockLogH.clear();
	_totalLogH.clear();
}

void TLogHCorrelation::addTotalLogH(double LogH) { _totalLogH.add(LogH); }
void TLogHCorrelation::addBlockLogH(double LogH) { _blockLogH.emplace_back(LogH); }

double TLogHCorrelation::calculateR(size_t Block) { return _calculateR(_mergeBlocks({Block})); }
double TLogHCorrelation::calculateR(const std::vector<size_t> &Blocks) { return _calculateR(_mergeBlocks(Blocks)); }

std::vector<size_t> TLogHCorrelation::getBlockOrder() {
	// start with block that has highest correlation with totalLogH
	std::vector<size_t> blockOrder = {getBlockIxWithHighestR()};

	// successively add block such that merged block correlation is maximized
	while (blockOrder.size() != _numBlocks) {
		size_t maxIx = 0;
		double maxR  = -10000;
		for (size_t b = 0; b < _numBlocks; b++) {
			// skip if block is already used
			if (std::find(blockOrder.begin(), blockOrder.end(), b) != blockOrder.end()) { continue; }
			// else: temporarily add block to blockOrder and calculate correlation of merged blocks with totalLogH
			blockOrder.push_back(b);
			const double r = calculateR(blockOrder);
			blockOrder.pop_back();
			// check if better
			if (r > maxR) {
				maxIx = b;
				maxR  = r;
			}
		}
		// add block that maximizes the merged correlation
		blockOrder.push_back(maxIx);
	}

	return blockOrder;
}

std::tuple<double, double, double> TLogHCorrelation::fitLinearModel(const std::vector<size_t> &BlockIxs) {
	const auto x           = _mergeBlocks(BlockIxs);
	const double slope     = _calculateSlope(x);
	const double intercept = _calculateIntercept(x, slope);
	const double sigma     = _calculateSigma(intercept, slope, BlockIxs);
	return {intercept, slope, sigma};
}

//--------------------------------------
// TLogHCalculatorBlocks
//--------------------------------------

void TLogHCalculatorBlocks::_setBlockEnds(double RelativeBlockSize) {
	size_t blockSize = std::floor(RelativeBlockSize * (double)_N);

	_numBlocks       = _N / blockSize;
	size_t extraSize = _N % blockSize;
	_blockEnds.resize(_numBlocks);
	for (size_t i = 0; i < _numBlocks; i++) { _blockEnds[i] = (i + 1) * blockSize; }
	if (extraSize > 0) {
		_blockEnds.push_back(_N);
		++_numBlocks;
	}
}

void TLogHCalculatorBlocks::_fitLinearModelLogH() {
	_blockOrder = _correlation.getBlockOrder();
	_coeff.resize(std::array<size_t, 2>{_numBlocks, 3});

	for (size_t b = 0; b < _numBlocks; b++) {
		const auto end                       = _blockOrder.begin() + (int)b + 1;
		std::vector<size_t> blocks           = std::vector<size_t>(_blockOrder.begin(), end);
		const auto [intercept, slope, sigma] = _correlation.fitLinearModel(blocks);
		_coeff(b, 0)                         = intercept;
		_coeff(b, 1)                         = slope;
		_coeff(b, 2)                         = sigma;
	}
}

void TLogHCalculatorBlocks::_clearLogH() { _correlation.clear(); }

std::tuple<size_t, size_t, double> TLogHCalculatorBlocks::_startBlock(size_t BlockIndex, double LogPriorRatio) const {
	// Note: end is not included!
	const size_t end   = _blockEnds[BlockIndex];
	const size_t start = (BlockIndex == 0) ? 0 : _blockEnds[BlockIndex - 1];
	const size_t len   = end - start;

	double blockLogH = (double)len / (double)_N * LogPriorRatio;
	return {start, end, blockLogH};
}

double TLogHCalculatorBlocks::_calculateLogProbPredLargerR(double r, double Pred_totalLogH, double Sigma) {
	// = pnorm(r, Pred_totalLogH, Sigma, lower.tail = FALSE, log.p = TRUE)
	return log(1.0 - coretools::probdist::TNormalDistr::cumulativeDensity(r, Pred_totalLogH, Sigma));
}

void TLogHCalculatorBlocks::initialize(double RelativeBlockSize, double LogProbReject, double LogProbAccept) {
	_log_prob_reject   = LogProbReject;
	_log_prob_accept   = LogProbAccept;
	_relativeBlockSize = RelativeBlockSize;
}

void TLogHCalculatorBlocks::setSizeData(size_t N) {
	_N = N;

	// set block ends
	_setBlockEnds(_relativeBlockSize);

	// set block order at beginning: 0, 1, ..., numBlocks - 1
	_blockOrder.resize(_blockEnds.size());
	std::iota(std::begin(_blockOrder), std::end(_blockOrder), 0);
}

void TLogHCalculatorBlocks::startTrackingBlockLogH(size_t NumIterations) {
	_trackBlockLogH = true;
	_correlation.reserve(NumIterations, _numBlocks);
}

void TLogHCalculatorBlocks::startEvaluatingBlockLogH() {
	// make linear models
	_correlation.finalize();
	_fitLinearModelLogH();
	_clearLogH();

	// stop tracking, start calculating in blocks
	_trackBlockLogH    = false;
	_evaluateBlockLogH = true;
}

//--------------------------------------
// TMultiLogHCalculatorBlocks
//--------------------------------------

void TMultiLogHCalculatorBlocks::initialize(size_t NumLogHCalculators, double RelativeBlockSize, double LogProbReject,
											double LogProbAccept) {
	_shared = (NumLogHCalculators == 1);
	_logHCalculators.resize(NumLogHCalculators);
	for (auto &it : _logHCalculators) { it.initialize(RelativeBlockSize, LogProbReject, LogProbAccept); }
}

void TMultiLogHCalculatorBlocks::setSizeData(size_t N) {
	for (auto &it : _logHCalculators) { it.setSizeData(N); }
}

void TMultiLogHCalculatorBlocks::startTrackingBlockLogH(size_t NumIterations) {
	for (auto &it : _logHCalculators) { it.startTrackingBlockLogH(NumIterations); }
}

void TMultiLogHCalculatorBlocks::startEvaluatingBlockLogH() {
	for (auto &it : _logHCalculators) { it.startEvaluatingBlockLogH(); }
}

bool TMultiLogHCalculatorBlocks::acceptUpdate(double LogH) const { return evalLogH(LogH); }

bool TMultiLogHCalculatorBlocks::isShared() const { return _shared; }
