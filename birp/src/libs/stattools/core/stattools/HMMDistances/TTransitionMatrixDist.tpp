#include "coretools/Math/mathFunctions.h"
namespace stattools {

//-------------------------------------------
// TTransitionMatrixDistBase
//-------------------------------------------
template<typename PrecisionType, typename NumStatesType>
TTransitionMatrixDistBase<PrecisionType, NumStatesType>::TTransitionMatrixDistBase() {
	_transitionMatrix.zeros(0, 0);
}

template<typename PrecisionType, typename NumStatesType>
TTransitionMatrixDistBase<PrecisionType, NumStatesType>::TTransitionMatrixDistBase(NumStatesType NumStates) {
	resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixDistBase<PrecisionType, NumStatesType>::resize(NumStatesType NumStates) {
	_transitionMatrix.zeros(NumStates, NumStates);
	_stationary.resize(NumStates, 0.);
}

template<typename PrecisionType, typename NumStatesType>
PrecisionType TTransitionMatrixDistBase<PrecisionType, NumStatesType>::operator()(NumStatesType From,
																				  NumStatesType To) const {
	return _transitionMatrix(From, To);
}

template<typename PrecisionType, typename NumStatesType>
PrecisionType TTransitionMatrixDistBase<PrecisionType, NumStatesType>::stationary(NumStatesType State) const {
	return _stationary[State];
}

template<typename PrecisionType, typename NumStatesType>
const arma::mat &TTransitionMatrixDistBase<PrecisionType, NumStatesType>::getTransitionMatrix() const {
	return _transitionMatrix;
}

template<typename PrecisionType, typename NumStatesType>
arma::mat TTransitionMatrixDistBase<PrecisionType, NumStatesType>::getStationaryMatrix() const {
	// fill vector (stationary) into matrix (all rows are the same)

	arma::mat mat(_stationary.size(), _stationary.size());
	for (size_t i = 0; i < _stationary.size(); i++) {
		for (size_t j = 0; j < _stationary.size(); j++) { mat(i, j) = _stationary[j]; }
	}
	return mat;
}

template<typename PrecisionType, typename NumStatesType>
template<typename TypeGenerMat>
bool TTransitionMatrixDistBase<PrecisionType, NumStatesType>::_fillTransitionMatrixAsExponential(
	coretools::TConstView<PrecisionType> Values, TypeGenerMat &GeneratingMatrix) {
	// fill generating matrix
	GeneratingMatrix.fillGeneratingMatrix(Values);

	// now take matrix exponential
	bool ok = arma::expmat(_transitionMatrix, GeneratingMatrix.getGeneratingMatrix());
	return ok;
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixDistBase<PrecisionType, NumStatesType>::fillStationary(coretools::TConstView<PrecisionType>) {
	// default: transition matrix does not know how to calculate stationary distribution of Markov Chain
	// -> can be calculated with normal equations from transition matrix
	// fill armadillo matrix with t(I - Q(1))
	// where Q(1) is the transition matrix for distanceGroup 1
	auto numStates = _stationary.size();
	arma::mat A(numStates + 1, numStates);
	for (NumStatesType i = 0; i < numStates; i++) {
		for (NumStatesType j = 0; j < numStates; j++) {
			if (i == j) {
				A(i, j) = 1. - _transitionMatrix(j, i);
			} else {
				A(i, j) = -_transitionMatrix(j, i);
			}
		}
	}
	// fill last row with 1's
	for (NumStatesType j = 0; j < numStates; j++) { A(numStates, j) = 1.; }

	// construct column vector of size _numStates filled with zeros, and add an extra row with a 1
	arma::mat b(numStates + 1, 1, arma::fill::zeros);
	b(numStates, 0) = 1.;

	// solve for pi (the stationary distribution)
	arma::vec pi = arma::solve(A.t() * A, A.t() * b);

	// fill pi into stationary
	for (NumStatesType i = 0; i < numStates; i++) { _stationary[i] = pi[i]; }

	return true;
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixDistBase<PrecisionType, NumStatesType>::fillStationaryFromStartValues() {
	fillStationary(getStartValues());
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType> TTransitionMatrixDistBase<PrecisionType, NumStatesType>::transformToNelderMeadSpace(
	coretools::TConstView<PrecisionType> Values) {
	// default: values do not need to be transformed
	// idea: there might be certain restrictions on the parameters that Nelder-Mead should optimize,
	//        e.g. they must be positive.
	//        -> transform them (e.g. with log), and let Nelder-Mead operate on that transformed space,
	//           such that values are always valid after back-transformation
	return std::vector<PrecisionType>(Values.begin(), Values.end());
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType> TTransitionMatrixDistBase<PrecisionType, NumStatesType>::transformFromNelderMeadSpace(
	coretools::TConstView<PrecisionType> Values) {
	// default: values do not need to be transformed
	// idea: there might be certain restrictions on the parameters that Nelder-Mead should optimize,
	//        e.g. they must be positive.
	//        -> transform them (e.g. with log), and let Nelder-Mead operate on that transformed space,
	//           such that values are always valid after back-transformation
	return std::vector<PrecisionType>(Values.begin(), Values.end());
}

template<typename PrecisionType, typename NumStatesType>
const arma::mat &TTransitionMatrixDistBase<PrecisionType, NumStatesType>::getUnparametrizedGeneratingMatrix() const {
	// needed for Newton-Raphson
	// overridden in derived classes if there is indeed an unparametrized generating matrix available
	DEVERROR("Class TTransitionMatrixDistBase does not manage a generating matrix!");
}

template<typename PrecisionType, typename NumStatesType>
NumStatesType TTransitionMatrixDistBase<PrecisionType, NumStatesType>::numStates() const {
	return _transitionMatrix.n_rows;
}

//-------------------------------------------
// TTransitionMatrixBool
//-------------------------------------------

template<typename PrecisionType, typename NumStatesType>
TTransitionMatrixBool<PrecisionType, NumStatesType>::TTransitionMatrixBool()
	: TTransitionMatrixDistBase<PrecisionType, NumStatesType>() {
	// transition matrix for bools always has 2 states
	resize(2);
}

template<typename PrecisionType, typename NumStatesType>
TTransitionMatrixBool<PrecisionType, NumStatesType>::TTransitionMatrixBool(NumStatesType NumStates)
	: TTransitionMatrixDistBase<PrecisionType, NumStatesType>() {
	resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixBool<PrecisionType, NumStatesType>::resize(NumStatesType NumStates) {
	if (NumStates != 2) { DEVERROR("Transition matrix for booleans can only have 2 states (not ", NumStates, ")!"); }
	TTransitionMatrixDistBase<PrecisionType, NumStatesType>::resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixBool<PrecisionType, NumStatesType>::_isValid(coretools::TConstView<PrecisionType> Values) const {
	// pi: [0,1], gamma: [0, inf)
	return coretools::ZeroOneClosed::isInsideInterval(Values[0]) && coretools::Positive::isInsideInterval(Values[1]);
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixBool<PrecisionType, NumStatesType>::fillTransitionMatrix(
	coretools::TConstView<PrecisionType> Values) {
	assert(Values.size() == 2);
	if (!_isValid(Values)) { return false; }

	auto pi    = Values[0];
	auto gamma = Values[1];

	// fill transition matrix
	double denominator = 1. + gamma;
	double p1          = pi / denominator;
	double p2          = (1. - pi) / denominator;

	_transitionMatrix(0, 0) = 1. - p1;
	_transitionMatrix(0, 1) = p1;
	_transitionMatrix(1, 0) = p2;
	_transitionMatrix(1, 1) = 1. - p2;

	return true;
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixBool<PrecisionType, NumStatesType>::fillTransitionMatrixFromStartValues() {
	fillTransitionMatrix(getStartValues());
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixBool<PrecisionType, NumStatesType>::fillStationary(coretools::TConstView<PrecisionType> Values) {
	// stationary distribution of Markov Chain corresponds to pi
	if (!_isValid(Values)) { return false; }

	const auto pi        = Values[0];
	this->_stationary[0] = 1. - pi;
	this->_stationary[1] = pi;
	return true;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType> TTransitionMatrixBool<PrecisionType, NumStatesType>::getStartValues() const {
	// pi and gamma: choose some more or less sensible starting values (used to start initialization of EM)
	return {0.01, 0.01};
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixBool<PrecisionType, NumStatesType>::optimizeWithNelderMead() const {
	return true;
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixBool<PrecisionType, NumStatesType>::optimizeWithLineSearch() const {
	return false;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType> TTransitionMatrixBool<PrecisionType, NumStatesType>::transformToNelderMeadSpace(
	coretools::TConstView<PrecisionType> Values) {
	// normal space to Nelder-Mead space
	// pi must be [0,1] -> take logit to transform it to [-inf,inf] space where Nelder-Mead can operate on
	// gamma must be [0, inf] -> take log to transform it to [-inf,inf] space where Nelder-Mead can operate on
	assert(Values.size() == 2);
	std::vector<PrecisionType> result(Values.begin(), Values.end());
	result[0] = coretools::logit(Values[0]);
	result[1] = log(Values[1]);
	return result;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType> TTransitionMatrixBool<PrecisionType, NumStatesType>::transformFromNelderMeadSpace(
	coretools::TConstView<PrecisionType> Values) {
	// Nelder-Mead space to normal space
	// pi must be [0,1] -> take logistic to transform back from [-inf,inf] space where Nelder-Mead has operated on
	// gamma must be [0, inf] -> take exp to transform back from [-inf,inf] space where Nelder-Mead has operated on
	assert(Values.size() == 2);
	std::vector<PrecisionType> result(Values.begin(), Values.end());
	result[0] = coretools::logistic(Values[0]);
	result[1] = exp(Values[1]);
	return result;
}

template<typename PrecisionType, typename NumStatesType>
size_t TTransitionMatrixBool<PrecisionType, NumStatesType>::numParameters() const {
	return 2; // pi and gamma
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixBool<PrecisionType, NumStatesType>::fillHeaderEMReportFile(
	std::vector<std::string> &Header) const {
	Header.emplace_back("pi");
	Header.emplace_back("gamma");
}

//-------------------------------------------
// TTransitionMatrixCategorical
//-------------------------------------------

template<typename PrecisionType, typename NumStatesType>
TTransitionMatrixCategorical<PrecisionType, NumStatesType>::TTransitionMatrixCategorical()
	: TTransitionMatrixDistBase<PrecisionType, NumStatesType>() {}

template<typename PrecisionType, typename NumStatesType>
TTransitionMatrixCategorical<PrecisionType, NumStatesType>::TTransitionMatrixCategorical(NumStatesType NumStates)
	: TTransitionMatrixDistBase<PrecisionType, NumStatesType>() {
	// NumStates = _D + 1 since there is one neutral state plus D non-neutral states
	resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixCategorical<PrecisionType, NumStatesType>::resize(NumStatesType NumStates) {
	if (NumStates < 2) { DEVERROR("Transition matrix must have at least two 2 states (not ", NumStates, ")!"); }
	_D = NumStates - 1;
	TTransitionMatrixDistBase<PrecisionType, NumStatesType>::resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
size_t TTransitionMatrixCategorical<PrecisionType, NumStatesType>::D() const {
	return _D;
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixCategorical<PrecisionType, NumStatesType>::_isValid(
	coretools::TConstView<PrecisionType> Values) const {

	// phi: [0,1], kappa: [0, inf)
	if (!coretools::ZeroOneClosed::isInsideInterval(Values[0]) || !coretools::Positive::isInsideInterval(Values[1])) {
		return false;
	}
	// rhos: > 0
	for (size_t d = 2; d < Values.size(); d++) {
		if (Values[d] < 0.0) { return false; }
	}
	return true;
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixCategorical<PrecisionType, NumStatesType>::fillTransitionMatrix(
	coretools::TConstView<PrecisionType> Values) {
	assert(Values.size() == 2 + _D);
	if (!_isValid(Values)) { return false; }

	PrecisionType pi    = Values[0];
	PrecisionType gamma = Values[1];

	// fill transition matrix
	const double oneMinPi       = 1.0 - pi;
	const double invDenominator = 1. / (1. + gamma);
	const double p_0_0          = invDenominator * (oneMinPi + gamma);
	const double p_d_0          = invDenominator * oneMinPi;
	const double p_0_d          = invDenominator * pi;
	const double p_d_d          = invDenominator * (pi + gamma);

	_transitionMatrix(0, 0) = p_0_0;
	for (size_t d = 0; d < _D; d++) {
		// first row
		_transitionMatrix(0, d + 1)     = p_0_d * Values[2 + d]; // multiply with rho_d
		// first col
		_transitionMatrix(d + 1, 0)     = p_d_0;
		// diagonal
		_transitionMatrix(d + 1, d + 1) = p_d_d;
	}

	return true;
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixCategorical<PrecisionType, NumStatesType>::fillTransitionMatrixFromStartValues() {
	fillTransitionMatrix(getStartValues());
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixCategorical<PrecisionType, NumStatesType>::fillStationary(
	coretools::TConstView<PrecisionType> Values) {
	if (!_isValid(Values)) { return false; }

	const auto pi        = Values[0];
	this->_stationary[0] = 1. - pi;
	for (size_t d = 0; d < _D; d++) {
		this->_stationary[d + 1] = pi * Values[2 + d]; // pi * rho_d
	}
	return true;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType> TTransitionMatrixCategorical<PrecisionType, NumStatesType>::getStartValues() const {
	// pi, gamma and rhos: choose some more or less sensible starting values (used to start initialization of EM)
	std::vector<PrecisionType> vals(2 + _D, 0.01);             // pi and gamma = 0.01
	for (size_t d = 0; d < _D; d++) { vals[d + 2] = 1. / _D; } // rhos
	return vals;
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixCategorical<PrecisionType, NumStatesType>::optimizeWithNelderMead() const {
	return true;
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixCategorical<PrecisionType, NumStatesType>::optimizeWithLineSearch() const {
	return false;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType> TTransitionMatrixCategorical<PrecisionType, NumStatesType>::transformToNelderMeadSpace(
	coretools::TConstView<PrecisionType> Values) {
	// normal space to Nelder-Mead space
	// pi must be [0,1] -> take logit to transform it to [-inf,inf] space where Nelder-Mead can operate on
	// gamma must be [0, inf] -> take log to transform it to [-inf,inf] space where Nelder-Mead can operate on
	// rho_d_hat must be [0, inf] -> take log to transform it to [-inf,inf] space where Nelder-Mead can operate on
	assert(Values.size() == _D + 2);
	std::vector<PrecisionType> result(_D + 2);
	result[0] = coretools::logit(Values[0]);
	result[1] = log(Values[1]);
	for (size_t d = 0; d < _D; d++) { result[d + 2] = log(Values[d + 2]); }
	return result;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType> TTransitionMatrixCategorical<PrecisionType, NumStatesType>::transformFromNelderMeadSpace(
	coretools::TConstView<PrecisionType> Values) {
	// Nelder-Mead space to normal space
	// pi must be [0,1] -> take logistic to transform back from [-inf,inf] space where Nelder-Mead has operated on
	// gamma must be [0, inf] -> take exp to transform back from [-inf,inf] space where Nelder-Mead has operated on
	// rho_d_hat must be [0,inf] -> take exp to transform  back from [-inf,inf] space where Nelder-Mead has operated on
	// then: divide each rho_d_hat by the sum over all rho_d_hat to get normalized rho_d
	assert(Values.size() == _D + 2);
	std::vector<PrecisionType> result(_D + 2);
	result[0] = coretools::logistic(Values[0]);
	result[1] = exp(Values[1]);

	double sum = 0.0;
	for (size_t d = 0; d < _D; d++) {
		const double rho_d_hat = exp(Values[d + 2]);
		result[d + 2]          = rho_d_hat;
		sum += rho_d_hat;
	}

	// normalize rho's
	std::transform(result.begin() + 2, result.end(), result.begin() + 2, [sum](auto v) { return v / sum; });
	return result;
}

template<typename PrecisionType, typename NumStatesType>
size_t TTransitionMatrixCategorical<PrecisionType, NumStatesType>::numParameters() const {
	return 2 + _D; // pi and gamma and D-times a rho
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixCategorical<PrecisionType, NumStatesType>::fillHeaderEMReportFile(
	std::vector<std::string> &Header) const {
	Header.emplace_back("pi");
	Header.emplace_back("gamma");
	for (size_t d = 0; d < _D; d++) { Header.emplace_back("rho_", d + 1); }
}

//-------------------------------------------
// TTransitionMatrixBoolGeneratingMatrix
//-------------------------------------------

template<typename PrecisionType, typename NumStatesType>
TTransitionMatrixBoolGeneratingMatrix<PrecisionType, NumStatesType>::TTransitionMatrixBoolGeneratingMatrix()
	: TTransitionMatrixDistBase<PrecisionType, NumStatesType>() {
	// transition matrix for bools always has 2 states
	resize(2);
}

template<typename PrecisionType, typename NumStatesType>
TTransitionMatrixBoolGeneratingMatrix<PrecisionType, NumStatesType>::TTransitionMatrixBoolGeneratingMatrix(
	NumStatesType NumStates)
	: TTransitionMatrixDistBase<PrecisionType, NumStatesType>() {
	resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixBoolGeneratingMatrix<PrecisionType, NumStatesType>::resize(NumStatesType NumStates) {
	if (NumStates != 2) { DEVERROR("Transition matrix for booleans can only have 2 states (not ", NumStates, ")!"); }
	TTransitionMatrixDistBase<PrecisionType, NumStatesType>::resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixBoolGeneratingMatrix<PrecisionType, NumStatesType>::_isValid(
	coretools::TConstView<PrecisionType> Values) const {
	return coretools::Positive::isInsideInterval(Values[0]) && coretools::Positive::isInsideInterval(Values[1]);
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixBoolGeneratingMatrix<PrecisionType, NumStatesType>::fillTransitionMatrix(
	coretools::TConstView<PrecisionType> Values) {
	if (!_isValid(Values)) { return false; }

	return this->_fillTransitionMatrixAsExponential(Values, _generatingMatrix);
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixBoolGeneratingMatrix<PrecisionType, NumStatesType>::fillTransitionMatrixFromStartValues() {
	this->_fillTransitionMatrixAsExponential(getStartValues(), _generatingMatrix);
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixBoolGeneratingMatrix<PrecisionType, NumStatesType>::fillStationary(
	coretools::TConstView<PrecisionType> Values) {
	if (!_isValid(Values)) { return false; }

	this->_stationary = _generatingMatrix.fillStationary(Values);

	return true;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType> TTransitionMatrixBoolGeneratingMatrix<PrecisionType, NumStatesType>::getStartValues() const {
	// Lambda0 and Lambda1: choose some more or less sensible starting values (used to start initialization of EM)
	return {1e-05, 1e-05};
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixBoolGeneratingMatrix<PrecisionType, NumStatesType>::optimizeWithNelderMead() const {
	return true;
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixBoolGeneratingMatrix<PrecisionType, NumStatesType>::optimizeWithLineSearch() const {
	return false;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType>
TTransitionMatrixBoolGeneratingMatrix<PrecisionType, NumStatesType>::transformToNelderMeadSpace(
	coretools::TConstView<PrecisionType> Values) {
	// normal space to Nelder-Mead space
	// Lambda1 and Lambda2 must both be [0,inf] -> take log to transform it to [-inf,inf] space where Nelder-Mead
	// can operate on
	assert(Values.size() == 2);
	std::vector<PrecisionType> result{Values.begin(), Values.end()};
	result[0] = log(Values[0]);
	result[1] = log(Values[1]);
	return result;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType>
TTransitionMatrixBoolGeneratingMatrix<PrecisionType, NumStatesType>::transformFromNelderMeadSpace(
	coretools::TConstView<PrecisionType> Values) {
	// Nelder-Mead space to normal space
	// Lambda1 and Lambda2 must both be [0,inf] -> take exp to transform back from [-inf,inf] space where
	// Nelder-Mead has operated on
	assert(Values.size() == 2);
	std::vector<PrecisionType> result{Values.begin(), Values.end()};
	result[0] = exp(Values[0]);
	result[1] = exp(Values[1]);
	return result;
}

template<typename PrecisionType, typename NumStatesType>
size_t TTransitionMatrixBoolGeneratingMatrix<PrecisionType, NumStatesType>::numParameters() const {
	return _generatingMatrix.numParameters();
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixBoolGeneratingMatrix<PrecisionType, NumStatesType>::fillHeaderEMReportFile(
	std::vector<std::string> &Header) const {
	Header.emplace_back("Lambda_1");
	Header.emplace_back("Lambda_2");
}

//-------------------------------------------
// TTransitionMatrixLadder
//-------------------------------------------

template<typename PrecisionType, typename NumStatesType>
TTransitionMatrixLadder<PrecisionType, NumStatesType>::TTransitionMatrixLadder()
	: TTransitionMatrixDistBase<PrecisionType, NumStatesType>() {}

template<typename PrecisionType, typename NumStatesType>
TTransitionMatrixLadder<PrecisionType, NumStatesType>::TTransitionMatrixLadder(NumStatesType NumStates) {
	resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixLadder<PrecisionType, NumStatesType>::resize(NumStatesType NumStates) {
	if (NumStates < 2) {
		DEVERROR("Transition matrix for "
				 "ladder-type transition matrix must have at least 2 states (not ",
				 NumStates, ")!");
	}
	TTransitionMatrixDistBase<PrecisionType, NumStatesType>::resize(NumStates);
	_generatingMatrix.resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixLadder<PrecisionType, NumStatesType>::_isValid(
	coretools::TConstView<PrecisionType> Values) const {
	return coretools::Positive::isInsideInterval(Values[0]);
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixLadder<PrecisionType, NumStatesType>::fillTransitionMatrix(
	coretools::TConstView<PrecisionType> Values) {
	if (!_isValid(Values)) { return false; }

	return this->_fillTransitionMatrixAsExponential(Values, _generatingMatrix);
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixLadder<PrecisionType, NumStatesType>::fillTransitionMatrixFromStartValues() {
	this->_fillTransitionMatrixAsExponential(getStartValues(), _generatingMatrix);
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixLadder<PrecisionType, NumStatesType>::fillStationary(
	coretools::TConstView<PrecisionType> Values) {
	if (!_isValid(Values)) { return false; }

	this->_stationary = _generatingMatrix.fillStationary(Values);
	return true;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType> TTransitionMatrixLadder<PrecisionType, NumStatesType>::getStartValues() const {
	// kappa: start at small value
	return {1e-05};
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixLadder<PrecisionType, NumStatesType>::optimizeWithNelderMead() const {
	return false;
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixLadder<PrecisionType, NumStatesType>::optimizeWithLineSearch() const {
	return true;
}

template<typename PrecisionType, typename NumStatesType>
const arma::mat &TTransitionMatrixLadder<PrecisionType, NumStatesType>::getUnparametrizedGeneratingMatrix() const {
	return _generatingMatrix.getUnparametrizedGeneratingMatrix();
}

template<typename PrecisionType, typename NumStatesType>
size_t TTransitionMatrixLadder<PrecisionType, NumStatesType>::numParameters() const {
	return _generatingMatrix.numParameters();
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixLadder<PrecisionType, NumStatesType>::fillHeaderEMReportFile(
	std::vector<std::string> &Header) const {
	Header.emplace_back("kappa");
}

//-------------------------------------------
// TTransitionMatrixScaledLadder
//-------------------------------------------

template<typename PrecisionType, typename NumStatesType>
TTransitionMatrixScaledLadder<PrecisionType, NumStatesType>::TTransitionMatrixScaledLadder()
	: TTransitionMatrixDistBase<PrecisionType, NumStatesType>() {}

template<typename PrecisionType, typename NumStatesType>
TTransitionMatrixScaledLadder<PrecisionType, NumStatesType>::TTransitionMatrixScaledLadder(NumStatesType NumStates)
	: TTransitionMatrixDistBase<PrecisionType, NumStatesType>() {
	resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixScaledLadder<PrecisionType, NumStatesType>::resize(NumStatesType NumStates) {
	TTransitionMatrixDistBase<PrecisionType, NumStatesType>::resize(NumStates);
	_generatingMatrix.resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixScaledLadder<PrecisionType, NumStatesType>::_isValid(
	coretools::TConstView<PrecisionType> Values) const {
	if (!coretools::Positive::isInsideInterval(Values[0]) || !coretools::ZeroOneClosed::isInsideInterval(Values[1])) {
		return false;
	}
	if (!_generatingMatrix.fixMu() && !coretools::ZeroOneClosed::isInsideInterval(Values[2])) { return false; }
	return true;
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixScaledLadder<PrecisionType, NumStatesType>::fillTransitionMatrix(
	coretools::TConstView<PrecisionType> Values) {
	if (!_isValid(Values)) { return false; }

	return this->_fillTransitionMatrixAsExponential(Values, _generatingMatrix);
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixScaledLadder<PrecisionType, NumStatesType>::fillTransitionMatrixFromStartValues() {
	this->_fillTransitionMatrixAsExponential(getStartValues(), _generatingMatrix);
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixScaledLadder<PrecisionType, NumStatesType>::fillStationary(
	coretools::TConstView<PrecisionType> Values) {
	if (!_isValid(Values)) { return false; }

	this->_stationary = _generatingMatrix.fillStationary(Values);
	return true;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType> TTransitionMatrixScaledLadder<PrecisionType, NumStatesType>::getStartValues() const {
	// choose some more or less sensible starting values (used to start initialization of EM)
	return {0.0001, 0.99, 0.99};
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixScaledLadder<PrecisionType, NumStatesType>::optimizeWithNelderMead() const {
	return true;
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixScaledLadder<PrecisionType, NumStatesType>::optimizeWithLineSearch() const {
	return false;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType> TTransitionMatrixScaledLadder<PrecisionType, NumStatesType>::transformToNelderMeadSpace(
	coretools::TConstView<PrecisionType> Values) {
	// normal space to Nelder-Mead space
	// kappa must be [0,inf] -> take log to transform them to [-inf,inf] space where Nelder-Mead can operate on
	// Note: nu and mu must also be [0,inf], however, the parametrization of a scaled ladder only makes sense if
	// they are <1, since then, it is an attractor
	//  -> take logit to transform them to [-inf,inf] space where Nelder-Mead can operate on
	std::vector<PrecisionType> result(Values.size());
	result[0]       = log(Values[0]);
	const double nu = std::max(std::min(Values[1], 0.999), 0.001); // avoid issues with nan
	result[1]       = coretools::logit(nu);
	if (_generatingMatrix.fixMu()) {
		result[2] = 0.0;
	} else {
		const double mu = std::max(std::min(Values[2], 0.999), 0.001); // avoid issues with nan
		result[2]       = coretools::logit(mu);
	}
	return result;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType> TTransitionMatrixScaledLadder<PrecisionType, NumStatesType>::transformFromNelderMeadSpace(
	coretools::TConstView<PrecisionType> Values) {
	// Nelder-Mead space to normal space
	// kappa must be [0,inf] -> take exp to transform back from [-inf,inf] space where Nelder-Mead has operated on
	// Note: nu and mu must also be [0,inf], however, the parametrization of a scaled ladder only makes sense if
	// they are <1, since then, it is an attractor
	//  -> take logistic to transform back from [-inf,inf] space where Nelder-Mead has operated on
	std::vector<PrecisionType> result(Values.size());
	result[0] = exp(Values[0]);
	result[1] = coretools::logistic(Values[1]);
	if (_generatingMatrix.fixMu()) {
		result[2] = 1.0;
	} else {
		result[2] = coretools::logistic(Values[2]);
	}
	return result;
}

template<typename PrecisionType, typename NumStatesType>
size_t TTransitionMatrixScaledLadder<PrecisionType, NumStatesType>::numParameters() const {
	return _generatingMatrix.numParameters();
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixScaledLadder<PrecisionType, NumStatesType>::fillHeaderEMReportFile(
	std::vector<std::string> &Header) const {
	Header.emplace_back("kappa");
	Header.emplace_back("mu");
	Header.emplace_back("nu");
}

//--------------------------------------------
// TTransitionMatrixScaledLadderAttractorShift
//--------------------------------------------

template<typename PrecisionType, typename NumStatesType>
TTransitionMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::TTransitionMatrixScaledLadderAttractorShift()
	: TTransitionMatrixDistBase<PrecisionType, NumStatesType>() {}

template<typename PrecisionType, typename NumStatesType>
TTransitionMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::TTransitionMatrixScaledLadderAttractorShift(
	NumStatesType NumStates)
	: TTransitionMatrixDistBase<PrecisionType, NumStatesType>() {
	resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::resize(NumStatesType NumStates) {
	TTransitionMatrixDistBase<PrecisionType, NumStatesType>::resize(NumStates);
	_generatingMatrix.resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::_isValid(
	coretools::TConstView<PrecisionType> Values) const {

	if (!coretools::Positive::isInsideInterval(Values[0]) || !coretools::ZeroOneClosed::isInsideInterval(Values[1])) {
		return false;
	}
	if (!_generatingMatrix.fixMu() && !coretools::ZeroOneClosed::isInsideInterval(Values[2])) { return false; }
	return true;
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::fillTransitionMatrix(
	coretools::TConstView<PrecisionType> Values) {
	if (!_isValid(Values)) { return false; }

	return this->_fillTransitionMatrixAsExponential(Values, _generatingMatrix);
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::fillTransitionMatrixFromStartValues() {
	this->_fillTransitionMatrixAsExponential(getStartValues(), _generatingMatrix);
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::setIxAttractor(size_t Ix) {
	_generatingMatrix.setIxAttractor(Ix);
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType>
TTransitionMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::getStartValues() const {
	// choose some more or less sensible starting values (used to start initialization of EM)
	return {0.0001, 0.99, 0.99};
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::optimizeWithNelderMead() const {
	return true;
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::optimizeWithLineSearch() const {
	return false;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType>
TTransitionMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::transformToNelderMeadSpace(
	coretools::TConstView<PrecisionType> Values) {
	// normal space to Nelder-Mead space
	// kappa must be [0,inf] -> take log to transform them to [-inf,inf] space where Nelder-Mead can operate on
	// Note: nu and mu must also be [0,inf], however, the parametrization of a scaled ladder only makes sense if
	// they are <1, since then, it is an attractor
	//  -> take logit to transform them to [-inf,inf] space where Nelder-Mead can operate on
	std::vector<PrecisionType> result(Values.size());
	result[0]       = log(Values[0]);
	const double nu = std::max(std::min(Values[1], 0.999), 0.001); // avoid issues with nan
	result[1]       = coretools::logit(nu);
	if (_generatingMatrix.fixMu()) {
		result[2] = 0.0;
	} else {
		const double mu = std::max(std::min(Values[2], 0.999), 0.001); // avoid issues with nan
		result[2]       = coretools::logit(mu);
	}
	return result;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType>
TTransitionMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::transformFromNelderMeadSpace(
	coretools::TConstView<PrecisionType> Values) {
	// Nelder-Mead space to normal space
	// kappa must be [0,inf] -> take exp to transform back from [-inf,inf] space where Nelder-Mead has operated on
	// Note: nu and mu must also be [0,inf], however, the parametrization of a scaled ladder only makes sense if
	// they are <1, since then, it is an attractor
	//  -> take logistic to transform back from [-inf,inf] space where Nelder-Mead has operated on
	std::vector<PrecisionType> result(Values.size());
	result[0] = exp(Values[0]);
	result[1] = coretools::logistic(Values[1]);
	if (_generatingMatrix.fixMu()) {
		result[2] = 1.0;
	} else {
		result[2] = coretools::logistic(Values[2]);
	}
	return result;
}

template<typename PrecisionType, typename NumStatesType>
size_t TTransitionMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::numParameters() const {
	return _generatingMatrix.numParameters();
}

template<typename PrecisionType, typename NumStatesType>
size_t TTransitionMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::ix() const {
	return _generatingMatrix.ix();
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::fillHeaderEMReportFile(
	std::vector<std::string> &Header) const {
	Header.emplace_back("kappa");
	Header.emplace_back("nu");
	Header.emplace_back("mu");
	Header.emplace_back("ix");
}

//-----------------------------------------------
// TTransitionMatrixScaledLadderAttractorShift2
//-----------------------------------------------

template<typename PrecisionType, typename NumStatesType>
TTransitionMatrixScaledLadderAttractorShift2<PrecisionType,
											 NumStatesType>::TTransitionMatrixScaledLadderAttractorShift2()
	: TTransitionMatrixDistBase<PrecisionType, NumStatesType>() {}

template<typename PrecisionType, typename NumStatesType>
TTransitionMatrixScaledLadderAttractorShift2<
	PrecisionType, NumStatesType>::TTransitionMatrixScaledLadderAttractorShift2(NumStatesType NumStates)
	: TTransitionMatrixDistBase<PrecisionType, NumStatesType>() {
	resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::resize(NumStatesType NumStates) {
	TTransitionMatrixDistBase<PrecisionType, NumStatesType>::resize(NumStates);
	_generatingMatrix.resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::_isValid(
	coretools::TConstView<PrecisionType> Values) const {

	if (!coretools::Positive::isInsideInterval(Values[0]) || !coretools::ZeroOneClosed::isInsideInterval(Values[1])) {
		return false;
	}
	if (!_generatingMatrix.fixMu() && !coretools::ZeroOneClosed::isInsideInterval(Values[2])) { return false; }
	return true;
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::fillTransitionMatrix(
	coretools::TConstView<PrecisionType> Values) {
	if (!_isValid(Values)) { return false; }

	return this->_fillTransitionMatrixAsExponential(Values, _generatingMatrix);
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::fillTransitionMatrixFromStartValues() {
	this->_fillTransitionMatrixAsExponential(getStartValues(), _generatingMatrix);
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::setIxAttractor(size_t Ix) {
	_generatingMatrix.setIxAttractor(Ix);
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType>
TTransitionMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::getStartValues() const {
	// choose some more or less sensible starting values (used to start initialization of EM)
	// nu = 0 -> stay in attractor -> choose large value -> attractor doesn't matter so much
	// mu = 0 -> move equally likely to border as to attractor -> choose small value -> move freely
	return {0.0001, 0.95, 0.05};
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::optimizeWithNelderMead() const {
	return true;
}

template<typename PrecisionType, typename NumStatesType>
bool TTransitionMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::optimizeWithLineSearch() const {
	return false;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType>
TTransitionMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::transformToNelderMeadSpace(
	coretools::TConstView<PrecisionType> Values) {
	// normal space to Nelder-Mead space
	// kappa must be [0,inf] -> take log to transform them to [-inf,inf] space where Nelder-Mead can operate on
	// nu and mu must be [0,1] -> take logit to transform them to [-inf,inf] space where Nelder-Mead can operate on
	std::vector<PrecisionType> result(Values.size());
	result[0]       = log(Values[0]);
	const double nu = std::max(std::min(Values[1], 0.999), 0.001); // avoid issues with nan
	result[1]       = coretools::logit(nu);
	if (_generatingMatrix.fixMu()) {
		result[2] = 0.0;
	} else {
		const double mu = std::max(std::min(Values[2], 0.999), 0.001); // avoid issues with nan
		result[2]       = coretools::logit(mu);
	}
	return result;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType>
TTransitionMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::transformFromNelderMeadSpace(
	coretools::TConstView<PrecisionType> Values) {
	// Nelder-Mead space to normal space
	// kappa must be [0,inf] -> take exp to transform back from [-inf,inf] space where Nelder-Mead has operated on
	// nu and mu must be [0,1] -> take logistic to transform from [-inf,inf] space where Nelder-Mead has operated on
	std::vector<PrecisionType> result(Values.size());
	result[0] = exp(Values[0]);
	result[1] = coretools::logistic(Values[1]);
	if (_generatingMatrix.fixMu()) {
		result[2] = 1.0;
	} else {
		result[2] = coretools::logistic(Values[2]);
	}
	return result;
}

template<typename PrecisionType, typename NumStatesType>
size_t TTransitionMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::numParameters() const {
	return _generatingMatrix.numParameters();
}

template<typename PrecisionType, typename NumStatesType>
size_t TTransitionMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::ix() const {
	return _generatingMatrix.ix();
}

template<typename PrecisionType, typename NumStatesType>
void TTransitionMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::fillHeaderEMReportFile(
	std::vector<std::string> &Header) const {
	Header.emplace_back("kappa");
	Header.emplace_back("nu");
	Header.emplace_back("mu");
	Header.emplace_back("ix");
}

}; // end namespace stattools
