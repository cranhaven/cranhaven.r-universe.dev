#include "coretools/Main/TError.h"
namespace stattools {

//-------------------------------------------
// TGeneratingMatrix
//-------------------------------------------
template<typename PrecisionType, typename NumStatesType>
TGeneratingMatrixBase<PrecisionType, NumStatesType>::TGeneratingMatrixBase() {
	_generatingMatrix.zeros(0, 0);
}

template<typename PrecisionType, typename NumStatesType>
TGeneratingMatrixBase<PrecisionType, NumStatesType>::TGeneratingMatrixBase(NumStatesType NumStates) {
	resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixBase<PrecisionType, NumStatesType>::resize(NumStatesType NumStates) {
	_generatingMatrix.zeros(NumStates, NumStates);
}

template<typename PrecisionType, typename NumStatesType>
PrecisionType TGeneratingMatrixBase<PrecisionType, NumStatesType>::operator()(NumStatesType From,
																			  NumStatesType To) const {
	return _generatingMatrix(From, To);
}

template<typename PrecisionType, typename NumStatesType>
const arma::mat &TGeneratingMatrixBase<PrecisionType, NumStatesType>::getGeneratingMatrix() const {
	return _generatingMatrix;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType>
TGeneratingMatrixBase<PrecisionType, NumStatesType>::fillStationary(coretools::TConstView<PrecisionType>) {
	// default: generating matrix does not know how to calculate stationary distribution of Markov Chain
	std::vector<PrecisionType> stationary(_generatingMatrix.n_rows, 0.);
	return stationary;
}

template<typename PrecisionType, typename NumStatesType>
const arma::mat &TGeneratingMatrixBase<PrecisionType, NumStatesType>::getUnparametrizedGeneratingMatrix() const {
	// for some generating matrices (those that only have one parameter),
	// it is possible to seperate the generating matrix into parameter * Matrix
	// where Matrix is then unparametrized
	throw coretools::TDevError("not implemented for base class TGeneratingMatrixBase!");
}

//-------------------------------------------
// TGeneratingMatrixBool
//-------------------------------------------

template<typename PrecisionType, typename NumStatesType>
TGeneratingMatrixBool<PrecisionType, NumStatesType>::TGeneratingMatrixBool()
	: TGeneratingMatrixBase<PrecisionType, NumStatesType>() {
	// always 2 states
	resize(2);
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixBool<PrecisionType, NumStatesType>::resize(NumStatesType NumStates) {
	if (NumStates != 2) {
		throw coretools::TDevError("Generating matrix for booleans can only have 2 states (not " + coretools::str::toString(NumStates) +
				 ")!");
	}
	TGeneratingMatrixBase<PrecisionType, NumStatesType>::resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixBool<PrecisionType, NumStatesType>::fillGeneratingMatrix(
	coretools::TConstView<PrecisionType> Values) {
	assert(Values.size() == 2);
	PrecisionType Lambda1 = Values[0];
	PrecisionType Lambda2 = Values[1];

	// fill generating matrix
	_generatingMatrix(0, 0) = -Lambda1;
	_generatingMatrix(0, 1) = Lambda1;
	_generatingMatrix(1, 0) = Lambda2;
	_generatingMatrix(1, 1) = -Lambda2;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType>
TGeneratingMatrixBool<PrecisionType, NumStatesType>::fillStationary(coretools::TConstView<PrecisionType> Values) {
	// stationary distribution of Markov Chain can be directly computed from parameters of generating matrix
	PrecisionType Lambda1 = Values[0];
	PrecisionType Lambda2 = Values[1];
	double P0             = Lambda2 / (Lambda1 + Lambda2);

	return {P0, 1.0 - P0};
}

template<typename PrecisionType, typename NumStatesType>
size_t TGeneratingMatrixBool<PrecisionType, NumStatesType>::numParameters() const {
	return 2; // Lambda1 and Lambda2
}

//-------------------------------------------
// TGeneratingMatrixLadder
//-------------------------------------------

template<typename PrecisionType, typename NumStatesType>
TGeneratingMatrixLadder<PrecisionType, NumStatesType>::TGeneratingMatrixLadder()
	: TGeneratingMatrixBase<PrecisionType, NumStatesType>() {}

template<typename PrecisionType, typename NumStatesType>
TGeneratingMatrixLadder<PrecisionType, NumStatesType>::TGeneratingMatrixLadder(NumStatesType NumStates) {
	resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixLadder<PrecisionType, NumStatesType>::resize(NumStatesType NumStates) {
	if (NumStates < 2) {
		throw coretools::TDevError("Generating matrix for ladder-type transition matrix must have at least 2 states (not " +
				 coretools::str::toString(NumStates) + ")!");
	}
	TGeneratingMatrixBase<PrecisionType, NumStatesType>::resize(NumStates);

	// fill raw lambda
	_fillRawLambda(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixLadder<PrecisionType, NumStatesType>::_fillRawLambda(NumStatesType NumStates) {
	_rawLambda.resize(NumStates, NumStates);

	// fill "raw" lambda matrix -> matrix without kappa
	_rawLambda(0, 0) = -1;
	_rawLambda(0, 1) = 1;
	for (NumStatesType i = 1; i < NumStates - 1; i++) {
		_rawLambda(i, i - 1) = 1;
		_rawLambda(i, i)     = -2;
		_rawLambda(i, i + 1) = 1;
	}
	_rawLambda(NumStates - 1, NumStates - 2) = 1;
	_rawLambda(NumStates - 1, NumStates - 1) = -1;
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixLadder<PrecisionType, NumStatesType>::fillGeneratingMatrix(
	coretools::TConstView<PrecisionType> Values) {
	// only expect one value: kappa
	assert(Values.size() == 1);

	PrecisionType kappa     = Values[0];
	NumStatesType numStates = _generatingMatrix.n_rows;

	// fill generating matrix
	_generatingMatrix(0, 0) = -kappa;
	_generatingMatrix(0, 1) = kappa;
	for (NumStatesType i = 1; i < numStates - 1; i++) {
		_generatingMatrix(i, i - 1) = kappa;
		_generatingMatrix(i, i)     = -2. * kappa;
		_generatingMatrix(i, i + 1) = kappa;
	}
	_generatingMatrix(numStates - 1, numStates - 2) = kappa;
	_generatingMatrix(numStates - 1, numStates - 1) = -kappa;
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType>
TGeneratingMatrixLadder<PrecisionType, NumStatesType>::fillStationary(coretools::TConstView<PrecisionType>) {
	// stationary distribution of Markov Chain can be directly computed from parameters of generating matrix
	// 1/NStates for each state: probability to go up/down is exactly the same for all states
	auto numStates = _generatingMatrix.n_rows;
	std::vector<PrecisionType> stationary(numStates, 1. / numStates);

	return stationary;
}

template<typename PrecisionType, typename NumStatesType>
const arma::mat &TGeneratingMatrixLadder<PrecisionType, NumStatesType>::getUnparametrizedGeneratingMatrix() const {
	return _rawLambda;
}

template<typename PrecisionType, typename NumStatesType>
size_t TGeneratingMatrixLadder<PrecisionType, NumStatesType>::numParameters() const {
	return 1; // kappa
}

//-------------------------------------------
// TGeneratingMatrixScaledLadderBase
//-------------------------------------------

template<typename PrecisionType, typename NumStatesType>
TGeneratingMatrixScaledLadderBase<PrecisionType, NumStatesType>::TGeneratingMatrixScaledLadderBase()
	: TGeneratingMatrixBase<PrecisionType, NumStatesType>() {}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderBase<PrecisionType, NumStatesType>::_fillFirstRow(PrecisionType Kappa) {
	_generatingMatrix(0, 0) = -Kappa;
	_generatingMatrix(0, 1) = Kappa;
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderBase<PrecisionType, NumStatesType>::_fillLastRow(PrecisionType Kappa) {
	const size_t numStates                          = _generatingMatrix.n_rows;
	_generatingMatrix(numStates - 1, numStates - 2) = Kappa;
	_generatingMatrix(numStates - 1, numStates - 1) = -Kappa;
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderBase<PrecisionType, NumStatesType>::_fillRowAboveAttractor(size_t i,
																							 PrecisionType Kappa,
																							 PrecisionType Mu) {
	_generatingMatrix(i, i - 1) = Kappa * Mu;
	_generatingMatrix(i, i)     = Kappa * (-1.0 - Mu);
	_generatingMatrix(i, i + 1) = Kappa;
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderBase<PrecisionType, NumStatesType>::_fillRowBelowAttractor(size_t i,
																							 PrecisionType Kappa,
																							 PrecisionType Mu) {
	_generatingMatrix(i, i - 1) = Kappa;
	_generatingMatrix(i, i)     = Kappa * (-1.0 - Mu);
	_generatingMatrix(i, i + 1) = Kappa * Mu;
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderBase<PrecisionType, NumStatesType>::_fillRowAttractor(size_t i, PrecisionType Kappa,
																						PrecisionType Mu,
																						PrecisionType Nu) {
	_generatingMatrix(i, i - 1) = Kappa * Nu * Mu;
	_generatingMatrix(i, i)     = -2.0 * Kappa * Nu * Mu;
	_generatingMatrix(i, i + 1) = Kappa * Nu * Mu;
}

//-------------------------------------------
// TGeneratingMatrixScaledLadder
//-------------------------------------------

template<typename PrecisionType, typename NumStatesType>
TGeneratingMatrixScaledLadder<PrecisionType, NumStatesType>::TGeneratingMatrixScaledLadder()
	: TGeneratingMatrixScaledLadderBase<PrecisionType, NumStatesType>() {}

template<typename PrecisionType, typename NumStatesType>
TGeneratingMatrixScaledLadder<PrecisionType, NumStatesType>::TGeneratingMatrixScaledLadder(NumStatesType NumStates)
	: TGeneratingMatrixScaledLadderBase<PrecisionType, NumStatesType>() {
	resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadder<PrecisionType, NumStatesType>::resize(NumStatesType NumStates) {
	if (NumStates < 3) {
		throw coretools::TDevError("Generating matrix for ladder-type transition matrix must have at least 3 states (not ", NumStates,
				 ")!");
	}
	if (NumStates % 2 == 0) {
		throw coretools::TDevError("Generating matrix for ladder-type transition matrix must have an uneven number of states (not ",
				 NumStates, ")!");
	}
	TGeneratingMatrixBase<PrecisionType, NumStatesType>::resize(NumStates);
	_s_max = (NumStates - 1) / 2;
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadder<PrecisionType, NumStatesType>::fillGeneratingMatrix(
	coretools::TConstView<PrecisionType> Values) {
	// if only 3 states: mu is omitted
	PrecisionType kappa = Values[0];
	PrecisionType nu    = Values[1];
	PrecisionType mu    = 1.0;
	if (!fixMu()) { mu = Values[2]; }

	NumStatesType numStates = _generatingMatrix.n_rows;

	// fill generating matrix
	this->_fillFirstRow(kappa);
	for (NumStatesType i = 1; i < numStates - 1; i++) {
		if (i < _s_max) {
			this->_fillRowAboveAttractor(i, kappa, mu);
		} else if (i == _s_max) {
			this->_fillRowAttractor(i, kappa, mu, nu);
		} else {
			this->_fillRowBelowAttractor(i, kappa, mu);
		}
	}
	this->_fillLastRow(kappa);
}

template<typename PrecisionType, typename NumStatesType>
std::vector<PrecisionType> TGeneratingMatrixScaledLadder<PrecisionType, NumStatesType>::fillStationary(
	coretools::TConstView<PrecisionType> Values) {
	// if only 3 states: mu is omitted
	PrecisionType nu = Values[1];
	PrecisionType mu = 1.0;
	if (!fixMu()) { mu = Values[2]; }

	NumStatesType numStates = _generatingMatrix.n_rows;
	std::vector<PrecisionType> stationary(numStates);

	if (mu == 1.0) {
		// equation for stationary results in NaN -> separate equation here
		double c    = nu * numStates - nu + 1.0;
		double frac = nu / c;
		std::fill(stationary.begin(), stationary.end(), frac);
		stationary[_s_max] = 1. / c;
	} else {
		// calculate c
		double c = (mu - 1.0) * nu * pow(mu, _s_max) / (2.0 * mu * nu * (pow(mu, _s_max) - 1.0) + mu - 1.0);

		// fill first row only
		for (NumStatesType i = 0; i < _s_max; i++) { // first half
			stationary[i] = c / pow(mu, i);
		}
		stationary[_s_max] = c / (pow(mu, _s_max) * nu); // middle state
		for (int i = (int)_s_max - 1; i >= 0; i--) {     // second half
			stationary[numStates - i - 1] = c / pow(mu, i);
		}
	}
	return stationary;
}

template<typename PrecisionType, typename NumStatesType>
bool TGeneratingMatrixScaledLadder<PrecisionType, NumStatesType>::fixMu() const {
	if (_s_max <= 1) { return true; }
	return false;
}

template<typename PrecisionType, typename NumStatesType>
size_t TGeneratingMatrixScaledLadder<PrecisionType, NumStatesType>::numParameters() const {
	return 3; // kappa, nu and mu
}

//--------------------------------------------
// TGeneratingMatrixScaledLadderAttractorShift
//--------------------------------------------

template<typename PrecisionType, typename NumStatesType>
TGeneratingMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::TGeneratingMatrixScaledLadderAttractorShift()
	: TGeneratingMatrixScaledLadderBase<PrecisionType, NumStatesType>() {}

template<typename PrecisionType, typename NumStatesType>
TGeneratingMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::TGeneratingMatrixScaledLadderAttractorShift(
	NumStatesType NumStates)
	: TGeneratingMatrixScaledLadderBase<PrecisionType, NumStatesType>() {
	resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::resize(NumStatesType NumStates) {
	if (NumStates < 2) {
		throw coretools::TDevError("Generating matrix for shifted ladder-type transition matrix must have at least 2 states (not ",
				 NumStates, ")!");
	}
	TGeneratingMatrixBase<PrecisionType, NumStatesType>::resize(NumStates);
	_numStates = NumStates;
	_ix        = (_numStates - 1) / 2; // set to middle row (or one above, if numStates is even)
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::setIxAttractor(size_t Ix) {
	assert(Ix < _numStates);
	_ix = Ix;
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::_fillFirstRowAttractor(
	PrecisionType Kappa, PrecisionType Nu, PrecisionType Mu) {
	_generatingMatrix(0, 0) = -Kappa * Nu * Mu;
	_generatingMatrix(0, 1) = Kappa * Nu * Mu;
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::_fillLastRowAttractor(
	PrecisionType Kappa, PrecisionType Nu, PrecisionType Mu) {
	const size_t numStates                          = _generatingMatrix.n_rows;
	_generatingMatrix(numStates - 1, numStates - 2) = Kappa * Nu * Mu;
	_generatingMatrix(numStates - 1, numStates - 1) = -Kappa * Nu * Mu;
}

template<typename PrecisionType, typename NumStatesType>
bool TGeneratingMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::fixMu() const {
	return _numStates == 2 || (_numStates == 3 && _ix == 1);
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::fillGeneratingMatrix(
	coretools::TConstView<PrecisionType> Values) {
	PrecisionType kappa = Values[0];
	PrecisionType nu    = Values[1];
	PrecisionType mu    = 1.0;
	if (!fixMu()) { mu = Values[2]; }

	// fill generating matrix
	if (_ix == 0) {
		_fillFirstRowAttractor(kappa, nu, mu);
	} else {
		this->_fillFirstRow(kappa);
	}
	for (NumStatesType i = 1; i < _numStates - 1; i++) {
		if (i < _ix) {
			this->_fillRowAboveAttractor(i, kappa, mu);
		} else if (i == _ix) {
			this->_fillRowAttractor(i, kappa, mu, nu);
		} else {
			this->_fillRowBelowAttractor(i, kappa, mu);
		}
	}
	if ((int)_ix == (int)_numStates - 1) {
		_fillLastRowAttractor(kappa, nu, mu);
	} else {
		this->_fillLastRow(kappa);
	}
}

template<typename PrecisionType, typename NumStatesType>
size_t TGeneratingMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::numParameters() const {
	return 3; // kappa, nu, mu
}

template<typename PrecisionType, typename NumStatesType>
size_t TGeneratingMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType>::ix() const {
	return _ix;
}

//-----------------------------------------------
// TGeneratingMatrixScaledLadderAttractorShift2
//-----------------------------------------------

template<typename PrecisionType, typename NumStatesType>
TGeneratingMatrixScaledLadderAttractorShift2<PrecisionType,
											 NumStatesType>::TGeneratingMatrixScaledLadderAttractorShift2()
	: TGeneratingMatrixBase<PrecisionType, NumStatesType>() {}

template<typename PrecisionType, typename NumStatesType>
TGeneratingMatrixScaledLadderAttractorShift2<
	PrecisionType, NumStatesType>::TGeneratingMatrixScaledLadderAttractorShift2(NumStatesType NumStates)
	: TGeneratingMatrixBase<PrecisionType, NumStatesType>() {
	resize(NumStates);
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::resize(NumStatesType NumStates) {
	if (NumStates < 2) {
		throw coretools::TDevError("Generating matrix for shifted ladder-type transition matrix must have at least 2 states (not ",
				 NumStates, ")!");
	}
	TGeneratingMatrixBase<PrecisionType, NumStatesType>::resize(NumStates);
	_numStates = NumStates;
	_ix        = (_numStates - 1) / 2; // set to middle row (or one above, if numStates is even)
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::setIxAttractor(size_t Ix) {
	assert(Ix < _numStates);
	_ix = Ix;
}

template<typename PrecisionType, typename NumStatesType>
bool TGeneratingMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::fixMu() const {
	return _numStates == 2 || (_numStates == 3 && _ix == 1);
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::fillGeneratingMatrix(
	coretools::TConstView<PrecisionType> Values) {
	PrecisionType kappa = Values[0];
	PrecisionType nu    = Values[1];
	PrecisionType mu    = 1.0;
	if (!fixMu()) { mu = Values[2]; }

	// fill generating matrix
	if (_ix == 0) {
		_fillFirstRowAttractor(kappa, nu);
	} else {
		_fillFirstRow(kappa);
	}
	for (NumStatesType i = 1; i < _numStates - 1; i++) {
		if (i < _ix) {
			_fillRowAboveAttractor(i, kappa, mu);
		} else if (i == _ix) {
			_fillRowAttractor(i, kappa, nu);
		} else {
			_fillRowBelowAttractor(i, kappa, mu);
		}
	}
	if ((int)_ix == (int)_numStates - 1) {
		_fillLastRowAttractor(kappa, nu);
	} else {
		_fillLastRow(kappa);
	}
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::_fillFirstRowAttractor(
	PrecisionType Kappa, PrecisionType Nu) {
	_generatingMatrix(0, 0) = -Kappa * Nu;
	_generatingMatrix(0, 1) = Kappa * Nu;
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::_fillLastRowAttractor(
	PrecisionType Kappa, PrecisionType Nu) {
	const size_t numStates                          = _generatingMatrix.n_rows;
	_generatingMatrix(numStates - 1, numStates - 2) = Kappa * Nu;
	_generatingMatrix(numStates - 1, numStates - 1) = -Kappa * Nu;
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::_fillFirstRow(PrecisionType Kappa) {
	_generatingMatrix(0, 0) = -Kappa;
	_generatingMatrix(0, 1) = Kappa;
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::_fillLastRow(PrecisionType Kappa) {
	const size_t numStates                          = _generatingMatrix.n_rows;
	_generatingMatrix(numStates - 1, numStates - 2) = Kappa;
	_generatingMatrix(numStates - 1, numStates - 1) = -Kappa;
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::_fillRowAboveAttractor(
	size_t i, PrecisionType Kappa, PrecisionType Mu) {
	_generatingMatrix(i, i - 1) = Kappa * (1.0 - Mu);
	_generatingMatrix(i, i)     = -2.0 * Kappa;
	_generatingMatrix(i, i + 1) = Kappa * (1.0 + Mu);
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::_fillRowBelowAttractor(
	size_t i, PrecisionType Kappa, PrecisionType Mu) {
	_generatingMatrix(i, i - 1) = Kappa * (1.0 + Mu);
	_generatingMatrix(i, i)     = -2.0 * Kappa;
	_generatingMatrix(i, i + 1) = Kappa * (1.0 - Mu);
}

template<typename PrecisionType, typename NumStatesType>
void TGeneratingMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::_fillRowAttractor(size_t i,
																								   PrecisionType Kappa,
																								   PrecisionType Nu) {
	_generatingMatrix(i, i - 1) = Kappa * Nu;
	_generatingMatrix(i, i)     = -2.0 * Kappa * Nu;
	_generatingMatrix(i, i + 1) = Kappa * Nu;
}

template<typename PrecisionType, typename NumStatesType>
size_t TGeneratingMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::numParameters() const {
	return 3; // kappa, nu, mu
}

template<typename PrecisionType, typename NumStatesType>
size_t TGeneratingMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType>::ix() const {
	return _ix;
}

}; // end namespace stattools
