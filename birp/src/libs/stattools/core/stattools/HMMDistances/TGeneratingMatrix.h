//
// Created by madleina on 15.11.21.
//

#ifndef TGENERATINGMATRIX_H
#define TGENERATINGMATRIX_H

#include <cmath>
#include "coretools/arma_include.h"
#include "coretools/Containers/TView.h"

namespace stattools {

//-------------------------------------------
// TGeneratingMatrixBase
//-------------------------------------------
template<typename PrecisionType, typename NumStatesType> class TGeneratingMatrixBase {
	// pure virtual base class for generating matrices
	// stores generating matrix
	// provides all functions for interface
protected:
	// generating matrix
	arma::mat _generatingMatrix;

public:
	TGeneratingMatrixBase();
	TGeneratingMatrixBase(NumStatesType NumStates);
	virtual ~TGeneratingMatrixBase() = default;

	virtual void resize(NumStatesType NumStates);

	// fill generating matrix from values
	virtual void fillGeneratingMatrix(coretools::TConstView<PrecisionType> Values) = 0;

	// stationary
	virtual std::vector<PrecisionType> fillStationary(coretools::TConstView<PrecisionType> Values);

	// get entries
	PrecisionType operator()(NumStatesType From, NumStatesType To) const;
	const arma::mat &getGeneratingMatrix() const;
	virtual const arma::mat &getUnparametrizedGeneratingMatrix() const;
	virtual size_t numParameters() const = 0;
};

template<typename PrecisionType, typename NumStatesType>
class TGeneratingMatrixBool : public TGeneratingMatrixBase<PrecisionType, NumStatesType> {
	// generating matrix for bools (always 2 states)
	// parameterized by two values (Lambda1 and Lambda2)
	// Mat = (-Lambda1,  Lambda1)
	//       ( Lambda2, -Lambda2)

protected:
	using TGeneratingMatrixBase<PrecisionType, NumStatesType>::_generatingMatrix;

public:
	TGeneratingMatrixBool();
	TGeneratingMatrixBool(NumStatesType NumStates);
	~TGeneratingMatrixBool() override = default;
	void resize(NumStatesType NumStates) override;

	// fill
	void fillGeneratingMatrix(coretools::TConstView<PrecisionType> Values) override;

	// stationary
	std::vector<PrecisionType> fillStationary(coretools::TConstView<PrecisionType> Values) override;

	// getters
	size_t numParameters() const override;
};

template<typename PrecisionType, typename NumStatesType>
class TGeneratingMatrixLadder : public TGeneratingMatrixBase<PrecisionType, NumStatesType> {
	// generating matrix with N states and bandwidth 1
	// can stay, go up or go down (ladder-type transition matrix)
	// parameterized by one value (kappa)
	// Mat = kappa * (-1, 1, 0, 0, ..., 0, 0)
	//               (1, -2, 1, 0, ..., 0, 0)
	//               (0, 1, -2, 1, ..., 0, 0)
	//               (......................)
	//               (0, 0, 0, 0, ..., 1, -1)
protected:
	using TGeneratingMatrixBase<PrecisionType, NumStatesType>::_generatingMatrix;

	// unparametrized generating matrix (matrix without kappa)
	arma::mat _rawLambda;
	void _fillRawLambda(NumStatesType NumStates);

public:
	TGeneratingMatrixLadder();
	TGeneratingMatrixLadder(NumStatesType NumStates);
	~TGeneratingMatrixLadder() override = default;
	void resize(NumStatesType NumStates) override;

	// fill
	void fillGeneratingMatrix(coretools::TConstView<PrecisionType> Values) override;
	const arma::mat &getUnparametrizedGeneratingMatrix() const override;

	// stationary
	std::vector<PrecisionType> fillStationary(coretools::TConstView<PrecisionType> Values) override;

	// getters
	size_t numParameters() const override;
};

template<typename PrecisionType, typename NumStatesType>
class TGeneratingMatrixScaledLadderBase : public TGeneratingMatrixBase<PrecisionType, NumStatesType> {
	// abstract base class for TGeneratingMatrixScaledLadder and TGeneratingMatrixScaledLadderShifted
protected:
	using TGeneratingMatrixBase<PrecisionType, NumStatesType>::_generatingMatrix;

	void _fillRowAboveAttractor(size_t i, PrecisionType Kappa, PrecisionType Mu);
	void _fillRowBelowAttractor(size_t i, PrecisionType Kappa, PrecisionType Mu);
	void _fillRowAttractor(size_t i, PrecisionType Kappa, PrecisionType Mu, PrecisionType Nu);
	void _fillFirstRow(PrecisionType Kappa);
	void _fillLastRow(PrecisionType Kappa);

public:
	TGeneratingMatrixScaledLadderBase();
};

template<typename PrecisionType, typename NumStatesType>
class TGeneratingMatrixScaledLadder : public TGeneratingMatrixScaledLadderBase<PrecisionType, NumStatesType> {
	// generating matrix with N states and bandwidth 1
	// can stay, go up or go down (ladder-type transition matrix)
	// parameterized by three values (kappa, nu and mu)
	// Mat = kappa * (-1,  1,    0,     0,   ..., 0,   0,     0,      0)
	//               (mu, -1-mu, 1,     0,   ..., 0,   0,     0,      0)
	//               (0,   mu,   -1-mu, 1,   ..., 0,   0,     0,      0)
	//               (..., ...,  ...,   ..., ..., ..., ...,   ...,    ...)
	//               (0,   0,    0,     0,   ..., 1,   -1-mu, mu,      0)
	//               (0,   0,    0,     0,   ..., 0,   1,     -1-mu,  -mu)
	//               (0,   0,    0,     0,   ..., 0,   0,     1,      -1)
	// where the middle row is given by
	//               (0, ..., 0, nu*mu, -2*nu*mu, nu*mu, 0, ..., 0)
	// Used for Flink and ApproxWF transition matrices on selection coefficients
protected:
	using TGeneratingMatrixScaledLadderBase<PrecisionType, NumStatesType>::_generatingMatrix;

	// maximum value for state, there are 2*_s_max + 1 states
	NumStatesType _s_max = 0;

public:
	TGeneratingMatrixScaledLadder();
	TGeneratingMatrixScaledLadder(NumStatesType NumStates);
	~TGeneratingMatrixScaledLadder() override = default;
	void resize(NumStatesType NumStates) override;

	// fill
	void fillGeneratingMatrix(coretools::TConstView<PrecisionType> Values) override;

	// stationary
	std::vector<PrecisionType> fillStationary(coretools::TConstView<PrecisionType> Values) override;

	// getters
	bool fixMu() const;
	size_t numParameters() const override;
};

template<typename PrecisionType, typename NumStatesType>
class TGeneratingMatrixScaledLadderAttractorShift
	: public TGeneratingMatrixScaledLadderBase<PrecisionType, NumStatesType> {
	// generating matrix with N states and bandwidth 1
	// can stay, go up or go down (ladder-type transition matrix)
	// parameterized by three values (kappa, mu and nu), as well as an index of where the attractor line is
	// Mat = kappa * (-1,  1,    0,     0,   ..., 0,   0,     0,      0)
	//               (mu, -1-mu, 1,     0,   ..., 0,   0,     0,      0)
	//               (0,   mu,   -1-mu, 1,   ..., 0,   0,     0,      0)
	//               (..., ...,  ...,   ..., ..., ..., ...,   ...,    ...)
	//               (0,   0,    0,     0,   ..., 1,   -1-mu, mu,      0)
	//               (0,   0,    0,     0,   ..., 0,   1,     -1-mu,  mu)
	//               (0,   0,    0,     0,   ..., 0,   0,     1,      -1)
	// where the attractor row is given by (not necessarily the middle row!)
	//               (0, ..., 0, nu*mu, -2*nu*mu, nu*mu, 0, ..., 0)
protected:
	using TGeneratingMatrixScaledLadderBase<PrecisionType, NumStatesType>::_generatingMatrix;

	NumStatesType _numStates = 0;
	size_t _ix               = 0;

	void _fillLastRowAttractor(PrecisionType Kappa, PrecisionType Nu, PrecisionType Mu);
	void _fillFirstRowAttractor(PrecisionType Kappa, PrecisionType Nu, PrecisionType Mu);

public:
	TGeneratingMatrixScaledLadderAttractorShift();
	TGeneratingMatrixScaledLadderAttractorShift(NumStatesType NumStates);
	~TGeneratingMatrixScaledLadderAttractorShift() override = default;
	void resize(NumStatesType NumStates) override;

	// set index of attractor row
	void setIxAttractor(size_t Ix);

	// fill
	void fillGeneratingMatrix(coretools::TConstView<PrecisionType> Values) override;

	// getters
	bool fixMu() const;
	size_t numParameters() const override;
	size_t ix() const;
};

template<typename PrecisionType, typename NumStatesType>
class TGeneratingMatrixScaledLadderAttractorShift2 : public TGeneratingMatrixBase<PrecisionType, NumStatesType> {
	// generating matrix with N states and bandwidth 1
	// can stay, go up or go down (ladder-type transition matrix)
	// parameterized by three values (kappa, mu and nu), as well as an index of where the attractor line is
	// Mat = kappa * (-1,     1,     0,     0,     ...,     0,     0,     0,     0)
	//               (1-mu,   -2,    1+mu,  0,     ...,     0,     0,     0,     0)
	//               (0,      1-mu,  -2,    1+mu,  ...,     0,     0,     0,     0)
	//               (...,    ...,   ...,   ...,   ...,     ...,   ...,   ...,   ...)
	//               (0,      0,     0,     0,     ...,     1+mu,  -2,    1-mu,  0)
	//               (0,      0,     0,     0,     ...,     0,     1+mu,  -2,    1-mu)
	//               (0,      0,     0,     0,     ...,     0,     0,     1,    -1)
	// where the attractor row is given by (not necessarily the middle row!)
	//               (0, ..., 0,     nu/2,  -nu,   nu/2,    0,     ...,   0)
protected:
	using TGeneratingMatrixBase<PrecisionType, NumStatesType>::_generatingMatrix;

	NumStatesType _numStates = 0;
	size_t _ix               = 0;

	void _fillLastRowAttractor(PrecisionType Kappa, PrecisionType Nu);
	void _fillFirstRowAttractor(PrecisionType Kappa, PrecisionType Nu);
	void _fillRowAboveAttractor(size_t i, PrecisionType Kappa, PrecisionType Mu);
	void _fillRowBelowAttractor(size_t i, PrecisionType Kappa, PrecisionType Mu);
	void _fillRowAttractor(size_t i, PrecisionType Kappa, PrecisionType Nu);
	void _fillFirstRow(PrecisionType Kappa);
	void _fillLastRow(PrecisionType Kappa);

public:
	TGeneratingMatrixScaledLadderAttractorShift2();
	TGeneratingMatrixScaledLadderAttractorShift2(NumStatesType NumStates);
	~TGeneratingMatrixScaledLadderAttractorShift2() override = default;
	void resize(NumStatesType NumStates) override;

	// set index of attractor row
	void setIxAttractor(size_t Ix);

	// fill
	void fillGeneratingMatrix(coretools::TConstView<PrecisionType> Values) override;

	// getters
	bool fixMu() const;
	size_t numParameters() const override;
	size_t ix() const;
};

}; // end namespace stattools

#include "TGeneratingMatrix.tpp"

#endif // TGENERATINGMATRIX_H
