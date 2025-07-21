//
// Created by madleina on 15.11.21.
//

#ifndef TTRANSITIONMATRIXDIST_H
#define TTRANSITIONMATRIXDIST_H

#include "coretools/arma_include.h"
#include "coretools/Containers/TView.h"
#include "stattools/HMMDistances/TGeneratingMatrix.h"

namespace stattools {

//-------------------------------------------
// TTransitionMatrixDistBase
//-------------------------------------------
template<typename PrecisionType, typename NumStatesType> class TTransitionMatrixDistBase {
	// pure virtual base class for transition matrices
	// stores transition matrix
	// provides all functions for interface
protected:
	// transition matrix
	arma::mat _transitionMatrix;
	std::vector<PrecisionType> _stationary;

	template<typename TypeGenerMat>
	bool _fillTransitionMatrixAsExponential(coretools::TConstView<PrecisionType> Values,
											TypeGenerMat &GeneratingMatrix);

public:
	TTransitionMatrixDistBase();

	TTransitionMatrixDistBase(NumStatesType NumStates);

	virtual ~TTransitionMatrixDistBase() = default;

	virtual void resize(NumStatesType NumStates);

	// fill generating matrix from values
	virtual void fillTransitionMatrixFromStartValues()                             = 0;
	virtual bool fillTransitionMatrix(coretools::TConstView<PrecisionType> Values) = 0;

	// stationary
	void fillStationaryFromStartValues();
	virtual bool fillStationary(coretools::TConstView<PrecisionType> Values);

	// sensible starting points (before initialization of EM)
	virtual std::vector<PrecisionType> getStartValues() const = 0;

	// how to optimize?
	virtual bool optimizeWithNelderMead() const = 0;

	virtual bool optimizeWithLineSearch() const = 0;

	// transform values (for optimization, e.g. update in log for Nelder-Mead)
	virtual std::vector<PrecisionType> transformToNelderMeadSpace(coretools::TConstView<PrecisionType> Values);

	virtual std::vector<PrecisionType> transformFromNelderMeadSpace(coretools::TConstView<PrecisionType> Values);

	// get entries
	PrecisionType operator()(NumStatesType From, NumStatesType To) const;

	PrecisionType stationary(NumStatesType State) const;

	const arma::mat &getTransitionMatrix() const;

	arma::mat getStationaryMatrix() const;

	virtual const arma::mat &getUnparametrizedGeneratingMatrix() const;

	NumStatesType numStates() const;

	virtual size_t numParameters() const = 0;

	virtual void fillHeaderEMReportFile(std::vector<std::string> &Header) const = 0;
};

template<typename PrecisionType, typename NumStatesType>
class TTransitionMatrixBool : public TTransitionMatrixDistBase<PrecisionType, NumStatesType> {
protected:
	// parametrized directly by transition matrix
	using TTransitionMatrixDistBase<PrecisionType, NumStatesType>::_transitionMatrix;

	bool _isValid(coretools::TConstView<PrecisionType> Values) const;

public:
	TTransitionMatrixBool();

	TTransitionMatrixBool(NumStatesType NumStates);

	~TTransitionMatrixBool() override = default;

	void resize(NumStatesType NumStates) override;

	// fill
	void fillTransitionMatrixFromStartValues() override;
	bool fillTransitionMatrix(coretools::TConstView<PrecisionType> Values) override;

	// stationary
	bool fillStationary(coretools::TConstView<PrecisionType> Values) override;

	// sensible starting points (before initialization of EM)
	std::vector<PrecisionType> getStartValues() const override;

	// how to optimize?
	bool optimizeWithNelderMead() const override;

	bool optimizeWithLineSearch() const override;

	// transform values (for optimization, e.g. update in log for Nelder-Mead)
	std::vector<PrecisionType> transformToNelderMeadSpace(coretools::TConstView<PrecisionType> Values) override;

	std::vector<PrecisionType> transformFromNelderMeadSpace(coretools::TConstView<PrecisionType> Values) override;

	// getters
	size_t numParameters() const override;

	void fillHeaderEMReportFile(std::vector<std::string> &Header) const override;
};

template<typename PrecisionType, typename NumStatesType>
class TTransitionMatrixCategorical : public TTransitionMatrixDistBase<PrecisionType, NumStatesType> {
protected:
	// parametrized directly by transition matrix
	using TTransitionMatrixDistBase<PrecisionType, NumStatesType>::_transitionMatrix;

	size_t _D = 0;

	bool _isValid(coretools::TConstView<PrecisionType> Values) const;

public:
	TTransitionMatrixCategorical();

	TTransitionMatrixCategorical(NumStatesType NumStates);

	~TTransitionMatrixCategorical() override = default;

	void resize(NumStatesType NumStates) override;

	// fill
	void fillTransitionMatrixFromStartValues() override;
	bool fillTransitionMatrix(coretools::TConstView<PrecisionType> Values) override;

	// stationary
	bool fillStationary(coretools::TConstView<PrecisionType> Values) override;

	// sensible starting points (before initialization of EM)
	std::vector<PrecisionType> getStartValues() const override;

	// how to optimize?
	bool optimizeWithNelderMead() const override;

	bool optimizeWithLineSearch() const override;

	// transform values (for optimization, e.g. update in log for Nelder-Mead)
	std::vector<PrecisionType> transformToNelderMeadSpace(coretools::TConstView<PrecisionType> Values) override;

	std::vector<PrecisionType> transformFromNelderMeadSpace(coretools::TConstView<PrecisionType> Values) override;

	// getters
	size_t numParameters() const override;

	size_t D() const;

	void fillHeaderEMReportFile(std::vector<std::string> &Header) const override;
};

template<typename PrecisionType, typename NumStatesType>
class TTransitionMatrixBoolGeneratingMatrix : public TTransitionMatrixDistBase<PrecisionType, NumStatesType> {
protected:
	// parametrized by generating matrix
	TGeneratingMatrixBool<PrecisionType, NumStatesType> _generatingMatrix;
	using TTransitionMatrixDistBase<PrecisionType, NumStatesType>::_transitionMatrix;

	bool _isValid(coretools::TConstView<PrecisionType> Values) const;

public:
	TTransitionMatrixBoolGeneratingMatrix();

	TTransitionMatrixBoolGeneratingMatrix(NumStatesType NumStates);

	~TTransitionMatrixBoolGeneratingMatrix() override = default;

	void resize(NumStatesType NumStates) override;

	// fill
	void fillTransitionMatrixFromStartValues() override;
	bool fillTransitionMatrix(coretools::TConstView<PrecisionType> Values) override;

	// stationary
	bool fillStationary(coretools::TConstView<PrecisionType> Values) override;

	// sensible starting points (before initialization of EM)
	std::vector<PrecisionType> getStartValues() const override;

	// how to optimize?
	bool optimizeWithNelderMead() const override;

	bool optimizeWithLineSearch() const override;

	// transform values (for optimization, e.g. update in log for Nelder-Mead)
	std::vector<PrecisionType> transformToNelderMeadSpace(coretools::TConstView<PrecisionType> Values) override;

	std::vector<PrecisionType> transformFromNelderMeadSpace(coretools::TConstView<PrecisionType> Values) override;

	// getters
	size_t numParameters() const override;

	void fillHeaderEMReportFile(std::vector<std::string> &Header) const override;
};

template<typename PrecisionType, typename NumStatesType>
class TTransitionMatrixLadder : public TTransitionMatrixDistBase<PrecisionType, NumStatesType> {
protected:
	// parametrized by generating matrix
	TGeneratingMatrixLadder<PrecisionType, NumStatesType> _generatingMatrix;
	using TTransitionMatrixDistBase<PrecisionType, NumStatesType>::_transitionMatrix;

	bool _isValid(coretools::TConstView<PrecisionType> Values) const;

public:
	TTransitionMatrixLadder();

	TTransitionMatrixLadder(NumStatesType NumStates);

	~TTransitionMatrixLadder() override = default;

	void resize(NumStatesType NumStates) override;

	// fill
	void fillTransitionMatrixFromStartValues() override;
	bool fillTransitionMatrix(coretools::TConstView<PrecisionType> Values) override;

	// stationary
	bool fillStationary(coretools::TConstView<PrecisionType> Values) override;

	// sensible starting points (before initialization of EM)
	std::vector<PrecisionType> getStartValues() const override;

	// how to optimize?
	bool optimizeWithNelderMead() const override;

	bool optimizeWithLineSearch() const override;

	// generating matrix
	const arma::mat &getUnparametrizedGeneratingMatrix() const override;

	// getters
	size_t numParameters() const override;

	void fillHeaderEMReportFile(std::vector<std::string> &Header) const override;
};

template<typename PrecisionType, typename NumStatesType>
class TTransitionMatrixScaledLadder : public TTransitionMatrixDistBase<PrecisionType, NumStatesType> {
protected:
	// parametrized by generating matrix
	TGeneratingMatrixScaledLadder<PrecisionType, NumStatesType> _generatingMatrix;
	using TTransitionMatrixDistBase<PrecisionType, NumStatesType>::_transitionMatrix;

	bool _isValid(coretools::TConstView<PrecisionType> Values) const;

public:
	TTransitionMatrixScaledLadder();

	TTransitionMatrixScaledLadder(NumStatesType NumStates);

	~TTransitionMatrixScaledLadder() override = default;

	void resize(NumStatesType NumStates) override;

	// fill
	void fillTransitionMatrixFromStartValues() override;
	bool fillTransitionMatrix(coretools::TConstView<PrecisionType> Values) override;

	// stationary
	bool fillStationary(coretools::TConstView<PrecisionType> Values) override;

	// sensible starting points (before initialization of EM)
	std::vector<PrecisionType> getStartValues() const override;

	// how to optimize?
	bool optimizeWithNelderMead() const override;

	bool optimizeWithLineSearch() const override;

	// transform values (for optimization, e.g. update in log for Nelder-Mead)
	std::vector<PrecisionType> transformToNelderMeadSpace(coretools::TConstView<PrecisionType> Values) override;

	std::vector<PrecisionType> transformFromNelderMeadSpace(coretools::TConstView<PrecisionType> Values) override;

	// getters
	size_t numParameters() const override;

	void fillHeaderEMReportFile(std::vector<std::string> &Header) const override;
};

template<typename PrecisionType, typename NumStatesType>
class TTransitionMatrixScaledLadderAttractorShift : public TTransitionMatrixDistBase<PrecisionType, NumStatesType> {
protected:
	// parametrized by generating matrix
	TGeneratingMatrixScaledLadderAttractorShift<PrecisionType, NumStatesType> _generatingMatrix;
	using TTransitionMatrixDistBase<PrecisionType, NumStatesType>::_transitionMatrix;

	bool _isValid(coretools::TConstView<PrecisionType> Values) const;

public:
	TTransitionMatrixScaledLadderAttractorShift();
	TTransitionMatrixScaledLadderAttractorShift(NumStatesType NumStates);

	~TTransitionMatrixScaledLadderAttractorShift() override = default;

	void resize(NumStatesType NumStates) override;

	// set index of attractor row
	void setIxAttractor(size_t Ix);

	// fill
	void fillTransitionMatrixFromStartValues() override;
	bool fillTransitionMatrix(coretools::TConstView<PrecisionType> Values) override;

	// sensible starting points (before initialization of EM)
	std::vector<PrecisionType> getStartValues() const override;

	// how to optimize?
	bool optimizeWithNelderMead() const override;

	bool optimizeWithLineSearch() const override;

	// transform values (for optimization, e.g. update in log for Nelder-Mead)
	std::vector<PrecisionType> transformToNelderMeadSpace(coretools::TConstView<PrecisionType> Values) override;

	std::vector<PrecisionType> transformFromNelderMeadSpace(coretools::TConstView<PrecisionType> Values) override;

	// getters
	size_t numParameters() const override;

	size_t ix() const;

	void fillHeaderEMReportFile(std::vector<std::string> &Header) const override;
};

template<typename PrecisionType, typename NumStatesType>
class TTransitionMatrixScaledLadderAttractorShift2 : public TTransitionMatrixDistBase<PrecisionType, NumStatesType> {
protected:
	// parametrized by generating matrix
	TGeneratingMatrixScaledLadderAttractorShift2<PrecisionType, NumStatesType> _generatingMatrix;
	using TTransitionMatrixDistBase<PrecisionType, NumStatesType>::_transitionMatrix;

	bool _isValid(coretools::TConstView<PrecisionType> Values) const;

public:
	TTransitionMatrixScaledLadderAttractorShift2();
	TTransitionMatrixScaledLadderAttractorShift2(NumStatesType NumStates);

	~TTransitionMatrixScaledLadderAttractorShift2() override = default;

	void resize(NumStatesType NumStates) override;

	// set index of attractor row
	void setIxAttractor(size_t Ix);

	// fill
	void fillTransitionMatrixFromStartValues() override;
	bool fillTransitionMatrix(coretools::TConstView<PrecisionType> Values) override;

	// sensible starting points (before initialization of EM)
	std::vector<PrecisionType> getStartValues() const override;

	// how to optimize?
	bool optimizeWithNelderMead() const override;

	bool optimizeWithLineSearch() const override;

	// transform values (for optimization, e.g. update in log for Nelder-Mead)
	std::vector<PrecisionType> transformToNelderMeadSpace(coretools::TConstView<PrecisionType> Values) override;

	std::vector<PrecisionType> transformFromNelderMeadSpace(coretools::TConstView<PrecisionType> Values) override;

	// getters
	size_t numParameters() const override;

	size_t ix() const;

	void fillHeaderEMReportFile(std::vector<std::string> &Header) const override;
};

}; // end namespace stattools

#include "TTransitionMatrixDist.tpp"

#endif // TTRANSITIONMATRIXDIST_H
