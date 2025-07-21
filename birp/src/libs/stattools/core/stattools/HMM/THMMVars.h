/*
 * THMM.h
 *
 *  Created on: Dec 4, 2020
 *      Author: phaentu
 */

#ifndef THMMVars_H_
#define THMMVars_H_

#include "stattools/EM/TDataVector.h"
#include "stattools/HMM/TTransitionMatrix.h"

namespace stattools {

//-------------------------------------
// THMMEmission
//-------------------------------------
template<typename PrecisionType, typename NumStatesType> using THMMEmission = TDataVector<PrecisionType, NumStatesType>;

//-------------------------------------
// THMMForwardAlpha
//-------------------------------------
template<typename PrecisionType, typename NumStatesType, typename LengthType>
class THMMForwardAlpha final : public TDataVectorPair<PrecisionType, NumStatesType> {
	// forward alpha inherits from TDataVectorPair (i.e. it stores only two alphas, the current and previous one)
	// backward beta inherits from TDataVectorMulti (i.e. it stores the full chain)
	// backward is run first -> fills all beta
	// then, we run forward and calculate gamma and xi on the fly
	// advantage of "backward-forward" approach (compared to classic forward-backward):
	// -> we only need the emission probability at point t to calculate both gamma and xi
	// -> if it was the other way around, we would need the emission at point t+1 to calculate beta_t(z_t) plus the
	// emission at point t to calculate xi -> more complicated to handle!
protected:
	PrecisionType _LL;

	void _startCalculations(size_t Rep, TDataVector<PrecisionType, NumStatesType> &FirstAlpha,
							const TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> &TransMat,
							const THMMEmission<PrecisionType, NumStatesType> &EmissionProbs) {
		_LL = 0.0;

		// calculate initial alpha: initial * emission
		PrecisionType sumToNormalize = 0.0;
		for (NumStatesType i = 0; i < FirstAlpha.size(); ++i) {
			FirstAlpha[i] = EmissionProbs[i] * TransMat.initialProbability(Rep, i);
			sumToNormalize += FirstAlpha[i];
		}

		// normalize
		FirstAlpha.normalize(sumToNormalize);

		// add to LL
		_LL += log(sumToNormalize);
	};

	void
	_moveCalculationsForward(TDataVector<PrecisionType, NumStatesType> &CurrentAlpha,
							 const TDataVector<PrecisionType, NumStatesType> PreviousAlpha, LengthType Index,
							 size_t Rep,
							 const TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> &TransMat,
							 const THMMEmission<PrecisionType, NumStatesType> &EmissionProbs) {
		// calculate forward iteration: alpha_i(t) = Emission_i(t) * \sum_j alpha_j(t-1) * P(j|i)
		PrecisionType sumToNormalize  = 0.0;
		const NumStatesType numStates = CurrentAlpha.size();
		for (NumStatesType i = 0; i < numStates; ++i) {
			CurrentAlpha[i] = 0.0;
			for (NumStatesType j = 0; j < numStates; ++j) {
				CurrentAlpha[i] += PreviousAlpha[j] * TransMat(Index, Rep, j, i);
			}
			CurrentAlpha[i] *= EmissionProbs[i];
			sumToNormalize += CurrentAlpha[i];
		}

		// normalize
		CurrentAlpha.normalize(sumToNormalize);

		// add to LL
		_LL += log(sumToNormalize);
	};

public:
	THMMForwardAlpha() { _LL = 0.0; };
	THMMForwardAlpha(NumStatesType NumStates) : TDataVectorPair<PrecisionType, NumStatesType>(NumStates) { _LL = 0.0; };

	virtual ~THMMForwardAlpha() = default;

	PrecisionType LL() const { return _LL; };

	void
	startCalculationsForwards(size_t Rep,
							  const TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> &TransMat,
							  const THMMEmission<PrecisionType, NumStatesType> &EmissionProbs) {
		// ensure size
		this->resize(TransMat.numStates());

		// start!
		_startCalculations(Rep, *this, TransMat, EmissionProbs);
	};

	void
	moveCalculationsForward(LengthType Index, size_t Rep,
							const TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> &TransMat,
							const THMMEmission<PrecisionType, NumStatesType> &EmissionProbs) {
		this->swap();
		_moveCalculationsForward(*this, this->other(), Index, Rep, TransMat, EmissionProbs);
	};
};

//-------------------------------------
// THMMBackwardBeta
//-------------------------------------
template<typename PrecisionType, typename NumStatesType, typename LengthType>
class THMMBackwardBeta final : public TDataVectorMulti<PrecisionType, NumStatesType, LengthType> {
protected:
	void _startCalculationsBackwards(TDataVector<PrecisionType, NumStatesType> &LastBeta,
									 const TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> &) {
		// set last beta = 1
		for (NumStatesType i = 0; i < LastBeta.size(); ++i) { LastBeta[i] = 1.0; }
	};

	void
	_moveCalculationsBackward(TDataVector<PrecisionType, NumStatesType> &CurrentBeta,
							  const TDataVector<PrecisionType, NumStatesType> NextBeta, LengthType Index, size_t Rep,
							  const TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> &TransMat,
							  const THMMEmission<PrecisionType, NumStatesType> &NextEmissionProbs) {
		// calculate previous beta: beta_i(t) = \sum_j beta_j(t+1) * P(i | j) * Emission_j(t+1)
		PrecisionType sumToNormalize  = 0.0;
		const NumStatesType numStates = CurrentBeta.size();
		for (NumStatesType i = 0; i < numStates; ++i) {
			CurrentBeta[i] = 0.0;
			for (NumStatesType j = 0; j < numStates; ++j) {
				CurrentBeta[i] += NextBeta[j] * TransMat(Index, Rep, i, j) * NextEmissionProbs[j];
			}
			sumToNormalize += CurrentBeta[i];
		}

		// normalize
		CurrentBeta.normalize(sumToNormalize);
	};

public:
	THMMBackwardBeta() = default;
	THMMBackwardBeta(NumStatesType NumStates, LengthType Length)
		: TDataVectorMulti<PrecisionType, NumStatesType, LengthType>(NumStates, Length){};
	virtual ~THMMBackwardBeta() = default;

	using TDataVectorMulti<PrecisionType, NumStatesType, LengthType>::resize;

	void startCalculationsBackwards(
		const TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> &TransMat) {
		if (!this->startBackwards()) {
			throw coretools::TDevError("failed to start, did you forget to initialize the length of the container?");
		}

		// calculate last beta
		_startCalculationsBackwards(*this, TransMat);
	};

	void
	moveCalculationsBackward(LengthType Index, size_t Rep,
							 const TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> &TransMat,
							 const THMMEmission<PrecisionType, NumStatesType> &NextEmissionProbs) {
		if (!this->moveBackward()) { throw coretools::TDevError("moved beyond beginning of container!"); }

		// calculate forward iteration
		_moveCalculationsBackward(*this, this->next(), Index, Rep, TransMat, NextEmissionProbs);
	};
};

//-------------------------------------
// THMMPosteriorGamma
//-------------------------------------
template<typename PrecisionType, typename NumStatesType, typename LengthType>
class THMMPosteriorGamma final : public TDataVector<PrecisionType, NumStatesType> {
public:
	THMMPosteriorGamma(){};
	THMMPosteriorGamma(const TDataVector<PrecisionType, NumStatesType> &Alpha,
					   const TDataVector<PrecisionType, NumStatesType> &Beta) {
		update(Alpha, Beta);
	};
	~THMMPosteriorGamma(){};

	void update(const TDataVector<PrecisionType, NumStatesType> &Alpha,
				const TDataVector<PrecisionType, NumStatesType> &Beta) {
		// ensure container sizes match
		if (Alpha.size() != Beta.size()) { throw coretools::TDevError("Alpha and Beta are of different size!"); }
		this->resize(Alpha.size());

		// calculate gamma_t \propto alpha_t * beta_t
		PrecisionType sumToNormalize = 0.0;
		for (NumStatesType i = 0; i < Alpha.size(); ++i) {
			this->_current[i] = Alpha[i] * Beta[i];
			sumToNormalize += this->_current[i];
		}

		// normalize
		this->normalize(sumToNormalize);
	};

	using TDataVector<PrecisionType, NumStatesType>::print;
};

//-------------------------------------
// THMMPosteriorXi
//-------------------------------------
template<typename PrecisionType, typename NumStatesType, typename LengthType>
class THMMPosteriorXi final : public TDataSquareMatrix<PrecisionType, uint32_t> {
	// NOTE: the matrix xi is linearized: index = to * nStates + from
	// Note: linearized index is always uint32_t
public:
	THMMPosteriorXi() = default;

	THMMPosteriorXi(const TDataVector<PrecisionType, NumStatesType> &Alpha,
					const TDataVector<PrecisionType, NumStatesType> &Beta,
					const TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> &TransMat,
					const THMMEmission<PrecisionType, NumStatesType> &EmissionProbs) {
		update(Alpha, Beta, TransMat, EmissionProbs);
	};

	~THMMPosteriorXi() = default;

	void update(const TDataVector<PrecisionType, NumStatesType> &PreviousAlpha,
				const TDataVector<PrecisionType, NumStatesType> &Beta, LengthType Index, size_t Rep,
				const TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> &TransMat,
				const THMMEmission<PrecisionType, NumStatesType> &EmissionProbs) {
		// ensure container size
		if (PreviousAlpha.size() != Beta.size()) { throw coretools::TDevError("Alpha and Beta are of different size!"); }
		this->resize(PreviousAlpha.size());

		// calculate xi_ij(t) \propto alpha_i(t-1) * P(j|i) * beta_j(t) * Emission_j(t)
		uint32_t index               = 0;
		PrecisionType sumToNormalize = 0.0;
		for (NumStatesType j = 0; j < this->size(); ++j) { // z_t
			PrecisionType tmp = Beta[j] * EmissionProbs[j];
			for (NumStatesType i = 0; i < this->size(); ++i) { // z_{t-1}
				this->_current[index] = PreviousAlpha[i] * TransMat(Index, Rep, i, j) * tmp;
				sumToNormalize += this->_current[index];
				++index;
			}
		}

		// normalize
		this->normalize(sumToNormalize);
	};

	using TDataSquareMatrix<PrecisionType, uint32_t>::operator();
};

//-------------------------------------
// THMMForwardBackward
//-------------------------------------
template<typename PrecisionType, typename NumStatesType, typename LengthType> class THMMForwardBackward {
private:
	// HMM variables
	THMMForwardAlpha<PrecisionType, NumStatesType, LengthType> _alpha;
	bool _alphaInitialized;
	THMMBackwardBeta<PrecisionType, NumStatesType, LengthType> _beta;

	// posteriors
	THMMPosteriorGamma<PrecisionType, NumStatesType, LengthType> _gamma;
	bool _calculateGamma;
	bool _updatedGamma;
	THMMPosteriorXi<PrecisionType, NumStatesType, LengthType> _xi;
	bool _calculateXi;
	bool _updatedXi;

public:
	THMMForwardBackward() {
		_alphaInitialized = false;
		_calculateGamma   = false;
		_calculateXi      = false;
		_updatedGamma     = false;
		_updatedXi        = false;
	};

	THMMForwardBackward(bool CalculateGamma, bool CalculateXi) {
		_alphaInitialized = false;
		_calculateGamma   = CalculateGamma;
		_calculateXi      = CalculateXi;
		_updatedGamma     = false;
		_updatedXi        = false;
	};

	~THMMForwardBackward() = default;

	void enableGammaCalculation() { _calculateGamma = true; };
	void enableXiCalculation() { _calculateXi = true; };

	LengthType length() { return _beta.length(); };

	// access beta to run backward
	THMMBackwardBeta<PrecisionType, NumStatesType, LengthType> &beta() {
		_alphaInitialized = false;
		return _beta;
	};
	// access alpha to calculate LL
	THMMForwardAlpha<PrecisionType, NumStatesType, LengthType> &alpha() { return _alpha; };

	// loop forward
	void
	startCalculationsForwards(size_t Rep,
							  const TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> &TransMat,
							  const THMMEmission<PrecisionType, NumStatesType> &EmissionProbs) {
		// ensure size of alpha
		_alpha.resize(TransMat.numStates());

		// start
		_alpha.startCalculationsForwards(Rep, TransMat, EmissionProbs);
		_beta.startForwards();

		// calculate gamma
		if (_calculateGamma) {
			_gamma.update(_alpha, _beta);
			_updatedGamma = true;
		}
		// can not calculate Xi at first
		_updatedXi = false;
	};

	void
	moveCalculationsForward(LengthType Index, size_t Rep,
							const TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> &TransMat,
							const THMMEmission<PrecisionType, NumStatesType> &EmissionProbs) {
		_alpha.moveCalculationsForward(Index, Rep, TransMat, EmissionProbs);
		_beta.moveForward();

		// calculate gamma
		if (_calculateGamma) { _gamma.update(_alpha, _beta); }

		// calculate xi
		if (_calculateXi) {
			_xi.update(_alpha.other(), _beta, Index, Rep, TransMat, EmissionProbs);
			_updatedXi = true;
		}
	};

	// const access
	bool gammaCalculated() const { return _updatedGamma; };
	bool xiCalculated() const { return _updatedXi; };
	const THMMForwardAlpha<PrecisionType, NumStatesType, LengthType> &alpha() const { return _alpha; };
	const THMMBackwardBeta<PrecisionType, NumStatesType, LengthType> &beta() const { return _beta; };
	const THMMPosteriorGamma<PrecisionType, NumStatesType, LengthType> &gamma() const {
		if (!_calculateGamma) { throw coretools::TDevError("gamma is not calculated!"); }
		return _gamma;
	};
	const THMMPosteriorXi<PrecisionType, NumStatesType, LengthType> &xi() const {
		if (!_calculateXi) { throw coretools::TDevError("xi is not calculated!"); }
		if (!_updatedXi) { throw coretools::TDevError("xi was not updated at first index!"); }
		return _xi;
	};
};

}; // end namespace stattools

#endif /* THMM_H_ */
