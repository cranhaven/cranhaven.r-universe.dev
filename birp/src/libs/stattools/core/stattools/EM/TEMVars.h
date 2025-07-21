//
// Created by madleina on 13.01.21.
//

#ifndef TEMVARS_H
#define TEMVARS_H

#include "stattools/EM/TDataVector.h"
#include "stattools/EM/TEMPriorIndependent.h"

namespace stattools {

//-------------------------------------
// THMMEmission
//-------------------------------------
template<typename PrecisionType, typename NumStatesType> using THMMEmission = TDataVector<PrecisionType, NumStatesType>;

//-------------------------------------
// TEMWeights
//-------------------------------------
template<typename PrecisionType, typename NumStatesType, typename LengthType> class TEMWeights {
	// equivalent class to THMMForwardBackward, but only for "simple" EM Weights (without HMM)
private:
	// weights
	TDataVector<PrecisionType, NumStatesType> _weights;

	// log likelihood
	PrecisionType _LL;

protected:
	// loop
	void _moveCalculationsForward(LengthType Index, size_t Rep,
								  const TEMPriorIndependent_baseWithRep<PrecisionType, NumStatesType, LengthType> &EMPrior,
								  const THMMEmission<PrecisionType, NumStatesType> &EmissionProbs) {
		// calculate weights at current Index: p_gi = (Emission_gi * Prior_gi) / (sum_h Emission_hi * Prior_hi)
		PrecisionType sumToNormalize  = 0.0;
		const NumStatesType numStates = _weights.size();
		for (NumStatesType i = 0; i < numStates; ++i) {
			_weights[i] = EmissionProbs[i] * EMPrior(Index, Rep, i);
			sumToNormalize += _weights[i];
		}

		// normalize
		_weights.normalize(sumToNormalize);

		// add to LL
		_LL += log(sumToNormalize);
	};

	void _startCalculations(size_t Rep,
							const TEMPriorIndependent_baseWithRep<PrecisionType, NumStatesType, LengthType> &EMPrior,
							const THMMEmission<PrecisionType, NumStatesType> &EmissionProbs) {
		_LL = 0.0;
		_moveCalculationsForward(0, Rep, EMPrior, EmissionProbs);
	};

public:
	TEMWeights() { _LL = 0.0; };

	~TEMWeights() = default;

	PrecisionType LL() const { return _LL; };

	void startCalculationsForwards(size_t Rep,
								   const TEMPriorIndependent_baseWithRep<PrecisionType, NumStatesType, LengthType> &EMPrior,
								   const THMMEmission<PrecisionType, NumStatesType> &EmissionProbs) {
		// ensure size
		_weights.resize(EMPrior.numStates());

		// start!
		_startCalculations(Rep, EMPrior, EmissionProbs);
	};

	void moveCalculationsForward(LengthType Index, size_t Rep,
								 const TEMPriorIndependent_baseWithRep<PrecisionType, NumStatesType, LengthType> &EMPrior,
								 const THMMEmission<PrecisionType, NumStatesType> &EmissionProbs) {
		_moveCalculationsForward(Index, Rep, EMPrior, EmissionProbs);
	};

	const TDataVector<PrecisionType, NumStatesType> &weights() const { return _weights; };
};

}; // end namespace stattools

#endif // TEMVARS_H
