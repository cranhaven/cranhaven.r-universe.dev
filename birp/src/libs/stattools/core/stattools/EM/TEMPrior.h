//
// Created by madleina on 13.01.21.
//

#ifndef TEMPRIOR_H
#define TEMPRIOR_H

#include "coretools/Main/TError.h"
#include "coretools/Containers/TView.h"
#include <cmath>
#include <vector>

namespace stattools {

// forward declare TLatentVariableWithRep
template<typename PrecisionType, typename NumStatesType, typename LengthType> class TLatentVariableWithRep;

//--------------------------------------------
// TEMPrior_base
// A pure virtual class
// Provides a minimal interface for standard EM priors and HMM transition matrices
//--------------------------------------------

template<typename PrecisionType, typename NumStatesType, typename LengthType> class TEMPrior_base {
protected:
	NumStatesType _numStates = 0;

public:
	TEMPrior_base() = default;
	TEMPrior_base(NumStatesType NumStates) { _numStates = NumStates; };

	virtual ~TEMPrior_base() = 0;

	// getters
	NumStatesType numStates() const { return _numStates; };

	virtual std::vector<PrecisionType> getParameters() const {
		throw coretools::TDevError("This function must be implemented when SQUAREM is used!");
	};
	virtual bool setParameters(coretools::TConstView<PrecisionType> /*Params*/) {
		throw coretools::TDevError("This function must be implemented when SQUAREM is used!");
	};

	// EM initialization (functions can stay empty if parameters should not be initialized prior to EM)
	virtual void
	initializeEMParameters(const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> & /*LatVar*/,
						   const std::vector<LengthType> & /*ChunkEnds*/, size_t /*NumReplicates*/){};
	// handleEMParameterInitialization is specific to derived class
	virtual void finalizeEMParameterInitialization(){};

	// EM update
	virtual void prepareEMParameterEstimationInitial(){};
	virtual void prepareEMParameterEstimationOneIteration(){};

	// handleEMParameterEstimationOneIteration is specific to derived class (take different arguments)
	virtual void finalizeEMParameterEstimationOneIteration(){};
	virtual void finalizeEMParameterEstimationFinal(){};
	virtual void reportEMParameters(){};

	// output
	virtual void addStateHeader(std::vector<std::string> &header) {
		for (NumStatesType s = 0; s < _numStates; ++s) { header.push_back("State_" + coretools::str::toString(s + 1)); }
	};
};

template<typename PrecisionType, typename NumStatesType, typename LengthType>
TEMPrior_base<PrecisionType, NumStatesType, LengthType>::~TEMPrior_base() = default;

}; // end namespace stattools

#endif // TEMPRIOR_H
