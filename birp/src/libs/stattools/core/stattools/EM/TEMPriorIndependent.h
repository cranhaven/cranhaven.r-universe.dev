/*
 * TEMPriorIID.h
 *
 *  Created on: Jan 18, 2021
 *      Author: phaentu
 */

#ifndef TEMPRIORIID_H_
#define TEMPRIORIID_H_

#include "stattools/EM/TEMPrior.h"
#include "stattools/EM/TLatentVariable.h"

namespace stattools {

//--------------------------------------------
// TEMPriorIID_base
// A pure virtual class
// IID stands for independent and identically distributed: no Markov property (not an HMM)
// used if there are replicates
//--------------------------------------------

template<typename PrecisionType, typename NumStatesType, typename LengthType>
class TEMPriorIndependent_baseWithRep : public TEMPrior_base<PrecisionType, NumStatesType, LengthType> {
public:
	TEMPriorIndependent_baseWithRep() : TEMPrior_base<PrecisionType, NumStatesType, LengthType>(){};
	TEMPriorIndependent_baseWithRep(NumStatesType NumStates)
		: TEMPrior_base<PrecisionType, NumStatesType, LengthType>(NumStates){};

	virtual ~TEMPriorIndependent_baseWithRep() = default;

	// getters
	virtual PrecisionType operator()(LengthType Index, size_t Rep, NumStatesType State) const = 0;

	// EM initialization
	void initializeEMParameters(const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatVar,
								const std::vector<LengthType> &ChunkEnds, size_t NumReplicates) override {
		// prepare storage
		this->prepareEMParameterEstimationOneIteration();

		LengthType first = 0;
		// go over all chunks
		for (LengthType last : ChunkEnds) {
			for (size_t r = 0; r < NumReplicates; r++) {
				for (LengthType i = first; i < last; ++i) {
					NumStatesType state = LatVar.getMLEmissionState(i, r, this->_numStates);
					handleEMParameterInitialization(i, r, state);
				}
				// update first of next
				first = last;
			}
		}
		this->finalizeEMParameterInitialization();
	};

	// EM initialization: only need current state
	virtual void handleEMParameterInitialization(LengthType /*Index*/, size_t /*Rep*/, NumStatesType /*State*/){};

	// EM update: maximization function gets a TDataVector as argument (= EM weights)
	virtual void
	handleEMParameterEstimationOneIteration(LengthType /*Index*/, size_t /*Rep*/,
											const TDataVector<PrecisionType, NumStatesType> & /*Weights*/){};

	virtual void print(size_t Rep, LengthType Index) {
		coretools::cout << "Probabilities:" << std::endl;
		for (NumStatesType s = 0; s < this->_numStates; ++s) {
			if (s > 0) { coretools::cout << ", "; }
			coretools::cout << operator()(Index, Rep, s);
		}
	};
};

//--------------------------------------------
// TEMPriorIID_base
// A pure virtual class
// used if there are no replicates
//--------------------------------------------

template<typename PrecisionType, typename NumStatesType, typename LengthType>
class TEMPriorIndependent_base : public TEMPriorIndependent_baseWithRep<PrecisionType, NumStatesType, LengthType> {
private:
	using BaseClass = TEMPriorIndependent_baseWithRep<PrecisionType, NumStatesType, LengthType>;

public:
	TEMPriorIndependent_base() : BaseClass(){};
	TEMPriorIndependent_base(NumStatesType NumStates) : BaseClass(NumStates){};

	virtual ~TEMPriorIndependent_base() = default;

	// getters
	virtual PrecisionType operator()(LengthType /*Index*/, NumStatesType State) const = 0;
	PrecisionType operator()(LengthType Index, size_t /*Rep*/, NumStatesType State) const final {
		// this function is called by EM framework and only wraps operator[] without Rep
		return operator()(Index, State);
	};

	// EM initialization
	virtual void initializeEMParameters(const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatVar,
										const std::vector<LengthType> &ChunkEnds) {
		BaseClass::initializeEMParameters(LatVar, ChunkEnds, 1);
	};
	void initializeEMParameters(const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatVar,
								const std::vector<LengthType> &ChunkEnds, size_t /*NumReplicates*/) final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		initializeEMParameters(LatVar, ChunkEnds);
	}

	// EM initialization: only need current state
	virtual void handleEMParameterInitialization(LengthType Index, NumStatesType State) {
		BaseClass::handleEMParameterInitialization(Index, 0, State);
	};
	void handleEMParameterInitialization(LengthType Index, size_t /*Rep*/, NumStatesType State) final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		handleEMParameterInitialization(Index, State);
	};

	// EM update: maximization function gets a TDataVector as argument (= EM weights)
	virtual void handleEMParameterEstimationOneIteration(LengthType Index,
														 const TDataVector<PrecisionType, NumStatesType> &Weights) {
		BaseClass::handleEMParameterEstimationOneIteration(Index, 0, Weights);
	};
	virtual void
	handleEMParameterEstimationOneIteration(LengthType Index, size_t /*Rep*/,
											const TDataVector<PrecisionType, NumStatesType> &Weights) final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		handleEMParameterEstimationOneIteration(Index, Weights);
	};

	virtual void print(LengthType Index) { BaseClass::print(0, Index); };
	void print(size_t /*Rep*/, LengthType Index) final { print(Index); }
};

};     // end namespace stattools

#endif /* TEMPRIORIID_H_ */
