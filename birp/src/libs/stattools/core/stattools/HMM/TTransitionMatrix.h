/*
 * TTransitionMatrix.h
 *
 *  Created on: Jan 18, 2021
 *      Author: phaentu
 */

#ifndef TTRANSITIONMATRIX_H_
#define TTRANSITIONMATRIX_H_

#include "coretools/Main/TRandomGenerator.h"
#include "stattools/EM/TEMPrior.h"
#include "stattools/EM/TLatentVariable.h"
#include <cmath>
#include <vector>

namespace stattools {

// forward declare THMMPosteriorXi
template<typename PrecisionType, typename NumStatesType, typename LengthType> class THMMPosteriorXi;

// HMM initialization
enum class TypeTransMatInitialization { none, ML, defaultValues, smart };

//--------------------------------------------
// TTransitionMatrix_baseWithRep
// A pure virtual class
// used if there are replicates
//--------------------------------------------
template<typename PrecisionType, typename NumStatesType, typename LengthType>
class TTransitionMatrix_baseWithRep : public TEMPrior_base<PrecisionType, NumStatesType, LengthType> {
protected:
	std::vector<TDataVector<PrecisionType, NumStatesType>> _initialProbabilities;     // per replicate and per state
	std::vector<TDataVector<PrecisionType, NumStatesType>> _initialProbabilities_tmp; // used during EM estimation
	LengthType _numChunksUsedForUpdate;

	size_t _numReplicates = 0;

	TypeTransMatInitialization _initializationType = TypeTransMatInitialization::ML;

	void _init() {
		_initialProbabilities.resize(_numReplicates);
		for (auto &r : _initialProbabilities) {
			r.resize(this->_numStates);
			r.set(1.0 / (PrecisionType)this->_numStates);
		}
	};

public:
	TTransitionMatrix_baseWithRep(size_t NumReplicates)
	    : TEMPrior_base<PrecisionType, NumStatesType, LengthType>(), _numReplicates(NumReplicates){};
	TTransitionMatrix_baseWithRep(size_t NumReplicates, NumStatesType NumStates)
	    : TEMPrior_base<PrecisionType, NumStatesType, LengthType>(NumStates), _numReplicates(NumReplicates) {
		_init();
	};
	~TTransitionMatrix_baseWithRep() override = default;

	// initialization strategies
	virtual void setInitialization(TypeTransMatInitialization InitType) { _initializationType = InitType; }
	virtual TypeTransMatInitialization initializationType() const { return _initializationType; }

	// getters
	virtual PrecisionType operator()(LengthType Index, size_t Rep, NumStatesType From, NumStatesType To) const = 0;
	virtual PrecisionType stationary(size_t Rep, NumStatesType State) const                                    = 0;
	virtual PrecisionType initialProbability(size_t Rep, NumStatesType State) const {
		return _initialProbabilities[Rep][State];
	};
	virtual const TDataVector<PrecisionType, NumStatesType> &initialProbabilities(size_t Rep) const {
		return _initialProbabilities[Rep];
	};

	virtual void startNewChunk(size_t /*Index*/, size_t /*Rep*/){
	    // can stay empty if there is nothing to do if new chunk starts
	};

	// EM initialization (functions can stay empty if parameters should not be initialized prior to EM)
	void initializeEMParameters(const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatentVariable,
	                            const std::vector<LengthType> &ChunkEnds, size_t NumReplicates) override {
		// prepare storage
		prepareEMParameterEstimationOneIteration();

		LengthType first = 0;
		NumStatesType previousState;

		// go over all junks
		for (LengthType last : ChunkEnds) {
			for (size_t r = 0; r < NumReplicates; r++) {
				// first element in junk: initial distribution
				previousState = LatentVariable.getMLEmissionState(0, r, this->_numStates);
				handleEMParameterInitializationInitialDistribution(r, previousState);

				// go over all other elements in one junk: transition probabilities
				for (LengthType i = first + 1; i < last; ++i) {
					NumStatesType state = LatentVariable.getMLEmissionState(i, r, this->_numStates);
					handleEMParameterInitializationTransitionProbabilities(i, r, previousState, state);
					previousState = state;
				}
				// update first of next
				first = last;
			}
		}
		finalizeEMParameterInitialization();
	};

	virtual void handleEMParameterInitializationInitialDistribution(size_t Rep, NumStatesType State) {
		_initialProbabilities_tmp[Rep][State]++;
		if (Rep == 0) {
			_numChunksUsedForUpdate++;
		} // only count for first rep (assuming all reps have the same chunks)
	};

	virtual void handleEMParameterInitializationTransitionProbabilities(LengthType /*Index*/, size_t /*Rep*/,
	                                                                    NumStatesType /*From*/, NumStatesType /*To*/){};

	void finalizeEMParameterInitialization() override {
		finalizeEMParameterInitializationTransitionProbabilities();
		finalizeEMParameterInitializationInitialDistribution();
	};

	virtual void finalizeEMParameterInitializationTransitionProbabilities(){};
	virtual void finalizeEMParameterInitializationInitialDistribution() {
		for (size_t r = 0; r < _numReplicates; r++) {
			for (NumStatesType s = 0; s < this->_numStates; ++s) {
				_initialProbabilities[r][s] = (_initialProbabilities_tmp[r][s] + 1) /
				                              (PrecisionType)(_numChunksUsedForUpdate + this->numStates());
			}
		}
	};

	// prepare EM
	void prepareEMParameterEstimationOneIteration() override {
		prepareEMParameterEstimationOneIterationInitialDistribution();
		prepareEMParameterEstimationOneIterationTransitionProbabilities();
	};

	virtual void prepareEMParameterEstimationOneIterationInitialDistribution() {
		_initialProbabilities_tmp.resize(_numReplicates);
		for (auto &r : _initialProbabilities_tmp) {
			r.resize(this->_numStates);
			r.set(0.0);
		}
		_numChunksUsedForUpdate = 0;
	};

	virtual void prepareEMParameterEstimationOneIterationTransitionProbabilities(){};

	// handle EM
	// for initial distribution: maximization function gets a TDataVector rather than THMMPosteriorGamma as argument to
	// avoid circular header inclusion.
	virtual void
	handleEMParameterEstimationOneIterationInitialDistribution(LengthType /*Index*/, size_t Rep,
	                                                           const TDataVector<PrecisionType, NumStatesType> &Gamma) {
		for (NumStatesType s = 0; s < this->_numStates; ++s) { _initialProbabilities_tmp[Rep][s] += Gamma[s]; }
		if (Rep == 0) {
			++_numChunksUsedForUpdate;
		} // only count for first rep (assuming all reps have the same chunks)
	};

	// EM update: maximization function gets a THMMPosteriorXi as argument
	virtual void handleEMParameterEstimationOneIterationTransitionProbabilities(
	    LengthType /*Index*/, size_t /*Rep*/,
	    const THMMPosteriorXi<PrecisionType, NumStatesType, LengthType> & /*Xi*/){};

	// finalize EM
	void finalizeEMParameterEstimationOneIteration() override {
		finalizeEMParameterEstimationOneIterationInitialDistribution();
		finalizeEMParameterEstimationOneIterationTransitionProbabilities();
	};

	virtual void finalizeEMParameterEstimationOneIterationInitialDistribution() {
		for (size_t r = 0; r < _numReplicates; r++) {
			for (NumStatesType s = 0; s < this->_numStates; ++s) {
				_initialProbabilities[r][s] = _initialProbabilities_tmp[r][s] / (PrecisionType)_numChunksUsedForUpdate;
			}
		}
	};

	virtual void finalizeEMParameterEstimationOneIterationTransitionProbabilities(){};

	// Simulate
	virtual NumStatesType sampleFromStationary(size_t Rep) const {
		double random   = coretools::instances::randomGenerator().getRand();
		NumStatesType s = 0;
		double cumul    = stationary(Rep, s);

		while (cumul <= random) {
			++s;
			cumul += stationary(Rep, s);
		}

		return s;
	};

	virtual NumStatesType sampleNextState(LengthType Index, size_t Rep, NumStatesType State) const {
		double random   = coretools::instances::randomGenerator().getRand();
		NumStatesType s = 0;
		double cumul    = operator()(Index, Rep, State, s);

		while (cumul <= random) {
			++s;
			cumul += operator()(Index, Rep, State, s);
		}

		return s;
	};

	template<typename Container> void simulateStates(Container &Data, size_t Rep) {
		size_t previousState = 0;
		for (size_t i = 0; i < Data.size(); ++i) {
			size_t currentState = sampleNextState(i, Rep, previousState);
			Data[i]             = currentState;
			previousState       = currentState;
		}
	}

	// output
	virtual void print(LengthType Index, size_t Rep) {
		coretools::cout << "Transition matrix:" << std::endl;
		for (NumStatesType s = 0; s < this->_numStates; ++s) {
			for (NumStatesType t = 0; t < this->_numStates; ++t) {
				if (t > 0) { coretools::cout << ", "; }
				coretools::cout << operator()(Index, Rep, s, t);
			}
			coretools::cout << std::endl;
		}
	};
};

//--------------------------------------------
// TTransitionMatrix_base
// A pure virtual class
// used if there are no replicates
//--------------------------------------------
template<typename PrecisionType, typename NumStatesType, typename LengthType>
class TTransitionMatrix_base : public TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType> {
private:
	using BaseClass = TTransitionMatrix_baseWithRep<PrecisionType, NumStatesType, LengthType>;

public:
	TTransitionMatrix_base() : BaseClass(1){};
	TTransitionMatrix_base(NumStatesType NumStates) : BaseClass(1, NumStates){};
	~TTransitionMatrix_base() override = default;

	// getters
	virtual PrecisionType operator()(LengthType Index, NumStatesType From, NumStatesType To) const = 0;
	PrecisionType operator()(LengthType Index, size_t /*Rep*/, NumStatesType From, NumStatesType To) const final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		return operator()(Index, From, To);
	};

	virtual PrecisionType stationary(NumStatesType State) const = 0;
	PrecisionType stationary(size_t /*Rep*/, NumStatesType State) const final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		return stationary(State);
	}

	virtual PrecisionType initialProbability(NumStatesType State) const {
		return BaseClass::initialProbability(0, State);
	};
	PrecisionType initialProbability(size_t /*Rep*/, NumStatesType State) const final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		return initialProbability(State);
	};

	virtual const TDataVector<PrecisionType, NumStatesType> &initialProbabilities() const {
		return BaseClass::initialProbabilities(0);
	};
	const TDataVector<PrecisionType, NumStatesType> &initialProbabilities(size_t /*Rep*/) const final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		return initialProbabilities();
	};

	virtual void startNewChunk(size_t Chunk) { BaseClass::startNewChunk(Chunk, 0); };
	void startNewChunk(size_t Chunk, size_t /*Rep*/) final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		startNewChunk(Chunk);
	};

	// EM initialization (functions can stay empty if parameters should not be initialized prior to EM)
	virtual void
	initializeEMParameters(const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatentVariable,
	                       const std::vector<LengthType> &ChunkEnds) {
		BaseClass::initializeEMParameters(LatentVariable, ChunkEnds, 1);
	};
	void initializeEMParameters(const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatentVariable,
	                            const std::vector<LengthType> &ChunkEnds, size_t /*NumReplicates*/) final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		initializeEMParameters(LatentVariable, ChunkEnds);
	};

	virtual void handleEMParameterInitializationInitialDistribution(NumStatesType State) {
		BaseClass::handleEMParameterInitializationInitialDistribution(0, State);
	};
	void handleEMParameterInitializationInitialDistribution(size_t /*Rep*/, NumStatesType State) final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		handleEMParameterInitializationInitialDistribution(State);
	};

	virtual void handleEMParameterInitializationTransitionProbabilities(LengthType Index, NumStatesType From,
	                                                                    NumStatesType To) {
		BaseClass::handleEMParameterInitializationTransitionProbabilities(Index, 0, From, To);
	};
	void handleEMParameterInitializationTransitionProbabilities(LengthType Index, size_t /*Rep*/, NumStatesType From,
	                                                            NumStatesType To) final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		handleEMParameterInitializationTransitionProbabilities(Index, From, To);
	};

	// handle EM
	// for initial distribution: maximization function gets a TDataVector rather than THMMPosteriorGamma as argument to
	// avoid circular header inclusion.
	virtual void
	handleEMParameterEstimationOneIterationInitialDistribution(LengthType Index,
	                                                           const TDataVector<PrecisionType, NumStatesType> &Gamma) {
		BaseClass::handleEMParameterEstimationOneIterationInitialDistribution(Index, 0, Gamma);
	};
	void handleEMParameterEstimationOneIterationInitialDistribution(
	    LengthType Index, size_t /*Rep*/, const TDataVector<PrecisionType, NumStatesType> &Gamma) final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		handleEMParameterEstimationOneIterationInitialDistribution(Index, Gamma);
	};

	// EM update: maximization function gets a THMMPosteriorXi as argument
	virtual void handleEMParameterEstimationOneIterationTransitionProbabilities(
	    LengthType Index, const THMMPosteriorXi<PrecisionType, NumStatesType, LengthType> &Xi) {
		BaseClass::handleEMParameterEstimationOneIterationTransitionProbabilities(Index, 0, Xi);
	};
	void handleEMParameterEstimationOneIterationTransitionProbabilities(
	    LengthType Index, size_t /*Rep*/, const THMMPosteriorXi<PrecisionType, NumStatesType, LengthType> &Xi) final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		handleEMParameterEstimationOneIterationTransitionProbabilities(Index, Xi);
	};

	// Simulate
	virtual NumStatesType sampleFromStationary() const { return BaseClass::sampleFromStationary(0); };
	NumStatesType sampleFromStationary(size_t /*Rep*/) const final { return sampleFromStationary(); }

	virtual NumStatesType sampleNextState(LengthType Index, NumStatesType State) const {
		return BaseClass::sampleNextState(Index, 0, State);
	}
	NumStatesType sampleNextState(LengthType Index, size_t /*Rep*/, NumStatesType State) const final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		return sampleNextState(Index, State);
	}

	template<typename Container> void simulateStates(Container &Data) { BaseClass::simulateStates(Data, 0); }

	// output
	void print(LengthType Index) { BaseClass::print(Index, 0); }
	void print(LengthType Index, size_t /*Rep*/) final { print(Index); }
};

}; // end namespace stattools

#endif /* TTRANSITIONMATRIX_H_ */
