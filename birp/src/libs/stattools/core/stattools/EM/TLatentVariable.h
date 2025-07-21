/*
 * TLatentVariable.h
 *
 *  Created on: Dec 21, 2020
 *      Author: phaentu
 */

#ifndef TLATENTVARIABLE_H_
#define TLATENTVARIABLE_H_

#include "coretools/Containers/TView.h"
#include "coretools/Files/TOutputFile.h"
#include "stattools/EM/TDataVector.h"
#include <numeric>

namespace stattools {

//-------------------------------------
// TLatentVariableWithRep
// A pure virtual class
// used if there are replicates
//-------------------------------------
template<typename PrecisionType, typename NumStatesType, typename LengthType> class TLatentVariableWithRep {
protected:
	size_t _numRep           = 1;
	LengthType _sizeOfSubset = 0;

	void _fillHeaderStatePosteriors(NumStatesType NumStates, std::vector<std::string> &header) {
		for (NumStatesType s = 0; s < NumStates; ++s) { header.push_back("P(S=" + coretools::str::toString(s) + ")"); }
	}

	void _writeStatePosteriors(const TDataVector<PrecisionType, NumStatesType> &StatePosteriorProbabilities,
							   coretools::TOutputFile &out) {
		for (NumStatesType s = 0; s < StatePosteriorProbabilities.size(); ++s) {
			out.write(StatePosteriorProbabilities[s]);
		}
		out.endln();
	}

public:
	TLatentVariableWithRep() = default;
	TLatentVariableWithRep(size_t NumRep) { setNumRep(NumRep); }
	virtual ~TLatentVariableWithRep() = default;

	void setNumRep(size_t NumRep) { _numRep = NumRep; }

	// function to calculate emission probabilities
	virtual void calculateEmissionProbabilities(LengthType Index, size_t Replicate,
												TDataVector<PrecisionType, NumStatesType> &Emission) const = 0;

	virtual std::vector<PrecisionType> getParameters() const {
		throw coretools::TDevError("This function must be implemented when SQUAREM is used!");
	};
	virtual bool setParameters(coretools::TConstView<PrecisionType> /*Params*/) {
		throw coretools::TDevError("This function must be implemented when SQUAREM is used!");
	}

	// EM initialization
	virtual NumStatesType getMLEmissionState(LengthType Index, size_t Rep, NumStatesType NumStates) const {
		TDataVector<PrecisionType, NumStatesType> emission(NumStates);
		calculateEmissionProbabilities(Index, Rep, emission);
		return emission.maxIndex();
	};

	virtual PrecisionType getPosteriorMeanState(LengthType Index, size_t Rep, NumStatesType NumStates) const {
		TDataVector<PrecisionType, NumStatesType> emission(NumStates);
		calculateEmissionProbabilities(Index, Rep, emission);
		const double sum    = std::accumulate(emission.begin(), emission.end(), 0.0);
		double weightedMean = 0.0;
		for (NumStatesType i = 1; i < NumStates; ++i) { weightedMean += (double)i * emission[i] / sum; }
		return weightedMean;
	};

	virtual void initializeEMParameters(){};
	virtual void finalizeEMParameterInitialization(){};

	// EM estimation.
	// These functions are not required as THMM may be used to calculate posterior distributions only.
	// Note: to estimate the transition matrix only, these functions can stay "empty"
	virtual void prepareEMParameterEstimationInitial(){};
	virtual void prepareEMParameterEstimationOneIteration(){};
	virtual void
	handleEMParameterEstimationOneIteration(LengthType /*Index*/, size_t /*Rep*/,
											const TDataVector<PrecisionType, NumStatesType> & /*Weights*/){};
	virtual void finalizeEMParameterEstimationOneIteration(){};
	virtual void finalizeEMParameterEstimationFinal(){};
	virtual void reportEMParameters(){};

	// Calculate LL.
	virtual void prepareLLCalculation(NumStatesType /*State*/){};
	virtual void finalizeLLCalculation(){};

	// Handling posterior estimates.
	virtual void prepareStatePosteriorEstimation(NumStatesType /*State*/){};
	virtual void handleStatePosteriorEstimation(LengthType /*Index*/, size_t /*Rep*/,
												const TDataVector<PrecisionType, NumStatesType> & /*Weights*/){};
	virtual void finalizeStatePosteriorEstimation(){};

	// Write posterior estimates.
	virtual void prepareStatePosteriorEstimationAndWriting(NumStatesType NumStates, std::vector<std::string> &header) {
		// fill header
		header.clear();
		header = {"Index", "Rep"};
		_fillHeaderStatePosteriors(NumStates, header);
	};

	virtual void handleStatePosteriorEstimationAndWriting(
		LengthType Index, size_t Replicate,
		const TDataVector<PrecisionType, NumStatesType> &StatePosteriorProbabilities, coretools::TOutputFile &out) {

		out.write(Index, Replicate);
		_writeStatePosteriors(StatePosteriorProbabilities, out);
	};
	virtual void finalizeStatePosteriorEstimationAndWriting(){};

	// number of replicates
	size_t numRep() const { return _numRep; }

	// running EM on subset -> latent variable should know this (in some cases)
	void setSizeOfSubset(size_t Size) { _sizeOfSubset = Size; }
	LengthType getSizeOfSubset() const { return _sizeOfSubset; }
};

//-------------------------------------
// TLatentVariable
// A pure virtual class
// used if there are no replicates
//-------------------------------------
template<typename PrecisionType, typename NumStatesType, typename LengthType>
class TLatentVariable : public TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> {
private:
	using BaseClass = TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType>;

public:
	TLatentVariable() : TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType>(1){};
	~TLatentVariable() override = default;

	// function to calculate emission probabilities
	virtual void calculateEmissionProbabilities(LengthType Index,
												TDataVector<PrecisionType, NumStatesType> &Emission) const = 0;
	void calculateEmissionProbabilities(LengthType Index, size_t /*Rep*/,
										TDataVector<PrecisionType, NumStatesType> &Emission) const final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		calculateEmissionProbabilities(Index, Emission);
	};

	// EM initialization
	virtual NumStatesType getMLEmissionState(LengthType Index, NumStatesType NumStates) const {
		return BaseClass::getMLEmissionState(Index, 0, NumStates);
	};
	NumStatesType getMLEmissionState(LengthType Index, size_t /*Rep*/, NumStatesType NumStates) const final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		return getMLEmissionState(Index, NumStates);
	};

	virtual PrecisionType getPosteriorMeanState(LengthType Index, NumStatesType NumStates) const {
		return BaseClass::getPosteriorMeanState(Index, 0, NumStates);
	};
	PrecisionType getPosteriorMeanState(LengthType Index, size_t /*Rep*/, NumStatesType NumStates) const final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		return getPosteriorMeanState(Index, NumStates);
	};

	// EM estimation.
	// These functions are not required as THMM may be used to calculate posterior distributions only.
	// Note: to estimate the transition matrix only, these functions can stay "empty"
	virtual void handleEMParameterEstimationOneIteration(LengthType Index,
														 const TDataVector<PrecisionType, NumStatesType> &Weights) {
		BaseClass::handleEMParameterEstimationOneIteration(Index, 0, Weights);
	};
	void handleEMParameterEstimationOneIteration(LengthType Index, size_t /*Rep*/,
												 const TDataVector<PrecisionType, NumStatesType> &Weights) final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		handleEMParameterEstimationOneIteration(Index, Weights);
	};

	virtual void handleStatePosteriorEstimation(LengthType Index,
												const TDataVector<PrecisionType, NumStatesType> &Weights) {
		BaseClass::handleStatePosteriorEstimation(Index, 0, Weights);
	};
	void handleStatePosteriorEstimation(LengthType Index, size_t /*Rep*/,
										const TDataVector<PrecisionType, NumStatesType> &Weights) final {
		// this function is called by EM framework and only wraps overloaded function without Rep
		handleStatePosteriorEstimation(Index, Weights);
	};

	// Write posterior estimates.
	void prepareStatePosteriorEstimationAndWriting(NumStatesType NumStates, std::vector<std::string> &header) override {
		// fill header
		header.clear();
		header = {"Index"};
		this->_fillHeaderStatePosteriors(NumStates, header);
	};

	virtual void handleStatePosteriorEstimationAndWriting(
		LengthType Index, const TDataVector<PrecisionType, NumStatesType> &StatePosteriorProbabilities,
		coretools::TOutputFile &out) {
		out.write(Index);
		this->_writeStatePosteriors(StatePosteriorProbabilities, out);
	};
	void handleStatePosteriorEstimationAndWriting(
		LengthType Index, size_t /*Rep*/, const TDataVector<PrecisionType, NumStatesType> &StatePosteriorProbabilities,
		coretools::TOutputFile &out) final {
		handleStatePosteriorEstimationAndWriting(Index, StatePosteriorProbabilities, out);
	};
};

}; // end namespace stattools

#endif /* TLATENTVARIABLE_H_ */
