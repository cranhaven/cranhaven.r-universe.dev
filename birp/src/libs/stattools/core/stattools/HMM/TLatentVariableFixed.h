//
// Created by madleina on 12.02.24.
//

#ifndef TREESWIRL_TLATENTVARIABLEFIXED_H
#define TREESWIRL_TLATENTVARIABLEFIXED_H

#include <array>

#include "coretools/algorithms.h"
#include "stattools/EM/TDataVector.h"
#include "stattools/EM/TLatentVariable.h"

//-------------------------------------
// TLatentVariableFixed
//-------------------------------------

namespace stattools {

template<typename PrecisionType, typename NumStatesType, typename LengthType, typename ProbType>
class TLatentVariableFixed : public TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> {
	// Note: ProbType is likely one of genometools PhredTypes (e.g. genometools::HighPrecisionPhredIntProbability)
protected:
	// number of states, total size and number of replicates
	size_t _numStates = 0;
	size_t _size      = 0;
	size_t _numRep    = 0;
	std::array<size_t, 3> _dim;

	// store the emission probabilities of other latent variable
	std::vector<ProbType> _emissionProbs;

	void _fillEmissions(const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> *Other) {
		_emissionProbs.reserve(_numRep * _size * _numStates);

		for (size_t rep = 0; rep < _numRep; ++rep) {
			for (size_t i = 0; i < _size; ++i) {
				// fill emission probabilities
				stattools::TDataVector<PrecisionType, NumStatesType> tmp_emissions(_numStates);
				Other->calculateEmissionProbabilities(i, rep, tmp_emissions);

				// divide by max and store as HighPrecisionPhredIntProbability
				const auto max = tmp_emissions.max();
				for (size_t j = 0; j < _numStates; ++j) {
					const coretools::Probability emission = coretools::P(tmp_emissions[j] / max);
					_emissionProbs.emplace_back(emission);
				}
			}
		}
	}

public:
	TLatentVariableFixed() = default;

	TLatentVariableFixed(size_t NumStates, size_t Size,
						 const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> *OtherLatentVariable)
		: TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType>() {
		initialize(NumStates, Size, OtherLatentVariable);
	}

	~TLatentVariableFixed() override = default;

	void initialize(size_t NumStates, size_t Size,
					const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> *OtherLatentVariable) {
		using namespace coretools::instances;

		_numStates = NumStates;
		_size      = Size;
		_numRep    = OtherLatentVariable->numRep();
		_dim       = {_numRep, _size, _numStates};

		// check if valid
		if (_numStates == 0) { DEVERROR("Number of states can not be zero!"); }
		if (_numRep == 0) { DEVERROR("Number of replicates can not be zero!"); }

		_fillEmissions(OtherLatentVariable);
	}

	std::vector<PrecisionType> getParameters() const { return {}; };
	bool setParameters(coretools::TConstView<PrecisionType>){ return true; };

	void calculateEmissionProbabilities(LengthType Index, size_t Replicate,
										TDataVector<PrecisionType, NumStatesType> &Emission) const override {
		// just return stored emissions (without phred)
		const size_t linear = coretools::getLinearIndex({Replicate, Index, 0}, _dim);
		for (size_t j = 0; j < _numStates; ++j) { Emission[j] = (coretools::Probability)_emissionProbs[linear + j]; }
	};
};

} // namespace stattools

#endif // TREESWIRL_TLATENTVARIABLEFIXED_H
