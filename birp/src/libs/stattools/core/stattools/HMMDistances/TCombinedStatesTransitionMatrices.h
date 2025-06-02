//
// Created by madleina on 21.12.21.
//

#ifndef APPROXWF3_TCOMBINEDSTATESTRANSITIONMATRICES_H
#define APPROXWF3_TCOMBINEDSTATESTRANSITIONMATRICES_H

#include "coretools/Storage/TDimension.h"
#include "stattools/HMMDistances/TTransitionMatrixHMMDistances.h"
#include "coretools/Main/TLog.h"

namespace stattools {

template<typename PrecisionType, typename NumStatesType, typename LengthType>
class TCombinedStatesTransitionMatrices : public TTransitionMatrix_base<PrecisionType, NumStatesType, LengthType> {
	// class that deals with transition matrices on combined states
	// each state has its own transition matrix
	// but gamma's and xis from Baum-Welch run on combination of states
	// -> this class knows how to sum them up
private:
	using TOptimizer = TTransitionMatrixDistancesOptimizerBase<PrecisionType, NumStatesType, LengthType>;

protected:
	// vector of transition matrices
	std::vector<std::shared_ptr<stattools::TTransitionMatrixDistances<PrecisionType, NumStatesType, LengthType>>> _taus;
	// distances: non-owning pointer
	coretools::TDistancesBinnedBase *_distances = nullptr;

	// dimensions of states combinations
	std::vector<size_t> _dimensions;
	using TTransitionMatrix_base<PrecisionType, NumStatesType, LengthType>::_numStates;
	coretools::TMultiIndex _index;

	// report EM parameters
	coretools::TOutputFile _fileReportEM;
	void _writeEMReportFile();

	void _setFixedPriorParameters(coretools::TDistancesBinnedBase *Distances,
								  const std::vector<std::shared_ptr<TOptimizer>> &Optimizers);

	void _runEMEstimationOrStatePosteriors(TLatentVariableWithRep<double, size_t, size_t> &latentVariable, bool RunEM);

public:
	TCombinedStatesTransitionMatrices() = default;
	TCombinedStatesTransitionMatrices(coretools::TDistancesBinnedBase *Distances,
									  const std::vector<std::shared_ptr<TOptimizer>> &Optimizers);
	TCombinedStatesTransitionMatrices(coretools::TDistancesBinnedBase *Distances,
									  const std::vector<std::shared_ptr<TOptimizer>> &Optimizers,
									  std::string_view Filename, std::vector<std::string> Header = {});
	~TCombinedStatesTransitionMatrices() = default;

	void initialize(coretools::TDistancesBinnedBase *Distances,
					const std::vector<std::shared_ptr<TOptimizer>> &Optimizers);
	void initialize(coretools::TDistancesBinnedBase *Distances,
					const std::vector<std::shared_ptr<TOptimizer>> &Optimizers, std::string_view Filename,
					std::vector<std::string> Header = {});

	void fixTransitionMatricesDuringEM(const std::vector<bool> &Fix, coretools::TConstView<PrecisionType> Values);

	// initialization strategies
	void setInitialization(TypeTransMatInitialization InitType) override;
	TypeTransMatInitialization initializationType() const override;

	void initializeEMParameters(const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatentVariable,
								const std::vector<LengthType> &ChunkEnds) override;

	// fill matrices
	bool fillProbabilities(coretools::TConstView<PrecisionType> Values);

	// transition and stationary probabilities of combined states
	PrecisionType operator()(LengthType Index, NumStatesType From, NumStatesType To) const override;
	PrecisionType stationary(NumStatesType State) const override;
	PrecisionType transitionProbability(LengthType Index, NumStatesType From, NumStatesType To) const;
	PrecisionType oldTransitionProbability(LengthType Index, NumStatesType From, NumStatesType To) const;
	PrecisionType logTransitionProbability(LengthType Index, NumStatesType From, NumStatesType To) const;
	PrecisionType logOldTransitionProbability(LengthType Index, NumStatesType From, NumStatesType To) const;
	PrecisionType dist1_transitionProbability(NumStatesType From, NumStatesType To) const;
	void swapNewOldTau();
	std::vector<PrecisionType> getFinalParameterEstimatesEM() const;

	// getters
	[[nodiscard]] coretools::TDistancesBinnedBase *distances() const;
	[[nodiscard]] size_t numParameters() const;
	std::vector<PrecisionType> getParameters() const override;
	bool setParameters(coretools::TConstView<PrecisionType> Params) override;

	// EM initialization
	void startNewChunk(size_t Chunk) override;
	void setValuesEM(coretools::TConstView<PrecisionType> Values);
	void noInitializationBeforeEM();
	void handleEMParameterInitializationTransitionProbabilities(LengthType Index, NumStatesType PreviousState,
																NumStatesType CurrentState) override;
	void finalizeEMParameterInitializationTransitionProbabilities() override;
	// EM
	void runEMEstimation(TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &latentVariable);
	void estimateStatePosteriors(TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &latentVariable);
	void prepareEMParameterEstimationInitial() override;
	void prepareEMParameterEstimationOneIterationTransitionProbabilities() override;
	void handleEMParameterEstimationOneIterationTransitionProbabilities(
		LengthType Index, const THMMPosteriorXi<PrecisionType, NumStatesType, LengthType> &xi) override;
	void finalizeEMParameterEstimationOneIterationTransitionProbabilities() override;
	void finalizeEMParameterEstimationFinal() override;

	void simulateDistances(int length);
	NumStatesType sampleNextState(LengthType Index, NumStatesType State) const override;

	void report();
	void reportEMParameters() override;
	using TTransitionMatrix_base<PrecisionType, NumStatesType, LengthType>::print;
	void print();
};

}; // end namespace stattools

#include "TCombinedStatesTransitionMatrices.tpp"

#endif // APPROXWF3_TCOMBINEDSTATESTRANSITIONMATRICES_H
