//
// Created by madleina on 22.01.21.
//

#ifndef TPARAMETERBASE_H
#define TPARAMETERBASE_H

#include "coretools/Files/TOutputFile.h"
#include "coretools/Storage/TDimension.h"
#include "stattools/ParametersObservations/TDefinition.h"
#include <string>
#include <vector>

namespace stattools {

// forward declarations
class TDAG;
class TUpdateBase;

//-------------------------------------------
// TNodeBase
//-------------------------------------------
class TNodeBase {
	// abstract base class for TParameterBase and TObservationBase
private:
	// is this parameter/observation part of a prior (e.g. a prior parameter or covariate observation)?
	// -> if false: is at bottom of DAG
	bool _isPartOfBox = false;

	// are we currently initializing the parameter? Or are we in MCMC?
	bool _initializationIsRunning = false;
	bool _hasFixedInitialValue    = false;

public:
	virtual ~TNodeBase() = default;

	// name of parameter
	[[nodiscard]] virtual const std::string &name() const = 0;

	// functions for DAG
	virtual void tellBoxAboveToInitStorage()                 = 0;
	virtual void constructDAG(TDAG &DAG, TDAG &temporaryDAG) = 0;

	void setIsPartOfBox() { _isPartOfBox = true; }
	[[nodiscard]] bool isPartOfBox() const { return _isPartOfBox; }

	// initialize prior parameters (e.g. with MLE)
	virtual void guessInitialValues() = 0;
	constexpr bool hasFixedInitialValue() const noexcept { return _hasFixedInitialValue; }
	constexpr void fixInitialization(bool HasFixed = true) noexcept { _hasFixedInitialValue = HasFixed; }
	constexpr void startInitialization() noexcept { _initializationIsRunning = true; }
	constexpr bool initializationIsRunning() const noexcept { return _initializationIsRunning; }
	constexpr void endInitialization() noexcept { _initializationIsRunning = false; }

	// updates in MCMC
	virtual void tellBoxAboveThatBurninFinished() = 0;
	virtual void tellBoxAboveThatMCMCFinished()   = 0;

	// posterior
	[[nodiscard]] virtual double getSumLogPriorDensity() const = 0;

	// simulate values under prior distribution
	virtual void simulateUnderPrior() = 0;

	// writers
	virtual void writeToTrace(coretools::TOutputMaybeRcppFile &File) const                   = 0;
	virtual void writeToSummary(MCMCFiles Type, coretools::TOutputMaybeRcppFile &File) const = 0;
	virtual void fillNames(std::vector<std::string> &Header) const                           = 0;

	virtual TObservationDefinition &getDefinition() = 0;
};

//-------------------------------------------
// TParameterBase
//-------------------------------------------
class TParameterBase : public TNodeBase {
	// abstract base class for TParameter
public:
	~TParameterBase() override = default;

	// definition
	virtual TParameterDefinition &getDefinition() override = 0;

	// set from string
	virtual void setFromString(const std::string &Values)     = 0;
	virtual void setProposalWidths(const std::string &Values) = 0;

	// proposal stuff
	[[nodiscard]] virtual bool isDeterministic() const          = 0;
	[[nodiscard]] virtual bool isRJMCMCModel() const            = 0;
	[[nodiscard]] virtual bool isExcludedFromDAGUpdates() const = 0;
	[[nodiscard]] virtual bool isUpdated() const                = 0;
	virtual void clearMeanVar()                                 = 0;
	virtual TUpdateBase *getPtrToUpdater()                      = 0;

	// updates
	virtual void update(size_t Iteration)                                      = 0;
	virtual double calculateLLRatio(const coretools::TRange &Range)            = 0;
	virtual void setAllTempVals()                                              = 0;
	virtual void updateTempVals(const coretools::TRange &Range, bool Accepted) = 0;

	// writers
	virtual void writeValsOneString(coretools::TOutputMaybeRcppFile &file) const     = 0;
	virtual size_t getNumStatesForStatePosterior() const                             = 0;
	virtual void writeJumpSizeOneString(coretools::TOutputMaybeRcppFile &file) const = 0;
	virtual std::string getPosteriorMeans() const                                    = 0;
};

using TObservationBase = TNodeBase;

} // end namespace stattools

#endif // TPARAMETERBASE_H
