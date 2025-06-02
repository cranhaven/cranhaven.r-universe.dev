//
// Created by madleina on 11.12.20.
//

#ifndef TDEFINITION_H
#define TDEFINITION_H

#include <cstddef>
#include <string>
#include <vector>

#include "coretools/Types/probability.h"

namespace stattools {

//--------------------------------------------
// ProposalKernels
//--------------------------------------------

namespace ProposalKernel {

enum MCMCProposalKernel : size_t {
	min    = 0,
	normal = min,
	uniform,
	scaleLogNormal,
	integer,
	randomInteger,
	boolean,
	custom,
	missing,
	max = missing
};
std::string proposalKernelToString(MCMCProposalKernel PropKernel);
MCMCProposalKernel stringToProposalKernel(std::string_view String);

} // end namespace ProposalKernel

//--------------------------------------------
// Files
//--------------------------------------------

enum class MCMCFiles { trace, meanVar, statePosteriors, posteriorMode, simulation };
std::string MCMCFileToString(MCMCFiles Type);

//-------------------------------------------
// TDefinitionBase
//-------------------------------------------

class TDefinition {
	// definition for observations
protected:
	// name of observation
	std::string _name;

	// prior parameter(s) -> only fixed values allowed
	std::string _priorParameters;

	// names
	size_t _offsetDimensionNames = 1;

	// files
	std::string _generalPrefix;
	std::vector<std::pair<MCMCFiles, std::string>> _files;

	// manipulate precision for writing files
	size_t _precision = 6;

public:
	TDefinition() = default;
	TDefinition(std::string_view Prefix);
	TDefinition(std::string_view Prefix, std::string_view Parameters);
	TDefinition(std::string_view Prefix, size_t Precision);
	virtual ~TDefinition() = default;

	// parameter or observation?
	virtual bool isObserved() const;
	void reSetObserved(std::string_view observed);

	// prior pararmeters (fixed)
	void setPriorParameters(std::string_view Parameters);
	std::string_view priorParameters() const;

	// dimension names
	void setOffsetForDimensionNames(size_t Offset);
	size_t offSetDimensionNames() const;

	// files
	void editFile(MCMCFiles Type);
	void editFile(MCMCFiles Type, std::string_view Prefix);
	bool writesFile(MCMCFiles Type) const;
	std::string getPrefix(MCMCFiles Type) const;
	void setDefaultFiles(std::string_view filename);

	// precision for writing to file
	void setPrecision(size_t Precision);
	size_t precision() const;
};

using TObservationDefinition = TDefinition;

//-------------------------------------------
// TParameterDefinition
//-------------------------------------------

class TParameterDefinition : public TDefinition {
protected:
	// in addition to stuff that TDefinition stores,
	// this class knows if and how to update (proposal kernels etc.) and to write to files
	// -> only parameter needs to know this

	// updating schemes
	bool _isUpdated             = true;
	bool _excludeFromDAGUpdates = false;
	std::vector<std::string> _argsWeightedUpdates;
	bool _unequalNumberOfUpdates = false;

	// update every n^th iteration?
	bool _doUpdateEveryNthIter = false;
	size_t _updateEveryNthIter = 1;

	// files
	std::vector<std::string> _stateNames;

	// propKernel
	std::string _initPropKernel                         = "1";
	ProposalKernel::MCMCProposalKernel _propKernelDistr = ProposalKernel::normal;
	bool _hasDefaultInitPropKernel                      = true;
	bool _oneJumpSizeForAll                             = true;
	bool _scaleInitPropKernelToValue                    = false;

	// initial values
	bool _hasDefaultInitVal = true;
	std::string _initVal    = "0";

	// RJ-MCMC
	std::string _rjProposalDistrParams;
	coretools::Probability _q_1_To_0{0.1};

public:
	TParameterDefinition() = default;
	TParameterDefinition(std::string_view Prefix);
	TParameterDefinition(std::string_view Prefix, std::string_view PriorParameters);
	TParameterDefinition(std::string_view Prefix, std::string_view PriorParameters, std::string_view InitialValue);
	TParameterDefinition(std::string_view Prefix, std::string_view PriorParameters,
						 ProposalKernel::MCMCProposalKernel ProposalKernel);
	TParameterDefinition(std::string_view Prefix, std::string_view PriorParameters, size_t OffsetDimensionNames);
	~TParameterDefinition() override = default;

	bool isObserved() const override;

	// updating schemes
	void update(bool IsUpdated);
	bool isUpdated() const;
	void excludeFromDAGUpdates(bool Exclude);
	bool isExcludedFromDAGUpdates() const;
	void setArgsWeightedUpdates(const std::string &Args);
	void setArgsWeightedUpdates(const std::vector<std::string> &Args);
	const std::vector<std::string> &argsWeightedUpdates() const;
	void setUpdateEveryNthIter(size_t N);
	bool doUpdateEveryNthIter() const;
	size_t updateEveryNthIter() const;
	void setUnequalNumberOfUpdates(bool Unequal);
	bool unequalNumberOfUpdates() const;

	// files
	void setStateNames(const std::vector<std::string> &Header);
	std::vector<std::string> getStateNames() const;

	// propKernel
	void setJumpSizeForAll(bool oneJumpSizeForAll);
	void setInitPropKernelScalesToValue(bool shouldScale);
	void setInitJumpSizeProposal(std::string_view init);
	void setPropKernel(std::string_view distr);
	void setPropKernel(const ProposalKernel::MCMCProposalKernel &distr);
	std::string initJumpSizeProposal() const;
	bool hasDefaultJumpSizeProposal() const;
	ProposalKernel::MCMCProposalKernel propKernel() const;
	bool oneJumpSizeForAll() const;
	bool scaleInitPropKernelToValue() const;

	// initial values
	void setInitVal(std::string_view value);
	void reSetInitValToDefault();
	std::string initVal() const;
	bool hasDefaultInitVal() const;

	// RJ-MCMC
	void setRJProposalDistrParams(std::string_view Params);
	std::string_view rjProposalDistrParams() const;
	void setQ1To0RJMCMC(coretools::Probability q_1_To_0);
	double q1To0RJMCMC() const;
};

}; // end namespace stattools

#endif // TDEFINITION_H
