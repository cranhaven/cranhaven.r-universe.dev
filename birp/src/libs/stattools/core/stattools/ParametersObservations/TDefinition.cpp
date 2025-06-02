//
// Created by madleina on 11.12.20.
//

#include "stattools/ParametersObservations/TDefinition.h"
#include "coretools/Strings/fillContainer.h"

namespace stattools {

//--------------------------------------------
// ProposalKernel
//--------------------------------------------

namespace ProposalKernel {

std::string proposalKernelToString(MCMCProposalKernel Proposal) {
	if (Proposal == normal) {
		return "normal";
	} else if (Proposal == uniform) {
		return "uniform";
	} else if (Proposal == scaleLogNormal) {
		return "scaleLogNormal";
	} else if (Proposal == integer) {
		return "integer";
	} else if (Proposal == randomInteger) {
		return "randomInteger";
	} else if (Proposal == boolean) {
		return "boolean";
	}
	return "-";
}

MCMCProposalKernel stringToProposalKernel(std::string_view String) {
	for (size_t p = min; p != max; p++) {
		if (proposalKernelToString(MCMCProposalKernel(p)) == String) { return MCMCProposalKernel(p); }
	}
	UERROR("Proposal kernel '", String, "' does not exist!");
}
} // end namespace ProposalKernel

//--------------------------------------------
// Files
//--------------------------------------------

std::string MCMCFileToString(MCMCFiles Type) {
	if (Type == MCMCFiles::trace) {
		return "trace";
	} else if (Type == MCMCFiles::meanVar) {
		return "meanVar";
	} else if (Type == MCMCFiles::statePosteriors) {
		return "statePosteriors";
	} else if (Type == MCMCFiles::posteriorMode) {
		return "posteriorMode";
	} else if (Type == MCMCFiles::simulation) {
		return "simulated";
	}
	DEVERROR("Type ", (size_t)Type, " does not exist!");
}

//--------------------------------------------
// TDefinitionBase
//--------------------------------------------

TDefinition::TDefinition(std::string_view Prefix) { setDefaultFiles(Prefix); }

TDefinition::TDefinition(std::string_view Prefix, std::string_view PriorParameters) {
	setDefaultFiles(Prefix);
	setPriorParameters(PriorParameters);
}

TDefinition::TDefinition(std::string_view Prefix, size_t Precision) {
	setDefaultFiles(Prefix);
	setPrecision(Precision);
}

bool TDefinition::isObserved() const { return true; }

void TDefinition::reSetObserved(std::string_view Observed) {
	bool observed = coretools::str::fromString<bool, true>(Observed);
	if (observed != isObserved()) {
		// changed parameter to observation or vice-versa -> not allowed, throw
		UERROR("Error when building parameter '", _name, "': Can not change parameter to observation or vice-versa!");
	}
}

void TDefinition::setPriorParameters(std::string_view Parameters) { _priorParameters = Parameters; }

std::string_view TDefinition::priorParameters() const { return _priorParameters; }

void TDefinition::setOffsetForDimensionNames(size_t Offset) { _offsetDimensionNames = Offset; }

size_t TDefinition::offSetDimensionNames() const { return _offsetDimensionNames; }

void TDefinition::editFile(MCMCFiles Type) {
	if (_generalPrefix != "") { editFile(Type, _generalPrefix); }
}

void TDefinition::editFile(MCMCFiles Type, std::string_view Prefix) {
	for (auto &it : _files) {
		if (it.first == Type) {
			it.second = Prefix; // type already exists: modify prefix
			return;
		}
	}
	_files.emplace_back(Type, Prefix); // new type
}

bool TDefinition::writesFile(stattools::MCMCFiles Type) const {
	for (const auto &it : _files) {
		if (it.first == Type) { return it.second != ""; }
	}
	return false;
}

std::string TDefinition::getPrefix(stattools::MCMCFiles Type) const {
	for (auto it : _files) {
		if (it.first == Type) { return it.second; }
	}
	return "";
}

void TDefinition::setDefaultFiles(std::string_view Prefix) {
	_generalPrefix = Prefix;

	editFile(MCMCFiles::simulation);
	editFile(MCMCFiles::trace);
}

void TDefinition::setPrecision(size_t Precision) { _precision = Precision; }

size_t TDefinition::precision() const { return _precision; }

//--------------------------------------------
// TParameterDefinition
//--------------------------------------------

TParameterDefinition::TParameterDefinition(std::string_view Prefix) { setDefaultFiles(Prefix); }

TParameterDefinition::TParameterDefinition(std::string_view Prefix, std::string_view PriorParameters) {
	setDefaultFiles(Prefix);
	setPriorParameters(PriorParameters);
}

TParameterDefinition::TParameterDefinition(std::string_view Prefix, std::string_view PriorParameters,
										   std::string_view InitialValue) {
	setDefaultFiles(Prefix);
	setPriorParameters(PriorParameters);
	setInitVal(InitialValue);
}

TParameterDefinition::TParameterDefinition(std::string_view Prefix, std::string_view PriorParameters,
										   ProposalKernel::MCMCProposalKernel ProposalKernel) {
	setDefaultFiles(Prefix);
	setPriorParameters(PriorParameters);
	setPropKernel(ProposalKernel);
}

TParameterDefinition::TParameterDefinition(std::string_view Prefix, std::string_view PriorParameters,
										   size_t OffsetDimensionNames) {
	setDefaultFiles(Prefix);
	setPriorParameters(PriorParameters);
	setOffsetForDimensionNames(OffsetDimensionNames);
}

bool TParameterDefinition::isObserved() const { return false; }

void TParameterDefinition::update(bool IsUpdated) { _isUpdated = IsUpdated; }

bool TParameterDefinition::isUpdated() const { return _isUpdated; }

void TParameterDefinition::excludeFromDAGUpdates(bool Exclude) { _excludeFromDAGUpdates = Exclude; }

bool TParameterDefinition::isExcludedFromDAGUpdates() const { return _excludeFromDAGUpdates; }

void TParameterDefinition::setArgsWeightedUpdates(const std::string &Args) {
	coretools::str::fillContainerFromString(Args, _argsWeightedUpdates, ',');
}

void TParameterDefinition::setArgsWeightedUpdates(const std::vector<std::string> &Args) { _argsWeightedUpdates = Args; }

const std::vector<std::string> &TParameterDefinition::argsWeightedUpdates() const { return _argsWeightedUpdates; }

void TParameterDefinition::setUpdateEveryNthIter(size_t N) {
	if (N == 0) {
		_doUpdateEveryNthIter = false;
		update(false); // don't update at all!
	} else if (N == 1) {
		_doUpdateEveryNthIter = false;
	} else {
		_doUpdateEveryNthIter = true;
		_updateEveryNthIter   = N;
	}
}

bool TParameterDefinition::doUpdateEveryNthIter() const { return _doUpdateEveryNthIter; }
size_t TParameterDefinition::updateEveryNthIter() const { return _updateEveryNthIter; }

void TParameterDefinition::setUnequalNumberOfUpdates(bool Unequal) { _unequalNumberOfUpdates = Unequal; };
bool TParameterDefinition::unequalNumberOfUpdates() const { return _unequalNumberOfUpdates; }

void TParameterDefinition::setStateNames(const std::vector<std::string> &StateNames) { _stateNames = StateNames; }

std::vector<std::string> TParameterDefinition::getStateNames() const { return _stateNames; }

void TParameterDefinition::setJumpSizeForAll(bool oneJumpSizeForAll) { _oneJumpSizeForAll = oneJumpSizeForAll; }

void TParameterDefinition::setInitJumpSizeProposal(std::string_view init) {
	_initPropKernel           = init;
	_hasDefaultInitPropKernel = false;
}

void TParameterDefinition::setInitPropKernelScalesToValue(bool shouldScale) {
	_scaleInitPropKernelToValue = shouldScale;
}

void TParameterDefinition::setPropKernel(std::string_view distr) {
	_propKernelDistr = ProposalKernel::stringToProposalKernel(coretools::str::strip(distr));
}

void TParameterDefinition::setPropKernel(const ProposalKernel::MCMCProposalKernel &distr) { _propKernelDistr = distr; }

std::string TParameterDefinition::initJumpSizeProposal() const { return _initPropKernel; }

bool TParameterDefinition::hasDefaultJumpSizeProposal() const { return _hasDefaultInitPropKernel; }

ProposalKernel::MCMCProposalKernel TParameterDefinition::propKernel() const { return _propKernelDistr; }

void TParameterDefinition::setInitVal(std::string_view value) {
	_initVal           = value;
	_hasDefaultInitVal = false;
}

void TParameterDefinition::reSetInitValToDefault() {
	_initVal           = "0";
	_hasDefaultInitVal = true;
}

std::string TParameterDefinition::initVal() const { return _initVal; }

bool TParameterDefinition::hasDefaultInitVal() const { return _hasDefaultInitVal; }

bool TParameterDefinition::oneJumpSizeForAll() const { return _oneJumpSizeForAll; }

bool TParameterDefinition::scaleInitPropKernelToValue() const { return _scaleInitPropKernelToValue; }

void TParameterDefinition::setRJProposalDistrParams(std::string_view Params) { _rjProposalDistrParams = Params; }
std::string_view TParameterDefinition::rjProposalDistrParams() const { return _rjProposalDistrParams; }
void TParameterDefinition::setQ1To0RJMCMC(coretools::Probability q_1_To_0) { _q_1_To_0 = q_1_To_0; }
double TParameterDefinition::q1To0RJMCMC() const { return _q_1_To_0; }

} // end namespace stattools
