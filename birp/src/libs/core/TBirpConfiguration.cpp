#include "TBirpPrior.h" // note: needs to be included first to avoid include issues with Rcpp and RcppArmadillo
#include "TBirpConfiguration.h"
#include "TData.h"
#include "coretools/Strings/repeatString.h"
#include "coretools/Types/strongTypes.h"
#include "stattools/MCMC/TMCMC.h"

using namespace coretools::instances;
using namespace stattools;
using namespace stattools::prior;

std::string readAlphaDirichlet(const std::string &ArgName, size_t Size, coretools::StrictlyPositive Default) {
	// read values of alpha from command line
	std::vector<coretools::StrictlyPositive> alphas;
	if (parameters().exists(ArgName)) {
		parameters().fill(ArgName, alphas);
	} else {
		alphas.push_back(Default);
	}

	if (alphas.empty()) { throw coretools::TUserError("Invalid argument '", ArgName, "': is empty!"); }
	if (alphas.size() == 1) { return coretools::str::repeat(alphas[0], Size); }
	if (alphas.size() == Size) { return coretools::str::concatenateString(alphas, ","); }

	throw coretools::TUserError("Size of argument '", ArgName, "' (", alphas.size(), ") does not match expected size (", Size, ").");
}

//------------------------------------------
// TBirpCommonModel
//------------------------------------------

void TBirpCommonModel::_createAlpha(size_t i, TData &Data, const std::string &Filename) {
	// create alpha
	auto name = "alpha_" + Data[i].name();
	TParameterDefinition def(Filename, readAlphaDirichlet("dirichlet_alpha", Data[i].numCovariatesEffort(),
	                                                      10.0)); // alpha = 10 is quite arbitrary choice
	def.excludeFromDAGUpdates(true);
	alpha[i] = std::make_shared<stattools::TParameter<SpecAlpha, BirpBox>>(name, &_boxOnAlpha, def);
}

void TBirpCommonModel::_createBeta0(size_t i, TData &Data, const std::string &Filename) {
	// create beta
	auto name = "beta0_" + Data[i].name();
	TParameterDefinition def(Filename, "0,1");
	def.excludeFromDAGUpdates(true);
	beta0[i] = std::make_shared<stattools::TParameter<SpecBeta, BirpBox>>(name, &_boxOnBeta0, def);
}

void TBirpCommonModel::_createBeta(size_t i, TData &Data, const std::string &Filename) {
	// create beta
	auto name = "beta_" + Data[i].name();
	TParameterDefinition def(Filename, "0,1");
	def.excludeFromDAGUpdates(true);
	beta[i] = std::make_shared<stattools::TParameter<SpecBeta, BirpBox>>(name, &_boxOnBeta, def);
}

TBirpCommonModel::TBirpCommonModel(TData &Data, const std::string &Filename)
    : _boxOnGamma(), _boxOnAlpha(), _boxOnBeta(), _boxOnBeta0(), gamma("gamma", &_boxOnGamma, {Filename}) {

	// don't update gamma and TOC via DAG
	gamma.getDefinition().excludeFromDAGUpdates(true);

	gamma.getDefinition().setJumpSizeForAll(false);

	// create alpha and beta
	const size_t numMethods = Data.size();
	alpha.resize(numMethods);
	beta.resize(numMethods);
	beta0.resize(numMethods);
	for (size_t i = 0; i < numMethods; ++i) {
		_createAlpha(i, Data, Filename);
		_createBeta0(i, Data, Filename);
		_createBeta(i, Data, Filename);
	}
}

//------------------------------------------
// TBirpNegBinomAddOn
//------------------------------------------

std::string getNameMuOrN(bool HasDetectionCovariates) {
	if (HasDetectionCovariates) { // if detection probabilities are modelled through covariates: mu reduces to N_T0
		return "N";
	}
	return "mu";
}

std::string getLambdaB() { return parameters().get("lambda_b", "0.1"); }

TBirpNegBinomAddOn::TBirpNegBinomAddOn(bool HasDetectionCovariates, const std::string &Filename)
    : _boxOnMuOrN(), _boxOnB(), muOrN(getNameMuOrN(HasDetectionCovariates), &_boxOnMuOrN, {Filename}),
      b("b", &_boxOnB, {Filename, getLambdaB()}) {

	// don't update muOrN and b via DAG
	muOrN.getDefinition().excludeFromDAGUpdates(true);
	b.getDefinition().excludeFromDAGUpdates(true);
}

//------------------------------------------
// TBirpStochasticAddOn
//------------------------------------------

TBirpStochasticAddOn::TBirpStochasticAddOn(const std::string &Filename)
    : _boxOnLogPhi(), _boxOnLogSigma(), logPhi("logPhi", &_boxOnLogPhi, {Filename}),
      logSigma("logSigma", &_boxOnLogSigma, {Filename}) {

	// don't update muOrN and b via DAG
	logPhi.getDefinition().excludeFromDAGUpdates(true);
	logSigma.getDefinition().excludeFromDAGUpdates(true);

	logPhi.getDefinition().setJumpSizeForAll(false);
	logPhi.getDefinition().setUnequalNumberOfUpdates(true); // first logPhi is not updated

	TypeLogSigma ::setMin(-10.); // prevent numeric issues with very small sigma: 1/sigma can result in nan
}

//------------------------------------------
// TBirpModel
//------------------------------------------

TBirpModel::TBirpModel(TData &Data, const TUniqueContainer<std::string> &Locations,
                       const TUniqueContainer<std::string> &CIGroupNames, const TUniqueContainer<TypeTime> &TimePoints,
                       const TUniqueContainer<std::string> &SpeciesNames,
                       const TUniqueContainer<std::string> &CovariateEffortNames,
                       const TUniqueContainer<std::string> &CovariateDetectionNames, const std::string &Filename,
                       bool Stochastic, bool Simulate, const std::map<std::string, std::string> &InitVals)
    : _commonModel(Data, Filename) {

	// do negative binomial?
	const bool hasDetectionCovariates = !CovariateDetectionNames.empty();
	if (parameters().exists("negativeBinomial")) {
		logfile().list("Will use a negative binomial distribution to model counts (argument 'negativeBinomial').");
		_negBinomAddOn = std::make_unique<TBirpNegBinomAddOn>(hasDetectionCovariates, Filename);
	} else {
		logfile().list("Will use a Poisson distribution to model counts (use argument 'negativeBinomial' to change).");
	}

	// do stochastic?
	if (Stochastic) {
		// set initial values of all non-stochastic parameters from deterministic MCMC
#ifdef USE_RCPP
		coretools::instances::rcppResults().clear();
#endif
		if (!Simulate) {
			for (const auto &p : stattools::instances::dagBuilder().getAllParameters()) {
				auto it = InitVals.find(p->name());
				if (it != InitVals.end()) { p->getDefinition().setInitVal(it->second); }
			}
		}
		// create stochastic add-on
		_stochasticAddOn = std::make_unique<TBirpStochasticAddOn>(Filename);
	}

	// create birp prior (Poisson or Negative Binomial)
	_birpPrior = std::make_shared<TBirpPrior>(Data, Locations, CIGroupNames, TimePoints, CovariateEffortNames,
	                                          CovariateDetectionNames, SpeciesNames.size(), _commonModel,
	                                          _negBinomAddOn, _stochasticAddOn, Filename);

	// add updater for joint logPhi update
	if (Stochastic) { stattools::instances::dagBuilder().addExtraUpdater(_birpPrior->getPtrToJointUpdater()); }

	// create fake storage: data are stored in TSpecies, but stattools requires TMultiDimensionalStorage
	coretools::TMultiDimensionalStorage<TypeCounts, 2> fakeData;
	_data = std::make_unique<stattools::TObservation<TypeCounts, NumDimCounts, BirpBox>>(
	    "likelihood", _birpPrior.get(), fakeData, stattools::TObservationDefinition(Filename));

	// define function that is called when updating
	_funUpdate = &TBirpPrior::updateParams;
	stattools::instances::dagBuilder().addFuncToUpdate(*_birpPrior, _funUpdate);
}

void TBirpModel::writeSummaryGammaPosterior(const std::string &Prefix) {
	_birpPrior->writeSummaryGammaPosterior(Prefix);
}

std::map<std::string, std::string> TBirpModel::getFinalValues() {
	std::map<std::string, std::string> map;

	// get mean of all model parameters
	for (const auto &p : stattools::instances::dagBuilder().getAllParameters()) {
		map[p->name()] = p->getPosteriorMeans();
	}
	return map;
}
