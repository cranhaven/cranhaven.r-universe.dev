
#include "TBirpPrior.h"
#include "TBirpCore.h"
#include "TModels.h"
#include "coretools/Storage/TDataFile.h"
#include <utility>

using namespace coretools::instances;

TBirpPrior::TBirpPrior(TData &Data, const TUniqueContainer<std::string> &Locations,
                       const TUniqueContainer<std::string> &CIGroupNames, const TUniqueContainer<TypeTime> &TimePoints,
                       const TUniqueContainer<std::string> &CovariateEffortNames,
                       const TUniqueContainer<std::string> &CovariateDetectionNames, size_t numSpecies,
                       TBirpCommonModel &CommonModel, std::unique_ptr<TBirpNegBinomAddOn> &NegBinomAddOn,
                       std::unique_ptr<TBirpStochasticAddOn> &StochasticAddOn, std::string Prefix)
    : _data(Data), _locations(Locations), _CIGroupNames(CIGroupNames), _timepoints(TimePoints),
      _covariateEffortNames(CovariateEffortNames), _covariateDetectionNames(CovariateDetectionNames),
      _numSpecies(numSpecies), _out(std::move(Prefix)) {

	// stattools: tell which ones are prior parameters
	addPriorParameter(&CommonModel.gamma);
	for (auto &a : CommonModel.alpha) { addPriorParameter(a.get()); }
	for (auto &b : CommonModel.beta0) { addPriorParameter(b.get()); }
	for (auto &b : CommonModel.beta) { addPriorParameter(b.get()); }
	if (_numSpecies > 1) { throw coretools::TUserError("Birp only supports single species at the moment.."); }
	if (NegBinomAddOn) { addPriorParameter({&NegBinomAddOn->muOrN, &NegBinomAddOn->b}); }
	if (StochasticAddOn) { addPriorParameter({&StochasticAddOn->logPhi, &StochasticAddOn->logSigma}); }

	// create models
	size_t speciesID = 0;
	if (StochasticAddOn) {
		_models.emplace_back(std::make_unique<TStochastic>(
		    speciesID, Data, Locations.size(), TimePoints.size(), NegBinomAddOn, &CommonModel.gamma,
		    &StochasticAddOn->logPhi, &StochasticAddOn->logSigma, _timepoints, _CIGroupNames, _out));
	} else {
		_models.emplace_back(std::make_unique<TDeterministic>(speciesID, Data, NegBinomAddOn, &CommonModel.gamma,
		                                                      _timepoints, _CIGroupNames, _out));
	}

	for (size_t i = 0; i < Data.size(); ++i) {
		_data[i].addAlphaBeta(CommonModel.alpha[i], CommonModel.beta0[i], CommonModel.beta[i]);
	}
}

std::string TBirpPrior::name() const { return "birp"; }

//------------------------------------
// Initialization

void TBirpPrior::initialize() {
	// set size of parameters etc.
	for (auto &_method : _data) { _method.initialize(_covariateEffortNames, _covariateDetectionNames, this); }
	for (size_t s = 0; s < _numSpecies; s++) { _models[s]->initialize(_data, _locations, _timepoints, this); }
}

void TBirpPrior::guessInitialValues() {
	// set initial values of parameters
	for (auto &_method : _data) { _method.estimateInitialAlphaBeta(); }
	for (size_t s = 0; s < _numSpecies; ++s) { _models[s]->estimateInitial(_data, _timepoints); }

	// now initialize temporary variables (LL, JP etc)
	for (size_t s = 0; s < _numSpecies; ++s) { _models[s]->initializeTempVariables(_data); }
}

//------------------------------------
// MCMC

void TBirpPrior::updateParams() {
	for (size_t s = 0; s < _numSpecies; ++s) { _models[s]->update(_data); }
	for (auto &_method : _data) { _method.updateAlphaBeta(_data, _models); }
}

double TBirpPrior::getSumLogPriorDensity(const Storage &) const {
	// calculates likelihood
	double sum = 0.0;
	for (size_t s = 0; s < _numSpecies; ++s) { sum += _models[s]->sumCurLL(); }
	return sum;
}

void TBirpPrior::burninHasFinished() {
	for (auto &model : _models) { model->resetSummaryGammaPosterior(); }
}

void TBirpPrior::writeSummaryGammaPosterior(const std::string &Prefix) {
	// todo: make sure for multispecies to add name of species to Prefix!
	for (auto &model : _models) { model->writeSummaryGammaPosterior(Prefix); }
}

//------------------------------------
// Simulations

void TBirpPrior::_simulateUnderPrior(Storage *) {
	// first calculate effort
	for (auto &method : _data) { method.initializeEffort(); }

	// do we simulate counts conditioned on nu_ij (all the rest is given by data file)?
	if (parameters().exists("data")) {
		for (size_t i = 0; i < _numSpecies; ++i) { _models[i]->simulateConditioned(_data); }
	} else {
		// read N_0 or n_bar
		bool use_n_bar = true;
		double n       = parameters().get("n_bar", 1000.0);
		if (parameters().exists("N_0")) {
			n         = parameters().get<double>("N_0");
			use_n_bar = false;
		}

		for (size_t i = 0; i < _numSpecies; ++i) { _models[i]->simulate(_data, use_n_bar, n); }
	}
}

//------------------------------------
// Getters

const TUniqueContainer<std::string> &TBirpPrior::locations() const { return _locations; }

const TData &TBirpPrior::data() const { return _data; }

const TUniqueContainer<TypeTime> &TBirpPrior::timepoints() const { return _timepoints; }
