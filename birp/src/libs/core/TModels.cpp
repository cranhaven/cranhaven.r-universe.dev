#include "TModels.h"
#include "TDistributions.h"

//---------------------------------------
// TModelBase
//---------------------------------------

TModelBase::TModelBase(size_t SpeciesIDinUniqueContainer, const TData &Data,
                       const std::unique_ptr<TBirpNegBinomAddOn> &NegBinomAddOn)
    : _speciesIDinUniqueContainer(SpeciesIDinUniqueContainer) {

	// create distribution
	if (NegBinomAddOn) { // negative binomial
		_distr = std::make_unique<TNegBinDistribution>(_speciesIDinUniqueContainer, &NegBinomAddOn->muOrN,
		                                               &NegBinomAddOn->b);
	} else { // Poisson
		_distr = std::make_unique<TPoissonDistribution>(_speciesIDinUniqueContainer);
	}

	// LL is per method/location combination
	_curLL.resize(Data.numMethLoc(_speciesIDinUniqueContainer), 0.0);
	_tryLL.resize(Data.numMethLoc(_speciesIDinUniqueContainer), 0.0);
}

void TModelBase::_initializeDistribution(const TData &Data, const TUniqueContainer<std::string> &Locations,
                                         TBirpPrior *BirpPrior) {
	_distr->initialize(Data, Locations, BirpPrior);
}

void TModelBase::_estimateInitialParameters(const TData &Data) { _distr->estimateInitialParameters(Data); }

void TModelBase::_updateDistributionParameters(const TData &Data) { _distr->update(Data, *this); }

void TModelBase::_simulateDistributionParameters(TData &Data, bool use_n_bar, double n) {
	_distr->simulate(Data, *this, use_n_bar, n);
}

void TModelBase::_simulateConditionedDistributionParameters(TData &Data) { _distr->simulateConditioned(Data, *this); }

std::vector<double> TModelBase::calculateSigma(const TLocations &loc) const {
	std::vector<double> sigma(loc.size());
	for (size_t k = 0; k < loc.size(); k++) {
		// calculate sigma and fill into p (will normalize later)
		sigma[k] = phi(loc.location_id(), loc[k].timepoint_id(), loc.group_id()) * loc[k].effort();
	}
	return sigma;
}

std::vector<double> TModelBase::calculateSigmaFromDeterministicPhi(const TLocations &loc) const {
	std::vector<double> sigma(loc.size());
	for (size_t k = 0; k < loc.size(); k++) {
		// calculate sigma and fill into p (will normalize later)
		sigma[k] = deterministicPhi(loc[k].timepoint_id(), loc.group_id()) * loc[k].effort();
	}
	return sigma;
}

arma::mat TModelBase::_calcFisherInfo(const TGamma &Gamma, const std::vector<double> &p,
                                      const std::vector<double> &sigma, TypeNu nu, const TLocations &loc) {
	// size of p and sigma = number of timepoints of current location
	arma::mat R(p.size(), Gamma.numEpochs());
	arma::mat RtP(Gamma.numEpochs(), p.size());
	for (size_t k = 0; k < p.size(); k++) {
		for (size_t m = 0; m < Gamma.numEpochs(); m++) {
			double denom       = 0.0;
			double rhoSumSigma = 0.0;
			for (size_t l = 0; l < p.size(); l++) {
				denom += sigma[l];
				const auto diffRho = Gamma.rho(loc[k].timepoint_id(), m) - Gamma.rho(loc[l].timepoint_id(), m);
				rhoSumSigma += diffRho * sigma[l];
			}
			denom     = denom * denom;
			R(k, m)   = sigma[k] * rhoSumSigma / denom;
			RtP(m, k) = R(k, m) * 1.0 / p[k];
		}
	}
	// real actual matrix multiplication
	auto tmp = RtP * R;
	return (double)nu * tmp;
}

double TModelBase::calculateJeffreyPrior(const TData &Data, const TGamma &Gamma, size_t CI_index) const {
	// used in update of gamma of deterministic and stochastic model
	arma::mat fisherInfo(Gamma.numEpochs(), Gamma.numEpochs(), arma::fill::zeros);
	for (size_t i = 0; i < Data.size(); ++i) {
		const auto &method = Data[i];
		if (!method.hasDataForSpeciesID(_speciesIDinUniqueContainer)) { continue; }
		if (!method.hasDataForCIGroup(CI_index)) { continue; }

		size_t index_in_counts = method.getIndexInCounts(_speciesIDinUniqueContainer);
		// loop over locations with that CI group
		for (const auto j_in_method : method.getLocationIndicesForCIGroup(CI_index)) {
			std::vector sigma     = calculateSigmaFromDeterministicPhi(method[j_in_method]);
			std::vector<double> p = sigma;
			// now normalize by row sum!
			coretools::normalize(p);
			// add to JP
			fisherInfo +=
			    _calcFisherInfo(Gamma, p, sigma, method[j_in_method].nu(index_in_counts), method[j_in_method]);
		}
	}
	const auto det = arma::det(fisherInfo);
	if (det == 0.0) { return 0.0; } // can happen if all counts are zero
	return log(det);                // sqrt drops out in hastings ratio
}

void TModelBase::_initializeLL(const TData &Data) {
	// loop over all datapoints (methods - locations - timepoints - CI groups) of a single species
	// used to initialize curLL
	size_t ix_linNumMethLoc = 0;
	for (size_t i = 0; i < Data.size(); ++i) {
		const auto &method = Data[i];
		if (!method.hasDataForSpeciesID(_speciesIDinUniqueContainer)) { continue; }
		size_t index_in_counts = method.getIndexInCounts(_speciesIDinUniqueContainer);
		for (size_t l = 0; l < method.size(); l++, ix_linNumMethLoc++) { // loop over locations
			std::vector sigma     = calculateSigma(method[l]);
			std::vector<double> p = sigma;
			// now normalize by row sum!
			coretools::normalize(p);
			// add to Likelihood
			_curLL[ix_linNumMethLoc] = _distr->calculateLL(sigma, method[l], p, index_in_counts, ix_linNumMethLoc, i);
		}
	}
	_tryLL = _curLL;
}

void TModelBase::_updateTryLL(const TData &Data, size_t index_in_counts, size_t ix_method, size_t j_in_method) {
	size_t ix_linNumMethLoc = Data.i_j_to_linear(_speciesIDinUniqueContainer, ix_method, j_in_method);
	const auto &location    = Data[ix_method][j_in_method];

	std::vector sigma     = calculateSigma(location);
	std::vector<double> p = sigma;
	// now normalize by row sum!
	coretools::normalize(p);
	// calculate Likelihood
	_tryLL[ix_linNumMethLoc] = _distr->calculateLL(sigma, location, p, index_in_counts, ix_linNumMethLoc, ix_method);
}

double TModelBase::calculateLLRatio(const TData &Data) {
	// loop over all datapoints (methods - locations - timepoints - CI groups) of a single species
	// used in update of sigma in stochastic model
	for (size_t i = 0; i < Data.size(); ++i) {
		const auto &method = Data[i];
		if (!method.hasDataForSpeciesID(_speciesIDinUniqueContainer)) { continue; }
		size_t index_in_counts = method.getIndexInCounts(_speciesIDinUniqueContainer);
		// loop over locations within that method
		for (size_t l = 0; l < method.size(); l++) { _updateTryLL(Data, index_in_counts, i, l); }
	}
	return coretools::containerSum(_tryLL) - coretools::containerSum(_curLL);
}

double TModelBase::calculateLLRatio_perCIGroup(size_t CI_index, const TData &Data) {
	// loop over all locations of a specific CI group (across all methods and timepoints) of a single species
	// used in update of gamma of deterministic model
	for (size_t i = 0; i < Data.size(); ++i) {
		const auto &method = Data[i];
		if (!method.hasDataForSpeciesID(_speciesIDinUniqueContainer)) { continue; }
		if (!method.hasDataForCIGroup(CI_index)) { continue; }
		size_t index_in_counts = method.getIndexInCounts(_speciesIDinUniqueContainer);
		// loop over locations with that CI group
		for (const auto j_in_method : method.getLocationIndicesForCIGroup(CI_index)) {
			_updateTryLL(Data, index_in_counts, i, j_in_method);
		}
	}
	return coretools::containerSum(_tryLL) - coretools::containerSum(_curLL);
}

double TModelBase::calculateLLRatio_perLocation(size_t j, const TData &Data) {
	// loop over all methods and timepoints of a particular location of a single species
	// used in update of phi in stochastic model
	for (size_t i = 0; i < Data.size(); ++i) {
		const auto &method = Data[i];
		// check if method has data for this species and the location with index j
		if (!method.hasDataForSpeciesID(_speciesIDinUniqueContainer)) { continue; }
		if (!method.hasDataForLocation(j)) { continue; }
		size_t index_in_counts = method.getIndexInCounts(_speciesIDinUniqueContainer);
		size_t j_in_method     = method.getLocationIndexInMethod(j);
		_updateTryLL(Data, index_in_counts, i, j_in_method);
	}
	return coretools::containerSum(_tryLL) - coretools::containerSum(_curLL);
}

double TModelBase::calculateLLRatio_perMethod(size_t i, const TData &Data) {
	// loop over all locations and timepoints of a particular method of a single species
	const auto &method = Data[i];
	if (!method.hasDataForSpeciesID(_speciesIDinUniqueContainer)) { return 0.0; }

	for (size_t j = 0; j < method.size(); ++j) { // loop over locations
		size_t index_in_counts = method.getIndexInCounts(_speciesIDinUniqueContainer);
		_updateTryLL(Data, index_in_counts, i, j);
	}

	return coretools::containerSum(_tryLL) - coretools::containerSum(_curLL);
}

void TModelBase::swapTryCur() {
	// used when all datapoints are affected (update of sigma)
	std::swap(_tryLL, _curLL);
}

void TModelBase::swapTryCur_perCIGroup(size_t CI_index, const TData &Data) {
	// used for update of deterministic gamma
	for (size_t i = 0; i < Data.size(); ++i) {
		const auto &method = Data[i];
		if (method.hasDataForSpeciesID(_speciesIDinUniqueContainer) && method.hasDataForCIGroup(CI_index)) {
			for (const auto j_in_method : method.getLocationIndicesForCIGroup(CI_index)) {
				size_t ix_linNumMethLoc = Data.i_j_to_linear(_speciesIDinUniqueContainer, i, j_in_method);
				std::swap(_tryLL[ix_linNumMethLoc], _curLL[ix_linNumMethLoc]);
			}
		}
	}
}

void TModelBase::swapTryCur_perLocation(size_t j, const TData &Data) {
	// used when a location has changed
	for (size_t i = 0; i < Data.size(); ++i) {
		const auto &method = Data[i];
		if (method.hasDataForSpeciesID(_speciesIDinUniqueContainer) && method.hasDataForLocation(j)) {
			size_t j_in_method      = method.getLocationIndexInMethod(j);
			size_t ix_linNumMethLoc = Data.i_j_to_linear(_speciesIDinUniqueContainer, i, j_in_method);
			std::swap(_tryLL[ix_linNumMethLoc], _curLL[ix_linNumMethLoc]);
		}
	}
}

void TModelBase::swapTryCur_perMethod(size_t i, const TData &Data) {
	// used when a method has changed
	const auto &method = Data[i];
	if (!method.hasDataForSpeciesID(_speciesIDinUniqueContainer)) { return; }
	for (size_t j = 0; j < method.size(); ++j) { // loop over locations
		size_t ix_linNumMethLoc = Data.i_j_to_linear(_speciesIDinUniqueContainer, i, j);
		std::swap(_tryLL[ix_linNumMethLoc], _curLL[ix_linNumMethLoc]);
	}
}

double TModelBase::curLL(size_t ij) const { return _curLL[ij]; }

double TModelBase::curLLPerMethod(size_t i, const TData &Data) {
	// used when a method has changed
	const auto &method = Data[i];
	if (!method.hasDataForSpeciesID(_speciesIDinUniqueContainer)) { return 0.0; }
	double LL = 0.0;
	for (size_t j = 0; j < method.size(); ++j) { // loop over locations
		size_t ix_linNumMethLoc = Data.i_j_to_linear(_speciesIDinUniqueContainer, i, j);
		LL += _curLL[ix_linNumMethLoc];
	}
	return LL;
}

double TModelBase::sumCurLL() const { return coretools::containerSum(_curLL); }

void TModelBase::setCurLL(size_t ij, double Value) { _curLL[ij] = Value; }

void TModelBase::setCurLLPerMethod(const std::vector<double> &Values, size_t i, const TData &Data) {
	// used when a method has changed
	const auto &method = Data[i];
	if (!method.hasDataForSpeciesID(_speciesIDinUniqueContainer)) { return; }
	for (size_t j = 0; j < method.size(); ++j) { // loop over locations
		size_t ix_linNumMethLoc  = Data.i_j_to_linear(_speciesIDinUniqueContainer, i, j);
		_curLL[ix_linNumMethLoc] = Values[j];
	}
}

//------------------------------------------
// TDeterministic
//------------------------------------------

TDeterministic::TDeterministic(size_t SpeciesIDinUniqueContainer, const TData &Data,
                               const std::unique_ptr<TBirpNegBinomAddOn> &NegBinomAddOn, TypeParamGamma *Gamma,
                               const TUniqueContainer<TypeTime> &Timepoints,
                               const TUniqueContainer<std::string> &CIGroupNames, std::string_view Prefix)
    : TModelBase(SpeciesIDinUniqueContainer, Data, NegBinomAddOn),
      _gamma(Gamma, Timepoints, CIGroupNames, SpeciesIDinUniqueContainer, Prefix) {}

void TDeterministic::initialize(const TData &Data, const TUniqueContainer<std::string> &Locations,
                                const TUniqueContainer<TypeTime> &TimePoints, TBirpPrior *BirpPrior) {
	_initializeDistribution(Data, Locations, BirpPrior);
	_gamma.initialize(TimePoints, BirpPrior);

	_newPhi.resize(_gamma.numCIGroups(), std::vector<TypePhi>(_gamma.numTimepoints()));
	_oldPhi = _newPhi;
}

void TDeterministic::estimateInitial(const TData &Data, const TUniqueContainer<TypeTime> &Timepoints) {
	_estimateInitialParameters(Data);
	_gamma.estimateInitialGamma(Data, Timepoints);
	for (size_t i = 0; i < _gamma.numCIGroups(); ++i) { _fillPhi(i); }
	_oldPhi = _newPhi;
}

void TDeterministic::initializeTempVariables(const TData &Data) {
	_gamma.initializeJP(Data, *this);
	_initializeLL(Data);
}

void TDeterministic::_fillPhi(size_t CI_ix) { _newPhi[CI_ix] = _gamma.phi(CI_ix); }

TypePhi TDeterministic::phi(size_t /*location_id*/, size_t timepoint_id, size_t CI_index) const {
	return deterministicPhi(timepoint_id, CI_index);
}

TypePhi TDeterministic::deterministicPhi(size_t timepoint_id, size_t CI_index) const {
	return _newPhi[CI_index][timepoint_id];
}

void TDeterministic::update(const TData &Data) {
	// update distribution parameters
	_updateDistributionParameters(Data);

	for (size_t ix = 0; ix < _gamma.size(); ix++) { // loop over all gamma
		if (!_gamma.updateSpecificIndex(ix)) { continue; }

		double logH = 0.0;
		for (const auto CI_index : _gamma.getCIGroups(ix)) {
			_oldPhi[CI_index] = _newPhi[CI_index];
			_fillPhi(CI_index);
			const auto LLRatio         = calculateLLRatio_perCIGroup(CI_index, Data);
			const double logPriorRatio = _gamma.getLogPriorRatio(Data, *this, CI_index); // Jeffrey prior
			logH += LLRatio + logPriorRatio;
		}

		if (_gamma.acceptOrReject(logH, ix)) {
			// accepted
			for (const auto CI_index : _gamma.getCIGroups(ix)) { swapTryCur_perCIGroup(CI_index, Data); }
		} else {
			// reject
			for (const auto CI_index : _gamma.getCIGroups(ix)) { _newPhi[CI_index] = _oldPhi[CI_index]; }
		}
	}

	// update gamma summaries
	_gamma.updateSummaryGammaPosterior();
}

void TDeterministic::_simulate() {
	_gamma.setSimulatedGammaToZero();
	for (size_t i = 0; i < _gamma.numCIGroups(); ++i) { _fillPhi(i); }
}

void TDeterministic::simulate(TData &Data, bool use_n_bar, double n) {
	_simulate();
	_simulateDistributionParameters(Data, use_n_bar, n);
}

void TDeterministic::simulateConditioned(TData &Data) {
	_simulate();
	_simulateConditionedDistributionParameters(Data);
}

// write posterior summaries of gamma's
void TDeterministic::resetSummaryGammaPosterior() { _gamma.resetSummaryGammaPosterior(); }

void TDeterministic::writeSummaryGammaPosterior(const std::string &Prefix) {
	_gamma.writeSummaryGammaPosterior(Prefix);
}

//------------------------------------------
// TStochastic
//------------------------------------------

TStochastic::TStochastic(size_t SpeciesIDinUniqueContainer, const TData &Data, size_t NumLocations,
                         size_t NumTimePoints, const std::unique_ptr<TBirpNegBinomAddOn> &NegBinomAddOn,
                         TypeParamGamma *Gamma, TypeParamLogPhi *LogPhi, TypeParamLogSigma *LogSigma,
                         const TUniqueContainer<TypeTime> &Timepoints,
                         const TUniqueContainer<std::string> &CIGroupNames, std::string_view Prefix)
    : TModelBase(SpeciesIDinUniqueContainer, Data, NegBinomAddOn),
      _prior(LogPhi, LogSigma, Gamma, Timepoints, CIGroupNames, SpeciesIDinUniqueContainer, Prefix), _logPhi(LogPhi),
      _logSigma(LogSigma) {

	_jointUpdater = std::make_unique<stattools::TUpdateUnique<TypeLogPhi, false>>(
	    NumLocations * NumTimePoints, true, std::make_unique<stattools::TPropKernelNormal<TypeLogPhi, double>>(),
	    "jointLogPhi");
	_jointUpdater->setProposalWidth("0.1");
}

void TStochastic::initialize(const TData &Data, const TUniqueContainer<std::string> &Locations,
                             const TUniqueContainer<TypeTime> &TimePoints, TBirpPrior *BirpPrior) {
	_initializeDistribution(Data, Locations, BirpPrior);
	_prior.initialize(TimePoints, BirpPrior);

	// create name classes for locations
	auto locNames = std::make_shared<coretools::TNamesStrings>(Locations.vec());
	std::vector<std::string> tmp(TimePoints.size());
	for (size_t t = 0; t < TimePoints.size(); ++t) { tmp[t] = coretools::str::toString(TimePoints[t]); }
	auto timeNames = std::make_shared<coretools::TNamesStrings>(tmp);

	_logPhi->initStorage(BirpPrior, {Locations.size(), TimePoints.size()}, {locNames, timeNames});
	_logSigma->initStorage(BirpPrior);
}

void TStochastic::_setPhiFromDeterministic(const TData &Data) {
	// calculate deterministic phi for all CI groups
	std::vector<std::vector<TypePhi>> deterministicPhi(_prior.numCIGroups());
	for (size_t i = 0; i < _prior.numCIGroups(); ++i) { deterministicPhi[i] = _prior.calculateDeterministicPhi(i); }

	for (size_t j = 0; j < _logPhi->dimensions()[0]; ++j) {     // loop over locations
		for (size_t k = 0; k < _logPhi->dimensions()[1]; ++k) { // loop over timepoints
			// get CI group for that location
			const size_t CI_index = Data.locationsId_to_CI_index(_speciesIDinUniqueContainer, j);
			const auto index      = _logPhi->getIndex({j, k}); // get linear index
			_logPhi->set(index, log(deterministicPhi[CI_index][k]));
		}
	}
}

void TStochastic::estimateInitial(const TData &Data, const TUniqueContainer<TypeTime> &Timepoints) {
	_estimateInitialParameters(Data);
	_prior.estimateInitialGamma(Data, Timepoints);

	// initialize phi based on deterministic phi
	_setPhiFromDeterministic(Data);

	// set sigma
	_logSigma->set(0.0);
}

void TStochastic::initializeTempVariables(const TData &Data) {
	_prior.initializeTempVariables(Data, *this, true);
	_initializeLL(Data);
}

void TStochastic::_updateLogPhi(size_t j, size_t k, const TData &Data) {
	// propose a new value
	const size_t ix = _logPhi->getIndex({j, k});
	const coretools::TRange range(ix);
	_logPhi->propose(range);

	const size_t CI_index = Data.locationsId_to_CI_index(_speciesIDinUniqueContainer, j);

	const auto logPriorRatio =
	    _prior.calculateLogPriorRatio(j, k, CI_index) + _prior.calculateLogPriorRatio(j, k + 1, CI_index);
	// only need to calculate LL for this location j and timepoint k, but across all methods (if they have data for j
	// and k)
	const auto LLRatio = calculateLLRatio_perLocation(j, Data);
	const double logH  = LLRatio + logPriorRatio;
	if (_logPhi->acceptOrReject(logH, range)) {
		_prior.swapTryCur(j, k);
		_prior.swapTryCur(j, k + 1);
		swapTryCur_perLocation(j, Data);
	}
}

void TStochastic::_updateLogPhi(const TData &Data) {
	for (size_t j = 0; j < _logPhi->dimensions()[0]; ++j) { // loop over all locations
		for (size_t k = 1; k < _logPhi->dimensions()[1]; ++k) {
			// loop over all timepoints (ignore first, since this is always 1)
			_updateLogPhi(j, k, Data);
		}
	}
}

void TStochastic::_jointUpdateLogPhiGamma(const TData &Data) {
	if (!_prior.gammaIsUpdated()) { return; }

	for (size_t ix = 0; ix < _prior.size(); ix++) { // loop over all gamma
		// propose new gamma
		_prior.updateGamma_forLogPhiJointUpdate(ix);

		// adjust logPhi by Delta_k, per CI group
		double LLRatio = 0.0;
		for (const auto CI_index : _prior.getCIGroups(ix)) {
			const auto Delta = _prior.getDelta_forLogPhiJointUpdate(CI_index);
			// loop over all locations j for that CI group
			for (const auto j : Data.get_locationsIds_for_CI_index(_speciesIDinUniqueContainer, CI_index)) {
				for (size_t k = 1; k < _logPhi->dimensions()[1]; ++k) { // loop over all timepoints
					const size_t linear     = _logPhi->getIndex({j, k});
					const double logPhi_new = _logPhi->value(linear) + Delta[k];
					_logPhi->set(linear, logPhi_new);
				}
			}
			// add to LLRatio: no need to add prior on phi (= transition probabilities), since they didn't change!
			LLRatio += calculateLLRatio_perCIGroup(CI_index, Data);
		}

		if (_prior.evaluateGamma_forLogPhiJointUpdate(ix, LLRatio, Data, *this)) {
			// accepted: swap LL for all locations of that CI group
			for (const auto CI_index : _prior.getCIGroups(ix)) { swapTryCur_perCIGroup(CI_index, Data); }
		} else {
			// rejected: re-set all logPhi that were changed
			for (const auto CI_index : _prior.getCIGroups(ix)) {
				for (const auto j : Data.get_locationsIds_for_CI_index(_speciesIDinUniqueContainer, CI_index)) {
					for (size_t k = 1; k < _logPhi->dimensions()[1]; ++k) { // loop over all timepoints
						_logPhi->reset(_logPhi->getIndex({j, k}));
					}
				}
			}
		}
	}
	// update summaries of gamma posterior after update has finished
	_prior.updateSummaryGammaPosterior();
}

void TStochastic::_shiftLogPhis(size_t j, size_t k, double diff) {
	for (size_t l = k; l < _logPhi->dimensions()[1]; ++l) {
		const size_t ix = _logPhi->getIndex({j, l});
		_logPhi->set(ix, _logPhi->value(ix) + diff);
	}
}

void TStochastic::_jointUpdateLogPhi(size_t j, size_t k, const TData &Data) {
	// propose a new value
	const size_t ix = _logPhi->getIndex({j, k});
	auto diff       = _jointUpdater->propose(0.0, ix);

	// calculate difference and add this difference to all logPhi on the right (including current phi)
	_shiftLogPhis(j, k, diff);

	// only transition probability from previous matters!
	const size_t CI_index    = Data.locationsId_to_CI_index(_speciesIDinUniqueContainer, j);
	const auto logPriorRatio = _prior.calculateLogPriorRatio(j, k, CI_index);

	// only need to calculate LL for this location j and timepoint k, but across all methods (if they have data for
	// j and k)
	const auto LLRatio  = calculateLLRatio_perLocation(j, Data);
	const double logH   = LLRatio + logPriorRatio;
	const size_t ixLast = _logPhi->getIndex({j, _logPhi->dimensions()[1] - 1});
	coretools::TRange range(ix, ixLast + 1, 1);
	if (_logPhi->acceptOrReject(logH, range, coretools::TRange())) { // don't count rejection
		_prior.swapTryCur(j, k);
		swapTryCur_perLocation(j, Data);
	} else {
		_jointUpdater->reject(ix);
	}
}

void TStochastic::_jointUpdateLogPhi(const TData &Data) {
	for (size_t j = 0; j < _logPhi->dimensions()[0]; ++j) { // loop over all locations
		for (size_t k = 1; k < _logPhi->dimensions()[1]; ++k) {
			// loop over all timepoints (ignore first, since this is always 1)
			_jointUpdateLogPhi(j, k, Data);
		}
	}
}

void TStochastic::_updateLogSigma(const TData &Data) {
	// propose a new value
	const coretools::TRange range(0);
	_logSigma->propose(range);

	double LLRatio = 0.0;
	for (size_t j = 0; j < _logPhi->dimensions()[0]; ++j) { // loop over all locations
		for (size_t k = 1; k < _logPhi->dimensions()[1]; ++k) {
			const size_t CI_index = Data.locationsId_to_CI_index(_speciesIDinUniqueContainer, j);
			LLRatio += _prior.calculateLogPriorRatio(j, k, CI_index);
		}
	}

	const double logH = LLRatio + _logSigma->getLogDensityRatio();
	if (_logSigma->acceptOrReject(logH, range)) {
		// accepted: update all transition probabilities
		_prior.swapTryCur();
	}
}

void TStochastic::update(const TData &Data) {
	// update distribution parameters
	_updateDistributionParameters(Data);

	// update gamma
	_prior.updateGamma(Data, *this);

	// update logPhi
	if (_logPhi->isUpdated()) {
		_updateLogPhi(Data);
		_jointUpdateLogPhiGamma(Data);
		_jointUpdateLogPhi(Data);
	}

	// update logSigma
	if (_logSigma->isUpdated()) { _updateLogSigma(Data); }
}

void TStochastic::_simulate(TData &Data) {
	// simulate gamma
	_prior.setSimulatedGammaToZero();
	// set logSigma to something small (only if it has not been specified by the user)
	_logSigma->set(-1.0);

	_prior.initializeTempVariables(Data, *this, false);

	// simulate phi from transition probabilities
	for (size_t j = 0; j < _logPhi->dimensions()[0]; ++j) { // loop over all locations
		size_t ix = _logPhi->getIndex({j, 0});
		_logPhi->set(ix, 0.0); // set first phi = 1 -> logPhi = 0
		const size_t CI_index = Data.locationsId_to_CI_index(_speciesIDinUniqueContainer, j);
		for (size_t k = 1; k < _logPhi->dimensions()[1]; ++k) {
			ix = _logPhi->getIndex({j, k});
			_logPhi->set(ix, _prior.sampleFromTransitionProbability(j, k, CI_index));
		}
	}
}

void TStochastic::simulate(TData &Data, bool use_n_bar, double n) {
	_simulate(Data);
	_simulateDistributionParameters(Data, use_n_bar, n);
}

void TStochastic::simulateConditioned(TData &Data) {
	_simulate(Data);
	_simulateConditionedDistributionParameters(Data);
}

coretools::StrictlyPositive TStochastic::phi(size_t j, size_t k, size_t /*CI_index*/) const {
	const double phi = exp(_logPhi->value(_logPhi->getIndex({j, k})));
	if (phi < coretools::StrictlyPositive::min()) { return coretools::StrictlyPositive::min(); }
	if (phi > coretools::StrictlyPositive::max()) { return coretools::StrictlyPositive::max(); }
	return phi;
}

TypePhi TStochastic::deterministicPhi(size_t timepoint_id, size_t CI_index) const {
	return _prior.deterministicPhi(timepoint_id, CI_index);
}

// write posterior summaries of gamma's
void TStochastic::resetSummaryGammaPosterior() { _prior.resetSummaryGammaPosterior(); }

void TStochastic::writeSummaryGammaPosterior(const std::string &Prefix) { _prior.writeSummaryGammaPosterior(Prefix); }
