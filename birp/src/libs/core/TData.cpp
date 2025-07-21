#include "TModels.h" // note: needs to be included first to avoid include issues with Rcpp and RcppArmadillo
#include "TData.h"
#include "coretools/Distributions/TGammaDistr.h"
#include "coretools/Distributions/TNormalDistr.h"
#include "coretools/Distributions/TUniformDistr.h"
#include "stattools/Updates/TPairIndexSampler.h"
#include <set>
#include <utility>

using namespace coretools::instances;

//-------------------------------------------
// TTimepoints
//-------------------------------------------

TTimepoints::TTimepoints(const std::vector<TypeCounts> &Counts, const std::vector<TypeCovariateEffort> &CovsEffort,
                         const std::vector<TypeCovariateDetection> &CovsDetection, const size_t &Timepoint_id)
    : _counts_per_species(Counts), _covariatesEffort(CovsEffort), _covariatesDetection(CovsDetection),
      _timepoint_id(Timepoint_id) {

	// we (temporarily) only allow a single speces
	if (_counts_per_species.size() > 1) { throw coretools::TUserError("Currently only supports single species!"); }
}

TTimepoints::TTimepoints(size_t timepoint_id, size_t numSpecies, size_t numCovariatesEffort,
                         size_t numCovariatesDetection) {
	_timepoint_id = timepoint_id;
	_counts_per_species.resize(numSpecies);
	_covariatesEffort.resize(numCovariatesEffort);
	_covariatesDetection.resize(numCovariatesDetection);
}

const std::vector<TypeCovariateEffort> &TTimepoints::covariatesEffort() const { return _covariatesEffort; }

const std::vector<TypeCovariateDetection> &TTimepoints::covariatesDetection() const { return _covariatesDetection; }

size_t TTimepoints::numSpecies() const { return _counts_per_species.size(); }

TypeCounts TTimepoints::counts_per_species(size_t species_id) const { return _counts_per_species[species_id]; }

TypeEffort TTimepoints::effort() const { return _effort; }

size_t TTimepoints::timepoint_id() const { return _timepoint_id; }

void TTimepoints::setTimepoint(size_t timepointToAdd) { _timepoint_id = timepointToAdd; }
void TTimepoints::setCounts(size_t species_id, TypeCounts counts) { _counts_per_species[species_id] = counts; }

void TTimepoints::setCovariateEffort(size_t c, TypeCovariateEffort Covariate) { _covariatesEffort[c] = Covariate; }
void TTimepoints::setCovariateDetection(size_t c, TypeCovariateDetection Covariate) {
	_covariatesDetection[c] = Covariate;
}

bool TTimepoints::inferAlpha() const {
	// do not infer if single covariate for effort
	return _covariatesEffort.size() > 1;
}

bool TTimepoints::inferBeta() const {
	// do infer if single for detection
	return !_covariatesDetection.empty();
}

void TTimepoints::_standardizeCovariatesDetection(const std::vector<coretools::TMeanVar<double>> &MeanVar,
                                                  bool AssumeTrueDetectionProbability) {
	// standardize detection probability covariates with standard score (mean zero, unit variance)
	for (size_t d = 0; d < _covariatesDetection.size(); ++d) {
		if (AssumeTrueDetectionProbability) { // do not standardize, but take logit instead
			_covariatesDetection[d] = coretools::logit((coretools::Probability)_covariatesDetection[d]);
		} else {
			_covariatesDetection[d] = (_covariatesDetection[d] - MeanVar[d].mean()) / MeanVar[d].sd();
		}
	}
}

void TTimepoints::_standardizeCovariatesEffort(const std::vector<double> &SumsPerCovariate, size_t NumTimepoints,
                                               size_t NumLocations) {
	assert(!_covariatesEffort.empty()); // should always have at least one effort covariate

	// standardize effort covariates such that they sum to one
	for (size_t c = 0; c < _covariatesEffort.size(); ++c) {
		_covariatesEffort[c] =
		    _covariatesEffort[c] / SumsPerCovariate[c] * (double)NumTimepoints * (double)NumLocations;
	}
}

void TTimepoints::standardizeCovariates(const std::vector<coretools::TMeanVar<double>> &MeanVar,
                                        const std::vector<double> &SumsPerCovariate,
                                        bool AssumeTrueDetectionProbability, size_t NumTimepoints,
                                        size_t NumLocations) {
	_standardizeCovariatesDetection(MeanVar, AssumeTrueDetectionProbability);
	_standardizeCovariatesEffort(SumsPerCovariate, NumTimepoints, NumLocations);
}

double TTimepoints::_calculateEffortTerm(const std::shared_ptr<TypeParamAlpha> &Alpha) {
	assert(!_covariatesEffort.empty());
	if (!inferAlpha()) { return _covariatesEffort[0]; }

	// calculate sum_{c=1}^C alpha_{ic} x_{ijkc}
	double effortTerm = 0.0;
	for (size_t c = 0; c < _covariatesEffort.size(); ++c) { effortTerm += Alpha->value(c) * _covariatesEffort[c]; }
	return effortTerm;
}

double TTimepoints::_calculateDetectionTerm(const std::shared_ptr<TypeParamBeta> &Beta0,
                                            const std::shared_ptr<TypeParamBeta> &Beta) {
	if (_covariatesDetection.empty()) { return 1.0; }

	// calculate logistic(beta_{i0} + sum_{d=1}^D * beta_{id} y_{ijkd})
	double detectionTerm = Beta0->value();
	for (size_t d = 0; d < _covariatesDetection.size(); ++d) {
		detectionTerm += Beta->value(d) * _covariatesDetection[d];
	}
	return coretools::logistic(detectionTerm);
}

void TTimepoints::initializeEffort(const std::shared_ptr<TypeParamAlpha> &Alpha,
                                   const std::shared_ptr<TypeParamBeta> &Beta0,
                                   const std::shared_ptr<TypeParamBeta> &Beta) {
	// update alpha and beta
	// calculate s_{ijk} = sum_{c=1}^C alpha_{ic} x_{ijkc} * logistic(beta_{i0} + sum_{d=1}^D * beta_{id} y_{ijkd})
	_effortTerm    = _calculateEffortTerm(Alpha);
	_detectionTerm = _calculateDetectionTerm(Beta0, Beta);
	_effort        = _effortTerm * _detectionTerm;

	// also set old
	_effortTerm_old    = _effortTerm;
	_detectionTerm_old = _detectionTerm;
	_effort_old        = _effort;
}

void TTimepoints::updateAlphaEffort(TypeAlpha NewAlpha_c1, TypeAlpha OldAlpha_c1, TypeAlpha NewAlpha_c2,
                                    TypeAlpha OldAlpha_c2, size_t c1, size_t c2) {
	// remember current effort
	_effort_old     = _effort;
	_effortTerm_old = _effortTerm;

	// update effort: no need to re-calculate sum over all alpha, just update with difference for both alpha
	// s_{ijk}' = s_{ijk} + detection_term * x_{ijkc} * (alpha_{ic}' - alpha_{ic})
	_effortTerm += _covariatesEffort[c1] * ((double)NewAlpha_c1 - (double)OldAlpha_c1) +
	               _covariatesEffort[c2] * ((double)NewAlpha_c2 - (double)OldAlpha_c2);
	_effort = _effortTerm * _detectionTerm;
}

void TTimepoints::updateBetaEffort(const std::shared_ptr<TypeParamBeta> &Beta0,
                                   const std::shared_ptr<TypeParamBeta> &Beta) {
	// remember current effort
	_effort_old        = _effort;
	_detectionTerm_old = _detectionTerm;

	_detectionTerm = _calculateDetectionTerm(Beta0, Beta);
	_effort        = _effortTerm * _detectionTerm;
}

void TTimepoints::resetAlphaEffort() {
	// this function is called in case alpha update is rejected
	_effortTerm = _effortTerm_old;
	_effort     = _effort_old;
}

void TTimepoints::resetBetaEffort() {
	// this function is called in case beta update is rejected
	_detectionTerm = _detectionTerm_old;
	_effort        = _effort_old;
}

void TTimepoints::write(coretools::TOutputMaybeRcppFile &File, std::string location,
                        const TUniqueContainer<TypeTime> &Timepoints, const std::string &CIGroupName) const {
	File << location << Timepoints[_timepoint_id] << CIGroupName << _counts_per_species;

	// write covariate(s)
	for (const auto &it : _covariatesEffort) { File << it; }
	for (const auto &it : _covariatesDetection) { File << it; }

	File << coretools::endl;
}

double TTimepoints::calculatePhiEffort(double phi) {
	coretools::Positive lambda = phi * _effort; // note: without N0 or mu
	return lambda;
}

void TTimepoints::simulatePoisson(double N, double phi) {
	coretools::Positive lambda = N * phi * _effort;

	if (lambda == 0.0) {
		_counts_per_species[0] = 0;
	} else {
		_counts_per_species[0] = randomGenerator().getPoissonRandom((double)lambda);
	}
}

void TTimepoints::simulateNB(double phi, double a, double N) {
	// first draw lambda from a Gamma distr and then from there draw counts from Pois
	double alpha           = N * phi * _effort / a;
	double beta            = 1.0 / a;
	double lambda          = randomGenerator().getGammaRand(alpha, beta);
	_counts_per_species[0] = randomGenerator().getPoissonRandom(lambda);
}

//-------------------------------------------
// TLocations
//-------------------------------------------

TLocations::TLocations(const TTimepoints &pt, size_t location_id, size_t group_id) {
	_location_id = location_id;
	_group_id    = group_id;
	add(pt);
}

TLocations::TLocations(size_t location_id, size_t group_id, size_t TimepointSize, size_t numSpecies,
                       size_t numCovariatesEffort, size_t numCovariatesDetection) {
	_location_id = location_id;
	_group_id    = group_id;
	for (size_t t = 0; t < TimepointSize; t++) {
		_timepoints.emplace_back(t, numSpecies, numCovariatesEffort, numCovariatesDetection);
	}
}

size_t TLocations::size() const { return _timepoints.size(); }

TypeNu TLocations::nu(size_t species_id) const { return _nu[species_id]; }

size_t TLocations::location_id() const { return _location_id; }
size_t TLocations::group_id() const { return _group_id; }

const TTimepoints &TLocations::operator[](size_t index) const { return _timepoints[index]; }

void TLocations::add(const TTimepoints &pt) {
	for (auto &t : _timepoints) {
		if (t.timepoint_id() == pt.timepoint_id()) {
			throw coretools::TUserError("Make sure your input file does not contain multiple rows with identical location and timepoints");
		}
	}
	// when we are here: timpoint is unique -> push it back
	_timepoints.push_back(pt);
	// now add nu, first check if nu exists, otherwise create it
	if (_nu.empty()) { _nu.resize(pt.numSpecies(), 0); }
	for (size_t i = 0; i < pt.numSpecies(); i++) { _nu[i] += (uint64_t)pt.counts_per_species(i); }
}

void TLocations::sorttimes(const std::vector<size_t> &index) {
	std::vector<size_t> positionOfTimepointIds;
	for (auto &timepoint : _timepoints) {
		auto it    = std::find(index.begin(), index.end(), timepoint.timepoint_id());
		size_t pos = it - index.begin();
		positionOfTimepointIds.push_back(pos);
	}
	// sort the newly found positions of timepoints using ranksort()
	std::vector<size_t> sortingIndex = coretools::rankSort(positionOfTimepointIds);
	// sort _timepoints
	_timepoints                      = coretools::sortContainerByRank(_timepoints, sortingIndex);
	// now sort positionOfTimepointIds
	positionOfTimepointIds           = coretools::sortContainerByRank(positionOfTimepointIds, sortingIndex);
	// now set timepoint_id
	for (size_t i = 0; i < _timepoints.size(); i++) { _timepoints[i].setTimepoint(positionOfTimepointIds[i]); }
}

void TLocations::write(coretools::TOutputMaybeRcppFile &File, const TUniqueContainer<std::string> &Locations,
                       const TUniqueContainer<TypeTime> &Timepoints,
                       const TUniqueContainer<std::string> &CIGroupNames) const {
	const std::string &location      = Locations[_location_id];
	const std::string &CI_group_name = CIGroupNames[_group_id];
	for (const auto &timepoint : _timepoints) { timepoint.write(File, location, Timepoints, CI_group_name); }
}

std::vector<TTimepoints>::const_iterator TLocations::cbegin() const { return _timepoints.cbegin(); }

std::vector<TTimepoints>::iterator TLocations::begin() { return _timepoints.begin(); }

std::vector<TTimepoints>::const_iterator TLocations::cend() const { return _timepoints.cend(); }

std::vector<TTimepoints>::iterator TLocations::end() { return _timepoints.end(); }

void TLocations::initializeEffort(const std::shared_ptr<TypeParamAlpha> &Alpha,
                                  const std::shared_ptr<TypeParamBeta> &Beta0,
                                  const std::shared_ptr<TypeParamBeta> &Beta) {
	for (auto &timepoint : _timepoints) { timepoint.initializeEffort(Alpha, Beta0, Beta); }
}

void TLocations::updateAlphaEffort(TypeAlpha NewAlpha_c1, TypeAlpha OldAlpha_c1, TypeAlpha NewAlpha_c2,
                                   TypeAlpha OldAlpha_c2, size_t c1, size_t c2) {
	for (auto &timepoint : _timepoints) {
		timepoint.updateAlphaEffort(NewAlpha_c1, OldAlpha_c1, NewAlpha_c2, OldAlpha_c2, c1, c2);
	}
}

void TLocations::updateBetaEffort(const std::shared_ptr<TypeParamBeta> &Beta0,
                                  const std::shared_ptr<TypeParamBeta> &Beta) {
	for (auto &timepoint : _timepoints) { timepoint.updateBetaEffort(Beta0, Beta); }
}

void TLocations::resetAlphaEffort() {
	for (auto &timepoint : _timepoints) { timepoint.resetAlphaEffort(); }
}

void TLocations::resetBetaEffort() {
	for (auto &timepoint : _timepoints) { timepoint.resetBetaEffort(); }
}

void TLocations::simulatePoisson(double N, const TModelBase &Model) {
	for (auto &timepoint : _timepoints) {
		timepoint.simulatePoisson(N, Model.phi(location_id(), timepoint.timepoint_id(), group_id()));
	}
}

void TLocations::simulateMultinomial(const TModelBase &Model) {
	// get p_ijk
	std::vector<double> p(_timepoints.size());
	for (size_t k = 0; k < _timepoints.size(); ++k) { // phi * effort
		p[k] = Model.phi(location_id(), _timepoints[k].timepoint_id(), group_id()) * _timepoints[k].effort();
	}
	coretools::normalize(p);

	// simulate multinomial
	for (size_t i = 0; i < _nu.size(); i++) { // per species
		std::vector<size_t> counts(_timepoints.size());
		randomGenerator().fillMultinomialRandomRawProbabilities(_nu[i], p, counts);

		// fill timepoints
		for (size_t k = 0; k < _timepoints.size(); ++k) { _timepoints[k].setCounts(i, counts[k]); }
	}
}

void TLocations::simulateNB(const TModelBase &Model, double a, double N) {
	for (auto &timepoint : _timepoints) {
		timepoint.simulateNB(Model.phi(location_id(), timepoint.timepoint_id(), group_id()), a, N);
	}
}

void TLocations::simulateDirichletMultinomial(const TModelBase &Model, double mu, double b) {
	// get alpha
	std::vector<double> alpha(_timepoints.size());
	for (size_t k = 0; k < _timepoints.size(); ++k) { // phi * effort
		alpha[k] =
		    mu / b * Model.phi(location_id(), _timepoints[k].timepoint_id(), group_id()) * _timepoints[k].effort();
	}

	// simulate dirichlet-multinomial
	for (size_t i = 0; i < _nu.size(); i++) { // per species
		// Dirichlet
		std::vector<double> p(_timepoints.size());
		randomGenerator().fillDirichletRandom(alpha, p);
		// Multinomial
		std::vector<size_t> counts(_timepoints.size());
		randomGenerator().fillMultinomialRandomRawProbabilities(_nu[i], p, counts);

		// fill timepoints
		for (size_t k = 0; k < _timepoints.size(); ++k) { _timepoints[k].setCounts(i, counts[k]); }
	}
}

//-------------------------------------------
// TMethods
//-------------------------------------------

TMethods::TMethods(const std::string_view &methodName, size_t index,
                   const std::vector<std::size_t> &speciesIDsinUniqueContainer,
                   const std::vector<std::size_t> &covariateEffortIDsinUniqueContainer,
                   const std::vector<std::size_t> &covariateDetectionIDsinUniqueContainer)
    : _methodName(methodName), _i(index), _covariateEffortIDsinUniqueContainer(covariateEffortIDsinUniqueContainer),
      _covariateDetectionIDsinUniqueContainer(covariateDetectionIDsinUniqueContainer),
      _speciesIDsinUniqueContainer(speciesIDsinUniqueContainer) { // infer or simulate from file
}

TMethods::TMethods(std::string methodName, size_t locationSize, size_t TimepointSize, size_t index,
                   const std::vector<size_t> &speciesIDsinUniqueContainer,
                   const std::vector<std::size_t> &covariateEffortIDsinUniqueContainer,
                   const std::vector<std::size_t> &covariateDetectionIDsinUniqueContainer, size_t NumCIGroups,
                   const TData &Data)
    : _methodName(std::move(methodName)), _i(index),
      _covariateEffortIDsinUniqueContainer(covariateEffortIDsinUniqueContainer),
      _covariateDetectionIDsinUniqueContainer(covariateDetectionIDsinUniqueContainer),
      _speciesIDsinUniqueContainer(speciesIDsinUniqueContainer) { // simulate

	_locations.reserve(locationSize);
	for (size_t i = 0; i < locationSize; i++) {
		size_t group_id;
		if (Data.size() == 0) { // first method: location does not yet exist
			if (parameters().exists("CI_group_per_location")) {
				if (i >= NumCIGroups) {
					throw coretools::TUserError("More locations (", locationSize, ") than CI groups (", NumCIGroups,
					       ") (argument 'CI_group_per_location').");
				}
				group_id = i;
			} else {
				// draw CI group randomly
				size_t zero = 0; // just to be really sure about compiler error on windows
				group_id = randomGenerator().getRand<size_t>(zero, NumCIGroups);
			}
		} else { // else match CI group of location of previous method
			group_id = Data[Data.size() - 1][i].group_id();
		}
		_locations.emplace_back(i, group_id, TimepointSize, _speciesIDsinUniqueContainer.size(),
		                        covariateEffortIDsinUniqueContainer.size(),
		                        covariateDetectionIDsinUniqueContainer.size());
	}
}

void TMethods::addAlphaBeta(const std::shared_ptr<TypeParamAlpha> &Alpha, const std::shared_ptr<TypeParamBeta> &Beta0,
                            std::shared_ptr<TypeParamBeta> &Beta) {
	_alpha = Alpha;
	_beta0 = Beta0;
	_beta  = Beta;
}

auto getCovNames(const TUniqueContainer<std::string> &CovariateNames, const std::vector<size_t> &ID) {
	auto covNames = std::make_shared<coretools::TNamesStrings>();
	for (auto id : ID) { covNames->addName({CovariateNames[id]}); }
	return covNames;
}

void TMethods::initialize(const TUniqueContainer<std::string> &CovariateEffortNames,
                          const TUniqueContainer<std::string> &CovariateDetectionNames, TBirpPrior *BirpPrior) {
	// initialize alpha: get names from effort covariates
	auto covNamesAlpha = getCovNames(CovariateEffortNames, _covariateEffortIDsinUniqueContainer);
	_alpha->initStorage(BirpPrior, {numCovariatesEffort()}, {covNamesAlpha});

	// initialize beta0: single element
	if (CovariateDetectionNames.empty()) {
		_beta0->initStorage(BirpPrior, {0}, {});
	} else {
		_beta0->initStorage(BirpPrior, {1});
	}

	// initialize beta: get names from detection covariates
	auto covNamesBeta = getCovNames(CovariateDetectionNames, _covariateDetectionIDsinUniqueContainer);
	_beta->initStorage(BirpPrior, {numCovariatesDetection()}, {covNamesBeta});
}

void TMethods::add(const TTimepoints &pt, size_t location_id, size_t group_id,
                   const TUniqueContainer<std::string> &LocationNames,
                   const TUniqueContainer<std::string> &CIGroupNames) {
	bool dataPointExists = false;
	for (auto &loc : _locations) {
		if (loc.location_id() == location_id) {
			if (loc.group_id() != group_id) {
				throw coretools::TUserError("Location '", LocationNames[location_id],
				       "' has been surveyed at multiple timepoints, but has a different control-intervention group (",
				       CIGroupNames[group_id], " vs ", CIGroupNames[loc.group_id()], " depending on the timepoint.");
			}
			dataPointExists = true;
			loc.add(pt);
			break;
		}
	}
	if (!dataPointExists) { _locations.emplace_back(pt, location_id, group_id); }
}

auto TMethods::_getMeanVarDetectionCovariates() {
	// calculate mean and variance across all detection covariates
	// but check per detection covariate for unique values (not allowed)
	const size_t numCovariates = _covariateDetectionIDsinUniqueContainer.size();
	std::vector<coretools::TMeanVar<double>> meanVar(numCovariates);
	std::vector<std::set<double>> uniqueValues_perCovariate(numCovariates);
	for (auto &loc : _locations) { // loop over locations
		for (auto &tp : loc) {     // loop over timepoints
			const auto &covariates = tp.covariatesDetection();
			for (size_t d = 0; d < covariates.size(); ++d) {
				meanVar[d].add(covariates[d]);
				uniqueValues_perCovariate[d].insert(covariates[d]);
			}
		}
	}

	// check if a covariate is the same across all locations and timepoints
	for (const auto &d : uniqueValues_perCovariate) {
		if (d.size() == 1) {
			throw coretools::TUserError("All detection covariates are equal (", *d.begin(),
			       "). Please exclude this covariate or provide non-equal values.");
		}
	}
	return meanVar;
}

auto TMethods::_getSumEffortCovariates() {
	// calculate sum across all locations and timepoints (but per effort covariate)
	const size_t numCovariates = _covariateEffortIDsinUniqueContainer.size();
	std::vector<double> sum(numCovariates, 0.0);
	for (auto &loc : _locations) { // loop over locations
		for (auto &tp : loc) {     // loop over timepoints
			const auto &covariates = tp.covariatesEffort();
			for (size_t c = 0; c < covariates.size(); ++c) { sum[c] += covariates[c]; }
		}
	}
	return sum;
}

bool assumeTrueDetectionProbability() { return parameters().exists("assumeTrueDetectionProbability"); }

void TMethods::standardizeCovariates() {
	auto meanVar = _getMeanVarDetectionCovariates();
	auto sum     = _getSumEffortCovariates();
	if (assumeTrueDetectionProbability()) {
		logfile().list("Will assume that the provided detection probabilities are true, meaning that they will be "
		               "transform to logit and not standardized (argument 'assumeTrueDetectionProbability').");
		// user has provided true detection probabilities -> transform to logit and do not standardize!
		if (_covariateDetectionIDsinUniqueContainer.size() > 1) {
			throw coretools::TUserError("Argument 'assumeTrueDetectionProbability' can only be used if a single detection probability is "
			       "provided");
		}
		// check if all detection are proper probabilities
		for (auto &loc : _locations) {
			for (auto &timepoint : loc) {
				for (auto detection : timepoint.covariatesDetection()) {
					if (detection < 0.0 || detection > 1.0) {
						throw coretools::TUserError("Detection probability must be between [0,1] if argument "
						       "'assumeTrueDetectionProbability' is used (found ",
						       detection, ").");
					}
				}
			}
		}
	} else {
		if (!_covariateDetectionIDsinUniqueContainer.empty()) {
			logfile().list("Will assume that the provided detection probabilities are covariates (use argument "
			               "'assumeTrueDetectionProbability' to change).");
		}
	}

	// now standardize
	for (auto &loc : _locations) {
		for (auto &timepoint : loc) {
			timepoint.standardizeCovariates(meanVar, sum, assumeTrueDetectionProbability(), loc.size(),
			                                _locations.size());
		}
	}
}

void TMethods::fillIDVectors(size_t totalSizeOfUniqueContainer) {
	_hasDataForSpeciesID.resize(totalSizeOfUniqueContainer, false);
	_indexInCounts.resize(totalSizeOfUniqueContainer, 0);
	for (size_t c = 0; c < _speciesIDsinUniqueContainer.size(); c++) {
		_hasDataForSpeciesID[_speciesIDsinUniqueContainer[c]] = true;
		_indexInCounts[_speciesIDsinUniqueContainer[c]]       = c;
	}
}

void TMethods::initializeLookups(const TUniqueContainer<std::string> &Locations,
                                 const TUniqueContainer<std::string> &CIGroupNames) {
	_fillLocationIDs(Locations);
	_fillCIGroupIDs(CIGroupNames);
}

void TMethods::_fillLocationIDs(const TUniqueContainer<std::string> &Locations) {
	_hasDataForLocationID.resize(Locations.size(), false);
	_map_j_location.resize(Locations.size(), 0);
	for (size_t j = 0; j < Locations.size(); ++j) {         // loop over unique container
		for (size_t ix = 0; ix < _locations.size(); ++ix) { // loop over locations of this method
			if (j == _locations[ix].location_id()) {        // check if this j exists
				_hasDataForLocationID[j] = true;
				_map_j_location[j]       = ix;
			}
		}
	}
}

void TMethods::_fillCIGroupIDs(const TUniqueContainer<std::string> &CIGroupNames) {
	_hasDataForCIGroup.resize(CIGroupNames.size(), false);
	_location_per_CI_group.resize(CIGroupNames.size());

	for (size_t i = 0; i < CIGroupNames.size(); ++i) {      // loop over all unique CI groups
		for (size_t ix = 0; ix < _locations.size(); ++ix) { // loop over locations of this method
			if (i == _locations[ix].group_id()) {           // check if this group exists
				_hasDataForCIGroup[i] = true;
				_location_per_CI_group[i].emplace_back(ix);
			}
		}
	}
}

void TMethods::sorttimes(const std::vector<size_t> &sortingIndex) {
	for (auto &loc : _locations) { loc.sorttimes(sortingIndex); }
}

void TMethods::initializeEffort() {
	_alpha->normalize(_alpha->getFull());

	// initialize effort
	for (auto &location : _locations) { location.initializeEffort(_alpha, _beta0, _beta); }
}

size_t TMethods::size() const { return _locations.size(); }
const std::string &TMethods::name() const { return _methodName; }

size_t TMethods::numCovariatesEffort() const { return _covariateEffortIDsinUniqueContainer.size(); }
size_t TMethods::numCovariatesDetection() const { return _covariateDetectionIDsinUniqueContainer.size(); }
bool TMethods::inferAlpha() const { return _locations.cbegin()->cbegin()->inferAlpha(); }
bool TMethods::inferBeta() const { return _locations.cbegin()->cbegin()->inferBeta(); }

bool TMethods::hasDataForSpeciesID(size_t speciesIDinUniqueContainer) const {
	return _hasDataForSpeciesID[speciesIDinUniqueContainer];
}

size_t TMethods::getIndexInCounts(size_t speciesIDinUniqueContainer) const {
	return _indexInCounts[speciesIDinUniqueContainer];
}

bool TMethods::hasDataForLocation(size_t j) const { return _hasDataForLocationID[j]; }
size_t TMethods::getLocationIndexInMethod(size_t j) const { return _map_j_location[j]; }

bool TMethods::hasDataForCIGroup(size_t CI_index) const { return _hasDataForCIGroup[CI_index]; }

const std::vector<size_t> &TMethods::getLocationIndicesForCIGroup(size_t CI_index) const {
	return _location_per_CI_group[CI_index];
}

const std::vector<size_t> &TMethods::speciesIDsinUniqueContainer() const { return _speciesIDsinUniqueContainer; }
const std::vector<size_t> &TMethods::covariateEffortIDsinUniqueContainer() const {
	return _covariateEffortIDsinUniqueContainer;
}
const std::vector<size_t> &TMethods::covariateDetectionIDsinUniqueContainer() const {
	return _covariateDetectionIDsinUniqueContainer;
}

std::vector<TLocations>::const_iterator TMethods::cbegin() const { return _locations.cbegin(); }

std::vector<TLocations>::iterator TMethods::begin() { return _locations.begin(); }

std::vector<TLocations>::const_iterator TMethods::cend() const { return _locations.cend(); }

std::vector<TLocations>::iterator TMethods::end() { return _locations.end(); }

std::vector<std::string> splitVecWithDistributions(const std::string &Input) {
	std::vector<std::string> output;

	std::string pattern;
	if (coretools::str::stringContains(Input, "(") && coretools::str::stringContains(Input, ")")) {
		// is a distribution -> split after ),
		pattern = "),";
	} else {
		// is a comma-separated list of numbers -> split after ,
		pattern = ",";
	}
	for (auto si : coretools::str::TSplitter(Input, pattern)) { output.push_back((std::string)si); }

	return output;
}

bool hasCommasOutsideBracket(const std::string &Input) { return splitVecWithDistributions(Input).size() > 1; }

std::pair<std::string, std::string> TMethods::_getParamsAndDistr(const std::string &Cov) {
	coretools::str::TSplitter sp(Cov, '(');
	std::string_view distr = sp.front();          // takes first element
	sp.popFront();                                // takes 'Poisson' away
	std::string params = (std::string)sp.front(); // now parameters are in front
	params             = coretools::str::extractBefore(params, ')');
	return {(std::string)distr, params};
}

void TMethods::_simulateOneCovariateEffort(size_t c, const std::string &Cov) {
	if (coretools::str::stringIsProbablyANumber(Cov)) {
		// case 1: is a number: use it for all covariates
		auto value = coretools::str::fromString<double>(Cov);
		for (auto &_location : _locations) {
			for (auto &t : _location) { t.setCovariateEffort(c, value); }
		}
	} else {
		// case 2: parse distribution
		const auto [distr, params] = _getParamsAndDistr(Cov);
		if (distr == "gamma") {
			_simulateCovariateFromDistribution<coretools::probdist::TGammaDistr, true>(c, params);
		} else if (distr == "uniform") {
			_simulateCovariateFromDistribution<coretools::probdist::TUniformDistr, true>(c, params);
		} else {
			throw coretools::TUserError("Unknown distribution '", distr,
			       "' (argument 'covariatesEffort'). Supported distributions are: gamma and uniform.");
		}
	}
}

void TMethods::_simulateCovariatesEffort() {
	std::string effortString = "gamma(1, 2)";
	if (parameters().exists("covariatesEffort_" + _methodName)) {
		effortString = parameters().get("covariatesEffort_" + _methodName);
	} else if (parameters().exists("covariatesEffort")) {
		effortString = parameters().get("covariatesEffort");
	}

	logfile().list("Covariates for effort = ", effortString, " (arguments 'covariatesEffort' and '",
	               "covariatesEffort_" + _methodName, "').");

	if (coretools::str::stringIsProbablyANumber(effortString)) {
		// case 1: is a number: use it for all covariates
		for (size_t c = 0; c < numCovariatesEffort(); ++c) { _simulateOneCovariateEffort(c, effortString); }
	} else if (hasCommasOutsideBracket(effortString)) {
		// cases 2 and 4: comma-separated string of numbers (case 2) or distributions (case 4) or both
		std::vector<std::string> vec = splitVecWithDistributions(effortString);
		if (vec.size() != numCovariatesEffort()) {
			throw coretools::TUserError("The size of covariates provided with argument 'covariatesEffort' (", vec.size(),
			       ") does not match the number of covariates (", numCovariatesEffort(), ").");
		}
		for (size_t c = 0; c < vec.size(); ++c) { _simulateOneCovariateEffort(c, vec[c]); }
	} else {
		// case 3: use the same distribution for all covariates
		for (size_t c = 0; c < numCovariatesEffort(); ++c) { _simulateOneCovariateEffort(c, effortString); }
	}
}

void TMethods::_simulateOneCovariateDetection(size_t c, const std::string &Cov) {
	if (coretools::str::stringIsProbablyANumber(Cov)) {
		// case 1: is a number: use it for all covariates
		auto value = coretools::str::fromString<double>(Cov);
		for (auto &_location : _locations) {
			for (auto &t : _location) { t.setCovariateDetection(c, value); }
		}
	} else {
		// case 2: parse distributions. "normal(0, 1), uniform(a,b)
		const auto [distr, params] = _getParamsAndDistr(Cov);
		if (distr == "normal") {
			_simulateCovariateFromDistribution<coretools::probdist::TNormalDistr, false>(c, params);
		} else if (distr == "uniform") {
			_simulateCovariateFromDistribution<coretools::probdist::TUniformDistr, false>(c, params);
		} else {
			throw coretools::TUserError("Unknown distribution '", distr,
			       "' (argument 'covariatesDetection'). Supported distributions are: normal and uniform.");
		}
	}
}

void TMethods::_simulateCovariatesDetection() {
	std::string effortString = "normal(0, 1)";
	if (parameters().exists("covariatesDetection_" + _methodName)) {
		effortString = parameters().get("covariatesDetection_" + _methodName);
	} else if (parameters().exists("covariatesDetection")) {
		effortString = parameters().get("covariatesDetection");
	}

	logfile().list("Covariates for detection probabilities = ", effortString, " (arguments 'covariatesDetection' and '",
	               "covariatesDetection_" + _methodName, "').");

	if (coretools::str::stringIsProbablyANumber(effortString)) {
		// case 1: is a number: use it for all covariates
		for (size_t c = 0; c < numCovariatesDetection(); ++c) { _simulateOneCovariateDetection(c, effortString); }
	} else if (hasCommasOutsideBracket(effortString)) {
		// cases 2 and 4: comma-separated string of numbers (case 2) or distributions (case 4) or both
		std::vector<std::string> vec = splitVecWithDistributions(effortString);
		if (vec.size() != numCovariatesDetection()) {
			throw coretools::TUserError("The size of covariates provided with argument 'covariatesDetection' (", vec.size(),
			       ") does not match the number of covariates (", numCovariatesDetection(), ").");
		}
		for (size_t c = 0; c < vec.size(); ++c) { _simulateOneCovariateDetection(c, vec[c]); }
	} else {
		// case 3: use the same distribution for all covariates
		for (size_t c = 0; c < numCovariatesDetection(); ++c) { _simulateOneCovariateDetection(c, effortString); }
	}
}

void TMethods::_simulateZeroCovariates() {
	auto pi = coretools::Probability(0.0);
	if (parameters().exists("proportionZeroEffort_" + _methodName)) {
		pi = parameters().get<coretools::Probability>("proportionZeroEffort_" + _methodName);
	} else if (parameters().exists("proportionZeroEffort")) {
		pi = parameters().get<coretools::Probability>("proportionZeroEffort");
	}

	logfile().list("Proportion of zero-valued covariates = ", pi, " (arguments 'proportionZeroEffort' and '",
	               "proportionZeroEffort_" + _methodName, "').");

	if (pi > 0.0) {
		// go over data again and set to zero, if needed
		for (auto &_location : _locations) {
			for (auto &t : _location) {
				if (randomGenerator().pickOneOfTwo(pi) == 1) { // set it to zero!
					for (size_t c = 0; c < t.covariatesEffort().size(); ++c) { t.setCovariateEffort(c, 0.0); }
				}
			}
		}
	}
}

void TMethods::_simulateCovariates() {
	_simulateCovariatesEffort();
	_simulateCovariatesDetection();
	_simulateZeroCovariates();

	// standarize covariates
	standardizeCovariates();

	// now that we have simulated all covariates: re-calculate effort
	initializeEffort();
}

void TMethods::_setValuesForSingleCovariate() {
	if (!inferAlpha()) {
		for (size_t c = 0; c < _alpha->size(); ++c) { _alpha->set(c, 1.0); }
		_alpha->setIsUpdated(false);
	}
	if (!inferBeta() || assumeTrueDetectionProbability()) {
		if (_beta0->size() > 0) { _beta0->set(0.0); } // set beta0 to zero
		_beta0->setIsUpdated(false);

		for (size_t c = 0; c < _beta->size(); ++c) { _beta->set(c, 1.0); }
		_beta->setIsUpdated(false);
	}
}

double TMethods::_calculateU_i(double n_i_bar, const TModelBase &Model) {
	double sum = 0.0;
	size_t JK  = 0;
	for (size_t j = 0; j < size(); j++) {       // loop over all locations
		for (auto &timepoint : _locations[j]) { // loop over all timepoints
			sum += timepoint.calculatePhiEffort(
			    Model.phi(_locations[j].location_id(), timepoint.timepoint_id(), _locations[j].group_id()));
			++JK;
		}
	}
	const double u_i = n_i_bar * (double)JK / sum;
	return u_i;
}

void TMethods::simulatePoisson(bool use_n_bar, double n, const TModelBase &Model) {
	// simulate covariates
	_alpha->normalize(_alpha->getFull());
	_setValuesForSingleCovariate();

	_simulateCovariates();

	// calculate scaling factor
	double N = n;
	if (use_n_bar) { N = _calculateU_i(n, Model); }

	for (size_t j = 0; j < size(); j++) { // loop over all locations
		_locations[j].simulatePoisson(N, Model);
	}
}

void TMethods::simulateMultinomial(const TModelBase &Model) {
	for (size_t j = 0; j < size(); j++) { // loop over all locations
		_locations[j].simulateMultinomial(Model);
	}
}

double TMethods::simulateNB(bool use_n_bar, double n, const TModelBase &Model, double a) {
	// simulate covariates, if necessary
	_alpha->normalize(_alpha->getFull());
	_setValuesForSingleCovariate();

	_simulateCovariates();

	// calculate scaling factor (only if n_bar was provided)
	double N = n;
	if (use_n_bar) { N = _calculateU_i(n, Model); }

	for (size_t j = 0; j < size(); j++) { // loop over all locations
		_locations[j].simulateNB(Model, a, N);
	}

	return N;
}

void TMethods::simulateDirichletMultinomial(const TModelBase &Model, const std::vector<double> &Mu, double B) {
	for (size_t j = 0; j < size(); j++) { // loop over all locations
		_locations[j].simulateDirichletMultinomial(Model, Mu[j], B);
	}
}

void TMethods::write(coretools::TOutputMaybeRcppFile &File, const TUniqueContainer<std::string> &Locations,
                     const TUniqueContainer<TypeTime> &Timepoints,
                     const TUniqueContainer<std::string> &CIGroupNames) const {
	for (const auto &location : _locations) { location.write(File, Locations, Timepoints, CIGroupNames); }
}

void TMethods::estimateInitialAlphaBeta() {
	// TODO: How to initialize alpha and beta?
	// set initial value of all alpha's and beta's to one
	for (size_t c = 0; c < _alpha->size(); ++c) { _alpha->set(c, 1.0); }
	for (size_t c = 0; c < _beta->size(); ++c) { _beta->set(c, 1.0); }
	if (_beta0->size() > 0) { _beta0->set(0.0); }

	_alpha->normalize(_alpha->getFull());

	// set effort based on initial values of alpha and beta
	initializeEffort();
	_setValuesForSingleCovariate();
}

void TMethods::_updateAlpha(size_t c1, size_t c2, const TData &Data,
                            const std::vector<std::unique_ptr<TModelBase>> &Models) {
	// propose new value
	const coretools::TRange range(c1, c2);
	_alpha->propose(range);

	// update effort for all locations and timepoints of current method
	for (auto &location : _locations) {
		location.updateAlphaEffort(_alpha->value(c1), _alpha->oldValue(c1), _alpha->value(c2), _alpha->oldValue(c2), c1,
		                           c2);
	}
	// calculate Hastings ratio
	double logRatio = _alpha->getLogDensityRatio(c1) + _alpha->getLogDensityRatio(c2);
	for (const auto &Model : Models) { // loop over species
		logRatio += Model->calculateLLRatio_perMethod(_i, Data);
	}
	// decide if accept or reject
	if (_alpha->acceptOrReject(logRatio, range, coretools::TRange(range.begin))) {
		// accepted: set likelihood in all epoch models
		for (const auto &Model : Models) { Model->swapTryCur_perMethod(_i, Data); }
	} else {
		// rejected: re-set effort to old value before update
		for (auto &location : _locations) { location.resetAlphaEffort(); }
	}
}

void TMethods::_updateAlpha(const TData &Data, const std::vector<std::unique_ptr<TModelBase>> &Models) {
	if (!_alpha->isUpdated()) { return; }

	stattools::TPairIndexSampler sampler(numCovariatesEffort()); // draw random indices from range [0, numCovariates]
	sampler.sampleIndices();

	for (size_t c = 0; c < sampler.length(); ++c) {
		// pick two indices i to update
		auto [c1, c2] = sampler.getIndexPair(c);
		_updateAlpha(c1, c2, Data, Models);
	}
}

void TMethods::_updateBeta(size_t d, const TData &Data, const std::vector<std::unique_ptr<TModelBase>> &Models,
                           const std::shared_ptr<TypeParamBeta> &Beta) {
	// Beta is either beta0 or beta (both need to calculate the same terms)
	// propose new value
	const coretools::TRange range(d);
	Beta->propose(range);

	// update effort for all locations and timepoints of current method
	for (auto &location : _locations) { location.updateBetaEffort(_beta0, _beta); }

	// calculate Hastings ratio
	double logRatio = Beta->getLogDensityRatio(d);
	for (const auto &Model : Models) { // loop over species
		logRatio += Model->calculateLLRatio_perMethod(_i, Data);
	}
	// decide if accept or reject
	if (Beta->acceptOrReject(logRatio, range)) {
		// accepted: set likelihood in all epoch models
		for (const auto &Model : Models) { Model->swapTryCur_perMethod(_i, Data); }
	} else {
		// rejected: re-set effort to old value before update
		for (auto &location : _locations) { location.resetBetaEffort(); }
	}
}

void TMethods::_updateBeta(const TData &Data, const std::vector<std::unique_ptr<TModelBase>> &Models) {
	// update beta0
	if (_beta0->size() > 0 && _beta0->isUpdated()) { _updateBeta(0, Data, Models, _beta0); }

	// update beta
	if (_beta->isUpdated()) {
		for (size_t d = 0; d < numCovariatesDetection(); ++d) { _updateBeta(d, Data, Models, _beta); }
	}
}

void TMethods::updateAlphaBeta(const TData &Data, const std::vector<std::unique_ptr<TModelBase>> &Models) {
	_updateAlpha(Data, Models);
	_updateBeta(Data, Models);
}

//-------------------------------------------
// TData
//-------------------------------------------

TData::TData() = default;

void TData::addMethod(const TMethods &Method) { _methods.emplace_back(Method); }

void TData::_fillNumMethLocs() {
	// check how many combinations of methods/locations we have - used to initialize vector of LL
	_numMethLoc.resize(_numSpecies, 0);
	for (size_t species = 0; species < _numSpecies; ++species) {
		for (const auto &m : _methods) {
			if (m.hasDataForSpeciesID(species)) { _numMethLoc[species] += m.size(); }
		}
	}
}

std::pair<size_t, size_t> TData::_getMethAndLocIndex(size_t ij_index, size_t speciesID) const {
	// returns the actual indices of specific method and location, not id in unique container.
	size_t counter = 0;
	for (size_t i = 0; i < _methods.size(); i++) {
		if (_methods[i].hasDataForSpeciesID(speciesID)) {
			if (counter + _methods[i].size() > ij_index) {
				size_t j = ij_index - counter;
				return {i, j};
			}
		}
		counter += _methods[i].size();
	}
	throw coretools::TDevError("Did not find method and location index for species - this should not happen.");
}

void TData::_fillLinearToIJ() {
	// fill linear to i_j
	_linear_to_i_j.resize(_numSpecies);
	for (size_t species = 0; species < _numSpecies; ++species) {
		_linear_to_i_j[species].resize(_numMethLoc[species]);
		for (size_t ij = 0; ij < _numMethLoc[species]; ++ij) {
			_linear_to_i_j[species][ij] = _getMethAndLocIndex(ij, species);
		}
	}
}

void TData::_fillIJToLinear() {
	// fill i_j to linear
	_i_j_to_linear.resize(_numSpecies);
	for (size_t species = 0; species < _numSpecies; ++species) {
		_i_j_to_linear[species].resize(_methods.size()); // size = numMethods
		for (size_t i = 0; i < _methods.size(); ++i) {
			_i_j_to_linear[species][i].resize(_methods[i].size()); // size = numLocations of this method
			for (size_t j = 0; j < _methods[i].size(); ++j) {
				auto it =
				    std::find(_linear_to_i_j[species].begin(), _linear_to_i_j[species].end(), std::make_pair(i, j));
				if (it == _linear_to_i_j[species].end()) {
					throw coretools::TDevError("Something went wrong - could not find ", i, ", ", j, " in _linear_to_i_j!");
				}
				_i_j_to_linear[species][i][j] = std::distance(_linear_to_i_j[species].begin(), it);
			}
		}
	}
}

void TData::_fillIJPerLocationID(const TUniqueContainer<std::string> &Locations) {
	// fill _ij_per_locationId: numSpecies x locations (in unique-container) x linear methLoc-indices for that location
	_ij_per_locationId.resize(_numSpecies);
	for (size_t species = 0; species < _numSpecies; ++species) {
		_ij_per_locationId[species].resize(Locations.size());
		for (size_t location_id = 0; location_id < Locations.size(); ++location_id) {
			// loop over all possible method-location combinations of current species
			for (size_t ij = 0; ij < numMethLoc(species); ++ij) {
				const auto [i, j] = linear_to_i_j(species, ij);
				// j=0 in method 1 is not necessarily the first location in the unique container -> check for
				// location_id
				if (_methods[i][j].location_id() == location_id) {
					_ij_per_locationId[species][location_id].push_back(ij);
				}
			}
		}
	}
}

void TData::_fillLocationIDPerCI(const TUniqueContainer<std::string> &Locations,
                                 const TUniqueContainer<std::string> &CIGroupNames) {
	// fill _locationsIds_per_CI_index: numSpecies x number of CI groups x number of locations in that CI group
	// also fill _CI_index_per_locationId: numSpecies x number of locations (in unique container)
	// also check: if multiple methods have same location, but use different group -> throw!
	_locationsIds_per_CI_index.resize(_numSpecies);
	_CI_index_per_locationId.resize(_numSpecies);
	for (size_t species = 0; species < _numSpecies; ++species) {
		_locationsIds_per_CI_index[species].resize(CIGroupNames.size());
		_CI_index_per_locationId[species].resize(Locations.size());
		// loop over locations in unique container
		for (size_t location_id = 0; location_id < Locations.size(); ++location_id) {
			bool first = true;
			// loop over all possible method-location combinations of current species
			for (size_t ij = 0; ij < numMethLoc(species); ++ij) {
				const auto [i, j] = linear_to_i_j(species, ij);
				// j=0 in method 1 is not necessarily the first location in the unique container -> check for
				// location_id
				if (_methods[i][j].location_id() != location_id) { continue; } // not that location -> go to next
				// else: found method with that location
				if (first) {
					// first match: append location_id to that group_id
					_locationsIds_per_CI_index[species][_methods[i][j].group_id()].push_back(location_id);
					// also set CI group index for that location
					_CI_index_per_locationId[species][location_id] = _methods[i][j].group_id();
					first                                          = false;
				} else if (_methods[i][j].group_id() != _CI_index_per_locationId[species][location_id]) {
					// another method has the same location -> check if group matches
					throw coretools::TUserError(
					    "Location '", Locations[location_id],
					    "' has been surveyed using multiple methods but with different control-intervention groups (",
					    CIGroupNames[_methods[i][j].group_id()], " vs ",
					    CIGroupNames[_CI_index_per_locationId[species][location_id]], ").");
				}
			}
		}
	}
}

void TData::fillMethLocIndices(size_t NumSpecies, const TUniqueContainer<std::string> &Locations,
                               const TUniqueContainer<std::string> &CIGroupNames) {
	_numSpecies = NumSpecies;

	for (auto &m : _methods) { m.initializeLookups(Locations, CIGroupNames); }

	_fillNumMethLocs();
	_fillLinearToIJ();
	_fillIJToLinear();
	_fillIJPerLocationID(Locations);
	_fillLocationIDPerCI(Locations, CIGroupNames);
}

size_t TData::size() const { return _methods.size(); }

const TMethods &TData::operator[](size_t index) const { return _methods[index]; }

TMethods &TData::operator[](size_t index) { return _methods[index]; }

std::vector<TMethods>::const_iterator TData::cbegin() const { return _methods.cbegin(); }

std::vector<TMethods>::iterator TData::begin() { return _methods.begin(); }

std::vector<TMethods>::const_iterator TData::cend() const { return _methods.cend(); }

std::vector<TMethods>::iterator TData::end() { return _methods.end(); }
TMethods &TData::back() { return _methods.back(); }

size_t TData::numMethLoc(size_t SpeciesIDInUniqueContainer) const { return _numMethLoc[SpeciesIDInUniqueContainer]; }
size_t TData::i_j_to_linear(size_t SpeciesIDInUniqueContainer, size_t i, size_t j) const {
	return _i_j_to_linear[SpeciesIDInUniqueContainer][i][j];
}
const std::pair<size_t, size_t> &TData::linear_to_i_j(size_t SpeciesIDInUniqueContainer, size_t linear) const {
	return _linear_to_i_j[SpeciesIDInUniqueContainer][linear];
}

const std::vector<size_t> &TData::locationId_to_ij(size_t SpeciesIDInUniqueContainer, size_t location_id) const {
	return _ij_per_locationId[SpeciesIDInUniqueContainer][location_id];
}

size_t TData::locationsId_to_CI_index(size_t SpeciesIDInUniqueContainer, size_t location_id) const {
	return _CI_index_per_locationId[SpeciesIDInUniqueContainer][location_id];
}

const std::vector<size_t> &TData::get_locationsIds_for_CI_index(size_t SpeciesIDInUniqueContainer,
                                                                size_t CI_index) const {
	return _locationsIds_per_CI_index[SpeciesIDInUniqueContainer][CI_index];
}

void TData::clear() {
	_methods.clear();

	_numSpecies = 0;

	_numMethLoc.clear();

	// indices (per species)
	_linear_to_i_j.clear();
	_i_j_to_linear.clear();
	_ij_per_locationId.clear();
}
