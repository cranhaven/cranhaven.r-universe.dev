#include "TGamma.h"

#include "TData.h"
#include "TModels.h"
#include "coretools/Distributions/TNormalDistr.h"
#include "coretools/Main/TParameters.h"

using namespace coretools::instances;

//------------------------------------------
// TOLSGamma
//------------------------------------------

TOLSGamma::TOLSGamma(size_t NumGamma, size_t NumEpochs, const std::vector<TypeTime> &EpochStarts,
                     const std::vector<std::vector<size_t>> &Gamma_ix_per_CIgroup_and_epoch,
                     const std::vector<std::vector<size_t>> &CI_indices_per_gamma)
    : _numGamma(NumGamma), _gamma_ix_per_CIgroup_and_epoch(Gamma_ix_per_CIgroup_and_epoch),
      _CI_indices_per_gamma(CI_indices_per_gamma), _numEpochs(NumEpochs), _epochStarts(EpochStarts) {
	_gamma.resize(NumGamma, 0.0);
}

bool TOLSGamma::_timePointIsRelevant(const TUniqueContainer<TypeTime> &Timepoints, size_t t, size_t e) const {
	// returns true if a timepoint is withing an epoch (borders included)
	return Timepoints[t] >= _epochStarts[e] && Timepoints[t] <= _epochStarts[e + 1];
}

auto TOLSGamma::_loopOverTimepoints(const TData &Data, const TUniqueContainer<TypeTime> &Timepoints, size_t SpeciesID,
                                    size_t i, size_t j_in_method, size_t e, size_t L, std::vector<double> &y_ix,
                                    std::vector<TypeTime> &t_ix, std::vector<size_t> &dataSeriesIndex) const {
	// loop over all timepoints in current datapoint (method/location)
	bool hasTimepoint = false;
	size_t numZero    = 0;
	bool allZero      = true;
	for (size_t k = 0; k < Data[i][j_in_method].size(); ++k) { // loop over timepoints
		const auto &timepoint = Data[i][j_in_method][k];
		// skip timepoints that are not part of epoch
		if (!_timePointIsRelevant(Timepoints, timepoint.timepoint_id(), e)) { continue; }

		// ignore datapoints with effort zero
		if (timepoint.effort() == 0.0) { continue; }

		hasTimepoint = true;

		// prevent issues with zero counts: log(0) = inf
		// exclude them from analysis, but remember how many were excluded
		size_t counts = timepoint.counts_per_species(SpeciesID);
		if (counts == 0) {
			++numZero;
			continue;
		} else {
			allZero = false;
		}

		// calculate y
		const double y = log((double)counts / (double)timepoint.effort());

		// add to containers
		y_ix.push_back(y);
		t_ix.emplace_back(Timepoints[timepoint.timepoint_id()]);
		dataSeriesIndex.emplace_back(L);
	}
	return std::make_tuple(hasTimepoint, numZero, allZero);
}

auto TOLSGamma::_get_Y_ix(const TData &Data, const TUniqueContainer<TypeTime> &Timepoints, size_t ix,
                          size_t SpeciesID) const {
	// calculates response variable y_ix for each gamma as well as other variables needed for OLS
	std::vector<double> y_ix;            // response variable
	std::vector<TypeTime> t_ix;          // timepoints, same size as y_ix
	std::vector<size_t> dataSeriesIndex; // index: which group-epoch-method-location-combi was used, same size as y_ix
	std::vector<std::pair<size_t, size_t>> l_to_ij;       // map that indicates i-j for each linear l
	std::vector<std::pair<size_t, size_t>> l_to_CI_epoch; // map that indicates CIgroup-epoch for each linear l
	std::vector<size_t> numDataPointsWithZeroCounts;      // count number of counts=0 for each linear l
	std::vector<bool> allDataPointsAreZero;               // count number of counts=0 for each linear l

	size_t L = 0;
	for (const size_t CI_index : _CI_indices_per_gamma[ix]) { // loop over all CI groups that are relevant for gamma
		for (size_t i = 0; i < Data.size(); ++i) {            // loop over methods
			if (!Data[i].hasDataForCIGroup(CI_index)) { continue; }
			// loop over locations relevant for current gamma
			for (const auto j_in_method : Data[i].getLocationIndicesForCIGroup(CI_index)) {
				for (size_t e = 0; e < _numEpochs; ++e) { // loop over all epochs
					// skip epoch if gamma_ix is not relevant
					if (_gamma_ix_per_CIgroup_and_epoch[CI_index][e] != ix) { continue; }

					const auto [hasTimepoint, numZero, allZero] = _loopOverTimepoints(
					    Data, Timepoints, SpeciesID, i, j_in_method, e, L, y_ix, t_ix, dataSeriesIndex);

					if (!hasTimepoint) { continue; } // no timepoint (with non-zero effort) for this group-epoch-i-j

					// check if next epoch will have the same gamma as current epoch
					if (e == _numEpochs - 1 || _gamma_ix_per_CIgroup_and_epoch[CI_index][e + 1] != ix) {
						// if last epoch or next epoch is different -> create separate group -> increase L here
						++L;
						l_to_ij.emplace_back(i, j_in_method);
						l_to_CI_epoch.emplace_back(CI_index, e);
						numDataPointsWithZeroCounts.emplace_back(numZero);
						allDataPointsAreZero.emplace_back(allZero);
					}
				}
			}
		}
	}
	return std::make_tuple(y_ix, t_ix, dataSeriesIndex, l_to_ij, l_to_CI_epoch, numDataPointsWithZeroCounts,
	                       allDataPointsAreZero);
}

arma::vec TOLSGamma::_doOLS(const std::vector<double> &Y_ix, const std::vector<TypeTime> &T_ix,
                            const std::vector<size_t> &DataSeriesIndex, size_t L) {
	// fill matrix X
	arma::mat X(Y_ix.size(), L + 1, arma::fill::zeros);
	for (size_t row = 0; row < X.n_rows; ++row) {
		const size_t l       = DataSeriesIndex[row];
		X(row, l)            = 1;
		X(row, X.n_cols - 1) = T_ix[row]; // last column contains timepoints
	}

	// do OLS
	const arma::vec arma_y(Y_ix);
	arma::mat beta;
	bool valid = arma::solve(beta, X, arma_y);
	if (!valid) { DEVERROR("Failed to solve system of linear equations when initializing gamma."); }

	return beta.as_col(); // matrix -> vector
}

void TOLSGamma::_augmentDataSeriesIndexWithOLSEstimates(
    std::vector<std::vector<std::pair<size_t, size_t>>> &l_to_ij,
    std::vector<std::vector<std::pair<size_t, size_t>>> &l_to_CI_epoch, size_t l,
    std::vector<std::vector<size_t>> &dataSeriesIndex, size_t curGamma, size_t otherGamma) {

	// get method (i) and location (j) index of intercept (l) of current gamma
	const size_t i = l_to_ij[curGamma][l].first;
	const size_t j = l_to_ij[curGamma][l].second;

	// get CI_index and epoch index of intercept (l) of current gamma
	const size_t CI_index = l_to_CI_epoch[curGamma][l].first;
	const size_t epoch    = l_to_CI_epoch[curGamma][l].second;

	// check if other gamma already contains this specific combination of i and j for that epoch and CI_index
	const auto &l_to_ij_other_gamma = l_to_ij[otherGamma];
	bool found                      = false;
	for (size_t l_other = 0; l_other < l_to_ij_other_gamma.size(); ++l_other) {
		if (l_to_ij_other_gamma[l_other].first == i && l_to_ij_other_gamma[l_other].second == j) {
			// method and location exists in other gamma as well
			// -> use that index l to augment dataSeriesIndex
			// -> no need to augment l_to_ij or l_to_ij_other_gamma since this ij already exists
			dataSeriesIndex[otherGamma].push_back(l_other);
			found = true;
			break;
		}
	}
	// other gamma does not yet contain this specific combination of i and j
	// -> add a new entry {i, j} to l_to_ij and {CI_index, epoch} to l_to_ij_other_gamma
	// -> use this new l to augment dataSeriesIndex
	if (!found) {
		l_to_ij[otherGamma].emplace_back(i, j);
		l_to_CI_epoch[otherGamma].emplace_back(CI_index, epoch);
		const size_t l_other = l_to_ij[otherGamma].size() - 1;
		dataSeriesIndex[otherGamma].push_back(l_other);
	}
}

bool TOLSGamma::_dontAugmentDataSeries(const std::vector<std::vector<std::pair<size_t, size_t>>> &l_to_ij,
                                       const std::vector<std::vector<std::pair<size_t, size_t>>> &l_to_CI_epoch,
                                       const std::vector<std::vector<size_t>> &dataSeriesIndex,
                                       const std::vector<std::vector<TypeTime>> &t, size_t CI_index, size_t epoch,
                                       size_t l, size_t curGamma, size_t otherGamma, TypeTime EpochBorder) {
	// skip if intercept l affects a different CI group or epoch
	if (l_to_CI_epoch[curGamma][l].first != CI_index || l_to_CI_epoch[curGamma][l].second != epoch) { return true; }

	// check if other gamma already has data at method i and location j and epoch border (start or end)
	// get method (i) and location (j) index of data series (l) of current epoch
	const size_t i = l_to_ij[curGamma][l].first;
	const size_t j = l_to_ij[curGamma][l].second;
	// loop over all timepoints of other epoch
	for (size_t r = 0; r < t[otherGamma].size(); ++r) {
		// check if other gamma contains this method (i) and location (j) and the timepoint corresponding to
		// EpochBorder
		const size_t other_l   = dataSeriesIndex[otherGamma][r];
		const size_t other_i   = l_to_ij[otherGamma][other_l].first;
		const size_t other_j   = l_to_ij[otherGamma][other_l].second;
		const TypeTime other_t = t[otherGamma][r];
		if (other_i == i && other_j == j && other_t == EpochBorder) { return true; }
	}
	return false;
}

void TOLSGamma::_augmentYWithOLSEstimatesStart(size_t curGamma, size_t CI_index, size_t epoch, const arma::vec &Beta,
                                               std::vector<std::vector<double>> &y,
                                               std::vector<std::vector<TypeTime>> &t,
                                               std::vector<std::vector<size_t>> &dataSeriesIndex,
                                               std::vector<std::vector<std::pair<size_t, size_t>>> &l_to_ij,
                                               std::vector<std::vector<std::pair<size_t, size_t>>> &l_to_CI_epoch,
                                               bool augment_zero) const {

	const TypeTime epochStart = _epochStarts[epoch];

	for (size_t otherGamma = 0; otherGamma < _numGamma; ++otherGamma) { // loop over all other gamma
		if (otherGamma == curGamma) { continue; }               // no need to augment curGamma: this was just estimated
		for (size_t l = 0; l < l_to_ij[curGamma].size(); ++l) { // loop over all intercepts that were estimated
			if (_dontAugmentDataSeries(l_to_ij, l_to_CI_epoch, dataSeriesIndex, t, CI_index, epoch, l, curGamma,
			                           otherGamma, epochStart)) {
				continue;
			}

			double y_start = 0.0;
			if (!augment_zero) {
				const double alpha_l = Beta(l);
				y_start              = alpha_l + (double)epochStart * _gamma[curGamma];
			}

			// augment
			y[otherGamma].push_back(y_start);
			t[otherGamma].emplace_back(epochStart);
			_augmentDataSeriesIndexWithOLSEstimates(l_to_ij, l_to_CI_epoch, l, dataSeriesIndex, curGamma, otherGamma);
		}
	}
}

void TOLSGamma::_augmentYWithOLSEstimatesStart_All(
    size_t curGamma, const arma::vec &Beta, std::vector<std::vector<double>> &y, std::vector<std::vector<TypeTime>> &t,
    std::vector<std::vector<size_t>> &dataSeriesIndex, std::vector<std::vector<std::pair<size_t, size_t>>> &l_to_ij,
    std::vector<std::vector<std::pair<size_t, size_t>>> &l_to_CI_epoch, bool augment_zero) const {
	// for each CI group that uses this gamma: add start
	for (const auto CI_index : _CI_indices_per_gamma[curGamma]) {
		for (size_t e = 1; e < _numEpochs; ++e) { // loop over all epochs (but skip first one: there is no previous)
			if (_gamma_ix_per_CIgroup_and_epoch[CI_index][e] != curGamma ||
			    _gamma_ix_per_CIgroup_and_epoch[CI_index][e - 1] == curGamma) {
				// skip epoch if gamma_ix is not relevant or if previous epoch uses the same gamma
				continue;
			}
			// previous epoch uses a different gamma -> augment it!
			_augmentYWithOLSEstimatesStart(curGamma, CI_index, e, Beta, y, t, dataSeriesIndex, l_to_ij, l_to_CI_epoch,
			                               augment_zero);
		}
	}
}

void TOLSGamma::_augmentYWithOLSEstimatesEnd(size_t curGamma, size_t CI_index, size_t epoch, const arma::vec &Beta,
                                             std::vector<std::vector<double>> &y, std::vector<std::vector<TypeTime>> &t,
                                             std::vector<std::vector<size_t>> &dataSeriesIndex,
                                             std::vector<std::vector<std::pair<size_t, size_t>>> &l_to_ij,
                                             std::vector<std::vector<std::pair<size_t, size_t>>> &l_to_CI_epoch,
                                             bool augment_zero) const {
	const TypeTime epochEnd = _epochStarts[epoch + 1];

	for (size_t otherGamma = 0; otherGamma < _numGamma; ++otherGamma) { // loop over all other gamma
		if (otherGamma == curGamma) { continue; }               // no need to augment curGamma: this was just estimated
		for (size_t l = 0; l < l_to_ij[curGamma].size(); ++l) { // loop over all intercepts that were estimated
			if (_dontAugmentDataSeries(l_to_ij, l_to_CI_epoch, dataSeriesIndex, t, CI_index, epoch, l, curGamma,
			                           otherGamma, epochEnd)) {
				continue;
			}

			double y_end = 0.0;
			if (!augment_zero) {
				const double alpha_l = Beta(l);
				y_end                = alpha_l + (double)epochEnd * _gamma[curGamma];
			}
			// augment
			y[otherGamma].push_back(y_end);
			t[otherGamma].emplace_back(epochEnd);
			_augmentDataSeriesIndexWithOLSEstimates(l_to_ij, l_to_CI_epoch, l, dataSeriesIndex, curGamma, otherGamma);
		}
	}
}

void TOLSGamma::_augmentYWithOLSEstimatesEnd_All(
    size_t curGamma, const arma::vec &Beta, std::vector<std::vector<double>> &y, std::vector<std::vector<TypeTime>> &t,
    std::vector<std::vector<size_t>> &dataSeriesIndex, std::vector<std::vector<std::pair<size_t, size_t>>> &l_to_ij,
    std::vector<std::vector<std::pair<size_t, size_t>>> &l_to_CI_epoch, bool augment_zero) const {
	// for each CI group that uses this gamma: add end
	for (const auto CI_index : _CI_indices_per_gamma[curGamma]) {
		for (size_t e = 0; e < _numEpochs - 1; ++e) { // loop over all epochs (but skip last one: there is no next)
			_augmentYWithOLSEstimatesEnd(curGamma, CI_index, e, Beta, y, t, dataSeriesIndex, l_to_ij, l_to_CI_epoch,
			                             augment_zero);
		}
	}
}

size_t TOLSGamma::_getNumNonZeroDataPointsInRange(size_t l, const std::vector<size_t> &dataSeriesIndex) {
	size_t counter = 0;
	for (auto r : dataSeriesIndex) {
		if (r == l) { ++counter; }
	}
	return counter;
}

auto TOLSGamma::_getScore(size_t l, const std::vector<size_t> &dataSeriesIndex,
                          const std::vector<size_t> &numDataPointsWithZeroCounts) {
	if (numDataPointsWithZeroCounts.empty()) { std::make_tuple(0, 0, 0); } // no data points at all

	// count the number of occurrences of that l for the current gamma (non-zero data points)
	size_t numNonZeroDataPointsInRange = _getNumNonZeroDataPointsInRange(l, dataSeriesIndex);

	// count the total number of data points in range (including zero-valued data points)
	size_t numDataPointsInRange = numNonZeroDataPointsInRange + numDataPointsWithZeroCounts[l];

	// calculate score: zero-valued data points are valued 0.5
	double score = (double)numNonZeroDataPointsInRange + 0.5 * (double)numDataPointsWithZeroCounts[l];

	return std::make_tuple(numNonZeroDataPointsInRange, numDataPointsInRange, score);
}

auto TOLSGamma::_getNextGammaToInitialize(const std::vector<bool> &GammaInitialized,
                                          const std::vector<std::vector<std::pair<size_t, size_t>>> &l_to_ij,
                                          const std::vector<std::vector<size_t>> &dataSeriesIndex,
                                          const std::vector<std::vector<size_t>> &numDataPointsWithZeroCounts) const {
	// find gamma that contains the most data points per degree of freedom (= number of parameters = L + 1)
	size_t max_gamma_ix   = 0;
	double max_score      = std::numeric_limits<double>::lowest();
	bool max_ols_possible = false;
	for (size_t ix = 0; ix < _numGamma; ++ix) {
		double score      = 0;
		bool ols_possible = false;
		for (size_t l = 0; l < l_to_ij[ix].size(); ++l) { // loop over all possible ranges
			// calculate scores
			const auto [numNonZeroDataPointsInRange, numDataPointsInRange, score_l] =
			    _getScore(l, dataSeriesIndex[ix], numDataPointsWithZeroCounts[ix]);
			if (numDataPointsInRange >= 2) { score += score_l; }
			if (numNonZeroDataPointsInRange >= 2) { ols_possible = true; }
		}

		if (!GammaInitialized[ix] && score > max_score) {
			max_gamma_ix     = ix;
			max_score        = score;
			max_ols_possible = ols_possible;
		}
	}

	if (max_score == 0) {
		UERROR("Need at least one range with two or more timepoints for estimating gamma [", max_gamma_ix + 1, "]!");
	}

	return std::make_tuple(max_gamma_ix, max_ols_possible);
}

std::vector<double> TOLSGamma::estimateInitialGamma(const TData &Data, const TUniqueContainer<TypeTime> &Timepoints,
                                                    size_t SpeciesID, const TUniqueContainer<std::string> &GammaNames) {
	logfile().startIndent("Estimating initial gamma with OLS:");

	// fill response variable y and other indicator vectors
	std::vector<std::vector<double>> y(_numGamma);
	std::vector<std::vector<TypeTime>> t(_numGamma);
	std::vector<std::vector<size_t>> dataSeriesIndex(_numGamma);
	std::vector<std::vector<std::pair<size_t, size_t>>> l_to_ij(_numGamma);
	std::vector<std::vector<std::pair<size_t, size_t>>> l_to_CI_epoch(_numGamma);
	std::vector<std::vector<size_t>> numDataPointsWithZeroCounts(_numGamma);
	std::vector<std::vector<bool>> allDataPointsAreZero(_numGamma);
	for (size_t ix = 0; ix < _numGamma; ++ix) {
		std::tie(y[ix], t[ix], dataSeriesIndex[ix], l_to_ij[ix], l_to_CI_epoch[ix], numDataPointsWithZeroCounts[ix],
		         allDataPointsAreZero[ix]) = _get_Y_ix(Data, Timepoints, ix, SpeciesID);
	}

	std::vector<bool> gammaInitialized(_numGamma, false);
	while (std::accumulate(gammaInitialized.begin(), gammaInitialized.end(), 0U) < _numGamma) {
		// 1) find gamma that has the highest score
		const auto [curGamma, ols_possible] =
		    _getNextGammaToInitialize(gammaInitialized, l_to_ij, dataSeriesIndex, numDataPointsWithZeroCounts);

		logfile().listFlush("Gamma ", GammaNames[curGamma], ":");

		// 2) we need at least one group-epoch-i-j combination with at least 2 timepoints
		const size_t L = l_to_ij[curGamma].size();
		if (ols_possible) {
			// 3) estimate intercept and gamma with OLS
			auto beta        = _doOLS(y[curGamma], t[curGamma], dataSeriesIndex[curGamma], L);
			_gamma[curGamma] = beta(L);
			// make sure we don't get crazy gammas
			_gamma[curGamma] = std::min(_gamma[curGamma], 1.0);
			_gamma[curGamma] = std::max(_gamma[curGamma], -1.0);

			// 4) calculate y_start and y_end and add it to existing data
			_augmentYWithOLSEstimatesStart_All(curGamma, beta, y, t, dataSeriesIndex, l_to_ij, l_to_CI_epoch, false);
			_augmentYWithOLSEstimatesEnd_All(curGamma, beta, y, t, dataSeriesIndex, l_to_ij, l_to_CI_epoch, false);
		} else {
			// gamma would be identifiable but isn't because we excluded counts = 0 for OLS
			// set gamma to zero: identifiable for MCMC, but not for OLS
			_gamma[curGamma] = 0.0;

			// augment with zero
			_augmentYWithOLSEstimatesStart_All(curGamma, arma::vec(), y, t, dataSeriesIndex, l_to_ij, l_to_CI_epoch,
			                                   true);
			_augmentYWithOLSEstimatesEnd_All(curGamma, arma::vec(), y, t, dataSeriesIndex, l_to_ij, l_to_CI_epoch,
			                                 true);
		}

		gammaInitialized[curGamma] = true;
		logfile().flush(" initial value = ", _gamma[curGamma], "!");
		logfile().newLine();
	}
	logfile().endIndent();

	return _gamma;
}

//------------------------------------------
// TGamma
//------------------------------------------

TGamma::TGamma(TypeParamGamma *Gamma, const TUniqueContainer<TypeTime> &Timepoints,
               const TUniqueContainer<std::string> &CIGroupNames, size_t SpeciesID, std::string_view Prefix)
    : _gamma(Gamma), _species_id(SpeciesID), _numCIGroups(CIGroupNames.size()) {
	// find out how many epochs there are and how many CI groups
	_readGammaGrouping(Timepoints, CIGroupNames, Prefix);

	// calculate Jeffrey's prior?
	_noJP = parameters().exists("noJP");
	if (_noJP) { logfile().list("Will not use Jeffrey's prior on gamma (argument 'noJP')."); }
}

void TGamma::initialize(const TUniqueContainer<TypeTime> &Timepoints, TBirpPrior *BirpPrior) {
	_gamma->initStorage(BirpPrior, {_gamma_names.size()},
	                    {std::make_shared<coretools::TNamesStrings>(_gamma_names.vec())});

	_summaryGammaPosterior.resize(_gamma_names.size(), std::vector<size_t>(_gamma_names.size(), 0));

	_fillRho(Timepoints);
}

void TGamma::_readGammaGrouping(const TUniqueContainer<TypeTime> &Timepoints,
                                const TUniqueContainer<std::string> &CIGroupNames, std::string_view Prefix) {
	if (parameters().exists("timesOfChange")) {
		_fillTimesOfChange(Timepoints, Prefix);
	} else {
		logfile().list("Will assume a single epoch without any times of change (use argument "
		               "'timesOfChange' to change).");
		_numEpochs = 1;
	}

	// read gamma grouping BACI
	if (_numCIGroups > 1 && _numEpochs == 1) { // CI only
		_fillGammaIndicatorCI(CIGroupNames);
	} else if (_numCIGroups == 1) { // BA only
		_fillGammaIndicatorBA(Timepoints);
	} else { // BACI
		_fillGammaIndicatorBACI(CIGroupNames);
	}

	// write to file
	coretools::TOutputMaybeRcppFile file((std::string)Prefix + "_BACI_configuration.txt");
	for (size_t i = 0; i < _numCIGroups; ++i) {
		for (size_t e = 0; e < _numEpochs; ++e) { file << _gamma_names[_gamma_ix_per_CIgroup_and_epoch[i][e]]; }
		file.endln();
	}
}

void TGamma::_fillGammaIndicatorCI(const TUniqueContainer<std::string> &CIGroupNames) {
	// if only a single epoch and multiple groups -> we do CI -> no need to provide BACI-grouping file
	logfile().list("Will perform control-intervention analysis (CI) without before-after grouping (provide arguments "
	               "'timesOfChange' and 'BACI' to switch to a BACI analysis).");
	_gamma_ix_per_CIgroup_and_epoch.resize(_numCIGroups);
	_CI_indices_per_gamma.resize(_numCIGroups); // there will be as many gammas as there are CI groups
	for (size_t i = 0; i < _numCIGroups; i++) {
		size_t ix = _gamma_names.add(CIGroupNames[i]);    // one gamma per CI group
		_gamma_ix_per_CIgroup_and_epoch[i].push_back(ix); // a single epoch, index of gamma = ix
		_CI_indices_per_gamma[i] = {i};                   // for each gamma: what is the CI group index?
	}
}

void TGamma::_fillGammaIndicatorBA(const TUniqueContainer<TypeTime> &Timepoints) {
	// if only a single group -> we do BA -> no need to provide BACI-grouping file
	logfile().list("Will perform before-after analysis (BA) without control-intervention grouping (provide column "
	               "'CI_group' in counts file and argument 'BACI' to switch to a BACI analysis).");
	_numCIGroups = 1;
	// a single CI group and _numEpoch epochs
	_gamma_ix_per_CIgroup_and_epoch.resize(1, std::vector<size_t>(_numEpochs, 0));
	_CI_indices_per_gamma.resize(_numEpochs); // there will be as many gammas as there are epochs

	// get epoch starts for nice names
	auto epochStarts      = getEpochStarts(Timepoints);
	size_t ix_epoch_start = 1;

	for (size_t i = 0; i < _numEpochs; i++, ix_epoch_start++) {
		std::string name = "Epoch_" + coretools::str::toString(epochStarts[ix_epoch_start - 1]) + "-" +
		                   coretools::str::toString(epochStarts[ix_epoch_start]);
		size_t ix                             = _gamma_names.add(name); // one gamma per epoch
		_gamma_ix_per_CIgroup_and_epoch[0][i] = ix;                     // a single group, gamma index for epoch i is ix
		_CI_indices_per_gamma[i]              = {0};                    // every gamma has CI group index = 0
	}
}

void TGamma::_fillGammaIndicatorBACI(const TUniqueContainer<std::string> &CIGroupNames) {
	// open file
	if (!parameters().exists("BACI")) {
		UERROR("The argument 'BACI' is required when analysing ", _numCIGroups, " control-intervention groups and ",
		       _numEpochs, " epochs.");
	}
	auto name = parameters().get<std::string>("BACI");
	coretools::TInputMaybeRcppFile file(name, coretools::FileType::NoHeader);
	logfile().list("Reading BACI grouping information from file ", name, "...");

	// number of columns = number of epochs in total -> check if this matches timesOfChange!
	int numEpochs = (int)file.numCols() - 1;
	if (numEpochs != (int)_numEpochs) {
		UERROR("The number of epochs provided in BACI-file (", numEpochs,
		       ") does not match the number of epochs provided through the argument 'timesOfChange' (", _numEpochs,
		       ").");
	}

	// resize _gamma_ix_per_CIgroup_and_epoch: numCIGroups x numEpochs
	_gamma_ix_per_CIgroup_and_epoch.resize(_numCIGroups, std::vector<size_t>(_numEpochs, 0));

	// parse file and fill index containers
	std::vector<bool> found_CI_group(_numCIGroups, false);
	for (; !file.empty(); file.popFront()) {
		// new line = new group (control / intervention)
		auto group_name = file.get<std::string>(0); // first column = CI group name
		if (!CIGroupNames.exists(group_name)) {
			UERROR("CI group name '", group_name,
			       "' from BACI file does not match any of the CI groups provided in the counts files (column "
			       "'CI_group')!");
		}
		size_t group_id          = CIGroupNames.getIndex(group_name);
		found_CI_group[group_id] = true;

		// loop over all epochs
		for (size_t m = 0; m < _numEpochs; ++m) {
			// read name of gamma
			auto gamma_name = file.get<std::string>(m + 1); // skip first column
			size_t ix       = _gamma_names.add(gamma_name);

			_gamma_ix_per_CIgroup_and_epoch[group_id][m] = ix; // store gamma index for current group and epoch
			if (ix < _CI_indices_per_gamma.size()) {           // gamma index already exists -> append group index
				// if that group is already present in gamma_ix: don't append
				if (std::find(_CI_indices_per_gamma[ix].begin(), _CI_indices_per_gamma[ix].end(), group_id) ==
				    _CI_indices_per_gamma[ix].end()) {
					_CI_indices_per_gamma[ix].push_back(group_id);
				}
			} else if (ix == _CI_indices_per_gamma.size()) { // new gamma index (+1 of previous) -> append new index
				_CI_indices_per_gamma.push_back({group_id});
			} else {
				DEVERROR("Gamma index ", ix, " is larger than size of _CI_indices_per_gamma ",
				         _CI_indices_per_gamma.size(), ", this should never happen.");
			}
		}
	}

	// check if all CI groups were parsed
	for (size_t i = 0; i < _numCIGroups; ++i) {
		if (!found_CI_group[i]) {
			UERROR("CI group with name ", CIGroupNames[i],
			       " from counts file (column 'CI_group') was not found in BACI file.");
		}
	}

	logfile().conclude("Found ", _numCIGroups, " control-intervention (CI) groups with a total number of ", _numEpochs,
	                   " epochs.");
}

void TGamma::_fillTimesOfChange(const TUniqueContainer<TypeTime> &Timepoints, std::string_view Prefix) {
	// check if times of change, as provided by the user, are valid and adjust them if necessary
	assert(!Timepoints.empty());

	// read command-line argument
	std::vector<TypeTime> vec;
	parameters().fill("timesOfChange", vec);

	// get first and last timepoint
	TypeTime minTime = *std::min_element(Timepoints.cbegin(), Timepoints.cend());
	TypeTime maxTime = *std::max_element(Timepoints.cbegin(), Timepoints.cend());

	for (auto toc : vec) {
		if (toc <= minTime) {
			logfile().warning("Time of change ", toc, " pre-dates or equals the first time point ", minTime,
			                  ". Will ignore this time of change.");
		} else if (toc >= maxTime) {
			logfile().warning("Time of change ", toc, " post-dates or equals the last time point ", maxTime,
			                  ". Will ignore this time of change.");
		} else if (!_timesOfChange.empty() && toc <= _timesOfChange.back()) { // check if it is sorted
			UERROR("Time of change ", toc, " pre-dates or equals the previous time of change ", _timesOfChange.back(),
			       ". Please provide times of change in increasing order.");
		} else {
			_timesOfChange.push_back(toc);
		}
	}

	// count number of epochs
	_numEpochs = _timesOfChange.size() + 1;

	// write to file
	coretools::TOutputMaybeRcppFile file((std::string)Prefix + "_timesOfChange.txt");
	file.writeln(_timesOfChange);

	// report
	logfile().list("Times of change = ", _timesOfChange, " (argument 'timesOfChange').");
	logfile().list("Number of epochs = ", _numEpochs, ".");
}

void TGamma::estimateInitialGamma(const TData &Data, const TUniqueContainer<TypeTime> &Timepoints) {
	if (_gamma->hasFixedInitialValue()) { return; }

	std::vector<TypeTime> epochStarts = getEpochStarts(Timepoints);
	TOLSGamma ols(_gamma->size(), _numEpochs, epochStarts, _gamma_ix_per_CIgroup_and_epoch, _CI_indices_per_gamma);
	auto gamma = ols.estimateInitialGamma(Data, Timepoints, _species_id, _gamma_names);
	assert(gamma.size() == _gamma->size());
	for (size_t ix = 0; ix < gamma.size(); ++ix) { _gamma->set(ix, gamma[ix]); }
}

void TGamma::initializeJP(const TData &Data, const TModelBase &Model) {
	_curJP.resize(_numCIGroups);
	for (size_t i = 0; i < _numCIGroups; ++i) { _curJP[i] = Model.calculateJeffreyPrior(Data, *this, i); }
	_tryJP = _curJP;
}

std::vector<TypeTime> TGamma::getEpochStarts(const TUniqueContainer<TypeTime> &Timepoints) const {
	// return start of each epoch, including for a hypothetical epoch after the last epoch
	assert(!Timepoints.empty());

	// T_0 = t_1
	auto minTime                      = *std::min_element(Timepoints.cbegin(), Timepoints.cend());
	std::vector<TypeTime> epochStarts = {minTime};

	// all intermediate T
	epochStarts.insert(epochStarts.end(), _timesOfChange.begin(), _timesOfChange.end());

	// T_M = t_K
	epochStarts.push_back(*max_element(Timepoints.cbegin(), Timepoints.cend()));
	return epochStarts;
}

void TGamma::_fillRho(const TUniqueContainer<TypeTime> &Timepoints) {
	std::vector<TypeTime> epochStarts = getEpochStarts(Timepoints);
	_rho.zeros((int)Timepoints.size(), (int)_numEpochs);

	for (size_t t = 0; t < Timepoints.size(); ++t) {
		for (size_t e = 1; e < epochStarts.size(); ++e) {
			if (Timepoints[t] <= epochStarts[e - 1]) {
				_rho(t, e - 1) = 0;
			} else if (epochStarts[e - 1] < Timepoints[t] && Timepoints[t] < epochStarts[e]) {
				_rho(t, e - 1) = Timepoints[t] - epochStarts[e - 1];
			} else {
				_rho(t, e - 1) = epochStarts[e] - epochStarts[e - 1];
			}
		}
	}
}

void TGamma::setSimulatedGammaToZero() {
	for (size_t g = 0; g < _gamma->size(); ++g) { _gamma->set(g, 0.0); }
}

// functions to update gamma

bool TGamma::updateSpecificIndex(size_t ix) {
	if (_gamma->isUpdated()) {
		_gamma->propose(coretools::TRange(ix));
		return true;
	}
	return false;
}

double TGamma::getLogPriorRatio(const TData &Data, const TModelBase &Model, size_t CI_ix) {
	// -> prior on gamma: Jeffrey prior
	if (_noJP) { return 0.0; }
	_tryJP[CI_ix] = Model.calculateJeffreyPrior(Data, *this, CI_ix);
	return _tryJP[CI_ix] - _curJP[CI_ix];
}

bool TGamma::acceptOrReject(double logH, size_t ix) {
	if (_gamma->acceptOrReject(logH, coretools::TRange(ix))) {
		for (const auto CI_ix : _CI_indices_per_gamma[ix]) { _curJP[CI_ix] = _tryJP[CI_ix]; }
		return true;
	}
	return false;
}

void TGamma::resetSummaryGammaPosterior() {
	_counter = 0;
	for (auto &m : _summaryGammaPosterior) { std::fill(m.begin(), m.end(), 0); }
}

void TGamma::updateSummaryGammaPosterior() {
	++_counter;
	for (size_t ix_1 = 0; ix_1 < _gamma->size(); ++ix_1) {     // rows
		for (size_t ix_2 = 0; ix_2 < _gamma->size(); ++ix_2) { // column
			if (ix_1 == ix_2) {
				// on the diagonal: count if gamma_ix_1 > 0
				if (_gamma->value(ix_1) >= 0.0) { ++_summaryGammaPosterior[ix_1][ix_2]; }
			} else {
				// off the diagonal: count if gamma_ix_1 > gamma_ix_2
				if (_gamma->value(ix_1) > _gamma->value(ix_2)) { ++_summaryGammaPosterior[ix_1][ix_2]; }
			}
		}
	}
}

void TGamma::writeSummaryGammaPosterior(const std::string &Prefix) {
	auto header = _gamma_names.vec();
	header.insert(header.begin(), "row_bigger_than_column");
	coretools::TOutputMaybeRcppFile file(Prefix + "_gammaSummaries.txt", header);
	for (size_t ix_1 = 0; ix_1 < _gamma->size(); ++ix_1) {
		file << _gamma_names[ix_1];
		for (size_t ix_2 = 0; ix_2 < _gamma->size(); ++ix_2) {
			file << (double)_summaryGammaPosterior[ix_1][ix_2] / (double)_counter;
		}
		file.endln();
	}
}

// getters
size_t TGamma::numEpochs() const { return _numEpochs; }
size_t TGamma::numCIGroups() const { return _numCIGroups; }
size_t TGamma::numTimepoints() const { return _rho.n_rows; }
size_t TGamma::size() const { return _gamma->size(); }
size_t TGamma::species_id() const { return _species_id; }
const std::vector<size_t> &TGamma::getCIGroups(size_t gamma_ix) const { return _CI_indices_per_gamma[gamma_ix]; }

TypeGamma TGamma::gamma(size_t epoch, size_t CI_ix) const {
	assert(epoch < _numEpochs);
	assert(CI_ix < _numCIGroups);

	auto ix = _gamma_ix_per_CIgroup_and_epoch[CI_ix][epoch];
	return _gamma->value(ix);
}

double TGamma::rho(size_t row, size_t col) const { return _rho(row, col); }

bool TGamma::gammaIsUpdated() const { return _gamma->isUpdated(); }

[[nodiscard]] std::vector<TypePhi> TGamma::phi(size_t CI_ix) const {
	std::vector<TypePhi> phi(numTimepoints());
	for (size_t k = 0; k < numTimepoints(); ++k) {
		double logPhi = 0.0;
		for (size_t m = 0; m < _numEpochs; m++) { logPhi += _rho(k, m) * gamma(m, CI_ix); }
		phi[k] = exp(logPhi); // make logPhi to phi
	}
	return phi;
}

//------------------------------------------
// TStochasticPrior
//------------------------------------------

TStochasticPrior::TStochasticPrior(TypeParamLogPhi *LogPhi, TypeParamLogSigma *LogSigma, TypeParamGamma *Gamma,
                                   const TUniqueContainer<TypeTime> &Timepoints,
                                   const TUniqueContainer<std::string> &CIGroupNames, size_t SpeciesID,
                                   std::string_view Prefix)
    : _logPhi(LogPhi), _logSigma(LogSigma), _gamma(Gamma, Timepoints, CIGroupNames, SpeciesID, Prefix) {}

void TStochasticPrior::initialize(const TUniqueContainer<TypeTime> &Timepoints, TBirpPrior *BirpPrior) {
	// initialize gamma and times of change
	_gamma.initialize(Timepoints, BirpPrior);

	// pre-calculate deltaTime
	_deltaTimeVec.resize(_gamma.numTimepoints(), 0);
	for (size_t k = 1; k < Timepoints.size(); k++) { _deltaTimeVec[k] = Timepoints[k] - Timepoints[k - 1]; }

	// fill rho
	_fillStochasticRho(Timepoints);

	// resize temporary vectors
	_newSumRhoGamma.resize(_gamma.numCIGroups(), std::vector<double>(_gamma.numTimepoints(), 0.0));
	_newDeterministicPhi.resize(_gamma.numCIGroups(), std::vector<TypePhi>(_gamma.numTimepoints()));
}

void TStochasticPrior::estimateInitialGamma(const TData &Data, const TUniqueContainer<TypeTime> &Timepoints) {
	_gamma.estimateInitialGamma(Data, Timepoints);
}

void TStochasticPrior::initializeTempVariables(const TData &Data, const TModelBase &Model, bool CalculateJP) {
	// initialize rho sum gamma
	for (size_t i = 0; i < _gamma.numCIGroups(); ++i) { _initializeSumRhoGamma(i); }
	_oldSumRhoGamma = _newSumRhoGamma;

	// initialize deterministic phi
	for (size_t i = 0; i < _gamma.numCIGroups(); ++i) { _newDeterministicPhi[i] = _gamma.phi(i); }
	_oldDeterministicPhi = _newDeterministicPhi;

	// initialize JP
	if (CalculateJP) { _gamma.initializeJP(Data, Model); }

	_initializeTransitionProbabilities(Data);
}

void TStochasticPrior::_fillStochasticRho(const TUniqueContainer<TypeTime> &Timepoints) {
	std::vector<TypeTime> epochStarts = _gamma.getEpochStarts(Timepoints);
	_stochasticRho.zeros((int)Timepoints.size(), (int)_gamma.numEpochs());

	for (size_t t = 1; t < Timepoints.size(); ++t) {
		for (size_t e = 1; e < epochStarts.size(); ++e) {
			// this is equation 6 in MS (stochastic rho)
			if (Timepoints[t] >= epochStarts[e] && Timepoints[t - 1] <= epochStarts[e - 1]) {
				_stochasticRho(t, e - 1) = epochStarts[e] - epochStarts[e - 1];
			} else if (epochStarts[e - 1] < Timepoints[t] && Timepoints[t] < epochStarts[e] &&
			           Timepoints[t - 1] <= epochStarts[e - 1]) {
				_stochasticRho(t, e - 1) = Timepoints[t] - epochStarts[e - 1];
			} else if (Timepoints[t] >= epochStarts[e] && epochStarts[e - 1] < Timepoints[t - 1] &&
			           Timepoints[t - 1] < epochStarts[e]) {
				_stochasticRho(t, e - 1) = epochStarts[e] - Timepoints[t - 1];
			} else if (epochStarts[e - 1] < Timepoints[t] && Timepoints[t] < epochStarts[e] &&
			           epochStarts[e - 1] < Timepoints[t - 1] && Timepoints[t - 1] < epochStarts[e]) {
				_stochasticRho(t, e - 1) = Timepoints[t] - Timepoints[t - 1];
			} else {
				_stochasticRho(t, e - 1) = 0.0;
			}
		}
	}
}

void TStochasticPrior::_initializeSumRhoGamma(size_t CI_index) {
	std::fill(_newSumRhoGamma[CI_index].begin(), _newSumRhoGamma[CI_index].end(), 0.0);
	for (size_t k = 1; k < _gamma.numTimepoints(); ++k) {
		for (size_t e = 0; e < _gamma.numEpochs(); e++) {
			_newSumRhoGamma[CI_index][k] += _stochasticRho(k, e) * (double)_gamma.gamma(e, CI_index);
		}
	}
}

void TStochasticPrior::_initializeTransitionProbabilities(const TData &Data) {
	const size_t numLocations  = _logPhi->dimensions()[0];
	const size_t numTimePoints = _logPhi->dimensions()[1];

	_tryLogTransitionProbs.resize(numLocations, std::vector<double>(numTimePoints, 0.0));
	_curLogTransitionProbs.resize(numLocations, std::vector<double>(numTimePoints, 0.0));

	for (size_t i = 0; i < _gamma.numCIGroups(); ++i) { // loop over all CI groups
		// loop over locations of that CI group
		for (const auto j : Data.get_locationsIds_for_CI_index(_gamma.species_id(), i)) {
			for (size_t k = 1; k < numTimePoints; ++k) { // loop over timepoints
				_curLogTransitionProbs[j][k] = _calculateLogTransitionProbability(j, k, i);
			}
		}
		_tryLogTransitionProbs = _curLogTransitionProbs;
	}
}

coretools::probdist::TNormalDistr TStochasticPrior::_getMeanSdTransitionProbability(size_t j, size_t k,
                                                                                    size_t CI_index) const {
	const auto deltaTime = (double)_deltaTimeVec[k];

	// calculate mean of normal distribution
	const auto sigma                = exp(_logSigma->value());
	const auto sigma2               = sigma * sigma;
	const auto sumRhoGamma          = _newSumRhoGamma[CI_index][k];
	const auto log_phi_j_previous_k = _logPhi->value(_logPhi->getIndex({j, k - 1}));
	const auto mean                 = log_phi_j_previous_k + sumRhoGamma - sigma2 / 2.0 * deltaTime;

	// calculate sd of normal distribution
	const double sd = sqrt(sigma2 * deltaTime);

	coretools::probdist::TNormalDistr distr(mean, sd);
	return distr;
}

double TStochasticPrior::_calculateLogTransitionProbability(size_t j, size_t k, size_t CI_index) const {
	assert(k > 0);

	coretools::probdist::TNormalDistr distr = _getMeanSdTransitionProbability(j, k, CI_index);
	const double log_phi_j_k                = _logPhi->value(_logPhi->getIndex({j, k}));
	return distr.logDensity(log_phi_j_k);
}

double TStochasticPrior::calculateLLRatioForUpdateGamma(const TData &Data, size_t CI_index) {
	// likelihood: just go through Markov chain and sum up log transition probabilities
	// only need to consider locations relevant for current CI index
	double LL = 0.0;
	for (const auto j : Data.get_locationsIds_for_CI_index(_gamma.species_id(), CI_index)) { // loop over locations
		for (size_t k = 1; k < _logPhi->dimensions()[1]; ++k) {                              // loop over timepoints
			_tryLogTransitionProbs[j][k] = _calculateLogTransitionProbability(j, k, CI_index);
			LL += _tryLogTransitionProbs[j][k] - _curLogTransitionProbs[j][k];
		}
	}
	return LL;
}

double TStochasticPrior::calculateLL(const TData &Data, size_t CI_index) const {
	// likelihood: just go through Markov chain and sum up log transition probabilities
	// only need to consider locations relevant for current CI index
	double LL = 0.0;
	for (const auto j : Data.get_locationsIds_for_CI_index(_gamma.species_id(), CI_index)) { // loop over locations
		for (size_t k = 1; k < _logPhi->dimensions()[1]; ++k) {                              // loop over timepoints
			LL += _calculateLogTransitionProbability(j, k, CI_index);
		}
	}
	return LL;
}

void TStochasticPrior::updateGamma(const TData &Data, const TModelBase &Model) {
	for (size_t ix = 0; ix < _gamma.size(); ix++) { // update gamma one by one
		if (!_gamma.updateSpecificIndex(ix)) { continue; }
		double logH = 0.0;
		for (auto CI_index : _gamma.getCIGroups(ix)) { // loop over all CI groups relevant for that gamma
			// remember current sum rho
			_oldSumRhoGamma[CI_index] = _newSumRhoGamma[CI_index];
			_initializeSumRhoGamma(CI_index);
			// remember current phi
			_oldDeterministicPhi[CI_index] = _newDeterministicPhi[CI_index];
			_newDeterministicPhi[CI_index] = _gamma.phi(CI_index);
			const double LLRatio           = calculateLLRatioForUpdateGamma(Data, CI_index);
			const double logPriorRatio     = _gamma.getLogPriorRatio(Data, Model, CI_index); // Jeffrey prior
			logH += LLRatio + logPriorRatio;
		}

		if (_gamma.acceptOrReject(logH, ix)) { // already swaps JP (if necessary)
			// accepted
			swapTryCur(ix, Data); // -> _curLogTransitionProbs = _tryLogTransitionProbs;
		} else {
			// rejected
			for (const auto CI_index : _gamma.getCIGroups(ix)) {
				_newSumRhoGamma[CI_index]      = _oldSumRhoGamma[CI_index];
				_newDeterministicPhi[CI_index] = _oldDeterministicPhi[CI_index];
			}
		}
	}
	// update summaries of gamma posterior after update has finished
	updateSummaryGammaPosterior();
}

bool TStochasticPrior::gammaIsUpdated() const { return _gamma.gammaIsUpdated(); }

void TStochasticPrior::updateGamma_forLogPhiJointUpdate(size_t ix) { _gamma.updateSpecificIndex(ix); }

std::vector<double> TStochasticPrior::getDelta_forLogPhiJointUpdate(size_t CI_index) {
	std::vector<double> Delta(_gamma.numTimepoints(), 0.0);
	if (!_gamma.gammaIsUpdated()) { return Delta; }

	// remember current sum rho
	_oldSumRhoGamma[CI_index] = _newSumRhoGamma[CI_index];
	_initializeSumRhoGamma(CI_index);
	// remember current phi
	_oldDeterministicPhi[CI_index] = _newDeterministicPhi[CI_index];
	_newDeterministicPhi[CI_index] = _gamma.phi(CI_index);

	// calculate Delta
	for (size_t k = 0; k < _gamma.numTimepoints(); ++k) {
		for (size_t l = 0; l <= k; ++l) { Delta[k] += _newSumRhoGamma[CI_index][l] - _oldSumRhoGamma[CI_index][l]; }
	}

	return Delta;
}

bool TStochasticPrior::evaluateGamma_forLogPhiJointUpdate(size_t ix, double LLRatio, const TData &Data,
                                                          const TModelBase &Model) {
	double logH = LLRatio;
	for (const auto CI_index : _gamma.getCIGroups(ix)) {
		logH += _gamma.getLogPriorRatio(Data, Model, CI_index); // Jeffrey prior
	}

	if (_gamma.acceptOrReject(logH, ix)) { // already swaps JP (if necessary)
		// no need to swap transition probabilities: these didn't change!
		return true;
	}
	// rejected
	for (auto CI_index : _gamma.getCIGroups(ix)) {
		_newSumRhoGamma[CI_index]      = _oldSumRhoGamma[CI_index];
		_newDeterministicPhi[CI_index] = _oldDeterministicPhi[CI_index];
	}

	return false;
}

void TStochasticPrior::updateSummaryGammaPosterior() { _gamma.updateSummaryGammaPosterior(); }

double TStochasticPrior::calculateLogPriorRatio(size_t j, size_t k, size_t CI_index) {
	if (k == _gamma.numTimepoints()) { return 0.0; } // last timepoint: no data on the right

	_tryLogTransitionProbs[j][k] = _calculateLogTransitionProbability(j, k, CI_index);
	return _tryLogTransitionProbs[j][k] - _curLogTransitionProbs[j][k];
}

void TStochasticPrior::swapTryCur() {
	// used when sigma updated (-> all transition probabilities change)
	std::swap(_tryLogTransitionProbs, _curLogTransitionProbs);
}

void TStochasticPrior::swapTryCur(size_t ix, const TData &Data) {
	// used when gamma updated
	for (const auto CI_index : _gamma.getCIGroups(ix)) {
		for (const auto j : Data.get_locationsIds_for_CI_index(_gamma.species_id(), CI_index)) {
			std::swap(_tryLogTransitionProbs[j], _curLogTransitionProbs[j]);
		}
	}
}

void TStochasticPrior::swapTryCur(size_t j, size_t k) {
	// used when a phi is updated
	if (k < _gamma.numTimepoints()) { // last timepoint: no data on the right
		std::swap(_tryLogTransitionProbs[j][k], _curLogTransitionProbs[j][k]);
	}
}

double TStochasticPrior::sampleFromTransitionProbability(size_t j, size_t k, size_t CI_index) const {
	coretools::probdist::TNormalDistr distr = _getMeanSdTransitionProbability(j, k, CI_index);
	return distr.sample();
}

void TStochasticPrior::setSimulatedGammaToZero() { _gamma.setSimulatedGammaToZero(); }

TypePhi TStochasticPrior::deterministicPhi(size_t k, size_t CI_index) const {
	return _newDeterministicPhi[CI_index][k];
}

std::vector<TypePhi> TStochasticPrior::calculateDeterministicPhi(size_t CI_index) const { return _gamma.phi(CI_index); }

size_t TStochasticPrior::numEpochs() const { return _gamma.numEpochs(); }
size_t TStochasticPrior::numCIGroups() const { return _gamma.numCIGroups(); }
size_t TStochasticPrior::size() const { return _gamma.size(); }
const std::vector<size_t> &TStochasticPrior::getCIGroups(size_t gamma_ix) const { return _gamma.getCIGroups(gamma_ix); }

void TStochasticPrior::resetSummaryGammaPosterior() { _gamma.resetSummaryGammaPosterior(); }

void TStochasticPrior::writeSummaryGammaPosterior(const std::string &Prefix) {
	_gamma.writeSummaryGammaPosterior(Prefix);
}
