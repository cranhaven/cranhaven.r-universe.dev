#ifndef BIRP_TDATA_H
#define BIRP_TDATA_H

#include "BirpDAG.h"
#include "BirpTypes.h"
#include "TUniqueContainer.h"
#include "stattools/ParametersObservations/TParameter.h"
#include <string>
#include <vector>

class TModelBase;
class TData;
class TDistributionBase;

//-------------------------------------------
// TTimepoints
//-------------------------------------------

class TTimepoints {
private:
	// counts
	std::vector<TypeCounts> _counts_per_species;

	// covariates
	std::vector<TypeCovariateEffort> _covariatesEffort;
	std::vector<TypeCovariateDetection> _covariatesDetection;

	// timepoint id
	size_t _timepoint_id = 0;

	// store effort (updated in MCMC)
	TypeEffort _effort        = 0;
	TypeEffort _effort_old    = 0;
	double _detectionTerm     = 0.0;
	double _detectionTerm_old = 0.0;
	double _effortTerm        = 0.0;
	double _effortTerm_old    = 0.0;

	// covariates
	void _standardizeCovariatesDetection(const std::vector<coretools::TMeanVar<double>> &MeanVar,
	                                     bool AssumeTrueDetectionProbability);
	void _standardizeCovariatesEffort(const std::vector<double> &SumsPerCovariate, size_t NumTimepoints,
	                                  size_t NumLocations);
	double _calculateEffortTerm(const std::shared_ptr<TypeParamAlpha> &Alpha);
	double _calculateDetectionTerm(const std::shared_ptr<TypeParamBeta> &Beta0,
	                               const std::shared_ptr<TypeParamBeta> &Beta);

public:
	TTimepoints() = default;

	TTimepoints(const std::vector<TypeCounts> &Counts, const std::vector<TypeCovariateEffort> &CovsEffort,
	            const std::vector<TypeCovariateDetection> &CovsDetection, const size_t &Timepoint_id);

	explicit TTimepoints(size_t timepoint_id, size_t numSpecies, size_t numCovariatesEffort,
	                     size_t numCovariatesDetection);

	// setters
	void setTimepoint(size_t timepointToAdd);
	void setCounts(size_t species_id, TypeCounts counts);

	void setCovariateEffort(size_t c, TypeCovariateEffort Covariate);
	void setCovariateDetection(size_t c, TypeCovariateDetection Covariate);
	void standardizeCovariates(const std::vector<coretools::TMeanVar<double>> &MeanVar,
	                           const std::vector<double> &SumsPerCovariate, bool AssumeTrueDetectionProbability,
	                           size_t NumTimepoints, size_t NumLocations);

	void write(coretools::TOutputMaybeRcppFile &File, std::string location,
	           const TUniqueContainer<TypeTime> &Timepoints, const std::string &CIGroupName) const;

	// getters
	[[nodiscard]] const std::vector<TypeCovariateEffort> &covariatesEffort() const;
	[[nodiscard]] const std::vector<TypeCovariateDetection> &covariatesDetection() const;
	[[nodiscard]] size_t numSpecies() const;
	[[nodiscard]] TypeCounts counts_per_species(size_t species_id) const;
	[[nodiscard]] TypeEffort effort() const;
	[[nodiscard]] size_t timepoint_id() const;
	[[nodiscard]] bool inferAlpha() const;
	[[nodiscard]] bool inferBeta() const;

	// update effort
	void initializeEffort(const std::shared_ptr<TypeParamAlpha> &Alpha, const std::shared_ptr<TypeParamBeta> &Beta0,
	                      const std::shared_ptr<TypeParamBeta> &Beta);
	void updateAlphaEffort(TypeAlpha NewAlpha_c1, TypeAlpha OldAlpha_c1, TypeAlpha NewAlpha_c2, TypeAlpha OldAlpha_c2,
	                       size_t c1, size_t c2);
	void updateBetaEffort(const std::shared_ptr<TypeParamBeta> &Beta0, const std::shared_ptr<TypeParamBeta> &Beta);
	void resetAlphaEffort();
	void resetBetaEffort();

	// functions for simulating covariates
	template<class Object, bool SimulateCovEffort> void simulateCovariate(size_t c, Object obj) {
		if (SimulateCovEffort) {
			_covariatesEffort[c] = obj.sample();
		} else {
			_covariatesDetection[c] = obj.sample();
		}
	}

	double calculatePhiEffort(double phi);

	// functions for simulating under Poisson model
	void simulatePoisson(double N, double phi);

	// functions for simulating under Negative Binomial model
	void simulateNB(double phi, double a, double N);
};

//-------------------------------------------
// TLocations
//-------------------------------------------

class TLocations {
private:
	size_t _location_id = 0;
	std::vector<TTimepoints> _timepoints;
	std::vector<TypeNu> _nu;
	size_t _group_id;

public:
	TLocations() = default;

	TLocations(const TTimepoints &pt, size_t location_id, size_t group_id);

	TLocations(size_t location_id, size_t group_id, size_t TimepointSize, size_t numSpecies, size_t numCovariatesEffort,
	           size_t numCovariatesDetection);

	// setters
	void add(const TTimepoints &pt);

	void sorttimes(const std::vector<size_t> &index);

	// getters
	[[nodiscard]] size_t size() const;
	[[nodiscard]] TypeNu nu(size_t species_id) const;
	[[nodiscard]] size_t location_id() const;
	[[nodiscard]] size_t group_id() const;
	[[nodiscard]] const TTimepoints &operator[](size_t index) const;

	void write(coretools::TOutputMaybeRcppFile &File, const TUniqueContainer<std::string> &Locations,
	           const TUniqueContainer<TypeTime> &Timepoints, const TUniqueContainer<std::string> &CIGroupNames) const;

	// iterators
	[[nodiscard]] std::vector<TTimepoints>::const_iterator cbegin() const;
	std::vector<TTimepoints>::iterator begin();
	[[nodiscard]] std::vector<TTimepoints>::const_iterator cend() const;
	std::vector<TTimepoints>::iterator end();

	// update effort
	void initializeEffort(const std::shared_ptr<TypeParamAlpha> &Alpha, const std::shared_ptr<TypeParamBeta> &Beta0,
	                      const std::shared_ptr<TypeParamBeta> &Beta);
	void updateAlphaEffort(TypeAlpha NewAlpha_c1, TypeAlpha OldAlpha_c1, TypeAlpha NewAlpha_c2, TypeAlpha OldAlpha_c2,
	                       size_t c1, size_t c2);
	void updateBetaEffort(const std::shared_ptr<TypeParamBeta> &Beta0, const std::shared_ptr<TypeParamBeta> &Beta);
	void resetAlphaEffort();
	void resetBetaEffort();

	// functions for simulating effort
	template<class Object, bool SimulateCovEffort> void simulateCovariate(size_t c, Object obj) {
		for (auto &timepoint : _timepoints) { timepoint.simulateCovariate<Object, SimulateCovEffort>(c, obj); }
	}

	// functions for simulating under Poisson model
	void simulatePoisson(double N, const TModelBase &Model);
	void simulateMultinomial(const TModelBase &Model);

	// functions for simulating under Negative Binomial model
	void simulateNB(const TModelBase &Model, double a, double N);
	void simulateDirichletMultinomial(const TModelBase &Model, double mu, double b);
};

//-------------------------------------------
// TMethods
//-------------------------------------------

class TMethods {
private:
	std::string _methodName;
	size_t _i = 0;

	std::vector<TLocations> _locations;

	std::vector<size_t> _covariateEffortIDsinUniqueContainer;
	std::vector<size_t> _covariateDetectionIDsinUniqueContainer;

	// variables to identify species in epoch model
	std::vector<size_t> _speciesIDsinUniqueContainer;
	std::vector<bool> _hasDataForSpeciesID;
	std::vector<size_t> _indexInCounts;

	// variables to identify locations
	std::vector<bool> _hasDataForLocationID;
	std::vector<size_t> _map_j_location;

	// variables to identify CI group
	std::vector<bool> _hasDataForCIGroup;
	std::vector<std::vector<size_t>> _location_per_CI_group;

	// alpha and beta (regression coefficients of covariates)
	std::shared_ptr<TypeParamAlpha> _alpha;
	std::shared_ptr<TypeParamBeta> _beta;
	std::shared_ptr<TypeParamBeta> _beta0;

	// update functions for alpha and beta
	void _updateAlpha(size_t c1, size_t c2, const TData &Data, const std::vector<std::unique_ptr<TModelBase>> &Models);
	void _updateAlpha(const TData &Data, const std::vector<std::unique_ptr<TModelBase>> &Models);
	void _updateBeta(size_t d, const TData &Data, const std::vector<std::unique_ptr<TModelBase>> &Models,
	                 const std::shared_ptr<TypeParamBeta> &Beta);
	void _updateBeta(const TData &Data, const std::vector<std::unique_ptr<TModelBase>> &Models);
	void _setValuesForSingleCovariate();

	// standardize covariates
	auto _getMeanVarDetectionCovariates();
	auto _getSumEffortCovariates();

	// simulate
	void _simulateOneCovariateEffort(size_t c, const std::string &Cov);
	void _simulateOneCovariateDetection(size_t c, const std::string &Cov);
	static std::pair<std::string, std::string> _getParamsAndDistr(const std::string &Cov);
	template<class Distribution, bool SimulateCovEffort>
	void _simulateCovariateFromDistribution(size_t c, const std::string &Params) {
		Distribution distr(Params);
		for (size_t i = 0; i < size(); i++) {
			_locations[i].simulateCovariate<Distribution, SimulateCovEffort>(c, distr);
		}
	}
	void _simulateCovariatesDetection();
	void _simulateCovariatesEffort();
	void _simulateZeroCovariates();
	void _simulateCovariates();
	double _calculateU_i(double n_i_bar, const TModelBase &Model);

	// index lookup stuff
	void _fillLocationIDs(const TUniqueContainer<std::string> &Locations);
	void _fillCIGroupIDs(const TUniqueContainer<std::string> &CIGroupNames);

public:
	explicit TMethods(const std::string_view &methodName, size_t index,
	                  const std::vector<std::size_t> &speciesIDsinUniqueContainer,
	                  const std::vector<std::size_t> &covariateEffortIDsinUniqueContainer,
	                  const std::vector<std::size_t> &covariateDetectionIDsinUniqueContainer);

	TMethods(std::string methodName, size_t locationSize, size_t TimepointSize, size_t index,
	         const std::vector<size_t> &speciesIDsinUniqueContainer,
	         const std::vector<std::size_t> &covariateEffortIDsinUniqueContainer,
	         const std::vector<std::size_t> &covariateDetectionIDsinUniqueContainer, size_t NumCIGroups,
	         const TData &Data);

	TMethods() = default;

	// add parameters alpha and beta
	void addAlphaBeta(const std::shared_ptr<TypeParamAlpha> &Alpha, const std::shared_ptr<TypeParamBeta> &Beta0,
	                  std::shared_ptr<TypeParamBeta> &Beta);
	void initialize(const TUniqueContainer<std::string> &CovariateEffortNames,
	                const TUniqueContainer<std::string> &CovariateDetectionNames, TBirpPrior *BirpPrior);

	// add data
	void add(const TTimepoints &pt, size_t location_id, size_t group_id,
	         const TUniqueContainer<std::string> &LocationNames, const TUniqueContainer<std::string> &CIGroupNames);

	// finalize data filling
	void sorttimes(const std::vector<size_t> &index);
	void standardizeCovariates();
	void fillIDVectors(size_t totalSizeOfUniqueContainer);
	void initializeEffort();
	void initializeLookups(const TUniqueContainer<std::string> &Locations,
	                       const TUniqueContainer<std::string> &CIGroupNames);

	// getters
	[[nodiscard]] size_t size() const;
	[[nodiscard]] const std::string &name() const;
	[[nodiscard]] size_t numCovariatesEffort() const;
	[[nodiscard]] size_t numCovariatesDetection() const;
	[[nodiscard]] bool inferAlpha() const;
	[[nodiscard]] bool inferBeta() const;

	// access counts and covariates
	[[nodiscard]] const std::vector<size_t> &speciesIDsinUniqueContainer() const;
	[[nodiscard]] const std::vector<size_t> &covariateEffortIDsinUniqueContainer() const;
	[[nodiscard]] const std::vector<size_t> &covariateDetectionIDsinUniqueContainer() const;
	[[nodiscard]] bool hasDataForSpeciesID(size_t speciesIDinUniqueContainer) const;
	[[nodiscard]] size_t getIndexInCounts(size_t speciesIDinUniqueContainer) const;
	[[nodiscard]] bool hasDataForLocation(size_t j) const;
	[[nodiscard]] size_t getLocationIndexInMethod(size_t j) const;
	[[nodiscard]] bool hasDataForCIGroup(size_t CI_index) const;
	[[nodiscard]] const std::vector<size_t> &getLocationIndicesForCIGroup(size_t CI_index) const;

	// iterators
	const TLocations &operator[](const size_t index) const { return _locations[index]; }
	[[nodiscard]] std::vector<TLocations>::const_iterator cbegin() const;
	std::vector<TLocations>::iterator begin();
	[[nodiscard]] std::vector<TLocations>::const_iterator cend() const;
	std::vector<TLocations>::iterator end();

	// update effort
	void estimateInitialAlphaBeta();
	void updateAlphaBeta(const TData &Data, const std::vector<std::unique_ptr<TModelBase>> &Models);

	// functions for simulating
	void simulatePoisson(bool use_n_bar, double n, const TModelBase &Model);
	void simulateMultinomial(const TModelBase &Model);
	double simulateNB(bool use_n_bar, double n, const TModelBase &Model, double A);
	void simulateDirichletMultinomial(const TModelBase &Model, const std::vector<double> &Mu, double B);

	// writing
	void write(coretools::TOutputMaybeRcppFile &File, const TUniqueContainer<std::string> &Locations,
	           const TUniqueContainer<TypeTime> &Timepoints, const TUniqueContainer<std::string> &CIGroupNames) const;
};

//-------------------------------------------
// TData
//-------------------------------------------

class TData {
private:
	std::vector<TMethods> _methods;

	size_t _numSpecies = 0;

	std::vector<size_t> _numMethLoc; // per species

	// indices (per species)
	std::vector<std::vector<std::pair<size_t, size_t>>> _linear_to_i_j;
	std::vector<std::vector<std::vector<size_t>>> _i_j_to_linear;
	std::vector<std::vector<std::vector<size_t>>> _ij_per_locationId;
	std::vector<std::vector<size_t>> _CI_index_per_locationId;
	std::vector<std::vector<std::vector<size_t>>> _locationsIds_per_CI_index;

	// fill indices
	void _fillNumMethLocs();
	[[nodiscard]] std::pair<size_t, size_t> _getMethAndLocIndex(size_t ij_index, size_t speciesID) const;
	void _fillLinearToIJ();
	void _fillIJToLinear();
	void _fillIJPerLocationID(const TUniqueContainer<std::string> &Locations);
	void _fillLocationIDPerCI(const TUniqueContainer<std::string> &Locations,
	                          const TUniqueContainer<std::string> &CIGroupNames);

public:
	TData();
	virtual ~TData() = default;

	void addMethod(const TMethods &Method);

	void fillMethLocIndices(size_t NumSpecies, const TUniqueContainer<std::string> &Locations,
	                        const TUniqueContainer<std::string> &CIGroupNames);

	// iterators
	[[nodiscard]] size_t size() const;
	[[nodiscard]] const TMethods &operator[](size_t index) const;
	TMethods &operator[](size_t index);
	[[nodiscard]] std::vector<TMethods>::const_iterator cbegin() const;
	std::vector<TMethods>::iterator begin();
	[[nodiscard]] std::vector<TMethods>::const_iterator cend() const;
	std::vector<TMethods>::iterator end();
	TMethods &back();

	// getters
	[[nodiscard]] size_t numMethLoc(size_t SpeciesIDInUniqueContainer) const;
	[[nodiscard]] size_t i_j_to_linear(size_t SpeciesIDInUniqueContainer, size_t i, size_t j) const;
	[[nodiscard]] const std::pair<size_t, size_t> &linear_to_i_j(size_t SpeciesIDInUniqueContainer,
	                                                             size_t linear) const;
	[[nodiscard]] const std::vector<size_t> &locationId_to_ij(size_t SpeciesIDInUniqueContainer,
	                                                          size_t location_id) const;
	[[nodiscard]] size_t locationsId_to_CI_index(size_t SpeciesIDInUniqueContainer, size_t location_id) const;
	[[nodiscard]] const std::vector<size_t> &get_locationsIds_for_CI_index(size_t SpeciesIDInUniqueContainer,
	                                                                       size_t CI_index) const;

	// clear
	void clear();
};

#endif // BIRP_TDATA_H
