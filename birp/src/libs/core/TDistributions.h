#ifndef BIRP_TDISTRIBUTIONS_H
#define BIRP_TDISTRIBUTIONS_H

#include <vector>
#include "coretools/arma_include.h" // note: needs to be included first to avoid include issues with Rcpp and RcppArmadillo

#include "coretools/Math/TMatrix.h"

#include "stattools/ParametersObservations/TObservation.h"
#include "stattools/ParametersObservations/TParameter.h"

#include "BirpTypes.h"
#include "TData.h"

// forward declarations
class TModelBase;

//---------------------------------------
// TDistribution
//---------------------------------------
class TDistributionBase {
protected:
	size_t _speciesIDinUniqueContainer = 0; // refers to ID in unique container

public:
	explicit TDistributionBase(size_t SpeciesIDinUniqueContainer);
	virtual ~TDistributionBase() = default;

	virtual void initialize(const TData &Data, const TUniqueContainer<std::string> &Locations,
	                        TBirpPrior *BirpPrior)            = 0;
	virtual void update(const TData &Data, TModelBase &Model) = 0;
	virtual void estimateInitialParameters(const TData &Data) = 0;

	[[nodiscard]] virtual double calculateLL(const std::vector<double> &sigma, const TLocations &loc,
	                                         const std::vector<double> &p, size_t index_in_counts,
	                                         size_t ix_linNumMethLoc, size_t ix_method) const = 0;

	virtual void simulate(TData &Data, const TModelBase &Model, bool use_n_bar, double n) = 0;
	virtual void simulateConditioned(TData &Data, const TModelBase &Model)                = 0;
};

//---------------------------------------
// TPoissonDistribution
//---------------------------------------
class TPoissonDistribution : public TDistributionBase {
public:
	explicit TPoissonDistribution(size_t SpeciesIDinUniqueContainer);
	~TPoissonDistribution() override = default;

	void initialize(const TData &, const TUniqueContainer<std::string> &, TBirpPrior *) override {};
	void update(const TData &, TModelBase &) override {};
	void estimateInitialParameters(const TData &) override {};

	[[nodiscard]] double calculateLL(const std::vector<double> &sigma, const TLocations &loc,
	                                 const std::vector<double> &p, size_t index_in_counts, size_t ix_linNumMethLoc,
	                                 size_t ix_method) const override;

	// simulation
	void simulate(TData &Data, const TModelBase &Model, bool use_n_bar, double n) override;
	void simulateConditioned(TData &Data, const TModelBase &Model) override;
};

//--------------------------------
// TNegBinDistribution
//--------------------------------

class TNegBinDistribution : public TDistributionBase {
protected:
	// parameters: mu (or N) and b
	TypeParamMuOrN *_muOrN = nullptr;
	TypeParamB *_b         = nullptr;
	bool _isMu;

	[[nodiscard]] double _getMuOrN(size_t j, size_t ij) const;

	// update functions
	void _updateMu(const TData &Data, TModelBase &Model);
	void _updateB(const TData &Data, TModelBase &Model);
	std::pair<double, std::vector<double>> _calcHastingsRatioN(const std::vector<size_t> &ij, const TData &Data,
	                                                           TModelBase &Model);
	void _updateN(const TData &Data, TModelBase &Model);

	// initialization
	void _initializeN(const TUniqueContainer<std::string> &Locations, TBirpPrior *BirpPrior);
	void _initializeMu(const TData &Data, const TUniqueContainer<std::string> &Locations, TBirpPrior *BirpPrior);
	void _estimateInitialMu(const TData &Data);
	void _estimateInitialN(const TData &Data);
	std::vector<double> _getNuPerIJ(const TData &Data);

	// simulate
	static std::vector<double> _readAFromCommandLine(const TData &Data);

public:
	TNegBinDistribution(size_t SpeciesIDinUniqueContainer, TypeParamMuOrN *MuOrN, TypeParamB *B);
	~TNegBinDistribution() override = default;

	void initialize(const TData &Data, const TUniqueContainer<std::string> &Locations, TBirpPrior *BirpPrior) override;
	void update(const TData &Data, TModelBase &Model) override;
	void estimateInitialParameters(const TData &Data) override;

	[[nodiscard]] double calculateLL(const std::vector<double> &sigma, const TLocations &loc,
	                                 const std::vector<double> &p, size_t index_in_counts, size_t ix_linNumMethLoc,
	                                 size_t ix_method) const override;

	// simulation
	void simulate(TData &Data, const TModelBase &Model, bool use_n_bar, double n) override;
	void simulateConditioned(TData &Data, const TModelBase &Model) override;
};

#endif // BIRP_TDISTRIBUTIONS_H
