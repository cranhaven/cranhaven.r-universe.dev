#ifndef TBIRPPRIOR_H
#define TBIRPPRIOR_H

#include "TModels.h" // note: needs to be included first to avoid include issues with Rcpp and RcppArmadillo
#include "BirpTypes.h"
#include "TBirpConfiguration.h"
#include "TData.h"
#include "coretools/Files/TOutputFile.h"

class TBirpPrior : public stattools::prior::TBaseLikelihoodPrior<stattools::TObservationBase, TypeCounts, 2> {
private:
	using typename stattools::prior::TBaseLikelihoodPrior<stattools::TObservationBase, TypeCounts, 2>::Storage;
	using typename stattools::prior::TBaseLikelihoodPrior<stattools::TObservationBase, TypeCounts, 2>::UpdatedStorage;

	using BoxType = TBirpPrior;

protected:
	// data
	TData &_data;

	// model
	std::vector<std::unique_ptr<TModelBase>> _models;

	// locations, timepoints, covNames: these are references to the objects that are stored in TBirpCore
	const TUniqueContainer<std::string> &_locations;
	const TUniqueContainer<std::string> &_CIGroupNames;
	const TUniqueContainer<TypeTime> &_timepoints;
	const TUniqueContainer<std::string> &_covariateEffortNames;
	const TUniqueContainer<std::string> &_covariateDetectionNames;

	size_t _numSpecies = 0;

	// prefix for writing
	std::string _out;

	// simulate values
	void _simulateUnderPrior(Storage *Data) override;

public:
	TBirpPrior(TData &Data, const TUniqueContainer<std::string> &Locations,
	           const TUniqueContainer<std::string> &CIGroupNames, const TUniqueContainer<TypeTime> &TimePoints,
	           const TUniqueContainer<std::string> &CovariateEffortNames,
	           const TUniqueContainer<std::string> &CovariateDetectionNames, size_t numSpecies,
	           TBirpCommonModel &CommonModel, std::unique_ptr<TBirpNegBinomAddOn> &NegBinomAddOn,
	           std::unique_ptr<TBirpStochasticAddOn> &StochasticAddOn, std::string Prefix);
	~TBirpPrior() override = default;

	void initialize() override;

	[[nodiscard]] std::string name() const override;

	// estimate initial values
	void guessInitialValues() override;

	// full log densities
	double getSumLogPriorDensity(const Storage &) const override;

	// update all parameters
	void updateParams();

	// write summaries of gamma posterior
	void burninHasFinished() override;
	void writeSummaryGammaPosterior(const std::string &Prefix);

	// getters for locations, methods, timepoints, species for tests
	const TUniqueContainer<std::string> &locations() const;
	const TUniqueContainer<TypeTime> &timepoints() const;
	const TData &data() const;

	stattools::TUpdateBase *getPtrToJointUpdater() { return _models[0]->getPtrToJointUpdater(); }
};

#endif // TBIRPPRIOR_H
