#ifndef BIRP_TBIRPCONFIGURATION_H
#define BIRP_TBIRPCONFIGURATION_H

#include "BirpTypes.h"
#include "TData.h"
#include "coretools/Main/TParameters.h"
#include "stattools/DAG/TDAGBuilder.h"
#include "stattools/ParametersObservations/TObservation.h"
#include "stattools/ParametersObservations/TParameter.h"

//------------------------------------------
// TBirpCommonModel
//------------------------------------------

class TBirpCommonModel {
private:
	void _createAlpha(size_t i, TData &Data, const std::string &Filename);
	void _createBeta(size_t i, TData &Data, const std::string &Filename);
	void _createBeta0(size_t i, TData &Data, const std::string &Filename);

	BoxOnGamma _boxOnGamma;
	BoxOnAlpha _boxOnAlpha;
	BoxOnBeta _boxOnBeta;
	BoxOnBeta _boxOnBeta0;

public:
	// all parameters of model (including prior parameters)
	TypeParamGamma gamma;

	// covariates: regression coefficients
	std::vector<std::shared_ptr<TypeParamAlpha>> alpha;
	std::vector<std::shared_ptr<TypeParamBeta>> beta;
	std::vector<std::shared_ptr<TypeParamBeta>> beta0;

	TBirpCommonModel(TData &Data, const std::string &Filename);
};

//------------------------------------------
// TBirpNegBinomAddOn
//------------------------------------------

class TBirpNegBinomAddOn {
private:
	BoxOnMuOrN _boxOnMuOrN;
	BoxOnB _boxOnB;

public:
	// only relevant for negative binomial model
	TypeParamMuOrN muOrN;
	TypeParamB b;

	TBirpNegBinomAddOn(bool HasDetectionCovariates, const std::string &Filename);
};

//------------------------------------------
// TBirpStochasticAddOn
//------------------------------------------

class TBirpStochasticAddOn {
private:
	BoxOnLogPhi _boxOnLogPhi;
	BoxOnLogSigma _boxOnLogSigma;

public:
	// only relevant for stochastic model
	TypeParamLogPhi logPhi;
	TypeParamLogSigma logSigma;

	TBirpStochasticAddOn(const std::string &Filename);
};

//------------------------------------------
// TBirpModel
//------------------------------------------

class TBirpModel {
private:
	TBirpCommonModel _commonModel;
	std::unique_ptr<TBirpNegBinomAddOn> _negBinomAddOn;
	std::unique_ptr<TBirpStochasticAddOn> _stochasticAddOn;

	std::shared_ptr<BirpBox> _birpPrior;

	std::unique_ptr<stattools::TObservation<TypeCounts, NumDimCounts, BirpBox>> _data;

	void (TBirpPrior:: *_funUpdate)();

public:
	TBirpModel(TData &Data, const TUniqueContainer<std::string> &Locations,
	           const TUniqueContainer<std::string> &CIGroupNames, const TUniqueContainer<TypeTime> &TimePoints,
	           const TUniqueContainer<std::string> &SpeciesNames,
	           const TUniqueContainer<std::string> &CovariateEffortNames,
	           const TUniqueContainer<std::string> &CovariateDetectionNames, const std::string &Filename,
	           bool Stochastic, bool Simulate, const std::map<std::string, std::string> &InitVals);

	void writeSummaryGammaPosterior(const std::string &Prefix);

	std::map<std::string, std::string> getFinalValues();
};

#endif // BIRP_TBIRPCONFIGURATION_H
