#ifndef BIRP_TMODELS_H
#define BIRP_TMODELS_H

#include <vector>

#include "coretools/arma_include.h" // note: needs to be included first to avoid include issues with Rcpp and RcppArmadillo
#include "coretools/Math/TMatrix.h"

#include "BirpDAG.h"
#include "BirpTypes.h"
#include "TBirpConfiguration.h"
#include "TData.h"
#include "TDistributions.h"
#include "TGamma.h"

class TGamma;

//---------------------------------------------------
// TModelBase
// pure abstract base class of TStochastic and TDeterministic
//---------------------------------------------------

class TModelBase {
public:
private:
	// temporary values of likelihood
	std::vector<double> _tryLL;
	std::vector<double> _curLL;

	// distribution: Poisson or negative binomial
	std::unique_ptr<TDistributionBase> _distr;

	// Fisher information
	static arma::mat _calcFisherInfo(const TGamma &Gamma, const std::vector<double> &p,
	                                 const std::vector<double> &sigma, TypeNu nu, const TLocations &loc);

protected:
	size_t _speciesIDinUniqueContainer = 0;

	void _initializeDistribution(const TData &Data, const TUniqueContainer<std::string> &Locations,
	                             TBirpPrior *BirpPrior);
	void _estimateInitialParameters(const TData &Data);
	void _updateDistributionParameters(const TData &Data);
	void _simulateDistributionParameters(TData &Data, bool use_n_bar, double n);
	void _simulateConditionedDistributionParameters(TData &Data);
	void _initializeLL(const TData &Data);
	void _updateTryLL(const TData &Data, size_t index_in_counts, size_t ix_method, size_t j_in_method);

public:
	TModelBase(size_t SpeciesIDinUniqueContainer, const TData &Data,
	           const std::unique_ptr<TBirpNegBinomAddOn> &NegBinomAddOn);
	virtual ~TModelBase() = default;

	virtual void initialize(const TData &Data, const TUniqueContainer<std::string> &Locations,
	                        const TUniqueContainer<TypeTime> &TimePoints, TBirpPrior *BirpPrior)  = 0;
	virtual void estimateInitial(const TData &Data, const TUniqueContainer<TypeTime> &Timepoints) = 0;
	virtual void initializeTempVariables(const TData &Data)                                       = 0;
	virtual void update(const TData &Data)                                                        = 0;
	virtual void simulate(TData &Data, bool use_n_bar, double n)                                  = 0;
	virtual void simulateConditioned(TData &Data)                                                 = 0;

	// calculate sigma
	[[nodiscard]] std::vector<double> calculateSigma(const TLocations &loc) const;
	[[nodiscard]] std::vector<double> calculateSigmaFromDeterministicPhi(const TLocations &loc) const;

	// abstract function for phi
	[[nodiscard]] virtual TypePhi phi(size_t location_id, size_t timepoint_k, size_t CI_index) const = 0;
	[[nodiscard]] virtual TypePhi deterministicPhi(size_t timepoint_k, size_t CI_index) const        = 0;

	[[nodiscard]] double calculateJeffreyPrior(const TData &Data, const TGamma &Gamma, size_t CI_index) const;
	double calculateLLRatio(const TData &Data);
	double calculateLLRatio_perLocation(size_t j, const TData &Data);
	double calculateLLRatio_perCIGroup(size_t CI_index, const TData &Data);
	double calculateLLRatio_perMethod(size_t i, const TData &Data);

	void swapTryCur();
	void swapTryCur_perLocation(size_t j, const TData &Data);
	void swapTryCur_perMethod(size_t i, const TData &Data);
	void swapTryCur_perCIGroup(size_t CI_index, const TData &Data);

	[[nodiscard]] double curLL(size_t ij) const;
	double curLLPerMethod(size_t i, const TData &Data);
	[[nodiscard]] double sumCurLL() const;
	void setCurLL(size_t ij, double Value);
	void setCurLLPerMethod(const std::vector<double> &Values, size_t i, const TData &Data);

	// write posterior summaries of gamma's
	virtual void resetSummaryGammaPosterior()                          = 0;
	virtual void writeSummaryGammaPosterior(const std::string &Prefix) = 0;

	virtual stattools::TUpdateBase *getPtrToJointUpdater() { return nullptr; }
};

//------------------------------------------
// TDeterministic
//------------------------------------------

class TDeterministic : public TModelBase {
protected:
	// gamma and times of change
	TGamma _gamma;

	// store temporary phi
	std::vector<std::vector<TypePhi>> _newPhi;
	std::vector<std::vector<TypePhi>> _oldPhi;

	void _fillPhi(size_t CI_ix);
	void _simulate();

public:
	TDeterministic(size_t SpeciesIDinUniqueContainer, const TData &Data,
	               const std::unique_ptr<TBirpNegBinomAddOn> &NegBinomAddOn, TypeParamGamma *Gamma,
	               const TUniqueContainer<TypeTime> &Timepoints, const TUniqueContainer<std::string> &CIGroupNames,
	               std::string_view Prefix);
	~TDeterministic() override = default;

	void initialize(const TData &Data, const TUniqueContainer<std::string> &Locations,
	                const TUniqueContainer<TypeTime> &TimePoints, TBirpPrior *BirpPrior) override;
	void estimateInitial(const TData &Data, const TUniqueContainer<TypeTime> &Timepoints) override;
	void initializeTempVariables(const TData &Data) override;
	void update(const TData &Data) override;

	void simulate(TData &Data, bool use_n_bar, double n) override;
	void simulateConditioned(TData &Data) override;

	[[nodiscard]] TypePhi phi(size_t location_id, size_t timepoint_id, size_t CI_index) const override;
	[[nodiscard]] TypePhi deterministicPhi(size_t timepoint_k, size_t CI_index) const override;

	// write posterior summaries of gamma's
	void resetSummaryGammaPosterior() override;
	void writeSummaryGammaPosterior(const std::string &Prefix) override;
};

//------------------------------------------
// TStochastic
//------------------------------------------

class TStochastic : public TModelBase {
protected:
	// ptr to prior above
	TStochasticPrior _prior;

	// parameters: non-owning
	TypeParamLogPhi *_logPhi     = nullptr;
	TypeParamLogSigma *_logSigma = nullptr;

	std::unique_ptr<stattools::TUpdateUnique<TypeLogPhi, false>> _jointUpdater;

	// initialization
	void _setPhiFromDeterministic(const TData &Data);

	// update single logPhi
	void _updateLogPhi(size_t j, size_t k, const TData &Data);
	void _updateLogPhi(const TData &Data);

	// joint update logPhi and gamma
	void _jointUpdateLogPhiGamma(const TData &Data);

	// joint update of logPhi
	void _shiftLogPhis(size_t j, size_t k, double diff);
	void _jointUpdateLogPhi(size_t j, size_t k, const TData &Data);
	void _jointUpdateLogPhi(const TData &Data);

	// update logSigma
	void _updateLogSigma(const TData &Data);

	// simulate
	void _simulate(TData &Data);

public:
	TStochastic(size_t SpeciesIDinUniqueContainer, const TData &Data, size_t NumLocations, size_t NumTimePoint,
	            const std::unique_ptr<TBirpNegBinomAddOn> &NegBinomAddOn, TypeParamGamma *Gamma,
	            TypeParamLogPhi *LogPhi, TypeParamLogSigma *Logigma, const TUniqueContainer<TypeTime> &Timepoints,
	            const TUniqueContainer<std::string> &CIGroupNames, std::string_view Prefix);
	~TStochastic() override = default;

	void initialize(const TData &Data, const TUniqueContainer<std::string> &Locations,
	                const TUniqueContainer<TypeTime> &TimePoints, TBirpPrior *BirpPrior) override;
	void estimateInitial(const TData &Data, const TUniqueContainer<TypeTime> &Timepoints) override;
	void initializeTempVariables(const TData &Data) override;
	void update(const TData &Data) override;

	void simulate(TData &Data, bool use_n_bar, double n) override;
	void simulateConditioned(TData &Data) override;

	[[nodiscard]] TypePhi phi(size_t location_id, size_t timepoint_k, size_t CI_index) const override;
	[[nodiscard]] TypePhi deterministicPhi(size_t timepoint_k, size_t CI_index) const override;

	// write posterior summaries of gamma's
	void resetSummaryGammaPosterior() override;
	void writeSummaryGammaPosterior(const std::string &Prefix) override;

	stattools::TUpdateBase *getPtrToJointUpdater() override { return _jointUpdater.get(); }
};

#endif // BIRP_TMODELS_H
