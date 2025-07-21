//
// Created by caduffm on 7/1/21.
//

#ifndef TUPDATE_H
#define TUPDATE_H

#include "coretools/Distributions/TBinomialDistr.h"
#include "coretools/Main/TLog.h"
#include "coretools/Math/TMeanVar.h"
#include "coretools/Types/probability.h"
#include "coretools/algorithms.h"
#include "stattools/ParametersObservations/TReadInitialValues.h"
#include "stattools/Updates/TProposal.h"

#ifdef _OPENMP
#include "omp.h"
#endif

namespace stattools {

//----------------------------------------
// TUpdateBase
//----------------------------------------

class TUpdateBase {
	// untemplated, pure virtual base class -> can be stored in vector
public:
	virtual ~TUpdateBase()                                                           = default;
	virtual void adjustProposalWidth()                                               = 0;
	virtual void printAccRateToLogfile() const                                       = 0;
	virtual void clear()                                                             = 0;
	virtual std::pair<size_t, size_t> numAccRatesWithin90PQuantileOfBinomial() const = 0;

	virtual bool proposalWidthIsShared() const                        = 0;
	virtual coretools::Probability acceptanceRate(size_t Index) const = 0;
	virtual size_t size() const                                       = 0;
	virtual bool hasAcceptanceRate() const                            = 0;
	virtual const std::string &name() const                           = 0;
};

//----------------------------------------
// TUpdateTypedBase
//----------------------------------------

template<typename Type> class TUpdateTypedBase : public TUpdateBase {
private:
	typedef decltype(std::declval<Type>().get()) UnderlyingType;

	std::string _name;
	bool _isUpdated = true;
	std::unique_ptr<TPropKernelBase<Type, UnderlyingType>> _propKernel;

protected:
	UnderlyingType _getDefaultProposalWidth() const {
		if constexpr (std::is_same_v<bool, UnderlyingType>) { // bools
			return true;
		} else if constexpr (std::is_integral_v<UnderlyingType>) { // integers
			return 1;
		} else { // floating point numbers
			return 0.1;
		}
	}

	std::pair<size_t, size_t> _get90QuantilesBinomial(size_t NumUpdates) const {
		using coretools::P;
		using coretools::probdist::TBinomialDistr;
		size_t k_lower = TBinomialDistr::invCumulativeDensity(0.05, NumUpdates, P(_propKernel->idealAccRate()));
		size_t k_upper = TBinomialDistr::invCumulativeDensity(0.95, NumUpdates, P(_propKernel->idealAccRate()));
		return {k_lower, k_upper};
	}

	bool _isWithin90QuantileBinomial(size_t NumUpdates, size_t NumAcceptedUpdates) const {
		const auto [k_lower, k_upper] = _get90QuantilesBinomial(NumUpdates);
		return NumAcceptedUpdates >= k_lower && NumAcceptedUpdates <= k_upper;
	}

	UnderlyingType _restrictProposalWidth(UnderlyingType ProposalWidth) {
		return _propKernel->adjustPropKernelIfTooBig(ProposalWidth, _name);
	}

	UnderlyingType _adjustProposalWidth(UnderlyingType ProposalWidth, size_t Index) {
		if (_isUpdated && total(Index) > 0) {
			return _propKernel->adjustProposalWidth(ProposalWidth, this->acceptanceRate(Index), _name);
		}
		return ProposalWidth;
	}

	virtual void _propose(size_t Index) = 0;

public:
	TUpdateTypedBase(bool IsUpdated, std::unique_ptr<TPropKernelBase<Type, UnderlyingType>> PropKernel,
					 std::string_view Name) {
		_isUpdated  = IsUpdated;
		_propKernel = std::move(PropKernel);
		_name       = Name;
	}

	void setRange() { _propKernel->setRange(); }
	void setIsUpdated(bool Update) { _isUpdated = Update; }
	virtual void setProposalWidth(UnderlyingType PropWidth, size_t Index) = 0;
	virtual void setProposalWidth(std::string_view PropWidth)             = 0;

	Type propose(Type Value, size_t Index) {
		if (_isUpdated) {
			_propose(Index);
			return _propKernel->propose(Value, proposalWidth(Index));
		}
		return Value;
	};

	Type propose(Type Value, size_t Index, UnderlyingType Min, UnderlyingType Max, double ProposalWidth) {
		// used for pairwise updates: custom min/max and proposal width
		if (_isUpdated) {
			_propose(Index);
			return _propKernel->propose(Value, Min, Max, ProposalWidth);
		}
		return Value;
	};

	Type proposeWithoutCounting(Type Value, size_t Index) {
		// propose new value without increasing update counters
		if (_isUpdated) { return _propKernel->propose(Value, proposalWidth(Index)); }
		return Value;
	}

	void scaleProposalWidthsToValue(Type Value, size_t Index, double FactorScale = 0.1) {
		if constexpr (!std::is_same_v<UnderlyingType, bool>) { Value *= Type(FactorScale); }
		setProposalWidth(Value, Index);
	}

	virtual void reject(size_t Index) = 0;

	const std::string &name() const override { return _name; }
	bool isUpdated() const { return _isUpdated; }
	bool hasAcceptanceRate() const override { return _isUpdated && _propKernel->isAdjusted() && total() > 0; }

	virtual size_t total(size_t) const                                       = 0;
	virtual size_t total() const                                             = 0;
	virtual UnderlyingType proposalWidth(size_t) const                       = 0;
	virtual std::string getProposalWidthAsString(size_t Index) const         = 0;
	virtual std::string getAllProposalWidthsAsString(std::string_view) const = 0;

	// about proposal kernel
	ProposalKernel::MCMCProposalKernel proposalKernelName() const {
		if (_isUpdated) {
			return _propKernel->name();
		} else {
			return ProposalKernel::missing;
		}
	}
	bool proposalKernelIsReversible() const { return _propKernel->isReversible(); }
	double hastingsRatioPropKernel(Type Value, Type OldValue) const {
		return _propKernel->hastingsRatioPropKernel(Value, OldValue);
	}
	double logHastingsRatioPropKernel(Type Value, Type OldValue) const {
		return _propKernel->logHastingsRatioPropKernel(Value, OldValue);
	}
};

//----------------------------------------
// TUpdateNoAdjust
//----------------------------------------

template<typename Type> class TUpdateNoAdjust : public TUpdateTypedBase<Type> {
	// Class responsible for a proposal width that is not adjustable
private:
	typedef decltype(std::declval<Type>().get()) UnderlyingType;

protected:
	void _propose(size_t /*Index*/) override {
		// empty: no counters to set
	}

public:
	TUpdateNoAdjust(bool IsUpdated, std::unique_ptr<TPropKernelBase<Type, UnderlyingType>> PropKernel,
					std::string_view Name)
		: TUpdateTypedBase<Type>(IsUpdated, std::move(PropKernel), Name) {}

	void setProposalWidth(UnderlyingType /*ProposalWidth*/, size_t /*Index*/) override {
		// empty: no proposal width to set
	}

	void setProposalWidth(std::string_view /*ProposalWidth*/) override {
		// empty: no proposal width to set
	}

	void clear() override {
		// empty: no counters to reset
	}

	void reject(size_t /*Index*/) override {
		// empty: no counters to set
	}

	void adjustProposalWidth() override {
		// empty: no proposal width to adjust
	}

	std::pair<size_t, size_t> numAccRatesWithin90PQuantileOfBinomial() const override { return {0, 0}; }

	size_t size() const override { return 0; }
	size_t total() const override { return 0; }
	size_t total(size_t /*Index*/) const override { return 0; }
	coretools::Probability acceptanceRate(size_t /*Index*/) const override { return coretools::P(0.0); }
	void printAccRateToLogfile() const override {
		// don't print
	}

	UnderlyingType proposalWidth(size_t) const override { return 0.0; }
	std::string getProposalWidthAsString(size_t Index) const override {
		return coretools::str::toString(proposalWidth(Index));
	}
	std::string getAllProposalWidthsAsString(std::string_view) const override { return getProposalWidthAsString(0); }
	bool proposalWidthIsShared() const override { return true; }
};

//----------------------------------------
// TUpdateShared
//----------------------------------------

template<typename Type> class TUpdateShared : public TUpdateTypedBase<Type> {
	// Class responsible for a proposal width that is shared across all indices of a parameter
private:
	typedef decltype(std::declval<Type>().get()) UnderlyingType;

	size_t _sum   = 0; // number of accepted updates
	size_t _total = 0;
	UnderlyingType _proposalWidth{}; // shared across all indices

protected:
	void _propose(size_t /*Index*/) override {
#ifdef _OPENMP
#pragma omp critical // Race condition if multiple threads try to read and write this!
#endif
		{
			++_total;
			++_sum;
		}
	}

public:
	TUpdateShared(bool IsUpdated, std::unique_ptr<TPropKernelBase<Type, UnderlyingType>> PropKernel,
				  std::string_view Name)
		: TUpdateTypedBase<Type>(IsUpdated, std::move(PropKernel), Name) {
		_proposalWidth = this->_restrictProposalWidth(this->_getDefaultProposalWidth());
	};

	void setProposalWidth(UnderlyingType ProposalWidth, size_t /*Index*/) override {
		_proposalWidth = this->_restrictProposalWidth(ProposalWidth);
	}

	void setProposalWidth(std::string_view ProposalWidth) override {
		using namespace coretools::str;
		try {
			std::string propWidth{ProposalWidth};
			coretools::str::eraseAllWhiteSpaces(propWidth);
			_proposalWidth = this->_restrictProposalWidth(fromString<UnderlyingType, true>(propWidth));
		} catch (...) {
			throw coretools::TUserError("Invalid initial proposal width (", ProposalWidth, ") for parameter ", this->name(),
				   "! Should be a number and inside the numerical boundaries of that parameter.");
		}
	}

	void clear() override {
		_sum   = 0;
		_total = 0;
	}

	void reject(size_t /*Index*/) override {
#ifdef _OPENMP
#pragma omp critical // Race condition if multiple threads try to read and write this!
#endif
		--_sum;
	}

	void adjustProposalWidth() override { _proposalWidth = this->_adjustProposalWidth(_proposalWidth, 0); }

	std::pair<size_t, size_t> numAccRatesWithin90PQuantileOfBinomial() const override {
		if (this->hasAcceptanceRate()) { return {this->_isWithin90QuantileBinomial(_total, _sum), 1}; }
		return {0, 0};
	}

	size_t size() const override { return 1; }
	size_t total() const override { return _total; }
	size_t total(size_t /*Index*/) const override { return _total; }
	coretools::Probability acceptanceRate(size_t /*Index*/) const override {
		using coretools::P;
		if (this->isUpdated()) { return P((_sum + 1.) / (_total + 1.)); }
		return P(0.0);
	}
	void printAccRateToLogfile() const override {
		using namespace coretools::instances;
		if (this->hasAcceptanceRate()) {
			logfile().conclude("Acceptance rate ", this->name(), " = ", acceptanceRate(0));
		}
	}

	UnderlyingType proposalWidth(size_t) const override { return _proposalWidth; }
	std::string getProposalWidthAsString(size_t Index) const override {
		return coretools::str::toString(proposalWidth(Index));
	}
	std::string getAllProposalWidthsAsString(std::string_view) const override { return getProposalWidthAsString(0); }
	bool proposalWidthIsShared() const override { return true; }
};

//----------------------------------------
// TUpdateUnique
//----------------------------------------

template<typename Type, bool Regular> class TUpdateUnique : public TUpdateTypedBase<Type> {
	// class responsible for proposal width that is unique for each index of a parameter
private:
	// type alias
	using TypeTotal      = std::conditional_t<Regular, size_t, std::vector<uint32_t>>;
	using UnderlyingType = decltype(std::declval<Type>().get());

	std::vector<uint32_t> _sum;                 // number of accepted updates (per index)
	std::vector<UnderlyingType> _proposalWidth; // per index
	TypeTotal _total;                           // single size_t or vector per index

	void _restrictProposalWidthAll() {
		for (size_t i = 0; i < _proposalWidth.size(); i++) {
			_proposalWidth[i] = this->_restrictProposalWidth(_proposalWidth[i]);
		}
	}

protected:
	void _propose(size_t Index) override {
		++_sum[Index];
		if constexpr (Regular) {
#ifdef _OPENMP
#pragma omp critical // Race condition if multiple threads try to read and write this!
#endif
			{ ++_total; }
		} else {
			++_total[Index];
		}
	}

public:
	TUpdateUnique(size_t Size, bool IsUpdated, std::unique_ptr<TPropKernelBase<Type, UnderlyingType>> PropKernel,
				  std::string_view Name)
		: TUpdateTypedBase<Type>(IsUpdated, std::move(PropKernel), Name) {
		// for integral types & booleans: do not use this class, only allow shared proposal kernels
		// reason: for booleans does not make sense to have unique proposal widths (not adjusted anyways)
		//         for integers, we need to store cumulative probabilities per proposal width -> memory overhead
		//          -> if this is a desired feature at some point, one would need to refactor class a bit such
		//          that these are stored per element
		if (std::is_integral_v<UnderlyingType>) { throw coretools::TDevError("Do not use TUpdateUniqueBase for integer types!"); }

		_sum.resize(Size, 0);
		const auto defaultProp = this->_restrictProposalWidth(this->_getDefaultProposalWidth());
		_proposalWidth.resize(Size, defaultProp);

		if constexpr (Regular) {
			_total = 0;
		} else {
			_total.resize(Size, 0);
		}
	};

	void setProposalWidth(UnderlyingType ProposalWidth, size_t Index) override {
		_proposalWidth[Index] = this->_restrictProposalWidth(ProposalWidth);
	}

	void setProposalWidth(std::string_view ProposalWidth) override {
		// remove white spaces
		std::string propWidth{ProposalWidth};
		coretools::str::eraseAllWhiteSpaces(propWidth);

		TReadInitialValues<UnderlyingType, 1> reader;
		reader.readVals(propWidth, _proposalWidth, this->name());
		_restrictProposalWidthAll();
	}

	void clear() override {
		std::fill(_sum.begin(), _sum.end(), 0);
		if constexpr (Regular) {
			_total = 0;
		} else {
			std::fill(_total.begin(), _total.end(), 0);
		}
	}

	void reject(size_t Index) override { --_sum[Index]; }

	void adjustProposalWidth() override {
		for (size_t i = 0; i < size(); i++) { _proposalWidth[i] = this->_adjustProposalWidth(_proposalWidth[i], i); }
	}

	size_t size() const override { return _proposalWidth.size(); }

	uint32_t sum(size_t Index) const { return _sum[Index]; }

	coretools::Probability meanAcceptanceRate() const {
		// mean over all elements
		coretools::TMeanVar<double> meanVar;
		for (size_t i = 0; i < _sum.size(); i++) {
			if (this->total(i) > 0) { meanVar.add(acceptanceRate(i)); }
		}
		return coretools::P(meanVar.mean());
	}

	coretools::Probability acceptanceRate(size_t Index) const override {
		using coretools::P;
		if (this->isUpdated()) { return P((_sum[Index] + 1.) / (this->total(Index) + 1.)); }
		return P(0.0);
	}

	void printAccRateToLogfile() const override {
		using namespace coretools::instances;
		if (this->hasAcceptanceRate()) {
			logfile().conclude("Mean acceptance rate ", this->name() + " = ", meanAcceptanceRate());
		}
	}

	std::pair<size_t, size_t> numAccRatesWithin90PQuantileOfBinomial() const override {
		// for each update kernel: check if the number of accepted updates lies within the 90% quantiles of a
		// binomial distribution for an acceptance rate = 1/3 return pair: first number is the number of kernels
		// that are within quantile; second number if the total number of kernels
		if (!this->hasAcceptanceRate()) { return {0, 0}; }
		size_t c = 0;
		for (size_t i = 0; i < this->size(); i++) { c += this->_isWithin90QuantileBinomial(total(i), this->sum(i)); }
		return {c, this->size()};
	}

	UnderlyingType proposalWidth(size_t Index) const override { return _proposalWidth[Index]; }

	std::string getProposalWidthAsString(size_t Index) const override {
		return coretools::str::toString(proposalWidth(Index));
	}

	std::string getAllProposalWidthsAsString(std::string_view Delim) const override {
		return coretools::str::concatenateString(_proposalWidth, Delim);
	}

	bool proposalWidthIsShared() const override { return false; }

	size_t total() const override {
		if constexpr (Regular) {
			return _total;
		} else {
			return coretools::containerSum(_total);
		}
	}

	size_t total(size_t Index) const override {
		if constexpr (Regular) {
			return static_cast<size_t>((double)_total / (double)this->size());
		} else {
			return _total[Index];
		}
	}
};

} // namespace stattools

#endif // TUPDATE_H
