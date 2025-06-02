//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORCHISQ_H
#define TPRIORCHISQ_H

#include "stattools/Priors/TPriorBase.h"
#include "coretools/Main/TRandomGenerator.h"
#include "stattools/Priors/TypesIdentifiers.h"
#include "coretools/Distributions/TChisqDistr.h"
#include "coretools/Distributions/TChiDistr.h"

namespace stattools::prior {

//-------------------------------------------
// TChisqFixed
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim> class TChisqFixed : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(TypesArePositiveFloatingPoints<Type>());

private:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	uint32_t _k       = 1;
	double _kDiv2Min1 = (_k / 2. - 1.);

	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) {
			(*Data)[i] = coretools::instances::randomGenerator().getChisqRand(_k);
		}
	};

public:
	[[nodiscard]] std::string name() const override { return "chisq"; }
	~TChisqFixed() override = default;

	void setFixedPriorParameters(std::string_view Params) override {
		// chisq(k)
		coretools::str::convertString(Params, "Parameter of " + name() + " distribution is k. ", _k);
		if (_k == 0) { UERROR("In Chisq prior: k = 0 is invalid!"); }
		_kDiv2Min1 = (_k / 2. - 1.);
	};

	auto k() const { return _k; }

	double getDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TChisqDistr::density((Type)Data[Index], _k);
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TChisqDistr::logDensity((Type)Data[Index], _k);
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		return _kDiv2Min1 * (log((Type)Data[Index]) - log(Data[Index].oldValue())) +
			   (Data[Index].oldValue() - (Type)Data[Index]) / 2.;
	};
};

//-------------------------------------------
// TMultivariateChisqFixed
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim>
class TMultivariateChisqFixed : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(TypesArePositiveFloatingPoints<Type>() || TypesAreStrictlyPositiveFloatingPoints<Type>());
	// chisq prior with multiple degrees of freedom (k) - one k per parameter
private:
	size_t _D = 0;
	std::vector<size_t> _ks;

protected:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	void _fillK_fromDimensions(size_t D) {
		_D = D;
		_ks.resize(_D);
		for (size_t r = D; r >= 1; r--) { _ks.push_back(r); }
	};

	void _simulateUnderPrior(Storage *Data) override {
		assert(Data->size() == (size_t)_D);
		for (size_t i = 0; i < Data->size(); ++i) {
			(*Data)[i] = coretools::instances::randomGenerator().getChisqRand(_ks[i]);
		}
	};

public:
	~TMultivariateChisqFixed() override = default;
	[[nodiscard]] std::string name() const override { return "multivariateChisq"; }

	void setFixedPriorParameters(std::string_view Params) override {
		// chisq(k1, k2, k3, ..., kD)
		_ks = impl::readDistributionParams_SameTypeUnknownSize<size_t>(
			Params, "Parameters of " + name() + " distribution are (k1, k2, ..., kD).");
		_D = _ks.size();

		// check if size of parameter matches size of k
		for (const auto &storage : this->_storageBelow) {
			if (storage->size() != static_cast<size_t>(_D)) {
				// does not match -> set K from size
				_fillK_fromDimensions(storage->size());
			}
		}
	};

	const auto &ks() const { return _ks; }

	const auto &D() const { return _D; }

	double getDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TChisqDistr::density((double)(Type)Data[Index], _ks[Index]);
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TChisqDistr::logDensity((double)(Type)Data[Index], _ks[Index]);
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		return (_ks[Index] / 2. - 1.) * (log((Type)Data[Index]) - log(Data[Index].oldValue())) +
			   ((double)Data[Index].oldValue() - (double)(Type)Data[Index]) / 2.;
	};
};

//-------------------------------------------
// TMultivariateChiFixed
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim>
class TMultivariateChiFixed : public TMultivariateChisqFixed<Derived, Type, NumDim> {
	// chi prior with multiple degrees of freedom (k) - one k per parameter
	// used for prior on mrr of multivariate normal distribution
	// m_rr^2 ~ Chisq(K)
	// and hence m_rr ~ Chi(K)

private:
	using typename TMultivariateChisqFixed<Derived, Type, NumDim>::Storage;
	using typename TMultivariateChisqFixed<Derived, Type, NumDim>::UpdatedStorage;

	void _simulateUnderPrior(Storage *Data) override {
		assert(Data->size() == (size_t)this->D());
		for (size_t i = 0; i < Data->size(); ++i) {
			(*Data)[i] = sqrt(coretools::instances::randomGenerator().getChisqRand(this->ks()[i]));
		}
	};

public:
	~TMultivariateChiFixed() override = default;
	[[nodiscard]] std::string name() const override { return "multivariateChi"; }

	double getDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TChiDistr::density((double)(Type)Data[Index], this->ks()[Index]);
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TChiDistr::logDensity((double)(Type)Data[Index], this->ks()[Index]);
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		return (this->ks()[Index] - 1.) * (log((Type)Data[Index]) - log(Data[Index].oldValue())) +
			   ((double)Data[Index].oldValue() * (double)Data[Index].oldValue() -
				(double)(Type)Data[Index] * (double)(Type)Data[Index]) /
				   2.;
	};
};

}; // end namespace stattools::prior

#endif // TPRIORCHISQ_H
