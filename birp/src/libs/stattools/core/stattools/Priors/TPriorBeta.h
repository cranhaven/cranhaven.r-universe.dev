//
// Created by caduffm on 3/15/22.
//

#ifndef TPRIORBETA_H
#define TPRIORBETA_H

#include "coretools/Distributions/TBetaDistr.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TPriorBase.h"

namespace stattools::prior {

//-------------------------------------------
// TBetaSymmetricFixed
//-------------------------------------------

namespace impl {
template<typename Storage, typename Type> void simulateSafeBeta(Storage *Data, double Alpha, double Beta) {
	for (size_t i = 0; i < Data->size(); ++i) {
		double value = coretools::instances::randomGenerator().getBetaRandom(Alpha, Beta);
		while (std::isnan(value) || std::isinf(value) || value < Type::min() || value > Type::max()) {
			value = coretools::instances::randomGenerator().getBetaRandom(Alpha, Beta);
		}
		(*Data)[i] = value;
	}
};
} // namespace impl

template<typename Derived, typename Type, size_t NumDim>
class TBetaSymmetricFixed : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(TypesAreZeroOneOpenFloatingPoints<Type>() || TypesAreZeroOpenOneClosed<Type>());

protected:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	coretools::StrictlyPositive _alpha = coretools::StrictlyPositive::min();

	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) {
			(*Data)[i] = (Type)coretools::instances::randomGenerator().getBetaRandom(_alpha, _alpha);
		}
	};

public:
	~TBetaSymmetricFixed() override = default;

	[[nodiscard]] std::string name() const override { return "betaSymmetric"; }

	void setFixedPriorParameters(std::string_view Params) override {
		coretools::str::convertString(Params, "Parameter of " + name() + " distribution is alpha. ", _alpha);
	};

	auto alpha() const { return _alpha; }

	double getDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TBetaDistr::density(coretools::P((Type)Data[Index]), _alpha, _alpha);
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TBetaDistr::logDensity(coretools::P((Type)Data[Index]), _alpha, _alpha);
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		const double amin1 = alpha() - 1;
		return amin1 * (log((Type)Data[Index]) - log(Data[Index].oldValue())) +
			   amin1 * (log(1. - (Type)Data[Index]) - log(1. - Data[Index].oldValue()));
	};
};

//-------------------------------------------
// TBetaFixed
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim>
class TBetaFixed : public TBetaSymmetricFixed<Derived, Type, NumDim> {
	static_assert(TypesAreZeroOneOpenFloatingPoints<Type>() || TypesAreZeroOpenOneClosed<Type>());

private:
	coretools::StrictlyPositive _beta = coretools::StrictlyPositive::min();

	using typename TBetaSymmetricFixed<Derived, Type, NumDim>::Storage;
	using typename TBetaSymmetricFixed<Derived, Type, NumDim>::UpdatedStorage;

	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) {
			(*Data)[i] = (Type)coretools::instances::randomGenerator().getBetaRandom(this->alpha(), _beta);
		}
	};

public:
	~TBetaFixed() override = default;

	[[nodiscard]] std::string name() const override { return "beta"; }

	void setFixedPriorParameters(std::string_view Params) override {
		coretools::str::convertString(Params, "Parameters of " + name() + " distribution are alpha,beta. ",
									  this->_alpha, _beta);
	};

	auto beta() const { return _beta; }

	double getDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TBetaDistr::density(coretools::P((Type)Data[Index]), this->alpha(), _beta);
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TBetaDistr::logDensity(coretools::P((Type)Data[Index]), this->alpha(), _beta);
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		const double amin1 = this->alpha() - 1;
		const double bmin1 = beta() - 1;
		return amin1 * (log((Type)Data[Index]) - log(Data[Index].oldValue())) +
			   bmin1 * (log(1. - (Type)Data[Index]) - log(1. - Data[Index].oldValue()));
	};
};

//-------------------------------------------
// TBetaSymmetricInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecAlpha>
class TBetaSymmetricInferred : public TStochasticBase<Derived, Type, NumDim> {
	static_assert((TypesAreZeroOneOpenFloatingPoints<Type>() ||
				   TypesAreZeroOpenOneClosed<Type>()) && // Type must be (0, 1) or (0, 1]
				  TypesAreStrictlyPositiveFloatingPoints<typename SpecAlpha::value_type>());

private:
	using BoxType        = TBetaSymmetricInferred<Derived, Type, NumDim, SpecAlpha>;
	using TypeParamAlpha = TParameter<SpecAlpha, BoxType>;

protected:
	TypeParamAlpha *_alpha = nullptr;

	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	void _simulateUnderPrior(Storage *Data) override { impl::simulateSafeBeta<Storage, Type>(Data, alpha(), alpha()); };

public:
	explicit TBetaSymmetricInferred(TypeParamAlpha *Alpha) : _alpha(Alpha) { this->addPriorParameter(_alpha); };
	~TBetaSymmetricInferred() override = default;

	[[nodiscard]] std::string name() const override { return "betaSymmetric"; }

	void initialize() override { _alpha->initStorage(this); }

	constexpr auto alpha() const noexcept { return _alpha->value(); }

	constexpr auto oldAlpha() const noexcept { return _alpha->oldValue(); }

	double getDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TBetaDistr::density(coretools::P((Type)Data[Index]), alpha(), alpha());
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TBetaDistr::logDensity(coretools::P((Type)Data[Index]), alpha(), alpha());
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		const double amin1 = alpha() - 1;
		return amin1 * (log((Type)Data[Index]) - log(Data[Index].oldValue())) +
			   amin1 * (log(1. - (Type)Data[Index]) - log(1. - Data[Index].oldValue()));
	};

	[[nodiscard]] auto calculateLLRatio(TypeParamAlpha *, size_t) const {
		using coretools::gammaLog;
		double v1 =
			gammaLog(2.0 * alpha()) + 2.0 * gammaLog(oldAlpha()) - gammaLog(2.0 * oldAlpha()) - 2.0 * gammaLog(alpha());
		double v2 = (double)alpha() - (double)oldAlpha();
		auto f    = [v1, v2](Storage *Data) {
			return Data->size() * v1 + v2 * Data->customLogSum([](auto v) { return -(v - 1.) * v; });
		};
		return f;
	}

	void updateTempVals(TypeParamAlpha *, size_t, bool){/* empty: no tmp variables */};

	void guessInitialValues() override {
		auto [mean, var] = impl::calculateMeanVarOfStorages<Storage *, Type>(this->_storageBelow);

		// mean of symmetric Beta must be 0.5
		// var = 1/(4(2*alpha + 1)) -> solve for alpha
		// -> Method of Moments estimate for alpha = 1/8*var - 0.5
		double MOM = coretools::probdist::TBetaDistr::calculateSymmetricAlphaForGivenMeanVar(var);
		_alpha->set(MOM);
	}
};

//-------------------------------------------
// TBetaInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecAlpha, typename SpecBeta>
class TBetaInferred : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(TypesAreZeroOneOpenFloatingPoints<Type>() &&
				  TypesAreStrictlyPositiveFloatingPoints<typename SpecAlpha::value_type>() &&
				  TypesAreStrictlyPositiveFloatingPoints<typename SpecBeta::value_type>());

private:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;
	using BoxType        = TBetaInferred<Derived, Type, NumDim, SpecAlpha, SpecBeta>;
	using TypeParamAlpha = TParameter<SpecAlpha, BoxType>;
	using TypeParamBeta  = TParameter<SpecBeta, BoxType>;

	TypeParamAlpha *_alpha = nullptr;
	TypeParamBeta *_beta   = nullptr;

protected:
	void _setAlphaToMOM(double mean, double nu) { _alpha->set(mean * nu); }

	void _setBetaToMOM(double mean, double nu) {
		const auto MLE = (1. - mean) * nu;
		_beta->set(MLE);
	};

	void _simulateUnderPrior(Storage *Data) override { impl::simulateSafeBeta<Storage, Type>(Data, alpha(), beta()); };

public:
	TBetaInferred(TypeParamAlpha *Alpha, TypeParamBeta *Beta)
		: TStochasticBase<Derived, Type, NumDim>(), _alpha(Alpha), _beta(Beta) {
		this->addPriorParameter({_alpha, _beta});
	};
	~TBetaInferred() override = default;

	[[nodiscard]] std::string name() const override { return "beta"; }

	void initialize() override {
		_alpha->initStorage(this);
		_beta->initStorage(this);
	};

	constexpr auto alpha() const noexcept { return _alpha->value(); }

	constexpr auto oldAlpha() const noexcept { return _alpha->oldValue(); }

	constexpr auto beta() const noexcept { return _beta->value(); }

	constexpr auto oldBeta() const noexcept { return _beta->oldValue(); }

	double getDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TBetaDistr::density(coretools::P((Type)Data[Index]), alpha(), beta());
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return coretools::probdist::TBetaDistr::logDensity(coretools::P((Type)Data[Index]), alpha(), beta());
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		const double amin1 = alpha() - 1;
		const double bmin1 = beta() - 1;
		return amin1 * (log((Type)Data[Index]) - log(Data[Index].oldValue())) +
			   bmin1 * (log(1. - (Type)Data[Index]) - log(1. - Data[Index].oldValue()));
	};

	auto calculateLLRatio(TypeParamAlpha *, size_t) const {
		using coretools::gammaLog;
		const auto a    = alpha();
		const auto oldA = oldAlpha();
		double v1       = gammaLog(a + beta()) + gammaLog(oldA) - gammaLog(oldA + beta()) - gammaLog(a);
		double v2       = (double)a - (double)oldA;
		auto f          = [v1, v2](Storage *Data) { return Data->size() * v1 + v2 * Data->sumOfLogs(); };
		return f;
	}

	auto calculateLLRatio(TypeParamBeta *, size_t) const {
		using coretools::gammaLog;
		const auto a = alpha();
		double v1    = gammaLog(a + beta()) + gammaLog(oldBeta()) - gammaLog(a + oldBeta()) - gammaLog(beta());
		double v2    = (double)beta() - (double)oldBeta();
		auto f       = [v1, v2](Storage *Data) { return Data->size() * v1 + v2 * Data->sumOfLogsComplement(); };
		return f;
	}

	void updateTempVals(TypeParamAlpha *, size_t, bool){/* empty: no tmp variables */};
	void updateTempVals(TypeParamBeta *, size_t, bool){/* empty: no tmp variables */};

	void guessInitialValues() override {
		auto [mean, var] = impl::calculateMeanVarOfStorages<Storage *, Type>(this->_storageBelow);

		// calculate nu
		double nu = (mean * (1. - mean)) / var - 1.;
		if (nu <= 0. || std::isinf(nu)) {
			// invalid nu -> re-set such that alpha = beta = 0.5 result
			nu   = 1.;
			mean = 0.5;
		}

		_setAlphaToMOM(mean, nu);
		_setBetaToMOM(mean, nu);
	}
};

//-------------------------------------------
// TBetaSymmetricZeroMixtureInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecAlpha, typename SpecZ, typename BoxTypeZ,
		 size_t ValueZBetaModel = 1>
class TBetaSymmetricZeroMixtureInferred : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(TypesAreZeroOneClosed<Type>() && // Type must be [0, 1]
				  (TypesAreUnsignedInteger<typename SpecZ::value_type>() ||
				   TypesAreBool<typename SpecZ::value_type>()) && // TypeZ must be integer or bool
				  TypesAreStrictlyPositiveFloatingPoints<typename SpecAlpha::value_type>());
	/* Symmetric Beta prior that includes the value "0" and "1" by using an indicator variable z:
	 * z = ValueZBetaModel: symmetric Beta distribution
	 * z != ValueZBetaModel: exactly zero or exactly one
	 * NOTE: This prior is NOT responsible for updating z!
	 * It simply uses z as an indicator for the update of alpha.
	 * z must be updated "below" in DAG, since this is an RJ-MCMC: when updating z, we also need to calculate the
	 * LL-ratio for the update of the parameter -> specific to model below, can not be done in here.
	 * BoxTypeZ corresponds to the box (prior) type of the prior that manages z
	 */
private:
	using BoxType =
		TBetaSymmetricZeroMixtureInferred<Derived, Type, NumDim, SpecAlpha, SpecZ, BoxTypeZ, ValueZBetaModel>;
	using TypeParamAlpha = TParameter<SpecAlpha, BoxType>;
	using TypeParamZ     = TParameter<SpecZ, BoxTypeZ>;

	TypeParamAlpha *_alpha = nullptr;
	TypeParamZ *_z         = nullptr;

protected:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	void _simulateUnderPrior(Storage *Data) override {
		using namespace coretools::instances;
		for (size_t i = 0; i < Data->size(); ++i) {
			if (_z->value(i) != ValueZBetaModel) {
				randomGenerator().pickOneOfTwo() ? (*Data)[i] = Type(0.0) : (*Data)[i] = Type(1.0);
			} else {
				double value = randomGenerator().getBetaRandom(alpha(), alpha());
				while (std::isnan(value) || std::isinf(value) || value < Type::min() || value > Type::max()) {
					value = randomGenerator().getBetaRandom(alpha(), alpha());
				}
				(*Data)[i] = Type(value);
			}
		}
	};

	size_t _getNumElementsInBetaModel() const {
		size_t c = 0;
		for (size_t i = 0; i < _z->size(); ++i) {
			if (_z->value(i) == ValueZBetaModel) { ++c; }
		}
		return c;
	}

public:
	TBetaSymmetricZeroMixtureInferred(TypeParamAlpha *Alpha, TypeParamZ *Z) : _alpha(Alpha), _z(Z) {
		this->addPriorParameter(_alpha); // do not add z in here: not strictly a prior parameter!
	};
	~TBetaSymmetricZeroMixtureInferred() override = default;

	[[nodiscard]] std::string name() const override { return "betaSymmetricZeroMixture"; }

	void initialize() override {
		_alpha->initStorage(this);
		// do not initialize z in here: not strictly a prior parameter!

		// check if size of z matches size of parameter
		for (auto &p : this->_storageBelow) {
			if (p->size() != _z->size()) {
				throw coretools::TDevError("Size of storage below (" + coretools::str::toString(p->size()) +
						 ") does not match size of z (" + coretools::str::toString(_z->size()) + ")!");
			}
		}
	};

	constexpr auto alpha() const noexcept { return _alpha->value(); }

	constexpr auto oldAlpha() const noexcept { return _alpha->oldValue(); }

	double getDensity(const Storage &Data, size_t Index) const override {
		if (_z->value(Index) != ValueZBetaModel) { return 1.0; }
		return coretools::probdist::TBetaDistr::density(coretools::P((Type)Data[Index]), alpha(), alpha());
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		if (_z->value(Index) != ValueZBetaModel) { return 0.0; }
		return coretools::probdist::TBetaDistr::logDensity(coretools::P((Type)Data[Index]), alpha(), alpha());
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		if (_z->value(Index) != ValueZBetaModel) { return 0.0; }
		const double amin1 = alpha() - 1;
		return amin1 * (log((Type)Data[Index]) - log(Data[Index].oldValue())) +
			   amin1 * (log(1. - (Type)Data[Index]) - log(1. - Data[Index].oldValue()));
	};

	auto calculateLLRatio(TypeParamAlpha *, size_t) const {
		using coretools::gammaLog;
		double v1 =
			gammaLog(2.0 * alpha()) + 2.0 * gammaLog(oldAlpha()) - gammaLog(2.0 * oldAlpha()) - 2.0 * gammaLog(alpha());
		double v2 = (double)alpha() - (double)oldAlpha();

		auto f = [v1, v2, z = _z, this](Storage *Data) {
			if (_getNumElementsInBetaModel() == 0) {          // there are no z that are in the Beta distribution model
				return std::numeric_limits<double>::lowest(); // always reject
			}

			coretools::TSumLog<> sum;
			size_t c = 0;
			for (size_t i = 0; i < Data->size(); ++i) {
				if (z->value(i) == ValueZBetaModel) {
					const auto x = value((*Data)[i]);
					sum.add(-(x - 1.) * x);
					++c;
				}
			}
			return (double)c * v1 + v2 * sum.getSum();
		};
		return f;
	}

	void updateTempVals(TypeParamAlpha *, size_t, bool){/* empty: no tmp variables */};

	void guessInitialValues() override {
		// calculate mean and variance of all data for which z = 1
		coretools::TMeanVar<double> meanVar;
		for (const auto &storage : this->_storageBelow) {
			for (size_t i = 0; i < storage->size(); ++i) {
				if (_z->value(i) == ValueZBetaModel) { meanVar.add((Type)(*storage)[i]); }
			}
		}

		// mean of symmetric Beta must be 0.5
		// var = 1/(4(2*alpha + 1)) -> solve for alpha
		// -> Method of Moments estimate for alpha = 1/8*var - 0.5
		double MOM = 1. / (8. * meanVar.variance()) - 0.5;
		if (MOM <= 0. || std::isinf(MOM)) {
			// invalid nu -> re-set such that alpha = beta = 0.5 result
			MOM = 0.5;
		}
		_alpha->set(MOM);
	}
};

} // end namespace stattools::prior

#endif // TPRIORBETA_H
