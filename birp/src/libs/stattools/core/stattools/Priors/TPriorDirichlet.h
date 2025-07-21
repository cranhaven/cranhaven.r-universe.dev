//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORDIRICHLET_H
#define TPRIORDIRICHLET_H

#include "coretools/Distributions/TDirichletDistr.h"
#include "coretools/Types/commonWeakTypes.h"
#include "stattools/MLEInference/TLineSearch.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TPriorBase.h"
#include "stattools/Priors/TypesIdentifiers.h"

namespace stattools::prior {

//-------------------------------------------
// TDirichletFixed
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim>
class TDirichletFixed : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(TypesAreZeroOpenOneClosedFloatingPoints<Type>() || TypesAreStrictlyPositiveWithVariableMax<Type>());

protected:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	coretools::probdist::TDirichletDistr _dirichletDistribution;
	double _normalizingBeta = 0.0;

	void _setNormalizingBeta() {
		_normalizingBeta = std::pow(1.0 / _dirichletDistribution.beta(), 1.0 / (double)_dirichletDistribution.size());
	}
	void _setDirichletDistr(const std::vector<coretools::StrictlyPositive> &Alphas) {
		_dirichletDistribution.set(Alphas); // will do the check
		_setNormalizingBeta();
	}

	void _simulateUnderPrior(Storage *Data) override {
		assert(_dirichletDistribution.size() == Data->size());

		// get random values from Dirichlet
		std::vector<coretools::ZeroOpenOneClosed> values;
		_dirichletDistribution.fillRandom(values);
		for (size_t i = 0; i < Data->size(); ++i) { (*Data)[i] = values[i].get(); }
	};

	static double _calculateLogPriorDensity(const Storage &Data, const coretools::probdist::TDirichletDistr &Distr,
											size_t Index) {
		// calculate log(prod_k x_k^(alpha_k - 1)) for a sinlge k
		// --> -log(beta(alpha))/K + (alpha_k - 1) * log(x_k)
		return -Distr.betaLog() / (double)Distr.size() + (Distr.alphas()[Index] - 1.) * log((Type)Data[Index]);
	}

public:
	~TDirichletFixed() override = default;

	[[nodiscard]] std::string name() const override { return "dirichlet"; }

	void setFixedPriorParameters(std::string_view Params) override {
		// dirichlet(alpha1, alpha2, ..., alphaK)
		auto alphas = impl::readDistributionParams_SameTypeUnknownSize<coretools::StrictlyPositive>(
			Params, "Parameters of " + name() + " distribution are (alpha_1, alpha_2, ..., alpha_K).");
		_setDirichletDistr(alphas);
	};

	void initialize() override {
		// check if size of parameter matches size of alpha
		for (const auto &storage : this->_storageBelow) {
			if (storage->size() != _dirichletDistribution.size()) {
				throw coretools::TDevError("Parameter has a different size (", storage->size(),
						 ") than expected based on size of alpha ( ", _dirichletDistribution.size(), ")!");
			}
		}
	}

	double getDensity(const Storage &Data, size_t Index) const override {
		// calculate 1 / Beta(alpha) * prod_k x_k^(alpha_k - 1) for a single k
		// --> prod_k (1 / Beta(alpha))^(1 / K) * x_k^(alpha_k - 1)
		return _normalizingBeta * pow((Type)Data[Index], _dirichletDistribution.alphas()[Index] - 1.);
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return _calculateLogPriorDensity(Data, _dirichletDistribution, Index);
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		return (_dirichletDistribution.alphas()[Index] - 1.) *
			   log((double)(Type)Data[Index] / (double)Data[Index].oldValue());
	};

	double getSumLogPriorDensity(const Storage &Data) const override {
		double sum = 0.0;
		for (size_t k = 0; k < _dirichletDistribution.size(); ++k) { sum += getLogDensity(Data, k); }
		return sum;
	};
};

//-------------------------------------------
// TDirichletVarInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecSigma>
class TDirichletVarInferred : public TDirichletFixed<Derived, Type, NumDim> {
	static_assert((TypesAreZeroOpenOneClosedFloatingPoints<Type>() ||
				   TypesAreStrictlyPositiveWithVariableMax<Type>()) &&
				  TypesAreStrictlyPositive<typename SpecSigma::value_type>());

protected:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;
	using Base = TDirichletFixed<Derived, Type, NumDim>;
	using TDirichletFixed<Derived, Type, NumDim>::_dirichletDistribution;
	using BoxType        = TDirichletVarInferred<Derived, Type, NumDim, SpecSigma>;
	using TypeParamSigma = TParameter<SpecSigma, BoxType>;

	coretools::probdist::TDirichletDistr _tempDist;

	std::vector<coretools::StrictlyPositive> _alphas;
	TypeParamSigma *_sigma = nullptr;

	double _getLogDensity_lineSearch(double LogSigma) {
		_sigma->set(exp(LogSigma));
		setScaledAlphas();

		double LL = 0.0;
		for (const auto &storage : this->_storageBelow) {
			for (size_t k = 0; k < _dirichletDistribution.size(); ++k) { LL += Base::getLogDensity(*storage, k); }
		}
		return LL;
	}

	void _estimateInitialSigma() {
		TLineSearch lineSearch;
		auto ptr    = &TDirichletVarInferred<Derived, Type, NumDim, SpecSigma>::_getLogDensity_lineSearch;
		auto result = lineSearch.findMax(*this, ptr, 0.0, 0.1, 10e-05, 100);

		_sigma->set(exp(result.result()));
		setScaledAlphas();
	}

	void _simulateUnderPrior(Storage *Data) override {
		_dirichletDistribution.set(scaleAlphas());
		TDirichletFixed<Derived, Type, NumDim>::_simulateUnderPrior(Data);
	};

public:
	TDirichletVarInferred(TypeParamSigma *Sigma) : _sigma(Sigma) { this->addPriorParameter(_sigma); };
	~TDirichletVarInferred() override = default;

	[[nodiscard]] std::string name() const override { return "dirichletWithVar"; }

	void setFixedPriorParameters(std::string_view Params) override {
		// store alphas (never modified)
		TDirichletFixed<Derived, Type, NumDim>::setFixedPriorParameters(Params);
		_alphas = _dirichletDistribution.alphas();
	};

	void initialize() override {
		TDirichletFixed<Derived, Type, NumDim>::initialize();

		_sigma->initStorage(this);
		_sigma->set(0.1); // avoid gammaLog issues with too small sigma
		setScaledAlphas();
	}

	[[nodiscard]] std::vector<coretools::StrictlyPositive> scaleAlphas() const {
		std::vector<coretools::StrictlyPositive> scaledAlphas(_alphas.size());
		for (size_t i = 0; i < _alphas.size(); i++) {
			scaledAlphas[i] = std::max(_alphas[i] / _sigma->value(), (double)coretools::StrictlyPositive::min());
		}
		return scaledAlphas;
	}

	void setScaledAlphas() { this->_setDirichletDistr(scaleAlphas()); }

	auto sigma() const { return _sigma->value(); }

	[[nodiscard]] auto calculateLLRatio(TypeParamSigma *, size_t) {
		_tempDist.set(scaleAlphas());

		auto f = [this](Storage *Data) {
			double ratio = 0.0;
			for (size_t k = 0; k < _alphas.size(); ++k) {
				ratio += Base::_calculateLogPriorDensity(*Data, _tempDist, k) -
						 Base::_calculateLogPriorDensity(*Data, _dirichletDistribution, k);
			}
			return ratio;
		};
		return f;
	}

	void updateTempVals(TypeParamSigma *, size_t, bool Accepted) {
		// this function is called if the update of sigma is accepted
		if (Accepted) {
			_dirichletDistribution = _tempDist;
			this->_setNormalizingBeta();
		}
	}

	void guessInitialValues() override { _estimateInitialSigma(); }
};

//-------------------------------------------
// TDirichletMeanVarInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecSigma, typename SpecP>
class TDirichletMeanVarInferred : public TStochasticBase<Derived, Type, NumDim> {
	static_assert((TypesAreZeroOpenOneClosedFloatingPoints<Type>() ||
				   TypesAreStrictlyPositiveWithVariableMax<Type>()) &&
				  NumDim == 2 && TypesAreStrictlyPositive<typename SpecSigma::value_type>() &&
				  TypesAreZeroOpenOneClosedFloatingPoints<typename SpecP::value_type>());
	static_assert(SpecP::constraint::constraint == Constraints::sumOne);

protected:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;
	using Base           = TStochasticBase<Derived, Type, NumDim>;
	using BoxType        = TDirichletMeanVarInferred<Derived, Type, NumDim, SpecSigma, SpecP>;
	using TypeParamSigma = TParameter<SpecSigma, BoxType>;
	using TypeParamP     = TParameter<SpecP, BoxType>;

	coretools::probdist::TDirichletDistr _tryDist;
	coretools::probdist::TDirichletDistr _curDist;

	std::vector<coretools::StrictlyPositive> _tryAlphas;
	std::vector<coretools::StrictlyPositive> _curAlphas;

	TypeParamSigma *_sigma = nullptr;
	TypeParamP *_p         = nullptr;

	void _estimateInitialP() {
		// initial p = average value across all rows
		std::vector<coretools::TMeanVar<double>> colMeans(_p->size());
		for (const auto &storage : this->_storageBelow) {
			for (size_t i = 0; i < storage->dimensions()[0]; ++i) {
				for (size_t k = 0; k < storage->dimensions()[1]; ++k) { colMeans[k].add((Type)(*storage)(i, k)); }
			}
		}

		for (size_t k = 0; k < _p->size(); ++k) { _p->set(k, colMeans[k].mean()); }
	}

	double _getLogDensity_lineSearch(double LogSigma) {
		_sigma->set(exp(LogSigma));
		setTryAlpha();
		accept();

		double LL = 0.0;
		for (const auto &storage : this->_storageBelow) {
			for (size_t i = 0; i < storage->size(); ++i) { LL += getLogDensity(*storage, i); }
		}
		return LL;
	}

	void _estimateInitialSigma() {
		TLineSearch lineSearch;
		auto ptr    = &TDirichletMeanVarInferred<Derived, Type, NumDim, SpecSigma, SpecP>::_getLogDensity_lineSearch;
		auto result = lineSearch.findMax(*this, ptr, 0.0, 0.1, 10e-05, 100);

		_sigma->set(exp(result.result()));
		setTryAlpha();
		accept();
	}

	void _simulateUnderPrior(Storage *Data) override {
		setTryAlpha();
		accept();

		// get random values from Dirichlet (re-draw per row)
		std::vector<coretools::ZeroOpenOneClosed> values;
		for (size_t i = 0; i < Data->dimensions()[0]; ++i) {
			_tryDist.fillRandom(values);
			for (size_t k = 0; k < _tryDist.size(); ++k) { (*Data)(i, k) = values[k].get(); }
		}
	}

	double _calculateLogPriorDensity(const Storage &Data, const coretools::probdist::TDirichletDistr &Distr, size_t k,
									 size_t i) const {
		return (Distr.alphas()[k] - 1.) * log((Type)Data(i, k));
	}

	double _calculateLogPriorDensity(const Storage &Data, const coretools::probdist::TDirichletDistr &Distr,
									 size_t k) const {
		// loop over all rows of data
		double sum = 0.0;
		for (size_t i = 0; i < Data.dimensions()[0]; ++i) { sum += _calculateLogPriorDensity(Data, Distr, k, i); }
		return sum;
	}

public:
	TDirichletMeanVarInferred(TypeParamSigma *Sigma, TypeParamP *P) : _sigma(Sigma), _p(P) {
		this->addPriorParameter({_sigma, _p});
	}
	~TDirichletMeanVarInferred() override = default;

	[[nodiscard]] std::string name() const override { return "dirichletWithMeanVar"; }

	void initialize() override {
		// sigma: single value
		_sigma->initStorage(this);

		// p: vector of length M = ncol(data)
		size_t M = this->_storageBelow[0]->dimensions()[1];
		for (const auto &storage : this->_storageBelow) {
			if (storage->dimensions()[1] != M) {
				throw coretools::TDevError("Parameter has a different size (", storage->size(),
						 ") than expected based on size of other ( ", M, ")!");
			}
		}
		_p->initStorage(this, {M}, {this->_storageBelow[0]->getDimensionName(1)});

		_tryAlphas.resize(M);
	}

	auto sigma() const { return _sigma->value(); }
	auto p(size_t i) const { return _p->value(i); }

	void accept() {
		_curDist   = _tryDist;
		_curAlphas = _tryAlphas;
	}

	void setTryAlpha() {
		for (size_t i = 0; i < _p->size(); i++) {
			_tryAlphas[i] = std::max(_p->value(i) / _sigma->value(), (double)coretools::StrictlyPositive::min());
		}

		_tryDist.set(_tryAlphas);
	}

	[[nodiscard]] auto calculateLLRatio(TypeParamSigma *, size_t) {
		setTryAlpha();

		auto f = [this](Storage *Data) {
			double ratio = (double)Data->dimensions()[0] * (_curDist.betaLog() - _tryDist.betaLog());
			for (size_t k = 0; k < _tryAlphas.size(); ++k) {
				ratio += _calculateLogPriorDensity(*Data, _tryDist, k) - _calculateLogPriorDensity(*Data, _curDist, k);
			}
			return ratio;
		};
		return f;
	}

	void updateTempVals(TypeParamSigma *, size_t, bool Accepted) {
		if (Accepted) { accept(); }
	}

	[[nodiscard]] auto calculateLLRatio(TypeParamP *, const coretools::TRange &Ix) {
		setTryAlpha();

		auto f = [this, &Ix](Storage *Data) {
			double ratio = (double)Data->dimensions()[0] * (_curDist.betaLog() - _tryDist.betaLog());

			for (size_t k = Ix.begin; k < Ix.end; k += Ix.increment) {
				ratio += _calculateLogPriorDensity(*Data, _tryDist, k) - _calculateLogPriorDensity(*Data, _curDist, k);
			}
			return ratio;
		};
		return f;
	}

	void updateTempVals(TypeParamP *, size_t, bool Accepted) {
		if (Accepted) { accept(); }
	}

	void guessInitialValues() override {
		_estimateInitialP();
		_estimateInitialSigma();
	}

	double getDensity(const Storage &Data, size_t Index) const override {
		const auto coord = Data.getSubscripts(Index);
		const size_t k   = coord[1]; // column index

		const double beta = std::pow(1.0 / _tryDist.beta(), 1.0 / (double)_tryDist.size()); // K^th square root
		return beta * pow((Type)Data[Index], _curDist.alphas()[k] - 1.);
	}

	double getLogDensity(const Storage &Data, size_t Index) const override {
		const auto coord = Data.getSubscripts(Index);
		const size_t i   = coord[0];
		const size_t k   = coord[1];
		return -_curDist.betaLog() / (double)_curDist.size() + _calculateLogPriorDensity(Data, _curDist, k, i);
	}

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		const auto coord = Data.getSubscripts(Index);
		const size_t k   = coord[1];
		return (_curDist.alphas()[k] - 1.) * log((double)(Type)Data[Index] / (double)Data[Index].oldValue());
	}
};

} // namespace stattools::prior

#endif // TPRIORDIRICHLET_H
