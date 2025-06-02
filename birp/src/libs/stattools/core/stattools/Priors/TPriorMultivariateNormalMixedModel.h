//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORMULTIVARIATENORMALMIXEDMODEL_H
#define TPRIORMULTIVARIATENORMALMIXEDMODEL_H

#include <cstddef>
#include <vector>
#include "coretools/arma_include.h"
#include "stattools/EM/TDataVector.h"

namespace stattools::prior {

namespace impl::mvn_mix {

void handleEMMaximizationOneIteration_updateSigma(size_t k, size_t D, const std::vector<arma::mat> &sumXpiMinusMuSquare,
												  const TDataVector<double, size_t> &Weights,
												  arma::mat &EM_update_Sigma_temp);
void updateEMParametersOneIteration_Sigma(arma::mat &Sigma, size_t D, size_t NumParametersBelow,
										  arma::mat &EM_update_Sigma_temp, double EM_update_Weights_temp);

template<typename Storage> auto getDimensionNameForZ(const std::vector<Storage *> &StorageBelow) {
	// derive N and dimension names from dimensionality of parameters below
	auto firstStorage     = StorageBelow[0];
	size_t N              = firstStorage->dimensions()[0];
	auto dimNamesFirstDim = firstStorage->getDimensionName(0);

	for (const auto &storage : StorageBelow) {
		if (storage->dimensions()[0] != N) {
			DEVERROR("N of storage below (=", storage->dimensions()[0], ") and of other shared storage (=", N,
					 ") do not match!");
		}
		// check if ptr to dimension names is the same
		if (storage->getDimensionName(0) != dimNamesFirstDim) {
			DEVERROR("Pointer to dimension name class of dimension 0 of storage below differs from that of other "
					 "shared storage!");
		}
	}
	return dimNamesFirstDim;
}

template<typename TypeParamM, typename TypeParamMrr, typename TypeParamMrs>
void checkForFixedInitialValues(const TypeParamM *M, const TypeParamMrr *Mrr, const TypeParamMrs *Mrs,
								std::string_view Name) {
	// check: either set all M-parameters (m, Mrr and Mrs) of a component, or none. Throw if only part of them
	// should be initialized
	if (!Mrr->hasFixedInitialValue() || !Mrs->hasFixedInitialValue() ||
		!M->hasFixedInitialValue()) { // at least one should be estimated
		if (!Mrr->hasFixedInitialValue() && !Mrs->hasFixedInitialValue() && !M->hasFixedInitialValue()) {
			// all are estimated -> ok
		} else {
			UERROR("Error when initializing ", Name,
				   " prior: parameters m, Mrr and Mrs for must either all or none have an initial value. Can not "
				   "initialize only partially!");
		}
	}
}

} // namespace impl::mvn_mix

/*
//-------------------------------------------------
// TMultivariateNormalMixedModelInferred
//-------------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecZ, typename SpecMus, typename SpecM,
		 typename SpecMrr, typename SpecMrs, size_t NumComponents>
class TMultivariateNormalMixedModelInferred : public TStochasticBase<Derived, Type, NumDim>,
											  public TLatentVariable<double, size_t, size_t> {
	static_assert(TypesAreUnsignedIntWithVariableMax<typename SpecZ::value_type>() ||
				  TypesAreBool<typename SpecZ::value_type>());
	static_assert(TypesAreUnboundedFloatingPoints<Type, typename SpecMus::value_type, typename SpecMrs::value_type>());
	static_assert(TypesAreStrictlyPositiveFloatingPoints<typename SpecM::value_type, typename SpecMrr::value_type>());
	static_assert(NumDim == 2);

private:
	using BoxType = TMultivariateNormalMixedModelInferred<Derived, Type, NumDim, SpecZ, SpecMus, SpecM, SpecMrr,
														  SpecMrs, NumComponents>;
	using Base    = TStochasticBase<Derived, Type, NumDim>;

	using TypeParamZ   = TParameter<SpecZ, BoxType>;
	using TypeParamMus = TParameter<SpecMus, BoxType>;
	using TypeParamM   = TParameter<SpecM, BoxType>;
	using TypeParamMrr = TParameter<SpecMrr, BoxType>;
	using TypeParamMrs = TParameter<SpecMrs, BoxType>;
	using typename Base::Storage;
	using typename Base::UpdatedStorage;

protected:
	TypeParamZ *_z;
	std::array<TypeParamMus *, NumComponents> _mus;
	std::array<TypeParamM *, NumComponents> _m;
	std::array<TypeParamMrr *, NumComponents> _Mrr;
	std::array<TypeParamMrs *, NumComponents> _Mrs;

	// dimensions and temporary variables
	impl::mvn::DimMVN _dim{};

	// EM
	std::vector<double> _EM_update_Mus_temp;
	std::vector<arma::mat> _EM_update_Sigma_temp;
	std::vector<double> _EM_update_Weights_temp;

	[[nodiscard]] std::vector<std::vector<double>> _calculateMeanParameterValues() const {
		// fill a vector with the mean values of all parameters
		// dimensions: N times D
		std::vector<std::vector<double>> meanParamValues(_z->size(), std::vector<double>(_dim.D, 0.));

		for (const auto &storage : this->_storageBelow) {
			for (size_t i = 0; i < storage->dimensions()[0]; i++) { // go over all rows (N)
				size_t c = 0;
				for (auto r = storage->beginSlice(0, i); !r.end(); ++r, ++c) { // go over all cols of current row (D)
					meanParamValues[i][c] += (Type)(*storage)[r.cur()];
				}
			}
		}

		// normalize
		for (auto &it : meanParamValues) {
			for (auto &it2 : it) { it2 /= this->_storageBelow.size(); }
		}
		return meanParamValues;
	};

	std::vector<std::vector<Type>>
	_findMostDistantPoints(const std::vector<std::vector<double>> &meanParameterValues) const {
		std::vector<double> minDistances(_z->size());
		std::vector<std::vector<Type>> minValues(NumComponents, std::vector<Type>(_dim.D, 0)); // K times D

		// 1st value = 1st group
		for (size_t d = 0; d < _dim.D; d++) { minValues[0][d] = meanParameterValues[0][d]; }

		for (size_t k = 1; k < NumComponents; k++) {
			for (size_t i = 0; i < _z->size(); i++) {
				// find minimal distance among all previous k
				double minDist = coretools::euclideanDistance(meanParameterValues[i], minValues[0]);
				for (size_t smallerK = 1; smallerK < k; smallerK++) {
					double tmp = coretools::euclideanDistance(meanParameterValues[i], minValues[smallerK]);
					if (tmp < minDist) { minDist = tmp; }
				}
				minDistances[i] = minDist;
			}

			// assign the data point that maximizes minDistances to the next K
			size_t maxIndex =
				std::distance(minDistances.begin(), std::max_element(minDistances.begin(), minDistances.end()));
			for (size_t d = 0; d < _dim.D; d++) { minValues[k][d] = meanParameterValues[maxIndex][d]; }
		}
		return minValues;
	};

	void _setInitialZ(const std::vector<std::vector<Type>> &minValues,
					  const std::vector<std::vector<double>> &meanParameterValues) {
		// go over the data again and assign the closest group to each point
		for (size_t i = 0; i < _z->size(); i++) {
			size_t minK = 0;
			for (size_t k = 1; k < NumComponents; k++) {
				if (coretools::euclideanDistance(meanParameterValues[i], minValues[k]) <
					coretools::euclideanDistance(meanParameterValues[i], minValues[minK])) {
					minK = k;
				}
			}
			_z->set(i, minK);
		}
	};

	void _setMusToMLE() {
		// calculate MLE of mus (based on classification by z)

		// initialize data: K times D
		std::vector<std::vector<double>> sums(NumComponents, std::vector<double>(_dim.D, 0.));
		std::vector<double> totalNumElements(NumComponents, 0.);

		for (const auto &storage : this->_storageBelow) {
			for (size_t n = 0; n < storage->dimensions()[0]; n++) { // go over all rows (N)
				const auto k = _z->value(n);
				impl::mvn::addToSumMLEMu(sums[k], n, *storage);
				totalNumElements[k]++;
			}
		}

		// normalize and set mus
		for (size_t k = 0; k < NumComponents; k++) {
			impl::mvn::normalizeSumMLEMu(sums[k], totalNumElements[k]);
			for (size_t d = 0; d < _dim.D; d++) { _mus[k]->set(d, sums[k][d]); }
		}
	};

	void _setMToMLE() {
		// calculate MLE of M (based on classification by z)
		// initialize data for sums: vector of size K with armadillo matrices of dimensions D times D
		std::vector<arma::mat> sums(NumComponents, arma::mat(_dim.D, _dim.D, arma::fill::zeros));
		std::vector<double> totalNumElements(NumComponents, 0.);
		for (const auto &storage : this->_storageBelow) {
			for (size_t n = 0; n < storage->dimensions()[0]; n++) { // go over all rows (N)
				const auto k = _z->value(n);
				impl::mvn::addToSumMLESigma(sums[k], n, *storage, _mus[k]);
				totalNumElements[k]++;
			}
		}

		// normalize
		for (size_t k = 0; k < NumComponents; k++) { impl::mvn::normalizeSumMLESigma(sums[k], totalNumElements[k]); }

		// fill parameters of M
		for (size_t k = 0; k < NumComponents; k++) {
			impl::mvn::fillParametersMFromSigma(sums[k], _dim.D, _Mrr[k], _Mrs[k], _m[k]);
		}
	};

	std::vector<double> _calculatePosterior_sampleZ(size_t i) {
		// prepare
		std::vector<double> densities(NumComponents, 0.0);
		_z->set(i, 0); // switch new->old once, in order to preserve correct old values

		// loop over all possible values of z and calculate density
		for (size_t z = 0; z < NumComponents; z++) {
			_z->setVal(i, z);
			densities[z] = calcLikelihoodSampleZ(i, z) * _z->getDensity(i);
		}
		return densities;
	};

	void _sampleZ(size_t i) { _z->sampleDiscreteFromUnnormalizedProbabilities(i, _calculatePosterior_sampleZ(i)); };

	void _updateMu(size_t k, size_t r) { this->_evaluateUpdate(calcLLRatioMu(k, r), _mus[k], r); };

	void _updateM(size_t k) { this->_evaluateUpdate(calcLLRatioM(k), _m[k]); };

	void _updateMrr(size_t k, size_t r) { this->_evaluateUpdate(calcLLRatioMrr(k, r), _Mrr[k], r); };

	void _updateMrs(size_t k, size_t r, size_t s) {
		auto l = impl::mvn::indexMrs(r, s);
		this->_evaluateUpdate(calcLLRatioMrs(k, r, s, l), _Mrs[k], l);
	};

	void _simulateUnderPrior(Storage *Data) override {
		assert(Data->dimensions()[1] == (size_t)_dim.D); // number of columns of data must match D

		// create re-usable MVN objects
		std::vector<coretools::probdist::TMultivariateNormalDistr<std::vector<double>>> MVNs(NumComponents);

		for (size_t k = 0; k < NumComponents; k++) {
			MVNs[k] = impl::mvn::createMVNForSimulation(_dim.D, _mus[k], _Mrr[k], _Mrs[k], _m[k]);
		}

		for (size_t n = 0; n < Data->dimensions()[0]; n++) { // go over all rows
			coretools::TRange row = Data->get1DSlice(1, {n, 0});
			const auto k          = _z->value(n);
			impl::mvn::simulateMultivariateNormal(*Data, row.begin, MVNs[k]);
		}
	};

	template<typename T> auto _decodeIndex(const T &Data, size_t Index) const {
		const auto coord = Data.getSubscripts(Index);
		const auto n     = coord[0];
		const auto d     = coord[1];
		const auto k     = _z->value(n);
		return std::make_tuple(n, d, k);
	}

public:
	TMultivariateNormalMixedModelInferred(TypeParamZ *Z, std::array<TypeParamMus *, NumComponents> Mus,
										  std::array<TypeParamM *, NumComponents> M,
										  std::array<TypeParamMrr *, NumComponents> Mrr,
										  std::array<TypeParamMrs *, NumComponents> Mrs)
		: _z(Z), _mus(Mus), _m(M), _Mrr(Mrr), _Mrs(Mrs) {
		this->addPriorParameter(_z);
		this->addPriorParameter(_mus);
		this->addPriorParameter(_m);
		this->addPriorParameter(_Mrr);
		this->addPriorParameter(_Mrs);
	};
	~TMultivariateNormalMixedModelInferred() override = default;

	[[nodiscard]] std::string name() const override { return "multivariateNormal_mixedModel"; };

	void initialize() override {
		// derive D from dimensionality of parameters below
		_dim = impl::mvn::calculateAndSetD(this->_storageBelow);

		auto dimNameD       = this->_storageBelow[0]->getDimensionName(1);
		const auto dimNameZ = impl::mvn_mix::getDimensionNameForZ(this->_storageBelow);

		for (auto &mu : _mus) { mu->initStorage(this, {_dim.D}, {dimNameD}); }
		for (auto &m : _m) { m->initStorage(this); }
		for (auto &mrr : _Mrr) {
			mrr->initStorage(this, {_dim.D}, {dimNameD});
			// check: if D=1 (univariate normal distribution), we will only update m and mu, and fix mrr to 1.
			if (_dim.D == 1) { mrr->set(0, 1.); }
		}

		for (auto &mrs : _Mrs) {
			if (_dim.D > 1) { // only check if there are >1 dimension (mrs are not defined for D=1)
				auto dimNamesMrs = impl::mvn::generateDimNamesMrs(dimNameD);
				mrs->initStorage(this, {impl::mvn::numberOfElementsInTriangularMatrix_Diagonal0(_dim.D)},
								 {dimNamesMrs});
			} else {
				auto dimNamesMrs = std::make_shared<coretools::TNamesStrings>(0);
				mrs->initStorage(this, {0}, {dimNamesMrs});
			}
		}

		_z->initStorage(this, {this->_storageBelow[0]->dimensions()[0]}, {dimNameZ});
	};

	double getDensity(const Storage &Data, size_t Index) const override {
		auto [n, d, k] = _decodeIndex(Data, Index);
		return impl::mvn::calcPriorDensity(Data, n, _mus[k], _m[k], _Mrr[k], _Mrs[k], _dim);
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		auto [n, d, k] = _decodeIndex(Data, Index);
		return impl::mvn::calcLogPriorDensity(Data, n, _mus[k], _m[k], _Mrr[k], _Mrs[k], _dim);
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		auto [n, d, k] = _decodeIndex(Data, Index);
		return impl::mvn::calcLogPriorRatio(Data, n, d, _mus[k], _m[k], _Mrr[k], _Mrs[k]);
	};

	double getSumLogPriorDensity(const Storage &Data) const override {
		double sum = 0.;
		for (size_t n = 0; n < Data.dimensions()[0]; n++) {
			coretools::TRange row = Data.get1DSlice(1, {n, 0});
			sum += getLogDensity(Data, row.begin);
		}
		return sum;
	};

	void update() override {
		// update mus
		for (size_t k = 0; k < NumComponents; k++) {
			while (_mus[k]->update()) {
				auto coord = _mus[k]->getUpdatedCoordinates();
				_updateMu(k, coord[0]);
			}
			// update m
			if (_m[k]->update()) { _updateM(k); }

			// update Mrr and Mrs (only if >1 dimensions to avoid overfitting)
			if (_dim.D > 1) {
				while (_Mrr[k]->update()) {
					auto coord = _Mrr[k]->getUpdatedCoordinates();
					_updateMrr(k, coord[0]);
				}
				while (_Mrs[k]->update()) {
					auto coord  = _Mrs[k]->getUpdatedCoordinates();
					auto [r, s] = impl::mvn::getCoordinatesFromLinearIndex_Mrs(coord[0]);
					_updateMrs(k, r, s);
				}
			}
		}
		// sample z
		while (_z->pickIndexToUpdate()) {
			auto coord = _z->getUpdatedCoordinates();
			_sampleZ(coord[0]);
		}
	};

	auto calcLLRatioMu(size_t k, size_t r) const {
		const auto sumM    = impl::mvn::calcSumM_dr_squared(_Mrr[k]->value(r), r, _Mrs[k]);
		size_t min_r_DMin1 = std::min(r + 1, (size_t)_dim.D - 1);

		auto f = [this, r, k, sumM, min_r_DMin1](Storage *Data) {
			size_t N        = Data->dimensions()[0];
			double sum      = 0.;
			double sumParam = 0.;
			size_t counter  = 0;
			for (size_t n = 0; n < N; n++) {
				if (_z->value(n) == k) {
					impl::mvn::addToSumsUpdateMu(sum, sumParam, Data, n, r, min_r_DMin1, _mus[k], _Mrr[k], _Mrs[k]);
					counter++;
				}
			}
			return impl::mvn::calcLLRatioUpdateMu(_mus[k]->oldValue(r), _mus[k]->value(r), sumM, counter, sumParam, sum,
												  _m[k]);
		};
		return f;
	}

	auto calcLLRatioM(size_t k) const {
		auto f = [this, k](Storage *Data) {
			size_t counter = 0;
			double sum     = 0.;
			for (size_t n = 0; n < Data->dimensions()[0]; n++) {
				if (_z->value(n) == k) {
					sum += impl::mvn::calcDoubleSum(*Data, n, _mus[k], _Mrr[k], _Mrs[k]);
					counter++;
				}
			}
			return impl::mvn::calcLLRatioUpdateM(counter, _dim.D, _m[k]->oldValue(), _m[k]->value(), sum);
		};
		return f;
	}

	auto calcLLRatioMrr(size_t k, size_t r) const {
		auto f = [this, k, r](Storage *Data) {
			size_t N       = Data->dimensions()[0];
			size_t counter = 0;
			double sum1    = 0.;
			double sum2    = 0.;
			for (size_t n = 0; n < N; n++) {
				if (_z->value(n) == k) {
					sum1 += impl::mvn::calcDoubleSum_updateMrr(*Data, n, r, sum2, _mus[k], _Mrs[k]);
					counter++;
				}
			}
			return impl::mvn::calcLLRatioUpdateMrr(counter, _Mrr[k]->oldValue(r), _Mrr[k]->value(r), sum1, sum2, _m[k]);
		};
		return f;
	}

	auto calcLLRatioMrs(size_t k, size_t r, size_t s, size_t l) {
		auto f = [this, k, r, s, l](Storage *Data) {
			size_t N    = Data->dimensions()[0];
			double sum1 = 0.;
			double sum2 = 0.;
			for (size_t n = 0; n < N; n++) {
				if (_z->value(n) == k) {
					sum1 += impl::mvn::calcDoubleSum_updateMrs(*Data, n, r, s, sum2, _mus[k], _Mrr[k], _Mrs[k]);
				}
			}

			return impl::mvn::calcLLRatioUpdateMrs(_Mrs[k]->oldValue(l), _Mrs[k]->value(l), sum1, sum2, _m[k]);
		};
		return f;
	}

	coretools::Positive calcLikelihoodSampleZ(size_t i, const typename SpecZ::value_type &Z) const {
		// compute likelihood
		coretools::Positive likelihood = 1.;
		for (const auto &storage : this->_storageBelow) {
			likelihood *= impl::mvn::calcPriorDensity(*storage, i, _mus[Z], _m[Z], _Mrr[Z], _Mrs[Z], _dim);

			if (!likelihood.isInsideInterval()) {
				DEVERROR("likelihood (", likelihood,
						 ") is not a probability. Probably, an underflow has happened. Should use log-sum-exp trick to "
						 "prevent this!");
			}
		}
		return likelihood;
	};

	void guessInitialValues() override {
		for (size_t k = 0; k < NumComponents; k++) {
			impl::mvn_mix::checkForFixedInitialValues(_m[k], _Mrr[k], _Mrs[k], name());
		}

		if (!_z->hasFixedInitialValue()) { // latent variable not known -> run EM
			_z->runEMEstimation(*this);
		} else {
			setInitialMeanAndM();
		}
	};

	void initializeEMParameters() override {
		setInitialZ();

		// calculate mean and M according to this classification
		setInitialMeanAndM();
	};

	void setInitialZ() {
		// calculate the mean value per entry (over all parameters)
		auto meanParameterValues = _calculateMeanParameterValues();

		// find points that are most far apart
		auto minValues = _findMostDistantPoints(meanParameterValues);

		// go over the data again and assign the closest group to each point
		_setInitialZ(minValues, meanParameterValues);
	};

	void setInitialMeanAndM() {
		_setMusToMLE();
		_setMToMLE();
	};

	[[nodiscard]] size_t getMLEmissionState(size_t Index, size_t) const override { return _z->value(Index); };

	void calculateEmissionProbabilities(size_t Index, TDataVector<double, size_t> &Emission) const override {
		// initialize emissions with zero
		for (size_t k = 0; k < NumComponents; k++) { Emission[k] = 0.; }

		// go over all parameters and over all components and sum emission probability
		// we want emission not in log, but we will use the log to sum and then de-log in the end
		for (const auto &storage : this->_storageBelow) {
			// log P(x | z = k, theta)
			for (size_t k = 0; k < NumComponents; k++) {
				Emission[k] += impl::mvn::calcLogPriorDensity(*storage, Index, _mus[k], _m[k], _Mrr[k], _Mrs[k], _dim);
			}
		}

		// get rid of log
		for (size_t k = 0; k < NumComponents; k++) { Emission[k] = exp(Emission[k]); }
	};

	void prepareEMParameterEstimationInitial() override {
		// initialize data: per component, we need to store three sums
		// 1) sum_{i=1}^N weights_ki * sum_{p=1}^P x_pi
		// 2) sum_{i=1}^N weights_ki * sum_{p=1}^P (x_pi - mu_k)(x_pi - mu_k)^T
		// 3) sum_{i=1}^N weights_ki
		_EM_update_Mus_temp.resize(NumComponents * _dim.D);
		_EM_update_Sigma_temp.resize(NumComponents, arma::mat(_dim.D, _dim.D, arma::fill::zeros));
		_EM_update_Weights_temp.resize(NumComponents);
	};

	void prepareEMParameterEstimationOneIteration() override {
		// fill data with zeros
		for (size_t i = 0; i < NumComponents * _dim.D; i++) { _EM_update_Mus_temp[i] = 0.; }
		for (size_t i = 0; i < NumComponents; i++) {
			_EM_update_Sigma_temp[i].zeros();
			_EM_update_Weights_temp[i] = 0.;
		}
	};

	void handleEMParameterEstimationOneIteration(size_t Index, const TDataVector<double, size_t> &Weights) override {
		// calculate
		// 1) sum_{p=1}^P sum_{i=1}^N x_pi
		// 2) sum_{p=1}^P sum_{i=1}^N (x_pi - mu_k)(x_pi - mu_k)^T

		// initialize data for sums
		std::vector<double> sumXpi(_dim.D, 0.);
		std::vector<arma::mat> sumXpiMinusMuSquare(
			NumComponents); // 3 dimensions: K times an Armadillo matrix with D times D dimensions
		for (size_t k = 0; k < NumComponents; k++) {
			sumXpiMinusMuSquare[k] = arma::mat(_dim.D, _dim.D, arma::fill::zeros);
		}

		for (const auto &storage : this->_storageBelow) {
			coretools::TRange row = storage->get1DSlice(1, {Index, 0}); // one row of param
			for (size_t d = 0; d < _dim.D; d++) {
				sumXpi[d] += (Type)(*storage)[row.begin + d * row.increment];
				for (size_t k = 0; k < NumComponents; k++) {
					for (size_t e = 0; e < _dim.D; e++) {
						sumXpiMinusMuSquare[k](d, e) +=
							((Type)(*storage)[row.begin + d * row.increment] - _mus[k]->value(d)) *
							((Type)(*storage)[row.begin + e * row.increment] - _mus[k]->value(e));
					}
				}
			}
		}

		// add to sums
		for (size_t k = 0; k < NumComponents; k++) {
			// add to sum x_pi * weights_ki
			for (size_t d = 0; d < _dim.D; d++) { _EM_update_Mus_temp[k * _dim.D + d] += sumXpi[d] * Weights[k]; }
			// add to sum (x_pi - mu_k)(x_pi - mu_k)^T * weights_ki
			impl::mvn_mix::handleEMMaximizationOneIteration_updateSigma(k, _dim.D, sumXpiMinusMuSquare, Weights,
																		_EM_update_Sigma_temp[k]);
			// add to sum weights
			_EM_update_Weights_temp[k] += Weights[k];
		}
	};

	void finalizeEMParameterEstimationOneIteration() override {
		// M-step: update parameters
		for (size_t k = 0; k < NumComponents; k++) {
			// mus
			for (size_t d = 0; d < _dim.D; d++) {
				double value =
					_EM_update_Mus_temp[k * _dim.D + d] / (this->_storageBelow.size() * _EM_update_Weights_temp[k]);
				_mus[k]->set(d, value);
			}

			// Sigma
			// first calculate EM update
			arma::mat Sigma;
			impl::mvn_mix::updateEMParametersOneIteration_Sigma(Sigma, _dim.D, this->_storageBelow.size(),
																_EM_update_Sigma_temp[k], _EM_update_Weights_temp[k]);
			// decompose Sigma and set parameters of M
			impl::mvn::fillParametersMFromSigma(Sigma, _dim.D, _Mrr[k], _Mrs[k], _m[k]);
		}
	};

	void finalizeEMParameterEstimationFinal() override {
		// clean memory
		_EM_update_Mus_temp.clear();
		_EM_update_Sigma_temp.clear();
		_EM_update_Weights_temp.clear();
	};

	void handleStatePosteriorEstimation(size_t Index,
										const TDataVector<double, size_t> &StatePosteriorProbabilities) override {
		_z->set(Index, StatePosteriorProbabilities.maxIndex());
	};
};
*/

} // end namespace stattools::prior

#endif // TPRIORMULTIVARIATENORMALMIXEDMODEL_H
