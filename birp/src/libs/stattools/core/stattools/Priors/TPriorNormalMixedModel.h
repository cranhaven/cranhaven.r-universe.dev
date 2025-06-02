//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORNORMALMIXEDMODEL_H
#define TPRIORNORMALMIXEDMODEL_H

#include <cstddef>
namespace stattools::prior {

//-------------------------------------------
// TNormalMixedModelInferred
//-------------------------------------------

namespace impl {
template<typename T> auto checkDimensionality(T StorageBelow) {
	// derive N and dimNames from dimensionality of storage below
	auto firstStorage = StorageBelow[0];
	size_t N          = firstStorage->size();
	auto dimNames     = firstStorage->getDimensionName(0);

	for (const auto &storage : StorageBelow) {
		// check if N is the same
		if (storage->size() != N) {
			DEVERROR("Total size of storage (=", storage->size(), ") and of other storage (=", N, ") do not match!");
		}
	}
	return dimNames;
};
} // namespace impl

/*
template<typename Derived, typename Type, size_t NumDim, typename SpecZ, typename SpecMus, typename SpecVars,
		 size_t NumComponents>
class TNormalMixedModelInferred : public TStochasticBase<Derived, Type, NumDim>, public TLatentVariable<double, size_t, size_t> {
	static_assert((TypesAreUnsignedIntWithVariableMax<typename SpecZ::value_type>() ||
				   TypesAreBool<typename SpecZ::value_type>()) &&
				  TypesAreUnboundedFloatingPoints<Type, typename SpecMus::value_type>() &&
				  TypesAreStrictlyPositiveFloatingPoints<typename SpecVars::value_type>());

private:
	using BoxType = TNormalMixedModelInferred<Derived, Type, NumDim, SpecZ, SpecMus, SpecVars, NumComponents>;
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;
	using TypeParamZ    = TParameter<SpecZ, BoxType>;
	using TypeParamMus  = TParameter<SpecMus, BoxType>;
	using TypeParamVars = TParameter<SpecVars, BoxType>;

	TypeParamZ *_z = nullptr;
	std::array<TypeParamMus *, NumComponents> _mus;
	std::array<TypeParamVars *, NumComponents> _vars;

	double *_EM_update_temp                   = nullptr;
	static constexpr double _oneDivTwo_log2Pi = 0.91893853320467266954;

protected:
	double _dist(const Type &x, const Type &y) const {
		// calculate Euclidean distance
		return (std::fabs(x - y));
	};

	[[nodiscard]] std::vector<double> _calculateMeanStorageValues() const {
		std::vector<double> vals(_z->size(), 0.);
		for (const auto &storage : this->_storageBelow) {
			for (size_t i = 0; i < storage->size(); ++i) { vals[i] += (Type)(*storage)[i]; }
		}

		for (auto &it : vals) { it /= this->_storageBelow.size(); }
		return vals;
	};

	std::vector<Type> _findMostDistantPoints(const std::vector<double> &MeanParameterValues) const {
		std::vector<Type> minValues(NumComponents, 0);
		std::vector<double> minDistances(_z->size());

		if (_mus[0]->hasFixedInitialValue()) {
			minValues[0] = minValues[0];
		} else {
			// 1st value = 1st group
			minValues[0] = MeanParameterValues[0];
		}

		for (size_t k = 1; k < NumComponents; k++) {
			for (size_t i = 0; i < _z->size(); i++) {
				// find minimal distance among all previous k
				double minDist = _dist(MeanParameterValues[i], minValues[0]);
				for (size_t smallerK = 1; smallerK < k; smallerK++) {
					double tmp = _dist(MeanParameterValues[i], minValues[smallerK]);
					if (tmp < minDist) { minDist = tmp; }
				}
				minDistances[i] = minDist;
			}

			// assign the data point that maximizes minDistances to the next K
			size_t maxIndex =
				std::distance(minDistances.begin(), std::max_element(minDistances.begin(), minDistances.end()));
			if (_mus[k]->hasFixedInitialValue()) {
				minValues[k] = minValues[k];
			} else {
				minValues[k] = MeanParameterValues[maxIndex];
			}
		}
		return minValues;
	};

	void _setInitialZ(const std::vector<Type> &MinValues, const std::vector<double> &MeanParameterValues) {
		// go over the data again and assign the closest group to each point
		for (size_t i = 0; i < _z->size(); i++) {
			size_t minK = 0;
			for (size_t k = 1; k < NumComponents; k++) {
				if (_dist(MeanParameterValues[i], MinValues[k]) < _dist(MeanParameterValues[i], MinValues[minK])) {
					minK = k;
				}
			}
			_z->set(i, minK);
		}
	};

	void _setInitialMeanAndVar() {
		std::vector<coretools::TMeanVar<double>> meanVars(NumComponents);
		for (const auto &storage : this->_storageBelow) {
			for (size_t i = 0; i < storage->size(); ++i) {
				size_t k = _z->value(i);
				meanVars[k].add((Type)(*storage)[i]);
			}
		}

		for (size_t k = 0; k < NumComponents; k++) {
			_mus[k]->set(meanVars[k].mean());
			double var = std::max(meanVars[k].variance(), 10e-10);
			_vars[k]->set(var);
		}
	};

	void _updateMean(size_t k) { this->_evaluateUpdate(calcLLRatioMean(k), _mus[k]); }
	void _updateVar(size_t k) { this->_evaluateUpdate(calcLLRatioVar(k), _vars[k]); }

	std::array<double, NumComponents> _calculatePosterior_sampleZ(size_t i) {
		std::array<double, NumComponents> logDens;
		for (size_t z = 0; z < NumComponents; z++) {
			_z->set(i, z);
			logDens[z] = _z->getLogDensity(i);
			for (const auto &storage : this->_storageBelow) { logDens[z] += getLogDensity(*storage, i); }
		}
		return logDens;
	};

	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) {
			size_t k = _z->value(i);
			(*Data)[i] =
				coretools::instances::randomGenerator().getNormalRandom(_mus[k]->value(), sqrt(_vars[k]->value()));
		}
	};

public:
	TNormalMixedModelInferred(TypeParamZ *Z, std::array<TypeParamMus *, NumComponents> Mus,
							  std::array<TypeParamVars *, NumComponents> Vars)
		: _z(Z), _mus(Mus), _vars(Vars) {

		this->addPriorParameter(_z);
		this->addPriorParameter(_mus);
		this->addPriorParameter(_vars);
	};
	~TNormalMixedModelInferred() override = default;

	std::string name() const override { return "normalMixedModel"; };

	void initialize() override {
		auto dimNames = impl::checkDimensionality(this->_storageBelow);
		auto N        = this->_storageBelow[0]->size();

		// go over all parameters and set size
		for (auto &mu : _mus) { mu->initStorage(this); }
		for (auto &var : _vars) { var->initStorage(this); }

		// check if maximum of z matches K
		if (_z->max() != NumComponents - 1) {
			DEVERROR("NumComponents-1 (", NumComponents - 1, ") differs from maximum value for z (", _z->max(), ").");
		}
		_z->initStorage(this, {N}, {dimNames});
	};

	double getDensity(const Storage &Data, size_t Index) const override {
		const auto x = (Type)Data[Index];
		const auto k = _z->value(Index);
		return coretools::probdist::TNormalDistr::density(x, _mus[k]->value(), sqrt(_vars[k]->value()));
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		const auto x     = (Type)Data[Index];
		const auto k     = _z->value(Index);
		const double tmp = x - _mus[k]->value();
		return -0.5 * log(_vars[k]->value()) - _oneDivTwo_log2Pi - (tmp * tmp) / (2. * _vars[k]->value());
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		size_t k       = _z->value(Index);
		double tmp     = (Type)Data[Index] - _mus[k]->value();
		double tmp_old = Data[Index].oldValue() - _mus[k]->value();
		return (tmp_old * tmp_old - tmp * tmp) / (2. * _vars[k]->value());
	};

	auto calcLLRatioMean(size_t k) const {
		const auto twoVar = 2. * _vars[k]->value();

		auto f = [mu = _mus[k]->value(), oldMu = _mus[k]->oldValue(), twoVar, k, z = _z](Storage *Data) {
			double sum = 0.0;
			for (size_t i = 0; i < Data->size(); ++i) {
				if (z->value(i) == k) {
					double tmpOld = (Type)(*Data)[i] - oldMu;
					double tmpNew = (Type)(*Data)[i] - mu;
					sum += tmpOld * tmpOld - tmpNew * tmpNew;
				}
			}
			return sum / twoVar;
		};
		return f;
	}

	auto calcLLRatioVar(size_t k) const {
		const auto v1 = log(_vars[k]->oldValue() / _vars[k]->value());
		const auto v2 = 1. / _vars[k]->oldValue() - 1. / _vars[k]->value();

		auto f = [mu = _mus[k]->value(), v1, v2, k, z = _z](Storage *Data) {
			double sum     = 0.;
			size_t counter = 0;
			for (size_t i = 0; i < Data->size(); ++i) {
				if (z->value(i) == k) {
					double tmp = (Type)(*Data)[i] - mu;
					sum += tmp * tmp;
					counter++;
				}
			}
			return 0.5 * (counter * v1 + v2 * sum);
		};
		return f;
	}

	void sampleZ(size_t i) { _z->sampleDiscreteFromLogPosteriorProbabilities(i, _calculatePosterior_sampleZ(i)); };

	void update() override {
		for (size_t k = 0; k < NumComponents; k++) {
			if (_mus[k]->update()) { _updateMean(k); }
			if (_vars[k]->update()) { _updateVar(k); }
		}

		while (_z->pickIndexToUpdate()) {
			auto coord = _z->getUpdatedCoordinates();
			sampleZ(coord[0]);
		}
	};

	void guessInitialValues() override {
		if (!_z->hasFixedInitialValue()) { // latent variable not known -> run EM
			_z->runEMEstimation(*this);
		} else {
			_setInitialMeanAndVar();
		}
	};

	void initializeEMParameters() override {
		// calculate the mean value per entry (over all storages below)
		auto meanParameterValues = _calculateMeanStorageValues();

		// find points that are most far apart
		auto minValues = _findMostDistantPoints(meanParameterValues);

		// go over the data again and assign the closest group to each point
		_setInitialZ(minValues, meanParameterValues);

		// calculate mean and variance according to this classification
		_setInitialMeanAndVar();
	};

	size_t getMLEmissionState(size_t Index, size_t) const override { return _z->value(Index); };

	void calculateEmissionProbabilities(size_t Index, TDataVector<double, size_t> &Emission) const override {
		// initialize emissions with zero
		for (size_t k = 0; k < NumComponents; k++) { Emission[k] = 0.; }

		// go over all storages and over all components and sum emission probability
		// we want emission not in log, but we will use the log to sum and then de-log in the end
		for (const auto &storage : this->_storageBelow) {
			// log P(x | z = k, theta)
			for (size_t k = 0; k < NumComponents; k++) {
				Emission[k] += coretools::probdist::TNormalDistr::logDensity((Type)(*storage)[Index], _mus[k]->value(),
																			 sqrt(_vars[k]->value()));
			}
		}

		// get rid of log
		for (size_t k = 0; k < NumComponents; k++) { Emission[k] = exp(Emission[k]); }
	};

	void prepareEMParameterEstimationInitial() override {
		// initialize data: per component, we need to store three sums
		// 1) sum_{i=1}^N weights_ki * sum_{p=1}^P x_pi
		// 2) sum_{i=1}^N weights_ki * sum_{p=1}^P (x_pi - mu_k)^2
		// 3) sum_{i=1}^N weights_ki
		_EM_update_temp = new double[3 * NumComponents];
	};

	void prepareEMParameterEstimationOneIteration() override {
		// fill data with zeros
		for (size_t i = 0; i < 3 * NumComponents; i++) { _EM_update_temp[i] = 0.; }
	};

	void handleEMParameterEstimationOneIteration(size_t Index, const TDataVector<double, size_t> &Weights) override {
		// calculate
		// 1) sum_{i=1}^N sum_{p=1}^P x_pi
		// 2) sum_{i=1}^N sum_{p=1}^P (x_pi - mu_k)^2
		double sumXpi = 0.;
		std::vector<double> sumXpiMinusMuSquare(NumComponents, 0.);
		for (const auto &storage : this->_storageBelow) {
			sumXpi += (Type)(*storage)[Index];
			for (size_t k = 0; k < NumComponents; k++) {
				double tmp = (Type)(*storage)[Index] - _mus[k]->value();
				sumXpiMinusMuSquare[k] += tmp * tmp;
			}
		}

		// add to sums
		for (size_t k = 0; k < NumComponents; k++) {
			// add to sum x_i * weights_ki
			_EM_update_temp[k * 3] += sumXpi * Weights[k];
			// add to sum (x_i - mu_k)^2 * weights_ki
			_EM_update_temp[k * 3 + 1] += sumXpiMinusMuSquare[k] * Weights[k];
			// add to sum weights
			_EM_update_temp[k * 3 + 2] += Weights[k];
		}
	};

	void finalizeEMParameterEstimationOneIteration() override {
		// M-step: update parameters
		for (size_t k = 0; k < NumComponents; k++) {

			_mus[k]->set(_EM_update_temp[k * 3] / (this->_storageBelow.size() * _EM_update_temp[k * 3 + 2]));

			double newValue = _EM_update_temp[k * 3 + 1] / (this->_storageBelow.size() * _EM_update_temp[k * 3 + 2]);
			if (newValue == 0.) {
				newValue = 0.0000001; // avoid var = 0
			}
			_vars[k]->set(newValue);
		}
	};

	void finalizeEMParameterEstimationFinal() override {
		// clean memory
		delete[] _EM_update_temp;
	};

	void handleStatePosteriorEstimation(size_t Index,
										const TDataVector<double, size_t> &StatePosteriorProbabilities) override {
		_z->set(Index, StatePosteriorProbabilities.maxIndex());
	};
};
*/

} // end namespace stattools::prior

#endif // TPRIORNORMALMIXEDMODEL_H
