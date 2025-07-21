//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORNORMALTWOMIXEDMODELSSTRETCH_H
#define TPRIORNORMALTWOMIXEDMODELSSTRETCH_H

#include "coretools/Distributions/TNormalDistr.h"
#include "stattools/Priors/TPriorNormalMixedModel.h"
#include "stattools/Priors/TPriorBase.h"
#include "stattools/ParametersObservations/TParameter.h"

namespace stattools::prior {

//-------------------------------------------
// TTwoNormalsMixedModelInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecMu, typename SpecVar0, typename SpecVar1,
		 typename SpecZ>
class TTwoNormalsMixedModelInferred : public TStochasticBase<Derived, Type, NumDim>,
									  public TLatentVariable<double, size_t, size_t> {
	static_assert(TypesAreBool<typename SpecZ::value_type>() &&
				  TypesAreUnboundedFloatingPoints<Type, typename SpecMu::value_type>() &&
				  TypesAreStrictlyPositiveFloatingPoints<typename SpecVar0::value_type>() &&
				  TypesAreStrictlyPositiveFloatingPoints<typename SpecVar1::value_type>());

private:
	// Note: written in a way that z makes distinction of zero/non-zero; i.e. there is a variance for the zero-model
	//       and one for the non-zero model. In most applications, TypeZ will be bool; but it can also be Unsigned
	//       int
	using BoxType = TTwoNormalsMixedModelInferred<Derived, Type, NumDim, SpecMu, SpecVar0, SpecVar1, SpecZ>;
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;
	using TypeParamZ    = TParameter<SpecZ, BoxType>;
	using TypeParamMu   = TParameter<SpecMu, BoxType>;
	using TypeParamVar0 = TParameter<SpecVar0, BoxType>;
	using TypeParamVar1 = TParameter<SpecVar1, BoxType>;

	// temporary values (stored for speed)
	static constexpr double _oneDivTwo_log2Pi = 0.91893853320467266954;
	double _variance1                         = 0.0;

	TypeParamZ *_z       = nullptr;
	TypeParamMu *_mu     = nullptr;
	TypeParamVar0 *_var0 = nullptr;
	TypeParamVar1 *_var1 = nullptr;
	bool _manageZ        = true;

	double *_EM_update_temp = nullptr;

protected:
	[[nodiscard]] double _getVarForThisK(size_t k) const { return (k == 0) ? (double)_var0->value() : _variance1; };

	[[nodiscard]] double _getVar1() const { return _var0->value() + _var1->value(); };

	void _updateTempVals() { _variance1 = _getVar1(); };

	std::array<double, 2> _calculateLogPosterior_sampleZ(size_t i) {
		std::array<double, 2> logDens;
		for (size_t z = 0; z < 2; z++) {
			_z->set(i, z);
			logDens[z] = _z->getLogDensity(i);
			for (const auto &storage : this->_storageBelow) { logDens[z] += _getLogDensity((Type)(*storage)[i], z); }
		}
		return logDens;
	};

	double _getLogDensity(Type x, size_t k) const {
		const double var = _getVarForThisK(k);
		const double tmp = x - _mu->value();
		return -0.5 * log(var) - _oneDivTwo_log2Pi - (tmp * tmp) / (2. * var);
	};

	double _setMean_ReturnVariance() {
		// set mu to MLE (shared by all x)
		auto [mean, var] = impl::calculateMeanVarOfStorages<Storage *, Type>(this->_storageBelow);

		_mu->set(mean);
		// return overall MLE variance
		return var;
	};

	void _initializeEMParams_basedOnCutoff(double var) {
		// before EM: initialize EM parameters
		// this is the first round: go over all data, calculate log density under overall mean and variance
		// and assign a certain percentage of x that have the highest log density to model 0 and the rest to model 1

		// calculate log density
		std::vector<double> logDens(_z->size(), 0.);
		for (const auto &storage : this->_storageBelow) {
			for (size_t i = 0; i < storage->size(); ++i) {
				logDens[i] +=
					coretools::probdist::TNormalDistr::logDensity((Type)(*storage)[i], _mu->value(), sqrt(var));
			}
		}
		// normalize
		for (auto &it : logDens) { it /= this->_storageBelow.size(); }

		// define cutoff: 10% of all x will go into model 1 and 90% into model 0
		double cutoffPercentage           = 0.1;
		// sort densities and define cutoff value
		std::vector<double> sortedLogDens = logDens;
		std::sort(sortedLogDens.begin(), sortedLogDens.end());
		double cutoffValue = sortedLogDens[_z->size() * cutoffPercentage];

		// define components
		if (_manageZ) {
			// go over the data again and assign components
			for (size_t i = 0; i < _z->size(); i++) {
				if (logDens[i] >= cutoffValue) { // z = 0
					_z->set(i, 0);
				} else { // z = 1
					_z->set(i, 1);
				}
			}
		}
		// re-calculate variances for these components
		_setVar();
	};

	void _initializeEMParams_refinement() {
		// before EM: initialize EM parameters
		// this is the second round: go over all data again, calculate log density under overall mean and the two
		// variances we've calculated in the first round assign x that have a higher log density under the 0-model
		// to z=0 and the rest to z=1
		if (_manageZ) {
			for (size_t i = 0; i < _z->size(); ++i) {
				size_t MLE_state = TLatentVariable<double, size_t, size_t>::getMLEmissionState(i, 2);
				_z->set(i, MLE_state);
			}
		}

		// re-calculate variances for these components
		_setVar();
	};

	void _setMeanAndVar() {
		_setMean_ReturnVariance();
		_setVar();
	};

	void _setVar() {
		// set var0 and var1 to MLE
		std::vector<coretools::TMeanVar<double>> meanVars(2);
		for (const auto &storage : this->_storageBelow) {
			for (size_t i = 0; i < storage->size(); ++i) {
				if (_z->value(i) == 0) {
					meanVars[0].add((Type)(*storage)[i]);
				} else {
					meanVars[1].add((Type)(*storage)[i]);
				}
			}
		}

		// var0
		double var0 = meanVars[0].variance();
		var0        = std::max(0.001, var0);
		_var0->set(var0);

		// var1
		if (!_var0->hasFixedInitialValue() || !_var1->hasFixedInitialValue()) {
			_variance1 = std::max(meanVars[1].variance(), 2.0 * _var0->value()); // make sure it is never 0
		}
	};

	void _switchEMLabels() {
		if (!_z->hasFixedInitialValue() && _manageZ) {
			for (size_t i = 0; i < _z->size(); i++) {
				_z->set(i, !_z->value(i)); // set to opposite
			}
			// switch prior on z
			_z->switchPriorClassificationAfterEM();
		}
	};

	void _simulateUnderPrior(Storage *Data) override {
		// first update temporary values: might have changed in the meantime
		_updateTempVals();
		for (size_t i = 0; i < Data->size(); ++i) {
			const size_t k   = _z->value(i);
			const double var = _getVarForThisK(k);
			(*Data)[i]       = coretools::instances::randomGenerator().getNormalRandom(_mu->value(), sqrt(var));
		}
	};

public:
	TTwoNormalsMixedModelInferred(TypeParamZ *Z, TypeParamMu *Mu, TypeParamVar0 *Var0, TypeParamVar1 *Var1,
								  bool ManageZ = true)
		: _z(Z), _mu(Mu), _var0(Var0), _var1(Var1), _manageZ(ManageZ) {
		this->addPriorParameter({_z, _mu, _var0, _var1});
	};
	~TTwoNormalsMixedModelInferred() override = default;

	std::string name() const override { return "normal_mixedModel_stretch"; };

	void initialize() override {
		auto dimNames            = impl::checkDimensionality(this->_storageBelow);
		const auto &firstStorage = this->_storageBelow[0];

		// go over all parameters and set size
		_mu->initStorage(this);
		_var0->initStorage(this);
		_var1->initStorage(this);
		if (_manageZ) { _z->initStorage(this, firstStorage->dimensions(), firstStorage->getDimensionNames()); }

		_updateTempVals();
	};

	double getDensity(const Storage &Data, size_t Index) const override {
		const auto x = (Type)Data[Index];
		const auto k = _z->value(Index);
		return coretools::probdist::TNormalDistr::density(x, _mu->value(), sqrt(_getVarForThisK(k)));
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		return _getLogDensity((Type)Data[Index], _z->value(Index));
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		double var     = _getVarForThisK(_z->value(Index));
		double tmp     = (Type)Data[Index] - _mu->value();
		double tmp_old = Data[Index].oldValue() - _mu->value();
		return (tmp_old * tmp_old - tmp * tmp) / (2. * var);
	};

	auto variance1() const { return _variance1; };

	[[nodiscard]] auto calculateLLRatio(TypeParamMu *, size_t) const {
		const auto twoVar0 = 2. * _var0->value();
		const auto twoVar1 = 2. * _variance1;

		auto f = [mu = _mu->value(), oldMu = _mu->oldValue(), z = _z, twoVar0, twoVar1](Storage *Data) {
			std::array<double, 2> sums = {0.0, 0.0};
			for (size_t i = 0; i < Data->size(); ++i) {
				const double tmpOld = (Type)(*Data)[i] - oldMu;
				const double tmpNew = (Type)(*Data)[i] - mu;
				const double res    = tmpOld * tmpOld - tmpNew * tmpNew;
				(z->value(i) == 0) ? sums[0] += res : sums[1] += res;
			}
			return sums[0] / twoVar0 + sums[1] / twoVar1;
		};
		return f;
	}

	void updateTempVals(TypeParamMu *, size_t, bool) { /* empty - there are no tmp values*/ }

	[[nodiscard]] auto calculateLLRatio(TypeParamVar0 *, size_t) const {
		const auto var1_new = _getVar1();
		const auto v0       = log(_var0->oldValue() / _var0->value());
		const auto v1       = log(_variance1 / var1_new);
		const auto w0       = 1. / _var0->oldValue() - 1. / _var0->value();
		const auto w1       = (1. / _variance1 - 1. / var1_new);

		auto f = [v0, v1, w0, w1, mu = _mu->value(), z = _z](Storage *Data) {
			size_t c1                  = 0;
			std::array<double, 2> sums = {0.0, 0.0};
			for (size_t i = 0; i < Data->size(); ++i) {
				double tmp    = (Type)(*Data)[i] - mu;
				double square = tmp * tmp;
				if (z->value(i) == 0) {
					sums[0] += square;
				} else {
					sums[1] += square;
					++c1;
				}
			}
			size_t c0 = z->size() - c1;
			return 0.5 * (c0 * v0 + c1 * v1 + w0 * sums[0] + w1 * sums[1]);
		};
		return f;
	}

	void updateTempVals(TypeParamVar0 *, size_t, bool Accepted) {
		if (Accepted) { _updateTempVals(); }
	}

	[[nodiscard]] auto calculateLLRatio(TypeParamVar1 *, size_t) const {
		// var1 is NOT the variance of the 1-model!
		// the variance of the 1-model is var0 + var1
		const auto var1_new = _getVar1();
		const auto v1       = log(_variance1 / var1_new);
		const auto w1       = (1. / _variance1 - 1. / var1_new);

		auto f = [v1, w1, mu = _mu->value(), z = _z](Storage *Data) {
			size_t c1   = 0;
			double sum1 = 0.;
			for (size_t i = 0; i < Data->size(); ++i) {
				if (z->value(i) != 0) {
					double tmp = (Type)(*Data)[i] - mu;
					sum1 += tmp * tmp;
					++c1;
				}
			}
			return 0.5 * (double)c1 * v1 + 0.5 * w1 * sum1;
		};
		return f;
	}

	void updateTempVals(TypeParamVar1 *, size_t, bool Accepted) {
		if (Accepted) { _updateTempVals(); }
	}

	[[nodiscard]] auto calculateLLRatio(TypeParamZ *, size_t i) const {
		auto f = [this, i](Storage *Data) {
			const double LLNew = _getLogDensity((Type)(*Data)[i], _z->value(i));
			const double LLOld = _getLogDensity((Type)(*Data)[i], _z->oldValue(i));
			return LLNew - LLOld;
		};
		return f;
	}

	void updateTempVals(TypeParamZ *, size_t, bool) { /* empty - there are no tmp values*/ }

	void doGibbs(TypeParamZ *, size_t i) {
		if (!_manageZ) return;
		_z->template sampleDiscrete<true, true>(i, _calculateLogPosterior_sampleZ(i));
	}

	void guessInitialValues() override {
		// run EM to initialize prior parameters, and pretend that the two variances are independent
		if (!_z->hasFixedInitialValue() && _manageZ) { // latent variable not known -> run EM
			_z->runEMEstimation(*this);
		} else {
			_setMeanAndVar();
		}

		// now adjust to model: set larger variance to variance of 1-model
		if (_var0->value() > _variance1) {
			if (!_var0->hasFixedInitialValue() && !_var1->hasFixedInitialValue()) {
				// both var0 and var1 are estimated
				// -> set var0 = variance1
				// -> and variance1 = var0
				// -> and calculate var1 as the difference between the two
				double tmp0 = _var0->value();
				if (_variance1 == 0.0) { _variance1 = _var0->min(); }
				_var0->set(_variance1);
				_variance1 = tmp0;
				_var1->set(_variance1 - _var0->value());
				_switchEMLabels();
			} else if (!_var1->hasFixedInitialValue()) {
				// var0 is fix, var1 is estimated
				// -> leave var0 as it is
				// -> set var1 to a very small value
				_var1->set(0.000001);
				_updateTempVals();

				// don't switch z: labels are still the same
			} else if (!_var0->hasFixedInitialValue()) {
				// var0 is estimated, var1 if fix
				// -> leave var1 as it is
				// -> set var0
				_variance1 = _var0->value();
				_var0->set(_variance1 - _var1->value());

				_switchEMLabels();
			} else {
				// both are fix -> won't be updated in EM, so var0 should never be larger than var1!
				throw coretools::TDevError("should have never gotten here! If both var0 and var1 are not updated in EM, we shouldn't "
						 "switch labels anyways!");
			}
		} else {
			// set _var1 as variance1 - sigma_0^2
			if (!_var1->hasFixedInitialValue()) {
				_var1->set(std::max(0.001, _variance1 - _var0->value()));
			}
		}

		// update temporary values
		_updateTempVals();

		// assert that condition holds
		assert(_var0->value() > 0);
		assert(_var1->value() > 0);
		assert(_variance1 > 0);
		assert(_var0->value() + _var1->value() == _variance1);
	};

	void initializeEMParameters() override {
		// calculate overall mean and variance
		double var = _setMean_ReturnVariance();

		// first round: go over all data and assign a certain percentage to z=0 and the rest to z=1 based on the log
		// density under overall mean and variance
		_initializeEMParams_basedOnCutoff(var);

		// second round: go over all data again and assign z to the model that maximizes the log density (based on
		// the variances we've initialized in the first round)
		_initializeEMParams_refinement();
	};

	size_t getMLEmissionState(size_t Index, size_t) const override { return _z->value(Index); };

	void calculateEmissionProbabilities(size_t Index, TDataVector<double, size_t> &Emission) const override {
		// initialize emissions with zero
		Emission[0] = 0.;
		Emission[1] = 0.;

		// go over all parameters and over all components and sum emission probability
		// we want emission not in log, but we will use the log to sum and then de-log in the end
		for (const auto &storage : this->_storageBelow) {
			// log P(x | z = k, theta)
			Emission[0] += _getLogDensity((Type)(*storage)[Index], 0);
			Emission[1] += _getLogDensity((Type)(*storage)[Index], 1);
		}

		// get rid of log
		Emission[0] = exp(Emission[0]);
		Emission[1] = exp(Emission[1]);
	};

	void prepareEMParameterEstimationInitial() override {
		// initialize data: per component, we need to store two sums
		// 1) sum_{i=1}^N weights_ki * sum_{p=1}^P (x_pi - mu)^2
		// 2) sum_{i=1}^N weights_ki
		_EM_update_temp = new double[4];
	};

	void prepareEMParameterEstimationOneIteration() override {
		// fill data with zeros
		for (size_t i = 0; i < 4; i++) { _EM_update_temp[i] = 0.; }
	};

	void handleEMParameterEstimationOneIteration(size_t Index, const TDataVector<double, size_t> &Weights) override {
		// calculate
		// sum_{p=1}^P sum_{i=1}^N (x_pi - mu)^2
		double sumXpiMinusMuSquare = 0.;
		for (const auto &storage : this->_storageBelow) {
			double tmp = (Type)(*storage)[Index] - _mu->value();
			sumXpiMinusMuSquare += tmp * tmp;
		}

		// add to sums
		for (size_t k = 0; k < 2; k++) {
			// add to sum (x_i - mu)^2 * weights_ki
			_EM_update_temp[k * 2] += sumXpiMinusMuSquare * Weights[k];
			// add to sum weights
			_EM_update_temp[k * 2 + 1] += Weights[k];
		}
	};

	void finalizeEMParameterEstimationOneIteration() override {
		// M-step: update parameters

		// 0-model
		double var0 = _EM_update_temp[0] / (this->_storageBelow.size() * _EM_update_temp[1]);
		_var0->set(std::max(0.001, var0));

		// 1-model
		if (!_var0->hasFixedInitialValue() || !_var1->hasFixedInitialValue()) {
			double var1 = _EM_update_temp[2] / (this->_storageBelow.size() * _EM_update_temp[3]);
			_variance1  = std::max(0.001, var1);
		}
	};

	void finalizeEMParameterEstimationFinal() override {
		// clean memory
		delete[] _EM_update_temp;
	};

	void handleStatePosteriorEstimation(size_t index,
										const TDataVector<double, size_t> &StatePosteriorProbabilities) override {
		if (_manageZ) { _z->set(index, StatePosteriorProbabilities.maxIndex()); }
	}
};

} // end namespace stattools::prior

#endif // TPRIORNORMALTWOMIXEDMODELSSTRETCH_H
