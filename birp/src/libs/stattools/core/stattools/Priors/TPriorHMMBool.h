//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORHMMBOOL_H
#define TPRIORHMMBOOL_H

#include "stattools/Priors/TPriorHMMBase.h"
#include "stattools/Priors/TypesIdentifiers.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/HMMDistances/TTransitionMatrixDist.h"
#include "stattools/HMMDistances/TTransitionMatrixHMMDistances.h"
#include "stattools/HMMDistances/TOptimizeTransMatNelderMead.h"

//-------------------------------------------
// THMMBoolInferred
//-------------------------------------------
namespace stattools::prior {

template<typename Derived, typename Type, size_t NumDim, typename SpecPi, typename SpecGamma>
class THMMBoolInferred
	: public THMMPriorStandard<Derived, Type, NumDim, TTransitionMatrixDistances<double, size_t, size_t>> {
	static_assert(TypesAreBool<Type>() && TypesAreZeroOneOpen<typename SpecPi::value_type>() &&
				  TypesArePositive<typename SpecGamma::value_type>());

private:
	using BoxType        = THMMBoolInferred<Derived, Type, NumDim, SpecPi, SpecGamma>;
	using TypeParamPi    = TParameter<SpecPi, BoxType>;
	using TypeParamGamma = TParameter<SpecGamma, BoxType>;

	TypeParamPi *_pi       = nullptr;
	TypeParamGamma *_gamma = nullptr;
	using Base = THMMPriorStandard<Derived, Type, NumDim, TTransitionMatrixDistances<double, size_t, size_t>>;
	using Base::_transitionMatrix;
	using TypeTransMat = TOptimizeTransMatNelderMead<double, size_t, size_t, TTransitionMatrixBool<double, size_t>>;

protected:
	using typename Base::Storage;
	using typename Base::UpdatedStorage;

	void _storeEMEstimatesInMCMCParameters(const std::vector<double> &Values) override {
		// fill MCMC parameters based on values from Baum-Welch
		if (!this->_fixTransitionMatricesDuringEM()) {
			_pi->set(Values[0]);
			_gamma->set(Values[1]);
		}
	}

	void _setValuesBeforeEM() override {
		if (this->_fixTransitionMatricesDuringEM()) { _transitionMatrix.setValuesEM(getParams()); }
	}

	void _simulateUnderPrior(Storage *Data) override {
		impl::simulateFixedFractionZ1<decltype(*this), decltype(Data), Type>(*this, Data);
	};

public:
	THMMBoolInferred(TypeParamPi *Pi, TypeParamGamma *Gamma, coretools::TDistancesBinnedBase *Distances)
		: Base(Distances, std::make_shared<TypeTransMat>(2)), _pi(Pi), _gamma(Gamma) {
		this->addPriorParameter({_pi, _gamma});
	};

	~THMMBoolInferred() override = default;

	[[nodiscard]] std::string name() const override { return "hmm_bool"; }

	void initialize() override {
		_pi->initStorage(this);
		_gamma->initStorage(this);

		this->updateTau(getParams());
	};

	[[nodiscard]] std::vector<double> getParams() const override { return {_pi->value(), _gamma->value()}; }

	auto pi() const { return _pi->value(); }

	auto gamma() const { return _gamma->value(); }

	[[nodiscard]] const arma::mat &getTau(size_t index) const { return _transitionMatrix[index]; }

	void switchPriorClassificationAfterEM() override {
		// classification of components by EM is random, but sometimes we have certain restrictions (e.g. normal mixed
		// model with 2 components, shared mean but different variances) we then might have to switch the EM labels and
		// all associated prior parameters

		// we switch z but are not allowed to switch pi and gamma -> will not match at all!
		if (this->_fixTransitionMatricesDuringEM()) {
			coretools::instances::logfile().warning(
				"Switched z after EM, but can not switch pi and gamma, because initial values are fix.");
		}
		// pi = 1-pi
		_pi->set(1. - _pi->value());

		// gamma = (1-pi)/Q(1->0) - 1
		// but Q(1->0) switched with Q(0->1)
		// -> gamma' = (1-pi)/Q(0->1) - 1
		auto p1 = _transitionMatrix.dist1_transitionProbability(0, 1);
		_gamma->set((1. - _pi->value()) / p1 - 1.);

		this->updateTau(getParams());
	}
};

} // end namespace stattools::prior

#endif // TPRIORHMMBOOL_H
