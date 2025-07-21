//
// Created by madleina on 27.05.22.
//

#ifndef TPRIORHMMCATEGORICAL_H
#define TPRIORHMMCATEGORICAL_H

#include "stattools/HMMDistances/TOptimizeTransMatNelderMead.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TPriorHMMBase.h"
#include "stattools/Priors/TypesIdentifiers.h"
#include "stattools/HMMDistances/TTransitionMatrixDist.h"
#include "stattools/HMMDistances/TTransitionMatrixHMMDistances.h"
#include "stattools/HMMDistances/TOptimizeTransMatNelderMead.h"

//-------------------------------------------
// THMMCategoricalInferred
//-------------------------------------------

namespace stattools::prior {

template<typename Derived, typename Type, size_t NumDim, typename SpecPi, typename SpecGamma, typename SpecRhos>
class THMMCategoricalInferred
	: public THMMPriorStandard<Derived, Type, NumDim, TTransitionMatrixDistances<double, size_t, size_t>> {
	static_assert(TypesAreUnsignedIntWithVariableMax<Type>() && TypesAreZeroOneOpen<typename SpecPi::value_type>() &&
				  TypesArePositive<typename SpecGamma::value_type>() &&
				  TypesAreZeroOpenOneClosed<typename SpecRhos::value_type>());
	static_assert(SpecRhos::constraint::constraint == Constraints::sumOne);
	static_assert(!SpecRhos::parallelize);

private:
	using Base = THMMPriorStandard<Derived, Type, NumDim, TTransitionMatrixDistances<double, size_t, size_t>>;
	using Base::_transitionMatrix;
	using TypeTransMat =
		TOptimizeTransMatNelderMead<double, size_t, size_t, TTransitionMatrixCategorical<double, size_t>>;
	using BoxType = THMMCategoricalInferred<Derived, Type, NumDim, SpecPi, SpecGamma, SpecRhos>;

	using TypeParamPi    = TParameter<SpecPi, BoxType>;
	using TypeParamGamma = TParameter<SpecGamma, BoxType>;
	using TypeParamRho   = TParameter<SpecRhos, BoxType>;

	TypeParamPi *_pi       = nullptr;
	TypeParamGamma *_gamma = nullptr;
	TypeParamRho *_rhos    = nullptr;
	size_t _D              = 0; // note: numStates of matrix = D + 1 (for neutral state)!

protected:
	using typename Base::Storage;
	using typename Base::UpdatedStorage;

	void _storeEMEstimatesInMCMCParameters(const std::vector<double> &Values) override {
		// fill MCMC parameters based on values from Baum-Welch
		if (!this->_fixTransitionMatricesDuringEM()) {
			_pi->set(Values[0]);
			_gamma->set(Values[1]);
			for (size_t d = 0; d < _D; d++) { _rhos->set(d, Values[d + 2]); }
		}
	}

	void _setValuesBeforeEM() override {
		if (this->_fixTransitionMatricesDuringEM()) { _transitionMatrix.setValuesEM(getParams()); }
	}

	void _simulateUnderPrior(Storage *Data) override {
		_rhos->normalize(_rhos->getFull());
		if (_D == 1) {
			impl::simulateFixedFractionZ1<decltype(*this), decltype(Data), Type>(*this, Data);
		} else {
			Base::_simulateUnderPrior(Data);
		}
	}

public:
	THMMCategoricalInferred(TypeParamPi *Pi, TypeParamGamma *Gamma, TypeParamRho *Rhos, size_t D,
							coretools::TDistancesBinnedBase *Distances)
		: Base(Distances, std::make_shared<TypeTransMat>(D + 1)),
		  // numStates = D + 1 (for neutral)
		  _pi(Pi), _gamma(Gamma), _rhos(Rhos), _D(D) {

		this->addPriorParameter({_pi, _gamma, _rhos});
	};
	~THMMCategoricalInferred() override = default;

	[[nodiscard]] std::string name() const override { return "hmm_categorical"; }

	void initialize() override {
		_pi->initStorage(this);
		_gamma->initStorage(this);

		// rho will have indices as dimension names (results in rho_1, rho_2, rho_3, ...)
		auto dimNames = std::make_shared<coretools::TNamesIndices>();
		dimNames->setOffset(_rhos->getDefinition().offSetDimensionNames());
		_rhos->initStorage(this, {_D}, {dimNames});
		if (_rhos->hasFixedInitialValue()) { _rhos->normalize(_rhos->getFull()); }

		// check if max of Type below = D (not D-1 since there is an additional neutral state)
		if (Type::max() != _D) { throw coretools::TDevError("D (", _D, ") differs from maximum value of Type (", Type::max(), ")."); }

		// if D = 1, rho must be 1 -> don't update
		if (_D == 1) { _rhos->setIsUpdated(false); }

		this->updateTau(getParams());
	};

	[[nodiscard]] std::vector<double> getParams() const override {
		std::vector<double> values(_D + 2);
		values[0] = _pi->value();
		values[1] = _gamma->value();
		for (size_t d = 0; d < _D; d++) { values[d + 2] = _rhos->value(d); }
		return values;
	}

	auto pi() const { return _pi->value(); }

	auto gamma() const { return _gamma->value(); }

	auto rho(size_t d) const { return _rhos->value(d); }

	[[nodiscard]] const arma::mat &getTau(size_t index) const { return _transitionMatrix[index]; }
};

} // end namespace stattools::prior

#endif // TPRIORHMMCATEGORICAL_H
