//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORHMMLADDER_H
#define TPRIORHMMLADDER_H

#include "stattools/HMMDistances/TOptimizeTransMatLineSearch.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TPriorHMMBase.h"
#include "stattools/HMMDistances/TTransitionMatrixDist.h"
#include "stattools/HMMDistances/TTransitionMatrixHMMDistances.h"


namespace stattools::prior {

//-------------------------------------------
// THMMLadderInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecKappa>
class THMMLadderInferred
	: public THMMPriorStandard<Derived, Type, NumDim, TTransitionMatrixDistances<double, size_t, size_t>> {
	static_assert(TypesAreUnsignedIntWithVariableMax<Type>() &&
				  TypesAreStrictlyPositiveFloatingPoints<typename SpecKappa::value_type>());

private:
	using BoxType = THMMLadderInferred<Derived, Type, NumDim, SpecKappa>;
	using Base    = THMMPriorStandard<Derived, Type, NumDim, TTransitionMatrixDistances<double, size_t, size_t>>;
	using Base::_transitionMatrix;
	using TypeTransMat   = TOptimizeTransMatLineSearch<double, size_t, size_t, TTransitionMatrixLadder<double, size_t>>;
	using TypeParamKappa = TParameter<SpecKappa, BoxType>;

	TypeParamKappa *_kappa = nullptr;

protected:
	using typename Base::Storage;
	using typename Base::UpdatedStorage;

	void _storeEMEstimatesInMCMCParameters(const std::vector<double> &Values) override {
		// fill MCMC parameters based on values from Baum-Welch
		if (!this->_fixTransitionMatricesDuringEM()) {
			assert(Values.size() == 1);
			_kappa->set(static_cast<typename SpecKappa::value_type>(Values[0]));
		}
	}

	void _setValuesBeforeEM() override {
		if (this->_fixTransitionMatricesDuringEM()) { _transitionMatrix.setValuesEM(getParams()); }
	}

public:
	THMMLadderInferred(TypeParamKappa *Kappa, size_t NumStates, coretools::TDistancesBinnedBase *Distances)
		: Base(Distances, std::make_shared<TypeTransMat>(NumStates)), _kappa(Kappa) {
		this->addPriorParameter(_kappa);
	};

	~THMMLadderInferred() override = default;

	std::string name() const override { return "hmm_ladder"; }

	void initialize() override {
		_kappa->initStorage(this);

		this->updateTau(getParams());

		// check if max = numStates-1
		if (Type::max() != _transitionMatrix.numStates() - 1) {
			DEVERROR("numStates-1 (", _transitionMatrix.numStates() - 1, ") differs from maximum value of Type (",
					 Type::max(), ").");
		}
	}

	[[nodiscard]] std::vector<double> getParams() const override { return {static_cast<double>(_kappa->value())}; }

	auto kappa() const { return _kappa->value(); }

	[[nodiscard]] const arma::mat &getTau(size_t index) const { return _transitionMatrix[index]; }
};

} // end namespace stattools::prior

#endif // TPRIORHMMLADDER_H
