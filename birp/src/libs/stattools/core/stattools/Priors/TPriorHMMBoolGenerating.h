//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORHMMBOOLGENERATING_H
#define TPRIORHMMBOOLGENERATING_H

#include "stattools/Priors/TPriorHMMBase.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TypesIdentifiers.h"
#include "stattools/HMMDistances/TTransitionMatrixDist.h"
#include "stattools/HMMDistances/TTransitionMatrixHMMDistances.h"
#include "stattools/HMMDistances/TOptimizeTransMatNelderMead.h"

namespace stattools::prior {

//-------------------------------------------
// THMMBoolGeneratingMatrixInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecLogLambda1, typename SpecLogLambda2>
class THMMBoolGeneratingMatrixInferred
	: public THMMPriorStandard<Derived, Type, NumDim,
							   TTransitionMatrixDistances<double, size_t, size_t>> {
	static_assert(TypesAreBool<Type>() &&
				  TypesAreFloatingPoint<typename SpecLogLambda1::value_type, typename SpecLogLambda2::value_type>());

private:
	using BoxType             = THMMBoolGeneratingMatrixInferred<Derived, Type, NumDim, SpecLogLambda1, SpecLogLambda2>;
	using Base                = THMMPriorStandard<Derived, Type, NumDim,
								   TTransitionMatrixDistances<double, size_t, size_t>>;
	using TypeParamLogLambda1 = TParameter<SpecLogLambda1, BoxType>;
	using TypeParamLogLambda2 = TParameter<SpecLogLambda2, BoxType>;

	TypeParamLogLambda1 *_log_lambda1 = nullptr;
	TypeParamLogLambda2 *_log_lambda2 = nullptr;

	using Base::_transitionMatrix;
	using TypeTransMat =
		TOptimizeTransMatNelderMead<double, size_t, size_t, TTransitionMatrixBoolGeneratingMatrix<double, size_t>>;

protected:
	using typename Base::Storage;
	using typename Base::UpdatedStorage;

	void _storeEMEstimatesInMCMCParameters(const std::vector<double> &Values) override {
		// fill MCMC parameters based on values from Baum-Welch
		if (!this->_fixTransitionMatricesDuringEM()) {
			assert(Values.size() == 2);
			_log_lambda1->set(log(Values[0]));
			_log_lambda2->set(log(Values[1]));
		}
	}

	void _setValuesBeforeEM() override {
		if (this->_fixTransitionMatricesDuringEM()) { _transitionMatrix.setValuesEM(getParams()); }
	}

	void _simulateUnderPrior(Storage *Data) override {
		impl::simulateFixedFractionZ1<decltype(*this), decltype(Data), Type>(*this, Data);
	};

public:
	THMMBoolGeneratingMatrixInferred(TypeParamLogLambda1 *LogLambda1, TypeParamLogLambda2 *LogLambda2,
									 coretools::TDistancesBinnedBase *Distances)
		: Base(Distances, std::make_shared<TypeTransMat>(2)), _log_lambda1(LogLambda1), _log_lambda2(LogLambda2) {

		this->addPriorParameter({_log_lambda1, _log_lambda2});
	};

	~THMMBoolGeneratingMatrixInferred() override = default;

	[[nodiscard]] std::string name() const override { return "hmm_bool_generating_matrix"; }

	void initialize() override {
		_log_lambda1->initStorage(this);
		_log_lambda2->initStorage(this);

		this->updateTau(getParams());
	};

	[[nodiscard]] std::vector<double> getParams() const override {
		return {exp(_log_lambda1->value()), exp(_log_lambda2->value())};
	}

	auto log_lambda_1() const { return _log_lambda1->value(); }

	auto log_lambda_2() const { return _log_lambda2->value(); }

	const arma::mat &getTau(size_t index) const { return _transitionMatrix[index]; }

	void switchPriorClassificationAfterEM() override {
		// classification of components by EM is random, but sometimes we have certain restrictions (e.g. normal mixed
		// model with 2 components, shared mean but different variances) we then might have to switch the EM labels and
		// all associated prior parameters

		// we switch z but are not allowed to switch lambda1 and lambda2 -> will not match at all!
		if (this->_fixTransitionMatricesDuringEM()) {
			coretools::instances::logfile().warning(
				"Switched z after EM, but can not switch Lambda, because initial values are fix.");
		}
		double tmpLambda1 = _log_lambda1->value();
		_log_lambda1->set(_log_lambda2->value());
		_log_lambda2->set(tmpLambda1);
		this->updateTau(getParams());
	}
};

} // end namespace stattools::prior

#endif // TPRIORHMMBOOLGENERATING_H
