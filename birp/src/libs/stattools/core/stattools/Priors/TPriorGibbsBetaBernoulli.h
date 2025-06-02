//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORGIBBSBETABERNOUILLI_H
#define TPRIORGIBBSBETABERNOUILLI_H

#include "stattools/Priors/TPriorBernoulli.h"
#include "stattools/Priors/TPriorBeta.h"

namespace stattools::prior {

//-------------------------------------------
// TGibbsBetaBernoulliInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecPi>
class TGibbsBetaBernoulliInferred
	: public TBernoulliInferredBase<Derived, Type, NumDim, SpecPi,
									 TGibbsBetaBernoulliInferred<Derived, Type, NumDim, SpecPi>> {
private:
	using BoxType = TGibbsBetaBernoulliInferred<Derived, Type, NumDim, SpecPi>;
	using Base    = TBernoulliInferredBase<Derived, Type, NumDim, SpecPi, BoxType>;
	using typename Base::Storage;
	using typename Base::TypeParamPi;
	using typename Base::UpdatedStorage;

	// beta prior of TypePi (-> multiple inheritance didn't work because of inheriting from base class with different
	// types)
	TBetaFixed<TParameterBase, typename SpecPi::value_type, SpecPi::numDim> *_betaPrior;

protected:
	using Base::_pi;

public:
	TGibbsBetaBernoulliInferred(TypeParamPi *Pi,
								 TBetaFixed<TParameterBase, typename SpecPi::value_type, SpecPi::numDim> *PriorOnPi)
		: Base(Pi), _betaPrior(PriorOnPi){};
	~TGibbsBetaBernoulliInferred() override = default;

	[[nodiscard]] std::string name() const override { return "gibbsBetaBernoulli"; }

	void initialize() override {
		_pi->initStorage(this);
		this->_updateTempVals();
	};

	auto alpha() const { return _betaPrior->alpha(); }

	auto beta() const { return _betaPrior->beta(); }

	void doGibbs(TypeParamPi *, size_t /*i*/) {
		double sum   = 0.;
		double total = 0.;
		for (const auto &storage : this->_storageBelow) {
			sum += storage->sum();
			total += storage->size();
		}

		// get ptr to getBetaRandom function
		auto ptr = static_cast<coretools::Probability (coretools::TRandomGenerator::*)(
			coretools::StrictlyPositive, coretools::StrictlyPositive)>(&coretools::TRandomGenerator::getBetaRandom);
		_pi->sample(0, ptr, sum + _betaPrior->alpha(), total - sum + _betaPrior->beta());
	}
};

} // end namespace stattools::prior

#endif // TPRIORGIBBSBETABERNOUILLI_H
