//
// Created by madleina on 19.12.23.
//

#ifndef STATTOOLS_THMMCOMBINEDSCALEDLADDERSTEST_H
#define STATTOOLS_THMMCOMBINEDSCALEDLADDERSTEST_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorExponential.h"
#include "stattools/Priors/TPriorHMMCombinedScaledLadders.h"
#include "stattools/ParametersObservations/TObservation.h"
#include "coretools/Types/TStringHash.h"

namespace stattools {

template<bool BelowIsObservation, typename Type, template<typename, typename> typename TypeTransitionMatrix>
struct THMMCombinedScaledLaddersTest {
	using TypeKappa                     = coretools::StrictlyPositive;
	using TypeNu                        = coretools::StrictlyPositive;
	using TypeMu                        = coretools::StrictlyPositive;
	constexpr static size_t NumDimObs   = 1;
	constexpr static size_t NumDimKappa = 1;
	constexpr static size_t NumDimNu    = 1;
	constexpr static size_t NumDimMu    = 1;

	using BoxOnKappa = prior::TExponentialFixed<TParameterBase, TypeKappa, NumDimKappa>;
	using BoxOnNu    = prior::TExponentialFixed<TParameterBase, TypeKappa, NumDimNu>;
	using BoxOnMu    = prior::TExponentialFixed<TParameterBase, TypeKappa, NumDimMu>;

	using SpecKappas = ParamSpec<TypeKappa, Hash<coretools::toHash("kappas")>, BoxOnKappa>;
	using SpecNus    = ParamSpec<TypeNu, Hash<coretools::toHash("nus")>, BoxOnNu>;
	using SpecMus    = ParamSpec<TypeMu, Hash<coretools::toHash("mus")>, BoxOnMu>;

	using BelowType = std::conditional_t<BelowIsObservation, TObservationBase, TParameterBase>;
	using BoxOnObs  = prior::THMMCombinedScaledLadderInferred<BelowType, Type, NumDimObs, SpecKappas, SpecNus, SpecMus,
															  TypeTransitionMatrix>;
	using SpecObs   = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnKappa boxOnKappas;
	BoxOnNu boxOnNus;
	BoxOnMu boxOnMus;

	TParameter<SpecKappas, BoxOnObs> kappa;
	TParameter<SpecNus, BoxOnObs> nu;
	TParameter<SpecMus, BoxOnObs> mu;

	BoxOnObs boxOnObs;

	THMMCombinedScaledLaddersTest(std::string_view Filename, const std::vector<size_t> &NumStatesPerChain,
								  coretools::TDistancesBinnedBase *Distances,
								  const std::vector<size_t> &AttractorIndex = {})
		: kappa("kappa", &boxOnKappas, {Filename, "1."}), nu("nu", &boxOnNus, {Filename, "10."}),
		  mu("mu", &boxOnMus, {Filename, "10."}),
		  boxOnObs(&kappa, &nu, &mu, NumStatesPerChain, Distances, AttractorIndex) {

		Type::setMax(coretools::containerProduct(NumStatesPerChain) - 1);
	}
};

} // namespace stattools

#endif // STATTOOLS_THMMCOMBINEDSCALEDLADDERSTEST_H
