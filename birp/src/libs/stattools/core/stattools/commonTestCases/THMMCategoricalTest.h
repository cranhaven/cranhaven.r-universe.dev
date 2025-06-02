//
// Created by madleina on 18.12.23.
//

#ifndef STATTOOLS_THMMCATEGORICALTEST_H
#define STATTOOLS_THMMCATEGORICALTEST_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorBeta.h"
#include "stattools/Priors/TPriorExponential.h"
#include "stattools/Priors/TPriorUniform.h"
#include "stattools/Priors/TPriorHMMCategorical.h"
#include "stattools/ParametersObservations/TObservation.h"
#include "coretools/Types/TStringHash.h"

namespace stattools {

struct THMMCategoricalTest {
	using Type                          = coretools::UnsignedInt8WithMax<0>;
	using TypePi                        = coretools::ZeroOneOpen;
	using TypeGamma                     = coretools::Positive;
	using TypeRhos                      = coretools::ZeroOpenOneClosed;
	constexpr static size_t NumDimObs   = 1;
	constexpr static size_t NumDimPi    = 1;
	constexpr static size_t NumDimGamma = 1;
	constexpr static size_t NumDimRhos  = 1;

	using BoxOnPi    = prior::TBetaFixed<TParameterBase, TypePi, NumDimPi>;
	using BoxOnGamma = prior::TExponentialFixed<TParameterBase, TypeGamma, NumDimGamma>;
	using BoxOnRhos  = prior::TUniformFixed<TParameterBase, TypeRhos, NumDimRhos>;

	using SpecPi    = ParamSpec<TypePi, Hash<coretools::toHash("pi")>, BoxOnPi>;
	using SpecGamma = ParamSpec<TypeGamma, Hash<coretools::toHash("gamma")>, BoxOnGamma>;
	using SpecRhos  = ParamSpec<TypeRhos, Hash<coretools::toHash("rhos")>, BoxOnRhos, SumOne<0>>;
	using BoxOnObs  = prior::THMMCategoricalInferred<TObservationBase, Type, 1, SpecPi, SpecGamma, SpecRhos>;
	using SpecObs   = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnPi boxOnPi;
	BoxOnGamma boxOnGamma;
	BoxOnRhos boxOnRhos;

	TParameter<SpecPi, BoxOnObs> pi;
	TParameter<SpecGamma, BoxOnObs> gamma;
	TParameter<SpecRhos, BoxOnObs> rhos;

	BoxOnObs boxOnObs;

	THMMCategoricalTest(std::string_view Filename, size_t D, coretools::TDistancesBinnedBase *Distances)
		: pi("pi", &boxOnPi, {Filename, "0.8, 7.2"}),
		  gamma("gamma", &boxOnGamma, {Filename, "0.2", ProposalKernel::MCMCProposalKernel::scaleLogNormal}),
		  rhos("rhos", &boxOnRhos, {Filename}), boxOnObs(&pi, &gamma, &rhos, D, Distances) {

		Type::setMax(D);
	}
};

} // namespace stattools

#endif // STATTOOLS_THMMCATEGORICALTEST_H
