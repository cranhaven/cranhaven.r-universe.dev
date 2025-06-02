//
// Created by madleina on 18.12.23.
//

#ifndef STATTOOLS_THMMBOOLTEST_H
#define STATTOOLS_THMMBOOLTEST_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorBeta.h"
#include "stattools/Priors/TPriorExponential.h"
#include "stattools/Priors/TPriorHMMBool.h"
#include "coretools/Types/TStringHash.h"
#include "stattools/ParametersObservations/TObservation.h"

namespace stattools {

template<bool BelowIsObservation> struct THMMBoolTest {
	using Type                          = coretools::Boolean;
	constexpr static size_t NumDimObs   = 1;
	using TypePi                        = coretools::ZeroOneOpen;
	constexpr static size_t NumDimPi    = 1;
	using TypeGamma                     = coretools::Positive;
	constexpr static size_t NumDimGamma = 1;

	using BoxOnPi    = prior::TBetaFixed<TParameterBase, TypePi, NumDimPi>;
	using BoxOnGamma = prior::TExponentialFixed<TParameterBase, TypeGamma, NumDimGamma>;

	using SpecPi    = ParamSpec<TypePi, Hash<coretools::toHash("pi")>, BoxOnPi>;
	using SpecGamma = ParamSpec<TypeGamma, Hash<coretools::toHash("gamma")>, BoxOnGamma>;

	using BelowType = std::conditional_t<BelowIsObservation, TObservationBase, TParameterBase>;
	using BoxOnZ    = prior::THMMBoolInferred<BelowType, Type, 1, SpecPi, SpecGamma>;
	using SpecZ     = TObservation<Type, NumDimObs, BoxOnZ>;

	BoxOnPi boxOnPi;
	BoxOnGamma boxOnGamma;

	TParameter<SpecPi, BoxOnZ> pi;
	TParameter<SpecGamma, BoxOnZ> gamma;

	BoxOnZ boxOnZ;

	THMMBoolTest(std::string_view Filename, coretools::TDistancesBinnedBase *Distances)
		: pi("pi", &boxOnPi, {Filename, "0.8, 7.2"}),
		  gamma("gamma", &boxOnGamma, {Filename, "0.2", ProposalKernel::MCMCProposalKernel::scaleLogNormal}),
		  boxOnZ(&pi, &gamma, Distances) {}
};

} // namespace stattools

#endif // STATTOOLS_THMMBOOLTEST_H
