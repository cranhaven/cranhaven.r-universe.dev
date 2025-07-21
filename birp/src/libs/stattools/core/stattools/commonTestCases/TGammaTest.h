//
// Created by madleina on 18.12.23.
//

#ifndef STATTOOLS_TGAMMATEST_H
#define STATTOOLS_TGAMMATEST_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorExponential.h"
#include "stattools/Priors/TPriorGamma.h"
#include "stattools/ParametersObservations/TObservation.h"
#include "coretools/Types/TStringHash.h"

namespace stattools {

struct TGammaTest {
	using Type                          = coretools::StrictlyPositive;
	constexpr static size_t NumDimObs   = 1;
	using TypeAlpha                     = coretools::StrictlyPositive;
	constexpr static size_t NumDimAlpha = 1;
	using TypeBeta                      = coretools::StrictlyPositive;
	constexpr static size_t NumDimBeta  = 1;

	using BoxOnAlpha = prior::TExponentialFixed<TParameterBase, TypeAlpha, NumDimAlpha>;
	using BoxOnBeta  = prior::TExponentialFixed<TParameterBase, TypeBeta, NumDimBeta>;

	using SpecAlpha = ParamSpec<TypeAlpha, Hash<coretools::toHash("alpha")>, BoxOnAlpha>;
	using SpecBeta  = ParamSpec<TypeBeta, Hash<coretools::toHash("beta")>, BoxOnBeta>;
	using BoxOnObs  = prior::TGammaInferred<TObservationBase, Type, 1, SpecAlpha, SpecBeta>;
	using SpecObs   = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnAlpha boxOnAlpha;
	BoxOnBeta boxOnBeta;

	TParameter<SpecAlpha, BoxOnObs> alpha;
	TParameter<SpecBeta, BoxOnObs> beta;

	BoxOnObs boxOnObs;

	TGammaTest(std::string_view Filename)
		: alpha("alpha", &boxOnAlpha, {Filename, "1"}), beta("beta", &boxOnBeta, {Filename, "1"}),
		  boxOnObs(&alpha, &beta) {}
};

} // namespace stattools

#endif // STATTOOLS_TGAMMATEST_H
