//
// Created by madleina on 18.12.23.
//

#ifndef STATTOOLS_TBETATEST_H
#define STATTOOLS_TBETATEST_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorBeta.h"
#include "stattools/Priors/TPriorExponential.h"
#include "stattools/Priors/TPriorUniform.h"
#include "coretools/Types/TStringHash.h"
#include "stattools/ParametersObservations/TObservation.h"

namespace stattools {

//-----------------------------------
// TBetaTest
//-----------------------------------

template<typename TypeAlpha, typename TypeBeta, typename BoxOnAlpha, typename BoxOnBeta> struct TBetaTest {
	using Type                          = coretools::ZeroOneOpen;
	constexpr static size_t NumDimObs   = 1;
	constexpr static size_t NumDimAlpha = 1;
	constexpr static size_t NumDimBeta  = 1;

	using SpecAlpha = ParamSpec<TypeAlpha, Hash<coretools::toHash("alpha")>, BoxOnAlpha>;
	using SpecBeta  = ParamSpec<TypeBeta, Hash<coretools::toHash("beta")>, BoxOnBeta>;
	using BoxOnObs  = prior::TBetaInferred<TObservationBase, Type, 1, SpecAlpha, SpecBeta>;
	using SpecObs   = TObservation<Type, NumDimObs, BoxOnObs>;

	TParameter<SpecAlpha, BoxOnObs> alpha;
	TParameter<SpecBeta, BoxOnObs> beta;

	BoxOnObs boxOnObs;

	TBetaTest(std::string_view Filename, std::string_view PriorParams, BoxOnAlpha *boxOnAlpha, BoxOnBeta *boxOnBeta)
	    : alpha("alpha", boxOnAlpha, {Filename, PriorParams}), beta("beta", boxOnBeta, {Filename, PriorParams}),
	      boxOnObs(&alpha, &beta) {}
};

//-----------------------------------
// TSymmetricBetaTest
//-----------------------------------

template<typename TypeAlpha, typename BoxOnAlpha> struct TSymmetricBetaTest {
	using Type                          = coretools::ZeroOneOpen;
	constexpr static size_t NumDimObs   = 1;
	constexpr static size_t NumDimAlpha = 1;

	using SpecAlpha = ParamSpec<TypeAlpha, Hash<coretools::toHash("alpha")>, BoxOnAlpha>;
	using BoxOnObs  = prior::TBetaSymmetricInferred<TObservationBase, Type, 1, SpecAlpha>;
	using SpecObs   = TObservation<Type, NumDimObs, BoxOnObs>;

	TParameter<SpecAlpha, BoxOnObs> alpha;

	BoxOnObs boxOnObs;

	TSymmetricBetaTest(std::string_view Filename, std::string_view PriorParams, BoxOnAlpha *boxOnAlpha)
	    : alpha("alpha", boxOnAlpha, {Filename, PriorParams}), boxOnObs(&alpha) {}
};

//-----------------------------------
// TSymmetricBetaZeroMixtureTest
//-----------------------------------

template<typename BoxTypeZ> struct TSymmetricBetaZeroMixtureTest {
	using Type                          = coretools::Probability;
	constexpr static size_t NumDimObs   = 1;
	using TypeAlpha                     = coretools::StrictlyPositive;
	constexpr static size_t NumDimAlpha = 1;
	using TypeZ                         = coretools::Boolean;
	constexpr static size_t NumDimZ     = 1;

	using BoxOnAlpha = prior::TExponentialFixed<TParameterBase, TypeAlpha, NumDimAlpha>;
	using BoxOnZ     = prior::TUniformFixed<TParameterBase, TypeZ, NumDimZ>;

	using SpecAlpha = ParamSpec<TypeAlpha, Hash<coretools::toHash("alpha")>, BoxOnAlpha>;
	using SpecZ     = ParamSpec<TypeZ, Hash<coretools::toHash("z")>, BoxOnZ>;
	using BoxOnObs  = prior::TBetaSymmetricZeroMixtureInferred<TObservationBase, Type, 1, SpecAlpha, SpecZ, BoxTypeZ>;
	using SpecObs   = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnAlpha boxOnAlpha;
	BoxOnZ boxOnZ;

	TParameter<SpecAlpha, BoxOnObs> alpha;
	TParameter<SpecZ, BoxTypeZ> z;

	BoxOnObs boxOnObs;

	TSymmetricBetaZeroMixtureTest(std::string_view Filename, std::string_view PriorParams)
	    : alpha("alpha", &boxOnAlpha, {Filename, PriorParams}), z("z", &boxOnZ, {Filename}), boxOnObs(&alpha, &z) {}
};

} // namespace stattools

#endif // STATTOOLS_TBETATEST_H
