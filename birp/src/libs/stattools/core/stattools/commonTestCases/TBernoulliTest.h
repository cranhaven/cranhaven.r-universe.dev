//
// Created by madleina on 18.12.23.
//

#ifndef STATTOOLS_TBERNOUILLITEST_H
#define STATTOOLS_TBERNOUILLITEST_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorBernoulli.h"
#include "stattools/Priors/TPriorBeta.h"
#include "stattools/Priors/TPriorGibbsBetaBernoulli.h"
#include "coretools/Types/TStringHash.h"
#include "stattools/ParametersObservations/TObservation.h"

namespace stattools {

//--------------------------------
// TBernoulliTest
//--------------------------------

template<bool BelowIsObservation, size_t NumDimObs> struct TBernoulliTest {
	using Type                        = coretools::Boolean;
	using TypePi                      = coretools::ZeroOneOpen;
	constexpr static size_t NumDimPi  = 1;
	constexpr static size_t numDimObs = NumDimObs;

	using BoxOnPi = prior::TBetaFixed<TParameterBase, TypePi, NumDimPi>;
	using SpecPi  = ParamSpec<TypePi, Hash<coretools::toHash("pi")>, BoxOnPi>;

	using BelowType = std::conditional_t<BelowIsObservation, TObservationBase, TParameterBase>;
	using BoxOnZ    = prior::TBernoulliInferred<BelowType, Type, NumDimObs, SpecPi>;
	using SpecZ     = TObservation<Type, NumDimObs, BoxOnZ>;

	BoxOnPi boxOnPi;
	TParameter<SpecPi, BoxOnZ> pi;

	BoxOnZ boxOnZ;

	TBernoulliTest(std::string_view Filename) : pi("pi", &boxOnPi, {Filename, "2,2"}), boxOnZ(&pi) {}
};

//--------------------------------
// TGibbsBetaBernoulliTest
//--------------------------------

struct TGibbsBetaBernoulliTest {
	using Type                        = coretools::Boolean;
	constexpr static size_t NumDimObs = 1;
	using TypePi                      = coretools::ZeroOneOpen;
	constexpr static size_t NumDimPi  = 1;

	using BoxOnPi  = prior::TBetaFixed<TParameterBase, TypePi, NumDimPi>;
	using SpecPi   = ParamSpec<TypePi, Hash<coretools::toHash("pi")>, BoxOnPi>;
	using BoxOnObs = prior::TGibbsBetaBernoulliInferred<TObservationBase, Type, 1, SpecPi>;
	using SpecObs  = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnPi boxOnPi;
	TParameter<SpecPi, BoxOnObs> pi;

	BoxOnObs boxOnObs;

	TGibbsBetaBernoulliTest(std::string_view Filename)
		: pi("pi", &boxOnPi, {Filename, "0.7, 0.8"}), boxOnObs(&pi, &boxOnPi) {}
};

} // namespace stattools

#endif // STATTOOLS_TBERNOUILLITEST_H
