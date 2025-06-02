//
// Created by madleina on 18.12.23.
//

#ifndef STATTOOLS_TNORMALTEST_H
#define STATTOOLS_TNORMALTEST_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorExponential.h"
#include "stattools/Priors/TPriorNormal.h"
#include "stattools/ParametersObservations/TObservation.h"
#include "coretools/Types/TStringHash.h"

namespace stattools {

template<bool BelowIsObservation = true> struct TNormalTest {
	using Type                           = coretools::Unbounded;
	constexpr static size_t NumDimObs    = 1;
	using TypeMu                         = coretools::Unbounded;
	constexpr static size_t NumDimMu     = 1;
	using TypeSigma2                     = coretools::StrictlyPositive;
	constexpr static size_t NumDimSigma2 = 1;

	using BoxOnMu     = prior::TNormalFixed<TParameterBase, TypeMu, NumDimMu>;
	using BoxOnSigma2 = prior::TExponentialFixed<TParameterBase, TypeSigma2, NumDimSigma2>;

	using SpecMu     = ParamSpec<TypeMu, Hash<coretools::toHash("mu")>, BoxOnMu>;
	using SpecSigma2 = ParamSpec<TypeSigma2, Hash<coretools::toHash("sigma2")>, BoxOnSigma2>;

	using BelowType = std::conditional_t<BelowIsObservation, TObservationBase, TParameterBase>;

	using BoxOnObs = prior::TNormalInferred<BelowType, Type, NumDimObs, SpecMu, SpecSigma2>;
	using SpecObs  = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnMu boxOnMu{};
	TParameter<SpecMu, BoxOnObs> mu;

	BoxOnSigma2 boxOnSigma2{};
	TParameter<SpecSigma2, BoxOnObs> sigma2;

	BoxOnObs boxOnObs;

	TNormalTest(std::string_view Filename)
		: mu("mu", &boxOnMu, {Filename, "0,1"}), sigma2("sigma2", &boxOnSigma2, {Filename, "1"}),
		  boxOnObs(&mu, &sigma2) {}
};

} // namespace stattools

#endif // STATTOOLS_TNORMALTEST_H
