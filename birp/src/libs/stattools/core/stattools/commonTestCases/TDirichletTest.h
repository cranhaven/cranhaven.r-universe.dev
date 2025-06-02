//
// Created by madleina on 18.12.23.
//

#ifndef STATTOOLS_TDIRICHLETTEST_H
#define STATTOOLS_TDIRICHLETTEST_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorDirichlet.h"
#include "coretools/Types/TStringHash.h"
#include "stattools/Priors/TPriorExponential.h"
#include "stattools/ParametersObservations/TObservation.h"

namespace stattools {

struct TDirichletTest {
	using Type                          = coretools::ZeroOpenOneClosed;
	constexpr static size_t NumDimObs   = 1;
	using TypeSigma                     = coretools::StrictlyPositive;
	constexpr static size_t NumDimSigma = 1;

	using BoxOnSigma = prior::TExponentialFixed<TParameterBase, TypeSigma, NumDimSigma>;
	using SpecSigma  = ParamSpec<TypeSigma, Hash<coretools::toHash("sigma")>, BoxOnSigma>;
	using BoxOnObs   = prior::TDirichletVarInferred<TObservationBase, Type, 1, SpecSigma>;
	using SpecObs    = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnSigma boxOnSigma;
	TParameter<SpecSigma, BoxOnObs> sigma;

	BoxOnObs boxOnObs;

	TDirichletTest(std::string_view Filename) : sigma("sigma", &boxOnSigma, {Filename, "10"}), boxOnObs(&sigma) {}
};

struct TDirichletMeanVarTest {
	using Type                          = coretools::ZeroOpenOneClosed;
	constexpr static size_t NumDimObs   = 2;
	using TypeSigma                     = coretools::StrictlyPositive;
	constexpr static size_t NumDimSigma = 1;
	using TypeP                         = coretools::ZeroOpenOneClosed;
	constexpr static size_t NumDimP     = 1;

	using BoxOnSigma = prior::TExponentialFixed<TParameterBase, TypeSigma, NumDimSigma>;
	using SpecSigma  = ParamSpec<TypeSigma, Hash<coretools::toHash("sigma")>, BoxOnSigma>;
	using BoxOnP     = prior::TDirichletFixed<TParameterBase, TypeP, NumDimP>;
	using SpecP      = ParamSpec<TypeP, Hash<coretools::toHash("p")>, BoxOnP, SumOne<>>;

	using BoxOnObs = prior::TDirichletMeanVarInferred<TObservationBase, Type, NumDimObs, SpecSigma, SpecP>;
	using SpecObs  = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnSigma boxOnSigma;
	TParameter<SpecSigma, BoxOnObs> sigma;

	BoxOnP boxOnP;
	TParameter<SpecP, BoxOnObs> p;

	BoxOnObs boxOnObs;

	TDirichletMeanVarTest(std::string_view Filename, std::string_view AlphasOnP)
		: sigma("sigma", &boxOnSigma, {Filename, "10"}), p("p", &boxOnP, {Filename, AlphasOnP}), boxOnObs(&sigma, &p) {}
};

} // namespace stattools

#endif // STATTOOLS_TDIRICHLETTEST_H
