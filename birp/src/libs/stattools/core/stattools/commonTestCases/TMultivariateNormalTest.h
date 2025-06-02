//
// Created by madleina on 19.12.23.
//

#ifndef STATTOOLS_TMULTIVARIATENORMALTEST_H
#define STATTOOLS_TMULTIVARIATENORMALTEST_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorChisq.h"
#include "stattools/Priors/TPriorExponential.h"
#include "stattools/Priors/TPriorMultivariateNormal.h"
#include "stattools/Priors/TPriorNormal.h"
#include "stattools/ParametersObservations/TObservation.h"
#include "coretools/Types/TStringHash.h"

namespace stattools {

struct TMultivariateNormalTest {
	using Type                        = coretools::Unbounded;
	using TypeMu                      = coretools::Unbounded;
	using TypeM                       = coretools::StrictlyPositive;
	using TypeMrr                     = coretools::StrictlyPositive;
	using TypeMrs                     = coretools::Unbounded;
	constexpr static size_t NumDimObs = 2;
	constexpr static size_t NumDim    = 1;

	using BoxOnMu  = prior::TNormalFixed<TParameterBase, TypeMu, NumDim>;
	using BoxOnM   = prior::TExponentialFixed<TParameterBase, TypeM, NumDim>;
	using BoxOnMrr = prior::TMultivariateChiFixed<TParameterBase, TypeMrr, NumDim>;
	using BoxOnMrs = prior::TNormalFixed<TParameterBase, TypeMrs, NumDim>;

	using SpecMu  = ParamSpec<TypeMu, Hash<coretools::toHash("mu")>, BoxOnMu>;
	using SpecM   = ParamSpec<TypeM, Hash<coretools::toHash("m")>, BoxOnM, Unconstrained<>>;
	using SpecMrr = ParamSpec<TypeMrr, Hash<coretools::toHash("mrr")>, BoxOnMrr>;
	using SpecMrs = ParamSpec<TypeMrs, Hash<coretools::toHash("mrs")>, BoxOnMrs>;
	using BoxOnObs =
		prior::TMultivariateNormalInferred<TObservationBase, Type, NumDimObs, SpecMu, SpecM, SpecMrr, SpecMrs>;
	using SpecObs = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnMu boxOnMu;
	BoxOnM boxOnM;
	BoxOnMrr boxOnMrr;
	BoxOnMrs boxOnMrs;

	TParameter<SpecMu, BoxOnObs> mu;
	TParameter<SpecM, BoxOnObs> m;
	TParameter<SpecMrr, BoxOnObs> Mrr;
	TParameter<SpecMrs, BoxOnObs> Mrs;

	BoxOnObs boxOnObs;

	TMultivariateNormalTest(std::string_view Filename)
		: mu("mu", &boxOnMu, {Filename, "0,1"}), m("m", &boxOnM, {Filename, "5"}),
		  Mrr("Mrr", &boxOnMrr, {Filename, "3,2,1"}), Mrs("Mrs", &boxOnMrs, {Filename, "0,1"}),
		  boxOnObs(&mu, &m, &Mrr, &Mrs) {}
};

} // namespace stattools

#endif // STATTOOLS_TMULTIVARIATENORMALTEST_H
