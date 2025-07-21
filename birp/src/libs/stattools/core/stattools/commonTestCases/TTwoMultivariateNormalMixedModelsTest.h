//
// Created by madleina on 19.12.23.
//

#ifndef STATTOOLS_TTWOMULTIVARIATENORMALMIXEDMODELSTEST_H
#define STATTOOLS_TTWOMULTIVARIATENORMALMIXEDMODELSTEST_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorChisq.h"
#include "stattools/Priors/TPriorExponential.h"
#include "stattools/Priors/TPriorNormal.h"
#include "stattools/Priors/TPriorTwoMultivariateNormalsMixedModel.h"
#include "stattools/ParametersObservations/TObservation.h"
#include "coretools/Types/TStringHash.h"

namespace stattools {

template<typename BoxOnZ> struct TTwoMulativariateNormalMixedModelsTest {
	using Type                        = coretools::Unbounded;
	using TypeZ                       = coretools::Boolean;
	using TypeMu                      = coretools::Unbounded;
	using TypeM                       = coretools::StrictlyPositive;
	using TypeMrr                     = coretools::StrictlyPositive;
	using TypeMrs                     = coretools::Unbounded;
	using TypeRho                     = coretools::StrictlyPositive;
	constexpr static size_t NumDim    = 1;
	constexpr static size_t NumDimObs = 2;

	using BoxOnMu  = prior::TNormalFixed<TParameterBase, TypeMu, NumDim>;
	using BoxOnM   = prior::TExponentialFixed<TParameterBase, TypeM, NumDim>;
	using BoxOnMrr = prior::TMultivariateChiFixed<TParameterBase, TypeMrr, NumDim>;
	using BoxOnMrs = prior::TNormalFixed<TParameterBase, TypeMrs, NumDim>;
	using BoxOnRho = prior::TExponentialFixed<TParameterBase, TypeRho, NumDim>;

	using SpecZ    = ParamSpec<TypeZ, Hash<coretools::toHash("z")>, BoxOnZ, Parallelize<MarkovOrder::different>>;
	using SpecMu   = ParamSpec<TypeMu, Hash<coretools::toHash("mu")>, BoxOnMu>;
	using SpecM    = ParamSpec<TypeM, Hash<coretools::toHash("m")>, BoxOnM>;
	using SpecMrr  = ParamSpec<TypeMrr, Hash<coretools::toHash("mrr")>, BoxOnMrr>;
	using SpecMrs  = ParamSpec<TypeMrs, Hash<coretools::toHash("mrs")>, BoxOnMrs>;
	using SpecRho  = ParamSpec<TypeRho, Hash<coretools::toHash("rho")>, BoxOnRho>;
	using BoxOnObs = prior::TTwoMultivariateNormalsMixedModelInferred<TObservationBase, Type, NumDimObs, SpecZ, SpecMu,
																	  SpecM, SpecMrr, SpecMrs, SpecRho>;
	using SpecObs  = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnMu boxOnMu;
	BoxOnM boxOnM;
	BoxOnMrr boxOnMrr;
	BoxOnMrs boxOnMrs;
	BoxOnRho boxOnRho;

	TParameter<SpecZ, BoxOnObs> z;
	TParameter<SpecMu, BoxOnObs> mu;
	TParameter<SpecM, BoxOnObs> m;
	TParameter<SpecMrr, BoxOnObs> Mrr;
	TParameter<SpecMrs, BoxOnObs> Mrs;
	TParameter<SpecRho, BoxOnObs> rho;

	BoxOnObs boxOnObs;

	TTwoMulativariateNormalMixedModelsTest(std::string_view Filename, BoxOnZ *boxOnZ)
		: z("z", boxOnZ, {Filename}, {1}), mu("mu", &boxOnMu, {Filename, "0,0.1"}), m("m", &boxOnM, {Filename, "5"}),
		  Mrr("Mrr", &boxOnMrr, {Filename, "3,2,1"}), Mrs("Mrs", &boxOnMrs, {Filename, "0,1"}),
		  rho("rho", &boxOnRho, {Filename, "0.5"}), boxOnObs(&z, &mu, &m, &Mrr, &Mrs, &rho) {}
};

} // namespace stattools

#endif // STATTOOLS_TTWOMULTIVARIATENORMALMIXEDMODELSTEST_H
