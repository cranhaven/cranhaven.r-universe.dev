//
// Created by madleina on 19.12.23.
//

#ifndef STATTOOLS_TNORMALMIXEDMODELSTRETCHTEST_H
#define STATTOOLS_TNORMALMIXEDMODELSTRETCHTEST_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorExponential.h"
#include "stattools/Priors/TPriorNormal.h"
#include "stattools/Priors/TPriorTwoNormalsMixedModel.h"
#include "stattools/ParametersObservations/TObservation.h"
#include "coretools/Types/TStringHash.h"

namespace stattools {

template<typename BoxOnZ, bool BelowIsObservation, size_t NumDimObs> struct TTwoNormalMixedModelsTest {
	using Type                          = coretools::Unbounded;
	using TypeZ                         = coretools::Boolean;
	using TypeMu                        = coretools::Unbounded;
	using TypeVar0                      = coretools::StrictlyPositive;
	using TypeVar1                      = coretools::StrictlyPositive;
	constexpr static size_t NumDimOther = 1;
	constexpr static size_t numDimObs   = NumDimObs;

	using BoxOnMu   = prior::TNormalFixed<TParameterBase, TypeMu, NumDimOther>;
	using BoxOnVar0 = prior::TExponentialFixed<TParameterBase, TypeVar0, NumDimOther>;
	using BoxOnVar1 = prior::TExponentialFixed<TParameterBase, TypeVar1, NumDimOther>;

	using SpecZ =
		ParamSpec<TypeZ, Hash<coretools::toHash("z")>, BoxOnZ, NumDim<NumDimObs>, Parallelize<MarkovOrder::different>>;
	using SpecMu    = ParamSpec<TypeMu, Hash<coretools::toHash("mu")>, BoxOnMu>;
	using SpecVar0  = ParamSpec<TypeVar0, Hash<coretools::toHash("var0")>, BoxOnVar0>;
	using SpecVar1  = ParamSpec<TypeVar1, Hash<coretools::toHash("var1")>, BoxOnVar1>;
	using BelowType = std::conditional_t<BelowIsObservation, TObservationBase, TParameterBase>;
	using BoxOnObs =
		prior::TTwoNormalsMixedModelInferred<BelowType, Type, NumDimObs, SpecMu, SpecVar0, SpecVar1, SpecZ>;
	using SpecObs = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnMu boxOnMu;
	BoxOnVar0 boxOnVar0;
	BoxOnVar1 boxOnVar1;

	TParameter<SpecZ, BoxOnObs> z;
	TParameter<SpecMu, BoxOnObs> mu;
	TParameter<SpecVar0, BoxOnObs> var0;
	TParameter<SpecVar1, BoxOnObs> var1;

	BoxOnObs boxOnObs;

	TTwoNormalMixedModelsTest(std::string_view Filename, BoxOnZ *boxOnZ, const std::string &NameZ = "z",
							  const std::string &Prefix = "")
		: z(NameZ, boxOnZ, {Filename}, {1}), mu(Prefix + "mu", &boxOnMu, {Filename, "0,1"}),
		  var0(Prefix + "var0", &boxOnVar0, {Filename, "10"}), var1(Prefix + "var1", &boxOnVar1, {Filename, "0.1"}),
		  boxOnObs(&z, &mu, &var0, &var1) {}
};

} // namespace stattools

#endif // STATTOOLS_TNORMALMIXEDMODELSTRETCHTEST_H
