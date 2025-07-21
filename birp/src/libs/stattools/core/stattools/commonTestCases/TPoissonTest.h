//
// Created by madleina on 18.12.23.
//

#ifndef STATTOOLS_TPOISSONTEST_H
#define STATTOOLS_TPOISSONTEST_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorExponential.h"
#include "stattools/Priors/TPriorPoisson.h"
#include "stattools/ParametersObservations/TObservation.h"
#include "coretools/Types/TStringHash.h"

namespace stattools {

struct TPoissonTest {
	using Type                           = coretools::UnsignedInt8;
	constexpr static size_t NumDimObs    = 1;
	using TypeLambda                     = coretools::StrictlyPositive;
	constexpr static size_t NumDimLambda = 1;

	using BoxOnLambda = prior::TExponentialFixed<TParameterBase, TypeLambda, NumDimLambda>;
	using SpecLambda  = ParamSpec<TypeLambda, Hash<coretools::toHash("lambda")>, BoxOnLambda>;
	using BoxOnObs    = prior::TPoissonInferred<TObservationBase, Type, 1, SpecLambda>;
	using SpecObs     = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnLambda boxOnLambda;
	TParameter<SpecLambda, BoxOnObs> lambda;

	BoxOnObs boxOnObs;

	TPoissonTest(std::string_view Filename) : lambda("lambda", &boxOnLambda, {Filename, "1"}), boxOnObs(&lambda) {}
};

} // namespace stattools

#endif // STATTOOLS_TPOISSONTEST_H
