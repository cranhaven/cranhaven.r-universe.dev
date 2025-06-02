//
// Created by madleina on 18.12.23.
//

#ifndef STATTOOLS_TCATEGORICALTEST_H
#define STATTOOLS_TCATEGORICALTEST_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorCategorical.h"
#include "stattools/Priors/TPriorDirichlet.h"
#include "coretools/Types/TStringHash.h"
#include "stattools/ParametersObservations/TObservation.h"

namespace stattools {

struct TCategoricalTest {
	using Type                        = coretools::UnsignedInt8WithMax<0>;
	constexpr static size_t NumDimObs = 1;
	using TypePi                      = coretools::ZeroOpenOneClosed;
	constexpr static size_t NumDimPi  = 1;

	using BoxOnPi  = prior::TDirichletFixed<TParameterBase, TypePi, NumDimPi>;
	using SpecPi   = ParamSpec<TypePi, Hash<coretools::toHash("pi")>, BoxOnPi, SumOne<0>>;
	using BoxOnObs = prior::TCategoricalInferred<TObservationBase, Type, 1, SpecPi>;
	using SpecObs  = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnPi boxOnPi;
	TParameter<SpecPi, BoxOnObs> pi;

	BoxOnObs boxOnObs;

	TCategoricalTest(std::string_view Filename, size_t K, std::string_view PriorParams)
		: pi("pis", &boxOnPi, {Filename, PriorParams}), boxOnObs(&pi, K) {
		Type::setMax(K - 1);
	}
};

} // namespace stattools

#endif // STATTOOLS_TCATEGORICALTEST_H
