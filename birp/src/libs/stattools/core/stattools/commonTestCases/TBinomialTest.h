//
// Created by madleina on 18.12.23.
//

#ifndef STATTOOLS_TBINOMIALTEST_H
#define STATTOOLS_TBINOMIALTEST_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorBeta.h"
#include "stattools/Priors/TPriorBinomial.h"
#include "coretools/Types/TStringHash.h"
#include "stattools/ParametersObservations/TObservation.h"

namespace stattools {

struct TBinomialTest {
	using Type                        = coretools::UnsignedInt8WithMax<0>;
	constexpr static size_t NumDimObs = 2;
	using TypeP                       = coretools::ZeroOneOpen;
	constexpr static size_t NumDimP   = 1;

	using BoxOnP   = prior::TBetaFixed<TParameterBase, TypeP, NumDimP>;
	using SpecP    = ParamSpec<TypeP, Hash<coretools::toHash("p")>, BoxOnP>;
	using BoxOnObs = prior::TBinomialInferred<TObservationBase, Type, 2, SpecP>;
	using SpecObs  = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnP boxOnP;
	TParameter<SpecP, BoxOnObs> p;

	BoxOnObs boxOnObs;

	TBinomialTest(std::string_view Filename) : p("p", &boxOnP, {Filename, "0.7,0.7"}), boxOnObs(&p) {}
};

} // namespace stattools

#endif // STATTOOLS_TBINOMIALTEST_H
