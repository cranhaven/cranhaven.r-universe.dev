//
// Created by madleina on 18.12.23.
//

#ifndef STATTOOLS_TNEGATIVEBINOMIALTEST_H
#define STATTOOLS_TNEGATIVEBINOMIALTEST_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorBeta.h"
#include "stattools/Priors/TPriorNegativeBinomial.h"
#include "stattools/ParametersObservations/TObservation.h"
#include "coretools/Types/TStringHash.h"

namespace stattools {

struct TNegativeBinomialTest {
	using Type                        = coretools::UnsignedInt16;
	constexpr static size_t NumDimObs = 2;
	using TypeP                       = coretools::ZeroOneOpen;
	constexpr static size_t NumDimP   = 1;

	using BoxOnP   = prior::TBetaFixed<TParameterBase, TypeP, NumDimP>;
	using SpecP    = ParamSpec<TypeP, Hash<coretools::toHash("p")>, BoxOnP>;
	using BoxOnObs = prior::TNegativeBinomialInferred<TObservationBase, Type, NumDimObs, SpecP>;
	using SpecObs  = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnP boxOnP;
	TParameter<SpecP, BoxOnObs> p;

	BoxOnObs boxOnObs;

	TNegativeBinomialTest(std::string_view Filename) : p("p", &boxOnP, {Filename, "0.7,0.7"}), boxOnObs(&p) {}
};

} // namespace stattools

#endif // STATTOOLS_TNEGATIVEBINOMIALTEST_H
