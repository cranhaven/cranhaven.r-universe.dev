//
// Created by madleina on 18.12.23.
//

#ifndef STATTOOLS_THMMBOOLGENERATINGTEST_H
#define STATTOOLS_THMMBOOLGENERATINGTEST_H

#include "coretools/Types/old/weakTypesWithLogExp.h"
#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorHMMBoolGenerating.h"
#include "stattools/Priors/TPriorUniform.h"
#include "stattools/ParametersObservations/TObservation.h"
#include "coretools/Types/TStringHash.h"

namespace stattools {

struct THMMBoolGeneratingTest {
	using Type                              = coretools::Boolean;
	constexpr static size_t NumDimObs       = 1;
	using TypeLogLambda                     = coretools::WeakTypeWithExp<double>;
	constexpr static size_t NumDimLogLambda = 1;

	using BoxOnLogLambda = prior::TUniformFixed<TParameterBase, TypeLogLambda, NumDimLogLambda>;

	using SpecLogLambda1 = ParamSpec<TypeLogLambda, Hash<coretools::toHash("log_lambda_1")>, BoxOnLogLambda>;
	using SpecLogLambda2 = ParamSpec<TypeLogLambda, Hash<coretools::toHash("log_lambda_2")>, BoxOnLogLambda>;
	using BoxOnObs = prior::THMMBoolGeneratingMatrixInferred<TObservationBase, Type, 1, SpecLogLambda1, SpecLogLambda2>;
	using SpecObs  = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnLogLambda boxOnLogLambda1;
	BoxOnLogLambda boxOnLogLambda2;

	TParameter<SpecLogLambda1, BoxOnObs> log_lambda_1;
	TParameter<SpecLogLambda2, BoxOnObs> log_lambda_2;

	BoxOnObs boxOnObs;

	THMMBoolGeneratingTest(std::string_view Filename, coretools::TDistancesBinnedBase *Distances)
		: log_lambda_1("log_lambda_1", &boxOnLogLambda1, {Filename}),
		  log_lambda_2("log_lambda_2", &boxOnLogLambda2, {Filename}),
		  boxOnObs(&log_lambda_1, &log_lambda_2, Distances) {}
};

} // namespace stattools

#endif // STATTOOLS_THMMBOOLGENERATINGTEST_H
