//
// Created by madleina on 19.12.23.
//

#ifndef STATTOOLS_THMMLADDERTEST_H
#define STATTOOLS_THMMLADDERTEST_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorExponential.h"
#include "stattools/Priors/TPriorHMMLadder.h"
#include "stattools/ParametersObservations/TObservation.h"
#include "coretools/Types/TStringHash.h"

namespace stattools {

struct THMMLadderTest {
	using Type                          = coretools::UnsignedInt8WithMax<0>;
	using TypeKappa                     = coretools::StrictlyPositive;
	constexpr static size_t NumDimObs   = 1;
	constexpr static size_t NumDimKappa = 1;

	using BoxOnKappa = prior::TExponentialFixed<TParameterBase, TypeKappa, NumDimKappa>;

	using SpecKappa = ParamSpec<TypeKappa, Hash<coretools::toHash("kappa")>, BoxOnKappa>;
	using BoxOnObs  = prior::THMMLadderInferred<TObservationBase, Type, 1, SpecKappa>;
	using SpecObs   = TObservation<Type, NumDimObs, BoxOnObs>;

	BoxOnKappa boxOnKappa;
	TParameter<SpecKappa, BoxOnObs> kappa;

	BoxOnObs boxOnObs;

	THMMLadderTest(std::string_view Filename, size_t K, coretools::TDistancesBinnedBase *Distances)
		: kappa("kappa", &boxOnKappa, {Filename, "1."}), boxOnObs(&kappa, K, Distances) {

		Type::setMax(K - 1);
	}
};

} // namespace stattools

#endif // STATTOOLS_THMMLADDERTEST_H
