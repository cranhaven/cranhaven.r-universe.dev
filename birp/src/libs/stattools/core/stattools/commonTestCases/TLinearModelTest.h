//
// Created by madleina on 20.12.23.
//

#ifndef STATTOOLS_TLINEARMODELTEST_H
#define STATTOOLS_TLINEARMODELTEST_H

#include "stattools/Deterministics/TLinearModel.h"
#include "stattools/Deterministics/TLinkFunctions.h"
#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorBernoulli.h"
#include "stattools/ParametersObservations/TObservation.h"
#include "coretools/Types/TStringHash.h"

namespace stattools {

template<bool Intercept, bool BelowIsObservation, class BoxOnBeta> struct TLinearModelTest {
	using TypeBeta                      = coretools::Unbounded;
	using TypeX                         = coretools::Unbounded;
	using TypeY                         = coretools::Unbounded;
	using TypeP                        = coretools::ZeroOneOpen;
	using TypeBelow                     = coretools::Boolean;
	constexpr static size_t NumDimBeta  = 2;
	constexpr static size_t NumDimX     = 2;
	constexpr static size_t NumDimY     = 2;
	constexpr static size_t NumDimP    = 2;
	constexpr static size_t NumDimBelow = 2;

	using SpecBeta = ParamSpec<TypeBeta, Hash<coretools::toHash("beta")>, BoxOnBeta, NumDim<NumDimBeta>>;
	using StorageX = coretools::TMultiDimensionalStorage<TypeX, NumDimX>;
	using BoxOnY   = det::TLinearModel<TParameterBase, TypeY, NumDimY, SpecBeta, TypeX, Intercept>;
	using SpecY    = ParamSpec<TypeY, Hash<coretools::toHash("y")>, BoxOnY, NumDim<NumDimY>>;
	using BoxOnP  = det::TLogistic<TParameterBase, TypeP, NumDimP, SpecY>;
	using SpecP   = ParamSpec<TypeP, Hash<coretools::toHash("p")>, BoxOnP, NumDim<NumDimP>>;

	using BelowType  = std::conditional_t<BelowIsObservation, TObservationBase, TParameterBase>;
	using BoxOnBelow = prior::TBernoulliPerElementInferred<BelowType, TypeBelow, NumDimBelow, SpecP>;
	using SpecObs    = TObservation<TypeBelow, NumDimBelow, BoxOnBelow>;

	TParameter<SpecBeta, BoxOnY> beta;

	StorageX x;

	BoxOnY boxOnY;
	TParameter<SpecY, BoxOnP> y;

	BoxOnP boxOnPi;
	TParameter<SpecP, BoxOnBelow> p;

	BoxOnBelow boxOnBelow;

	TLinearModelTest(std::string_view Filename, const std::array<size_t, 2> &DimX, BoxOnBeta *Box)
		: beta("beta", Box, {Filename, "0,1"}), boxOnY(&beta, &x), y("y", &boxOnY, {Filename}), boxOnPi(&y),
		  p("p", &boxOnPi, {Filename}), boxOnBelow(&p) {
		x.resize(DimX);
	}
};

} // namespace stattools

#endif // STATTOOLS_TLINEARMODELTEST_H
