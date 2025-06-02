//
// Created by madleina on 12.12.23.
//

#ifndef STATTOOLS_INDEXPICKERTRAITS_H
#define STATTOOLS_INDEXPICKERTRAITS_H

#include "stattools/ParametersObservations/spec.h"
#include "stattools/Updates/TUpdateJoint.h"
#include "stattools/Updates/TUpdatePair.h"
#include "stattools/Updates/TUpdateSingle.h"

namespace stattools {

//----------------------------------
// Helper functions
//----------------------------------

namespace impl {

// Figure out at compile time which class to use for picking indices to update
// depends on 1) constraints, 2) update types and 3) update weights

template<UpdateWeights... Weights> constexpr static bool allRegular() {
	return (true && ... && (Weights == UpdateWeights::regular));
}

template<size_t NumDim, typename Constraint, UpdateWeights... Weights> auto getTypeConstrainedRegular() {
	static_assert(Constraint::constraint != Constraints::unconstrained);
	static_assert(allRegular<Weights...>());
	static_assert(Constraint::updateType != UpdateTypes::one);

	if constexpr (Constraint::updateType == UpdateTypes::pair) {
		return TUpdatePair<NumDim, Constraint::alongDim, Weights...>{};
	} else if constexpr (Constraint::updateType == UpdateTypes::joint) {
		return TUpdateJoint<NumDim, Constraint::alongDim, Weights...>{};
	}
}

template<size_t NumDim, typename Constraint, UpdateWeights... Weights> auto getTypeConstrainedWeighted() {
	static_assert(Constraint::constraint != Constraints::unconstrained);
	static_assert(!allRegular<Weights...>());
	static_assert(Constraint::updateType != UpdateTypes::one);
	static_assert(Constraint::updateType != UpdateTypes::joint); // currently not allowed

	if constexpr (Constraint::updateType == UpdateTypes::pair) {
		return TUpdatePairWeighted<NumDim, Constraint::alongDim, Weights...>{};
	}
}

template<size_t NumDim, UpdateWeights... Weights> auto getTypeUnconstrained() {
	if constexpr (allRegular<Weights...>()) {
		return TUpdateSingle<NumDim, Weights...>{};
	} else {
		return TUpdateSingleWeighted<NumDim, Weights...>{};
	}
}

template<size_t NumDim, typename Constraint, UpdateWeights... Weights> auto getTypeConstrained() {
	if constexpr (allRegular<Weights...>()) {
		return getTypeConstrainedRegular<NumDim, Constraint, Weights...>();
	} else {
		return getTypeConstrainedWeighted<NumDim, Constraint, Weights...>();
	}
}

template<typename Spec, std::size_t... S> constexpr auto _getIndexPickerType(std::index_sequence<S...>) {
	using constraint = typename Spec::constraint;

	if constexpr (constraint::constraint == Constraints::unconstrained) {
		return impl::getTypeUnconstrained<Spec::numDim, Spec::weights[S]...>();
	} else {
		return impl::getTypeConstrained<Spec::numDim, constraint, Spec::weights[S]...>();
	}
}

template<typename Spec, std::size_t... S> constexpr auto _allWeightsAreRegular(std::index_sequence<S...>) {
	return impl::allRegular<Spec::weights[S]...>();
}

template<typename Spec> constexpr auto getIndexPickerType() {
	return impl::_getIndexPickerType<Spec>(std::make_index_sequence<Spec::numDim>());
}

template<typename Spec> constexpr auto allWeightsAreRegular() {
	return impl::_allWeightsAreRegular<Spec>(std::make_index_sequence<Spec::numDim>());
}

} // namespace impl

};     // namespace stattools
#endif // STATTOOLS_INDEXPICKERTRAITS_H
