//
// Created by madleina on 22.01.21.
//

#ifndef TOBSERVATION_H
#define TOBSERVATION_H

#include "stattools/DAG/TDAGBuilder.h"
#include "stattools/ParametersObservations/TNodeBase.h"
#include "stattools/ParametersObservations/TNodeTyped.h"

namespace stattools {

//-------------------------
// TObservation
//-------------------------
template<typename Type, size_t NumDim, typename TypeBoxAbove>
class TObservation : public TNodeTyped<TObservationBase, Type, NumDim, TypeBoxAbove> {
	static_assert(!TypeBoxAbove::isDeterministic);

private:
	// using
	using Base = TNodeTyped<TObservationBase, Type, NumDim, TypeBoxAbove>;
	using Base::_boxAbove;
	using Base::_storage;
	using Storage = coretools::TMultiDimensionalStorage<Type, NumDim>;

	// definition
	TObservationDefinition _def;

public:
	TObservation(std::string_view Name, TypeBoxAbove *BoxAbove, const Storage &Data, const TObservationDefinition &Def)
		: TNodeTyped<TObservationBase, Type, NumDim, TypeBoxAbove>(Name, BoxAbove, Def), _def(Def) {
		_storage = Data;

		// add to DAGBuilder
		instances::dagBuilder().addToDAG(this);
	}

	~TObservation() override = default;

	TObservationDefinition &getDefinition() override { return _def; }

	template<typename IndexType = size_t> [[nodiscard]] double getDensity(IndexType i = 0) const noexcept {
		return _boxAbove->getDensity(this->storage(), this->_getIndex(i));
	}

	template<typename IndexType = size_t> [[nodiscard]] double getLogDensity(IndexType i = 0) const noexcept {
		return _boxAbove->getLogDensity(this->storage(), this->getIndex(i));
	}
};

} // end namespace stattools

#endif // TOBSERVATION_H
