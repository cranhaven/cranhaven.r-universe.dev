//
// Created by madleina on 16.06.21.
//

#ifndef TPARAMETEROBSERVATIONTYPEDBASE_H
#define TPARAMETEROBSERVATIONTYPEDBASE_H

#include <memory>
#include <string>
#include <vector>

#include "coretools/Storage/TStorage.h"
#include "stattools/DAG/TDAG.h"
#include "stattools/ParametersObservations/TValue.h"

//-------------------------------------------
// TNodeTyped
//-------------------------------------------

namespace stattools {

template<typename Derived, typename Type, size_t NumDim, typename TypeBoxAbove> class TNodeTyped : public Derived {
public:
	static constexpr bool isUpdated = std::is_same_v<Derived, TParameterBase>;
	using StorageType               = std::conditional_t<isUpdated, TValueUpdated<Type>, Type>;
	using Storage                   = coretools::TMultiDimensionalStorage<StorageType, NumDim>;

	static_assert(std::is_same_v<typename TypeBoxAbove::type, Type>);
	static_assert(TypeBoxAbove::numDim == NumDim);

private:
	std::string _name;

protected:
	// data
	Storage _storage;

	// ptr to box above (prior / deterministic)
	// note: must be a ptr because potentially multiple parameters share a box above
	TypeBoxAbove *_boxAbove;

	void _writeToSimulation(coretools::TOutputMaybeRcppFile &File) const {
		for (size_t i = 0; i < this->size(); ++i) { File << getFullName(i) << (Type)_storage[i] << coretools::endl; }
	}

public:
	TNodeTyped(std::string_view Name, TypeBoxAbove *BoxAbove, const TObservationDefinition &Def)
		: _name(Name), _boxAbove(std::move(BoxAbove)) {
		_boxAbove->setFixedPriorParameters(Def.priorParameters());
		_boxAbove->addBelow(this, &_storage);
	}
	~TNodeTyped() override = default;

	// getters
	[[nodiscard]] const std::string &name() const override { return _name; };
	[[nodiscard]] std::string prior() const { return _boxAbove->name(); };

	const Storage &storage() const { return _storage; }
	Storage &storage() { return _storage; }

	void constructDAG(TDAG &DAG, TDAG &temporaryDAG) override {
		temporaryDAG.add(this);
		_boxAbove->constructDAG(DAG, temporaryDAG);
	}

	void tellBoxAboveToInitStorage() override { _boxAbove->initialize(); }
	void tellBoxAboveThatBurninFinished() override { _boxAbove->burninHasFinished(); };
	void tellBoxAboveThatMCMCFinished() override { _boxAbove->MCMCHasFinished(); };

	// intervals
	constexpr Type min() const noexcept { return Type::min(); };
	constexpr Type max() const noexcept { return Type::max(); };

	// values, indexing and dimensions
	Type value(size_t i = 0) const { return (Type)_storage[i]; };
	size_t getIndex(const std::array<size_t, NumDim> &coord) const { return _storage.getIndex(coord); };
	size_t getIndex(size_t i) const { return i; };
	Type value(const std::array<size_t, NumDim> &coord) const { return (Type)_storage[coord]; };
	coretools::TRange getRange(const std::array<size_t, NumDim> &startCoord,
							   const std::array<size_t, NumDim> &endCoord) const {
		return _storage.getRange(startCoord, endCoord);
	};
	coretools::TRange getFull() const { return _storage.getFull(); };
	coretools::TRange getDiagonal() const { return _storage.getDiagonal(); };
	coretools::TRange get1DSlice(size_t dim, const std::array<size_t, NumDim> &startCoord) const {
		return _storage.get1DSlice(dim, startCoord);
	};
	std::array<size_t, NumDim> dimensions() const { return _storage.dimensions(); };
	size_t size() const { return _storage.size(); };
	std::array<size_t, NumDim> getSubscripts(size_t linearIndex) const { return _storage.getSubscripts(linearIndex); };
	const std::shared_ptr<coretools::TNamesEmpty> &getDimensionName(size_t Dim) const {
		return _storage.getDimensionName(Dim);
	};

	// other operations
	double sum(const coretools::TRange &Range) const noexcept {
		double sum = 0.0;
		for (size_t i = Range.begin; i < Range.end; i += Range.increment) { sum += (double)(Type)_storage[i]; }
		return sum;
	}
	double sum() const noexcept { return sum(_storage.getFull()); }

	double vectorNorm(const coretools::TRange &Range) const noexcept {
		double sum = 0.0;
		for (size_t i = Range.begin; i < Range.end; i += Range.increment) {
			const auto v = (double)(Type)_storage[i];
			sum += v * v;
		}
		return sqrt(sum);
	}
	double vectorNorm() const noexcept { return sum(_storage.getFull()); }

	Type max_value() const { return (Type)*_storage.max_element(); }
	Type min_value() const { return (Type)*_storage.min_element(); }

	size_t ix_max_element() const { return _storage.ix_max_element(); }
	size_t ix_min_element() const { return _storage.ix_min_element(); }

	// prior
	void guessInitialValues() override { _boxAbove->guessInitialValues(); }

	double getSumLogPriorDensity() const override { return _boxAbove->getSumLogPriorDensity(storage()); }

	// set
	virtual void set(size_t i, Type Value) {
		assert(i < _storage.size());
		_storage[i] = Value;
	}
	virtual void set(const std::array<size_t, NumDim> &coords, Type Value) { return set(getIndex(coords), Value); }

	void set(Type Value) { set(0, Value); }

	void set(const std::vector<Type> &Container) {
		if (Container.size() != size()) {
			throw coretools::TDevError("Size of container (", Container.size(), ") does not match size of parameter (", size(), ").");
		}
		for (size_t i = 0; i < size(); ++i) { set(i, Container[i]); }
	}

	// write values
	void fillNames(std::vector<std::string> &header) const override {
		_storage.appendToVectorOfAllFullDimensionNamesWithPrefix(header, _name);
	}

	std::string getFullName(size_t i) const { return _storage.getFullDimensionNameWithPrefix(i, this->name()); }

	void writeToTrace(coretools::TOutputMaybeRcppFile &File) const override {
		for (size_t i = 0; i < this->size(); ++i) { File << (Type)_storage[i]; }
	}

	void writeToSummary(MCMCFiles FileType, coretools::TOutputMaybeRcppFile &File) const override {
		if (FileType == MCMCFiles::trace) {
			writeToTrace(File);
		} else if (FileType == MCMCFiles::simulation) {
			_writeToSimulation(File);
		}
	}

	// simulate values under prior distribution
	void simulateUnderPrior() override { _boxAbove->simulateUnderPrior(); }
};

} // end namespace stattools

#endif // TPARAMETEROBSERVATIONTYPEDBASE_H
