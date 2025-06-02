//
// Created by madleina on 12.05.20.
//

#ifndef MCMCFRAMEWORK_TESTCASE_H
#define MCMCFRAMEWORK_TESTCASE_H

#include "coretools/Distances/TDistances.h"
#include "stattools/ParametersObservations/TObservation.h"
#include "stattools/ParametersObservations/TValue.h"
#include "stattools/Priors/TPriorBase.h"

namespace testing {
// function that creates and initializes a parameter
// useful for unit-tests

template<typename T, size_t NumDim>
auto createUpdatedStorage(const coretools::TMultiDimensionalStorage<T, NumDim> &StorageObs) {
	coretools::TMultiDimensionalStorage<stattools::TValueUpdated<T>, NumDim> storage(StorageObs.dimensions());
	for (size_t i = 0; i < StorageObs.size(); ++i) { storage[i] = StorageObs[i]; }
	return storage;
}

template<typename T, size_t NumDim, typename BoxAbove, typename Underlying = typename T::value_type>
auto createObservation(std::string_view Name, BoxAbove *Prior, const std::array<size_t, NumDim> &Dimensions,
					   const stattools::TObservationDefinition &Def, const std::vector<Underlying> &Values = {}) {
	// create observation
	coretools::TMultiDimensionalStorage<T, NumDim> storage(Dimensions);
	auto obs = std::make_unique<stattools::TObservation<T, NumDim, BoxAbove>>(Name, Prior, storage, Def);

	// create dimension names
	std::array<std::shared_ptr<coretools::TNamesEmpty>, NumDim> dimNames;
	for (size_t d = 0; d < NumDim; d++) { dimNames[d] = std::make_shared<coretools::TNamesIndices>(); }
	obs->storage().setDimensionNames(dimNames);

	// initialize storage
	obs->tellBoxAboveToInitStorage();

	if (!Values.empty()) {
		// fill values
		for (size_t i = 0; i < Values.size(); i++) { (obs->storage())[i] = T(Values[i]); }
	}

	return obs;
}

std::unique_ptr<coretools::TDistancesBinned<uint8_t>> createAndFillDistances(size_t Size, size_t MaxDist);
std::unique_ptr<coretools::TDistancesBinned<uint8_t>> createAndFillDistances(std::string_view Filename,
																			   size_t MaxDist);

template<typename T> std::vector<T> readSingleLineIntoVec(std::string_view Filename) {
	std::vector<T> vec;
	const std::string full = std::string{coretools::str::readBeforeLast(__FILE__, "stattools/core")}
								 .append("stattools/tests/unittests/")
								 .append(Filename);
	coretools::TInputFile file(full, coretools::FileType::NoHeader);
	for (size_t i = 0; i < file.numCols(); ++i) {
		vec.push_back(file.get<T>(i));
	}
	file.popFront();

	if (!file.empty()) { DEVERROR("File ", Filename, " does not have one line!"); }
	return vec;
}

template<typename TypeObs, size_t NumDimObs>
struct DummyBox : public stattools::prior::TStochasticBase<stattools::TObservationBase, TypeObs, NumDimObs> {
private:
	void _add(size_t i, std::vector<size_t> &Vec) {
		if (i >= Vec.size()) { Vec.resize(i + 1, 0); }
		++Vec[i];
	}

public:
	using Base = stattools::prior::TStochasticBase<stattools::TObservationBase, TypeObs, NumDimObs>;
	using typename Base::Storage;
	using typename Base::UpdatedStorage;

	std::vector<size_t> updateCounts;
	std::vector<size_t> updateTmpValsCounts;
	std::vector<size_t> accepted;
	std::vector<size_t> rejected;

	// dummy example for a BoxType
	template<typename TypeParam> auto calculateLLRatio(TypeParam *, size_t i) {
		_add(i, updateCounts);
		return [](coretools::TMultiDimensionalStorage<TypeObs, NumDimObs> *) { return 0.0; };
	}

	template<typename TypeParam> void updateTempVals(TypeParam *, size_t i, bool Accepted) {
		_add(i, updateTmpValsCounts);
		if (Accepted) {
			_add(i, accepted);
		} else {
			_add(i, rejected);
		}
	};

	void clear() {
		updateCounts.clear();
		updateTmpValsCounts.clear();
		accepted.clear();
		rejected.clear();
	}

	// override pure virtual functions of TBase
	void _simulateUnderPrior(Storage *) override{};
	std::string name() const override { return ""; };
	double getDensity(const Storage &, size_t) const override { return 0.0; };
	double getLogDensityRatio(const UpdatedStorage &, size_t) const override { return 0.0; };
};

} // namespace testing

#endif // MCMCFRAMEWORK_TESTCASE_H
