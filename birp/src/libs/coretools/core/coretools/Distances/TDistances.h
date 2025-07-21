//
// Created by madleina on 07.01.21.
//

#ifndef TDISTANCES_H
#define TDISTANCES_H

#include "coretools/Main/TError.h"
#include "coretools/Main/TParameters.h"
#include "coretools/Main/TRandomGenerator.h"
#include <cstdint>
#include <string>
#include <vector>

namespace coretools {

// Note: positions are stored as uint32_t
// as this should easily be sufficient for a human genome (max for uint32_t = 4'294'967'296; human genome =
// 3'100'000'000) if at some point a larger type is needed, we suggest to solve this with inheritance as this derived
// class would need to store uint64_t, but probably most positions are smaller than this -> some smart indexing/binning
// scheme using offsets would be cool)

class TPositionsRaw {
protected:
	// chunk names
	std::vector<std::string> _chunkNames;

	// chunk ends
	// -> these correspond to the indices of the positions, not to the actual positions themselves!
	std::vector<uint32_t> _chunkEnds;

	// positions
	std::vector<uint32_t> _positions;

	// temporary while filling
	std::string _curChunkName;

	// some variables/functions for quick lookup
	bool _found;
	size_t _indexCurrentlyVisitedPosition;
	size_t _indexPreviouslyVisitedPosition;
	size_t _indexCurrentlyVisitedChunk;
	size_t _indexPreviouslyVisitedChunk;
	uint32_t _maxSearchDistance;
	bool _findChunk(std::string_view Chunk);
	bool _findPositionAfter(uint32_t Position, size_t Start);
	bool _findPositionBefore(uint32_t Position, size_t Start);
	bool _findPosition(uint32_t Position);

	void _addChunk(std::string_view ChunkName);
	virtual void _addPositionOnNewChunk(size_t Position);
	virtual void _addPositionOnExistingChunk(size_t Position);
	void _addToChunkEnds();
	static std::vector<size_t> _simulateChunks(size_t Length);

public:
	TPositionsRaw();
	virtual ~TPositionsRaw() = default;

	// fill
	void add(size_t Position, std::string_view ChunkName);
	void finalizeFilling();

	// get size
	virtual size_t size() const;
	size_t numChunks() const;

	// exists / get index
	void setMaxSearchDistance(size_t MaxSearchDistance);
	bool exists(uint32_t Pos, std::string_view Chunk);
	size_t getIndex(uint32_t Pos, std::string_view Chunk) const;

	// get: linear index over all
	size_t getPosition(size_t Index) const;
	const std::string &getChunkName(size_t Index) const;

	// get chunk ends
	template<typename T> std::vector<T> getChunkEnds() {
		// cast uint32_t to T
		std::vector<T> vec(_chunkEnds.begin(), _chunkEnds.end());
		return vec;
	}

	// get as string
	std::string getPositionChunkAsString(size_t Index, std::string_view Delimiter = ":") const;
	std::string getChunkPositionAsString(size_t Index, std::string_view Delimiter = ":") const;

	// simulate
	virtual void simulate(const std::vector<size_t> &ChunkSizes, double Lambda, bool SimulateWithFixedDistance,
	                      size_t FixedDistance);
	virtual void simulate(size_t Length);
	void writeDistancesToFile() const;
};

//-------------------------------------------
// TDistanceGroup
//-------------------------------------------

struct TDistanceGroup {
	uint32_t min;
	uint32_t maxPlusOne;
	uint32_t distance;
	bool hasSites;
};

//-------------------------------------------
// TDistancesBinnedBase
//-------------------------------------------
class TDistancesBinnedBase : public TPositionsRaw {
	// untemplated base class
	// provides common interface (with size_t) for templated derived classes
protected:
public:
	TDistancesBinnedBase() : TPositionsRaw() {};
	virtual void initialize(size_t maxDist) = 0;

	// get distances
	virtual size_t numDistanceGroups() const                 = 0;
	virtual TDistanceGroup distanceGroup(size_t Group) const = 0;
	virtual bool groupHasSites(size_t Group)                 = 0;

	// get distances of loci
	size_t size() const override              = 0;
	virtual size_t operator[](size_t l) const = 0;
};

//-------------------------------------------
// TDistancesBinned
//-------------------------------------------

template<class NumDistanceGroupsType> class TDistancesBinned : public TDistancesBinnedBase {
	// templated distance class
	// stores a vector of NumDistanceGroupsType, corresponding to the distance group of each position
protected:
	// distances
	NumDistanceGroupsType _numDistanceGroups;
	std::vector<TDistanceGroup> _distanceGroups;
	std::vector<NumDistanceGroupsType> _groupMembership; // for each site, to which group does it belong?

	void _addPositionOnNewChunk(size_t Position) override {
		// store raw position (base class)
		TDistancesBinnedBase::_addPositionOnNewChunk(Position);

		// add distance class 0 (first on chunk)
		_store(0);
	}

	void _addPositionOnExistingChunk(size_t Position) override {
		size_t lastPosition = _positions.back();

		// store raw position (base class)
		TDistancesBinnedBase::_addPositionOnExistingChunk(Position);

		// calculate distance
		NumDistanceGroupsType g = 0;
		if (_numDistanceGroups > 1) { // if maxDist = 1 -> results in _numDistanceGroups=1 -> all
			                          // loci have same distance group (= initial one)
			size_t dist = Position - lastPosition;
			for (g = 1; g < _numDistanceGroups; ++g) {
				if (g == _numDistanceGroups - 1 || dist < _distanceGroups[g].maxPlusOne) break;
			}
		}
		_store(g);
	}
	void _store(const NumDistanceGroupsType &g) {
		_groupMembership.push_back(g);
		_distanceGroups[g].hasSites = true;
	}

	// simulate
	uint32_t _getRandomDistance(double lambda) {
		uint32_t distance = coretools::instances::randomGenerator().getPoissonRandom(lambda);
		while (distance == 0) {
			// draw again
			distance = coretools::instances::randomGenerator().getPoissonRandom(lambda);
		}

		return distance;
	}

public:
	TDistancesBinned() : TDistancesBinnedBase() { _numDistanceGroups = 0; }

	TDistancesBinned(size_t maxDist) : TDistancesBinned() { initialize(maxDist); }

	void initialize(size_t maxDist) override {
		// initialize _distanceGroups
		DEV_ASSERT(maxDist > 0);

		_numDistanceGroups = static_cast<NumDistanceGroupsType>(
		    ceil(log2(maxDist)) + 1); // one extra for beginning of chromosome (and last in genome)
		_distanceGroups.assign(_numDistanceGroups, TDistanceGroup());

		// group 0: first site of chromosome
		_distanceGroups[0].min        = 0;
		_distanceGroups[0].maxPlusOne = 0;
		_distanceGroups[0].distance   = 0;
		_distanceGroups[0].hasSites   = false;

		// all other groups
		if (_numDistanceGroups > 1) {
			_distanceGroups[1].min      = 1;
			_distanceGroups[1].hasSites = false;
			int g;
			for (g = 2; g < _numDistanceGroups; g++) {
				_distanceGroups[g].min            = _distanceGroups[g - 1].min * 2;
				_distanceGroups[g - 1].maxPlusOne = _distanceGroups[g].min;
				_distanceGroups[g - 1].distance   = _distanceGroups[g - 1].maxPlusOne - _distanceGroups[g - 1].min;
				_distanceGroups[g].hasSites       = false;
			}
			_distanceGroups[_numDistanceGroups - 1].maxPlusOne = _distanceGroups[_numDistanceGroups - 1].min * 2;
			_distanceGroups[_numDistanceGroups - 1].distance =
			    _distanceGroups[_numDistanceGroups - 1].maxPlusOne - _distanceGroups[_numDistanceGroups - 1].min;
		}
	}

	// get distances
	size_t numDistanceGroups() const override { return _numDistanceGroups; }

	TDistanceGroup distanceGroup(size_t Group) const override {
		DEV_ASSERT(Group < _numDistanceGroups);
		return _distanceGroups[Group];
	}

	bool groupHasSites(size_t Group) override {
		DEV_ASSERT(Group < _numDistanceGroups);
		return _distanceGroups[Group].hasSites;
	}

	// get distances of loci
	size_t size() const override { return _groupMembership.size(); }

	size_t operator[](size_t l) const override {
		assert(l < _groupMembership.size());
		return _groupMembership[l];
	}

	// simulate distances
	void simulate(const std::vector<size_t> &ChunkSizes, double Lambda, bool SimulateWithFixedDistance,
	              size_t FixedDistance) override {
		// ChunkSizes: vector of length of chunks -> e.g. {10, 50, 5} will
		// simulate 3 chunks of size 10, 50 and 5, respectively. simulate
		// distances with random values from a Poisson distribution

		size_t position;
		std::string prefix = "chunk_";
		size_t counter     = 1;
		for (size_t last : ChunkSizes) {
			position = 0;
			for (size_t i = 0; i < last; ++i) {
				// add other
				if (Lambda == 0.) { // adjacent loci
					position++;
				} else if (SimulateWithFixedDistance) {
					position += FixedDistance;
				} else { // simulate distance
					position += _getRandomDistance(Lambda);
				}
				add(position, prefix + coretools::str::toString(counter));
			}
			counter++;
		}

		finalizeFilling();
	}

	void simulate(size_t Length) override {
		double lambda                  = coretools::instances::parameters().get("rateDistances", 10.);
		bool simulateWithFixedDistance = false;
		size_t fixedDistance           = 0;
		if (coretools::instances::parameters().exists("fixedDistances")) {
			simulateWithFixedDistance = true;
			fixedDistance             = coretools::instances::parameters().get<size_t>("fixedDistances");
		}

		// simulate chunks
		auto chunkSizes = this->_simulateChunks(Length);

		// now simulate with these parameters
		simulate(chunkSizes, lambda, simulateWithFixedDistance, fixedDistance);
		// write to file?
		writeDistancesToFile();
	}
};

}; // end namespace coretools

#endif // TDISTANCES_H
