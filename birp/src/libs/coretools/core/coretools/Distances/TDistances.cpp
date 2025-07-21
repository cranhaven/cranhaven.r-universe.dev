//
// Created by madleina on 07.01.21.
//

#include "coretools/Distances/TDistances.h"
#include "coretools/Files/TOutputFile.h"
#include "coretools/Main/TError.h"
#include "coretools/algorithms.h"

namespace coretools {

using namespace coretools::str;

//-------------------------------------------
// TPositionsRaw
//-------------------------------------------

TPositionsRaw::TPositionsRaw() {
	_indexCurrentlyVisitedChunk     = 0;
	_indexPreviouslyVisitedChunk    = 0;
	_indexCurrentlyVisitedPosition  = 0;
	_indexPreviouslyVisitedPosition = 0;
	_found                          = false;
	_maxSearchDistance              = 100;
	_curChunkName                   = "";
}

void TPositionsRaw::_addToChunkEnds() {
	// take size of container (and not -1) because end is not included (loop until < end)
	_chunkEnds.push_back(_positions.size());
}

void TPositionsRaw::_addChunk(std::string_view ChunkName) {
	_chunkNames.emplace_back(ChunkName);

	// add current index inside position to chunk ends (only if it is not the first position)
	if (!_positions.empty()) { _addToChunkEnds(); }

	// update _curChunkName
	_curChunkName = ChunkName;
}

void TPositionsRaw::_addPositionOnNewChunk(size_t Position) {
	// base class: doesn't matter if position is on new chunk or not
	// (but it does matter for derived classes)
	_positions.push_back(Position);
}

void TPositionsRaw::_addPositionOnExistingChunk(size_t Position) { _positions.push_back(Position); }

void TPositionsRaw::add(size_t Position, std::string_view ChunkName) {
	if (_curChunkName != ChunkName) {
		_addChunk(ChunkName);
		_addPositionOnNewChunk(Position);
	} else {
		_addPositionOnExistingChunk(Position);
	}
}

void TPositionsRaw::finalizeFilling() {
	// add index of last position to chunk ends
	_addToChunkEnds();
}

size_t TPositionsRaw::size() const { return _positions.size(); }

size_t TPositionsRaw::numChunks() const { return _chunkNames.size(); }

const std::string &TPositionsRaw::getChunkName(size_t Index) const {
	DEBUG_ASSERT(Index < _positions.size());
	// which interval of chunkEnds contains this index?
	for (size_t j = 0; j < _chunkEnds.size(); j++) {
		if (Index < _chunkEnds[j]) { return _chunkNames[j]; }
	}
	// should never get here
	throw TDevError("Should never get here - did not find chunk name for index ", Index, "!");
}

size_t TPositionsRaw::getPosition(size_t Index) const {
	DEBUG_ASSERT(Index < _positions.size());
	return _positions[Index];
}

std::string TPositionsRaw::getPositionChunkAsString(size_t Index, std::string_view Delimiter) const {
	return toString(getPosition(Index), Delimiter, getChunkName(Index));
}

std::string TPositionsRaw::getChunkPositionAsString(size_t Index, std::string_view Delimiter) const {
	return toString(getChunkName(Index), Delimiter, getPosition(Index));
}

void TPositionsRaw::setMaxSearchDistance(size_t MaxSearchDistance) { _maxSearchDistance = MaxSearchDistance; }

bool TPositionsRaw::_findChunk(std::string_view Chunk) {
	// same chunk as last?
	if (Chunk == _chunkNames[_indexPreviouslyVisitedChunk]) {
		_indexCurrentlyVisitedChunk = _indexPreviouslyVisitedChunk;
		return true;
	}
	// next?
	if (_indexPreviouslyVisitedChunk + 1 < _chunkNames.size() &&
	    Chunk == _chunkNames[_indexPreviouslyVisitedChunk + 1]) {
		_indexCurrentlyVisitedChunk = _indexPreviouslyVisitedChunk + 1;
		return true;
	}
	// previous?
	if (_indexPreviouslyVisitedChunk > 0 && Chunk == _chunkNames[_indexPreviouslyVisitedChunk - 1]) {
		_indexCurrentlyVisitedChunk = _indexPreviouslyVisitedChunk - 1;
		return true;
	}
	// some other -> binary search
	try {
		_indexCurrentlyVisitedChunk = coretools::binarySearch_getIndex(_chunkNames.begin(), _chunkNames.end(), Chunk);
		return true;
	} catch (const std::exception &error) {
		// not found
		return false;
	}
}

bool TPositionsRaw::_findPositionAfter(uint32_t Position, size_t Start) {
	size_t until = Start + _maxSearchDistance;
	for (size_t i = Start; i < until; i++) {
		// reached end of chunk?
		if (i == _chunkEnds[_indexCurrentlyVisitedChunk]) { return false; }
		if (_positions[i] == Position) {
			_indexCurrentlyVisitedPosition = i;
			return true;
		}
	}
	// not found within these first maxSearchDistance elements -> do binary search
	try {
		const auto start               = _positions.begin() + until; // skip elements we've already checked
		const auto end                 = _positions.begin() + _chunkEnds[_indexCurrentlyVisitedChunk]; // end of chunk
		_indexCurrentlyVisitedPosition = coretools::binarySearch_getIndex(start, end, Position) + until;
		return true;
	} catch (const std::exception &error) {
		// not found
		return false;
	}
}

bool TPositionsRaw::_findPositionBefore(uint32_t Position, size_t Start) {
	if (_indexPreviouslyVisitedPosition == 0) {
		// there is no previous -> directly return (otherwise issues with negative size_t)
		return false;
	}

	// define beginning of current chunk
	const int beginChunk = _indexCurrentlyVisitedChunk > 0 ? _chunkEnds[_indexCurrentlyVisitedChunk - 1] : 0;

	// define until where we want to search (until is included)
	const int until = Start > _maxSearchDistance ? static_cast<int>(Start) - _maxSearchDistance + 1 : 0;

	for (int i = static_cast<int>(Start); i >= until; i--) { // go backwards from Start
		if (_positions[i] == Position) {
			_indexCurrentlyVisitedPosition = i;
			return true;
		}
		// reached beginning of chunk? -> stop in next iteration
		if ((_indexCurrentlyVisitedChunk == 0 && i == 0) || (_indexCurrentlyVisitedChunk > 0 && i == beginChunk)) {
			return false;
		}
	}
	// not found within these first maxSearchDistance elements -> do binary search
	try {
		const auto start               = _positions.begin() + beginChunk;
		const auto end                 = _positions.begin() + until; // skip elements we've already checked
		_indexCurrentlyVisitedPosition = coretools::binarySearch_getIndex(start, end, Position) + beginChunk;
		return true;
	} catch (const std::exception &error) {
		// not found
		return false;
	}
}

bool TPositionsRaw::_findPosition(uint32_t Position) {
	// same chunk as last position?
	if (_indexCurrentlyVisitedChunk == _indexPreviouslyVisitedChunk) {
		// position same as previous?
		if (Position == _positions[_indexPreviouslyVisitedPosition]) {
			_indexCurrentlyVisitedPosition = _indexPreviouslyVisitedPosition;
			return true;
		} else if (Position > _positions[_indexPreviouslyVisitedPosition]) {
			// position after previous?
			return _findPositionAfter(Position, _indexPreviouslyVisitedPosition + 1);
		} else {
			// position before previous
			return _findPositionBefore(Position, _indexPreviouslyVisitedPosition - 1);
		}
	}
	// previous chunk?
	if (_indexCurrentlyVisitedChunk < _indexPreviouslyVisitedChunk) {
		// start at end of that chunk
		return _findPositionBefore(Position, _chunkEnds[_indexCurrentlyVisitedChunk] - 1);
	} else {
		// next chunk
		// start at beginning of chunk
		size_t startIndex = 0;
		if (_indexCurrentlyVisitedChunk > 0) {
			startIndex += _chunkEnds[_indexCurrentlyVisitedChunk - 1]; // beginning of chunk
		}
		return _findPositionAfter(Position, startIndex);
	}
}

bool TPositionsRaw::exists(uint32_t Pos, std::string_view Chunk) {
	// Idea: we will usually parse positions in some more or less ordered fashion
	// -> usually we want just consecutive element of the one we've visited before
	// algorithm:
	// get new index -> check if it is on same chunk as previous
	//    -> yes: if currentPos > lastPos -> search in consecutive elements of lastPos for currentPos
	//            -> if we can't find it (after searching for _maxSearchDistance positions) -> do binary search
	//            if currentPos < lastPos -> search in preceeding elements of lastPos for currentPos
	//            -> if we can't find it (after searching for _maxSearchDistance positions) -> do binary search
	//    -> no:  if newChunk > lastChunk -> start searching for element in first elements of newChunk
	//            -> if we can't find it (after searching for _maxSearchDistance positions) -> do binary search
	//            if newChunk < lastChunk -> start searching for element in last elements of newChunk
	//            -> if we can't find it (after searching for _maxSearchDistance positions) -> do binary search

	// first re-set all elements
	_found                          = false;
	_indexPreviouslyVisitedChunk    = _indexCurrentlyVisitedChunk;
	_indexPreviouslyVisitedPosition = _indexCurrentlyVisitedPosition;

	// now find
	if (_findChunk(Chunk)) {
		_found = _findPosition(Pos);
		if (!_found) {
			// found chunk, but not position -> reset chunk variable
			_indexCurrentlyVisitedChunk = _indexPreviouslyVisitedChunk;
		}
	}
	return _found;
}

size_t TPositionsRaw::getIndex(uint32_t Pos, std::string_view Chunk) const {
	// idea: always first check with exists() if element exists.
	// If yes, this search will already store the indices to the match, so we don't need to search twice
	if (_found) {
		// security check: someone might forget to use exists() and getIndex() right after each other
		if (_positions[_indexCurrentlyVisitedPosition] != Pos || _chunkNames[_indexCurrentlyVisitedChunk] != Chunk) {
			throw TDevError("Position ", Pos, " on chunk ", Chunk,
			         " is different than expected from lookup! Did you use getIndex() right after calling exists()?");
		}
		return _indexCurrentlyVisitedPosition;
	} else {
		throw TDevError("Position ", Pos, " on chunk ", Chunk,
		         " does not exist in TPositionsRaw! Always check first with exist() whether or not name class exists.");
	}
}

void TPositionsRaw::simulate(const std::vector<size_t> &ChunkSizes, double, bool, size_t) {
	// ChunkSizes: vector of length of chunks -> e.g. {10, 50, 5} will simulate 3 chunks of size 10, 50 and 5,
	// respectively. all distances = 1

	size_t position;
	std::string prefix = "chunk_";
	size_t counter     = 1;
	for (size_t last : ChunkSizes) {
		position = 0;
		for (size_t i = 0; i < last; ++i) {
			// add other
			position++;
			add(position, prefix + toString(counter));
		}
		counter++;
	}

	finalizeFilling();
}

std::vector<size_t> TPositionsRaw::_simulateChunks(size_t Length) {
	// define chunk sizes: by default, take size of parameter and split it in two
	std::vector<size_t> chunkSizes;
	if (Length % 2 == 0) { // even size
		auto len   = static_cast<size_t>((double)Length / 2.);
		chunkSizes = {len, len};
	} else { // odd size
		auto len   = static_cast<size_t>(((double)Length - 1.) / 2.);
		chunkSizes = {len + 1, len};
	}
	return chunkSizes;
}

void TPositionsRaw::simulate(size_t Length) {
	auto chunkSizes = _simulateChunks(Length);

	// now simulate with these chunks
	simulate(chunkSizes, 0., false, 0);
	// write to file?
	writeDistancesToFile();
}

void TPositionsRaw::writeDistancesToFile() const {
	// write distances to file?
	if (coretools::instances::parameters().exists("writeSimulatedDistances")) {
		std::string filename = coretools::instances::parameters().get("writeSimulatedDistances");

		coretools::TOutputFile file(filename, {"chunk", "position"});
		for (size_t i = 0; i < size(); i++) { file.writeln(getChunkName(i), getPosition(i)); }
		file.close();
	}
}

}; // end namespace coretools
