//
// Created by madleina on 27.04.21.
//

#include "coretools/Storage/TNames.h"
#include "coretools/Main/TError.h"
#include "coretools/Strings/concatenateString.h"
#include "coretools/Strings/fillContainer.h"
#include "coretools/Strings/stringConversions.h"
#include "coretools/Strings/stringManipulations.h"
#include "coretools/Distances/TDistances.h"

#include <set>


namespace coretools {

using namespace str;

namespace {
template<typename StreamType>
bool readUntilDelimiter(StreamType *FilePointer, std::string &String, char Delimiter,
                        std::string_view DelimiterComment = "//") {
	String.clear();
	if (FilePointer->good() && !FilePointer->eof()) {
		std::getline(*FilePointer, String, Delimiter);

		// skip comments
		String = extractBefore(String, DelimiterComment, false);
	}

	if (!FilePointer->good() || FilePointer->eof()) { return false; }
	return true;
}
}

//--------------------------
// TNamesEmpty
//--------------------------

void TNamesEmpty::_checkSizeNameVec(const std::vector<std::string> &Name, std::string_view ClassNameForError) const {
	if (Name.size() != _complexity) {
		DEVERROR("For class '", ClassNameForError, ": Expected vector Name to be of size ", _complexity,
		         ", but vector '", Name, "' is of size ", Name.size(), "!");
	}
};

TNamesEmpty::TNamesEmpty() {
	_size                  = 0;
	_complexity            = 0;
	_title                 = {""};
	_delimNames            = '\0';
	_indexVisited          = 0;
	_storesNonDefaultNames = false;
};

TNamesEmpty::TNamesEmpty(size_t Size) {
	_size                  = Size;
	_complexity            = 0;
	_title                 = {""};
	_delimNames            = '\0';
	_indexVisited          = 0;
	_storesNonDefaultNames = false;
};

void TNamesEmpty::resize(size_t Size) { _size = Size; };

void TNamesEmpty::addName(const std::vector<std::string> &Name) {
	_size++;
	_checkSizeNameVec(Name, "TNamesEmpty");
};

void TNamesEmpty::addName(const std::vector<std::string> &Name, size_t) { _checkSizeNameVec(Name, "TNamesEmpty"); };

void TNamesEmpty::addNameAndSplit(std::string_view Name) {
	std::vector<std::string> fullName;
	fillContainerFromString(Name, fullName, _delimNames);
	addName(fullName);
};

void TNamesEmpty::addNameAndSplit(std::string_view Name, size_t Index) {
	std::vector<std::string> fullName;
	fillContainerFromString(Name, fullName, _delimNames);
	addName(fullName, Index);
};

std::vector<std::string> TNamesEmpty::_extractFromStringAndReturnVec(std::string &String, char DelimiterLast,
                                                                     bool ThrowIfEmpty) const {
	if (_complexity == 0) {
		// for zero complexity: don't extract anything
		return {};
	}
	// split from String until full name except last element has been read
	std::vector<std::string> full;
	while (full.size() < _complexity - 1) {
		std::string name = split(String, _delimNames);
		// check: is name empty?
		if (ThrowIfEmpty && name.empty()) {
			std::string delimNameString = {_delimNames};
			DEVERROR("Names that was split from string ", String, " after delimiter ", delimNameString, " is empty!");
		}
		full.push_back(name);
	}
	// for last element: we have to search for a different delimiter
	std::string name = split(String, DelimiterLast);
	// check: is name empty?
	if (ThrowIfEmpty && name.empty()) {
		std::string delimLastString = {DelimiterLast};
		DEVERROR("Name that was split from string '", String, "' after delimiter '", delimLastString, "' is empty!");
	}
	full.push_back(name);

	// check: does full have expected size?
	if (full.size() != _complexity) {
		DEVERROR("Vector of names ", full, " of size ", full.size(), " does not have ",
		         _complexity, " elements as expected based on complexity of name class!");
	}

	return full;
};

std::string TNamesEmpty::_extractFromStringAndReturnString(std::string &String, char DelimiterLast,
                                                           bool ThrowIfEmpty) const {
	// return as one string
	// first split into vector, then concatenate with _delimNames
	// easier that way to test if all is valid (e.g. size of vector matches complexity)
	std::string delimNamesString = {_delimNames};
	return concatenateString(_extractFromStringAndReturnVec(String, DelimiterLast, ThrowIfEmpty), delimNamesString);
};

bool TNamesEmpty::_extractFromStreamAndFillVec(std::vector<std::string> &Vec, std::istream *FilePointer,
                                               char DelimiterLast, std::string_view DelimiterComment,
                                               bool ThrowIfEmpty) const {
	std::string name;
	Vec.clear();

	if (_complexity == 0) {
		// for zero complexity: don't extract anything
		return true;
	}

	// read from stream until full name except last element has been read
	while (Vec.size() < _complexity - 1) {
		// read until delimiter of names
		if (!readUntilDelimiter(FilePointer, name, _delimNames, DelimiterComment)) {
			return false; // reached end of file
		}
		// check if name is valid
		if (ThrowIfEmpty && name.empty()) {
			std::string delimNameString = {_delimNames};
			DEVERROR("Names that read from stream before delimiter ", delimNameString, " is empty!");
		}
		Vec.push_back(name);
	}
	// for last element: we have to search for a different delimiter
	if (!readUntilDelimiter(FilePointer, name, DelimiterLast, DelimiterComment)) {
		return false; // reached end of file
	}
	if (ThrowIfEmpty && name.empty()) {
		std::string delimLastString = {DelimiterLast};
		DEVERROR("Names that read from stream before delimiter '", delimLastString, "' is empty!");
	}
	Vec.push_back(name);

	// check: does Vec have expected size?
	if (Vec.size() != _complexity) {
		DEVERROR("Vector of names (", concatenateString(Vec, ", "), ") of size ", Vec.size(), " does not have ",
		         _complexity, " elements as expected based on complexity of name class!");
	}

	return true;
};

void TNamesEmpty::extractNameFromStringAndStore(std::string &String, char DelimiterLast) {
	// split from String until full name has been read, and store
	// throw if empty = true -> names can not be empty!
	std::vector<std::string> fullName = _extractFromStringAndReturnVec(String, DelimiterLast, true);
	addName(fullName);
};

std::string TNamesEmpty::extractNameFromStringAndReturn(std::string &String, char DelimiterLast) {
	// doesn't store anything!
	// throw if empty = true -> names can not be empty!
	return _extractFromStringAndReturnString(String, DelimiterLast, true);
};

bool TNamesEmpty::extractNameFromStreamAndFillIntoVec(std::istream *FilePointer, char DelimiterLast,
                                                      std::string_view DelimiterComment,
                                                      std::vector<std::string> &Vec) {
	// throw if empty = true -> names can not be empty!
	if (!_extractFromStreamAndFillVec(Vec, FilePointer, DelimiterLast, DelimiterComment, true)) { return false; }
	return true;
};

bool TNamesEmpty::checkIfNameShouldBeKept(const std::vector<std::string> &Name, std::string_view FileName) {
	// check if we reached end of name class
	if (_indexVisited >= size()) {
		UERROR("Reached end of known rownames in file ", FileName, "! Name ", concatenateString(Name, _delimNames),
		       " (corresponding to the ", _indexVisited,
		       " element stored) was found in file, but size of known names is only ", size(), ".");
	}
	// is the name the same as we would expect from name class?
	if (getName(_indexVisited) == Name) {
		// same name -> keep! (no need to store it, as it is already there)
		_indexVisited++;
		return true;
	} else {
		// check if name exists
		if (exists(Name)) {
			// it does exist, but not at the right place -> throw! We can't reorder most outer rownames
			UERROR("Rownames of file ", FileName, " are shuffled relative to the expected names! Name ",
			       concatenateString(Name, _delimNames), " was expected to be equal to ", operator[](_indexVisited),
			       ",  but it was found at another position in file.");
		} else {
			// it doesn't exist -> skip
			return false;
		}
	}
};

std::vector<std::string> TNamesEmpty::extractTitleFromString(std::string &String, char DelimiterLast) {
	// split from String until full name has been read
	// doesn't store anything!
	// throw if empty = false -> title can be empty!
	return _extractFromStringAndReturnVec(String, DelimiterLast, false);
};

void TNamesEmpty::setTitle(const std::vector<std::string> &Title) {
	if (Title.size() != _complexity) {
		DEVERROR("Title (", concatenateString(Title, ","), ") of size ", Title.size(),
		         " does not have expected size based on complexity (", _complexity, ")!");
	}
	_title = Title;
};

void TNamesEmpty::setDelimName(char Delim) { _delimNames = Delim; };

std::string TNamesEmpty::operator[](size_t) const { return ""; };

std::vector<std::string> TNamesEmpty::getName(size_t Index) const { return {operator[](Index)}; };

bool TNamesEmpty::storesNonDefaultNames() const { return _storesNonDefaultNames; };

bool TNamesEmpty::exists(std::string_view ) {
	// base class always returns false
	return false;
};

bool TNamesEmpty::exists(const std::vector<std::string> &) {
	// base class always returns false
	return false;
};

bool TNamesEmpty::isFilled() const {
	if (size() != 0) { return true; }
	return false;
};
void TNamesEmpty::finalizeFilling(){
    // no need to do anything special for base class
};

bool TNamesEmpty::operator==(const TNamesEmpty &Other) const {
	return size() == Other.size() && complexity() == Other.complexity() && getTitleVec() == Other.getTitleVec() &&
	       getDelimNames() == Other.getDelimNames() && storesNonDefaultNames() == Other.storesNonDefaultNames() &&
	       _indexVisited == Other._indexVisited;
};

bool TNamesEmpty::operator!=(const TNamesEmpty &Other) const { return !operator==(Other); };

size_t TNamesEmpty::getIndex(std::string_view Name) {
	// base class throws -> can not be filled, so don't ask for index
	DEVERROR("Name ", Name,
	         " does not exist in Name class TNamesEmpty! Always check first with exists() whether or not name class "
	         "exists.");
};

const std::vector<std::string> &TNamesEmpty::getTitleVec() const { return _title; };

std::string TNamesEmpty::getTitle() const {
	std::string delimNamesString = {_delimNames};
	return concatenateString(_title, delimNamesString);
};

char TNamesEmpty::getDelimNames() const { return _delimNames; };

size_t TNamesEmpty::size() const { return _size; };

size_t TNamesEmpty::complexity() const { return _complexity; };

size_t TNamesEmpty::offset() const { return 0; }

//--------------------------
// TNamesStrings
//--------------------------

TNamesStrings::TNamesStrings() : TNamesEmpty() {
	_complexity            = 1;
	_title                 = {"-"};
	_storesNonDefaultNames = true;
};

TNamesStrings::TNamesStrings(size_t Size) : TNamesEmpty(Size) {
	_complexity = 1;
	resize(Size);
	_title                 = {"-"};
	_storesNonDefaultNames = true;
};

TNamesStrings::TNamesStrings(const std::vector<std::string> &Names) : TNamesEmpty() {
	_complexity            = 1;
	_names                 = Names;
	_title                 = {"-"};
	_storesNonDefaultNames = true;
};

void TNamesStrings::resize(size_t Size) {
	_names.resize(Size);
	_size = Size;
};

void TNamesStrings::addName(const std::vector<std::string> &Name) {
	_checkSizeNameVec(Name, "TNamesStrings");
	_names.push_back(Name[0]);
	_size++;
};

void TNamesStrings::addName(const std::vector<std::string> &Name, size_t Index) {
	_checkSizeNameVec(Name, "TNamesStrings");
	if (Index >= _names.size()) {
		DEVERROR("Index ", Index, " is larger than size of Name vector (", _names.size(), ")!");
	}
	_names[Index] = Name[0];
};

std::string TNamesStrings::operator[](size_t Index) const {
	if (Index >= _names.size()) {
		DEVERROR("Index ", Index, " is larger than size of Name vector (", _names.size(), ")!");
	}
	return _names[Index];
};

bool TNamesStrings::exists(std::string_view Name) {
	// find Name in vector of names
	auto match = std::find(_names.begin(), _names.end(), Name);
	if (match != _names.end()) { return true; }
	return false;
};

bool TNamesStrings::exists(const std::vector<std::string> &Name) {
	_checkSizeNameVec(Name, "TNamesStrings");
	return exists(Name[0]);
};

bool TNamesStrings::hasDuplicateNames() const {
	std::set<std::string> s(_names.begin(), _names.end());
	return s.size() != _names.size();
};

bool TNamesStrings::operator==(const TNamesEmpty &Other) const {
	if (!TNamesEmpty::operator==(Other)) { return false; }
	for (size_t i = 0; i < _size; i++) {
		if (_names[i] != Other[i]) { return false; }
	}
	return true;
};

size_t TNamesStrings::getIndex(std::string_view Name) {
	// find Name in vector of names
	// if not found -> throw (but this should never happen, as developer should always check with exists() whether it
	// exists)
	auto match = std::find(_names.begin(), _names.end(), Name);
	if (match != _names.end()) { return std::distance(_names.begin(), match); }
	DEVERROR("Name ", Name,
	         " does not exist in Name class TNamesStrings! Always check first with exist() whether or not name class "
	         "exists.");
};

size_t TNamesStrings::size() const { return _names.size(); };

//--------------------------
// TNamesIndices
//--------------------------

TNamesIndices::TNamesIndices() : TNamesEmpty() {
	_offset                = 1;
	_complexity            = 1;
	_title                 = {"-"};
	_storesNonDefaultNames = false;
};

TNamesIndices::TNamesIndices(size_t Size) : TNamesEmpty(Size) {
	_offset                = 1;
	_complexity            = 1;
	_title                 = {"-"};
	_storesNonDefaultNames = false;
};

void TNamesIndices::setOffset(size_t Offset) { _offset = Offset; };

std::string TNamesIndices::operator[](size_t Index) const { return toString(_offset + Index); };

bool TNamesIndices::exists(std::string_view Name) {
	// return true if Name is smaller than maximum index stored by this class
	size_t max = _offset + _size;
	if (fromString<size_t>(Name) < max) { return true; }
	return false;
};

bool TNamesIndices::exists(const std::vector<std::string> &Name) {
	_checkSizeNameVec(Name, "TNamesIndices");
	return exists(Name[0]);
};

size_t TNamesIndices::offset() const { return _offset; };

bool TNamesIndices::operator==(const TNamesEmpty &Other) const {
	return TNamesEmpty::operator==(Other) && _offset == Other.offset();
};

size_t TNamesIndices::getIndex(std::string_view Name) {
	// return Name as number minus offset
	// if not found -> throw (but this should never happen, as developer should always check with exists() whether it
	// exists)
	size_t index = fromString<size_t>(Name) - _offset;
	if (index < _size) { return index; }
	DEVERROR("Name ", Name, " does not exist in Name class TNamesIndices (with size = ", _size,
	         ")! Always check first with exist() whether or not name class exists.");
};

//-------------------------------
// TNamesIndicesAlphabetUpperCase
//-------------------------------

TNamesIndicesAlphabetUpperCase::TNamesIndicesAlphabetUpperCase() : TNamesEmpty() {
	_complexity            = 1;
	_title                 = {"-"};
	_storesNonDefaultNames = false;
};

TNamesIndicesAlphabetUpperCase::TNamesIndicesAlphabetUpperCase(size_t Size) : TNamesEmpty(Size) {
	_complexity            = 1;
	_title                 = {"-"};
	_storesNonDefaultNames = false;
};

std::string TNamesIndicesAlphabetUpperCase::operator[](size_t Index) const {
	return numericToUpperCaseAlphabetIndex(static_cast<int>(Index));
};

bool TNamesIndicesAlphabetUpperCase::exists(std::string_view Name) {
	// return true if Name is smaller than maximum index stored by this class
	if (upperCaseAlphabetIndexToNumeric(Name) < _size) { return true; }
	return false;
};

bool TNamesIndicesAlphabetUpperCase::exists(const std::vector<std::string> &Name) {
	_checkSizeNameVec(Name, "TNamesIndicesAlphabetUpperCase");
	return exists(Name[0]);
};

size_t TNamesIndicesAlphabetUpperCase::getIndex(std::string_view Name) {
	// return Name as numeric version of alphabet letter
	// if not found -> throw (but this should never happen, as developer should always check with exists() whether it
	// exists)
	size_t index = upperCaseAlphabetIndexToNumeric(Name);
	if (index < _size) { return index; }
	DEVERROR("Name ", Name, " does not exist in Name class TNamesIndicesAlphabetUpperCase (with size = ", _size,
	         ")! Always check first with exist() whether or not name class exists.");
};

//-------------------------------
// TNamesIndicesAlphabetLowerCase
//-------------------------------

TNamesIndicesAlphabetLowerCase::TNamesIndicesAlphabetLowerCase() : TNamesEmpty() {
	_complexity            = 1;
	_title                 = {"-"};
	_storesNonDefaultNames = false;
};

TNamesIndicesAlphabetLowerCase::TNamesIndicesAlphabetLowerCase(size_t Size) : TNamesEmpty(Size) {
	_complexity            = 1;
	_title                 = {"-"};
	_storesNonDefaultNames = false;
};

std::string TNamesIndicesAlphabetLowerCase::operator[](size_t Index) const {
	return numericToLowerCaseAlphabetIndex(static_cast<int>(Index));
};

bool TNamesIndicesAlphabetLowerCase::exists(std::string_view Name) {
	// return true if Name is smaller than maximum index stored by this class
	if (lowerCaseAlphabetIndexToNumeric(Name) < _size) { return true; }
	return false;
};

bool TNamesIndicesAlphabetLowerCase::exists(const std::vector<std::string> &Name) {
	_checkSizeNameVec(Name, "TNamesIndicesAlphabetLowerCase");
	return exists(Name[0]);
};

size_t TNamesIndicesAlphabetLowerCase::getIndex(std::string_view Name) {
	// return Name as numeric version of alphabet letter
	// if not found -> throw (but this should never happen, as developer should always check with exists() whether it
	// exists)
	size_t index = lowerCaseAlphabetIndexToNumeric(Name);
	if (index < _size) { return index; }
	DEVERROR("Name ", Name, " does not exist in Name class TNamesIndicesAlphabetLowerCase (with size = ", _size,
	         ")! Always check first with exist() whether or not name class exists.");
};

//-------------------------------
// TNamesPositions
//-------------------------------
void TNamesPositions::_splitName(std::string Name, std::string &Chunk, size_t &Pos) {
	if (_orderIsChunkPos) {
		Chunk = split(Name, _delimNames);
		Pos   = fromString<size_t>(Name);
	} else {
		Pos   = fromString<size_t>(split(Name, _delimNames));
		Chunk = Name;
	}
};

TNamesPositions::TNamesPositions(TPositionsRaw *Positions) : TNamesEmpty() {
	addPositions(Positions);
	_complexity            = 2;
	_title                 = {"-", "-"};
	_delimNames            = ':';  // by default, use : to paste Chr:Pos together
	_orderIsChunkPos       = true; // by default, the order is chr-pos
	_storesNonDefaultNames = true;
};

void TNamesPositions::setOrderChrPos(bool OrderIsChrPos) { _orderIsChunkPos = OrderIsChrPos; };

void TNamesPositions::addPositions(TPositionsRaw *Positions) { _positions = Positions; };

void TNamesPositions::addName(const std::vector<std::string> &Name) {
	_checkSizeNameVec(Name, "TNamesPositions");
	// account for order Chr-Pos or Pos-Chr
	std::string pos;
	std::string chr;
	if (_orderIsChunkPos) {
		// Chr,Pos
		chr = Name[0];
		pos = Name[1];
	} else {
		// Pos,Chr
		pos = Name[0];
		chr = Name[1];
	}
	auto p = fromString<size_t, true>(pos);
	_positions->add(p, chr);
	_size++;
};

void TNamesPositions::addName(const std::vector<std::string> &Name, size_t) { addName(Name); };

void TNamesPositions::finalizeFilling() { _positions->finalizeFilling(); };

std::string TNamesPositions::operator[](size_t Index) const {
	std::string delimNamesAsString = {_delimNames};
	if (_orderIsChunkPos) {
		return _positions->getChunkPositionAsString(Index, delimNamesAsString);
	} else {
		return _positions->getPositionChunkAsString(Index, delimNamesAsString);
	}
};

std::vector<std::string> TNamesPositions::getName(size_t Index) const {
	if (_orderIsChunkPos) {
		return {_positions->getChunkName(Index), toString(_positions->getPosition(Index))};
	}
	return {toString(_positions->getPosition(Index)), _positions->getChunkName(Index)};
};

bool TNamesPositions::exists(std::string_view Name) {
	std::string chunk;
	size_t pos;
	_splitName(std::string{Name}, chunk, pos);
	return _positions->exists(pos, chunk);
};

bool TNamesPositions::exists(const std::vector<std::string> &Name) {
	if (_orderIsChunkPos) {
		return _positions->exists(fromString<size_t>(Name[1]), Name[0]);
	} else {
		return _positions->exists(fromString<size_t>(Name[0]), Name[1]);
	}
};

bool TNamesPositions::operator==(const TNamesEmpty &Other) const {
	if (!TNamesEmpty::operator==(Other)) { return false; }
	for (size_t i = 0; i < _size; i++) {
		if (operator[](i) != Other[i]) { return false; }
	}
	return true;
}

size_t TNamesPositions::getIndex(std::string_view Name) {
	std::string chunk;
	size_t pos;
	_splitName(std::string{Name}, chunk, pos);
	return _positions->getIndex(pos, chunk);
};

size_t TNamesPositions::size() const { return _positions->size(); };

}; // end namespace coretools
