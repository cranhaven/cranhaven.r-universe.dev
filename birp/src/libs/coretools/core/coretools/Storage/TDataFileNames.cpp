//
// Created by madleina on 29.04.21.
//

#include "coretools/Storage/TDataFileNames.h"
#include "coretools/Storage/TNames.h"
#include "coretools/Strings/concatenateString.h"
#include "coretools/Strings/fillContainer.h"
#include "coretools/Strings/stringManipulations.h"
#include "coretools/algorithms.h"

namespace coretools {

//--------------------
// TDataBlock
//--------------------

size_t TDataBlock::getBlockSize(const std::vector<size_t> &DimensionsOneBlock) {
	// get total size of vector
	// = product over all
	return std::accumulate(std::begin(DimensionsOneBlock), std::end(DimensionsOneBlock), 1, std::multiplies<>());
}

std::vector<char> TDataBlock::getDelimiterLookupOneBlock(const std::vector<char> &DelimitersOneBlock,
                                                         const std::vector<size_t> &DimensionsOneBlock,
                                                         char LastDelimiter) {
	// vector of delimiters and dimensions must have same size
	assert(DelimitersOneBlock.size() == DimensionsOneBlock.size());

	// get dimensions and block size
	size_t numDim    = DimensionsOneBlock.size();
	size_t blockSize = getBlockSize(DimensionsOneBlock);

	// initialize delimitersOneBlock with this size-1 and the most inner delimiter
	std::vector<char> delimiterLookupOneBlock;
	if (numDim > 0) {
		delimiterLookupOneBlock.resize(blockSize - 1, DelimitersOneBlock.back());

		// now go over all outer delimiters and replace
		size_t jumpSize = 1;
		for (int dim = static_cast<int>(numDim) - 2; dim >= 0; dim--) { // start at second last dimension
			jumpSize *= DimensionsOneBlock[dim + 1];
			for (size_t i = jumpSize - 1; i < delimiterLookupOneBlock.size(); i += jumpSize) {
				delimiterLookupOneBlock[i] = DelimitersOneBlock[dim];
			}
		}
	}

	// add delim for most outer delimiter to the end (typically newline)
	delimiterLookupOneBlock.push_back(LastDelimiter);

	// safety check: for each element in block, we now have a delimiter
	assert(delimiterLookupOneBlock.size() == blockSize);

	return delimiterLookupOneBlock;
}

std::vector<std::vector<size_t>> TDataBlock::getCoordinatesOneBlock(const std::vector<size_t> &DimensionsOneBlock) {
	// prepare storage
	size_t blockSize = getBlockSize(DimensionsOneBlock);
	std::vector<std::vector<size_t>> coordinatesOneBlock(blockSize, std::vector<size_t>(DimensionsOneBlock.size()));

	// get coordinates for linear index
	for (size_t i = 0; i < blockSize; i++) { coordinatesOneBlock[i] = coretools::getSubscripts(i, DimensionsOneBlock); }

	return coordinatesOneBlock;
}

//--------------------
// TFileNames
//--------------------

TFileNames::TFileNames(size_t NumDim, const std::vector<bool> &NameIsWritten,
                       const std::vector<std::shared_ptr<TNamesEmpty>> &Names, const std::vector<char> &Delimiters,
                       std::string_view FileName) {
	_numDim        = NumDim;
	_nameIsWritten = NameIsWritten;
	_names         = Names;
	_filename      = FileName;

	_indexToStoreName.resize(_numDim);
	_storeName.resize(_numDim);

	_fillNameIsRowName(Delimiters);
	_checkInput(Delimiters);
	_fillNameClassIsPrefilled();
}

void TFileNames::_fillNameIsRowName(const std::vector<char> &Delimiters) {
	// for each delimiter: decide if it results in a rowname or colname
	_nameIsRowName.resize(_numDim, false);

	for (size_t dim = 0; dim < _numDim; dim++) {
		// if a delimiter is newline -> starts a new line -> is a rowName
		if (Delimiters[dim] == '\n') { _nameIsRowName[dim] = true; }
	}
}

void TFileNames::_fillNameClassIsPrefilled() {
	_nameClassIsPrefilled.resize(_numDim, false);

	for (size_t dim = 0; dim < _numDim; dim++) {
		if (_names[dim]->isFilled()) { _nameClassIsPrefilled[dim] = true; }
	}
}

void TFileNames::_checkInput(const std::vector<char> &Delimiters) {
	// check if vector of names that should be written matches known dimensions
	if (_nameIsWritten.size() != _numDim) {
		DEVERROR("Error while writing file '", _filename, "'. Size of vector NameIsWritten (", _nameIsWritten.size(),
		         ") does not match the number of dimensions of storage (", _numDim, ").");
	}
	// check if name vector matches numDim
	if (_names.size() != _numDim) {
		DEVERROR("Error while writing file '", _filename, "'. Size of vector Names (", _names.size(),
		         ") does not match the number of dimensions of storage (", _numDim, ").");
	}
	// check if delimiter vector matches numDim
	if (Delimiters.size() != _numDim) {
		DEVERROR("Error while writing file '", _filename, "'. Size of vector Delimiters (", Delimiters.size(),
		         ") does not match the number of dimensions of storage (", _numDim, ").");
	}
}

void TFileNames::checkIfDimensionMatchSizeName(std::string_view FileName, const std::vector<size_t> &Dimensions) {
	// check if each element matches
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (_names[dim]->size() != Dimensions[dim]) {
			DEVERROR("Error while writing file '", FileName, "'. Size of dimension ", dim, " of storage (",
			         Dimensions[dim], ") does not match the size of the name class of that dimension (",
			         _names[dim]->size(), ").");
		}
	}
}

void TFileNames::checkForValidPrefilledNames(size_t Dim) {
	// rule: only non-default name classes are allowed to be prefilled
	// because only for those, we can reliably re-order and skip elements!

	if (!_names[Dim]->storesNonDefaultNames() && _names[Dim]->isFilled()) {
		DEVERROR("Error while reading file '", _filename, "'. Dimension ", Dim,
		         " stores default names (indices etc.), and is prefilled. This is not allowed,"
		         "as skipping and shuffling names is not possible with indices. Don't fill the name,"
		         "or use another name class!");
	}

	// rule: only names that are written in header/as rownames are allowed to be prefilled
	// because only for those, we can reliably re-order and skip elements!
	if (_names[Dim]->isFilled() && !_nameIsWritten[Dim]) {
		DEVERROR("Error while reading file '", _filename, "'. Dimension ", Dim,
		         " has pre-defined names, but these names are not written in the file. This is not allowed,"
		         "as skipping and shuffling names is can then not reliably be done. Don't fill the name,"
		         "or use another header format!");
	}
}

bool TFileNames::nameIsWritten(size_t Dim) {
	assert(Dim < _numDim);
	return _nameIsWritten[Dim];
}

bool TFileNames::nameIsRowName(size_t Dim) {
	assert(Dim < _numDim);
	return _nameIsRowName[Dim];
}

bool TFileNames::nameIsColName(size_t Dim) {
	assert(Dim < _numDim);
	return !_nameIsRowName[Dim];
}

size_t TFileNames::getSizeNames(size_t Dim) {
	assert(Dim < _numDim);
	return _names[Dim]->size();
}

void TFileNames::_assignIndices_NamedDim(const std::vector<std::string> &UniqueNames, size_t Dim) {
	size_t length = UniqueNames.size();
	if (_nameClassIsPrefilled[Dim]) {
		// sort according to other
		_assignIndices_Sorted(UniqueNames, Dim);
	} else {
		// just store indices as they are
		_assignIndices_Unsorted(length, Dim);
	}
}

void TFileNames::_checkForUnusedNames(size_t NumStoredNames, size_t Dim) {
	if (NumStoredNames != _names[Dim]->size()) {
		for (size_t j = 0; j < _names[Dim]->size(); j++) {
			// try to find each index from names inside the stored indices
			if (std::find(_indexToStoreName[Dim].begin(), _indexToStoreName[Dim].end(), j) ==
			    _indexToStoreName[Dim].end()) {
				// not found
				UERROR("Could not find required name ", (*_names[Dim])[j], " of dimension ", Dim,
				       " in row- or column names of file ", _filename, "!");
			}
		}
	}
}

void TFileNames::_assignIndices_Sorted(const std::vector<std::string> &UniqueNames, size_t Dim) {
	// resize containers
	size_t length = UniqueNames.size();
	_indexToStoreName[Dim].resize(length, 0);
	_storeName[Dim].resize(length, false);

	// loop over names
	size_t i              = 0;
	size_t numStoredNames = 0;
	for (auto name = UniqueNames.begin(); name != UniqueNames.end(); name++, i++) {
		if (_names[Dim]->exists(*name)) {
			// name exists! -> get position in TNames
			size_t positionInNames    = _names[Dim]->getIndex(*name);
			_indexToStoreName[Dim][i] = positionInNames;
			_storeName[Dim][i]        = true;
			numStoredNames++;
		} else {
			// doesn't exist
			_storeName[Dim][i] = false;
		}
	}

	// check if there are any elements in name class that were not used
	// -> throw! Some names would not have data; and I cannot simply erase a name -> maybe used by other storage
	_checkForUnusedNames(numStoredNames, Dim);
}

void TFileNames::_assignIndices_Unsorted(size_t Length, size_t Dim) {
	// resize containers for storing indices: all to true
	_indexToStoreName[Dim].resize(Length, 0);
	for (size_t i = 0; i < Length; i++) { _indexToStoreName[Dim][i] = i; }
	_storeName[Dim].resize(Length, true);
}

void TFileNames::_setLengthNamedDimension(size_t Length, size_t Dim, std::vector<size_t> &Dimensions,
                                          std::vector<bool> &DimensionsFilled) {
	// check if this dimension already has specific value -> if yes, check if it is the same as inferred based on header
	if (DimensionsFilled[Dim] && Dimensions[Dim] != Length) {
		DEVERROR("Error while reading file ", _filename, ". Dimension ", Dim, " has already been set to ",
		         Dimensions[Dim], ", which differs from the length ", Length,
		         " that was inferred based on row/col names.");
	}
	// set dimension
	Dimensions[Dim]       = Length;
	DimensionsFilled[Dim] = true;
}

void TFileNames::setLengthUnnamedDimension(size_t Length, size_t Dim, std::vector<size_t> &Dimensions,
                                           std::vector<bool> &DimensionsFilled) {
	// check if this dimension already has specific value -> if yes, check if it is the same as inferred based on header
	if (DimensionsFilled[Dim] && Dimensions[Dim] != Length) {
		DEVERROR("Error while reading file ", _filename, ". Dimension ", Dim, " has already been set to ",
		         Dimensions[Dim], ", which differs from the length ", Length,
		         " that was inferred based on row/col names.");
	}
	// set dimension
	Dimensions[Dim]       = Length;
	DimensionsFilled[Dim] = true;

	// assign indices
	_assignIndices_Unsorted(Length, Dim);

	// resize names: only if not filled and if has default names
	if (!_nameClassIsPrefilled[Dim] && !_names[Dim]->storesNonDefaultNames()) { _names[Dim]->resize(Length); }
}

bool TFileNames::elementIsStored(size_t Dim, size_t IndexInDim) {
	// check if indices are valid
	assert(Dim < _numDim);
	if (IndexInDim >= _storeName[Dim].size()) {
		DEVERROR("IndexInDim (", IndexInDim, ") for dimension ", Dim, " is larger than size of stored names (",
		         _storeName[Dim].size(), ")!");
	}
	return _storeName[Dim][IndexInDim];
}

size_t TFileNames::indexToStoreElement(size_t Dim, size_t IndexInDim) {
	// check if indices are valid
	assert(Dim < _numDim);
	if (IndexInDim >= _indexToStoreName[Dim].size()) {
		DEVERROR("IndexInDim (", IndexInDim, ") for dimension ", Dim, " is larger than size of stored names (",
		         _indexToStoreName[Dim].size(), ")!");
	}
	// check if element is stored
	if (!elementIsStored(Dim, IndexInDim)) {
		DEVERROR("IndexInDim (", IndexInDim, ") for dimension ", Dim,
		         " is not stored! Do not ask for index where to store it.");
	}
	return _indexToStoreName[Dim][IndexInDim];
}

void TFileNames::finalizeFillNames(size_t Dim, size_t LengthFromStorage) {
	_names[Dim]->finalizeFilling();

	// check if all non-default name classes are filled
	if (_names[Dim]->storesNonDefaultNames() && !_names[Dim]->isFilled()) {
		DEVERROR("Error while reading file '", _filename, "'. Name class of dimension ", Dim,
		         " stores non-default names, but has not been filled!");
	}

	// check if size of names matches size of filled data
	if (_names[Dim]->size() != LengthFromStorage) {
		// name was not written -> ok, just set it
		if (!_nameIsWritten[Dim] && !_names[Dim]->isFilled()) {
			_names[Dim]->resize(LengthFromStorage);
		} else { // length do not match
			UERROR("Error while reading file '", _filename, "'. Size of dimension ", Dim, " of storage (",
			       LengthFromStorage, ") does not match the size of the names of that dimension (", _names[Dim]->size(),
			       ").");
		}
	}
}

const std::shared_ptr<TNamesEmpty> &TFileNames::getDimensionName(size_t Dim) { return _names[Dim]; }

//--------------------
// TRowNames
//--------------------

TRowNames::TRowNames(size_t NumDim, const std::vector<bool> &NameIsWritten,
                     const std::vector<std::shared_ptr<TNamesEmpty>> &Names, const std::vector<char> &Delimiters,
                     std::string_view FileName)
    : TFileNames(NumDim, NameIsWritten, Names, Delimiters, FileName) {
	_delimiterRowNames = '\t';

	// prepare storage
	_parsedRowNames_PerDim.resize(_numDim);
	_numSubsequentEqual_PerDim.resize(_numDim, 0);
	_previousRowName_PerDim.resize(_numDim, "");

	_correctWrittenNamesBasedOnFormat();
}

void TRowNames::_correctWrittenNamesBasedOnFormat() {
	// name is written as rowName if
	// 1) it is a rowName and
	// 2) it is written in file
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (_nameIsRowName[dim] && _nameIsWritten[dim]) {
			_nameIsWritten[dim] = true;
		} else {
			_nameIsWritten[dim] = false;
		}
	}
}

void TRowNames::setDelimiterRowNames(char DelimiterRowNames) {
	if (DelimiterRowNames == '\0') {
		DEVERROR(
		    "Can not set the delimiter for rownames to empty! When reading file, we would not know where to split.");
	}
	_delimiterRowNames = DelimiterRowNames;
}

void TRowNames::writeTitle(std::ostream *FilePointer) {
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (_nameIsWritten[dim]) { *FilePointer << _names[dim]->getTitle() << _delimiterRowNames; }
	}
}

void TRowNames::extractTitleRowNames(std::string &Line, bool ThrowIfTitleDoesntMatch) {
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (_nameIsWritten[dim]) {
			// extract title from line
			std::vector<std::string> title = _names[dim]->extractTitleFromString(Line, _delimiterRowNames);
			// only store if name class was not prefilled
			if (!_nameClassIsPrefilled[dim]) {
				_names[dim]->setTitle(title);
			} else {
				// prefilled names -> check for match, if wanted, and throw if they don't match
				if (ThrowIfTitleDoesntMatch && title != _names[dim]->getTitleVec()) {
					UERROR("Error while reading title of row names in file ", _filename, ": Title that was expected (",
					       _names[dim]->getTitle(), ") does not match title detected in file (",
					       coretools::str::concatenateString(title, _names[dim]->getDelimNames()), ")!");
				}
			}
		}
	}
}

std::string TRowNames::_extractRowName(size_t Dim, std::string &Line) {
	// split rowname
	return _names[Dim]->extractNameFromStringAndReturn(Line, _delimiterRowNames);
}

bool TRowNames::allRowNameDimensionsAreKnown(const std::vector<bool> &DimensionsFilled) {
	for (size_t dim = 1; dim < _numDim;
	     dim++) { // ignore 1st dimension: does not need to be known, as we only want to know dimensions of 1 block
		if (_nameIsRowName[dim] && !DimensionsFilled[dim]) {
			// you are a 2nd dimension or higher rowName, but you are not filled yet
			return false;
		}
	}
	return true;
}

bool TRowNames::_rowNameHasChanged(std::string_view Rowname, std::string &PreviousRowName) {
	return Rowname != PreviousRowName;
}

bool TRowNames::_rowNameJustRestarted(std::string_view Rowname, size_t Dim) {
	// find rowName in vec
	if (std::find(_parsedRowNames_PerDim[Dim].begin(), _parsedRowNames_PerDim[Dim].end(), Rowname) !=
	    _parsedRowNames_PerDim[Dim].end()) {
		// rowName is already in vec -> restarted!
		return true;
	}
	// novel rowName
	return false;
}

void TRowNames::_addNewRowName(std::string_view Rowname, size_t Dim) {
	if (std::find(_parsedRowNames_PerDim[Dim].begin(), _parsedRowNames_PerDim[Dim].end(), Rowname) ==
	    _parsedRowNames_PerDim[Dim].end()) {
		// not yet in vector -> add
		_parsedRowNames_PerDim[Dim].emplace_back(Rowname);
	}
	_numSubsequentEqual_PerDim[Dim] = 1;
	_previousRowName_PerDim[Dim]    = Rowname;
}

void TRowNames::_setRowNames(const std::vector<std::string> &UniqueNames, size_t Dim) {
	if (_nameClassIsPrefilled[Dim]) {
		// do not set, names that are already filled will be used
	} else {
		// check: did we already add names to class? -> if yes, check if they are equal
		if (_names[Dim]->isFilled()) {
			if (_names[Dim]->size() != UniqueNames.size()) {
				UERROR("While reading file ", _filename, ": Size of row names (", UniqueNames.size(), ") of dimension ",
				       Dim, " does not match expected size of row name (", _names[Dim]->size(),
				       ") that was inferred from first block.");
			}
			// check if names are equal, too
			for (size_t i = 0; i < _names[Dim]->size(); i++) {
				if ((*_names[Dim])[i] != UniqueNames[i]) {
					UERROR("While reading file ", _filename, ": Row name ", UniqueNames[i], " of dimension ", Dim,
					       " at index ", i, " does not match expected row name at that position ", (*_names[Dim])[i],
					       " that was inferred from first block.");
				}
			}
		} else {
			// first time we add names -> just fill
			for (auto &name : UniqueNames) { _names[Dim]->addNameAndSplit(name); }
		}
	}
}

void TRowNames::_setLengthSubsequentDimensions_RowName(size_t Dim, size_t ProductOverAllNext,
                                                       std::vector<size_t> &Dimensions,
                                                       std::vector<bool> &DimensionsFilled) {
	// go over all next dimensions
	// -> if only one is unknown, we can derive its length
	size_t counterUnknown         = 0;
	size_t unknownDim             = 0;
	size_t prodAllKnownDimensions = 1;
	for (size_t dim = Dim + 1; dim < _numDim; dim++) {
		if (_nameIsRowName[dim]) {
			if (!DimensionsFilled[dim]) {
				counterUnknown++;
				unknownDim = dim;
			} else {
				prodAllKnownDimensions *= Dimensions[dim];
			}
		}
	}

	if (counterUnknown == 0) {
		// check if it is the same as expected
		if (prodAllKnownDimensions != ProductOverAllNext) {
			UERROR("Error while reading file ", _filename, ". The product of all subsequent dimensions of dimension ",
			       Dim, " has already been set to ", prodAllKnownDimensions, ", which differs from the product ",
			       ProductOverAllNext, " that was inferred based on row names.");
		} // else nice, we get the same!
	} else if (counterUnknown == 1) {
		// set the unknown dimension
		size_t length = ProductOverAllNext / prodAllKnownDimensions;
		if (_nameIsWritten[unknownDim]) {
			_setLengthNamedDimension(length, unknownDim, Dimensions, DimensionsFilled);
		} else {
			setLengthUnnamedDimension(length, unknownDim, Dimensions, DimensionsFilled);
		}
	} // else too many unknowns, can't compute anything
}

void TRowNames::parseRowNames_InferDimensions(std::string &FullLine, std::vector<size_t> &Dimensions,
                                              std::vector<bool> &DimensionsFilled, bool FirstLine) {
	// how to learn unknown dimensions of 1 block?

	// 1) set by developer
	// 2) written, restarts
	//    -> length of this dimension = number of unique rownames
	// 3) written, novel number
	//    -> product over all next dimensions = number of subsequent equal rownames
	// 4) all other dimensions (for rownames) are known -> calculate

	// initialize all known dimensions with their indices
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (DimensionsFilled[dim] && _nameIsRowName[dim]) { _assignIndices_Unsorted(Dimensions[dim], dim); }
	}

	// now infer unknown dimensions
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (_nameIsWritten[dim]) {
			std::string rowName = _extractRowName(dim, FullLine);

			if (_rowNameHasChanged(rowName, _previousRowName_PerDim[dim])) {
				if (_rowNameJustRestarted(rowName, dim)) {
					// length of this dimension = number of unique rowNames
					size_t length = _parsedRowNames_PerDim[dim].size();
					_setLengthNamedDimension(length, dim, Dimensions, DimensionsFilled);

					// assign rowNames
					_assignIndices_NamedDim(_parsedRowNames_PerDim[dim], dim);
					_setRowNames(_parsedRowNames_PerDim[dim], dim);

					_parsedRowNames_PerDim[dim].clear();
				}

				// whenever rowName changes:
				// -> number of subsequent equal rowNames = product over all next dimensions
				if (!FirstLine) {
					size_t productOverAllNext = _numSubsequentEqual_PerDim[dim];
					_setLengthSubsequentDimensions_RowName(dim, productOverAllNext, Dimensions, DimensionsFilled);
				}

				// update rowName
				_addNewRowName(rowName, dim);
			} else {
				_numSubsequentEqual_PerDim[dim]++;
			}
		}
	}
}

bool TRowNames::_extractMostOuterRowName(std::istream *FilePointer, std::string_view DelimiterComments, bool &Keep,
                                         bool NewBlock) {
	// most outer row name: keep entire line or not?
	if (_nameIsWritten[0]) {
		// get rowName
		std::vector<std::string> fullName;
		if (!_names[0]->extractNameFromStreamAndFillIntoVec(FilePointer, _delimiterRowNames, DelimiterComments,
		                                                    fullName)) {
			// reached end of file
			return false;
		}
		// ok, rowName was read
		if (NewBlock) {
			// only if new block has started: check if it should be kept
			// (if multiple \n -> most outer rowname is repeated several times -> messes things up)
			if (_nameClassIsPrefilled[0]) {
				// prefilled -> check if should be kept
				Keep = _names[0]->checkIfNameShouldBeKept(fullName, _filename);
			} else {
				// not prefilled -> check if it is the same as the last one
				// -> only add if different (because rowname might be repeated many times consecutively if there are
				// other rownames)
				if (_previousMostOuterRowName != fullName) { _names[0]->addName(fullName); }
				_previousMostOuterRowName = fullName;
				Keep                      = true;
			}
		}
	}
	return true;
}

void TRowNames::_checkIfBlockRowNamesMatchFirstBlock(const std::vector<std::string> &Name, size_t Dim,
                                                     const std::vector<size_t> &BlockCoordinates) {
	// check if rownames of subsequent blocks match what we inferred based on first block!
	size_t indexInFile = BlockCoordinates[Dim - 1]; // Dim-1 because this does not contain first (most outer) dimension
	if (elementIsStored(Dim, indexInFile)) {
		size_t indexInNameClass = indexToStoreElement(Dim, indexInFile);
		if (_names[Dim]->getName(indexInNameClass) != Name) {
			UERROR("While reading file ", _filename, ": Row name ",
			       coretools::str::concatenateString(Name, _names[Dim]->getDelimNames()), " of dimension ", Dim,
			       " at index ", indexInFile, " does not match expected row name at that position ",
			       (*_names[Dim])[indexInNameClass], " that was inferred from first block.");
		}
	}
}

bool TRowNames::_extractBlockRowNames(std::istream *FilePointer, std::string_view DelimiterComments,
                                      const std::vector<size_t> &BlockCoordinates) {
	for (size_t dim = 1; dim < _numDim; dim++) { // skip first dimension: is not part of block
		if (_nameIsWritten[dim]) {
			std::vector<std::string> name;
			if (!_names[dim]->extractNameFromStreamAndFillIntoVec(FilePointer, _delimiterRowNames, DelimiterComments,
			                                                      name)) {
				return false; // reached end of file
			}
			_checkIfBlockRowNamesMatchFirstBlock(name, dim, BlockCoordinates);
		}
	}
	return true;
}

bool TRowNames::extractRowNames(std::istream *FilePointer, std::string_view DelimiterComments,
                                const std::vector<size_t> &FullCoordinates, bool &Keep, bool NewBlock) {
	// first most outer rowname
	if (_extractMostOuterRowName(FilePointer, DelimiterComments, Keep, NewBlock)) {
		// now block rownames
		if (_extractBlockRowNames(FilePointer, DelimiterComments, FullCoordinates)) { return true; }
	}
	return false;
}

//--------------------
// TColNameBase
//--------------------

TColNameBase::TColNameBase(size_t NumDim, const std::vector<bool> &NameIsWritten,
                           const std::vector<std::shared_ptr<TNamesEmpty>> &Names, const std::vector<char> &Delimiters,
                           std::string_view FileName)
    : TFileNames(NumDim, NameIsWritten, Names, Delimiters, FileName) {}

void TColNameBase::initialize(const std::vector<char> &Delimiters) {
	// can not call this from within the constructor since virtual methods are called
	_correctWrittenNamesBasedOnFormat(Delimiters);
	_checkDelimNames(Delimiters);
}

void TColNameBase::setDelimiterConcatenation(char) {
	DEVERROR("Can not set Delimiter for concatenation for base class/deriving classes that do not explicitly override "
	         "this function!");
}

void TColNameBase::_checkDelimConcatenation(char DelimiterMostOuterColName, char DelimConcatenation) {
	// rule: delimiter for concatenation can not be the same as the delimiter of the most outer colname!
	if (DelimiterMostOuterColName == DelimConcatenation) {
		std::string delimMostOuterString = {DelimiterMostOuterColName};
		DEVERROR("Delimiter of most outer colname (", delimMostOuterString,
		         ") is the same as the delimiter that is used for concatenation! This is currently not allowed, "
		         "because I can not distinguish dimensions.");
	}
}

void TColNameBase::_checkDelimNames(const std::vector<char> &Delimiters) {
	// rule: delimiter for names (if complexity > 1) can not be the same as any of the delimiters used for dimensions!
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (_nameIsWritten[dim]) {
			if (std::find(Delimiters.begin(), Delimiters.end(), _names[dim]->getDelimNames()) != Delimiters.end()) {
				std::string delimAsString = {_names[dim]->getDelimNames()};
				DEVERROR("Delimiter that is used for pasting colnames of dimension ", dim, " (", delimAsString,
				         ") is the same as the delimiter that is used for separating dimensions! This is currently "
				         "not allowed, because I can not distinguish dimensions.");
			}
		}
	}
}

char TColNameBase::_getDelimMostOuterColumn(const std::vector<char> &Delimiters) {
	// return delimiter of first column (even if colname is not written!)
	for (size_t dim = 0; dim < _nameIsRowName.size(); dim++) {
		if (!_nameIsRowName[dim]) { return Delimiters[dim]; }
	}
	return '\0';
}

bool TColNameBase::_hasHeader() {
	// returns true if there is at least one colname that is written
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (_nameIsWritten[dim]) { return true; }
	}
	return false;
}

template<typename T> std::vector<T> TColNameBase::_getVecRelevantForHeader(const std::vector<T> &Vec) {
	// remove all entries that are rowNames
	std::vector<T> headerVec;
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (!_nameIsRowName[dim]) { headerVec.push_back(Vec[dim]); }
	}
	return headerVec;
}

void TColNameBase::_correctWrittenNamesBasedOnFormat(const std::vector<char> &Delimiters) {
	// name is written as colName if
	// 1) it is a colName
	// 2) and developer wants to write it
	// 3) and header format supports writing it
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (!_nameIsRowName[dim] && _nameIsWritten[dim] && _formatWritesColName(dim, Delimiters)) {
			_nameIsWritten[dim] = true;
		} else {
			_nameIsWritten[dim] = false;
		}
	}
}

void TColNameBase::writeHeader(std::ostream *FilePointer, const std::vector<size_t> &Dimensions,
                               const std::vector<char> &Delimiters, const std::unique_ptr<TRowNames> &RowNames) {
	if (_hasHeader()) {
		// get dimensions and delimiters relevant for header from full
		std::vector<size_t> headerDimensions = _getVecRelevantForHeader(Dimensions);
		std::vector<char> headerDelimiters   = _getVecRelevantForHeader(Delimiters);

		// construct lookup tables
		TDataBlock headerBlock;
		std::vector<std::vector<size_t>> coordinatesOneBlock = headerBlock.getCoordinatesOneBlock(headerDimensions);
		std::vector<char> delimitersOneBlock                 = headerBlock.getDelimiterLookupOneBlock(
		                    headerDelimiters, headerDimensions, '\n'); // end each header with a \n

		// now write header!
		_writeHeader(FilePointer, headerDimensions, headerDelimiters, coordinatesOneBlock, delimitersOneBlock,
		             RowNames);
	}
}

size_t TColNameBase::numLinesHeader() {
	if (_hasHeader()) {
		return 1;
	} else {
		return 0;
	}
}

void TColNameBase::parseHeader(std::vector<std::string> &Header, std::vector<size_t> &Dimensions,
                               std::vector<bool> &DimensionsFilled, const std::vector<char> &Delimiters,
                               const std::unique_ptr<TRowNames> &RowNames, bool ThrowIfTitleDoesntMatch) {
	// first assign indices of all dimensions that are already known
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (DimensionsFilled[dim] && !_nameIsRowName[dim]) { _assignIndices_Unsorted(Dimensions[dim], dim); }
	}

	// now parse header
	if (_hasHeader()) {
		_parseHeader(Header, Dimensions, DimensionsFilled, Delimiters, RowNames, ThrowIfTitleDoesntMatch);
	}
}

void TColNameBase::_setColNames(const std::vector<std::string> &UniqueNames, size_t Dim) {
	if (_nameClassIsPrefilled[Dim]) {
		// do not set, names that are already filled will be used
	} else {
		// first time we add names -> just fill
		for (auto &name : UniqueNames) { _names[Dim]->addNameAndSplit(name); }
	}
}

void TColNameBase::checkIfHeaderMatchesInferredDimensions(std::vector<std::string> &Header,
                                                          const std::vector<size_t> &Dimensions,
                                                          const std::vector<char> &Delimiters,
                                                          const std::unique_ptr<TRowNames> &RowNames) {
	if (_hasHeader()) {
		// get dimensions and delimiters relevant for header from full
		std::vector<size_t> headerDimensions = _getVecRelevantForHeader(Dimensions);
		std::vector<char> headerDelimiters   = _getVecRelevantForHeader(Delimiters);

		// construct lookup tables
		TDataBlock headerBlock;
		std::vector<std::vector<size_t>> coordinatesOneBlock = headerBlock.getCoordinatesOneBlock(headerDimensions);
		std::vector<char> delimitersOneBlock                 = headerBlock.getDelimiterLookupOneBlock(
		                    headerDelimiters, headerDimensions, '\n'); // end each header with a \n

		// now write header!
		_checkIfHeaderMatchesInferredDimensions(Header, headerDimensions, headerDelimiters, coordinatesOneBlock,
		                                        delimitersOneBlock, RowNames);
	}
}

//--------------------
// THeaderMultiLine
//--------------------
TColNameMultiLine::TColNameMultiLine(size_t NumDim, const std::vector<bool> &NameIsWritten,
                                     const std::vector<std::shared_ptr<TNamesEmpty>> &Names,
                                     const std::vector<char> &Delimiters, std::string_view FileName)
    : TColNameBase(NumDim, NameIsWritten, Names, Delimiters, FileName) {
	// format:
	// -> write header in n lines (as many as developer wishes to write)
	// -> e.g. delimiters are {'\n', '\t', '\t', '_'} -> write 2nd, 3rd and 4th colname as header, one below each other
	// -> the "upper" colnames must be repeated as many times as the product of the lower dimensions
	// -> the elements are separated among each other with the same delimiter as in the data
	// e.g.:
	// one  one one
	// a    b   c
	// 1_2_3    1_2_3   1_2_3
}

bool TColNameMultiLine::_formatWritesColName(size_t, const std::vector<char> &) {
	// multi-line: name will always be written
	return true;
}

size_t TColNameMultiLine::_getJumpSizeHeader(size_t CurDim, const std::vector<size_t> &Dimensions,
                                             const std::vector<char> &Delimiters) {
	size_t numDim = Dimensions.size();

	size_t jumpSize = 1;
	for (size_t nextDim = CurDim + 1; nextDim < numDim; nextDim++) {
		if (Delimiters[CurDim] != Delimiters[nextDim]) { jumpSize *= Dimensions[nextDim]; }
	}

	return jumpSize;
}

void TColNameMultiLine::_writeHeader(std::ostream *FilePointer, const std::vector<size_t> &HeaderDimensions,
                                     const std::vector<char> &HeaderDelimiters,
                                     const std::vector<std::vector<size_t>> &CoordinatesOneBlock,
                                     const std::vector<char> &DelimitersOneBlock,
                                     const std::unique_ptr<TRowNames> &RowNames) {
	size_t headerDim = 0;
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (!_nameIsRowName[dim]) {
			if (_nameIsWritten[dim]) {
				// first write title for rownames
				RowNames->writeTitle(FilePointer);

				// now write colnames
				size_t jumpSize = _getJumpSizeHeader(headerDim, HeaderDimensions, HeaderDelimiters);
				for (size_t i = jumpSize - 1; i < CoordinatesOneBlock.size(); i += jumpSize) {
					size_t index     = CoordinatesOneBlock[i][headerDim];
					std::string name = (*_names[dim])[index];
					*FilePointer << name << DelimitersOneBlock[i];
				}
			}
			headerDim++;
		}
	}
}

size_t TColNameMultiLine::numLinesHeader() {
	size_t numLines = 0;
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (_nameIsWritten[dim]) { numLines++; }
	}
	return numLines;
}

void TColNameMultiLine::_checkIfHeaderMatchesInferredDimensions(
    std::vector<std::string> &Header, const std::vector<size_t> &HeaderDimensions,
    const std::vector<char> &HeaderDelimiters, const std::vector<std::vector<size_t>> &CoordinatesOneBlock,
    const std::vector<char> &DelimitersOneBlock, const std::unique_ptr<TRowNames> &RowNames) {
	// now that we inferred dimensions from colnames: check if they actually match our expectations (i.e. number of
	// repetitions etc)
	size_t headerDim  = 0;
	size_t writtenDim = 0;
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (!_nameIsRowName[dim]) {
			if (_nameIsWritten[dim]) {
				// remove title for rownames
				RowNames->extractTitleRowNames(Header[writtenDim], false); // don't throw, checked that before

				// now check colnames
				size_t jumpSize = _getJumpSizeHeader(headerDim, HeaderDimensions, HeaderDelimiters);
				for (size_t i = jumpSize - 1; i < CoordinatesOneBlock.size(); i += jumpSize) {
					size_t index             = CoordinatesOneBlock[i][headerDim];
					std::string nameFromFile = coretools::str::split(Header[writtenDim], DelimitersOneBlock[i]);
					if (elementIsStored(dim, index)) { // account for re-arranged names
						size_t indexStoredElement              = indexToStoreElement(dim, index);
						std::string nameFromInferredDimensions = (*_names[dim])[indexStoredElement];
						if (nameFromInferredDimensions != nameFromFile) {
							DEVERROR("While parsing multi-line header of file ", _filename, ": Colname at line ",
							         writtenDim, " and index ", index, " (", nameFromFile,
							         ") does not match expected column name (", nameFromInferredDimensions, ")!");
						}
					}
				}
				writtenDim++;
			}
			headerDim++;
		}
	}
}

std::vector<std::string> TColNameMultiLine::_getUniqueColNames_ForDim(size_t Dim, std::string_view Colnames_ThisDim,
                                                                      const std::vector<char> &Delimiters) {
	// first remove delimiters of upper dimensions
	std::vector<std::string> trimmed;
	std::string trimmedString{Colnames_ThisDim};
	for (size_t previousDim = 0; previousDim < Dim; previousDim++) {
		if (_nameIsWritten[previousDim] && Delimiters[previousDim] != Delimiters[Dim]) {
			coretools::str::fillContainerFromString(trimmedString, trimmed, Delimiters[previousDim]);
			trimmedString = trimmed[0];
		}
	}

	// now get names of current dimension:
	// -> split at delimiter of that dimension
	std::vector<std::string> vecNames_OneDimension;
	coretools::str::fillContainerFromString(trimmedString, vecNames_OneDimension, Delimiters[Dim]);
	// only keep unique elements
	_removeDuplicatesFromVec(vecNames_OneDimension);

	return vecNames_OneDimension;
}

void TColNameMultiLine::_parseHeader(std::vector<std::string> &Header, std::vector<size_t> &Dimensions,
                                     std::vector<bool> &DimensionsFilled, const std::vector<char> &Delimiters,
                                     const std::unique_ptr<TRowNames> &RowNames, bool ThrowIfTitleDoesntMatch) {
	size_t row = 0;
	// loop over all dimensions
	for (size_t dim = 0; dim < _numDim; dim++) {
		// is this dimension written as a colname?
		if (_nameIsWritten[dim]) {
			// remove title of rowName
			RowNames->extractTitleRowNames(Header[row], ThrowIfTitleDoesntMatch);

			// get unique column names for current dimension
			std::vector<std::string> uniqueNames_OneDimension = _getUniqueColNames_ForDim(dim, Header[row], Delimiters);
			size_t length                                     = uniqueNames_OneDimension.size();

			// set length of dimension
			_setLengthNamedDimension(length, dim, Dimensions, DimensionsFilled);

			// assign names
			_assignIndices_NamedDim(uniqueNames_OneDimension, dim);
			_setColNames(uniqueNames_OneDimension, dim);

			// increase counter
			row++;
		}
	}
}

//--------------------
// THeaderConcatenated
//--------------------

TColNameConcatenated::TColNameConcatenated(size_t NumDim, const std::vector<bool> &NameIsWritten,
                                           const std::vector<std::shared_ptr<TNamesEmpty>> &Names,
                                           const std::vector<char> &Delimiters, std::string_view FileName)
    : TColNameBase(NumDim, NameIsWritten, Names, Delimiters, FileName) {
	// format:
	// -> write header in 1 line (linearized)
	// -> only write all dimension names that have the same delimiter as the most outer colname has (typically tab)
	// -> e.g. delimiters are {'\n', '\t', '\t', '_'} -> only write 2nd and 3rd colname as header
	// -> those are pasted together with 'DelimConcatenation'
	// e.g.:
	// one_a one_b    one_c

	_delimConcatenation = '_';
}

void TColNameConcatenated::setDelimiterConcatenation(char DelimConcatenation) {
	if (DelimConcatenation == '\0') {
		DEVERROR("Can not set the delimiter for concatenating colnames to empty (\\0)! When reading file, we would not "
		         "know where to split.");
	}
	_delimConcatenation = DelimConcatenation;
}

bool TColNameConcatenated::_formatWritesColName(size_t Dim, const std::vector<char> &Delimiters) {
	// concatenated: name will always be written if its delimiter is the same as the most the one of the most outer
	// colname (!) (even if this most outer colname is not written itself) if developer wants colname to be written, but
	// delimiter is different than most outer colname -> will not write it
	char delimMostOuter = _getDelimMostOuterColumn(Delimiters);

	if (Delimiters[Dim] == delimMostOuter) { return true; }
	return false;
}

size_t TColNameConcatenated::_getJumpSizeHeader(const std::vector<size_t> &Dimensions,
                                                const std::vector<char> &Delimiters) {
	size_t numDim = Dimensions.size();

	size_t jumpSize = 1;
	for (size_t dim = 1; dim < numDim; dim++) {
		if (Delimiters[0] != Delimiters[dim]) { jumpSize *= Dimensions[dim]; }
	}

	return jumpSize;
}

void TColNameConcatenated::_writeHeader(std::ostream *FilePointer, const std::vector<size_t> &HeaderDimensions,
                                        const std::vector<char> &HeaderDelimiters,
                                        const std::vector<std::vector<size_t>> &CoordinatesOneBlock,
                                        const std::vector<char> &, const std::unique_ptr<TRowNames> &RowNames) {
	// check on validity
	_checkDelimConcatenation(HeaderDelimiters[0], _delimConcatenation);

	// first write title for rownames
	RowNames->writeTitle(FilePointer);

	char delimMostUpper = HeaderDelimiters[0];
	size_t jumpSize     = _getJumpSizeHeader(HeaderDimensions, HeaderDelimiters);
	for (size_t i = jumpSize - 1; i < CoordinatesOneBlock.size(); i += jumpSize) {
		size_t headerDim = 0;
		bool first       = true;
		if (i != jumpSize - 1) { // for all except first: write delimiter
			*FilePointer << delimMostUpper;
		}
		for (size_t dim = 0; dim < _numDim; dim++) {
			if (!_nameIsRowName[dim]) {
				if (_nameIsWritten[dim]) {
					size_t index     = CoordinatesOneBlock[i][headerDim];
					std::string name = (*_names[dim])[index];
					if (first) {
						first = false;
					} else {
						*FilePointer << _delimConcatenation;
					}
					*FilePointer << name;
				}
				headerDim++;
			}
		}
	}
	*FilePointer << '\n';
}

void TColNameConcatenated::_checkIfHeaderMatchesInferredDimensions(
    std::vector<std::string> &Header, const std::vector<size_t> &HeaderDimensions,
    const std::vector<char> &HeaderDelimiters, const std::vector<std::vector<size_t>> &CoordinatesOneBlock,
    const std::vector<char> &, const std::unique_ptr<TRowNames> &RowNames) {
	// now that we inferred dimensions from colnames: check if they actually match our expectations (i.e. number of
	// repetitions etc) remove title of rowName
	RowNames->extractTitleRowNames(Header[0], false); // dont' throw if they don't match, checked that before

	char delimMostUpper                = HeaderDelimiters[0];
	std::string delimConcatAsString    = {_delimConcatenation};
	std::string delimMostUpperAsString = {delimMostUpper};
	size_t jumpSize                    = _getJumpSizeHeader(HeaderDimensions, HeaderDelimiters);
	for (size_t i = jumpSize - 1; i < CoordinatesOneBlock.size(); i += jumpSize) {
		size_t headerDim = 0;
		bool first       = true;
		for (size_t dim = 0; dim < _numDim; dim++) {
			if (!_nameIsRowName[dim]) {
				if (_nameIsWritten[dim]) {
					size_t index             = CoordinatesOneBlock[i][headerDim];
					std::string nameFromFile = coretools::str::split(
					    Header[0], delimConcatAsString + delimMostUpperAsString,
					    true); // split at either delimConcat or delimMostUpper (too lazy to make distinction here)
					if (first) { first = false; }
					if (elementIsStored(dim, index)) { // account for re-arranged names
						size_t indexStoredElement              = indexToStoreElement(dim, index);
						std::string nameFromInferredDimensions = (*_names[dim])[indexStoredElement];
						if (nameFromInferredDimensions != nameFromFile) {
							DEVERROR("While parsing concatenated header of file ", _filename, ": Colname at dimension ",
							         headerDim, " and index ", index, " (", nameFromFile,
							         ") does not match expected column name (", nameFromInferredDimensions, ")!");
						}
					}
				}
				headerDim++;
			}
		}
	}
}

std::vector<std::vector<std::string>> TColNameConcatenated::_getUniqueColNames(std::string_view Header,
                                                                               const std::vector<char> &Delimiters) {
	// split string at this delim
	std::vector<std::string> vec;
	coretools::str::fillContainerFromString(Header, vec, _getDelimMostOuterColumn(Delimiters));

	// now go over each entry assign to unique names
	size_t previousSize = 0;
	std::vector<std::vector<std::string>> namesVec(_numDim);
	for (auto &concatName : vec) {
		// split at delim that is used for concatenation
		std::vector<std::string> names_oneCell;
		coretools::str::fillContainerFromString(concatName, names_oneCell, _delimConcatenation);

		// check if size matches expectation
		if (previousSize == 0) {
			previousSize = names_oneCell.size();
		} else if (names_oneCell.size() != previousSize) {
			std::string delimString = {_delimConcatenation};
			UERROR("While reading colnames of concatenated header of file ", _filename, ": Number of ", delimString,
			       " delimited-elements in first header entry (", previousSize,
			       ") differs from the number of elements in subsequent entry (", names_oneCell.size(), ")!");
		}
		auto it = names_oneCell.begin();
		for (size_t dim = 0; dim < _numDim; dim++) {
			if (_nameIsWritten[dim]) {
				if (std::find(namesVec[dim].begin(), namesVec[dim].end(), *it) == namesVec[dim].end()) {
					// not yet in vector -> add!
					namesVec[dim].push_back(*it);
				}
				it++;
			}
		}
	}

	return namesVec;
}

void TColNameConcatenated::_parseHeader(std::vector<std::string> &Header, std::vector<size_t> &Dimensions,
                                        std::vector<bool> &DimensionsFilled, const std::vector<char> &Delimiters,
                                        const std::unique_ptr<TRowNames> &RowNames, bool ThrowIfTitleDoesntMatch) {
	// check on validity
	assert(Header.size() == 1);
	_checkDelimConcatenation(_getDelimMostOuterColumn(Delimiters), _delimConcatenation);

	// remove title of rowName
	RowNames->extractTitleRowNames(Header[0], ThrowIfTitleDoesntMatch);

	// get unique column names for each dimension
	std::vector<std::vector<std::string>> uniqueNames = _getUniqueColNames(Header[0], Delimiters);

	// now loop over all dimensions, derive length and names
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (_nameIsWritten[dim]) {
			size_t length = uniqueNames[dim].size();

			// set length of dimension
			_setLengthNamedDimension(length, dim, Dimensions, DimensionsFilled);

			// assign names
			_assignIndices_NamedDim(uniqueNames[dim], dim);
			_setColNames(uniqueNames[dim], dim);
		}
	}
}

//--------------------
// THeaderFull
//--------------------

TColNameFull::TColNameFull(size_t NumDim, const std::vector<bool> &NameIsWritten,
                           const std::vector<std::shared_ptr<TNamesEmpty>> &Names, const std::vector<char> &Delimiters,
                           std::string_view FileName)
    : TColNameBase(NumDim, NameIsWritten, Names, Delimiters, FileName) {
	// format:
	// -> write header in 1 line (linearized)
	// -> write all dimension names just like they are written as data
	// -> e.g. delimiters are {'\n', '\t', '\t', '_'} -> write 2nd, 3rd and 4th colname as header
	// -> all colnames encoding one single element are pasted together with 'DelimConcatenation'
	// -> and those are then pasted with the others with the same delimiter as in the data
	// e.g.:
	// onea1_onea2_onea3

	_delimConcatenation = '_';
}

void TColNameFull::setDelimiterConcatenation(char DelimConcatenation) {
	if (DelimConcatenation == '\0') {
		DEVERROR("Can not set the delimiter for concatenating colnames to empty (\\0)! When reading file, we would not "
		         "know where to split.");
	}
	_delimConcatenation = DelimConcatenation;
}

bool TColNameFull::_formatWritesColName(size_t, const std::vector<char> &) {
	// full: name will always be written if 1) it is a colname and 2) developer wants it to write
	return true;
}

void TColNameFull::_writeHeader(std::ostream *FilePointer, const std::vector<size_t> &,
                                const std::vector<char> &HeaderDelimiters,
                                const std::vector<std::vector<size_t>> &CoordinatesOneBlock,
                                const std::vector<char> &DelimitersOneBlock,
                                const std::unique_ptr<TRowNames> &RowNames) {
	// check on validity
	_checkDelimConcatenation(HeaderDelimiters[0], _delimConcatenation);

	// first write title for rownames
	RowNames->writeTitle(FilePointer);

	for (size_t i = 0; i < CoordinatesOneBlock.size(); i++) {
		size_t headerDim = 0;
		bool first       = true;
		for (size_t dim = 0; dim < _numDim; dim++) {
			if (!_nameIsRowName[dim]) {
				if (_nameIsWritten[dim]) {
					size_t index     = CoordinatesOneBlock[i][headerDim];
					std::string name = (*_names[dim])[index];
					if (first) {
						first = false;
					} else {
						*FilePointer << _delimConcatenation;
					}
					*FilePointer << name;
				}
				headerDim++;
			}
		}
		*FilePointer << DelimitersOneBlock[i];
	}
}

std::string TColNameFull::_getAllDelims_AsString(const std::vector<char> &Delimiters) {
	std::string allDelimsAsString;
	for (auto &it : Delimiters) {
		std::string tmp = {it};
		allDelimsAsString += tmp;
	}
	return allDelimsAsString;
}

size_t TColNameFull::_getLastDimension_ThatIsWritten() {
	size_t lastWrittenDim = 0;
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (_nameIsWritten[dim]) { lastWrittenDim = dim; }
	}
	return lastWrittenDim;
}

std::vector<std::vector<std::string>> TColNameFull::_getUniqueColNames(std::string &Header,
                                                                       const std::vector<char> &Delimiters) {
	// collect all delimiters as one string (used to split any)
	std::string allDelimsAsString = _getAllDelims_AsString(Delimiters);

	// determine which dimension is the last one to be written
	size_t lastWrittenDim = _getLastDimension_ThatIsWritten();

	std::vector<std::vector<std::string>> namesVec(_numDim);
	std::string name;
	while (!Header.empty()) {
		for (size_t dim = 0; dim < _numDim; dim++) {
			if (dim == lastWrittenDim) {
				break;
			} else if (_nameIsWritten[dim]) {
				// split off one element: split at delimiter for concatenation
				name = coretools::str::split(Header, _delimConcatenation);
				if (std::find(namesVec[dim].begin(), namesVec[dim].end(), name) == namesVec[dim].end()) {
					// not yet in vec -> add
					namesVec[dim].push_back(name);
				}
			}
		}
		// now remove delimiter of columns
		// as we don't know dimensions ahead, this could be any of the delimiters
		name = coretools::str::split(Header, allDelimsAsString, true);
		if (std::find(namesVec[lastWrittenDim].begin(), namesVec[lastWrittenDim].end(), name) ==
		    namesVec[lastWrittenDim].end()) {
			// not yet in vec -> add
			namesVec[lastWrittenDim].push_back(name);
		}
	}

	return namesVec;
}

void TColNameFull::_parseHeader(std::vector<std::string> &Header, std::vector<size_t> &Dimensions,
                                std::vector<bool> &DimensionsFilled, const std::vector<char> &Delimiters,
                                const std::unique_ptr<TRowNames> &RowNames, bool ThrowIfTitleDoesntMatch) {
	// check on validity
	assert(Header.size() == 1);
	_checkDelimConcatenation(_getDelimMostOuterColumn(Delimiters), _delimConcatenation);

	// remove title of rowName
	RowNames->extractTitleRowNames(Header[0], ThrowIfTitleDoesntMatch);

	// get unique column names for each dimension
	std::vector<std::vector<std::string>> uniqueNames = _getUniqueColNames(Header[0], Delimiters);

	// now loop over all dimensions, derive length and names
	for (size_t dim = 0; dim < _numDim; dim++) {
		if (_nameIsWritten[dim]) {
			size_t length = uniqueNames[dim].size();

			// set length of dimension
			_setLengthNamedDimension(length, dim, Dimensions, DimensionsFilled);

			// assign names
			_assignIndices_NamedDim(uniqueNames[dim], dim);
			_setColNames(uniqueNames[dim], dim);
		}
	}
}

void TColNameFull::_checkIfHeaderMatchesInferredDimensions(std::vector<std::string> &Header,
                                                           const std::vector<size_t> &,
                                                           const std::vector<char> &HeaderDelimiters,
                                                           const std::vector<std::vector<size_t>> &CoordinatesOneBlock,
                                                           const std::vector<char> &,
                                                           const std::unique_ptr<TRowNames> &RowNames) {
	// now that we inferred dimensions from colnames: check if they actually match our expectations (i.e. number of
	// repetitions etc) remove title of rowName
	RowNames->extractTitleRowNames(Header[0], false); // dont' throw if they don't match, checked that before

	// collect all delimiters as one string (used to split any)
	std::string allDelimsAsString = _getAllDelims_AsString(HeaderDelimiters);
	allDelimsAsString += _delimConcatenation;

	for (size_t i = 0; i < CoordinatesOneBlock.size(); i++) {
		size_t headerDim = 0;
		bool first       = true;
		for (size_t dim = 0; dim < _numDim; dim++) {
			if (!_nameIsRowName[dim]) {
				if (_nameIsWritten[dim]) {
					size_t index             = CoordinatesOneBlock[i][headerDim];
					std::string nameFromFile = coretools::str::split(
					    Header[0], allDelimsAsString,
					    true); // split at either delimConcat or some other delim (too lazy to make distinction here)
					if (first) { first = false; }
					if (elementIsStored(dim, index)) { // account for re-arranged names
						size_t indexStoredElement              = indexToStoreElement(dim, index);
						std::string nameFromInferredDimensions = (*_names[dim])[indexStoredElement];
						if (nameFromInferredDimensions != nameFromFile) {
							DEVERROR("While parsing full header of file ", _filename, ": Colname at dimension ",
							         headerDim, " and index ", index, " (", nameFromFile,
							         ") does not match expected column name (", nameFromInferredDimensions, ")!");
						}
					}
				}
				headerDim++;
			}
		}
	}
}

}; // end namespace coretools
