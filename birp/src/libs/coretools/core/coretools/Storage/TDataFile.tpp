//
// Created by madleina on 20.04.21.
//

#include "coretools/Main/TError.h"
#include "coretools/Strings/stringManipulations.h"
#include <fstream>

namespace coretools {

namespace impl {
template<typename StreamType>
bool readUntilDelimiter(StreamType *FilePointer, std::string &String, char Delimiter,
                        std::string_view DelimiterComment = "//") {
	String.clear();
	if (FilePointer->good() && !FilePointer->eof()) {
		std::getline(*FilePointer, String, Delimiter);

		// skip comments
		String = str::extractBefore(String, DelimiterComment, false);
	}

	if (!FilePointer->good() || FilePointer->eof()) { return false; }
	return true;
}
}

template<typename T> std::vector<T> TDataFileBase::_getAllExceptFirst(const std::vector<T> &Vec) {
	// remove first (most outer one) from vector
	std::vector<T> oneBlock = Vec;
	oneBlock.erase(oneBlock.begin());

	return oneBlock;
}

//--------------------
// TDataWriterBase
//--------------------

template<typename Type, size_t NumDim> TDataWriterBase<Type, NumDim>::TDataWriterBase() {
	_filePointer = nullptr;
	_isOpen      = false;
}

template<typename Type, size_t NumDim> TDataWriterBase<Type, NumDim>::~TDataWriterBase() { _closeFile(); }

template<typename Type, size_t NumDim> void TDataWriterBase<Type, NumDim>::_openFile() {
	_filePointer = new std::ofstream(_filename.c_str());
	_isOpen = true;

	if (!(*_filePointer) || _filePointer->fail() || !_filePointer->good()) {
		throw TUserError("Failed to open file '", _filename, "' for writing!");
	}
}

template<typename Type, size_t NumDim> void TDataWriterBase<Type, NumDim>::_closeFile() {
	if (_isOpen) {
		delete _filePointer;
		_isOpen = false;
	}
}

template<typename Type, size_t NumDim>
void TDataWriterBase<Type, NumDim>::write(const std::vector<std::shared_ptr<TNamesEmpty>> &Names,
                                          std::string_view Filename, TMultiDimensionalStorage<Type, NumDim> *Storage,
                                          const std::vector<char> &Delimiters, const std::vector<bool> &NameIsWritten,
                                          const colNameTypes &ColNameType) {
	// names are given as vector
	// initialize and write
	_initialize(Names, Filename, Storage, Delimiters, NameIsWritten, ColNameType);
}

template<typename Type, size_t NumDim>
void TDataWriterBase<Type, NumDim>::write(std::string_view Filename, TMultiDimensionalStorage<Type, NumDim> *Storage,
                                          const std::vector<char> &Delimiters, const std::vector<bool> &NameIsWritten,
                                          const colNameTypes &ColNameType) {
	// extract names from Storage
	std::vector<std::shared_ptr<TNamesEmpty>> names = _extractNamesFromStorage(Storage);
	// now initialize and write
	_initialize(names, Filename, Storage, Delimiters, NameIsWritten, ColNameType);
}

template<typename Type, size_t NumDim>
void TDataWriterBase<Type, NumDim>::_initialize(const std::vector<std::shared_ptr<TNamesEmpty>> &Names,
                                                std::string_view Filename,
                                                TMultiDimensionalStorage<Type, NumDim> *Storage,
                                                const std::vector<char> &Delimiters,
                                                const std::vector<bool> &NameIsWritten,
                                                const colNameTypes &ColNameType) {
	// set member variables
	_filename   = Filename;
	_storage    = Storage;
	_delimiters = Delimiters;
	_dimensionsVec.resize(NumDim);
	for (size_t i = 0; i < NumDim; i++) { _dimensionsVec[i] = Storage->dimensions()[i]; }

	// initialize row- and colNames
	_rowNames = std::make_unique<TRowNames>(NumDim, NameIsWritten, Names, _delimiters, _filename);
	if (ColNameType == colNames_multiline) {
		_colNames = std::make_unique<TColNameMultiLine>(NumDim, NameIsWritten, Names, _delimiters, _filename);
	} else if (ColNameType == colNames_concatenated) {
		_colNames = std::make_unique<TColNameConcatenated>(NumDim, NameIsWritten, Names, _delimiters, _filename);
	} else {
		_colNames = std::make_unique<TColNameFull>(NumDim, NameIsWritten, Names, _delimiters, _filename);
	}
	_colNames->initialize(_delimiters);

	// check on valid input
	_checkInput();

	// open file
	_openFile();

	// write header
	_writeHeader();

	// write rest
	_write();

	// close
	_closeFile();
}

template<typename Type, size_t NumDim> void TDataWriterBase<Type, NumDim>::_checkInput() {
	this->_checkDelimiters();
	_rowNames->checkIfDimensionMatchSizeName(_filename, _dimensionsVec);
	_colNames->checkIfDimensionMatchSizeName(_filename, _dimensionsVec);
}

template<typename Type, size_t NumDim>
std::vector<std::shared_ptr<TNamesEmpty>>
TDataWriterBase<Type, NumDim>::_extractNamesFromStorage(TMultiDimensionalStorage<Type, NumDim> *Storage) {
	// fill pointer to name class from Storage into vector
	// reason: at some point, we might want to use other name classes than the one the Storage stores for writing
	// -> just need to replace element in vector
	std::vector<std::shared_ptr<TNamesEmpty>> names(NumDim);
	for (size_t dim = 0; dim < NumDim; dim++) { names[dim] = Storage->getDimensionName(dim); }
	return names;
}

template<typename Type, size_t NumDim> void TDataWriterBase<Type, NumDim>::_writeHeader() {
	_colNames->writeHeader(_filePointer, _dimensionsVec, _delimiters, _rowNames);
}

template<typename Type, size_t NumDim> void TDataWriterBase<Type, NumDim>::_write() {
	// construct delimiters and coordinates for one block (= all except most outer dimension)
	std::vector<size_t> dimensionsOneBlock = this->_getAllExceptFirst(_dimensionsVec);
	std::vector<char> delimitersOneBlock   = this->_getAllExceptFirst(_delimiters);

	// create lookup tables for delimiters and coordinates
	std::vector<char> delimitersLookupOneBlock =
	    TDataBlock::getDelimiterLookupOneBlock(delimitersOneBlock, dimensionsOneBlock, _delimiters[0]);
	std::vector<std::vector<size_t>> coordinatesOneBlock = TDataBlock::getCoordinatesOneBlock(dimensionsOneBlock);

	_write(dimensionsOneBlock, delimitersOneBlock, delimitersLookupOneBlock, coordinatesOneBlock);
}

template<typename Type, size_t NumDim>
void TDataWriterBase<Type, NumDim>::_ensureLastBlockEndsWithNewline(size_t CurBlock, size_t BlockSize,
                                                                    std::vector<char> &DelimitersLookupOneBlock) {
	size_t lastBlock = _storage->dimensions()[0] - 1;
	if (CurBlock == lastBlock) { DelimitersLookupOneBlock[BlockSize - 1] = '\n'; }
}

template<typename Type, size_t NumDim>
void TDataWriterBase<Type, NumDim>::_write(const std::vector<size_t> &, const std::vector<char> &,
                                           std::vector<char> &DelimitersLookupOneBlock,
                                           const std::vector<std::vector<size_t>> &CoordinatesOneBlock) {
	// write file
	size_t blockSize = DelimitersLookupOneBlock.size();
	bool newLine     = true;
	for (size_t block = 0; block < _storage->dimensions()[0]; block++) {
		// new block starts
		_ensureLastBlockEndsWithNewline(block, blockSize, DelimitersLookupOneBlock);

		for (size_t i = 0; i < blockSize; i++) {
			// get full vector of coordinates
			std::array<size_t, NumDim> coordArray;
			coordArray[0] = block;
			std::copy_n(CoordinatesOneBlock[i].begin(), NumDim - 1, coordArray.begin() + 1);

			if (newLine) {
				// write rownames
				_rowNames->write(_filePointer, coordArray);
				newLine = false;
			}

			// get value of storage and delimiter
			auto value     = (*_storage)[coordArray];
			char delimiter = DelimitersLookupOneBlock[i];

			// write
			*_filePointer << value << delimiter;

			if (delimiter == '\n') {
				// next line will be a newline
				newLine = true;
			}
		}
	}
}

//--------------------
// TDataReaderBase
//--------------------

template<typename Type, size_t NumDim> TDataReaderBase<Type, NumDim>::TDataReaderBase() {
	_filePointer                   = nullptr;
	_isOpen                        = false;
	_delimiterComment              = "//";
	_throwIfTitleDoesntMatch       = false;
	_addedArtificialFirstDimension = false;
	_isZipped                      = false;
}

template<typename Type, size_t NumDim> TDataReaderBase<Type, NumDim>::~TDataReaderBase() { _closeFile(); }

template<typename Type, size_t NumDim> void TDataReaderBase<Type, NumDim>::_openFile() {
	// check if file is gzipped: ending must be .gz
	_filePointer = new std::ifstream(_filename.c_str());
	_isOpen = true;

	if (!(*_filePointer) || _filePointer->fail() || !_filePointer->good()) {
		throw TUserError("Failed to open file '", _filename, "' for reading! Does the file exists?");
	}
}

template<typename Type, size_t NumDim> void TDataReaderBase<Type, NumDim>::_restartAtBeginning() {
	// will set the filePointer to the very beginning of the file
	if (_isZipped) {
		// seekg is not supported in by gzstream -> close and re-open file
		_closeFile();
		_openFile();
	} else {
		_filePointer->clear();
		_filePointer->seekg(0);
	}
}

template<typename Type, size_t NumDim> void TDataReaderBase<Type, NumDim>::_closeFile() {
	if (_isOpen) {
		delete _filePointer;
		_isOpen = false;
	}
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_initialize(std::string_view Filename,
                                                TMultiDimensionalStorage<Type, NumDim> *Storage,
                                                const std::vector<std::shared_ptr<TNamesEmpty>> &Names,
                                                const std::vector<char> &Delimiters,
                                                const std::vector<bool> &NameIsWritten, std::vector<size_t> &,
                                                std::vector<bool> &, size_t, const colNameTypes &ColNameType) {

	// set member variables
	_filename   = Filename;
	_storage    = Storage;
	_delimiters = Delimiters;
	_dimensionsVec.resize(NumDim);
	for (size_t i = 0; i < NumDim; i++) { _dimensionsVec[i] = Storage->dimensions()[i]; }

	// initialize row- and colNames
	_rowNames = std::make_unique<TRowNames>(NumDim, NameIsWritten, Names, _delimiters, _filename);
	if (ColNameType == colNames_multiline) {
		_colNames = std::make_unique<TColNameMultiLine>(NumDim, NameIsWritten, Names, _delimiters, _filename);
	} else if (ColNameType == colNames_concatenated) {
		_colNames = std::make_unique<TColNameConcatenated>(NumDim, NameIsWritten, Names, _delimiters, _filename);
	} else {
		_colNames = std::make_unique<TColNameFull>(NumDim, NameIsWritten, Names, _delimiters, _filename);
	}
	_colNames->initialize(_delimiters);
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::read(std::string_view Filename, TMultiDimensionalStorage<Type, NumDim> *Storage,
                                         std::vector<std::shared_ptr<TNamesEmpty>> Names, std::vector<char> Delimiters,
                                         std::vector<bool> NameIsWritten, std::vector<size_t> Dimensions,
                                         std::vector<bool> DimensionsFilled, size_t GuessLengthUnknownDimension,
                                         const colNameTypes &ColNameType) {

	if (NumDim == 1 && Delimiters[0] == '\t') {
		// for 1D layout: if all entries are written on one row, we have several problems (e.g. blockSize = 1, but then
		// we cannot skip & reshuffle etc) -> throw. Developer should create storage that has 2 dimensions
		// and set the length of the first dimension to one. This will simplify everything greatly - otherwise, I would
		// have needed to write a big hack.
		throw TDevError("Class can not read 1D-storage written in one row. Please define storage as having 2 dimensions, and "
		         "set the length of the first dimension to 1.");
	}

	// set all member variables
	_initialize(Filename, Storage, Names, Delimiters, NameIsWritten, Dimensions, DimensionsFilled,
	            GuessLengthUnknownDimension, ColNameType);

	// check on valid input
	_checkInput(Dimensions, DimensionsFilled);

	// open file
	_openFile();

	// infer all dimensions needed to parse file
	_inferDimensions(Dimensions, DimensionsFilled, GuessLengthUnknownDimension);

	// set file pointer to begin of file
	_restartAtBeginning();

	// read again, now with known dimensions -> read and store data
	_read(Dimensions, GuessLengthUnknownDimension);
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::throwIfTitleDoesntMatch(bool ThrowIfTitleDoesntMatch) {
	_throwIfTitleDoesntMatch = ThrowIfTitleDoesntMatch;
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_checkInput(std::vector<size_t> &Dimensions, std::vector<bool> &DimensionsFilled) {
	this->_checkDelimiters();
	_checkDimensionsFromDeveloper(Dimensions, DimensionsFilled);
	_checkIfBlockSizeCanBeInferred(DimensionsFilled);
	for (size_t dim = 0; dim < NumDim; dim++) {
		if (_rowNames->nameIsRowName(dim)) {
			_rowNames->checkForValidPrefilledNames(dim);
		} else {
			_colNames->checkForValidPrefilledNames(dim);
		}
	}
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_checkDimensionsFromDeveloper(std::vector<size_t> &Dimensions,
                                                                  std::vector<bool> &DimensionsFilled) {
	if (Dimensions.size() != NumDim) {
		throw TDevError("Vector DimensionsFromDeveloper of size ", Dimensions.size(),
		         " differs from expected size based on number of dimensions (", NumDim, ").");
	}
	if (DimensionsFilled.size() != NumDim) {
		throw TDevError("Vector DimensionsFilled of size ", DimensionsFilled.size(),
		         " differs from expected size based on number of dimensions (", NumDim, ").");
	}
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_checkIfBlockSizeCanBeInferred(const std::vector<bool> &DimensionsFilled) {
	// for all dimension names with the same delimiter:
	// max. 1 can have unknown dimensions, all others must be known, otherwise we can't infer dimensions
	// dimensions are known if a) set by developer or b) written in header

	// initialize map with all delimiters that are being used
	std::map<char, size_t> numUnknownDimensions_PerDelim;
	for (size_t dim = 0; dim < NumDim; dim++) { numUnknownDimensions_PerDelim[_delimiters[dim]] = 0; }

	// now count: per delimiter, how many dimensions are unknown?
	for (size_t dim = 0; dim < NumDim; dim++) {
		// name is known if:
		// 1) name is written as rowname or as colname
		// 2) or dimension length is set
		if (DimensionsFilled[dim] || _rowNames->nameIsWritten(dim) || _colNames->nameIsWritten(dim)) {
			continue; // ok, we know enough
		} else {
			numUnknownDimensions_PerDelim[_delimiters[dim]]++;
		}
	}

	// finally evaluate: is there any delimiter which has >1 unknown dimensions?
	for (auto &it : numUnknownDimensions_PerDelim) {
		if (it.second > 1) {
			throw TDevError("Can not infer dimensions of file ", _filename, ". For delimiter ", it.first, ", ", it.second,
			         " dimensions are unknown (while at max 1 can be unknown in order to infer dimensions.");
		}
	}
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_inferDimensions(std::vector<size_t> &Dimensions,
                                                     std::vector<bool> &DimensionsFilled,
                                                     size_t &GuessLengthFirstDimension) {
	// infer other dimensions
	_readHeader(Dimensions, DimensionsFilled);
	_parseFirstLine(Dimensions, DimensionsFilled);

	// check if all but first dimension are known by now
	_finalCheck_InferDimensions(Dimensions, DimensionsFilled);

	// if first dimension is known: adjust GuessLengthFirstDimension
	if (DimensionsFilled[0]) { GuessLengthFirstDimension = Dimensions[0]; }
}

template<typename Type, size_t NumDim> std::vector<std::string> TDataReaderBase<Type, NumDim>::_readHeaderIntoVector() {
	size_t numLines = _colNames->numLinesHeader();

	// read line by line and add to vector
	std::vector<std::string> fullHeader(numLines);
	for (size_t row = 0; row < numLines; row++) {
		std::string line;
		if (!impl::readUntilDelimiter(_filePointer, line, '\n', _delimiterComment)) {
			throw TDevError("Reached end of file ", _filename, " while reading header. Expected ", numLines,
			       " header lines, but reached end of file after ", row + 1, " lines!");
		}
		fullHeader[row] = line;
	}
	return fullHeader;
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_readHeader(std::vector<size_t> &Dimensions, std::vector<bool> &DimensionsFilled) {
	std::vector<std::string> fullHeader = _readHeaderIntoVector();
	_colNames->parseHeader(fullHeader, Dimensions, DimensionsFilled, _delimiters, _rowNames, _throwIfTitleDoesntMatch);
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_parseFirstLine(std::vector<size_t> &Dimensions,
                                                    std::vector<bool> &DimensionsFilled) {
	// prepare storage for inferring dimensionality of column dimensions
	std::map<char, size_t> delimCounter;

	std::string line;
	bool firstLine = true;
	while (impl::readUntilDelimiter(_filePointer, line, '\n', _delimiterComment)) {
		// first parse rowNames
		_rowNames->parseRowNames_InferDimensions(line, Dimensions, DimensionsFilled, firstLine);

		// now parse data: count delimiters
		_countNumberDelims_oneLine(line, delimCounter, firstLine);

		// decide if we have all information we need
		if (_rowNames->allRowNameDimensionsAreKnown(DimensionsFilled)) { break; }

		firstLine = false;
	}

	// infer dimension of columns
	_inferDimensions_oneLine(delimCounter, Dimensions, DimensionsFilled);
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_countNumberDelims_oneLine(std::string_view Line,
                                                               std::map<char, size_t> &DelimCounter, bool FirstLine) {
	// count the total number of occurrences per delimiter
	std::set<char> uniqueDelims(_delimiters.begin(), _delimiters.end());
	for (auto &delim : uniqueDelims) {
		size_t count = std::count(Line.begin(), Line.end(), delim);

		if (FirstLine) {
			// first line: store
			DelimCounter[delim] = count;
		} else {
			// all other lines: check if the same as first line
			if (DelimCounter[delim] != count) {
				std::string delimString = {delim};
				throw TDevError("While reading file ", _filename, ": Count for delimiter ", delimString,
				       " has already been set to ", DelimCounter[delim], ", which differs from the length ", count,
				       " that was inferred based on a subsequent line.");
			}
		}
	}
}

template<typename Type, size_t NumDim>
size_t TDataReaderBase<Type, NumDim>::_getProductOverAllDim_WithSameDelim(size_t Dim,
                                                                          const std::vector<size_t> &Dimensions,
                                                                          const std::vector<bool> &DimensionsFilled) {
	size_t productAllOtherDimWithSameDelim = 1;
	for (size_t otherDim = 0; otherDim < NumDim; otherDim++) {
		if (otherDim != Dim && _delimiters[otherDim] == _delimiters[Dim]) {
			// other dimension with same delimiter
			if (!DimensionsFilled[otherDim]) {
				throw TDevError("Should not get here! Dimension ", otherDim, " must be known in order to infer dimension ",
				         Dim, "!");
			}
			productAllOtherDimWithSameDelim *= Dimensions[otherDim];
		}
	}

	return productAllOtherDimWithSameDelim;
}

template<typename Type, size_t NumDim>
size_t
TDataReaderBase<Type, NumDim>::_removeAllUpperColumns_FromDelimCounter(size_t TotalCountsDelim, size_t Dim,
                                                                       const std::vector<size_t> &Dimensions,
                                                                       const std::vector<bool> &DimensionsFilled) {
	// remove upper dimensions
	size_t countsWithoutRepetition = TotalCountsDelim;
	for (size_t previousDim = 0; previousDim < Dim; previousDim++) {
		if (_colNames->nameIsColName(previousDim) && _delimiters[previousDim] != _delimiters[Dim]) {
			if (!DimensionsFilled[previousDim]) {
				throw TDevError("Should not get here! Dimension ", previousDim, " must be known in order to infer dimension ",
				         Dim, "!");
			}
			countsWithoutRepetition /= Dimensions[previousDim];
		}
	}

	return countsWithoutRepetition;
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_inferDimensions_oneLine(std::map<char, size_t> &DelimCounter,
                                                             std::vector<size_t> &Dimensions,
                                                             std::vector<bool> &DimensionsFilled) {
	// infer dimensions of columns based on delimiters
	for (size_t dim = 0; dim < NumDim; dim++) {
		if (_colNames->nameIsColName(dim) && !DimensionsFilled[dim]) {
			size_t productAllOtherDimWithSameDelim =
			    _getProductOverAllDim_WithSameDelim(dim, Dimensions, DimensionsFilled);

			size_t totalCounterDelim = DelimCounter[_delimiters[dim]];
			size_t counterDelimWithoutRepetition =
			    _removeAllUpperColumns_FromDelimCounter(totalCounterDelim, dim, Dimensions, DimensionsFilled);
			size_t counterDim = counterDelimWithoutRepetition + 1; // we always have one more element than delimiters

			size_t length = counterDim / productAllOtherDimWithSameDelim;

			_colNames->setLengthUnnamedDimension(length, dim, Dimensions, DimensionsFilled);
		}
	}
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_finalCheck_InferDimensions(const std::vector<size_t> &Dimensions,
                                                                const std::vector<bool> &DimensionsFilled) {
	for (size_t dim = 1; dim < NumDim; dim++) {
		if (!DimensionsFilled[dim]) {
			throw TDevError("Should not get here! Dimension ", dim,
			         " is still unknown at the end of parsing header and first lines!");
		}
		if (Dimensions[dim] == 0) {
			throw TDevError("Should not get here! Dimension of ", dim,
			         " is still zero at the end of parsing header and first lines!");
		}
	}
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_read(std::vector<size_t> &Dimensions, size_t GuessLengthUnknownDimension) {
	// parse header again and check if colnames make sense with the dimensions that we've inferred
	std::vector<std::string> fullHeader = _readHeaderIntoVector();
	_colNames->checkIfHeaderMatchesInferredDimensions(fullHeader, Dimensions, _delimiters, _rowNames);

	// construct delimiters and coordinates for one block (= all except most outer dimension)
	std::vector<size_t> dimensionsOneBlock = this->_getAllExceptFirst(Dimensions);
	std::vector<char> delimitersOneBlock   = this->_getAllExceptFirst(_delimiters);

	// create lookup tables for delimiters and coordinates
	std::vector<char> delimitersLookupOneBlock =
	    TDataBlock::getDelimiterLookupOneBlock(delimitersOneBlock, dimensionsOneBlock, _delimiters[0]);
	std::vector<std::vector<size_t>> coordinatesOneBlock = TDataBlock::getCoordinatesOneBlock(dimensionsOneBlock);

	// translate these block coordinates to indices where to store each cell inside storage
	_translateBlockCoordinates_toIndexInStorage(dimensionsOneBlock, coordinatesOneBlock);

	// finally read!
	_read(dimensionsOneBlock, delimitersOneBlock, coordinatesOneBlock, delimitersLookupOneBlock,
	      GuessLengthUnknownDimension);
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_translateBlockCoordinates_toIndexInStorage(
    const std::vector<size_t> &BlockDimensions, const std::vector<std::vector<size_t>> &CoordinatesOneBlock) {
	// prepare storage:
	// per element in a cell: do we want to store it, and if yes, at what index inside one block?
	size_t numDim_Block = BlockDimensions.size();
	size_t blockSize    = TDataBlock::getBlockSize(BlockDimensions);
	// what are the dimensions of the storage (not necessarily the same as dimensions in file, if elements are skipped)
	_dimensionsOfStorage.resize(NumDim);

	std::vector<std::vector<size_t>> coordinateToStore(blockSize, std::vector<size_t>(numDim_Block, 0));
	std::vector<std::vector<bool>> shouldBeStored(blockSize, std::vector<bool>(numDim_Block, false));
	// go over all cells of one block
	for (size_t i = 0; i < blockSize; i++) {
		// go over all dimensions of one block
		for (size_t blockDim = 0; blockDim < numDim_Block; blockDim++) {
			size_t actualDim  = blockDim + 1; // actual dimension is +1, because block leaves out first dimension
			// get index inside current dimension
			size_t indexInDim = CoordinatesOneBlock[i][blockDim];
			if (_rowNames->nameIsRowName(actualDim)) {
				// ask rowName
				if (_rowNames->elementIsStored(actualDim, indexInDim)) {
					shouldBeStored[i][blockDim]    = true;
					coordinateToStore[i][blockDim] = _rowNames->indexToStoreElement(actualDim, indexInDim);
				}
			} else {
				// ask colName
				if (_colNames->elementIsStored(actualDim, indexInDim)) {
					shouldBeStored[i][blockDim]    = true;
					coordinateToStore[i][blockDim] = _colNames->indexToStoreElement(actualDim, indexInDim);
				}
			}
			// infer dimensions of storage
			_dimensionsOfStorage[actualDim] =
			    std::max(_dimensionsOfStorage[actualDim],
			             coordinateToStore[i][blockDim] + 1); // size of dimension is max index + 1
		}
	}

	// check if a dimension is fully omitted -> currently now allowed
	for (size_t dim = 1; dim < NumDim; dim++) {
		if (_dimensionsOfStorage[dim] == 0) {
			throw TDevError("Dimension ", dim, " is fully omitted in file ", _filename,
			         ". This feature is currently not implemented.");
		}
	}

	// now translate coordinates to a linear index
	_translateCoordinates_toIndexInStorage(blockSize, numDim_Block, shouldBeStored, coordinateToStore);
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_translateCoordinates_toIndexInStorage(
    size_t BlockSize, size_t, const std::vector<std::vector<bool>> &ShouldBeStored,
    const std::vector<std::vector<size_t>> &CoordinatesToStore) {
	std::vector<size_t> dimensionsOneBlockStorage = this->_getAllExceptFirst(_dimensionsOfStorage);

	// go over all cells of one block
	_indexCell_inOneBlockStorage.resize(BlockSize, 0);
	_storeCell.resize(BlockSize, false);
	for (size_t i = 0; i < BlockSize; i++) {
		// finally translate coordinates into linear indices
		if (std::all_of(ShouldBeStored[i].begin(), ShouldBeStored[i].end(), [](bool v) { return v; })) { // all are true
			_storeCell[i] = true;
			if (dimensionsOneBlockStorage.empty()) {
				_indexCell_inOneBlockStorage[i] = 0;
			} else {
				_indexCell_inOneBlockStorage[i] = getLinearIndex(CoordinatesToStore[i], dimensionsOneBlockStorage);
			}
		}
	}
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_prepareStorage(size_t GuessLengthUnknownDimension) {
	std::array<size_t, NumDim - 1> allKnownDimensions;
	for (size_t d = 0; d < NumDim - 1; d++) { allKnownDimensions[d] = _dimensionsOfStorage[d + 1]; }
	_storage->prepareFillData(GuessLengthUnknownDimension, allKnownDimensions);
}

template<typename Type, size_t NumDim> void TDataReaderBase<Type, NumDim>::_finalizeFillingStorage() {
	_storage->finalizeFillData();

	for (size_t dim = 0; dim < NumDim; dim++) {
		if (_rowNames->nameIsRowName(dim)) {
			_rowNames->finalizeFillNames(dim, _storage->dimensions()[dim]);
			auto name = _rowNames->getDimensionName(dim);
			_storage->setDimensionName(name, dim);
		} else {
			_colNames->finalizeFillNames(dim, _storage->dimensions()[dim]);
			auto name = _colNames->getDimensionName(dim);
			_storage->setDimensionName(name, dim);
		}
	}
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_storeDataInTempVec(std::string_view DataPoint,
                                                        std::vector<Type> &OneBlock_forStorage, bool KeepLine,
                                                        size_t ElementInBlock) {
	// store in temporary vector: only if 1) most outer rowname should be kept (keepLine) and 2) element should be kept
	if (KeepLine && _storeCell[ElementInBlock]) {
		size_t index = _indexCell_inOneBlockStorage[ElementInBlock];
		if (index >= OneBlock_forStorage.size()) {
			throw TDevError("Index ", index, " is larger or equal than size of block for storage (",
			         OneBlock_forStorage.size(), ")!");
		}
		OneBlock_forStorage[index] = str::fromString<Type, true>(DataPoint);
	}
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_storeInStorage(const std::vector<Type> &OneBlock_forStorage, bool KeepLine) {
	// reached end of block
	if (KeepLine) {
		// store in storage
		for (auto &data : OneBlock_forStorage) { _storage->emplace_back(data); }
	}
}

template<typename Type, size_t NumDim>
void TDataReaderBase<Type, NumDim>::_read(const std::vector<size_t> &, const std::vector<char> &,
                                          const std::vector<std::vector<size_t>> &CoordinatesOneBlock,
                                          const std::vector<char> &DelimitersLookupOneBlock,
                                          size_t GuessLengthUnknownDimension) {
	size_t blockSize_File    = DelimitersLookupOneBlock.size();
	size_t blockSize_storage = TDataBlock::getBlockSize(this->_getAllExceptFirst(_dimensionsOfStorage));
	std::vector<Type> oneBlock_forStorage(blockSize_storage, 0.);
	_prepareStorage(GuessLengthUnknownDimension);

	// parse file
	size_t elementInBlock = 0;
	std::string dataPoint;
	bool newLine  = true;
	bool keepLine = true;
	bool newBlock = true;
	for (;;) {
		if (newLine) {
			// read rownames
			if (!_rowNames->extractRowNames(_filePointer, _delimiterComment, CoordinatesOneBlock[elementInBlock],
			                                keepLine, newBlock)) {
				break; // end of file
			}
			newLine  = false;
			newBlock = false;
		}

		// get relevant delim
		char delim = DelimitersLookupOneBlock[elementInBlock];
		if (delim == '\n') {
			// next line will be a newline
			newLine = true;
		}

		// read cell
		if (!impl::readUntilDelimiter(_filePointer, dataPoint, delim, _delimiterComment)) { break; }

		// store in temporary vector, in right order
		_storeDataInTempVec(dataPoint, oneBlock_forStorage, keepLine, elementInBlock);

		if (elementInBlock == blockSize_File - 1) {
			// store in storage
			_storeInStorage(oneBlock_forStorage, keepLine);
			elementInBlock = 0;
			newBlock       = true;
		} else {
			elementInBlock++;
		}
	}
	_finalizeFillingStorage();
}

}; // end namespace coretools
