//
// Created by madleina on 20.04.21.
//

#ifndef TDATAFILE_H
#define TDATAFILE_H

#include <iosfwd>

#include "coretools/Storage/TDataFileNames.h"
#include "coretools/Storage/TStorage.h"

namespace coretools {

class TNamesEmpty;

//--------------------
// TDataFileBase
//--------------------

class TDataFileBase {
protected:
	// filename
	std::string _filename;

	// information on dimensions
	std::vector<size_t> _dimensionsVec;

	// information on names
	std::unique_ptr<TRowNames> _rowNames;
	std::unique_ptr<TColNameBase> _colNames;

	std::vector<char> _delimiters;

	template<class T> std::vector<T> _getAllExceptFirst(const std::vector<T> &Vec);

	// check functions
	void _checkDelimiters();

public:
	TDataFileBase();
	virtual ~TDataFileBase() = default;

	void setDelimiterRowNames(char DelimiterRowNames);
	void setDelimiterConcatenationColNames(char DelimiterConcatenation);
};

//--------------------
// TDataWriterBase
//--------------------

template<typename Type, size_t NumDim> class TDataWriterBase : public TDataFileBase {
	// class for writing a data file
protected:
	using TDataFileBase::_colNames;
	using TDataFileBase::_delimiters;
	using TDataFileBase::_filename;
	using TDataFileBase::_rowNames;

	// output file
	std::ostream *_filePointer;
	bool _isOpen;

	// storage
	TMultiDimensionalStorage<Type, NumDim> *_storage;

	// open file
	void _openFile();
	void _closeFile();

	// extract names from storage
	std::vector<std::shared_ptr<TNamesEmpty>> _extractNamesFromStorage(TMultiDimensionalStorage<Type, NumDim> *Storage);
	void _initialize(const std::vector<std::shared_ptr<TNamesEmpty>> &Names, std::string_view Filename,
	                 TMultiDimensionalStorage<Type, NumDim> *Storage, const std::vector<char> &Delimiters,
	                 const std::vector<bool> &NameIsWritten, const colNameTypes &ColNameType);

	// write header
	void _writeHeader();

	// write
	void _write();
	void _write(const std::vector<size_t> &BlockDimensions, const std::vector<char> &BlockDelimiters,
	            std::vector<char> &DelimitersLookupOneBlock,
	            const std::vector<std::vector<size_t>> &CoordinatesOneBlock);
	void _ensureLastBlockEndsWithNewline(size_t CurBlock, size_t BlockSize,
	                                     std::vector<char> &DelimitersLookupOneBlock);

	// check functions
	void _checkInput();

public:
	TDataWriterBase();
	~TDataWriterBase() override;

	void write(std::string_view Filename, TMultiDimensionalStorage<Type, NumDim> *Storage,
	           const std::vector<char> &Delimiters, const std::vector<bool> &NameIsWritten,
	           const colNameTypes &ColNameType);
	void write(const std::vector<std::shared_ptr<TNamesEmpty>> &Names, std::string_view Filename,
	           TMultiDimensionalStorage<Type, NumDim> *Storage, const std::vector<char> &Delimiters,
	           const std::vector<bool> &NameIsWritten, const colNameTypes &ColNameType);
};

//--------------------
// TDataReader
//--------------------

template<typename Type, size_t NumDim> class TDataReaderBase : public TDataFileBase {
	// pure virtual base class for reading a data file
	// derived classes differ in the way they read the header (override pure virtual function _readHeader())

protected:
	using TDataFileBase::_colNames;
	using TDataFileBase::_delimiters;
	using TDataFileBase::_filename;
	using TDataFileBase::_rowNames;

	// storage
	TMultiDimensionalStorage<Type, NumDim> *_storage;

	// input file
	std::istream *_filePointer;
	bool _isOpen;
	bool _isZipped;
	std::string _delimiterComment; // which delimiter is used to comment lines

	// for each element in one block: store it, and if yes in which order?
	std::vector<bool> _storeCell;
	std::vector<size_t> _indexCell_inOneBlockStorage;
	std::vector<size_t> _dimensionsOfStorage;

	// settings
	bool _throwIfTitleDoesntMatch;

	// tweaks
	bool _addedArtificialFirstDimension; // for 1D-objects
	TMultiDimensionalStorage<Type, NumDim + 1> *_artificialStorage_Row1DLayout;

	// open file
	void _openFile();
	void _closeFile();
	void _restartAtBeginning();

	// infer dimensions
	void _inferDimensions(std::vector<size_t> &Dimensions, std::vector<bool> &DimensionsFilled,
	                      size_t &GuessLengthFirstDimension);
	void _parseFirstLine(std::vector<size_t> &Dimensions, std::vector<bool> &DimensionsFilled);
	void _finalCheck_InferDimensions(const std::vector<size_t> &Dimensions, const std::vector<bool> &DimensionsFilled);
	void _countNumberDelims_oneLine(std::string_view Line, std::map<char, size_t> &DelimCounter, bool FirstLine);
	void _inferDimensions_oneLine(std::map<char, size_t> &DelimCounter, std::vector<size_t> &Dimensions,
	                              std::vector<bool> &DimensionsFilled);
	size_t _getProductOverAllDim_WithSameDelim(size_t Dim, const std::vector<size_t> &Dimensions,
	                                           const std::vector<bool> &DimensionsFilled);
	size_t _removeAllUpperColumns_FromDelimCounter(size_t TotalCountsDelim, size_t Dim,
	                                               const std::vector<size_t> &Dimensions,
	                                               const std::vector<bool> &DimensionsFilled);
	void _translateCoordinates_toIndexInStorage(size_t BlockSize, size_t NumDimBlock,
	                                            const std::vector<std::vector<bool>> &ShouldBeStored,
	                                            const std::vector<std::vector<size_t>> &CoordinatesToStore);

	// read header
	void _readHeader(std::vector<size_t> &Dimensions, std::vector<bool> &DimensionsFilled);
	std::vector<std::string> _readHeaderIntoVector();

	// read rest
	void _read(std::vector<size_t> &Dimensions, size_t GuessLengthUnknownDimension);
	void _read(const std::vector<size_t> &BlockDimensions, const std::vector<char> &BlockDelimiters,
	           const std::vector<std::vector<size_t>> &CoordinatesOneBlock,
	           const std::vector<char> &DelimitersLookupOneBlock, size_t GuessLengthUnknownDimension);
	void _translateBlockCoordinates_toIndexInStorage(const std::vector<size_t> &BlockDimensions,
	                                                 const std::vector<std::vector<size_t>> &CoordinatesOneBlock);
	void _prepareStorage(size_t GuessLengthUnknownDimension);
	void _storeDataInTempVec(std::string_view DataPoint, std::vector<Type> &OneBlock_forStorage, bool KeepLine,
	                         size_t ElementInBlock);
	void _storeInStorage(const std::vector<Type> &OneBlock_forStorage, bool KeepLine);
	void _finalizeFillingStorage();

	// check functions
	void _checkInput(std::vector<size_t> &Dimensions, std::vector<bool> &DimensionsFilled);
	void _checkIfBlockSizeCanBeInferred(const std::vector<bool> &DimensionsFilled);
	void _checkDimensionsFromDeveloper(std::vector<size_t> &Dimensions, std::vector<bool> &DimensionsFilled);
	void _initialize(std::string_view Filename, TMultiDimensionalStorage<Type, NumDim> *Storage,
	                 const std::vector<std::shared_ptr<TNamesEmpty>> &Names, const std::vector<char> &Delimiters,
	                 const std::vector<bool> &NameIsWritten, std::vector<size_t> &Dimensions,
	                 std::vector<bool> &DimensionsFilled, size_t GuessLengthUnknownDimension,
	                 const colNameTypes &ColNameType);

public:
	TDataReaderBase();
	~TDataReaderBase() override;

	void read(std::string_view Filename, TMultiDimensionalStorage<Type, NumDim> *Storage,
	          std::vector<std::shared_ptr<TNamesEmpty>> Names, std::vector<char> Delimiters,
	          std::vector<bool> NameIsWritten, std::vector<size_t> Dimensions, std::vector<bool> DimensionsFilled,
	          size_t GuessLengthUnknownDimension, const colNameTypes &ColNameType);

	// settings
	void throwIfTitleDoesntMatch(bool ThrowIfTitleDoesntMatch);
};

}; // end namespace coretools

#include "TDataFile.tpp"

#endif // TDATAFILE_H
