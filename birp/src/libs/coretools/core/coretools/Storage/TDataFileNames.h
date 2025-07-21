//
// Created by madleina on 29.04.21.
//

#ifndef TDATAFILENAMES_H
#define TDATAFILENAMES_H

#include <cstdint>
#include <memory>
#include <set>
#include <string>
#include <vector>


namespace coretools {
class TNamesEmpty;

enum colNameTypes : uint8_t { colNames_multiline, colNames_concatenated, colNames_full };

//--------------------
// TDataBlock
//--------------------

class TDataBlock {
protected:
public:
	// functions to deal with coordinates/block etc.
	static size_t getBlockSize(const std::vector<size_t> &DimensionsOneBlock);
	static std::vector<char> getDelimiterLookupOneBlock(const std::vector<char> &DelimitersOneBlock,
	                                                    const std::vector<size_t> &DimensionsOneBlock,
	                                                    char LastDelimiter);
	static std::vector<std::vector<size_t>> getCoordinatesOneBlock(const std::vector<size_t> &Dimensions);
};

//--------------------
// TFileNames
//--------------------
class TFileNames {
	// base class for row- and column names
protected:
	// number of dimension
	size_t _numDim;

	// information on file (only used for throwing errors!)
	std::string _filename;

	// information about names
	std::vector<std::shared_ptr<TNamesEmpty>> _names;
	std::vector<bool> _nameIsRowName;
	std::vector<bool> _nameIsWritten;
	std::vector<bool> _nameClassIsPrefilled;

	// information about order of names (relevant for reading)
	std::vector<std::vector<bool>> _storeName;
	std::vector<std::vector<size_t>> _indexToStoreName;

	void _fillNameIsRowName(const std::vector<char> &Delimiters);
	void _fillNameClassIsPrefilled();
	void _checkInput(const std::vector<char> &Delimiters);
	void _setLengthNamedDimension(size_t Length, size_t Dim, std::vector<size_t> &Dimensions,
	                              std::vector<bool> &DimensionsFilled);
	template<typename T> static void _removeDuplicatesFromVec(std::vector<T> &Vec) {
		std::set<std::string> unique;
		auto it = Vec.begin();
		while (it != Vec.end()) {
			// already in set? -> remove, duplicate!
			if (unique.find(*it) != unique.end()) {
				Vec.erase(it);
			} else {
				// not yet in set -> keep
				unique.insert(*it);
				it++;
			}
		}
	}
	void _checkForUnusedNames(size_t NumStoredNames, size_t Dim);

	// read
	void _assignIndices_NamedDim(const std::vector<std::string> &UniqueNames, size_t Dim);
	void _assignIndices_Unsorted(size_t Length, size_t Dim);
	void _assignIndices_Sorted(const std::vector<std::string> &UniqueNames, size_t Dim);

public:
	TFileNames(size_t NumDim, const std::vector<bool> &NameIsWritten,
	           const std::vector<std::shared_ptr<TNamesEmpty>> &Names, const std::vector<char> &Delimiters,
	           std::string_view FileName);
	virtual ~TFileNames() = default;

	// checks
	void checkIfDimensionMatchSizeName(std::string_view FileName, const std::vector<size_t> &Dimensions);
	void checkForValidPrefilledNames(size_t Dim);

	// setters
	void setLengthUnnamedDimension(size_t Length, size_t Dim, std::vector<size_t> &Dimensions,
	                               std::vector<bool> &DimensionsFilled);
	void finalizeFillNames(size_t Dim, size_t LengthFromObservation);

	// getters
	bool nameIsWritten(size_t Dim);
	bool nameIsRowName(size_t Dim);
	bool nameIsColName(size_t Dim);
	size_t getSizeNames(size_t Dim);
	bool elementIsStored(size_t Dim, size_t IndexInDim);
	size_t indexToStoreElement(size_t Dim, size_t IndexInDim);
	const std::shared_ptr<TNamesEmpty> &getDimensionName(size_t Dim);
};

//--------------------
// TRowNames
//--------------------

class TRowNames : public TFileNames {
protected:
	// delimiter used to separate rownames
	char _delimiterRowNames;

	// temporary storage for inferring dimensions
	std::vector<std::vector<std::string>> _parsedRowNames_PerDim;
	std::vector<std::string> _previousRowName_PerDim;
	std::vector<size_t> _numSubsequentEqual_PerDim;
	std::vector<std::string> _previousMostOuterRowName;

	// functions for reading
	std::string _extractRowName(size_t Dim, std::string &Line);
	bool _extractMostOuterRowName(std::istream *FilePointer, std::string_view DelimiterComments, bool &Keep,
	                              bool NewBlock);
	bool _extractBlockRowNames(std::istream *FilePointer, std::string_view DelimiterComments,
	                           const std::vector<size_t> &BlockCoordinates);
	void _checkIfBlockRowNamesMatchFirstBlock(const std::vector<std::string> &Name, size_t Dim,
	                                          const std::vector<size_t> &BlockCoordinates);
	bool _rowNameHasChanged(std::string_view Rowname, std::string &PreviousRowName);
	bool _rowNameJustRestarted(std::string_view Rowname, size_t Dim);
	void _addNewRowName(std::string_view Rowname, size_t Dim);
	void _setLengthSubsequentDimensions_RowName(size_t Dim, size_t ProductOverAllNext, std::vector<size_t> &Dimensions,
	                                            std::vector<bool> &DimensionsFilled);
	void _correctWrittenNamesBasedOnFormat();
	void _setRowNames(const std::vector<std::string> &UniqueNames, size_t Dim);

public:
	TRowNames(size_t NumDim, const std::vector<bool> &NameIsWritten,
	          const std::vector<std::shared_ptr<TNamesEmpty>> &Names, const std::vector<char> &Delimiters,
	          std::string_view FileName);
	~TRowNames() override = default;

	void setDelimiterRowNames(char DelimiterRowNames);

	// write
	template<class Container> void write(std::ostream *FilePointer, const Container &FullCoordinates) {
		for (size_t dim = 0; dim < _numDim; dim++) {
			if (_nameIsWritten[dim]) { *FilePointer << (*_names[dim])[FullCoordinates[dim]] << _delimiterRowNames; }
		}
	}
	void writeTitle(std::ostream *FilePointer);

	// read
	void extractTitleRowNames(std::string &Line, bool ThrowIfTitleDoesntMatch);
	void parseRowNames_InferDimensions(std::string &FullLine, std::vector<size_t> &Dimensions,
	                                   std::vector<bool> &DimensionsFilled, bool FirstLine);
	bool allRowNameDimensionsAreKnown(const std::vector<bool> &DimensionsFilled);
	bool extractRowNames(std::istream *FilePointer, std::string_view DelimiterComments,
	                     const std::vector<size_t> &BlockCoordinates, bool &Keep, bool NewBlock);
};

//--------------------
// TColNameBase
//--------------------

class TColNameBase : public TFileNames {
protected:
	// common information about header
	bool _hasHeader();
	template<class T> std::vector<T> _getVecRelevantForHeader(const std::vector<T> &Vec);
	char _getDelimMostOuterColumn(const std::vector<char> &Delimiters);
	void _correctWrittenNamesBasedOnFormat(const std::vector<char> &Delimiters);
	virtual bool _formatWritesColName(size_t Dim, const std::vector<char> &Delimiters) = 0;
	void _checkDelimConcatenation(char DelimiterMostOuterColName, char DelimConcatenation);
	void _checkDelimNames(const std::vector<char> &Delimiters);

	// write
	virtual void _writeHeader(std::ostream *FilePointer, const std::vector<size_t> &HeaderDimensions,
	                          const std::vector<char> &HeaderDelimiters,
	                          const std::vector<std::vector<size_t>> &CoordinatesOneBlock,
	                          const std::vector<char> &DelimitersOneBlock,
	                          const std::unique_ptr<TRowNames> &RowNames) = 0;

	// read
	virtual void _parseHeader(std::vector<std::string> &Header, std::vector<size_t> &Dimensions,
	                          std::vector<bool> &DimensionsFilled, const std::vector<char> &Delimiters,
	                          const std::unique_ptr<TRowNames> &RowNames, bool ThrowIfTitleDoesntMatch) = 0;
	void _setColNames(const std::vector<std::string> &UniqueNames, size_t Dim);
	virtual void _checkIfHeaderMatchesInferredDimensions(std::vector<std::string> &Header,
	                                                     const std::vector<size_t> &HeaderDimensions,
	                                                     const std::vector<char> &HeaderDelimiters,
	                                                     const std::vector<std::vector<size_t>> &CoordinatesOneBlock,
	                                                     const std::vector<char> &DelimitersOneBlock,
	                                                     const std::unique_ptr<TRowNames> &RowNames) = 0;

public:
	TColNameBase(size_t NumDim, const std::vector<bool> &NameIsWritten,
	             const std::vector<std::shared_ptr<TNamesEmpty>> &Names, const std::vector<char> &Delimiters,
	             std::string_view FileName);
	~TColNameBase() override = default;
	void initialize(const std::vector<char> &Delimiters);

	virtual void setDelimiterConcatenation(char DelimConcatenation);

	// write
	void writeHeader(std::ostream *FilePointer, const std::vector<size_t> &Dimensions,
	                 const std::vector<char> &Delimiters, const std::unique_ptr<TRowNames> &RowNames);

	// read
	virtual size_t numLinesHeader();
	void parseHeader(std::vector<std::string> &Header, std::vector<size_t> &Dimensions,
	                 std::vector<bool> &DimensionsFilled, const std::vector<char> &Delimiters,
	                 const std::unique_ptr<TRowNames> &RowNames, bool ThrowIfTitleDoesntMatch);
	void checkIfHeaderMatchesInferredDimensions(std::vector<std::string> &Header, const std::vector<size_t> &Dimensions,
	                                            const std::vector<char> &Delimiters,
	                                            const std::unique_ptr<TRowNames> &RowNames);
};

//--------------------
// TColNameMultiLine
//--------------------

class TColNameMultiLine : public virtual TColNameBase {
protected:
	bool _formatWritesColName(size_t Dim, const std::vector<char> &Delimiters) override;

	// functions for writing
	size_t _getJumpSizeHeader(size_t CurDim, const std::vector<size_t> &Dimensions,
	                          const std::vector<char> &Delimiters);
	void _writeHeader(std::ostream *FilePointer, const std::vector<size_t> &HeaderDimensions,
	                  const std::vector<char> &HeaderDelimiters,
	                  const std::vector<std::vector<size_t>> &CoordinatesOneBlock,
	                  const std::vector<char> &DelimitersOneBlock, const std::unique_ptr<TRowNames> &RowNames) override;

	// functions for reading
	void _parseHeader(std::vector<std::string> &Header, std::vector<size_t> &Dimensions,
	                  std::vector<bool> &DimensionsFilled, const std::vector<char> &Delimiters,
	                  const std::unique_ptr<TRowNames> &RowNames, bool ThrowIfTitleDoesntMatch) override;
	std::vector<std::string> _getUniqueColNames_ForDim(size_t Dim, std::string_view Colnames_ThisDim,
	                                                   const std::vector<char> &Delimiters);
	void _checkIfHeaderMatchesInferredDimensions(std::vector<std::string> &Header,
	                                             const std::vector<size_t> &HeaderDimensions,
	                                             const std::vector<char> &HeaderDelimiters,
	                                             const std::vector<std::vector<size_t>> &CoordinatesOneBlock,
	                                             const std::vector<char> &DelimitersOneBlock,
	                                             const std::unique_ptr<TRowNames> &RowNames) override;

public:
	TColNameMultiLine(size_t NumDim, const std::vector<bool> &NameIsWritten,
	                  const std::vector<std::shared_ptr<TNamesEmpty>> &Names, const std::vector<char> &Delimiters,
	                  std::string_view FileName);
	~TColNameMultiLine() override = default;

	// read
	size_t numLinesHeader() override;
};

//--------------------
// TColNameConcatenated
//--------------------

class TColNameConcatenated : public virtual TColNameBase {
protected:
	char _delimConcatenation;
	bool _formatWritesColName(size_t Dim, const std::vector<char> &Delimiters) override;

	// functions for writing
	size_t _getJumpSizeHeader(const std::vector<size_t> &Dimensions, const std::vector<char> &Delimiters);
	void _writeHeader(std::ostream *FilePointer, const std::vector<size_t> &HeaderDimensions,
	                  const std::vector<char> &HeaderDelimiters,
	                  const std::vector<std::vector<size_t>> &CoordinatesOneBlock,
	                  const std::vector<char> &DelimitersOneBlock, const std::unique_ptr<TRowNames> &RowNames) override;

	// functions for reading
	void _parseHeader(std::vector<std::string> &Header, std::vector<size_t> &Dimensions,
	                  std::vector<bool> &DimensionsFilled, const std::vector<char> &Delimiters,
	                  const std::unique_ptr<TRowNames> &RowNames, bool ThrowIfTitleDoesntMatch) override;
	std::vector<std::vector<std::string>> _getUniqueColNames(std::string_view Header,
	                                                         const std::vector<char> &Delimiters);
	void _checkIfHeaderMatchesInferredDimensions(std::vector<std::string> &Header,
	                                             const std::vector<size_t> &HeaderDimensions,
	                                             const std::vector<char> &HeaderDelimiters,
	                                             const std::vector<std::vector<size_t>> &CoordinatesOneBlock,
	                                             const std::vector<char> &DelimitersOneBlock,
	                                             const std::unique_ptr<TRowNames> &RowNames) override;

public:
	TColNameConcatenated(size_t NumDim, const std::vector<bool> &NameIsWritten,
	                     const std::vector<std::shared_ptr<TNamesEmpty>> &Names, const std::vector<char> &Delimiters,
	                     std::string_view FileName);
	~TColNameConcatenated() override = default;

	void setDelimiterConcatenation(char DelimConcatenation) override;
};

//--------------------
// TColNameFull
//--------------------

class TColNameFull : public virtual TColNameBase {
protected:
	char _delimConcatenation;
	bool _formatWritesColName(size_t Dim, const std::vector<char> &Delimiters) override;

	// functions for writing
	void _writeHeader(std::ostream *FilePointer, const std::vector<size_t> &HeaderDimensions,
	                  const std::vector<char> &HeaderDelimiters,
	                  const std::vector<std::vector<size_t>> &CoordinatesOneBlock,
	                  const std::vector<char> &DelimitersOneBlock, const std::unique_ptr<TRowNames> &RowNames) override;

	// functions for reading
	void _parseHeader(std::vector<std::string> &Header, std::vector<size_t> &Dimensions,
	                  std::vector<bool> &DimensionsFilled, const std::vector<char> &Delimiters,
	                  const std::unique_ptr<TRowNames> &RowNames, bool ThrowIfTitleDoesntMatch) override;
	std::vector<std::vector<std::string>> _getUniqueColNames(std::string &Header, const std::vector<char> &Delimiters);
	std::string _getAllDelims_AsString(const std::vector<char> &Delimiters);
	size_t _getLastDimension_ThatIsWritten();
	void _checkIfHeaderMatchesInferredDimensions(std::vector<std::string> &Header,
	                                             const std::vector<size_t> &HeaderDimensions,
	                                             const std::vector<char> &HeaderDelimiters,
	                                             const std::vector<std::vector<size_t>> &CoordinatesOneBlock,
	                                             const std::vector<char> &DelimitersOneBlock,
	                                             const std::unique_ptr<TRowNames> &RowNames) override;

public:
	TColNameFull(size_t NumDim, const std::vector<bool> &NameIsWritten,
	             const std::vector<std::shared_ptr<TNamesEmpty>> &Names, const std::vector<char> &Delimiters,
	             std::string_view FileName);
	~TColNameFull() override = default;

	void setDelimiterConcatenation(char DelimConcatenation) override;
};

}; // end namespace coretools

#endif // TDATAFILENAMES_H
