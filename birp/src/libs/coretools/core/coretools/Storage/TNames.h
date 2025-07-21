//
// Created by madleina on 12.04.21.
//

#ifndef TNAMES_H
#define TNAMES_H


#include <cstddef>
#include <string>
#include <vector>

#include "coretools/Distances/TDistances.h"

namespace coretools {

//--------------------------
// TNamesEmpty
//--------------------------

class TNamesEmpty {
	// base class that returns empty strings
	// provides interface for all deriving name classes
protected:
	size_t _size;
	size_t _complexity; // how many rows/columns must be parsed to fill one name?
	std::vector<std::string>
	    _title; // if name class is used as rowNames: what kind of colname must be written? Same size as complexity.
	char _delimNames;     // what kind of delimiter must be used to connect a name with complexity > 1?
	size_t _indexVisited; // which index did we visit last?
	bool _storesNonDefaultNames;

	std::string _extractFromStringAndReturnString(std::string &String, char DelimiterLast, bool ThrowIfEmpty) const;
	std::vector<std::string> _extractFromStringAndReturnVec(std::string &String, char DelimiterLast,
	                                                        bool ThrowIfEmpty) const;
	bool _extractFromStreamAndFillVec(std::vector<std::string> &Vec, std::istream *FilePointer, char DelimiterLast,
	                                  std::string_view DelimiterComment, bool ThrowIfEmpty) const;

public:
	TNamesEmpty();
	TNamesEmpty(size_t Size);
	virtual ~TNamesEmpty() = default;

	// resizing
	virtual void resize(size_t Size);

	// add names
	virtual void addName(const std::vector<std::string> &Name);
	virtual void addName(const std::vector<std::string> &Name, size_t Index);
	void addNameAndSplit(std::string_view Name);
	void addNameAndSplit(std::string_view Name, size_t Index);
	bool checkIfNameShouldBeKept(const std::vector<std::string> &Name, std::string_view FileName);
	std::string extractNameFromStringAndReturn(std::string &String, char DelimiterLast);
	void extractNameFromStringAndStore(std::string &String, char DelimiterLast);
	bool extractNameFromStreamAndFillIntoVec(std::istream *FilePointer, char DelimiterLast,
	                                         std::string_view DelimiterComment, std::vector<std::string> &Vec);

	// set title
	void setTitle(const std::vector<std::string> &Names);
	void setDelimName(char Delim);
	std::vector<std::string> extractTitleFromString(std::string &String, char DelimiterLast);

	// get names
	virtual std::string operator[](size_t Index) const;
	virtual std::vector<std::string> getName(size_t Index) const;

	// filled?
	bool storesNonDefaultNames() const;
	virtual bool exists(std::string_view Name);
	virtual bool exists(const std::vector<std::string> &Name);
	virtual bool isFilled() const;
	virtual size_t getIndex(std::string_view Name);
	virtual void finalizeFilling();
	virtual bool operator==(const TNamesEmpty &Other) const;
	bool operator!=(const TNamesEmpty &Other) const;

	// get title
	const std::vector<std::string> &getTitleVec() const;
	std::string getTitle() const;
	char getDelimNames() const;

	// get size/complexity
	virtual size_t size() const;
	size_t complexity() const;
	virtual size_t offset() const;
};

//--------------------------
// TNamesStrings
//--------------------------

class TNamesStrings : public TNamesEmpty {
	// name class that stores a vector of strings
	// and provides functions to fill and get those
protected:
	std::vector<std::string> _names;

public:
	TNamesStrings();
	TNamesStrings(size_t Size);
	TNamesStrings(const std::vector<std::string> &Names);

	// resizing
	void resize(size_t Size) override;

	// add names
	void addName(const std::vector<std::string> &Name) override;
	void addName(const std::vector<std::string> &Name, size_t Index) override;

	// getters
	std::string operator[](size_t Index) const override;
	bool exists(std::string_view Name) override;
	bool exists(const std::vector<std::string> &Name) override;
	bool hasDuplicateNames() const;
	bool operator==(const TNamesEmpty &Other) const override;
	size_t getIndex(std::string_view Name) override;
	size_t size() const override;
};

//--------------------------
// TNamesIndices
//--------------------------

class TNamesIndices : public TNamesEmpty {
	// name class that stores nothing except an offset
	// and simply returns the offset + index as a string
protected:
	size_t _offset;

public:
	TNamesIndices();

	TNamesIndices(size_t Size);

	void setOffset(size_t Offset);

	// getters
	std::string operator[](size_t Index) const override;
	bool exists(std::string_view Name) override;
	bool exists(const std::vector<std::string> &Name) override;
	size_t getIndex(std::string_view Name) override;
	bool operator==(const TNamesEmpty &Other) const override;
	size_t offset() const override;
};

//-------------------------------
// TNamesIndicesAlphabetUpperCase
//-------------------------------

class TNamesIndicesAlphabetUpperCase : public TNamesEmpty {
	// name class that returns the index as a upper case alphabet index (Excel column naming style)
protected:
public:
	TNamesIndicesAlphabetUpperCase();
	TNamesIndicesAlphabetUpperCase(size_t Size);

	// getters
	std::string operator[](size_t Index) const override;
	bool exists(std::string_view Name) override;
	bool exists(const std::vector<std::string> &Name) override;
	size_t getIndex(std::string_view Name) override;
};

//-------------------------------
// TNamesIndicesAlphabetLowerCase
//-------------------------------

class TNamesIndicesAlphabetLowerCase : public TNamesEmpty {
	// name class that returns the index as a lower case alphabet index (Excel column naming style)
protected:
public:
	TNamesIndicesAlphabetLowerCase();
	TNamesIndicesAlphabetLowerCase(size_t Size);

	// getters
	std::string operator[](size_t Index) const override;
	bool exists(std::string_view Name) override;
	bool exists(const std::vector<std::string> &Name) override;
	size_t getIndex(std::string_view Name) override;
};

//-------------------------------
// TNamesPositions
//-------------------------------

class TNamesPositions : public TNamesEmpty {
	// name class that stores a pointer to positions
	// and provides functions to add and get those
protected:
	TPositionsRaw *_positions; // non-owning ptr
	bool _orderIsChunkPos;

	void _splitName(std::string Name, std::string &Chunk, size_t &Pos);

public:
	TNamesPositions(TPositionsRaw *Positions);

	void setOrderChrPos(bool OrderIsChrPos);

	void addPositions(TPositionsRaw *Positions);
	void addName(const std::vector<std::string> &Name) override;
	void addName(const std::vector<std::string> &Name, size_t) override;

	void finalizeFilling() override;

	std::string operator[](size_t Index) const override;
	std::vector<std::string> getName(size_t Index) const override;
	bool exists(std::string_view Name) override;
	bool exists(const std::vector<std::string> &Name) override;
	bool operator==(const TNamesEmpty &Other) const override;
	size_t getIndex(std::string_view Name) override;

	size_t size() const override;
};


}; // end namespace coretools

#endif // TNAMES_H
