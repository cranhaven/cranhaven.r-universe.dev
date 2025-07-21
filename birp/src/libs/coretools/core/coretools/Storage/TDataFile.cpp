//
// Created by madleina on 11.05.21.
//

#include "coretools/Storage/TDataFile.h"

#include <set>

namespace coretools {

//--------------------
// TDataFileBase
//--------------------

TDataFileBase::TDataFileBase() = default;

void TDataFileBase::setDelimiterRowNames(char DelimiterRowNames) { _rowNames->setDelimiterRowNames(DelimiterRowNames); }

void TDataFileBase::setDelimiterConcatenationColNames(char DelimiterConcatenation) {
	_colNames->setDelimiterConcatenation(DelimiterConcatenation);
}

void TDataFileBase::_checkDelimiters() {
	// check input vector of delimiters:
	// 1) size of delimiters must match the number of dimensions
	if (_delimiters.size() != _dimensionsVec.size()) {
		throw TDevError("Error while writing file '", _filename, "'. Size of vector with delimiters (", _delimiters.size(),
		         ") does not match the number of dimension of storage (", _dimensionsVec.size(), ").");
	}

	// 2) all delimiters must either be the same as the delimiter of the previous dimension;
	//    or then be a character that has not occurred before
	std::set<char> usedDelims = {_delimiters[0]};
	for (size_t d = 1; d < _dimensionsVec.size(); d++) {
		if (_delimiters[d] != _delimiters[d - 1] && usedDelims.find(_delimiters[d]) != usedDelims.end()) {
			throw TDevError("Error while writing file '", _filename, "'. Delimiter of dimension ", d, "(", _delimiters[d],
			         ") is not the same as the delimiter of the previous dimension, but has been used for another "
			         "dimension! This is currently not allowed.");
		}
		usedDelims.insert(_delimiters[d]);
	}

	// 3) only first delimiter(s) are allowed to be \n. As soon a non-\n delimiter is used, we can't use \n anymore
	for (size_t d = 1; d < _dimensionsVec.size(); d++) {
		if (_delimiters[d - 1] != '\n' && _delimiters[d] == '\n') {
			throw TDevError("Error while writing file '", _filename, "'. Delimiter of dimension ", d, "(", _delimiters[d],
			         ") is not allowed to be a newline (\\n), if the delimiter of the previous dimension is not a "
			         "newline, too!");
		}
	}
}

}; // end namespace coretools
