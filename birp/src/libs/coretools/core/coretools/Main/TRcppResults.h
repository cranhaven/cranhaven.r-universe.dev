//
// Created by madleina on 08.05.24.
//

#ifndef TRCPPRESULTS_H
#define TRCPPRESULTS_H

#ifdef USE_RCPP
#include "coretools/Main/TError.h"
#include "coretools/arma_include.h" // makes sure order of includes is valid for Rcpp
#include <string>
#include <string_view>
#include <vector>
#include "coretools/Main/TLog.h"

namespace coretools {

class TRcppResults {
private:
	std::vector<std::string> _outputNames;
	Rcpp::List _output;

public:
	TRcppResults()  = default;
	~TRcppResults() = default;

	void add(std::string_view Filename, const Rcpp::DataFrame &Df) {
		auto it = std::find(_outputNames.begin(), _outputNames.end(), (std::string)Filename);
		if (it != _outputNames.end()) { throw TDevError("Object with name ", Filename, " already exists!"); }

		// now add
		_outputNames.push_back((std::string)Filename);
		_output[(std::string)Filename] = Df;
	}

	void clear(){
		_outputNames.clear();
		_output = Rcpp::List::create();
	}

	const Rcpp::List &getList(int Code) {
		_output["returnCode"] = Code;
		return _output;
	}
};

namespace instances {
inline TRcppResults &rcppResults() {
	static TRcppResults results;
	return results;
}

} // namespace instances
} // namespace coretools

#endif

#endif // TRCPPRESULTS_H
