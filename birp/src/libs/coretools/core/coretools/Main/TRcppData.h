//
// Created by madleina on 06.05.24.
//

#ifndef TRCPPDATA_H
#define TRCPPDATA_H

#ifdef USE_RCPP
#include "coretools/arma_include.h" // makes sure order of includes is valid for Rcpp
#include <string>
#include <string_view>
#include <vector>

namespace coretools {

class TRcppData {
private:
	std::vector<std::string> _inputNames;
	std::vector<Rcpp::DataFrame> _input;

public:
	TRcppData()  = default;
	~TRcppData() = default;
	TRcppData(Rcpp::List Data) { init(Data); }

	void init(Rcpp::List Data) {
		Rcpp::CharacterVector keys = Data.names();
		for (int i = 0; i < Data.size(); ++i) {
			_input.emplace_back((Rcpp::DataFrame)Data[i]);
			// also store names
			std::string key(keys[i]);
			_inputNames.emplace_back(key);
		}
	}

	// access input
	const Rcpp::DataFrame &operator[](size_t i) const { return _input[i]; }
	Rcpp::DataFrame &operator[](size_t i) { return _input[i]; }
	const std::string &name(size_t i) const { return _inputNames[i]; }

	size_t index(std::string_view Name) {
		auto it = std::find(_inputNames.begin(), _inputNames.end(), Name);
		if (it == _inputNames.end()) { throw TDevError("Failed to find data frame with name ", Name, "!"); }
		return std::distance(_inputNames.begin(), it);
	}

	void clear(){
		_inputNames.clear();
		_input.clear();
	}
};

namespace instances {
inline TRcppData &rcppData() {
	static TRcppData data;
	return data;
}

} // namespace instances
} // namespace coretools

#endif

#endif // TRCPPDATA_H
