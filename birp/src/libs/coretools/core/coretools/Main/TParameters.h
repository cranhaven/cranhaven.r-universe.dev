//---------------------------------------------------------------------------

#ifndef TParametersH
#define TParametersH

#include <map>
#include <string>
#include <vector>

#include "coretools/Strings/fromString.h"
#include "coretools/Strings/toString.h"
#include <filesystem>

namespace coretools {
class TOutputFile;

//---------------------------------------------------------------------------
// TParameters
// a set of TParameter and functions to interact with it.
//---------------------------------------------------------------------------
class TParameters {
private:
	struct TEntry {
		std::string value;
		mutable bool used;
	};
	using Map = std::map<std::string, TEntry, std::less<>>;

	Map _parameters;
	mutable std::vector<std::string> _requestedButInexistant;
	std::string _inputFileName;
	std::string _nameExecutable;
	std::string _logStrTask;
	bool _argsAreSpaced = false; // format with which parameters were specified, used for outputting command in matching
	                             // format if developer error occurs

	void _initialize(std::vector<std::string> &commandLineParams);

	void _parseArgsWithSpace(std::vector<std::string>::iterator it, std::vector<std::string> &commandLineParams);
	void _parseArgsWithEqualSign(std::vector<std::string>::iterator it, std::vector<std::string> &commandLineParams);

	template<typename T> static T _convertAndCheck(std::string_view Name, const std::string &Param) {
		try {
			return str::fromString<T, true>(Param);
		} catch (std::exception &error) {
			// will also catch TUserError and TDevError since they derive publicly from std::exception
			UERROR("Failed to parse parameter '", Name, "': ", error.what());
		} catch (...) { UERROR("Failed to parse parameter '", Name, "'!"); }
	}

	Map::const_iterator _find(std::string_view Name) const;

public:
	TParameters()  = default;
	TParameters(std::vector<std::string> &commandLineParams) { _initialize(commandLineParams); }
	TParameters(int &argc, char **argv) { init(argc, argv); }
	void init(int &argc, char **argv);
	void init(std::string_view ExecName);
	void clear();

	// read parameters
	void add(std::string_view Name, bool Used = false) { add(Name, "", Used); }

	template<typename T> void add(std::string_view Name, const T &Value, bool Used = false) {
		_parameters[std::string(Name)] = TEntry{str::toString(Value), Used};
	}

	void readFile(std::string_view fileName);
	void writeFile(std::string_view fileName);

	// parameter exists
	bool exists(std::string_view Name) const { return _find(Name) != _parameters.end(); }

	// fill parameter
	template<typename T> void fill(std::string_view Name, T &Dest) const { Dest = get<T>(Name); }

	template<typename T> void fill(std::string_view Name, T &Dest, const T &Default) const {
		Dest = get<T>(Name, Default);
	}

	const std::string &get(std::string_view Name) const;

	// get parameter
	template<typename T> T get(std::string_view Name) const { return _convertAndCheck<T>(Name, get(Name)); }

	std::string get(std::string_view Name, std::string_view Default) const {
		const auto it = _find(Name);
		if (it == _parameters.end()) return std::string(Default);
		return it->second.value;
	}

	template<typename T> T get(std::string_view Name, const T &Default) const {
		const auto it = _find(Name);
		if (it == _parameters.end()) return Default;
		return _convertAndCheck<T>(Name, it->second.value);
	}

	// access parameter lists
	std::string usedParametersAndVals() const;
	std::vector<std::string> usedFilenames() const;
	void writeUsedParametersAndValsToFile(TOutputFile &file) const;
	const std::string& getNameExecutable() const { return _nameExecutable; };
	const std::string& getLogStrTask() const { return _logStrTask; };

	std::string getListOfUnusedParameters() const;
	std::vector<std::string> unusedParameters() const;
	void reportUnusedParameters() const;

	// deprecated
	[[deprecated("use exists() instead!")]] bool parameterExists(std::string_view Name) const { return exists(Name); }

	template<typename T, typename... Ts>
	[[deprecated("use fill() instead!")]] void fillParameter(std::string_view Name, T &Dest, Ts...) {
		fill(Name, Dest);
	}

	template<typename T = std::string>
	[[deprecated("use get() instead!")]] T getParameter(std::string_view Name, const bool mandatory = true) {
		if (mandatory) return get<T>(Name);
		return get<T>(Name, T{});
	}

	template<typename T = std::string>
	[[deprecated("use get() instead!")]] T getParameter(std::string_view Name, std::string_view) {
		return get<T>(Name);
	}

	template<typename T = std::string>
	[[deprecated("use get() instead!")]] std::string getParameterWithDefault(std::string_view Name,
	                                                                         std::string_view Default) {
		return get(Name, Default);
	}

	template<typename T = std::string>
	[[deprecated("use get() instead!")]] T getParameterWithDefault(std::string_view Name, const T &Default) {
		return get<T>(Name, Default);
	}

	template<typename T = std::string>
	[[deprecated("use get() instead!")]] std::string getParameterFilename(std::string_view Name,
	                                                                      bool mandatory = true) {
		if (mandatory) return get<T>(Name);
		return get<T>(Name, T{});
	}

	[[deprecated("use get() instead!")]] std::string getParameterFilename(std::string_view Name, std::string_view) {
		return get(Name);
	}

	template<typename Iterable>
	[[deprecated("use fill(Name, Container) instead!")]] void
	fillParameterIntoContainer(std::string_view Name, Iterable &Container, char, bool mandatory = true) {
		if (mandatory) fill(Name, Container);
		fill(Name, Container, Iterable{});
	}

	template<typename Iterable>
	[[deprecated("use fill(Name, Container) instead!")]] void
	fillParameterIntoContainer(std::string_view Name, Iterable &Container, char, std::string_view) {
		fill(Name, Container);
	}

	template<typename Iterable>
	[[deprecated("use fill(Name, Container, Default) instead!")]] void
	fillParameterIntoContainerWithDefault(std::string_view Name, Iterable &Container, char, const Iterable &Default) {
		fill(Name, Container, Default);
	}

	template<typename Iterable>
	[[deprecated("use fill(Name, Container) instead!")]] void
	fillParameterIntoContainerRepeatIndexes(std::string_view Name, Iterable &Container, char, bool mandatory = true) {
		if (mandatory) fill(Name, Container);
		fill(Name, Container, Iterable{});
	}

	template<typename Iterable>
	[[deprecated("use fill(Name, Container, Default) instead!")]] void
	fillParameterIntoContainerRepeatIndexesWithDefault(std::string_view Name, Iterable &Container, char,
	                                                   const Iterable &Default) {
		fill(Name, Container, Default);
	}
};

size_t getNumThreads();

namespace instances {
inline TParameters &parameters() {
	static TParameters params;
	return params;
}

} // namespace instances

} // namespace coretools

#endif
