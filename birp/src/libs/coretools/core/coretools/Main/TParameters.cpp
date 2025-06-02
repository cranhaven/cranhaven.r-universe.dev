//---------------------------------------------------------------------------
#include "coretools/Main/TParameters.h"

#include "coretools/Files/TInputFile.h"
#include "coretools/Files/TOutputFile.h"
#include "coretools/Main/TLog.h"
#include "coretools/Strings/concatenateString.h"
#include "coretools/Strings/stringManipulations.h"
#include "coretools/Strings/stringProperties.h"
#ifdef _OPENMP
#include "omp.h"
#endif
//---------------------------------------------------------------------------

namespace coretools {

void TParameters::init(int &argc, char **argv) {
	_inputFileName = "";
	_argsAreSpaced = false;
	std::vector<std::string> commandLineParams;

	// first is name of executable
	_nameExecutable = argv[0];
	// others are parameters
	for (int i = 1; i < argc; ++i) commandLineParams.emplace_back(argv[i]);
	_initialize(commandLineParams);
}

void TParameters::init(std::string_view ExecName) {
	_inputFileName  = "";
	_argsAreSpaced  = false;
	_nameExecutable = ExecName;
}

void TParameters::clear() {
	_inputFileName = "";
	_parameters.clear();
}

void TParameters::_initialize(std::vector<std::string> &commandLineParams) {
	using str::stringContains;
	using str::stringStartsWith;
	// check if first is name of an input file which means no '=' or no '-'!
	if (commandLineParams.empty()) return;
	auto it = commandLineParams.begin();

	if (!stringContains(*it, '=') && !stringStartsWith(*it, '-')) {
		if (!std::filesystem::is_regular_file(*it)) {
			// first argument is not an input file -> interpret as name of task
			_logStrTask = "Interpreting '" + *it + "' as name of task.";
			add("task", *it);
		} else {
			_logStrTask = "Reading input file '" + *it + "'.";
			readFile(*it);
		}
		it++;
	}

	// check which mode is used: arg=val or --arg val
	if (it != commandLineParams.end()) {
		if (stringStartsWith(*it, "--")) // check if first argument starts with --
			_parseArgsWithSpace(it, commandLineParams);
		else
			_parseArgsWithEqualSign(it, commandLineParams);
	}
}

void TParameters::_parseArgsWithSpace(std::vector<std::string>::iterator it,
                                      std::vector<std::string> &commandLineParams) {
	using str::extractAfterLast;
	using str::stringContains;
	using str::stringStartsWith;
	// --arg val
	// example call: ./myprogram --vcf vcf.gz --gtl gtl.txt --storeProb --doThis --goAhead -3 --check --recal prob=7
	// only allow for "=" if 1) is a value (previous parameter had --, e.g. --param prob=7) or
	//                       2) if it starts with -- anyways, e.g. --param=7
	//                       Not ok: --param value param2=value2
	_argsAreSpaced = true;

	bool previousParamWasFlagged = false;
	std::string Name;

	for (; it != commandLineParams.end(); ++it) {
		if (stringContains(*it, '=')) { // string contains a = -> check if we should throw
			if (previousParamWasFlagged || stringStartsWith(*it, "--")) {
				// all ok
			} else
				UERROR("Specify all arguments either as '--arg val' OR as 'arg=val', but don't mix these two options! "
				       "(Argument '",
				       *it, "' contains a =).");
		}
		if (stringStartsWith(*it, "--")) { // flag
			// check if previous argument was a flag as well -> if yes, store previous as flag without value
			if (previousParamWasFlagged) add(Name);
			// don't store the current flag yet, as you don't know if there will be a value or not
			Name                    = extractAfterLast(*it, "-");
			previousParamWasFlagged = true;
			if (it == commandLineParams.end() - 1)
				// exception: if current is the last parameter -> store it
				add(Name);
		} else { // value
			if (!previousParamWasFlagged) UERROR("Parameter '", *it, "' requires flagging '--'!");
			add(Name, *it);
			previousParamWasFlagged = false;
		}
	}
}

void TParameters::_parseArgsWithEqualSign(std::vector<std::string>::iterator it,
                                          std::vector<std::string> &commandLineParams) {
	// arg=val
	// example call: ./myprogram task=simulate vcf=vcf.gz whatever
	_argsAreSpaced = false;

	for (; it != commandLineParams.end(); ++it) {
		if (str::stringStartsWith(*it, "--")) {
			// check if correct format is used
			UERROR("Specify all arguments either as '--arg val' OR as 'arg=val', but don't mix these two options! "
			       "(Argument '",
			       *it, "' starts with --).");
		}
		std::string Name = str::extractBefore(*it, '=');
		if (str::stringContains(*it, '=')) {
			add(Name, str::extractAfter(*it, '='));
		} else {
			add(Name);
		}
	}
}

//---------------------------------------------------------------------------
void TParameters::readFile(std::string_view fileName) {
	size_t lineNumber = 1;
	for (TInputFile iFile(fileName, FileType::NoHeader); !iFile.empty(); iFile.popFront()) {
		if (iFile.numCols() == 1) {
			add(iFile.get(0));
		} else if (iFile.numCols() == 2) {
			add(iFile.get(0), iFile.get(1));
		} else {
			instances::logfile().list(_logStrTask); // print this first such that user knows we're parsing the file
			UERROR("Line ", lineNumber, " in File ", fileName, " has ", iFile.numCols(), " columns, but only 1 or 2 are allowed!");
		}
		++lineNumber;
	}
}
void TParameters::writeFile(std::string_view fileName) {
	instances::logfile().list("Writing output file '", fileName, "'.");
	TOutputFile oFile(fileName, 2);

	for (auto &p : _parameters) {
		if (p.second.used) oFile.writeln(p.first, p.second.value);
	}
}
//---------------------------------------------------------------------------
const std::string &TParameters::get(std::string_view Name) const {
	const auto it = _find(Name);
	if (it == _parameters.end()) {
		if (!_inputFileName.empty())
			UERROR("The parameter '", Name, "' is not defined on the command line nor in the input file '",
			       _inputFileName, "'! ");
		else
			UERROR("The parameter '", Name, "' is not defined! ");
	}
	return it->second.value;
}

TParameters::Map::const_iterator TParameters::_find(std::string_view Name) const {
	const auto it = _parameters.find(Name);
	if (it == _parameters.end()) {
		_requestedButInexistant.emplace_back(Name);
	} else {
		it->second.used = true;
	}
	return it;
}

//---------------------------------------------------------------------------
std::string TParameters::usedParametersAndVals() const {
	std::string command;
	for (auto &p : _parameters) {
		if (!p.second.used) continue;

		// fill in correct format
		if (_argsAreSpaced) { // --arg val or --arg
			command += "--" + p.first + " ";
			if (!p.second.value.empty()) command += p.second.value + " ";
		} else { // arg=val or arg
			command += p.first;
			if (!p.second.value.empty()) command += "=" + p.second.value;
			command += " ";
		}
	}
	return command;
}

std::vector<std::string> TParameters::usedFilenames() const {
	std::vector<std::string> ret;
	for (auto &p : _parameters) {
		if (p.second.used && std::filesystem::exists(p.second.value)) ret.push_back(p.second.value);
	}
	return ret;
}

void TParameters::writeUsedParametersAndValsToFile(TOutputFile &file) const {
	for (auto &p : _parameters) {
		if (p.second.used) file.write(p.first, p.second.value).endln();
	}
}

std::vector<std::string> TParameters::unusedParameters() const {
	std::vector<std::string> vec;
	for (auto &p : _parameters) {
		if (!p.second.used) vec.push_back(p.first);
	}
	return vec;
}

std::string TParameters::getListOfUnusedParameters() const {
	std::vector<std::string> vec = unusedParameters();
	return coretools::str::concatenateString(vec, ", ");
}

void TParameters::reportUnusedParameters() const {
	using coretools::instances::logfile;
	auto unusedParams = unusedParameters();
	if (!unusedParams.empty()) {
		logfile().setVerboseLevel(VerboseLevel::verbose);
		logfile().newLine();
		logfile().warning("The following arguments were not used:");
		logfile().addIndent();

		for (auto &p : unusedParams) {
			// check if a similar parameter was requested
			auto match = coretools::str::findClosestMatchLevenshtein<false>(p, _requestedButInexistant, 1);

			// report best unless distance is too high
			if (match.second < p.length() - 1) {
				logfile().warning(p, " (did you mean '", match.first, "'?)");
			} else {
				logfile().warning(p);
			}
		}
		logfile().endIndent();
	}
}

size_t getNumThreads() {
#ifdef _OPENMP
	int avail = omp_get_max_threads();
	int numThreads;
	std::string s = coretools::instances::parameters().get("numThreads", coretools::str::toString(avail));
	if (s == "all") {
		numThreads = avail;
	} else if (s == "allButOne") {
		numThreads = avail - 1;
	} else {
		numThreads = (int)coretools::str::fromStringCheck<size_t>(s);
	}
	return std::max(numThreads, 1); // make sure we never use 0 threads
#else
	return 1;
#endif
}

} // namespace coretools
