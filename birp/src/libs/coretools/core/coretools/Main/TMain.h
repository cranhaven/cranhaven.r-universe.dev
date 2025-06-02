/*
 * TMain.h
 *
 *  Created on: Apr 1, 2019
 *      Author: phaentu
 */

#ifndef TMAIN_H_
#define TMAIN_H_

#include "coretools/Files/TOutputFile.h"
#include "coretools/IntegrationTests/TTest.h"
#include "coretools/IntegrationTests/TTesting.h"
#include "coretools/Main/TError.h"
#include "coretools/Main/TLog.h"
#include "coretools/Main/TParameters.h"
#include "coretools/Main/TTask.h"
#include "coretools/Main/TTaskList.h"
#include "coretools/Main/globalConstants.h"
#include "coretools/Strings/concatenateString.h"

#ifdef USE_RCPP
#include "coretools/Main/TRcppData.h"
#include "coretools/Main/TRcppResults.h"
#include "coretools/arma_include.h" // makes sure order of includes is valid for Rcpp
#endif

namespace coretools {

//---------------------------------------------------------------------------
// TMain
//---------------------------------------------------------------------------
class TMain {
private:
	TTaskList _taskList;
	TTestList _testList;
	std::string _applicationName;
	std::string _version;
	std::string _organization;
	std::string _web;
	std::string _title;
	std::string _email;
	TTimer _timer;
	bool _hadErrors;

	constexpr static const char *_getGitVersion() {
#ifndef GITVERSION
#define GITVERSION "N/A"
#endif
		return GITVERSION;
	}

	void _fillTitleString() {
		_title.clear();
		std::vector<std::string> tmp;

		// application name and version (plus underline and extra space)
		tmp.push_back(_applicationName + " " + _version);
		tmp.emplace_back(tmp[0].length() + 2, '-');
		tmp.emplace_back("");

		// add lab
		if (!_organization.empty()) tmp.emplace_back(_organization);

		// add website
		if (!_web.empty()) tmp.push_back(_web);

		// commit (first extra space)
		tmp.emplace_back("");
		tmp.push_back(std::string("Commit ") + _getGitVersion());

		// get max length
		size_t len = tmp[0].length();
		for (size_t i = 1; i < tmp.size(); ++i) {
			if (len < tmp[i].length()) len = tmp[i].length();
		}
		len += 2; // add one space on either side

		// now compile title. Add spaces in front to align center
		for (auto &i : tmp) {
			if (i.empty()) {
				_title += '\n';
			} else {
				// add extra spaces
				std::string add((len - i.length()) / 2, ' ');
				_title += add + i + '\n';
			}
		}
	}

	void _initialize() {
		_hadErrors = false;

		// compile title
		_fillTitleString();

		// set global variables
		__GLOBAL_APPLICATION_NAME__    = _applicationName;
		__GLOBAL_APPLICATION_VERSION__ = _version;
		__GLOBAL_APPLICATION_COMMIT__  = _getGitVersion();
	}

	std::string _constructDeveloperErrorFile(std::string_view ErrorWhat, std::string_view UsedFileNames,
	                                         std::string_view ErrorParamsFile) {
		using instances::parameters;
		// open file
		const auto filename = _applicationName + "_errorInfo.txt";
		TOutputFile file(filename, " ");

		// write general info (name, commit etc.)
		file.writeln(_applicationName, _version);
		file.writeln("Commit:", _getGitVersion());
		file.writeln("Email developer:", _email).endln(); // empty line

		file.writeln(parameters().getNameExecutable(), parameters().usedParametersAndVals());
		file.writeln("ERROR:", ErrorWhat).endln(); // empty line

		file.writeln("Files attached to email:", ErrorParamsFile, ',', filename, ',', UsedFileNames);

		return filename;
	}

	std::string _constructUsedParamsFile() {
		// open file
		const auto filename = _applicationName + "_usedParams.txt";
		TOutputFile file(filename, 2);
		// write used parameters in a format that can be re-used by program
		instances::parameters().writeUsedParametersAndValsToFile(file);
		return filename;
	}

	void _handleDeveloperError(std::string_view ErrorWhat) {
		const auto usedFileNames   = instances::parameters().usedFilenames();
		const auto errorParamsFile = _constructUsedParamsFile();
		const auto errorInfoFile =
		    _constructDeveloperErrorFile(ErrorWhat, str::concatenateString(usedFileNames, ", "), errorParamsFile);

		// error message
		instances::logfile().setVerboseLevel(VerboseLevel::verbose);
		instances::logfile().newLine();
		instances::logfile().clearIndent();
		instances::logfile().write("Yep, this error was caused by a bug. We apologize.");
		instances::logfile().write("You may help to get this bug fixed by writing an email to ", _email,
		                           ". Please attach the following ", usedFileNames.size() + 2, " files:");
		instances::logfile().addNumberingLevel();
		instances::logfile().number(errorInfoFile);
		instances::logfile().number(errorParamsFile);
		for (auto &it : usedFileNames) instances::logfile().number(it);
		instances::logfile().removeNumberingLevel();
		instances::logfile().newLine();
	}

	template<typename T, typename... Cs> static auto _createTask(std::string_view Explanation, Cs... Citations) {
		struct Task : coretools::TTask {
			Task(std::string_view Explanation, Cs... Citations) : coretools::TTask(Explanation, Citations...) {}

			void run() override {
				T t;
				t.run();
			}
		};
		return new Task(Explanation, Citations...);
	}

	void _init_logfile() {
		// set verbosity and print header
		using namespace coretools::instances;

		const std::string arg_ver = "verbose";
#ifdef USE_RCPP
		// Rcpp only knows verbose (level 2) vs non-verbose (level 0)
		parameters().exists(arg_ver) ? logfile().setVerboseLevel(2) : logfile().setVerboseLevel(0);
#else
		// command-line knows 3 levels
		logfile().setVerboseLevel(parameters().get<size_t>(arg_ver, 2));
#endif

		logfile().printHeader(_title, parameters().getNameExecutable(), parameters().getLogStrTask(), arg_ver);
	}

	void _run() {
		using namespace coretools::instances;

		// set verbosity and print header
		_init_logfile();

		// warnings?
		if (parameters().exists("suppressWarnings")) {
			logfile().list("Suppressing Warnings.");
			logfile().suppressWarings();
		}

		// open log file, if requested
		const auto logFilename = parameters().get("logFile", std::string{});
		if (!logFilename.empty()) {
			logfile().list("Will write log to file '", logFilename, "'.");
			logfile().openFile(logFilename);
			logfile().writeFileOnly(_title);
		}

		// is a task provided?
		if (!parameters().exists("task")) {
			_taskList.printAvailableTasks();
			logfile().list("Usage: ", parameters().getNameExecutable(), " taskName [options]");
		} else {
			// run requested task
			const auto task = parameters().get("task");
			if (task == "test") {
				// run testing utilities
				logfile().startIndent("Testing of ", _applicationName, " functionalities:");
				TTesting test(&_testList);
				test.runTests(&_taskList);
			} else {
				// run requested task
				_taskList.run(task);
			}
			logfile().clearIndent();

			// report not used arguments
			parameters().reportUnusedParameters();
		}
	}

public:
	TMain(std::string_view ApplicationName, std::string_view Version, std::string_view Organization,
	      std::string_view Web, std::string_view Email) {
		// store name
		_applicationName = ApplicationName;
		_version         = Version;
		_organization    = Organization;
		_web             = Web;
		_email           = Email;

		// open logfile
		_initialize();
	}

	TMain(std::string_view ApplicationName, std::string_view Version, std::string_view Organization,
	      std::string_view Email) {
		// store name
		_applicationName = ApplicationName;
		_version         = Version;
		_organization    = Organization;
		_email           = Email;

		// open logfile
		_initialize();
	}

	TMain(std::string_view ApplicationName, std::string_view Version, std::string_view Email) {
		// store name
		_applicationName = ApplicationName;
		_version         = Version;
		_email           = Email;

		// open logfile
		_initialize();
	}

	void addGeneralCitations(std::string_view Citation) { _taskList.addGeneralCitation(Citation); }

	// Regular task
	template<typename T, typename... Cs>
	void createGroupedTask(std::string_view Group, std::string_view Name, std::string_view Explanation,
	                       Cs... Citations) {
		addGroupedTask(Group, Name, _createTask<T>(Explanation, Citations...));
	}

	// Regular task
	template<typename T, typename... Cs>
	void createRegularTask(std::string_view Name, std::string_view Explanation, Cs... Citations) {
		addRegularTask(Name, _createTask<T>(Explanation, Citations...));
	}

	template<typename T> void addRegularTask(std::string_view Name) { addRegularTask(Name, new T()); }

	void addRegularTask(std::string_view name, TTask *Task) { _taskList.addRegularTask(name, Task); }
	void addGroupedTask(std::string_view Group, std::string_view name, TTask *task) {
		_taskList.addGroupedTask(Group, name, task);
	}

	// Debug task
	template<typename T, typename... Cs>
	void createDebugTask(std::string_view Name, std::string_view Explanation, Cs... Citations) {
		addDebugTask(Name, _createTask<T>(Explanation, Citations...));
	}

	template<typename T> void addDebugTask(std::string_view Name) { addDebugTask(Name, new T()); }

	void addDebugTask(std::string_view Name, TTask *Task) { _taskList.addDebugTask(Name, Task); }

	void addTest(std::string_view Name, TTest *Test) { _testList.addTest(Name, Test); }

	void addTestSuite(std::string_view Name, const std::vector<std::string> &TestSuiteNames) {
		_testList.addTestSuite(Name, TestSuiteNames);
	}

#ifdef USE_RCPP
	Rcpp::List run(Rcpp::List Parameters, Rcpp::List Data) {
		// Rcpp run() function
		using namespace instances;
		try {
			// clear data and results from previous runs (are singletons)
			rcppData().clear();
			rcppResults().clear();

			// store data
			rcppData().init(Data);

			// fill parameters
			parameters().init("Birp Rcpp");
			Rcpp::CharacterVector keys = Parameters.names();
			for (int i = 0; i < Parameters.size(); ++i) {
				std::string key(keys[i]);
				std::string value(Parameters[i]);
				parameters().add(key, value);
			}
			_run();
		} catch (err::TUserError &error) { // user error
			logfile().error(error.what());
			_hadErrors = true;
		} catch (std::string &error) { // user error -> deprecated, use macro UERROR instead!
			logfile().error(error);
			_hadErrors = true;
		} catch (const char *error) { // user error -> deprecated, use macro UERROR instead!
			logfile().error(error);
			_hadErrors = true;
		} catch (std::exception &error) { // developer error, also catches TDevError
			logfile().error(error.what());
			_handleDeveloperError(error.what());
			_hadErrors = true;
		} catch (...) {
			logfile().error("unhandled error!");
			_hadErrors = true;
		}
		logfile().clearIndent();
		parameters().clear(); // Note: needed for Rcpp -> may run 2x but with other parameters -> need to clear

		// report end of program and return exit status
		if (_hadErrors) {
			logfile().list(_applicationName, " terminated with errors in ", _timer.formattedTime(), "!");
			logfile().close();
			return rcppResults().getList(1);
		} else {
			logfile().list(_applicationName, " terminated successfully in ", _timer.formattedTime(), "!");
			logfile().close();
			return rcppResults().getList(0);
		}
	}
#else
	int run(int argc, char *argv[]) {
		// Command-line run() function
		using namespace instances;
		try {
			// read parameters from the command line
			parameters().init(argc, argv);
			_run();
		} catch (err::TUserError &error) { // user error
			logfile().error(error.what());
			_hadErrors = true;
		} catch (std::string &error) { // user error -> deprecated, use macro UERROR instead!
			logfile().error(error);
			_hadErrors = true;
		} catch (const char *error) { // user error -> deprecated, use macro UERROR instead!
			logfile().error(error);
			_hadErrors = true;
		} catch (std::exception &error) { // developer error, also catches TDevError
			logfile().error(error.what());
			_handleDeveloperError(error.what());
			_hadErrors = true;
		} catch (...) {
			logfile().error("unhandled error!");
			_hadErrors = true;
		}
		logfile().clearIndent();

		// report end of program and return exit status
		if (_hadErrors) {
			logfile().list(_applicationName, " terminated with errors in ", _timer.formattedTime(), "!");
			logfile().close();
			return 1;
		} else {
			logfile().list(_applicationName, " terminated successfully in ", _timer.formattedTime(), "!");
			logfile().close();
			return 0;
		}
	}
#endif
};

} // namespace coretools

#endif /* TMAIN_H_ */
