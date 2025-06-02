/*
 * TAtlasTesting.cpp
 *
 *  Created on: Dec 11, 2017
 *      Author: phaentu
 */

#include "coretools/IntegrationTests/TTesting.h"

#include <exception> // for exception
#include <ostream>   // for operator<<, ofstream, basic_...
#include <stddef.h>  // for size_t
#include <stdexcept> // for runtime_error

#include "coretools/IntegrationTests/TTest.h"     // for TTest
#include "coretools/IntegrationTests/TTestList.h" // for TTestList
#include "coretools/Main/TLog.h"
#include "coretools/Main/TParameters.h"

namespace coretools {
class TTaskList;

TTesting::TTesting(TTestList *TestList) {
	testList = TestList;

	// get name of report file
	outputName = instances::parameters().get<std::string>("out", "Testing_report.txt");
	instances::logfile().list("Writing testing report to '", outputName, "'.");

	// add tests
	testList->parseSuites();
	testList->parseTests();
	printTests();
}

TTesting::~TTesting() = default;

void TTesting::printTests() {
	if (testList->size() > 1)
		instances::logfile().startIndent("Will run the following ", testList->size(), " tests:");
	else if (testList->size() == 1)
		instances::logfile().startIndent("Will run the following test:");
	else
		UERROR("No tests requested!");

	testList->printTestToLogfile();
	instances::logfile().endIndent();
}

void TTesting::runTests(TTaskList *taskList) {
	using str::toString;
	// open report file
	std::ofstream out(outputName.c_str());
	if (!out) UERROR("Failed to open file '", outputName, "' for writing!");

	// prepare test runs
	if (testList->size() < 1) UERROR("No tests requested!");

	// now run all tests
	if (testList->size() > 1)
		instances::logfile().startNumbering("Running ", testList->size(), " tests:");
	else
		instances::logfile().startNumbering("Running 1 test:");

	for (size_t testNum = 0; testNum < testList->size(); ++testNum) {
		// report test number and name
		instances::logfile().numberWithIndent("Running test '", (*testList)[testNum]->name(), "' (test ", testNum + 1,
											  " of ", testList->size(), "):");
		out << testNum + 1 << '\t' << (*testList)[testNum]->name() << '\t';

		// run test
		const auto success = (*testList)[testNum]->run(taskList);

		// report
		instances::logfile().removeIndent();
		if (success) {
			instances::logfile().conclude("Test '", (*testList)[testNum]->name(), "' passed!");
			out << "passed\n";
		} else {
			instances::logfile().conclude("Test '", (*testList)[testNum]->name(), "' failed!");
			out << "failed\n";
		}
	}
	instances::logfile().endNumbering();

	// close report file
	out.close();
}

}; // namespace coretools
