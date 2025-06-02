//
// Created by caduffm on 3/3/20.
//

#include "coretools/IntegrationTests/TTest.h"

#include <exception> // for exception

#include "coretools/Main/TLog.h"
#include "coretools/Main/TParameters.h"
#include "coretools/Main/TTaskList.h" // for TTaskList

namespace coretools {

TTest::TTest() {
	_name    = "empty";
	taskList = nullptr;
};

bool TTest::runMain(std::string_view task) {
	instances::logfile().startIndent("Running task '", task, "':");
	instances::parameters().add("task", task);
	instances::parameters().add("verbose", "");

	if (!taskList) UERROR("taskList was not initialized!");

	// open task switcher and run task
	bool returnVal = true;
	try {
		taskList->run(task);
	} catch (std::string &error) {
		instances::logfile().conclude(error);
		returnVal = false;
	} catch (const char *error) {
		instances::logfile().conclude(error);
		returnVal = false;
	} catch (std::exception &error) {
		instances::logfile().conclude(error.what());
		returnVal = false;
	} catch (...) {
		instances::logfile().conclude("unhandled error!");
		returnVal = false;
	}
	instances::logfile().endIndent();
	return returnVal;
}

bool TTest::run(TTaskList *TaskList) {
	taskList = TaskList;
	return false;
}

}; // namespace coretools
