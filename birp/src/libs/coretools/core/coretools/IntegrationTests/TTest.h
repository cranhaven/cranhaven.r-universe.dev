//
// Created by caduffm on 3/3/20.
//

#ifndef APPROXWF2_TTEST_H
#define APPROXWF2_TTEST_H

#include <string> // for string

#include "coretools/Main/TTaskList.h"

namespace coretools {
class TTaskList;

class TTest {
protected:
	std::string _name;
	TTaskList *taskList;

	bool runMain(std::string_view task);

public:
	TTest();
	virtual ~TTest() = default;

	std::string name() { return _name; };
	virtual bool run(TTaskList *TaskList);
};

}; // namespace coretools

#endif // APPROXWF2_TTEST_H
