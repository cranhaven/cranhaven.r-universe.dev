/*
 * TTesting.h
 *
 *  Created on: Dec 11, 2017
 *      Author: phaentu
 */

#ifndef TTESTING_H_
#define TTESTING_H_

#include <stdexcept>
#include <string> // for string

#include "coretools/IntegrationTests/TTestList.h"

namespace coretools {
class TTaskList;
class TTestList;

//-----------------------------------
// TTesting
//-----------------------------------
class TTesting {
private:
	std::string outputName;
	TTestList *testList;

	void printTests();

public:
	TTesting(TTestList *TestList);
	~TTesting();
	void runTests(TTaskList *taskList);
};

}; // namespace coretools

#endif /* TTESTING_H_ */
