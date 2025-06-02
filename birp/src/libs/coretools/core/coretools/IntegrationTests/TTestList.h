//
// Created by caduffm on 3/3/20.
//

#ifndef TTESTLIST_H
#define TTESTLIST_H

#include "coretools/IntegrationTests/TTest.h"

namespace coretools {

//------------------------------------------
// TTestList
//------------------------------------------
class TTestList {
private:
	// map of name and test pointer of all possible tests
	std::map<std::string, TTest *, std::less<>> testMap;

	// maps test suit to test names
	std::map<std::string, std::vector<std::string>, std::less<>> testSuites;

	// vector of pointers to tests that should be performed
	std::vector<TTest *> testsToPerform;

public:
	TTestList() = default;

	~TTestList() {
		for (auto &it : testMap) delete it.second;
	}

	// functions to add tests and test suites; no matter if they will be used or not
	void addTest(std::string_view name, TTest *test) { testMap.insert(std::pair<std::string, TTest *>(name, test)); }

	void addTestSuite(std::string_view name, const std::vector<std::string> &suite) {
		testSuites.emplace(name, suite);
	}

	// now: check if test should actually be performed
	void parseTests() {
		// go over every test in test map and check if it should be performed
		for (auto &it : testMap) {
			if (instances::parameters().exists(it.first)) { initializeTest(it.first); }
		}
	}

	void parseSuites() {
		// go over every test suite in test suite map and check if it should be performed
		for (auto &testSuite : testSuites) {
			if (instances::parameters().exists(testSuite.first)) {
				for (const auto &i : testSuite.second) { initializeTest(i); }
			}
		}
	};

	void initializeTest(std::string_view name) {
		// only initialize test if it is not yet in testsToPerform
		auto it1 = testMap.find(name); // find task corresponding to name
		if (it1 != testMap.end()) {    // check if tests exists (e.g. testSuite may contain test that does not exist)
			auto it2 = std::find(testsToPerform.begin(), testsToPerform.end(), it1->second);
			if (it2 == testsToPerform.end()) {
				// if test is not yet in testsToPerform -> add it
				testsToPerform.push_back(it1->second);
			}
		}
	}

	size_t size() { return testsToPerform.size(); };

	void printTestToLogfile() {
		for (auto &it : testsToPerform) { instances::logfile().list(it->name()); }
	};

	TTest *operator[](size_t num) {
		if (num < testsToPerform.size())
			return testsToPerform[num];
		else
			DEVERROR("Test number out of range!");
	};
};

}; // namespace coretools

#endif // TTESTLIST_H
