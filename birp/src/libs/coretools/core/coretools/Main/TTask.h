/*
 * TTask.h
 *
 *  Created on: Mar 31, 2019
 *      Author: phaentu
 */

#ifndef TTASK_H_
#define TTASK_H_

#include "coretools/Main/TLog.h"
#include "coretools/Main/TParameters.h"
#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Strings/toString.h"

namespace coretools {

//---------------------------------------------------------------------------
// How to use it:
// Create a class for each task, as shown in the example below for task NAME
//---------------------------------------------------------------------------
//	class TTask_NAME:public TTask{
//		public:
//		TTask_NAME(){ _explanation = "SAY WHAT THIS TASK IS DOING"; };

//		void run(){
//			SPECIFY HOW TO EXECUTE THIS TASK
//		};
//	};
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// TTask
//---------------------------------------------------------------------------
class TTask {
private:
	std::string _explanation;
	mutable std::vector<std::string> _citations;
	bool _randomGeneratorInitialized = false;

	void initializeRandomGenerator() {
		using instances::parameters;
		using instances::randomGenerator;
		if (!_randomGeneratorInitialized) {
			if (parameters().exists("fixedSeed")) {
				randomGenerator().setSeed(parameters().get<long>("fixedSeed"), true);
			} else if (parameters().exists("addToSeed")) {
				randomGenerator().setSeed(parameters().get<long>("addToSeed"), false);
			}
			instances::logfile().list("Initializing random generator with seed ", randomGenerator().getSeed(), "!");
			_randomGeneratorInitialized = true;
			parameters().add("fixedSeed", randomGenerator().getSeed(), true);
		}
	}

public:
	template<typename... Cs>
	TTask(std::string_view Explanation, Cs... Citations) : _explanation(Explanation) {
			(_citations.push_back(Citations), ...);
	}
	virtual ~TTask() = default;

	void addCitation(std::string_view Citation) { _citations.emplace_back(Citation); }

	void run(std::string_view taskName) {
		using instances::parameters;
		using instances::logfile;
		logfile().startIndent(_explanation, " (task = ", taskName, "):");

		printCitations();
		initializeRandomGenerator();
		run();

		if (parameters().exists("out")) {
			parameters().writeFile(str::toString(parameters().get("out"), ".parameters"));
		} else {
			parameters().writeFile(str::toString(taskName, ".parameters"));
		}
		logfile().endIndent();
	};

	virtual void run() = 0;
	const std::string& explanation() const { return _explanation; }

	void printCitations() const {
		// write citations, if there are any
		if (!_citations.empty()) {
			// sort and remove doubles
			std::sort(_citations.begin(), _citations.end());
			_citations.erase(std::unique(_citations.begin(), _citations.end()), _citations.end());

			instances::logfile().startIndent("Relevant citations:");
			for (auto it = _citations.begin(); it != _citations.end(); ++it) { instances::logfile().list(*it); }
			instances::logfile().endIndent();
		}
	};
};

}; // namespace coretools

#endif /* TTASK_H_ */
