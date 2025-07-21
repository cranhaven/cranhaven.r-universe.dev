/*
 * progressTools.h
 *
 *  Created on: May 16, 2020
 *      Author: phaentu
 */

#ifndef COMMONUTILITIES_PROGRESSTOOLS_H_
#define COMMONUTILITIES_PROGRESSTOOLS_H_

#include <cmath>
#include <ctime>

#include "coretools/Main/TLog.h"
#include "coretools/TTimer.h"

// a suite of tools to measure and report progress during runtime

namespace coretools {

//-------------------------------------
// TProgressReporter
//-------------------------------------
template<typename T> class TProgressReporter {
private:
	std::string _progressString;
	T _goal;
	T _cur;
	uint32_t _lastPrintedPercentage;
	TTimer _timer;

public:
	TProgressReporter(const T &Goal, std::string_view ProgressString) {
		_goal                  = Goal;
		_progressString.append(ProgressString).append(" ...");
		_cur                   = 0;
		_lastPrintedPercentage = 0;
		_timer.start();
		instances::logfile().listFlush(_progressString);
	}

	void next() {
		++_cur;

		// check if we need to print next percentage
		const uint32_t p = 100.0 * (double)_cur / (double)_goal;
		if (p > _lastPrintedPercentage) {
			_lastPrintedPercentage = p;
			instances::logfile().overListFlush(_progressString, "(", _lastPrintedPercentage, "% in ",
											   _timer.formattedTime(), ")");
		}
	}

	void done() const {
		instances::logfile().overList(_progressString, " done (in ", _timer.formattedTime(), ")!  ");
	}
};

}; // namespace coretools

#endif /* COMMONUTILITIES_PROGRESSTOOLS_H_ */
