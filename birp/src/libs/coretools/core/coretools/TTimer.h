/*
 * TTimer.h
 *
 *  Created on: Jun 30, 2021
 *      Author: phaentu
 */

#ifndef CORETOOLS_CORE_TTIMER_H_
#define CORETOOLS_CORE_TTIMER_H_

#include <cmath>
#include <cstdint>
#include <string>
#include <sys/time.h>

#include "coretools/Strings/toString.h"

// a suite of tools to measure and report progress during runtime

namespace coretools {

enum class TimeFormat : uint8_t { full = 0, abbreviated = 1, iso = 2 };

//----------------------------------------------------
// TTimer
// a class to measure and report execution time
//----------------------------------------------------
class TTimer {
private:
	struct timeval _start;
	mutable struct timeval _end;
	bool running;

	inline void _recordEnd() const {
		if (running) { gettimeofday(&_end, NULL); }
	};

public:
	TTimer() { start(); };

	void start() {
		gettimeofday(&_start, NULL);
		running = true;
	};

	inline void stop() {
		if (running) {
			_recordEnd();
			running = false;
		}
	};

	int seconds() const {
		_recordEnd();
		return _end.tv_sec - _start.tv_sec;
	};

	double minutes() const {
		_recordEnd();
		return round(100.0 * (double)seconds() / 60.0) / 100.0;
	};

	double hours() const {
		_recordEnd();
		return round(100.0 * seconds() / 3600.0) / 100.0;
	};

	std::string formattedTime(TimeFormat format = TimeFormat::full) const {
		_recordEnd();

		// get num days, hours, minutes and seconds
		int sec = seconds();

		int hours = sec / 3600;
		sec       = sec % 3600;

		int minutes = sec / 60;
		sec         = sec % 60;

		// assemble string
		std::string s;

		// hours
		if (hours > 0) {
			if (format == TimeFormat::iso) {
				if (hours < 10) {
					s = "0" + str::toString(hours) + ":";
				} else {
					s = str::toString(hours) + ":";
				}
			} else if (format == TimeFormat::full) {
				if (hours == 1) {
					s = "1 hour";
				} else {
					s = str::toString(hours) + " hours";
				}
			} else if (format == TimeFormat::abbreviated) {
				s = str::toString(hours) + "h";
			}
		}

		// minutes
		if (minutes > 0 || s.size() > 0) {
			if (format == TimeFormat::iso) {
				if (minutes < 10) {
					s += "0" + str::toString(minutes) + ":";
				} else {
					s += str::toString(minutes) + ":";
				}
			} else {
				if (!s.empty()) { s += " "; }
				if (format == TimeFormat::full) {
					if (minutes == 1) {
						s += "1 minute";
					} else {
						s += str::toString(minutes) + " minutes";
					}
				} else if (format == TimeFormat::abbreviated) {
					if (!s.empty()) { s += " "; }
					s += str::toString(minutes) + "m";
				}
			}
		}

		// seconds
		if (format == TimeFormat::iso) {
			if (sec < 10) {
				s += "0" + str::toString(sec);
			} else {
				s += str::toString(sec);
			}
		} else {
			if (!s.empty()) { s += " "; }
			if (format == TimeFormat::full) {
				if (sec == 1) {
					s += "1 second";
				} else {
					s += str::toString(sec) + " seconds";
				}
			} else if (format == TimeFormat::abbreviated) {
				if (!s.empty()) { s += " "; }
				s += str::toString(sec) + "s";
			}
		}

		// return string
		return s;
	};
};

}; // end namespace coretools

#endif /* CORETOOLS_CORE_TTIMER_H_ */
