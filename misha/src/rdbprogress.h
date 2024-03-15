/*
 * rdbprogress.h
 *
 *  Created on: Dec 1, 2010
 *      Author: hoichman
 */

#ifndef RDBPROGRESS_H_
#define RDBPROGRESS_H_

#include <cstdint>
#include <stdint.h>

namespace rdb {

class Progress_reporter {
public:
	Progress_reporter() {}

	void init(uint64_t maxsteps, uint64_t init_report_step, uint64_t report_interval = 3000, uint64_t min_report_interval = 1000);
	void set_report_prefix(const char *report_prefix) { m_report_prefix = report_prefix; }

	void report(uint64_t delta_steps_done);
	void report_last();

	// returns elapsed time in msecs
	uint64_t get_elapsed() const { return m_elapsed_clock; }

	// return elapsed steps
	uint64_t get_elapsed_steps() const { return m_numsteps; }

private:
	uint64_t m_numsteps;
	uint64_t m_numsteps_from_last_report;
	uint64_t m_maxsteps;
	uint64_t m_report_step;
	uint64_t m_report_interval;
	uint64_t m_min_report_interval;
	uint64_t m_last_report_clock;
	uint64_t m_elapsed_clock;
	int      m_last_progress_reported;
	string   m_report_prefix;

	uint64_t get_cur_clock();
};

}

#endif /* RDBPROGRESS_H_ */
