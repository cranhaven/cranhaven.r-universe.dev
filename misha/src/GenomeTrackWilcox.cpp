/*
 * GenomeTrackWilcox.cpp
 *
 *  Created on: May 26, 2010
 *      Author: hoichman
 */

#include <cmath>
#include <vector>

#include "rdbinterval.h"
#include "rdbutils.h"
#include "IncrementalWilcox.h"
#include "GIntervalsBigSet1D.h"
#include "TrackExpressionFixedBinIterator.h"
#include "TrackExpressionScanner.h"

using namespace std;
using namespace rdb;

static SEXP build_rintervals_wilcox(const vector<IntervalPval> &res_intervals, GIntervals &out_intervals, IntervUtils &iu)
{
	out_intervals.clear();
	out_intervals.reserve(res_intervals.size());
	for (vector<IntervalPval>::const_iterator iinterval = res_intervals.begin(); iinterval != res_intervals.end(); ++iinterval)
		out_intervals.push_back((GInterval)*iinterval);

	SEXP answer = iu.convert_intervs(&out_intervals, IntervalPval::NUM_COLS, false);
	SEXP pvals;

	rprotect(pvals = RSaneAllocVector(REALSXP, res_intervals.size()));
	for (unsigned i = 0; i < res_intervals.size(); i++)
		REAL(pvals)[i] = res_intervals[i].minpval;

	SET_VECTOR_ELT(answer, IntervalPval::PVAL, pvals);
	SEXP colnames = getAttrib(answer, R_NamesSymbol);
	SET_STRING_ELT(colnames, IntervalPval::PVAL, mkChar(IntervalPval::COL_NAMES[IntervalPval::PVAL]));
	return answer;
}

class GenomeTrackSlidingWilcox {
public:
	enum What2find { FIND_LOWS_AND_HIGHS, FIND_LOWS, FIND_HIGHS };

private:
	enum { LARGE, SMALL, NUM_WINS }; // types of windows

	What2find m_what2find;
	unsigned m_num_read_samples;
	unsigned m_winsize[NUM_WINS];       // win size in samples
	unsigned m_winsize_aside[NUM_WINS]; // win size left or right to the central value
	unsigned m_tail[NUM_WINS];          // index of the tail
	unsigned m_center;                  // index of the central value
	vector<double> m_queue;             // queue of values (cyclic vector) - used for debugging only

	unsigned          m_binsize;
	int64_t           m_start_coord;
	int64_t           m_center_coord;
	double            m_maxz;
	double            m_minpval;
	int               m_chromid;
	vector<IntervalPval> &m_intervals;

	IncrementalWilcox m_wilcox;

	void debug_slide(double *old_v, double *new_v);

public:
	GenomeTrackSlidingWilcox(bool one_tailed, What2find what2find,
			unsigned winsize_in_coord1, unsigned winsize_in_coord2, unsigned binsize, int chromid, vector<IntervalPval> &res_intervals, double maxz);

	~GenomeTrackSlidingWilcox();

	void set_next_sample(double v);
};

GenomeTrackSlidingWilcox::GenomeTrackSlidingWilcox(bool one_tailed, What2find what2find, unsigned winsize_in_coord1, unsigned winsize_in_coord2,
		unsigned binsize, int chromid, vector<IntervalPval> &res_intervals, double maxz) :
			m_what2find(what2find), m_binsize(binsize), m_maxz(maxz), m_chromid(chromid), m_intervals(res_intervals), m_wilcox(one_tailed)
{
	unsigned winsize_in_coord[NUM_WINS];

	winsize_in_coord[LARGE] = max(winsize_in_coord1, winsize_in_coord2);
	winsize_in_coord[SMALL] = min(winsize_in_coord1, winsize_in_coord2);

	// We use the same queue for both sliding windows: large and small. This is the most effective.
	// The large sliding window runs afront of small window - it contains values at the front and at the tail.
	// However the indexing of two different windows in one cyclic buffer is quite messy: head and tail are
	// the same for the large window yet they differ for the small one... Tough luck!

	for (int i = 0; i < NUM_WINS; i++) {
		m_winsize_aside[i] = (unsigned)(0.5 * winsize_in_coord[i] / m_binsize + 0.5);
		m_winsize[i] = 2 * m_winsize_aside[i] + 1;
		if (m_winsize[i] < IncrementalWilcox::MIN_RELIABLE_WINSIZE)
			verror("Window of size %d containes too few samples (%d) to run Wilcoxon test", winsize_in_coord[i], m_winsize[i]);
//REprintf("winsize[%d] = %d\n", i, m_winsize[i]);
	}

	m_queue.resize(m_winsize[LARGE], numeric_limits<double>::quiet_NaN());

	m_center = m_winsize_aside[LARGE];

	// tail[SMALL] goes behind left[LARGE] by delta = winsize_aside[LARGE] - winsize_aside[SMALL]
	m_tail[LARGE] = 0;
	m_tail[SMALL] = (m_winsize_aside[LARGE] + m_winsize_aside[SMALL] + 1) % m_winsize[LARGE];

	m_start_coord = -1;
	// An attempt to write "-m_binsize * m_winsize_aside[LARGE]" produces a huge positive number rather
	// than a negative number. This happens because both arguments are unsigned.
	m_center_coord = -1 * int64_t(m_binsize * m_winsize_aside[LARGE]);
}

GenomeTrackSlidingWilcox::~GenomeTrackSlidingWilcox()
{
	for (unsigned i = 0; i <= m_winsize_aside[LARGE]; i++)
		set_next_sample(numeric_limits<double>::quiet_NaN());
}

void GenomeTrackSlidingWilcox::set_next_sample(double v)
{
	m_num_read_samples++;

	double old_v[NUM_WINS];
	double new_v[NUM_WINS];

	// head=tail for the large window
	old_v[LARGE] = m_queue[m_tail[LARGE]];

	// head=tail-winsize for small window, yet tail-winsize might be negative, so we need to wrap around it
	old_v[SMALL] = m_queue[(m_tail[SMALL] + m_winsize[LARGE] - m_winsize[SMALL]) % m_winsize[LARGE]];

	m_queue[m_tail[LARGE]] = v;

	new_v[LARGE] = v;
	new_v[SMALL] = m_queue[m_tail[SMALL]];

	m_tail[LARGE] = (m_tail[LARGE] + 1) % m_winsize[LARGE];
	m_tail[SMALL] = (m_tail[SMALL] + 1) % m_winsize[LARGE];
	m_center = (m_center + 1) % m_winsize[LARGE];

//REprintf("peak = %g, coord = %d\n", m_queue[m_center], (int)m_center_coord);
	m_wilcox.update(old_v[LARGE], new_v[LARGE], old_v[SMALL], new_v[SMALL]);
	double z;

	if (m_what2find == FIND_HIGHS)
		z = m_wilcox.z_highs();
	else if (m_what2find == FIND_LOWS)
		z = m_wilcox.z_lows();
	else
		z = m_wilcox.z();
//REprintf("z = %g\n", z);
//debug_slide(old_v, new_v);

	if (std::isnan(m_queue[m_center]) || z > m_maxz) {
		if (m_start_coord != -1) {
			int64_t start = max((int64_t)0, m_start_coord - m_winsize_aside[SMALL] * m_binsize);
			int64_t end = m_center_coord + m_winsize_aside[SMALL] * m_binsize;

			if (m_intervals.empty() || m_intervals.back().chromid != m_chromid || m_intervals.back().end < start)
				m_intervals.push_back(IntervalPval(m_chromid, start, end, 0, m_minpval));
			else {
				// unify overlapping intervals
				m_intervals.back().end = end;
				m_intervals.back().minpval = min(m_intervals.back().minpval, m_minpval);
			}
			m_start_coord = -1;
		}
	} else {
		double pval;

		if (m_what2find == FIND_HIGHS)
			pval = m_wilcox.pval_highs();
		else if (m_what2find == FIND_LOWS)
			pval = m_wilcox.pval_lows();
		else
			pval = m_wilcox.pval();

		if (m_start_coord == -1) {
			m_start_coord = m_center_coord;
			m_minpval = pval;
		} else
			m_minpval = min(m_minpval, pval);
	}

	m_center_coord += m_binsize;
//static int ccc = 1;
//if (ccc++>=40)
//verror("stam\n");
}

#include <deque>

void GenomeTrackSlidingWilcox::debug_slide(double *old_v, double *new_v)
{
	static deque<double> q[2];
	for (int i = 0; i < 2; i++) {
		if (!std::isnan(old_v[i])) {
			if (q[i].front() != old_v[i])
				verror("Deleting non existing val %g", old_v[i]);
			q[i].pop_front();
		}
		if (!std::isnan(new_v[i]))
			q[i].push_back(new_v[i]);

		REprintf("Q[%d] =", i);
		for (deque<double>::iterator iq = q[i].begin(); iq != q[i].end(); ++iq)
			REprintf(" %g", *iq);
		REprintf("\n");
	}
	REprintf("\n");
}


//------------------------------------ gwilcox ----------------------------------

extern "C" {

SEXP C_gwilcox(SEXP _expr, SEXP _intervals, SEXP _winsize1, SEXP _winsize2, SEXP _maxz, SEXP _one_tailed, SEXP _what2find, SEXP _iterator_policy,
			 SEXP _intervals_set_out, SEXP _envir)
{
	try {
		enum { LARGE, SMALL, NUM_WINS }; // types of windows

		RdbInitializer rdb_init;

		if (!isString(_expr) || length(_expr) != 1)
			verror("Track expression argument is not a string");

		if (!isReal(_winsize1) || length(_winsize1) != 1)
			verror("Winsize1 argument is not numeric");

		if (!isReal(_winsize2) || length(_winsize2) != 1)
			verror("Winsize2 argument is not numeric");

		if (!isReal(_maxz) || length(_maxz) != 1)
			verror("Max Z-score argument is not numeric");

		if (!isLogical(_one_tailed) || length(_one_tailed) != 1)
			verror("One-tailed argument is not boolean");

		if (!isInteger(_what2find) || length(_what2find) != 1)
			verror("What2find argument is not an integer");

		if (!isNull(_intervals_set_out) && (!isString(_intervals_set_out) || length(_intervals_set_out) != 1))
			verror("intervals.set.out argument is not a string");

		string intervset_out = isNull(_intervals_set_out) ? "" : CHAR(STRING_ELT(_intervals_set_out, 0));

		IntervUtils iu(_envir);
		GIntervalsFetcher1D *intervals = NULL;
		iu.convert_rintervs(_intervals, &intervals, NULL);
		unique_ptr<GIntervalsFetcher1D> intervals_guard(intervals);
		intervals->sort();
		intervals->unify_overlaps();

		double winsize[2] = { *REAL(_winsize1), *REAL(_winsize2) };
		double maxz = *REAL(_maxz);
		bool one_tailed = *LOGICAL(_one_tailed) == 1;
		GenomeTrackSlidingWilcox::What2find what2find;

		if (INTEGER(_what2find)[0] < 0)
			what2find = GenomeTrackSlidingWilcox::FIND_LOWS;
		else if (INTEGER(_what2find)[0] > 0)
			what2find = GenomeTrackSlidingWilcox::FIND_HIGHS;
		else
			what2find = GenomeTrackSlidingWilcox::FIND_LOWS_AND_HIGHS;

		for (int i = 0; i < 2; i++) {
			if (winsize[i] < 0)
				verror("Winsize cannot be a negative number");
			if (winsize[i] != (int)winsize[i])
				verror("Winsize must be an integer");
		}

		vector<IntervalPval> res_intervals;
		GIntervals out_intervals;
		TrackExprScanner scanner(iu);
		GenomeTrackSlidingWilcox *wilcox = NULL;
		GInterval last_interval(-1, -1, -1, -1);
		vector<GIntervalsBigSet1D::ChromStat> chromstats;

		scanner.begin(_expr, intervals, NULL, _iterator_policy);

		if (scanner.get_iterator()->get_type() != TrackExpressionIteratorBase::FIXED_BIN)
			verror("gwilcox() requires the iterator policy to be a fixed bin size.\n");

		unsigned bin_size = ((TrackExpressionFixedBinIterator *)scanner.get_iterator())->get_bin_size();

		if (!intervset_out.empty())
			GIntervalsBigSet1D::begin_save(intervset_out.c_str(), iu, chromstats);

		for (; !scanner.isend(); scanner.next()) {
			const GInterval &cur_interval = scanner.last_interval1d();

			if (last_interval.chromid != cur_interval.chromid || last_interval.end != cur_interval.start) {
				delete wilcox;
				wilcox = NULL;

				if (!intervset_out.empty() && !res_intervals.empty()) {
					SEXP answer = build_rintervals_wilcox(res_intervals, out_intervals, iu);
					GIntervalsBigSet1D::save_chrom(intervset_out.c_str(), &out_intervals, answer, iu, chromstats);
					res_intervals.clear();
					out_intervals.clear();
				}

				wilcox = new GenomeTrackSlidingWilcox(one_tailed, what2find, (unsigned)winsize[0], (unsigned)winsize[1], bin_size, cur_interval.chromid, res_intervals, maxz);
			}
			wilcox->set_next_sample(scanner.last_real(0));
			iu.verify_max_data_size(res_intervals.size(), "Result");
			last_interval = cur_interval;
		}
		delete wilcox;

		if (res_intervals.empty())
			return R_NilValue;

		SEXP answer = R_NilValue;

		if (!intervset_out.empty()) {
			if (!res_intervals.empty()) {
				SEXP answer = build_rintervals_wilcox(res_intervals, out_intervals, iu);
				GIntervalsBigSet1D::save_chrom(intervset_out.c_str(), &out_intervals, answer, iu, chromstats);
				res_intervals.clear();
				out_intervals.clear();
			}
			SEXP zeroline = build_rintervals_wilcox(res_intervals, out_intervals, iu);
			GIntervalsBigSet1D::end_save(intervset_out.c_str(), zeroline, iu, chromstats);
		} else
			answer = build_rintervals_wilcox(res_intervals, out_intervals, iu);

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
	}
	return R_NilValue;
}

}
