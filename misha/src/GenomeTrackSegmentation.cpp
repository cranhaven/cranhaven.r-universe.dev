/*
 * GenomeTrackSegmentation.cpp
 *
 *  Created on: Jun 3, 2010
 *      Author: hoichman
 */

#include <cmath>
#include <vector>

#include "IncrementalWilcox.h"
#include "rdbinterval.h"
#include "rdbutils.h"
#include "GIntervalsBigSet1D.h"
#include "TrackExpressionIterator.h"
#include "TrackExpressionFixedBinIterator.h"
#include "TrackExpressionScanner.h"

using namespace std;
using namespace rdb;

namespace GSegment_ns {

struct Winval {
	double  v;
	int64_t coord;

	Winval(double _v, int64_t _coord) : v(_v), coord(_coord) {}
};
}

using namespace GSegment_ns;

void gsegment_add_interval2res(const GInterval &interval, GIntervals &res_intervals, const string &intervset_out,
							   vector<GIntervalsBigSet1D::ChromStat> &chromstats1d, IntervUtils &iu)
{
	static char error_prefix[1000];

	if (!intervset_out.empty()) {
		if (res_intervals.empty() || res_intervals.front().chromid != interval.chromid)
			snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chrom %s", intervset_out.c_str(), iu.id2chrom(interval.chromid).c_str());

		if (!res_intervals.empty() && res_intervals.front().chromid != interval.chromid)
			GIntervalsBigSet1D::save_chrom_plain_intervals(intervset_out.c_str(), res_intervals, iu, chromstats1d);
	}

	res_intervals.push_back(interval);

	if (intervset_out.empty())
		iu.verify_max_data_size(res_intervals.size(), "Result");
	else
		iu.verify_max_data_size(res_intervals.size(), error_prefix, false);
}

extern "C" {

SEXP C_gsegment(SEXP _expr, SEXP _intervals, SEXP _minsegment, SEXP _maxz, SEXP _one_tailed, SEXP _iterator_policy, SEXP _intervals_set_out, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_expr) || length(_expr) != 1)
			verror("Track expression argument is not a string");

		if (!isReal(_minsegment) || length(_minsegment) != 1)
			verror("Min segment argument is not numeric");

		if (!isReal(_maxz) || length(_maxz) != 1)
			verror("Max Z-score argument is not numeric");

		if (!isLogical(_one_tailed) || length(_one_tailed) != 1)
			verror("One-tailed argument is not boolean");

		string intervset_out = isNull(_intervals_set_out) ? "" : CHAR(STRING_ELT(_intervals_set_out, 0));

		IntervUtils iu(_envir);
		GIntervalsFetcher1D *intervals = NULL;
		iu.convert_rintervs(_intervals, &intervals, NULL);
		unique_ptr<GIntervalsFetcher1D> intervals_guard(intervals);
		intervals->sort();
		intervals->unify_overlaps();

		double minsegment = *REAL(_minsegment);
		double maxz = *REAL(_maxz);
		bool one_tailed = *LOGICAL(_one_tailed) == 1;

		if (minsegment < 0)
			verror("Min segment cannot be a negative number");
		if (minsegment != (int)minsegment)
			verror("Min segment must be an integer");

		GIntervals segments;
		TrackExprScanner scanner(iu);

		vector<Winval> window; // cyclic vector (replacement for deque)
		vector<double> segment_tail;

		int cur_scope_idx = -1;
		IncrementalWilcox wilcox(one_tailed);
		unsigned winsize = 0;
		int cur_chromid = -1;
		int64_t cur_coord = 0;
		int64_t segment_start_coord = 0;
		int64_t segment_end_coord = 0;
		int64_t best_end_coord = 0;
		bool segment_closing = false;
		double best_z = 1;
		unsigned best_sample_idx = 0;
		unsigned winidx = 0;
		vector<GIntervalsBigSet1D::ChromStat> chromstats;
		SEXP answer = R_NilValue;

		scanner.begin(_expr, intervals, NULL, _iterator_policy);

		if (scanner.get_iterator()->get_type() != TrackExpressionIteratorBase::FIXED_BIN)
			verror("gsegment() requires the iterator policy to be a fixed bin size.\n");

		if (!intervset_out.empty())
			GIntervalsBigSet1D::begin_save(intervset_out.c_str(), iu, chromstats);

		unsigned bin_size = ((TrackExpressionFixedBinIterator *)scanner.get_iterator())->get_bin_size();
		winsize = max((unsigned)(minsegment / bin_size + 0.5), 1u);

		for (; ; scanner.next()) {
			if (scanner.isend() || cur_scope_idx != scanner.last_scope_idx()) {
				if (cur_scope_idx >= 0) {
					// Interval ended. Most probably that we have samples that haven't been added yet to any segment.
					// Let's increase the last interval. This is not the best we can do but it's the easiest solution.
					// (We might be just in the middle of closing the last segment in which case we could add the unassigned values
					// to the new segment. However we have to determine first the boundary of the last segment. Too messy. Let's
					// make the life easier for the last segment.)
					if (segments.empty() || segments.back().chromid != cur_chromid) {
						GInterval interval(cur_chromid, segment_start_coord, cur_coord, 0);
						gsegment_add_interval2res(interval, segments, intervset_out, chromstats, iu);
					} else {
						GInterval &segment = segments.back();
						segment.end = cur_coord;
					}
				}

				if (scanner.isend())
					break;

				cur_scope_idx = scanner.last_scope_idx();
				cur_chromid = scanner.last_interval1d().chromid;
				wilcox.reset();
				cur_coord = segment_start_coord = segment_end_coord = best_end_coord = scanner.last_interval1d().start;
				segment_closing = false;
				best_z = 1;
				best_sample_idx = 0;
				winidx = 0;
				window.clear();
				segment_tail.clear();
				window.resize(winsize, Winval(-1, 0));
				segment_tail.resize(winsize, -1);
			}

			double v = scanner.last_real(0);

			if (!std::isnan(v)) {
				// 1. fill first the current segment
				if (wilcox.n1() < winsize) {
					wilcox.update(numeric_limits<double>::quiet_NaN(), v, numeric_limits<double>::quiet_NaN(), numeric_limits<double>::quiet_NaN());
					segment_end_coord = cur_coord;
				}

				// 2. then fill the window that comes afterwards
				else if (wilcox.n2() < winsize) {
					wilcox.update(numeric_limits<double>::quiet_NaN(), numeric_limits<double>::quiet_NaN(), numeric_limits<double>::quiet_NaN(), v);
					window[winidx] = Winval(v, cur_coord);
					winidx = (winidx + 1) % winsize;

				// 3. now increase the segment and move the window
				} else {
					double old_v = window[winidx].v;
					wilcox.update(numeric_limits<double>::quiet_NaN(), old_v, old_v, v);
					segment_end_coord = window[winidx].coord;
					window[winidx] = Winval(v, cur_coord);
					winidx = (winidx + 1) % winsize;
					if (segment_closing)
						segment_tail.push_back(old_v);
				}

				if (wilcox.n2() >= winsize) {
					double z = wilcox.z();

					if (z > 0)
						verror("Wilcoxon test is unreliable on small windows. Min segment size must be at least %d", IncrementalWilcox::MIN_RELIABLE_WINSIZE * bin_size);

					if (segment_closing) {
						if (z < best_z) {
							best_z = z;
							best_end_coord = segment_end_coord;
							best_sample_idx = segment_tail.size();
						}

						// at the end of buffer intended for checking => close the segment
						if (segment_tail.size() >= winsize - 1) {
							best_end_coord += bin_size;
							GInterval interval(cur_chromid, segment_start_coord, best_end_coord, 0);
							gsegment_add_interval2res(interval, segments, intervset_out, chromstats, iu);
							segment_closing = false;
							segment_start_coord = best_end_coord;

							// reinitiate Wilcoxon algorithm (clear the segment and the window)
							wilcox.reset();

							// We have decided where the segment ends (at best_end_coord). However we have already read a bunch of values
							// past the best_end_coord. Some of these values are stuck in "segment_tail", others - in "window".
							// We'd love to do a sort of "ungetc" of all these values back to "scanner" and clear the window. However TrackExprScanner does not
							// support "ungetting" values. Besides there was unknown (and more severe: unbound!) number of NaNs so ungetting is not really possible.
							// (An attempt to buffer those values might blow up the memory.) Instead we need to push some of the values to the beginning of the next
							// segment and leave the rest in the window. The actual bits are a tad ugly though...

							// 1. add the values from the tail of the segment to Wilcoxon win1
							// (we add the values in the order opposite to how they were read, however for Wilcoxon test it doesn't matter)
							unsigned num_tail_samples = (unsigned)(winsize - best_sample_idx - 1); // = number of samples at the tail of the window
							for (unsigned i = segment_tail.size() - num_tail_samples; i < segment_tail.size(); i++)
								wilcox.update(numeric_limits<double>::quiet_NaN(), segment_tail[i], numeric_limits<double>::quiet_NaN(), numeric_limits<double>::quiet_NaN());
							segment_tail.clear();

							// 2. add more values from the beginning of the window to Wilcoxon win1
							for (unsigned i = 0; i < winsize - num_tail_samples; i++) {
								wilcox.update(numeric_limits<double>::quiet_NaN(), window[winidx].v, numeric_limits<double>::quiet_NaN(), numeric_limits<double>::quiet_NaN());
								segment_end_coord = window[winidx].coord;
								winidx = (winidx + 1) % winsize;
							}

							// 3. add values from the end of the window to Wilcoxon win2
							for (unsigned i = 0; i < num_tail_samples; i++) {
								wilcox.update(numeric_limits<double>::quiet_NaN(), numeric_limits<double>::quiet_NaN(), numeric_limits<double>::quiet_NaN(), window[winidx].v);
								winidx = (winidx + 1) % winsize;
							}
						}

					// segment is not closing yet
					} else if (z <= maxz) {
						segment_closing = true;
						best_z = z;
						best_end_coord = segment_end_coord;
						best_sample_idx = 0;
						segment_tail.clear();
					}
				}
			}

			cur_coord = scanner.last_interval1d().end;
		}

		if (intervset_out.empty()) {
			if (!segments.empty()) 
				answer = iu.convert_intervs(&segments);
		} else {
			if (!segments.empty()) 
				GIntervalsBigSet1D::save_chrom_plain_intervals(intervset_out.c_str(), segments, iu, chromstats);
			GIntervalsBigSet1D::end_save_plain_intervals(intervset_out.c_str(), iu, chromstats);
		}

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

}
