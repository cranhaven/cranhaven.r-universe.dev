/*
 * GenomeTrackPartition.cpp
 *
 *  Created on: Dec 26, 2010
 *      Author: hoichman
 */

#include <cmath>

#include "BinFinder.h"
#include "rdbinterval.h"
#include "rdbutils.h"
#include "GenomeTrack.h"
#include "GIntervalsBigSet1D.h"
#include "GIntervalsBigSet2D.h"
#include "TrackExpressionScanner.h"

using namespace std;
using namespace rdb;

template <typename Interval, typename Intervals>
SEXP gpartition_build_answer(Intervals &res_intervals, const vector<int> &res_bins, const BinFinder &bin_finder, bool include_lowest, IntervUtils &iu)
{
	SEXP answer;
	SEXP bins;

	answer = iu.convert_intervs(&res_intervals, Interval::NUM_COLS + 1, false);
	rprotect(bins = RSaneAllocVector(REALSXP, res_bins.size()));
	for (unsigned i = 0; i < res_bins.size(); i++)
		REAL(bins)[i] = res_bins[i];

	SET_VECTOR_ELT(answer, Interval::NUM_COLS, bins);
	SEXP colnames = getAttrib(answer, R_NamesSymbol);
	SET_STRING_ELT(colnames, Interval::NUM_COLS, mkChar("bin"));

	SEXP range;
	int numbins = bin_finder.get_numbins();
	rprotect(range = RSaneAllocVector(STRSXP, numbins));
	for (int bin = 0; bin < numbins; bin++) {
		char buf[10000];

		snprintf(buf, sizeof(buf), "%c%g, %g]", bin || !include_lowest ? '(' : '[', bin_finder.get_breaks()[bin], bin_finder.get_breaks()[bin + 1]);
		SET_STRING_ELT(range, bin, mkChar(buf));
	}
	setAttrib(answer, install("range"), range);
	return answer;
}

static void gpartition_add_interval2res(const GInterval &interval, GIntervals &res_intervals, vector<int> &res_bins, int bin,
										const BinFinder &bin_finder, bool include_lowest, const string &intervset_out,
										vector<GIntervalsBigSet1D::ChromStat> &chromstats, IntervUtils &iu)
{
	static char error_prefix[1000];

	if (!intervset_out.empty()) {
		if (res_intervals.empty() || res_intervals.front().chromid != interval.chromid)
			snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chrom %s", intervset_out.c_str(), iu.id2chrom(interval.chromid).c_str());

		if (!res_intervals.empty() && res_intervals.front().chromid != interval.chromid) {
			SEXP answer = gpartition_build_answer<GInterval>(res_intervals, res_bins, bin_finder, include_lowest, iu);
			GIntervalsBigSet1D::save_chrom(intervset_out.c_str(), &res_intervals, answer, iu, chromstats);
			res_intervals.clear();
			res_bins.clear();
		}
	}

	res_intervals.push_back(interval);
	res_bins.push_back(bin);

	if (intervset_out.empty())
		iu.verify_max_data_size(res_intervals.size(), "Result");
	else
		iu.verify_max_data_size(res_intervals.size(), error_prefix, false);
}

static void gpartition_add_interval2res(const GInterval2D &interval, GIntervals2D &res_intervals, vector<int> &res_bins, int bin,
										const BinFinder &bin_finder, bool include_lowest, const string &intervset_out,
										vector<GIntervalsBigSet2D::ChromStat> &chromstats, IntervUtils &iu)
{
	static char error_prefix[1000];

	if (!intervset_out.empty()) {
		if (res_intervals.empty() || !interval.is_same_chrom(res_intervals.back()))
			snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chroms (%s, %s)",
					 intervset_out.c_str(), iu.id2chrom(interval.chromid1()).c_str(), iu.id2chrom(interval.chromid2()).c_str());

		if (!res_intervals.empty() && !interval.is_same_chrom(res_intervals.back())) {
			SEXP answer = gpartition_build_answer<GInterval2D>(res_intervals, res_bins, bin_finder, include_lowest, iu);
			GIntervalsBigSet2D::save_chrom(intervset_out.c_str(), &res_intervals, answer, iu, chromstats);
			res_intervals.clear();
			res_bins.clear();
		}
	}

	res_intervals.push_back(interval);
	res_bins.push_back(bin);

	if (intervset_out.empty())
		iu.verify_max_data_size(res_intervals.size(), "Result");
	else
		iu.verify_max_data_size(res_intervals.size(), error_prefix, false);
}


extern "C" {

SEXP C_gpartition(SEXP _intervals, SEXP _track_expr, SEXP _breaks, SEXP _include_lowest, SEXP _iterator_policy, SEXP _band,
				SEXP _intervals_set_out, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		// check the arguments
		if (!isString(_track_expr) || length(_track_expr) != 1)
			verror("Expression argument is not a string");

		if (!isReal(_breaks))
			verror("Breaks argument is not a number");

		if (!isLogical(_include_lowest) || length(_include_lowest) != 1)
			verror("include.lowest argument is not logical");

		if (!isNull(_intervals_set_out) && (!isString(_intervals_set_out) || length(_intervals_set_out) != 1))
			verror("intervals.set.out argument is not a string");

		string intervset_out = isNull(_intervals_set_out) ? "" : CHAR(STRING_ELT(_intervals_set_out, 0));

		bool include_lowest = LOGICAL(_include_lowest)[0];
		BinFinder bin_finder(REAL(_breaks), length(_breaks), include_lowest);

		GIntervals res_intervals;
		vector<int> res_bins;
		IntervUtils iu(_envir);
		TrackExprScanner scanner(iu);
		GIntervalsFetcher1D *intervals1d = NULL;
		GIntervalsFetcher2D *intervals2d = NULL;
		iu.convert_rintervs(_intervals, &intervals1d, &intervals2d);
		unique_ptr<GIntervalsFetcher1D> intervals1d_guard(intervals1d);
		unique_ptr<GIntervalsFetcher2D> intervals2d_guard(intervals2d);
		intervals1d->sort();
		intervals1d->unify_overlaps();
		intervals2d->sort();
		intervals2d->verify_no_overlaps(iu.get_chromkey());
		SEXP answer = R_NilValue;

		scanner.begin(_track_expr, intervals1d, intervals2d, _iterator_policy, _band);

		if (scanner.get_iterator()->is_1d()) {
			int last_bin = -1;
			GInterval interval(-1, -1, -1, -1);
			vector<GIntervalsBigSet1D::ChromStat> chromstats;

			if (!intervset_out.empty())
				GIntervalsBigSet1D::begin_save(intervset_out.c_str(), iu, chromstats);

			for (; !scanner.isend(); scanner.next()) {
				const GInterval &cur_interval = scanner.last_interval1d();
				double val = scanner.last_real(0);
				int cur_bin = std::isnan(val) ? -1 : bin_finder.val2bin(val);

				if (last_bin >= 0 && (cur_bin != last_bin || cur_interval.start != interval.end || cur_interval.chromid != interval.chromid))
					gpartition_add_interval2res(interval, res_intervals, res_bins, last_bin + 1, bin_finder, include_lowest, intervset_out, chromstats, iu);

				if (cur_bin < 0)
					interval.start = -1;
				else if (last_bin == cur_bin && cur_interval.start == interval.end && cur_interval.chromid == interval.chromid)
					interval.end = cur_interval.end;
				else
					interval = cur_interval;

				last_bin = cur_bin;
			}

			if (interval.start != -1)
				gpartition_add_interval2res(interval, res_intervals, res_bins, last_bin + 1, bin_finder, include_lowest, intervset_out, chromstats, iu);

			if (!intervset_out.empty()) {
				if (!res_intervals.empty()) {
					SEXP answer = gpartition_build_answer<GInterval>(res_intervals, res_bins, bin_finder, include_lowest, iu);
					GIntervalsBigSet1D::save_chrom(intervset_out.c_str(), &res_intervals, answer, iu, chromstats);
					res_intervals.clear();
					res_bins.clear();
				}
				SEXP zeroline = gpartition_build_answer<GInterval>(res_intervals, res_bins, bin_finder, include_lowest, iu);
				GIntervalsBigSet1D::end_save(intervset_out.c_str(), zeroline, iu, chromstats);
			} else if (!res_intervals.empty())
				answer = gpartition_build_answer<GInterval>(res_intervals, res_bins, bin_finder, include_lowest, iu);
		} else {
			GIntervals2D res_intervals;
			vector<GIntervalsBigSet2D::ChromStat> chromstats;

			if (!intervset_out.empty())
				GIntervalsBigSet2D::begin_save(intervset_out.c_str(), iu, chromstats);

			for (; !scanner.isend(); scanner.next()) {
				const GInterval2D &cur_interval = scanner.last_interval2d();
				double val = scanner.last_real(0);
				int cur_bin = std::isnan(val) ? -1 : bin_finder.val2bin(val);

				if (cur_bin >= 0)
					gpartition_add_interval2res(cur_interval, res_intervals, res_bins, cur_bin + 1, bin_finder, include_lowest, intervset_out, chromstats, iu);
			}

			if (!intervset_out.empty()) {
				if (!res_intervals.empty()) {
					SEXP answer = gpartition_build_answer<GInterval2D>(res_intervals, res_bins, bin_finder, include_lowest, iu);
					GIntervalsBigSet2D::save_chrom(intervset_out.c_str(), &res_intervals, answer, iu, chromstats);
					res_intervals.clear();
					res_bins.clear();
				}
				SEXP zeroline = gpartition_build_answer<GInterval2D>(res_intervals, res_bins, bin_finder, include_lowest, iu);
				GIntervalsBigSet2D::end_save(intervset_out.c_str(), zeroline, iu, chromstats);
			} else if (!res_intervals.empty())
				answer = gpartition_build_answer<GInterval2D>(res_intervals, res_bins, bin_finder, include_lowest, iu);
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
