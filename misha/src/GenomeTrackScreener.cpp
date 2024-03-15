/*
 * GenomeTrackScreener.cpp
 *
 *  Created on: Mar 14, 2010
 *      Author: hoichman
 */

#include <cstdint>
#include <string>
#include <vector>

#include "GIntervalsBigSet1D.h"
#include "GIntervalsBigSet2D.h"
#include "rdbinterval.h"
#include "rdbutils.h"
#include "TrackExpressionScanner.h"

using namespace std;
using namespace rdb;

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>

void gscreen_add_interval2res(const GInterval &interval, GIntervals &res_intervals, const string &intervset_out,
							  vector<GIntervalsBigSet1D::ChromStat> &chromstats1d, IntervUtils &iu)
{
	static char error_prefix[1000];
	static GInterval last_interval;

	if (last_interval.chromid != interval.chromid) {
		last_interval = interval;
		snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chrom %s", intervset_out.c_str(), iu.id2chrom(interval.chromid).c_str());
	}

	if (!intervset_out.empty() && res_intervals.size() && res_intervals.front().chromid != interval.chromid)
		GIntervalsBigSet1D::save_chrom_plain_intervals(intervset_out.c_str(), res_intervals, iu, chromstats1d);

	res_intervals.push_back(interval);

	if (intervset_out.empty())
		iu.verify_max_data_size(res_intervals.size(), "Result");
	else
		iu.verify_max_data_size(res_intervals.size(), error_prefix, false);
}

void gscreen_add_interval2res(const GInterval2D &interval, GIntervals2D &res_intervals, const string &intervset_out,
							  vector<GIntervalsBigSet2D::ChromStat> &chromstats2d, IntervUtils &iu)
{
	static char error_prefix[1000];
	static GInterval2D last_interval;

	if (!last_interval.is_same_chrom(interval)) {
		last_interval = interval;
		snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chroms (%s, %s)",
				intervset_out.c_str(), iu.id2chrom(interval.chromid1()).c_str(), iu.id2chrom(interval.chromid2()).c_str());
	}

	if (!intervset_out.empty() && res_intervals.size() && !res_intervals.front().is_same_chrom(interval))
		GIntervalsBigSet2D::save_chrom_plain_intervals(intervset_out.c_str(), res_intervals, iu, chromstats2d); 

	res_intervals.push_back(interval);

	if (intervset_out.empty())
		iu.verify_max_data_size(res_intervals.size(), "Result");
	else
		iu.verify_max_data_size(res_intervals.size(), error_prefix, false);
}

extern "C" {

SEXP C_gscreen(SEXP _expr, SEXP _intervals, SEXP _iterator_policy, SEXP _band, SEXP _intervals_set_out, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_expr) || length(_expr) != 1)
			verror("Expression argument is not a string");

		if (!isNull(_intervals_set_out) && (!isString(_intervals_set_out) || length(_intervals_set_out) != 1))
			verror("intervals.set.out argument is not a string");

		string intervset_out = isNull(_intervals_set_out) ? "" : CHAR(STRING_ELT(_intervals_set_out, 0));

		IntervUtils iu(_envir);
		GIntervalsFetcher1D *intervals1d = NULL;
		GIntervalsFetcher2D *intervals2d = NULL;
		iu.convert_rintervs(_intervals, &intervals1d, &intervals2d);
		unique_ptr<GIntervalsFetcher1D> intervals1d_guard(intervals1d);
		unique_ptr<GIntervalsFetcher2D> intervals2d_guard(intervals2d);
		intervals1d->sort();
		intervals1d->unify_overlaps();
		intervals2d->sort();
		intervals2d->verify_no_overlaps(iu.get_chromkey());

		TrackExprScanner scanner(iu);

		scanner.begin(_expr, intervals1d, intervals2d, _iterator_policy, _band);

		if (scanner.get_iterator()->is_1d()) {
			GIntervals res_intervals;
			GInterval interval(-1, -1, -1, -1);
			vector<GIntervalsBigSet1D::ChromStat> chromstats;

			if (!intervset_out.empty())
				GIntervalsBigSet1D::begin_save(intervset_out.c_str(), iu, chromstats);

			for (; !scanner.isend(); scanner.next()) {
				if (scanner.last_logical(0) == 1) {
					const GInterval &last_interval = scanner.last_interval1d();

					if (interval.end != last_interval.start || interval.chromid != last_interval.chromid) {
						if (interval.start != -1)
							gscreen_add_interval2res(interval, res_intervals, intervset_out, chromstats, iu);
						interval = last_interval;
					} else
						interval.end = last_interval.end;
				} else if (interval.start != -1) { // result can be false or NA (in case that one of the arguments is NA or NaN)
					gscreen_add_interval2res(interval, res_intervals, intervset_out, chromstats, iu);
					interval.start = -1;
				}
			}

			if (interval.start != -1)
				gscreen_add_interval2res(interval, res_intervals, intervset_out, chromstats, iu);

			if (intervset_out.empty())
				return iu.convert_intervs(&res_intervals);

			GIntervalsBigSet1D::save_chrom_plain_intervals(intervset_out.c_str(), res_intervals, iu, chromstats);
			GIntervalsBigSet1D::end_save_plain_intervals(intervset_out.c_str(), iu, chromstats);
		} else {
			GIntervals2D res_intervals;
			vector<GIntervalsBigSet2D::ChromStat> chromstats;

			if (!intervset_out.empty())
				GIntervalsBigSet2D::begin_save(intervset_out.c_str(), iu, chromstats);

			for (; !scanner.isend(); scanner.next()) {
				if (scanner.last_logical(0) == 1)
					gscreen_add_interval2res(scanner.last_interval2d(), res_intervals, intervset_out, chromstats, iu);
			}

			if (intervset_out.empty())
				return iu.convert_intervs(&res_intervals);

			GIntervalsBigSet2D::save_chrom_plain_intervals(intervset_out.c_str(), res_intervals, iu, chromstats);
			GIntervalsBigSet2D::end_save_plain_intervals(intervset_out.c_str(), iu, chromstats);
		}
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP gscreen_multitask(SEXP _expr, SEXP _intervals, SEXP _iterator_policy, SEXP _band, SEXP _intervals_set_out, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_expr) || length(_expr) != 1)
			verror("Expression argument is not a string");

		if (!isNull(_intervals_set_out) && (!isString(_intervals_set_out) || length(_intervals_set_out) != 1))
			verror("intervals.set.out argument is not a string");

		string intervset_out = isNull(_intervals_set_out) ? "" : CHAR(STRING_ELT(_intervals_set_out, 0));
		IntervUtils iu(_envir);
		GIntervalsFetcher1D *intervals1d = NULL;
		GIntervalsFetcher2D *intervals2d = NULL;
		iu.convert_rintervs(_intervals, &intervals1d, &intervals2d);
		unique_ptr<GIntervalsFetcher1D> intervals1d_guard(intervals1d);
		unique_ptr<GIntervalsFetcher2D> intervals2d_guard(intervals2d);
		intervals1d->sort();
		intervals1d->unify_overlaps();
		intervals2d->sort();
		intervals2d->verify_no_overlaps(iu.get_chromkey());

		if (!iu.prepare4multitasking(_expr, intervals1d, intervals2d, _iterator_policy, _band))
			rreturn(R_NilValue);

		GIntervals res_intervals1d;
		GIntervals2D res_intervals2d;
		bool is_1d_iterator = iu.is_1d_iterator(_expr, intervals1d, intervals2d, _iterator_policy);
		vector<GIntervalsBigSet1D::ChromStat> chromstats1d;
		vector<GIntervalsBigSet2D::ChromStat> chromstats2d;

		if (!intervset_out.empty()) {
			if (is_1d_iterator) 
				GIntervalsBigSet1D::begin_save(intervset_out.c_str(), iu, chromstats1d);
			else
				GIntervalsBigSet2D::begin_save(intervset_out.c_str(), iu, chromstats2d);
		}

		if ((intervset_out.empty() && iu.distribute_task(0, (is_1d_iterator ? sizeof(GInterval) : sizeof(GInterval2D)))) ||
			(!intervset_out.empty() && iu.distribute_task(is_1d_iterator ?
														 sizeof(GIntervalsBigSet1D::ChromStat) * chromstats1d.size() :
														 sizeof(GIntervalsBigSet2D::ChromStat) * chromstats2d.size(), 0)) )
		{ // child process
			TrackExprScanner scanner(iu);

			scanner.begin(_expr, iu.get_kid_intervals1d(), iu.get_kid_intervals2d(), _iterator_policy, _band);

			if (scanner.get_iterator()->is_1d()) {
				GInterval interval(-1, -1, -1, -1);

				for (; !scanner.isend(); scanner.next()) {
					if (scanner.last_logical(0) == 1) {
						const GInterval &last_interval = scanner.last_interval1d();

						if (interval.end != last_interval.start || interval.chromid != last_interval.chromid) {
							if (interval.start != -1)
								gscreen_add_interval2res(interval, res_intervals1d, intervset_out, chromstats1d, iu);
							interval = last_interval;
						} else
							interval.end = last_interval.end;
					} else if (interval.start != -1) { // result can be false or NA (in case that one of the arguments is NA or NaN)
						gscreen_add_interval2res(interval, res_intervals1d, intervset_out, chromstats1d, iu);
						interval.start = -1;
					}
				}

				if (interval.start != -1)
					gscreen_add_interval2res(interval, res_intervals1d, intervset_out, chromstats1d, iu);

				// pack the result into shared memory
				if (intervset_out.empty()) {
					uint64_t num_intervals = res_intervals1d.size();
					void *result = allocate_res(num_intervals);

					if (num_intervals)
						pack_data(result, res_intervals1d.front(), num_intervals);
				} else {
					if (res_intervals1d.size())
						GIntervalsBigSet1D::save_chrom_plain_intervals(intervset_out.c_str(), res_intervals1d, iu, chromstats1d);

					void *ptr = allocate_res(0);
					pack_data(ptr, chromstats1d.front(), chromstats1d.size());
				}
			} else {
				for (; !scanner.isend(); scanner.next()) {
					if (scanner.last_logical(0) == 1)
						gscreen_add_interval2res(scanner.last_interval2d(), res_intervals2d, intervset_out, chromstats2d, iu);
				}

				// pack the result into shared memory
				if (intervset_out.empty()) {
					uint64_t num_intervals = res_intervals2d.size();
					void *result = allocate_res(num_intervals);

					if (num_intervals)
						pack_data(result, res_intervals2d.front(), num_intervals);
				} else {
					if (res_intervals2d.size())
						GIntervalsBigSet2D::save_chrom_plain_intervals(intervset_out.c_str(), res_intervals2d, iu, chromstats2d);

					void *ptr = allocate_res(0);
					pack_data(ptr, chromstats2d.front(), chromstats2d.size());
				}
			}
		} else { // parent process
			if (intervset_out.empty()) {
				GIntervals out_intervals1d;
				GIntervals2D out_intervals2d;

				// collect results from kids
				for (int i = 0; i < get_num_kids(); ++i) {
					void *ptr = get_kid_res(i);
					uint64_t num_intervals = get_kid_res_size(i);

					if (!num_intervals)
						continue;

					if (is_1d_iterator) {
						out_intervals1d.insert(out_intervals1d.end(), (GInterval *)ptr, (GInterval *)ptr + num_intervals);
						ptr = (GInterval *)ptr + num_intervals;
					} else {
						out_intervals2d.insert(out_intervals2d.end(), (GInterval2D *)ptr, (GInterval2D *)ptr + num_intervals);
						ptr = (GInterval2D *)ptr + num_intervals;
					}
				}

				if (!out_intervals1d.empty()) 
					rreturn(iu.convert_intervs(&out_intervals1d));

				if (!out_intervals2d.empty()) 
					rreturn(iu.convert_intervs(&out_intervals2d));
			} else {
				vector<GIntervalsBigSet1D::ChromStat> kid_chromstats1d(chromstats1d.size());
				vector<GIntervalsBigSet2D::ChromStat> kid_chromstats2d(chromstats2d.size());

				// collect results from kids
				for (int i = 0; i < get_num_kids(); ++i) {
					void *ptr = get_kid_res(i);

					if (is_1d_iterator) {
						unpack_data(ptr, kid_chromstats1d.front(), kid_chromstats1d.size());
						for (vector<GIntervalsBigSet1D::ChromStat>::const_iterator istat = kid_chromstats1d.begin(); istat < kid_chromstats1d.end(); ++istat) {
							if (istat->size)
								chromstats1d[istat - kid_chromstats1d.begin()] = *istat;
						}
					} else {
						unpack_data(ptr, kid_chromstats2d.front(), kid_chromstats2d.size());
						for (vector<GIntervalsBigSet2D::ChromStat>::const_iterator istat = kid_chromstats2d.begin(); istat < kid_chromstats2d.end(); ++istat) {
							if (istat->size)
								chromstats2d[istat - kid_chromstats2d.begin()] = *istat;
						}
					}
				}

				// finish saving (write meta)
				if (is_1d_iterator)
					GIntervalsBigSet1D::end_save_plain_intervals(intervset_out.c_str(), iu, chromstats1d);
				else
					GIntervalsBigSet2D::end_save_plain_intervals(intervset_out.c_str(), iu, chromstats2d);
			}
		}
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	rreturn(R_NilValue);
}

}
