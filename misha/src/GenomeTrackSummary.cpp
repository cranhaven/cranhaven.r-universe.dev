/*
 * GenomeTrackSummary.cpp
 *
 *  Created on: Jul 25, 2010
 *      Author: hoichman
 */

#include <cstdint>
#include <cmath>

#include "rdbinterval.h"
#include "rdbutils.h"
#include "BinsManager.h"
#include "GenomeTrack.h"
#include "GIntervalsBigSet1D.h"
#include "GIntervalsBigSet2D.h"
#include "TrackExpressionScanner.h"

using namespace std;
using namespace rdb;

struct IntervalSummary {
	double num_bins;
	double num_non_nan_bins;
	double total;
	double minval;
	double maxval;
	double mean_square_sum;

	double get_mean() const { return total / num_non_nan_bins; }

	double get_stdev() const {
		double mean = get_mean();
		// we are calaculating unbiased standard deviation:
		// sqrt(sum((x-mean)^2) / (N-1)) = sqrt(sum(x^2)/(N-1) - N*(mean^2)/(N-1))
		return sqrt(mean_square_sum / (num_non_nan_bins - 1) - (mean * mean) * (num_non_nan_bins / (num_non_nan_bins - 1)));
	}

	void update(double v) {
		++num_bins;
		if (!std::isnan(v)) {
			num_non_nan_bins++;
			total += v;
			minval = min(minval, v);
			maxval = max(maxval, v);
			mean_square_sum += v * v;
		}
	}

	void merge(const IntervalSummary &obj) {
		num_bins += obj.num_bins;
		num_non_nan_bins += obj.num_non_nan_bins;
		total += obj.total;
		minval = min(minval, obj.minval);
		maxval = max(maxval, obj.maxval);
		mean_square_sum += obj.mean_square_sum;
	}

	IntervalSummary() :
		num_bins(0),
		num_non_nan_bins(0),
		total(0),
		minval(numeric_limits<double>::max()),
		maxval(-numeric_limits<double>::max()),
		mean_square_sum(0) {}
};

enum IntervalSummaryCols { TOTAL_BINS, TOTAL_NAN_BINS, MIN, MAX, SUM, MEAN, STDEV, NUM_COLS };

static const char *IntervalSummaryColNames[NUM_COLS] = { "Total intervals", "NaN intervals", "Min", "Max", "Sum", "Mean", "Std dev" };

static SEXP build_rintervals_summary(GIntervalsFetcher1D *intervals1d, GIntervalsFetcher2D *intervals2d,
									 const vector<IntervalSummary> &summaries, IntervUtils &iu, bool use_original_index)
{
	SEXP answer, rsummary[NUM_COLS], colnames;
	unsigned num_interv_cols;
	uint64_t num_intervs;

	if (intervals1d) {
		num_interv_cols = GInterval::NUM_COLS;
		answer = iu.convert_intervs(intervals1d, num_interv_cols + NUM_COLS, false, use_original_index);
		num_intervs = intervals1d->size();
	} else {
		num_interv_cols = GInterval2D::NUM_COLS;
		answer = iu.convert_intervs(intervals2d, num_interv_cols + NUM_COLS, false, use_original_index);
		num_intervs = intervals2d->size();
	}

	colnames = getAttrib(answer, R_NamesSymbol);

	for (unsigned icol = 0; icol < NUM_COLS; ++icol)
		rprotect(rsummary[icol] = RSaneAllocVector(REALSXP, num_intervs));

	for (unsigned i = 0; i < num_intervs; i++) {
		REAL(rsummary[TOTAL_BINS])[i] = summaries[i].num_bins;
		REAL(rsummary[TOTAL_NAN_BINS])[i] = summaries[i].num_bins - summaries[i].num_non_nan_bins;
		REAL(rsummary[MIN])[i] = summaries[i].num_non_nan_bins ? summaries[i].minval : numeric_limits<double>::quiet_NaN();
		REAL(rsummary[MAX])[i] = summaries[i].num_non_nan_bins ? summaries[i].maxval : numeric_limits<double>::quiet_NaN();
		REAL(rsummary[SUM])[i] = summaries[i].num_non_nan_bins ? summaries[i].total : numeric_limits<double>::quiet_NaN();
		REAL(rsummary[MEAN])[i] = summaries[i].num_non_nan_bins ? summaries[i].get_mean() : numeric_limits<double>::quiet_NaN();
		REAL(rsummary[STDEV])[i] = summaries[i].num_non_nan_bins > 1 ? summaries[i].get_stdev() : numeric_limits<double>::quiet_NaN();
	}

    for (unsigned icol = 0; icol < NUM_COLS; ++icol) {
        SET_VECTOR_ELT(answer, num_interv_cols + icol, rsummary[icol]);
        SET_STRING_ELT(colnames, num_interv_cols + icol, mkChar(IntervalSummaryColNames[icol]));
    }

	return answer;
}

extern "C" {

SEXP gtracksummary(SEXP _expr, SEXP _intervals, SEXP _iterator_policy, SEXP _band, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		// check the arguments
		if (!isString(_expr) || length(_expr) != 1)
			verror("Track expression argument is not a string");

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

		IntervalSummary summary;
		TrackExprScanner scanner(iu);

		for (scanner.begin(_expr, intervals1d, intervals2d, _iterator_policy, _band); !scanner.isend(); scanner.next())
			summary.update(scanner.last_real(0));

		SEXP answer;
		SEXP colnames;

		rprotect(answer = RSaneAllocVector(REALSXP, NUM_COLS));
		rprotect(colnames = RSaneAllocVector(STRSXP, NUM_COLS));

		REAL(answer)[TOTAL_BINS] = summary.num_bins;
		REAL(answer)[TOTAL_NAN_BINS] = summary.num_bins - summary.num_non_nan_bins;
		REAL(answer)[MIN] = summary.num_non_nan_bins ? summary.minval : numeric_limits<double>::quiet_NaN();
		REAL(answer)[MAX] = summary.num_non_nan_bins ? summary.maxval : numeric_limits<double>::quiet_NaN();
		REAL(answer)[SUM] = summary.num_non_nan_bins ? summary.total : numeric_limits<double>::quiet_NaN();
		REAL(answer)[MEAN] = summary.num_non_nan_bins ? summary.get_mean() : numeric_limits<double>::quiet_NaN();
		REAL(answer)[STDEV] = summary.num_non_nan_bins > 1 ? summary.get_stdev() : numeric_limits<double>::quiet_NaN();

		for (int i = 0; i < NUM_COLS; i++)
			SET_STRING_ELT(colnames, i, mkChar(IntervalSummaryColNames[i]));

		setAttrib(answer, R_NamesSymbol, colnames);

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP gtracksummary_multitask(SEXP _expr, SEXP _intervals, SEXP _iterator_policy, SEXP _band, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		// check the arguments
		if (!isString(_expr) || length(_expr) != 1)
			verror("Track expression argument is not a string");

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

		IntervalSummary summary;

		if (!iu.prepare4multitasking(_expr, intervals1d, intervals2d, _iterator_policy, _band))
			rreturn(R_NilValue);

		if (iu.distribute_task(sizeof(summary), 0)) { // child process
			TrackExprScanner scanner(iu);

			for (scanner.begin(_expr, iu.get_kid_intervals1d(), iu.get_kid_intervals2d(), _iterator_policy, _band); !scanner.isend(); scanner.next())
				summary.update(scanner.last_real(0));

			void *result = allocate_res(0);
			pack_data(result, summary, 1);
		} else { // parent process
			// collect results from kids
			for (int i = 0; i < get_num_kids(); ++i) {
				void *ptr = get_kid_res(i);
				IntervalSummary kid_summary;

				unpack_data(ptr, kid_summary, 1);
				summary.merge(kid_summary);
			}

			SEXP answer;
			SEXP colnames;

			rprotect(answer = RSaneAllocVector(REALSXP, NUM_COLS));
			rprotect(colnames = RSaneAllocVector(STRSXP, NUM_COLS));

			REAL(answer)[TOTAL_BINS] = summary.num_bins;
			REAL(answer)[TOTAL_NAN_BINS] = summary.num_bins - summary.num_non_nan_bins;
			REAL(answer)[MIN] = summary.num_non_nan_bins ? summary.minval : numeric_limits<double>::quiet_NaN();
			REAL(answer)[MAX] = summary.num_non_nan_bins ? summary.maxval : numeric_limits<double>::quiet_NaN();
			REAL(answer)[SUM] = summary.num_non_nan_bins ? summary.total : numeric_limits<double>::quiet_NaN();
			REAL(answer)[MEAN] = summary.num_non_nan_bins ? summary.get_mean() : numeric_limits<double>::quiet_NaN();
			REAL(answer)[STDEV] = summary.num_non_nan_bins > 1 ? summary.get_stdev() : numeric_limits<double>::quiet_NaN();

			for (int i = 0; i < NUM_COLS; i++)
				SET_STRING_ELT(colnames, i, mkChar(IntervalSummaryColNames[i]));

			setAttrib(answer, R_NamesSymbol, colnames);

			rreturn(answer);
		}
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	rreturn(R_NilValue);
}

SEXP gintervals_summary(SEXP _expr, SEXP _intervals, SEXP _iterator_policy, SEXP _band, SEXP _intervals_set_out, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_expr) || length(_expr) != 1)
			verror("Track argument is not a string");

		if (!isNull(_intervals_set_out) && (!isString(_intervals_set_out) || length(_intervals_set_out) != 1))
			verror("intervals.set.out argument is not a string");

		IntervUtils iu(_envir);
		TrackExprScanner scanner(iu);
		GIntervalsFetcher1D *intervals1d = NULL;
		GIntervalsFetcher2D *intervals2d = NULL;
		iu.convert_rintervs(_intervals, &intervals1d, &intervals2d);
		unique_ptr<GIntervalsFetcher1D> intervals1d_guard(intervals1d);
		unique_ptr<GIntervalsFetcher2D> intervals2d_guard(intervals2d);
		intervals1d->sort();
		intervals2d->sort();
		intervals2d->verify_no_overlaps(iu.get_chromkey());

		scanner.begin(_expr, intervals1d, intervals2d, _iterator_policy, _band);

		uint64_t num_intervals = scanner.get_iterator()->is_1d() ? intervals1d->size() : intervals2d->size();
		vector<IntervalSummary> summaries;

		if (!num_intervals) 
			return R_NilValue;

		bool do_small_intervset_out = !isNull(_intervals_set_out) && !iu.needs_bigset(num_intervals);
		bool do_big_intervset_out = !isNull(_intervals_set_out) && !do_small_intervset_out;
		string intervset_out = isNull(_intervals_set_out) ? "" : CHAR(STRING_ELT(_intervals_set_out, 0));
		SEXP answer = R_NilValue;

		if (do_big_intervset_out) {
			vector<GIntervalsBigSet1D::ChromStat> chromstats1d;
			vector<GIntervalsBigSet2D::ChromStat> chromstats2d;
			GInterval last_scope_interval1d;
			GInterval2D last_scope_interval2d;
			int64_t cur_interval_idx = -1;
			int64_t cur_interval_chrom_idx = -1;
			set<int> chroms1d;
			set<ChromPair> chroms2d;

			if (scanner.get_iterator()->is_1d()) {
				GIntervalsBigSet1D::begin_save(intervset_out.c_str(), iu, chromstats1d);
				for (int chromid = 0; (uint64_t)chromid < iu.get_chromkey().get_num_chroms(); ++chromid) {
					if (intervals1d->size(chromid))
						chroms1d.insert(chromid);
				}
			} else {
				GIntervalsBigSet2D::begin_save(intervset_out.c_str(), iu, chromstats2d);
				for (int chromid1 = 0; (uint64_t)chromid1 < iu.get_chromkey().get_num_chroms(); ++chromid1) {
					for (int chromid2 = 0; (uint64_t)chromid2 < iu.get_chromkey().get_num_chroms(); ++chromid2) {
						if (intervals2d->size(chromid1, chromid2))
							chroms2d.insert(ChromPair(chromid1, chromid2));
					}
				}
			}

			while (!scanner.isend()) {
				float val = scanner.last_real(0);

				if (cur_interval_idx != scanner.last_scope_idx()) {
					uint64_t size;
					char error_prefix[1000];

					cur_interval_idx = scanner.last_scope_idx();
					cur_interval_chrom_idx = scanner.last_scope_chrom_idx();

					if (scanner.get_iterator()->is_1d()) {
						last_scope_interval1d = scanner.last_scope_interval1d();
						size = intervals1d->size(last_scope_interval1d.chromid);
						snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chrom %s",
								intervset_out.c_str(), iu.id2chrom(last_scope_interval1d.chromid).c_str());
					} else {
						last_scope_interval2d = scanner.last_scope_interval2d();
						size = intervals2d->size(last_scope_interval2d.chromid1(), last_scope_interval2d.chromid2());
						snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chroms (%s, %s)",
								intervset_out.c_str(), iu.id2chrom(last_scope_interval2d.chromid1()).c_str(), iu.id2chrom(last_scope_interval2d.chromid2()).c_str());
					}

					iu.verify_max_data_size(size, error_prefix, false);
					summaries.resize(size);
				}

				summaries[cur_interval_chrom_idx].update(val);

				scanner.next();

				// interval has finished => calculate the percentile
				if (scanner.get_iterator()->is_1d()) {
					if (scanner.isend() || last_scope_interval1d.chromid != scanner.last_scope_interval1d().chromid) {
						unique_ptr<GIntervalsFetcher1D> out_intervals(intervals1d->create_masked_copy(last_scope_interval1d.chromid));

						SEXP rintervals = build_rintervals_summary(out_intervals.get(), NULL, summaries, iu, false);
						GIntervalsBigSet1D::save_chrom(intervset_out.c_str(), out_intervals.get(), rintervals, iu, chromstats1d);
						chroms1d.erase(last_scope_interval1d.chromid);
						summaries.clear();
					}
				} else {
					if (scanner.isend() || !last_scope_interval2d.is_same_chrom(scanner.last_scope_interval2d())) {
						unique_ptr<GIntervalsFetcher2D> out_intervals(
							intervals2d->create_masked_copy(last_scope_interval2d.chromid1(), last_scope_interval2d.chromid2()));

						SEXP rintervals = build_rintervals_summary(NULL, out_intervals.get(), summaries, iu, false);
						GIntervalsBigSet2D::save_chrom(intervset_out.c_str(), out_intervals.get(), rintervals, iu, chromstats2d);
						chroms2d.erase(ChromPair(last_scope_interval2d.chromid1(), last_scope_interval2d.chromid2()));
						summaries.clear();
					}
				}
			}

			// finish saving (save skipped scope chromosomes + write meta)
			if (scanner.get_iterator()->is_1d()) {
				for (set<int>::const_iterator ichromid = chroms1d.begin(); ichromid != chroms1d.end(); ++ichromid) {
					unique_ptr<GIntervalsFetcher1D> out_intervals(intervals1d->create_masked_copy(*ichromid));

					int size = intervals1d->size(*ichromid);
					iu.verify_max_data_size(size, "Result", false);
					summaries.resize(size);
					SEXP rintervals = build_rintervals_summary(out_intervals.get(), NULL, summaries, iu, false);
					GIntervalsBigSet1D::save_chrom(intervset_out.c_str(), out_intervals.get(), rintervals, iu, chromstats1d);
					summaries.clear();
				}

				GIntervals out_intervals;
				SEXP zeroline = build_rintervals_summary(&out_intervals, NULL, summaries, iu, false);
				GIntervalsBigSet1D::end_save(intervset_out.c_str(), zeroline, iu, chromstats1d);
			} else {
				for (set<ChromPair>::const_iterator ichrompair = chroms2d.begin(); ichrompair != chroms2d.end(); ++ichrompair)  {
					unique_ptr<GIntervalsFetcher2D> out_intervals(intervals2d->create_masked_copy(ichrompair->chromid1, ichrompair->chromid2));

					int size = intervals2d->size(ichrompair->chromid1, ichrompair->chromid2);
					iu.verify_max_data_size(size, "Result", false);
					summaries.resize(size);
					SEXP rintervals = build_rintervals_summary(NULL, out_intervals.get(), summaries, iu, false);
					GIntervalsBigSet2D::save_chrom(intervset_out.c_str(), out_intervals.get(), rintervals, iu, chromstats2d);
					summaries.clear();
				}

				GIntervals2D out_intervals;
				SEXP zeroline = build_rintervals_summary(NULL, &out_intervals, summaries, iu, false);
				GIntervalsBigSet2D::end_save(intervset_out.c_str(), zeroline, iu, chromstats2d);
			}
		} else {
			iu.verify_max_data_size(num_intervals, "Result");
			summaries.resize(num_intervals);

			for (; !scanner.isend(); scanner.next()) {
				double v = scanner.last_real(0);
				IntervalSummary &summary = summaries[
					scanner.get_iterator()->is_1d() ?
						iu.get_orig_interv_idx(scanner.last_scope_interval1d()) :
						iu.get_orig_interv_idx(scanner.last_scope_interval2d())];

				summary.update(v);
			}

			// assemble the answer
			answer = scanner.get_iterator()->is_1d() ?
				build_rintervals_summary(intervals1d, NULL, summaries, iu, true) :
				build_rintervals_summary(NULL, intervals2d, summaries, iu, true);

			if (do_small_intervset_out) {
				string filename = interv2path(iu.get_env(), intervset_out);
				RSaneSerialize(answer, filename.c_str());
				answer = R_NilValue;
			}
		}

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP gbins_summary(SEXP _track_exprs, SEXP _breaks, SEXP _include_lowest, SEXP _intervals, SEXP _iterator_policy, SEXP _band, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_track_exprs) || length(_track_exprs) < 1)
			verror("Track argument is not a string vector");

		unsigned numexpr = length(_track_exprs);
		BinsManager bins_manager(_breaks, _include_lowest);

		if (bins_manager.get_num_bin_finders() != numexpr - 1)
			verror("Number of breaks sets must be equal to the number of tracks used");

		IntervUtils iu(_envir);
		TrackExprScanner scanner(iu);
		unsigned totalbins = bins_manager.get_total_bins();
		iu.verify_max_data_size(totalbins, "Result");
		vector<IntervalSummary> summaries(totalbins);
		vector<double> vals(bins_manager.get_num_bin_finders());
		GIntervalsFetcher1D *intervals1d = NULL;
		GIntervalsFetcher2D *intervals2d = NULL;
		iu.convert_rintervs(_intervals, &intervals1d, &intervals2d);
		unique_ptr<GIntervalsFetcher1D> intervals1d_guard(intervals1d);
		unique_ptr<GIntervalsFetcher2D> intervals2d_guard(intervals2d);
		intervals1d->sort();
		intervals1d->unify_overlaps();
		intervals2d->sort();
		intervals2d->verify_no_overlaps(iu.get_chromkey());

		for (scanner.begin(_track_exprs, intervals1d, intervals2d, _iterator_policy, _band); !scanner.isend(); scanner.next()) {
			float val = scanner.last_real(0);
			for (unsigned i = 1; i < numexpr; ++i)
				vals[i - 1] = scanner.last_real(i);

			int index = bins_manager.vals2idx(vals);

			if (index >= 0)
				summaries[index].update(val);
		}

		// pack the answer
		SEXP answer, dim, dimnames;
		rprotect(answer = RSaneAllocVector(REALSXP, totalbins * NUM_COLS));

		for (unsigned i = 0; i < totalbins; i++) {
			REAL(answer)[totalbins * TOTAL_NAN_BINS + i] = summaries[i].num_bins - summaries[i].num_non_nan_bins;
			REAL(answer)[totalbins * TOTAL_BINS + i] = summaries[i].num_bins;
			REAL(answer)[totalbins * MIN + i] = summaries[i].num_non_nan_bins ? summaries[i].minval : numeric_limits<double>::quiet_NaN();
			REAL(answer)[totalbins * MAX + i] = summaries[i].num_non_nan_bins ? summaries[i].maxval : numeric_limits<double>::quiet_NaN();
			REAL(answer)[totalbins * SUM + i] = summaries[i].num_non_nan_bins ? summaries[i].total : numeric_limits<double>::quiet_NaN();
			REAL(answer)[totalbins * MEAN + i] = summaries[i].num_non_nan_bins ? summaries[i].get_mean() : numeric_limits<double>::quiet_NaN();
			REAL(answer)[totalbins * STDEV + i] = summaries[i].num_non_nan_bins > 1 ? summaries[i].get_stdev() : numeric_limits<double>::quiet_NaN();
		}

		rprotect(dim = RSaneAllocVector(INTSXP, bins_manager.get_num_bin_finders() + 1));
		rprotect(dimnames = RSaneAllocVector(VECSXP, bins_manager.get_num_bin_finders() + 1));
		bins_manager.set_dims(dim, dimnames);

		SEXP dimname;
		rprotect(dimname = RSaneAllocVector(STRSXP, NUM_COLS));
		for (unsigned i = 0; i < NUM_COLS; i++)
			SET_STRING_ELT(dimname, i, mkChar(IntervalSummaryColNames[i]));

		SET_VECTOR_ELT(dimnames, bins_manager.get_num_bin_finders(), dimname);
		INTEGER(dim)[bins_manager.get_num_bin_finders()] = NUM_COLS;

		setAttrib(answer, R_DimSymbol, dim);
		setAttrib(answer, R_DimNamesSymbol, dimnames);
		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

}
