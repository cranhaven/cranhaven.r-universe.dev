/*
 * GenomeTrackMedian.cpp
 *
 *  Created on: Jun 9, 2010
 *      Author: hoichman
 */

#include <cstdint>
#include <algorithm>
#include <cmath>
#include <vector>

#include "rdbinterval.h"
#include "rdbutils.h"
#include "BinsManager.h"
#include "GenomeTrack.h"
#include "GIntervalsBigSet1D.h"
#include "GIntervalsBigSet2D.h"
#include "StreamSampler.h"
#include "StreamPercentiler.h"
#include "TrackExpressionCartesianGridIterator.h"
#include "TrackExpressionIntervals2DIterator.h"
#include "TrackExpressionTrackRectsIterator.h"
#include "TrackExpressionScanner.h"

using namespace std;
using namespace rdb;

struct Percentile {
	double    percentile;
	int64_t   index;
	bool      estimation;

	Percentile() {}
	Percentile(double _percentile, int64_t _index) : percentile(_percentile), index(_index) {}

	bool operator<(const Percentile &p) const { return percentile < p.percentile; }
};

static SEXP build_rintervals_quantiles(GIntervalsFetcher1D *out_intervals1d, GIntervalsFetcher2D *out_intervals2d,
									   const vector<Percentile> &percentiles, const vector<double> &quantiles,
									   IntervUtils &iu, bool use_original_index)
{
	SEXP answer;
	unsigned num_interv_cols;
	uint64_t num_intervs;

	if (out_intervals1d) {
		num_interv_cols = GInterval::NUM_COLS;
		answer = iu.convert_intervs(out_intervals1d, num_interv_cols + percentiles.size(), false, use_original_index);
		num_intervs = out_intervals1d->size();
	} else {
		num_interv_cols = GInterval2D::NUM_COLS;
		answer = iu.convert_intervs(out_intervals2d, num_interv_cols + percentiles.size(), false, use_original_index);
		num_intervs = out_intervals2d->size();
	}

	for (unsigned ipercentile = 0; ipercentile < percentiles.size(); ++ipercentile) {
		SEXP percentile_vals;
		rprotect(percentile_vals = RSaneAllocVector(REALSXP, num_intervs));
		for (uint64_t iinterv = 0; iinterv < num_intervs; ++iinterv)
			REAL(percentile_vals)[iinterv] = quantiles[iinterv * percentiles.size() + ipercentile];
		SET_VECTOR_ELT(answer, num_interv_cols + ipercentile, percentile_vals);
	}

	SEXP colnames = getAttrib(answer, R_NamesSymbol);

	for (vector<Percentile>::const_iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip) {
		char buf[100];

		snprintf(buf, sizeof(buf), "%g", ip->percentile);
		SET_STRING_ELT(colnames, num_interv_cols + ip->index, mkChar(buf));
	}

	return answer;
}

bool calc_medians(StreamPercentiler<double> &sp, vector<Percentile> &percentiles, vector<double> &medians, uint64_t offset)
{
	bool estimated_results = false;

	offset *= percentiles.size();
	if (sp.stream_size()) {
		for (vector<Percentile>::iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip) {
			medians[offset + ip->index] = sp.get_percentile(ip->percentile, ip->estimation);
			if (ip->estimation)
				estimated_results = true;
		}

		for (vector<Percentile>::iterator ip = percentiles.begin() + 1; ip != percentiles.end(); ++ip) {
			if (ip->estimation)
				medians[offset + ip->index] = max(medians[offset + ip->index], medians[offset + (ip - 1)->index]);
		}
		for (vector<Percentile>::reverse_iterator ip = percentiles.rbegin() + 1; ip != percentiles.rend(); ++ip) {
			if (ip->estimation)
				medians[offset + ip->index] = min(medians[offset + ip->index], medians[offset + (ip - 1)->index]);
		}
	} else {
		for (vector<Percentile>::iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip)
			medians[offset + ip->index] = numeric_limits<double>::quiet_NaN();
	}
	return estimated_results;
}

extern "C" {

SEXP C_gquantiles(SEXP _intervals, SEXP _expr, SEXP _percentiles, SEXP _iterator_policy, SEXP _band, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_expr) || length(_expr) != 1)
			verror("Track argument is not a string");

		if (!isReal(_percentiles) || length(_percentiles) < 1)
			verror("Percentile argument is not a vector of numbers");

		vector<Percentile> percentiles(length(_percentiles));
		for (int64_t i = 0; i < length(_percentiles); ++i)
			percentiles[i] = Percentile(REAL(_percentiles)[i], i);
		sort(percentiles.begin(), percentiles.end());

		for (vector<Percentile>::const_iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip) {
			if (ip->percentile < 0 || ip->percentile > 1)
				verror("Percentile (%g) is not in [0, 1] range\n", ip->percentile);
		}

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

		StreamPercentiler<double> sp(iu.get_max_data_size(), iu.get_quantile_edge_data_size(), iu.get_quantile_edge_data_size());

		for (scanner.begin(_expr, intervals1d, intervals2d, _iterator_policy, _band); !scanner.isend(); scanner.next()) {
			float val = scanner.last_real(0);

			if (!std::isnan(val))
				sp.add(val, unif_rand);
		}

		vector<double> medians(percentiles.size(), numeric_limits<float>::quiet_NaN());

		if (calc_medians(sp, percentiles, medians, 0)){
			warning("Data size (%llu) exceeds the limit (%llu).\n"
					"The data was sampled to fit the limit and the resulted quantiles are hence approximate.\n"
					"(The limit can be controlled by gmax.data.size limit)", (unsigned long long)sp.stream_size(), (unsigned long long)iu.get_max_data_size());
		}

		// assemble the answer
		SEXP answer;
		SEXP colnames;

		rprotect(answer = RSaneAllocVector(REALSXP, percentiles.size()));
		rprotect(colnames = RSaneAllocVector(STRSXP, percentiles.size()));

		for (vector<Percentile>::const_iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip) {
			char buf[100];

			REAL(answer)[ip->index] = medians[ip->index];

			snprintf(buf, sizeof(buf), "%g", ip->percentile);
			SET_STRING_ELT(colnames, ip->index, mkChar(buf));
		}

		setAttrib(answer, R_NamesSymbol, colnames);

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP gquantiles_multitask(SEXP _intervals, SEXP _expr, SEXP _percentiles, SEXP _iterator_policy, SEXP _band, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_expr) || length(_expr) != 1)
			verror("Track argument is not a string");

		if (!isReal(_percentiles) || length(_percentiles) < 1)
			verror("Percentile argument is not a vector of numbers");

		vector<Percentile> percentiles(length(_percentiles));
		for (int64_t i = 0; i < length(_percentiles); ++i)
			percentiles[i] = Percentile(REAL(_percentiles)[i], i);
		sort(percentiles.begin(), percentiles.end());

		for (vector<Percentile>::const_iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip) {
			if (ip->percentile < 0 || ip->percentile > 1)
				verror("Percentile (%g) is not in [0, 1] range\n", ip->percentile);
		}

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

		vector<double> medians(percentiles.size(), numeric_limits<float>::quiet_NaN());

		int num_kids = iu.prepare4multitasking(_expr, intervals1d, intervals2d, _iterator_policy, _band);

		if (num_kids) {
			uint64_t kid_rnd_sampling_buf_size = (uint64_t)ceil(iu.get_max_data_size() / (double)num_kids);
			uint64_t kid_lowest_vals_buf_size = iu.get_quantile_edge_data_size();
			uint64_t kid_highest_vals_buf_size = iu.get_quantile_edge_data_size();

			if (iu.distribute_task(4 * sizeof(uint64_t) +
								   (kid_rnd_sampling_buf_size + kid_lowest_vals_buf_size + kid_highest_vals_buf_size) * sizeof(double),
								   0))
			{ // child process
				StreamPercentiler<double> sp(kid_rnd_sampling_buf_size, kid_lowest_vals_buf_size, kid_highest_vals_buf_size);
				TrackExprScanner scanner(iu);

				for (scanner.begin(_expr, iu.get_kid_intervals1d(), iu.get_kid_intervals2d(), _iterator_policy, _band); !scanner.isend(); scanner.next()) {
					float val = scanner.last_real(0);

					if (!std::isnan(val))
						sp.add(val, unif_rand);
				}

				void *result = allocate_res(0);
				uint64_t kid_stream_size = sp.stream_size();
				uint64_t kid_samples_size = sp.samples().size();
				uint64_t kid_lowest_vals_size = sp.lowest_vals().size();
				uint64_t kid_highest_vals_size = sp.highest_vals().size();
				pack_data(result, kid_stream_size, 1);
				pack_data(result, kid_samples_size, 1);
				pack_data(result, kid_lowest_vals_size, 1);
				pack_data(result, kid_highest_vals_size, 1);
				pack_data(result, sp.samples().front(), kid_samples_size);
				pack_data(result, sp.lowest_vals().front(), kid_lowest_vals_size);
				pack_data(result, sp.highest_vals().front(), kid_highest_vals_size);

				rreturn(R_NilValue);
			} else { // parent process
				uint64_t stream_size = 0;
				vector<uint64_t> kids_stream_size(get_num_kids());
				vector<uint64_t> kids_samples_size(get_num_kids());
				vector<uint64_t> kids_lowest_vals_size(get_num_kids());
				vector<uint64_t> kids_highest_vals_size(get_num_kids());
				vector<double *> kids_vals(get_num_kids());
				vector<double> samples;
				vector<double> highest_vals;
				vector<double> lowest_vals;
				double min_sampling_rate = 1;
				bool was_lowest_vals_buf_used = false;
				bool was_highest_vals_buf_used = 0;

				// collect results from kids
				for (int i = 0; i < get_num_kids(); ++i) {
					void *result = get_kid_res(i);
					unpack_data(result, kids_stream_size[i], 1);
					unpack_data(result, kids_samples_size[i], 1);
					unpack_data(result, kids_lowest_vals_size[i], 1);
					unpack_data(result, kids_highest_vals_size[i], 1);
					kids_vals[i] = (double *)result;

					was_lowest_vals_buf_used |= kids_lowest_vals_size[i] > 0;
					was_highest_vals_buf_used |= kids_highest_vals_size[i] > 0;

					if (kids_stream_size[i]) {
						min_sampling_rate = min(min_sampling_rate, kids_samples_size[i] / (double)kids_stream_size[i]);
						stream_size += kids_stream_size[i];
					}
				}

				for (int i = 0; i < get_num_kids(); ++i) {
					if (!kids_stream_size[i]) 
						continue;

					double *kid_samples = kids_vals[i];
					double *kid_lowest_vals = kid_samples + kids_samples_size[i];
					double *kid_highest_vals = kid_lowest_vals + kids_lowest_vals_size[i];
					double kid_sampling_rate = kids_samples_size[i] / (double)kids_stream_size[i];

					if (kid_sampling_rate == min_sampling_rate)
						samples.insert(samples.end(), kid_samples, kid_samples + kids_samples_size[i]);
					else {
						double sampling_ratio = min_sampling_rate / kid_sampling_rate;
						for (uint64_t j = 0; j < kids_samples_size[i]; ++j) {
							if (unif_rand() < sampling_ratio)
								samples.push_back(kid_samples[j]);
						}
					}

					if (min_sampling_rate < 1) {
						if (was_lowest_vals_buf_used) {
							if (kids_lowest_vals_size[i]) 
								lowest_vals.insert(lowest_vals.end(), kid_lowest_vals, kid_lowest_vals + kids_lowest_vals_size[i]);
							else {
								if (kids_samples_size[i] <= kid_lowest_vals_buf_size) 
									lowest_vals.insert(lowest_vals.end(), kid_samples, kid_samples + kids_samples_size[i]);
								else {
									partial_sort(kid_samples, kid_samples + kid_lowest_vals_buf_size, kid_samples + kids_samples_size[i], less<double>());
									lowest_vals.insert(lowest_vals.end(), kid_samples, kid_samples + kid_lowest_vals_buf_size);
								}
							}
						}

						if (was_highest_vals_buf_used) {
							if (kids_highest_vals_size[i]) 
								highest_vals.insert(highest_vals.end(), kid_highest_vals, kid_highest_vals + kids_highest_vals_size[i]);
							else {
								if (kids_samples_size[i] <= kid_highest_vals_buf_size) 
									highest_vals.insert(highest_vals.end(), kid_samples, kid_samples + kids_samples_size[i]);
								else {
									partial_sort(kid_samples, kid_samples + kid_highest_vals_buf_size, kid_samples + kids_samples_size[i], greater<double>());
									highest_vals.insert(highest_vals.end(), kid_samples, kid_samples + kid_highest_vals_buf_size);
								}
							}
						}
					}
				}

				// only kid_lowest_vals_buf_size of all lowest values are really the lowest among all the samples
				if (was_lowest_vals_buf_used) {
					partial_sort(lowest_vals.begin(), lowest_vals.begin() + kid_lowest_vals_buf_size, lowest_vals.end(), less<double>());
					lowest_vals.resize(kid_lowest_vals_buf_size);
				}

				if (was_highest_vals_buf_used) {
					partial_sort(highest_vals.begin(), highest_vals.begin() + kid_highest_vals_buf_size, highest_vals.end(), greater<double>());
					highest_vals.resize(kid_lowest_vals_buf_size);
				}

				StreamPercentiler<double> sp;
				sp.init_with_swap(stream_size, samples, lowest_vals, highest_vals);

				// calculate the percentiles
				if (calc_medians(sp, percentiles, medians, 0))
					warning("The data was sampled to fit the limit and the resulted quantiles are hence approximate.\n"
							"(The limit can be controlled by gmax.data.size limit)");
			}
		}

		// assemble the answer
		SEXP answer;
		SEXP colnames;

		rprotect(answer = RSaneAllocVector(REALSXP, percentiles.size()));
		rprotect(colnames = RSaneAllocVector(STRSXP, percentiles.size()));

		for (vector<Percentile>::const_iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip) {
			char buf[100];

			REAL(answer)[ip->index] = medians[ip->index];

			snprintf(buf, sizeof(buf), "%g", ip->percentile);
			SET_STRING_ELT(colnames, ip->index, mkChar(buf));
		}

		setAttrib(answer, R_NamesSymbol, colnames);

		rreturn(answer);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	rreturn(R_NilValue);
}

SEXP gintervals_quantiles(SEXP _intervals, SEXP _expr, SEXP _percentiles, SEXP _iterator_policy, SEXP _band, SEXP _intervals_set_out, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_expr) || length(_expr) != 1)
			verror("Track argument is not a string");

		if (!isReal(_percentiles) || length(_percentiles) < 1)
			verror("Percentile argument is not a vector of numbers");

		if (!isNull(_intervals_set_out) && (!isString(_intervals_set_out) || length(_intervals_set_out) != 1))
			verror("intervals.set.out argument is not a string");

		vector<Percentile> percentiles(length(_percentiles));
		uint64_t num_percentiles = percentiles.size();
		for (int64_t i = 0; i < length(_percentiles); ++i)
			percentiles[i] = Percentile(REAL(_percentiles)[i], i);
		sort(percentiles.begin(), percentiles.end());

		for (vector<Percentile>::const_iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip) {
			if (ip->percentile < 0 || ip->percentile > 1)
				verror("Percentile (%g) is not in [0, 1] range\n", ip->percentile);
		}

		IntervUtils iu(_envir);
		TrackExprScanner scanner(iu);
		int64_t cur_interval_idx = -1;

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
		if (!num_intervals) 
			return R_NilValue;

		bool do_small_intervset_out = !isNull(_intervals_set_out) && !iu.needs_bigset(num_intervals);
		bool do_big_intervset_out = !isNull(_intervals_set_out) && !do_small_intervset_out;
		string intervset_out = isNull(_intervals_set_out) ? "" : CHAR(STRING_ELT(_intervals_set_out, 0));

		if (dynamic_cast<const TrackExpressionCartesianGridIterator *>(scanner.get_iterator()) ||
			dynamic_cast<const TrackExpressionIntervals2DIterator *>(scanner.get_iterator()) ||
			dynamic_cast<const TrackExpressionTrackRectsIterator *>(scanner.get_iterator()))
			verror("The type of iterator is currently not supported by the function");

		vector<double> medians;
		StreamPercentiler<double> sp(iu.get_max_data_size(), iu.get_quantile_edge_data_size(), iu.get_quantile_edge_data_size());
		bool generate_warning = false;
		SEXP answer = R_NilValue;

		if (do_big_intervset_out) {
			vector<GIntervalsBigSet1D::ChromStat> chromstats1d;
			vector<GIntervalsBigSet2D::ChromStat> chromstats2d;
			GInterval last_scope_interval1d;
			GInterval2D last_scope_interval2d;
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
					medians.resize(size * num_percentiles, numeric_limits<double>::quiet_NaN());
					sp.reset();
				}

				scanner.next();

				if (!std::isnan(val))
					sp.add(val, unif_rand);

				// interval has finished => calculate the percentile
				if (cur_interval_idx != scanner.last_scope_idx() || scanner.isend()) {
					generate_warning = calc_medians(sp, percentiles, medians, cur_interval_chrom_idx) | generate_warning;

					if (scanner.get_iterator()->is_1d()) {
						if (scanner.isend() || last_scope_interval1d.chromid != scanner.last_scope_interval1d().chromid) {
							unique_ptr<GIntervalsFetcher1D> out_intervals(intervals1d->create_masked_copy(last_scope_interval1d.chromid));

							SEXP rintervals = build_rintervals_quantiles(out_intervals.get(), NULL, percentiles, medians, iu, false);
							GIntervalsBigSet1D::save_chrom(intervset_out.c_str(), out_intervals.get(), rintervals, iu, chromstats1d);
							chroms1d.erase(last_scope_interval1d.chromid);
							medians.clear();
						}
					} else {
						if (scanner.isend() || !last_scope_interval2d.is_same_chrom(scanner.last_scope_interval2d())) {
							unique_ptr<GIntervalsFetcher2D> out_intervals(
								intervals2d->create_masked_copy(last_scope_interval2d.chromid1(), last_scope_interval2d.chromid2()));

							SEXP rintervals = build_rintervals_quantiles(NULL, out_intervals.get(), percentiles, medians, iu, false);
							GIntervalsBigSet2D::save_chrom(intervset_out.c_str(), out_intervals.get(), rintervals, iu, chromstats2d);
							chroms2d.erase(ChromPair(last_scope_interval2d.chromid1(), last_scope_interval2d.chromid2()));
							medians.clear();
						}
					}
				}
			}

			// finish saving (save skipped scope chromosomes + write meta)
			if (scanner.get_iterator()->is_1d()) {
				for (set<int>::const_iterator ichromid = chroms1d.begin(); ichromid != chroms1d.end(); ++ichromid) {
					unique_ptr<GIntervalsFetcher1D> out_intervals(intervals1d->create_masked_copy(*ichromid));

					int size = intervals1d->size(*ichromid);
					iu.verify_max_data_size(size, "Result", false);
					medians.resize(size, numeric_limits<double>::quiet_NaN());
					SEXP rintervals = build_rintervals_quantiles(out_intervals.get(), NULL, percentiles, medians, iu, false);
					GIntervalsBigSet1D::save_chrom(intervset_out.c_str(), out_intervals.get(), rintervals, iu, chromstats1d);
					medians.clear();
				}

				GIntervals out_intervals;
				SEXP zeroline = build_rintervals_quantiles(&out_intervals, NULL, percentiles, medians, iu, false);
				GIntervalsBigSet1D::end_save(intervset_out.c_str(), zeroline, iu, chromstats1d);
			} else {
				for (set<ChromPair>::const_iterator ichrompair = chroms2d.begin(); ichrompair != chroms2d.end(); ++ichrompair)  {
					unique_ptr<GIntervalsFetcher2D> out_intervals(intervals2d->create_masked_copy(ichrompair->chromid1, ichrompair->chromid2));

					int size = intervals2d->size(ichrompair->chromid1, ichrompair->chromid2);
					iu.verify_max_data_size(size, "Result", false);
					medians.resize(size, numeric_limits<double>::quiet_NaN());
					SEXP rintervals = build_rintervals_quantiles(NULL, out_intervals.get(), percentiles, medians, iu, false);
					GIntervalsBigSet2D::save_chrom(intervset_out.c_str(), out_intervals.get(), rintervals, iu, chromstats2d);
					medians.clear();
				}

				GIntervals2D out_intervals;
				SEXP zeroline = build_rintervals_quantiles(NULL, &out_intervals, percentiles, medians, iu, false);
				GIntervalsBigSet2D::end_save(intervset_out.c_str(), zeroline, iu, chromstats2d);
			}
		} else {
			uint64_t orig_scope_idx = 0;

			iu.verify_max_data_size(num_intervals, "Result");
			medians.resize(num_intervals * num_percentiles, numeric_limits<double>::quiet_NaN());

			while (!scanner.isend()) {
				float val = scanner.last_real(0);

				if (cur_interval_idx != scanner.last_scope_idx()) {
					cur_interval_idx = scanner.last_scope_idx();
					orig_scope_idx = scanner.get_iterator()->is_1d() ? iu.get_orig_interv_idx(scanner.last_scope_interval1d()) : iu.get_orig_interv_idx(scanner.last_scope_interval2d());
					sp.reset();
				}

				scanner.next();

				if (!std::isnan(val))
					sp.add(val, unif_rand);

				// interval has finished => calculate the percentile
				if (cur_interval_idx != scanner.last_scope_idx() || scanner.isend())
					generate_warning = calc_medians(sp, percentiles, medians, orig_scope_idx) | generate_warning;
			}

			// assemble the answer
			answer = intervals1d->size() ?
				build_rintervals_quantiles(intervals1d, NULL, percentiles, medians, iu, true) :
				build_rintervals_quantiles(NULL, intervals2d, percentiles, medians, iu, true);

			if (do_small_intervset_out) {
				string filename = interv2path(iu.get_env(), intervset_out);
				RSaneSerialize(answer, filename.c_str());
				answer = R_NilValue;
			}
		}

		if (generate_warning){
			warning("Data size in one or more intervals exceeds the limit (%llu).\n"
					"The data was sampled to fit the limit and the resulted quantiles are hence approximate.\n"
					"(The limit can be controlled by gmax.data.size limit)", (unsigned long long)iu.get_max_data_size());
		}
		
		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP gintervals_quantiles_multitask(SEXP _intervals, SEXP _expr, SEXP _percentiles, SEXP _iterator_policy, SEXP _band, SEXP _intervals_set_out, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_expr) || length(_expr) != 1)
			verror("Track argument is not a string");

		if (!isReal(_percentiles) || length(_percentiles) < 1)
			verror("Percentile argument is not a vector of numbers");

		if (!isNull(_intervals_set_out) && (!isString(_intervals_set_out) || length(_intervals_set_out) != 1))
			verror("intervals.set.out argument is not a string");

		vector<Percentile> percentiles(length(_percentiles));
		uint64_t num_percentiles = percentiles.size();
		for (int64_t i = 0; i < length(_percentiles); ++i)
			percentiles[i] = Percentile(REAL(_percentiles)[i], i);
		sort(percentiles.begin(), percentiles.end());

		for (vector<Percentile>::const_iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip) {
			if (ip->percentile < 0 || ip->percentile > 1)
				verror("Percentile (%g) is not in [0, 1] range\n", ip->percentile);
		}

		IntervUtils iu(_envir);

		GIntervalsFetcher1D *intervals1d = NULL;
		GIntervalsFetcher2D *intervals2d = NULL;
		iu.convert_rintervs(_intervals, &intervals1d, &intervals2d);
		unique_ptr<GIntervalsFetcher1D> intervals1d_guard(intervals1d);
		unique_ptr<GIntervalsFetcher2D> intervals2d_guard(intervals2d);
		intervals1d->sort();
		intervals2d->sort();
		intervals2d->verify_no_overlaps(iu.get_chromkey());

		bool is_1d_iterator = iu.is_1d_iterator(_expr, intervals1d, intervals2d, _iterator_policy);
		uint64_t num_intervals = is_1d_iterator ? intervals1d->size() : intervals2d->size();
		if (!num_intervals) 
			return R_NilValue;

		bool do_small_intervset_out = !isNull(_intervals_set_out) && !iu.needs_bigset(num_intervals);
		bool do_big_intervset_out = !isNull(_intervals_set_out) && !do_small_intervset_out;
		string intervset_out = isNull(_intervals_set_out) ? "" : CHAR(STRING_ELT(_intervals_set_out, 0));
		vector<double> medians;
		bool generate_warning = false;
		int cur_interval_idx = -1;
		int num_kids;
		SEXP answer = R_NilValue;

		if (!(num_kids = iu.prepare4multitasking(_expr, intervals1d, intervals2d, _iterator_policy, _band)))
			rreturn(R_NilValue);

		uint64_t kid_rnd_sampling_buf_size = (uint64_t)ceil(iu.get_max_data_size() / (double)num_kids);
		StreamPercentiler<double> sp(kid_rnd_sampling_buf_size, iu.get_quantile_edge_data_size(), iu.get_quantile_edge_data_size());

		if (do_big_intervset_out) {
			vector<GIntervalsBigSet1D::ChromStat> chromstats1d;
			vector<GIntervalsBigSet2D::ChromStat> chromstats2d;
			set<int> chroms1d;
			set<ChromPair> chroms2d;

			if (is_1d_iterator) {
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

			if (iu.distribute_task(sizeof(bool) +
								   (is_1d_iterator ? sizeof(GIntervalsBigSet1D::ChromStat) * chromstats1d.size() : sizeof(GIntervalsBigSet2D::ChromStat) * chromstats2d.size()),
								   0))
			{ // child process
				GInterval last_scope_interval1d;
				GInterval2D last_scope_interval2d;
				int64_t cur_interval_chrom_idx = -1;

				TrackExprScanner scanner(iu);

				scanner.begin(_expr, iu.get_kid_intervals1d(), iu.get_kid_intervals2d(), _iterator_policy, _band);

				if (dynamic_cast<const TrackExpressionCartesianGridIterator *>(scanner.get_iterator()) ||
					dynamic_cast<const TrackExpressionIntervals2DIterator *>(scanner.get_iterator()) ||
					dynamic_cast<const TrackExpressionTrackRectsIterator *>(scanner.get_iterator()))
					verror("The type of iterator is currently not supported by the function");

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
						medians.resize(size * num_percentiles, numeric_limits<double>::quiet_NaN());
						sp.reset();
					}

					scanner.next();

					if (!std::isnan(val))
						sp.add(val, unif_rand);

					// interval has finished => calculate the percentile
					if (cur_interval_idx != scanner.last_scope_idx() || scanner.isend()) {
						generate_warning = calc_medians(sp, percentiles, medians, cur_interval_chrom_idx) | generate_warning;

						if (scanner.get_iterator()->is_1d()) {
							if (scanner.isend() || last_scope_interval1d.chromid != scanner.last_scope_interval1d().chromid) {
								unique_ptr<GIntervalsFetcher1D> out_intervals(intervals1d->create_masked_copy(last_scope_interval1d.chromid));

								SEXP rintervals = build_rintervals_quantiles(out_intervals.get(), NULL, percentiles, medians, iu, false);
								GIntervalsBigSet1D::save_chrom(intervset_out.c_str(), out_intervals.get(), rintervals, iu, chromstats1d);
								medians.clear();
							}
						} else {
							if (scanner.isend() || !last_scope_interval2d.is_same_chrom(scanner.last_scope_interval2d())) {
								unique_ptr<GIntervalsFetcher2D> out_intervals(
									intervals2d->create_masked_copy(last_scope_interval2d.chromid1(), last_scope_interval2d.chromid2()));

								SEXP rintervals = build_rintervals_quantiles(NULL, out_intervals.get(), percentiles, medians, iu, false);
								GIntervalsBigSet2D::save_chrom(intervset_out.c_str(), out_intervals.get(), rintervals, iu, chromstats2d);
								medians.clear();
							}
						}
					}
				}

				// pack the result into shared memory
				void *ptr = allocate_res(0);

				pack_data(ptr, generate_warning, 1);
				if (scanner.get_iterator()->is_1d()) 
					pack_data(ptr, chromstats1d.front(), chromstats1d.size());
				else
					pack_data(ptr, chromstats2d.front(), chromstats2d.size());
			} else { // parent process
				vector<GIntervalsBigSet1D::ChromStat> kid_chromstats1d(chromstats1d.size());
				vector<GIntervalsBigSet2D::ChromStat> kid_chromstats2d(chromstats2d.size());

				for (int i = 0; i < get_num_kids(); ++i) {
					void *ptr = get_kid_res(i);
					bool kid_generate_warning;

					unpack_data(ptr, kid_generate_warning, 1);
					generate_warning |= kid_generate_warning;

					if (is_1d_iterator) {
						unpack_data(ptr, kid_chromstats1d.front(), kid_chromstats1d.size());
						for (vector<GIntervalsBigSet1D::ChromStat>::const_iterator istat = kid_chromstats1d.begin(); istat < kid_chromstats1d.end(); ++istat) {
							if (istat->size) {
								chromstats1d[istat - kid_chromstats1d.begin()] = *istat;
								chroms1d.erase(istat - kid_chromstats1d.begin());
							}
						}
					} else {
						int num_chroms = iu.get_chromkey().get_num_chroms();

						unpack_data(ptr, kid_chromstats2d.front(), kid_chromstats2d.size());
						for (vector<GIntervalsBigSet2D::ChromStat>::const_iterator istat = kid_chromstats2d.begin(); istat < kid_chromstats2d.end(); ++istat) {
							if (istat->size) {
								uint64_t idx = istat - kid_chromstats2d.begin();
								chromstats2d[idx] = *istat;
								chroms2d.erase(ChromPair(GIntervalsBigSet2D::idx2chrom1(idx, num_chroms), GIntervalsBigSet2D::idx2chrom2(idx, num_chroms)));
							}
						}
					}
				}

				// finish saving (save skipped scope chromosomes + write meta)
				if (intervals1d->size()) {
					for (set<int>::const_iterator ichromid = chroms1d.begin(); ichromid != chroms1d.end(); ++ichromid) {
						unique_ptr<GIntervalsFetcher1D> out_intervals(intervals1d->create_masked_copy(*ichromid));

						int size = intervals1d->size(*ichromid);
						iu.verify_max_data_size(size, "Result", false);
						medians.resize(size * num_percentiles, numeric_limits<double>::quiet_NaN());
						SEXP rintervals = build_rintervals_quantiles(out_intervals.get(), NULL, percentiles, medians, iu, false);
						GIntervalsBigSet1D::save_chrom(intervset_out.c_str(), out_intervals.get(), rintervals, iu, chromstats1d);
						medians.clear();
					}

					GIntervals out_intervals;
					SEXP zeroline = build_rintervals_quantiles(&out_intervals, NULL, percentiles, medians, iu, false);
					GIntervalsBigSet1D::end_save(intervset_out.c_str(), zeroline, iu, chromstats1d);
				} else {
					for (set<ChromPair>::const_iterator ichrompair = chroms2d.begin(); ichrompair != chroms2d.end(); ++ichrompair)  {
						unique_ptr<GIntervalsFetcher2D> out_intervals(intervals2d->create_masked_copy(ichrompair->chromid1, ichrompair->chromid2));

						int size = intervals2d->size(ichrompair->chromid1, ichrompair->chromid2);
						iu.verify_max_data_size(size, "Result", false);
						medians.resize(size * num_percentiles, numeric_limits<double>::quiet_NaN());
						SEXP rintervals = build_rintervals_quantiles(NULL, out_intervals.get(), percentiles, medians, iu, false);
						GIntervalsBigSet2D::save_chrom(intervset_out.c_str(), out_intervals.get(), rintervals, iu, chromstats2d);
						medians.clear();
					}

					GIntervals2D out_intervals;
					SEXP zeroline = build_rintervals_quantiles(NULL, &out_intervals, percentiles, medians, iu, false);
					GIntervalsBigSet2D::end_save(intervset_out.c_str(), zeroline, iu, chromstats2d);
				}
			}
		} else {  // !do_big_intervset_out
			vector<uint64_t> orig_scope_indices;

			iu.verify_max_data_size(num_intervals, "Result");
			if (iu.distribute_task(sizeof(bool), (intervals1d->size() ? sizeof(GInterval) : sizeof(GInterval2D)) + sizeof(double) * percentiles.size() + sizeof(uint64_t))) { // child process
				TrackExprScanner scanner(iu);
				scanner.begin(_expr, iu.get_kid_intervals1d(), iu.get_kid_intervals2d(), _iterator_policy, _band);

				if (dynamic_cast<const TrackExpressionCartesianGridIterator *>(scanner.get_iterator()) ||
					dynamic_cast<const TrackExpressionIntervals2DIterator *>(scanner.get_iterator()) ||
					dynamic_cast<const TrackExpressionTrackRectsIterator *>(scanner.get_iterator())) {
					verror("The type of iterator is currently not supported by the function");
				}

				while (!scanner.isend()) {
					float val = scanner.last_real(0);

					if (cur_interval_idx != scanner.last_scope_idx()) {
						cur_interval_idx = scanner.last_scope_idx();
						orig_scope_indices.push_back(scanner.get_iterator()->is_1d() ?
													 iu.get_orig_interv_idx(scanner.last_scope_interval1d()) :
													 iu.get_orig_interv_idx(scanner.last_scope_interval2d()));
						sp.reset();
					}

					scanner.next();

					if (!std::isnan(val))
						sp.add(val, unif_rand);

					// interval has finished => calculate the percentile
					if (cur_interval_idx != scanner.last_scope_idx() || scanner.isend()) {
						medians.resize(medians.size() + num_percentiles, numeric_limits<double>::quiet_NaN());
						generate_warning = calc_medians(sp, percentiles, medians, orig_scope_indices.size() - 1) | generate_warning;
					}
				}

				// pack the result into shared memory
				void *ptr = allocate_res(orig_scope_indices.size());

				pack_data(ptr, generate_warning, 1);
				pack_data(ptr, medians.front(), medians.size());
				pack_data(ptr, orig_scope_indices.front(), orig_scope_indices.size());
			} else { // parent process
				vector<double> kid_medians;
				vector<uint64_t> kid_orig_scope_indices;

				medians.resize(num_intervals * num_percentiles, numeric_limits<double>::quiet_NaN());

				// collect results from kids
				for (int i = 0; i < get_num_kids(); ++i) {
					void *ptr = get_kid_res(i);
					bool kid_generate_warning;
					uint64_t res_size = get_kid_res_size(i);

					kid_medians.resize(res_size * num_percentiles, numeric_limits<double>::quiet_NaN());
					kid_orig_scope_indices.resize(res_size);

					unpack_data(ptr, kid_generate_warning, 1);
					generate_warning |= kid_generate_warning;

					kid_medians.resize(res_size * num_percentiles);
					if(!kid_medians.empty() && ptr != nullptr) {
 					   unpack_data(ptr, kid_medians.front(), kid_medians.size());
					}					

					kid_orig_scope_indices.resize(res_size);
					if (!kid_orig_scope_indices.empty() && ptr != nullptr){
						unpack_data(ptr, kid_orig_scope_indices.front(), kid_orig_scope_indices.size());
					}

					for (uint64_t i = 0; i < kid_orig_scope_indices.size(); ++i) {
						uint64_t offset = i * num_percentiles;
						copy(kid_medians.begin() + offset, kid_medians.begin() + offset + num_percentiles,
							 medians.begin() + kid_orig_scope_indices[i] * num_percentiles);
					}
				}

				// assemble the answer
				answer = is_1d_iterator ?
					build_rintervals_quantiles(intervals1d, NULL, percentiles, medians, iu, true) :
					build_rintervals_quantiles(NULL, intervals2d, percentiles, medians, iu, true);

				if (do_small_intervset_out) {
					string filename = interv2path(iu.get_env(), intervset_out);
					RSaneSerialize(answer, filename.c_str());
					answer = R_NilValue;
				}
			}
		}

		if (generate_warning){
			warning("Data size in one or more intervals exceeds the limit (%llu).\n"
					"The data was sampled to fit the limit and the resulted quantiles are hence approximate.\n"
					"(The limit can be controlled by gmax.data.size limit)", (unsigned long long)iu.get_max_data_size());
		}
		
		rreturn(answer);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	rreturn(R_NilValue);
}

SEXP gbins_quantiles(SEXP _track_exprs, SEXP _breaks, SEXP _include_lowest, SEXP _percentiles, SEXP _intervals, SEXP _iterator_policy, SEXP _band, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_track_exprs) || length(_track_exprs) < 1)
			verror("Track argument is not a string vector");

		unsigned numexpr = length(_track_exprs);
		BinsManager bins_manager(_breaks, _include_lowest);

		if (bins_manager.get_num_bin_finders() != numexpr - 1)
			verror("Number of breaks sets must be equal to the number of tracks used");

		if (!isReal(_percentiles) || length(_percentiles) < 1)
			verror("Percentile argument is not a vector of numbers");

		vector<Percentile> percentiles(length(_percentiles));
		for (int64_t i = 0; i < length(_percentiles); ++i)
			percentiles[i] = Percentile(REAL(_percentiles)[i], i);
		sort(percentiles.begin(), percentiles.end());

		for (vector<Percentile>::const_iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip) {
			if (ip->percentile < 0 || ip->percentile > 1)
				verror("Percentile (%g) is not in [0, 1] range\n", ip->percentile);
		}

		IntervUtils iu(_envir);
		TrackExprScanner scanner(iu);
		unsigned totalbins = bins_manager.get_total_bins();
		iu.verify_max_data_size(totalbins, "Result");
		vector< StreamPercentiler<double> > sps(bins_manager.get_total_bins());
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

		for (vector< StreamPercentiler<double> >::iterator isp = sps.begin(); isp != sps.end(); ++isp)
			isp->init(iu.get_max_data_size(), iu.get_quantile_edge_data_size(), iu.get_quantile_edge_data_size());

		int64_t data_size = 0;

		for (scanner.begin(_track_exprs, intervals1d, intervals2d, _iterator_policy, _band); !scanner.isend(); scanner.next()) {
			float val = scanner.last_real(0);

			if (!std::isnan(val)) {
				for (unsigned i = 1; i < numexpr; ++i)
					vals[i - 1] = scanner.last_real(i);

				int index = bins_manager.vals2idx(vals);

				if (index >= 0) {
					StreamPercentiler<double> &sp = sps[index];
					uint64_t old_buf_size = sp.cur_rnd_sampling_buf_size();
					sp.add(val, unif_rand);
					data_size += sp.cur_rnd_sampling_buf_size() - old_buf_size;
					if (data_size > (int64_t)iu.get_max_data_size())
						verror("Memory limit was reached. To reduce memory usage you can lower down the number of bins.\n"
								"Note: the memory limit is controlled via gmax.data.size option (see options, getOptions).");
				}
			}
		}

		vector<double> medians(percentiles.size() * totalbins, numeric_limits<float>::quiet_NaN());
		bool generate_warning = false;

		for (unsigned i = 0; i < sps.size(); ++i) {
			if (!sps[i].stream_size())
				continue;

			bool estimation_occured = false;
			unsigned offset = i * percentiles.size();

			for (vector<Percentile>::iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip) {
				medians[offset + ip->index] = sps[i].get_percentile(ip->percentile, ip->estimation);
				if (ip->estimation)
					estimation_occured = true;
			}

			if (estimation_occured) {
				generate_warning = true;

				// make sure the percentiles are ascending (precise and estimated results might be mixed altogether)
				for (vector<Percentile>::iterator ip = percentiles.begin() + 1; ip != percentiles.end(); ++ip) {
					if (ip->estimation)
						medians[offset + ip->index] = max(medians[offset + ip->index], medians[offset + (ip - 1)->index]);
				}
				for (vector<Percentile>::reverse_iterator ip = percentiles.rbegin() + 1; ip != percentiles.rend(); ++ip) {
					if (ip->estimation)
						medians[offset + ip->index] = min(medians[offset + ip->index], medians[offset + (ip - 1)->index]);
				}
			}
		}

		if (generate_warning){
			warning("Data size in one or more intervals exceeds the limit (%llu).\n"
					"The data was sampled to fit the limit and the resulted quantiles are hence approximate.\n"
					"(The limit can be controlled by gmax.data.size limit)", (unsigned long long)iu.get_max_data_size());
		}

		// pack the answer
		SEXP answer, dim, dimnames;
		rprotect(answer = RSaneAllocVector(REALSXP, totalbins * percentiles.size()));

		for (unsigned i = 0; i < totalbins; i++) {
			unsigned offset = i * percentiles.size();

			for (vector<Percentile>::const_iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip)
				REAL(answer)[totalbins * ip->index + i] = medians[offset + ip->index];
		}

		rprotect(dim = RSaneAllocVector(INTSXP, bins_manager.get_num_bin_finders() + 1));
		rprotect(dimnames = RSaneAllocVector(VECSXP, bins_manager.get_num_bin_finders() + 1));
		bins_manager.set_dims(dim, dimnames);

		SEXP dimname;
		rprotect(dimname = RSaneAllocVector(STRSXP, percentiles.size()));
		for (vector<Percentile>::const_iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip) {
			char buf[100];

			snprintf(buf, sizeof(buf), "%g", ip->percentile);
			SET_STRING_ELT(dimname, ip->index, mkChar(buf));
		}
		SET_VECTOR_ELT(dimnames, bins_manager.get_num_bin_finders(), dimname);
		INTEGER(dim)[bins_manager.get_num_bin_finders()] = percentiles.size();

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
