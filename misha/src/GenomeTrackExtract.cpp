/*
 * GenomeTrackExtract.cpp
 *
 *  Created on: Mar 21, 2010
 *      Author: hoichman
 */

#include <cstdint>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <vector>

#include "rdbinterval.h"
#include "rdbutils.h"
#include "GenomeTrack.h"
#include "GIntervalsBigSet1D.h"
#include "GIntervalsBigSet2D.h"
#include "TrackExpressionScanner.h"

using namespace std;
using namespace rdb;

static SEXP build_rintervals_extract(GIntervalsFetcher1D *out_intervals1d, GIntervalsFetcher2D *out_intervals2d, const vector< vector<double> > &values,
									 vector<unsigned> *interv_ids, SEXP _exprs, SEXP _colnames, IntervUtils &iu)
{
	SEXP answer;
	unsigned num_interv_cols;
	unsigned num_exprs = values.size();

	if (out_intervals1d) {
		answer = iu.convert_intervs(out_intervals1d, interv_ids ? GInterval::NUM_COLS + num_exprs + 1 : GInterval::NUM_COLS + num_exprs, false);
		num_interv_cols = GInterval::NUM_COLS;
	} else {
		answer = iu.convert_intervs(out_intervals2d, interv_ids ? GInterval2D::NUM_COLS + num_exprs + 1 : GInterval2D::NUM_COLS + num_exprs, false);
		num_interv_cols = GInterval2D::NUM_COLS;
	}

	for (unsigned iexpr = 0; iexpr < num_exprs; ++iexpr) {
		SEXP expr_vals;
		rprotect(expr_vals = RSaneAllocVector(REALSXP, values[iexpr].size()));
		for (unsigned i = 0; i < values[iexpr].size(); ++i)
			REAL(expr_vals)[i] = values[iexpr][i];
        SET_VECTOR_ELT(answer, num_interv_cols + iexpr, expr_vals);
	}

	SEXP col_names = getAttrib(answer, R_NamesSymbol);
	for (unsigned iexpr = 0; iexpr < num_exprs; ++iexpr) {
		if (isNull(_colnames))
			SET_STRING_ELT(col_names, num_interv_cols + iexpr, mkChar(get_bounded_colname(CHAR(STRING_ELT(_exprs, iexpr))).c_str()));
		else
			SET_STRING_ELT(col_names, num_interv_cols + iexpr, STRING_ELT(_colnames, iexpr));
	}

	if (interv_ids) {
		SEXP ids;
		rprotect(ids = RSaneAllocVector(INTSXP, interv_ids->size()));
		for (vector<unsigned>::const_iterator iid = interv_ids->begin(); iid != interv_ids->end(); ++iid)
			INTEGER(ids)[iid - interv_ids->begin()] = *iid;
		SET_VECTOR_ELT(answer, num_interv_cols + num_exprs, ids);

		SET_STRING_ELT(col_names, num_interv_cols + num_exprs, mkChar("intervalID"));
	}

	return answer;
}


extern "C" {

SEXP C_gextract(SEXP _intervals, SEXP _exprs, SEXP _colnames, SEXP _iterator_policy, SEXP _band, SEXP _file, SEXP _intervals_set_out, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_exprs) || length(_exprs) < 1)
			verror("Track expressions argument must be a vector of strings");

		if (!isNull(_colnames)) {
			if (!isString(_colnames))
				verror("Column names argument must be a vector of strings");
			if (length(_colnames) != length(_exprs))
				verror("Number of column names must match the number of track expressions");
		}

		if (!isNull(_file) && (!isString(_file) || length(_file) != 1))
			verror("File argument must be a string or NULL");

		if (!isNull(_intervals_set_out) && (!isString(_intervals_set_out) || length(_intervals_set_out) != 1))
			verror("intervals.set.out argument is not a string");

		if (!isNull(_file) && !isNull(_intervals_set_out))
			verror("Cannot use both file and intervals.set.out arguments");

		const char *filename = isNull(_file) ? NULL : CHAR(STRING_ELT(_file, 0));
		string intervset_out = isNull(_intervals_set_out) ? "" : CHAR(STRING_ELT(_intervals_set_out, 0));
		unsigned num_exprs = (unsigned)length(_exprs);
		IntervUtils iu(_envir);

		GIntervalsFetcher1D *intervals1d = NULL;
		GIntervalsFetcher2D *intervals2d = NULL;
		iu.convert_rintervs(_intervals, &intervals1d, &intervals2d);
		unique_ptr<GIntervalsFetcher1D> intervals1d_guard(intervals1d);
		unique_ptr<GIntervalsFetcher2D> intervals2d_guard(intervals2d);
		intervals1d->sort();
		intervals2d->sort();
		intervals2d->verify_no_overlaps(iu.get_chromkey());

		TrackExprScanner scanner(iu);

		if (filename) {
			ofstream outfile;

			outfile.open(filename);
			if (outfile.fail())
				verror("Failed to open file %s for writing: %s\n", filename, strerror(errno));
			outfile << setprecision(15);

			scanner.begin(_exprs, intervals1d, intervals2d, _iterator_policy, _band);

			if (scanner.get_iterator()->is_1d()) {
				for (int i = 0; i < GInterval::NUM_COLS; ++i) 
					outfile << GInterval::COL_NAMES[i] << "\t";
			} else {
				for (int i = 0; i < GInterval2D::NUM_COLS; ++i) 
					outfile << GInterval2D::COL_NAMES[i] << "\t";
			}

			for (unsigned iexpr = 0; iexpr < num_exprs; ++iexpr) {
				if (iexpr) 
					outfile << "\t";
				if (isNull(_colnames))
					outfile << get_bounded_colname(CHAR(STRING_ELT(_exprs, iexpr)));
				else
					outfile << CHAR(STRING_ELT(_colnames, iexpr));
			}
			outfile << "\n";

			for (; !scanner.isend(); scanner.next()) { 
				if (scanner.get_iterator()->is_1d()) {
					const GInterval &interval = scanner.last_interval1d();
					outfile << iu.id2chrom(interval.chromid) << "\t" << interval.start << "\t" << interval.end;
				} else {
					const GInterval2D &interval = scanner.last_interval2d();
					outfile << iu.id2chrom(interval.chromid1()) << "\t" << interval.start1() << "\t" << interval.end1() << "\t" <<
						iu.id2chrom(interval.chromid2()) << "\t" << interval.start2() << "\t" << interval.end2();
				}

				for (unsigned iexpr = 0; iexpr < num_exprs; ++iexpr)
					outfile << "\t" << scanner.last_real(iexpr);
				outfile << "\n";

				check_interrupt();
			}
			if (outfile.fail())
				verror("Failed to write to file %s: %s\n", filename, strerror(errno));

			return R_NilValue;
		}

		GIntervals out_intervals1d;
		GIntervals2D out_intervals2d;
		vector< vector<double> > values(num_exprs);

		if (!intervset_out.empty()) {
			bool is_1d_iterator = iu.is_1d_iterator(_exprs, intervals1d, intervals2d, _iterator_policy);
			vector<GIntervalsBigSet1D::ChromStat> chromstats1d;
			vector<GIntervalsBigSet2D::ChromStat> chromstats2d;
			GInterval last_scope_interval1d;
			GInterval2D last_scope_interval2d;
			uint64_t size;
			char error_prefix[1000];

			if (is_1d_iterator)
				GIntervalsBigSet1D::begin_save(intervset_out.c_str(), iu, chromstats1d);
			else
				GIntervalsBigSet2D::begin_save(intervset_out.c_str(), iu, chromstats2d);

			scanner.begin(_exprs, intervals1d, intervals2d, _iterator_policy, _band);

			while (!scanner.isend()) {
				for (unsigned iexpr = 0; iexpr < num_exprs; ++iexpr)
					values[iexpr].push_back(scanner.last_real(iexpr));

				if (is_1d_iterator) {
					if (last_scope_interval1d.chromid != scanner.last_scope_interval1d().chromid) {
						last_scope_interval1d = scanner.last_scope_interval1d();
						snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chrom %s",
								intervset_out.c_str(), iu.id2chrom(last_scope_interval1d.chromid).c_str());
					}
					out_intervals1d.push_back(scanner.last_interval1d());
					size = out_intervals1d.size();
				} else {
					if (!last_scope_interval2d.is_same_chrom(scanner.last_scope_interval2d())) {
						last_scope_interval2d = scanner.last_scope_interval2d();
						snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chroms (%s, %s)",
								intervset_out.c_str(), iu.id2chrom(last_scope_interval2d.chromid1()).c_str(), iu.id2chrom(last_scope_interval2d.chromid2()).c_str());
					}
					out_intervals2d.push_back(scanner.last_interval2d());
					size = out_intervals2d.size();
				}

				iu.verify_max_data_size(size, error_prefix, false);

				scanner.next();

				if (is_1d_iterator) {
					if (scanner.isend() || last_scope_interval1d.chromid != scanner.last_scope_interval1d().chromid) {
						SEXP rintervals = build_rintervals_extract(&out_intervals1d, NULL, values, NULL, _exprs, _colnames, iu);
						GIntervalsBigSet1D::save_chrom(intervset_out.c_str(), &out_intervals1d, rintervals, iu, chromstats1d);
						out_intervals1d.clear();
						for (vector< vector<double> >::iterator ivalues = values.begin(); ivalues != values.end(); ++ivalues) 
							ivalues->clear();
					}
				} else {
					if (scanner.isend() || !last_scope_interval2d.is_same_chrom(scanner.last_scope_interval2d())) {
						SEXP rintervals = build_rintervals_extract(NULL, &out_intervals2d, values, NULL, _exprs, _colnames, iu);
						GIntervalsBigSet2D::save_chrom(intervset_out.c_str(), &out_intervals2d, rintervals, iu, chromstats2d);
						out_intervals2d.clear();
						for (vector< vector<double> >::iterator ivalues = values.begin(); ivalues != values.end(); ++ivalues) 
							ivalues->clear();
					}
				}
			}

			// finish saving (write meta)
			if (is_1d_iterator) {
				SEXP zeroline = build_rintervals_extract(&out_intervals1d, NULL, values, NULL, _exprs, _colnames, iu);
				GIntervalsBigSet1D::end_save(intervset_out.c_str(), zeroline, iu, chromstats1d);
			} else {
				SEXP zeroline = build_rintervals_extract(NULL, &out_intervals2d, values, NULL, _exprs, _colnames, iu);
				GIntervalsBigSet2D::end_save(intervset_out.c_str(), zeroline, iu, chromstats2d);
			}

			return R_NilValue;
		}

		vector<unsigned> interv_ids;

		for (scanner.begin(_exprs, intervals1d, intervals2d, _iterator_policy, _band); !scanner.isend(); scanner.next()) {
			for (unsigned iexpr = 0; iexpr < num_exprs; ++iexpr)
				values[iexpr].push_back(scanner.last_real(iexpr));

			if (scanner.get_iterator()->is_1d()) {
				out_intervals1d.push_back(scanner.last_interval1d());
				interv_ids.push_back(iu.get_orig_interv_idx(scanner.last_scope_interval1d()) + 1);
			} else {
				out_intervals2d.push_back(scanner.last_interval2d());
				interv_ids.push_back(iu.get_orig_interv_idx(scanner.last_scope_interval2d()) + 1);
			}

			iu.verify_max_data_size(values[0].size(), "Result");
			check_interrupt();
		}

		if (out_intervals1d.empty() && out_intervals2d.empty())
			return R_NilValue;

		// assemble the answer
		SEXP answer;

		if (!out_intervals1d.empty())
			answer = build_rintervals_extract(&out_intervals1d, NULL, values, &interv_ids, _exprs, _colnames, iu);
		else
			answer = build_rintervals_extract(NULL, &out_intervals2d, values, &interv_ids, _exprs, _colnames, iu);

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP gextract_multitask(SEXP _intervals, SEXP _exprs, SEXP _colnames, SEXP _iterator_policy, SEXP _band, SEXP _file, SEXP _intervals_set_out, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_exprs) || length(_exprs) < 1)
			verror("Tracks expressions argument must be a vector of strings");

		if (!isNull(_colnames)) {
			if (!isString(_colnames))
				verror("Column names argument must be a vector of strings");
			if (length(_colnames) != length(_exprs))
				verror("Number of column names must match the number of track expressions");
		}

		if (!isNull(_file) && (!isString(_file) || length(_file) != 1))
			verror("File argument must be a string or NULL");

		if (!isNull(_intervals_set_out) && (!isString(_intervals_set_out) || length(_intervals_set_out) != 1))
			verror("intervals.set.out argument is not a string");

		if (!isNull(_file) && !isNull(_intervals_set_out))
			verror("Cannot use both file and intervals.set.out arguments");

		const char *filename = isNull(_file) ? NULL : CHAR(STRING_ELT(_file, 0));
		unsigned num_exprs = (unsigned)length(_exprs);
		uint64_t num_intervals;
		GIntervals out_intervals1d;
		GIntervals2D out_intervals2d;
		vector<unsigned> interv_ids;
		vector< vector<double> > values(num_exprs);
		IntervUtils iu(_envir);

		GIntervalsFetcher1D *intervals1d = NULL;
		GIntervalsFetcher2D *intervals2d = NULL;
		iu.convert_rintervs(_intervals, &intervals1d, &intervals2d);
		unique_ptr<GIntervalsFetcher1D> intervals1d_guard(intervals1d);
		unique_ptr<GIntervalsFetcher2D> intervals2d_guard(intervals2d);
		intervals1d->sort();
		intervals2d->sort();
		intervals2d->verify_no_overlaps(iu.get_chromkey());

		string intervset_out = isNull(_intervals_set_out) ? "" : CHAR(STRING_ELT(_intervals_set_out, 0));

		if (filename) {
			TrackExprScanner scanner(iu);
			ofstream outfile;

			outfile.open(filename);
			if (outfile.fail())
				verror("Failed to open file %s for writing: %s\n", filename, strerror(errno));
			outfile << setprecision(15);

			scanner.begin(_exprs, intervals1d, intervals2d, _iterator_policy, _band);

			if (scanner.get_iterator()->is_1d()) {
				for (int i = 0; i < GInterval::NUM_COLS; ++i) 
					outfile << GInterval::COL_NAMES[i] << "\t";
			} else {
				for (int i = 0; i < GInterval2D::NUM_COLS; ++i) 
					outfile << GInterval2D::COL_NAMES[i] << "\t";
			}

			for (unsigned iexpr = 0; iexpr < num_exprs; ++iexpr) {
				if (iexpr) 
					outfile << "\t";
				if (isNull(_colnames))
					outfile << get_bounded_colname(CHAR(STRING_ELT(_exprs, iexpr)));
				else
					outfile << CHAR(STRING_ELT(_colnames, iexpr));
			}
			outfile << "\n";

			for (; !scanner.isend(); scanner.next()) { 
				if (scanner.get_iterator()->is_1d()) {
					const GInterval &interval = scanner.last_interval1d();
					outfile << iu.id2chrom(interval.chromid) << "\t" << interval.start << "\t" << interval.end;
				} else {
					const GInterval2D &interval = scanner.last_interval2d();
					outfile << iu.id2chrom(interval.chromid1()) << "\t" << interval.start1() << "\t" << interval.end1() << "\t" <<
						iu.id2chrom(interval.chromid2()) << "\t" << interval.start2() << "\t" << interval.end2();
				}

				for (unsigned iexpr = 0; iexpr < num_exprs; ++iexpr)
					outfile << "\t" << scanner.last_real(iexpr);
				outfile << "\n";

				check_interrupt();
			}
			if (outfile.fail())
				verror("Failed to write to file %s: %s\n", filename, strerror(errno));

			return R_NilValue;
		}

		bool is_1d_iterator = iu.is_1d_iterator(_exprs, intervals1d, intervals2d, _iterator_policy);

		if (!iu.prepare4multitasking(_exprs, intervals1d, intervals2d, _iterator_policy, _band))
			rreturn(R_NilValue);

		if (!intervset_out.empty()) {
			vector<GIntervalsBigSet1D::ChromStat> chromstats1d;
			vector<GIntervalsBigSet2D::ChromStat> chromstats2d;

			if (is_1d_iterator)
				GIntervalsBigSet1D::begin_save(intervset_out.c_str(), iu, chromstats1d);
			else
				GIntervalsBigSet2D::begin_save(intervset_out.c_str(), iu, chromstats2d);

			if (iu.distribute_task(is_1d_iterator ?
								   sizeof(GIntervalsBigSet1D::ChromStat) * chromstats1d.size() :
								   sizeof(GIntervalsBigSet2D::ChromStat) * chromstats2d.size(),
								   0))
			{ // child process
				GIntervalsFetcher1D *kid_intervals1d = iu.get_kid_intervals1d();
				GIntervalsFetcher2D *kid_intervals2d = iu.get_kid_intervals2d();
				TrackExprScanner scanner(iu);
				GInterval last_scope_interval1d;
				GInterval2D last_scope_interval2d;
				uint64_t size;
				char error_prefix[1000];

				scanner.begin(_exprs, kid_intervals1d, kid_intervals2d, _iterator_policy, _band);

				while (!scanner.isend()) {
					for (unsigned iexpr = 0; iexpr < num_exprs; ++iexpr)
						values[iexpr].push_back(scanner.last_real(iexpr));

					if (is_1d_iterator) {
						if (last_scope_interval1d.chromid != scanner.last_scope_interval1d().chromid) {
							last_scope_interval1d = scanner.last_scope_interval1d();
							snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chrom %s",
									intervset_out.c_str(), iu.id2chrom(last_scope_interval1d.chromid).c_str());
						}
						out_intervals1d.push_back(scanner.last_interval1d());
						size = out_intervals1d.size();
					} else {
						if (!last_scope_interval2d.is_same_chrom(scanner.last_scope_interval2d())) {
							last_scope_interval2d = scanner.last_scope_interval2d();
							snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chroms (%s, %s)",
									intervset_out.c_str(), iu.id2chrom(last_scope_interval2d.chromid1()).c_str(), iu.id2chrom(last_scope_interval2d.chromid2()).c_str());
						}
						out_intervals2d.push_back(scanner.last_interval2d());
						size = out_intervals2d.size();
					}

					iu.verify_max_data_size(size, error_prefix, false);

					scanner.next();

					if (is_1d_iterator) {
						if (scanner.isend() || last_scope_interval1d.chromid != scanner.last_scope_interval1d().chromid) {
							SEXP rintervals = build_rintervals_extract(&out_intervals1d, NULL, values, NULL, _exprs, _colnames, iu);
							GIntervalsBigSet1D::save_chrom(intervset_out.c_str(), &out_intervals1d, rintervals, iu, chromstats1d);
							out_intervals1d.clear();
							for (vector< vector<double> >::iterator ivalues = values.begin(); ivalues != values.end(); ++ivalues) 
								ivalues->clear();
						}
					} else {
						if (scanner.isend() || !last_scope_interval2d.is_same_chrom(scanner.last_scope_interval2d())) {
							SEXP rintervals = build_rintervals_extract(NULL, &out_intervals2d, values, NULL, _exprs, _colnames, iu);
							GIntervalsBigSet2D::save_chrom(intervset_out.c_str(), &out_intervals2d, rintervals, iu, chromstats2d);
							out_intervals2d.clear();
							for (vector< vector<double> >::iterator ivalues = values.begin(); ivalues != values.end(); ++ivalues) 
								ivalues->clear();
						}
					}
				}

				// pack the result into shared memory
				void *ptr = allocate_res(0);

				if (is_1d_iterator) 
					pack_data(ptr, chromstats1d.front(), chromstats1d.size());
				else
					pack_data(ptr, chromstats2d.front(), chromstats2d.size());
			} else { // parent process
				vector<GIntervalsBigSet1D::ChromStat> kid_chromstats1d(chromstats1d.size());
				vector<GIntervalsBigSet2D::ChromStat> kid_chromstats2d(chromstats2d.size());

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
				if (is_1d_iterator) {
					SEXP zeroline = build_rintervals_extract(&out_intervals1d, NULL, values, NULL, _exprs, _colnames, iu);
					GIntervalsBigSet1D::end_save(intervset_out.c_str(), zeroline, iu, chromstats1d);
				} else {
					SEXP zeroline = build_rintervals_extract(NULL, &out_intervals2d, values, NULL, _exprs, _colnames, iu);
					GIntervalsBigSet2D::end_save(intervset_out.c_str(), zeroline, iu, chromstats2d);
				}
			}
			rreturn(R_NilValue);
		}

		if (iu.distribute_task(0,
							   (is_1d_iterator ? sizeof(GInterval) : sizeof(GInterval2D)) + // interval
							   sizeof(unsigned) +                                                // interval id
							   sizeof(double) * num_exprs))                                      // values
		{  // child process
			GIntervalsFetcher1D *kid_intervals1d = iu.get_kid_intervals1d();
			GIntervalsFetcher2D *kid_intervals2d = iu.get_kid_intervals2d();
			TrackExprScanner scanner(iu);
			
			for (scanner.begin(_exprs, kid_intervals1d, kid_intervals2d, _iterator_policy, _band); !scanner.isend(); scanner.next()) {
				for (unsigned iexpr = 0; iexpr < num_exprs; ++iexpr)
					values[iexpr].push_back(scanner.last_real(iexpr));
			
				if (is_1d_iterator) {
					out_intervals1d.push_back(scanner.last_interval1d());
					interv_ids.push_back(iu.get_orig_interv_idx(scanner.last_scope_interval1d()) + 1);
				} else {
					out_intervals2d.push_back(scanner.last_interval2d());
					interv_ids.push_back(iu.get_orig_interv_idx(scanner.last_scope_interval2d()) + 1);
				}
			
				iu.verify_max_data_size(values[0].size(), "Result");
			}
			
			// now we finally know the result size => pack the result into shared memory
			num_intervals = is_1d_iterator ? out_intervals1d.size() : out_intervals2d.size();

			void *result = allocate_res(num_intervals);
			
			if (!num_intervals)
				rreturn(R_NilValue);

			if (is_1d_iterator)
				pack_data(result, out_intervals1d.front(), num_intervals);
			else
				pack_data(result, out_intervals2d.front(), num_intervals);
			
			pack_data(result, interv_ids.front(), num_intervals);
			
			for (unsigned i = 0; i < num_exprs; ++i)
				pack_data(result, values[i].front(), num_intervals);
			
		} else {  // parent process
			// collect results from kids
			for (int i = 0; i < get_num_kids(); ++i) {
				void *ptr = get_kid_res(i);
				num_intervals = get_kid_res_size(i);

				if (!num_intervals)
					continue;

				if (is_1d_iterator) {
					out_intervals1d.insert(out_intervals1d.end(), (GInterval *)ptr, (GInterval *)ptr + num_intervals);
					ptr = (GInterval *)ptr + num_intervals;
				} else {
					out_intervals2d.insert(out_intervals2d.end(), (GInterval2D *)ptr, (GInterval2D *)ptr + num_intervals);
					ptr = (GInterval2D *)ptr + num_intervals;
				}

				interv_ids.insert(interv_ids.end(), (unsigned *)ptr, (unsigned *)ptr + num_intervals);
				ptr = (unsigned *)ptr + num_intervals;

				for (unsigned i = 0; i < num_exprs; ++i) {
					values[i].insert(values[i].end(), (double *)ptr, (double *)ptr + num_intervals);
					ptr = (double *)ptr + num_intervals;
				}
			}

			if (out_intervals1d.empty() && out_intervals2d.empty()) 
				rreturn(R_NilValue);

			// assemble the answer
			SEXP answer;
			unsigned num_interv_cols;

			if (!out_intervals1d.empty()) {
				answer = iu.convert_intervs(&out_intervals1d, GInterval::NUM_COLS + num_exprs + 1);
				num_interv_cols = GInterval::NUM_COLS;
			} else {
				answer = iu.convert_intervs(&out_intervals2d, GInterval2D::NUM_COLS + num_exprs + 1);
				num_interv_cols = GInterval2D::NUM_COLS;
			}

			for (unsigned iexpr = 0; iexpr < num_exprs; ++iexpr) {
				SEXP expr_vals;
				rprotect(expr_vals = RSaneAllocVector(REALSXP, values[iexpr].size()));
				for (unsigned i = 0; i < values[iexpr].size(); ++i)
					REAL(expr_vals)[i] = values[iexpr][i];
                SET_VECTOR_ELT(answer, num_interv_cols + iexpr, expr_vals);
			}

			SEXP ids;
			rprotect(ids = RSaneAllocVector(INTSXP, interv_ids.size()));
			for (vector<unsigned>::const_iterator iid = interv_ids.begin(); iid != interv_ids.end(); ++iid)
				INTEGER(ids)[iid - interv_ids.begin()] = *iid;
			SET_VECTOR_ELT(answer, num_interv_cols + num_exprs, ids);

			SEXP col_names = getAttrib(answer, R_NamesSymbol);
			for (unsigned iexpr = 0; iexpr < num_exprs; ++iexpr) {
				if (isNull(_colnames))
					SET_STRING_ELT(col_names, num_interv_cols + iexpr, mkChar(get_bounded_colname(CHAR(STRING_ELT(_exprs, iexpr))).c_str()));
				else
					SET_STRING_ELT(col_names, num_interv_cols + iexpr, STRING_ELT(_colnames, iexpr));
			}
			SET_STRING_ELT(col_names, num_interv_cols + num_exprs, mkChar("intervalID"));

			rreturn(answer);
		}
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	rreturn(R_NilValue);
}

}
