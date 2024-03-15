#include <cstdint>
#include <vector>

#include "rdbinterval.h"
#include "rdbutils.h"
#include "GenomeTrack.h"
#include "TrackExpressionScanner.h"
#include "TrackExpressionCartesianGridIterator.h"
#include "TrackExpressionIntervals2DIterator.h"
#include "TrackExpressionTrackRectsIterator.h"

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>

using namespace std;
using namespace rdb;

extern "C" {

SEXP gmapply(SEXP _intervals, SEXP _fn, SEXP _track_exprs, SEXP _enable_gapply_intervals, SEXP _iterator_policy, SEXP _band,
			 SEXP _report_progress, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;


		if (!isFunction(_fn))
			verror("FUN argument must be a function");

		if (!isString(_track_exprs) || length(_track_exprs) < 1)
			verror("Tracks expressions argument must be a vector of strings");

		if (!isLogical(_enable_gapply_intervals) || length(_enable_gapply_intervals) != 1)
			verror("Allow GAPPLY.INTERVALS argument must be logical");

		bool enable_gapply_intervals = LOGICAL(_enable_gapply_intervals)[0];
		unsigned num_track_exprs = (unsigned)length(_track_exprs);
		vector<SEXP> rvars(num_track_exprs, R_NilValue);
		IntervUtils iu(_envir);
		GIntervals intervals1d;
		GIntervals2D intervals2d;
		_intervals = iu.convert_rintervs(_intervals, &intervals1d, &intervals2d);
		intervals1d.sort();
		intervals2d.sort();
		intervals2d.verify_no_overlaps(iu.get_chromkey());

		SEXP eval_expr;

		rprotect(eval_expr = allocList(num_track_exprs + 1));
		SET_TYPEOF(eval_expr, LANGSXP);
		SETCAR(eval_expr, _fn);

		TrackExprScanner scanner(iu);
		int interval_idx = -1;
		vector< vector<double> > vals(num_track_exprs);

		SEXP rinterv_id;
		rprotect(rinterv_id = RSaneAllocVector(INTSXP, 1));
		defineVar(install("GAPPLY.INTERVID"), rinterv_id, findVar(install(".misha"), _envir));

		GIntervals last_intervals1d;
		GIntervals2D last_intervals2d;
		SEXP rlast_intervals;

		scanner.report_progress(LOGICAL(_report_progress)[0]);
		scanner.begin(_track_exprs, &intervals1d, &intervals2d, _iterator_policy, _band);

		if (dynamic_cast<const TrackExpressionCartesianGridIterator *>(scanner.get_iterator()) ||
			dynamic_cast<const TrackExpressionIntervals2DIterator *>(scanner.get_iterator()) ||
			dynamic_cast<const TrackExpressionTrackRectsIterator *>(scanner.get_iterator()))
			verror("The type of iterator is currently not supported by the function");

		vector<double> result(scanner.get_iterator()->is_1d() ? intervals1d.size() : intervals2d.size(), numeric_limits<double>::quiet_NaN());

		while (!scanner.isend()) {
			if (interval_idx != scanner.last_scope_idx()) {
				interval_idx = scanner.last_scope_idx();
				INTEGER(rinterv_id)[0] = scanner.get_iterator()->is_1d() ? iu.get_orig_interv_idx(intervals1d[interval_idx]) + 1 : iu.get_orig_interv_idx(intervals2d[interval_idx]) + 1;

				for (unsigned iexpr = 0; iexpr < num_track_exprs; ++iexpr)
					vals[iexpr].clear();
				last_intervals1d.clear();
				last_intervals2d.clear();
			}

			for (unsigned iexpr = 0; iexpr < num_track_exprs; ++iexpr)
				vals[iexpr].push_back(scanner.last_real(iexpr));

			if (enable_gapply_intervals) {
				if (scanner.get_iterator()->is_1d())
					last_intervals1d.push_back(scanner.last_interval1d());
				else
					last_intervals2d.push_back(scanner.last_interval2d());
			}

			scanner.next();

			if (interval_idx != scanner.last_scope_idx() || scanner.isend()) {
				SEXP rarg = eval_expr;
				for (unsigned iexpr = 0; iexpr < num_track_exprs; ++iexpr) {
					uint64_t num_vals = vals.front().size();

					runprotect(rvars[iexpr]);
					rprotect(rvars[iexpr] = RSaneAllocVector(REALSXP, num_vals));
					rarg = CDR(rarg);
					SETCAR(rarg, rvars[iexpr]);

					if (scanner.get_iterator()->is_1d() && intervals1d[interval_idx].strand == -1) {
						for (unsigned i = 0; i < num_vals; ++i)
							REAL(rvars[iexpr])[i] = vals[iexpr][num_vals - i - 1];
					} else {
						for (unsigned i = 0; i < num_vals; ++i)
							REAL(rvars[iexpr])[i] = vals[iexpr][i];
					}
				}

				if (enable_gapply_intervals) {
					rlast_intervals = scanner.get_iterator()->is_1d() ? iu.convert_intervs(&last_intervals1d) : iu.convert_intervs(&last_intervals2d);
					defineVar(install("GAPPLY.INTERVALS"), rlast_intervals, findVar(install(".misha"), _envir));
				}

				SEXP res = eval_in_R(eval_expr, _envir);
				if (!isReal(res) || length(res) != 1)
					verror("Evaluation of function does not produce a single numeric value");
				result[interval_idx] = REAL(res)[0];

				if (enable_gapply_intervals)
					runprotect(rlast_intervals);

				runprotect(res);
			}
		}

		// pack the answer
		unsigned type_mask = iu.get_rintervs_type_mask(_intervals);

		if (type_mask == (IntervUtils::INTERVS1D | IntervUtils::INTERVS2D))
			_intervals = scanner.get_iterator()->is_1d() ? VECTOR_ELT(_intervals, 0) : VECTOR_ELT(_intervals, 1);

		int num_old_cols = length(_intervals);
		SEXP answer;
		SEXP old_colnames = getAttrib(_intervals, R_NamesSymbol);
		SEXP col_names;
		SEXP row_names;
		SEXP rvals;

		rprotect(answer = RSaneAllocVector(VECSXP, num_old_cols + 1));
		rprotect(col_names = RSaneAllocVector(STRSXP, num_old_cols + 1));

		for (int i = 0; i < num_old_cols; i++) {
			SET_VECTOR_ELT(answer, i, VECTOR_ELT(_intervals, i));
			SET_STRING_ELT(col_names, i, STRING_ELT(old_colnames, i));
		}

		rprotect(row_names = RSaneAllocVector(INTSXP, result.size()));
		rprotect(rvals = RSaneAllocVector(REALSXP, result.size()));
		for (unsigned i = 0; i < result.size(); i++) {
			INTEGER(row_names)[i] = i + 1;
			REAL(rvals)[scanner.get_iterator()->is_1d() ? iu.get_orig_interv_idx(intervals1d[i]) : iu.get_orig_interv_idx(intervals2d[i])] = result[i];
		}

		SET_VECTOR_ELT(answer, num_old_cols, rvals);
		SET_STRING_ELT(col_names, num_old_cols, mkChar("value"));

		setAttrib(answer, R_NamesSymbol, col_names);
		setAttrib(answer, R_ClassSymbol, mkString("data.frame"));
		setAttrib(answer, R_RowNamesSymbol, row_names);

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP gmapply_multitask(SEXP _intervals, SEXP _fn, SEXP _track_exprs, SEXP _enable_gapply_intervals, SEXP _iterator_policy, SEXP _band,
					   SEXP _report_progress, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isFunction(_fn))
			verror("FUN argument must be a function");

		if (!isString(_track_exprs) || length(_track_exprs) < 1)
			verror("Tracks expressions argument must be a vector of strings");

		if (!isLogical(_enable_gapply_intervals) || length(_enable_gapply_intervals) != 1)
			verror("Allow GAPPLY.INTERVALS argument must be logical");

		bool enable_gapply_intervals = LOGICAL(_enable_gapply_intervals)[0];
		unsigned num_track_exprs = (unsigned)length(_track_exprs);
		IntervUtils iu(_envir);

		GIntervals intervals1d;
		GIntervals2D intervals2d;
		_intervals = iu.convert_rintervs(_intervals, &intervals1d, &intervals2d);
		intervals1d.sort();
		intervals2d.sort();
		intervals2d.verify_no_overlaps(iu.get_chromkey());

		if (!iu.prepare4multitasking(_track_exprs, &intervals1d, &intervals2d, _iterator_policy, _band))
			rreturn(R_NilValue);

		if (iu.distribute_task(0, sizeof(double))) {  // child process
			GIntervalsFetcher1D *kid_intervals1d = iu.get_kid_intervals1d();
			GIntervalsFetcher2D *kid_intervals2d = iu.get_kid_intervals2d();
			vector<SEXP> rvars(num_track_exprs, R_NilValue);
			SEXP eval_expr;

			rprotect(eval_expr = allocList(num_track_exprs + 1));
			SET_TYPEOF(eval_expr, LANGSXP);
			SETCAR(eval_expr, _fn);

			TrackExprScanner scanner(iu);
			int interval_idx = -1;
			vector<double> result(kid_intervals1d ? kid_intervals1d->size() : kid_intervals2d->size(), numeric_limits<double>::quiet_NaN());
			vector< vector<double> > vals(num_track_exprs);

			SEXP rinterv_id;
			rprotect(rinterv_id = RSaneAllocVector(INTSXP, 1));
			defineVar(install("GAPPLY.INTERVID"), rinterv_id, findVar(install(".misha"), _envir));

			GIntervals last_intervals1d;
			GIntervals2D last_intervals2d;
			SEXP rlast_intervals;

			scanner.report_progress(LOGICAL(_report_progress)[0]);
			scanner.begin(_track_exprs, kid_intervals1d, kid_intervals2d, _iterator_policy, _band);

			if (dynamic_cast<const TrackExpressionCartesianGridIterator *>(scanner.get_iterator()) ||
				dynamic_cast<const TrackExpressionIntervals2DIterator *>(scanner.get_iterator()) ||
				dynamic_cast<const TrackExpressionTrackRectsIterator *>(scanner.get_iterator()))
				verror("The type of iterator is currently not supported by the function");

			while (!scanner.isend()) {
				if (interval_idx != scanner.last_scope_idx()) {
					interval_idx = scanner.last_scope_idx();
					INTEGER(rinterv_id)[0] = scanner.get_iterator()->is_1d() ?
						iu.get_orig_interv_idx(scanner.last_scope_interval1d()) + 1 :
						iu.get_orig_interv_idx(scanner.last_scope_interval2d()) + 1;

					for (unsigned iexpr = 0; iexpr < num_track_exprs; ++iexpr)
						vals[iexpr].clear();
					last_intervals1d.clear();
					last_intervals2d.clear();
				}

				for (unsigned iexpr = 0; iexpr < num_track_exprs; ++iexpr)
					vals[iexpr].push_back(scanner.last_real(iexpr));

				if (enable_gapply_intervals) {
					if (scanner.get_iterator()->is_1d())
						last_intervals1d.push_back(scanner.last_interval1d());
					else
						last_intervals2d.push_back(scanner.last_interval2d());
				}

				scanner.next();

				if (interval_idx != scanner.last_scope_idx() || scanner.isend()) {
					SEXP rarg = eval_expr;
					for (unsigned iexpr = 0; iexpr < num_track_exprs; ++iexpr) {
						uint64_t num_vals = vals.front().size();

						runprotect(rvars[iexpr]);
						rprotect(rvars[iexpr] = RSaneAllocVector(REALSXP, num_vals));
						rarg = CDR(rarg);
						SETCAR(rarg, rvars[iexpr]);

						if (scanner.get_iterator()->is_1d() && scanner.last_scope_interval1d().strand == -1) {
							for (unsigned i = 0; i < num_vals; ++i)
								REAL(rvars[iexpr])[i] = vals[iexpr][num_vals - i - 1];
						} else {
							for (unsigned i = 0; i < num_vals; ++i)
								REAL(rvars[iexpr])[i] = vals[iexpr][i];
						}
					}

					if (enable_gapply_intervals) {
						rlast_intervals = scanner.get_iterator()->is_1d() ? iu.convert_intervs(&last_intervals1d) : iu.convert_intervs(&last_intervals2d);
						defineVar(install("GAPPLY.INTERVALS"), rlast_intervals, findVar(install(".misha"), _envir));
					}

					SEXP res = eval_in_R(eval_expr, _envir);
					if (!isReal(res) || length(res) != 1)
						verror("Evaluation of function does not produce a single numeric value");
					result[interval_idx] = REAL(res)[0];

					if (enable_gapply_intervals)
						runprotect(rlast_intervals);

					runprotect(res);
				}
			}

			// pack the result into shared memory
			void *ptr = allocate_res(result.size());

			if (result.empty())
				rreturn(R_NilValue);

			pack_data(ptr, result.front(), result.size());

		} else {  // parent process
			vector<double> result;

			for (int i = 0; i < get_num_kids(); ++i) {
				void *ptr = get_kid_res(i);
				uint64_t res_size = get_kid_res_size(i);

				if (!res_size)
					continue;

				result.insert(result.end(), (double *)ptr, (double *)ptr + res_size);
			}

			if (result.empty()) 
				rreturn(R_NilValue);

			// pack the answer
			unsigned type_mask = iu.get_rintervs_type_mask(_intervals);

			if (type_mask == (IntervUtils::INTERVS1D | IntervUtils::INTERVS2D))
				_intervals = intervals2d.empty() ? VECTOR_ELT(_intervals, 0) : VECTOR_ELT(_intervals, 1);

			int num_old_cols = length(_intervals);
			SEXP answer;
			SEXP old_colnames = getAttrib(_intervals, R_NamesSymbol);
			SEXP col_names;
			SEXP row_names;
			SEXP rvals;

			rprotect(answer = RSaneAllocVector(VECSXP, num_old_cols + 1));
			rprotect(col_names = RSaneAllocVector(STRSXP, num_old_cols + 1));

			for (int i = 0; i < num_old_cols; i++) {
				SET_VECTOR_ELT(answer, i, VECTOR_ELT(_intervals, i));
				SET_STRING_ELT(col_names, i, STRING_ELT(old_colnames, i));
			}

			rprotect(row_names = RSaneAllocVector(INTSXP, result.size()));
			rprotect(rvals = RSaneAllocVector(REALSXP, result.size()));
			for (unsigned i = 0; i < result.size(); i++) {
				INTEGER(row_names)[i] = i + 1;
				REAL(rvals)[intervals2d.empty() ? iu.get_orig_interv_idx(intervals1d[i]) : iu.get_orig_interv_idx(intervals2d[i])] = result[i];
			}

			SET_VECTOR_ELT(answer, num_old_cols, rvals);
			SET_STRING_ELT(col_names, num_old_cols, mkChar("value"));

			setAttrib(answer, R_NamesSymbol, col_names);
			setAttrib(answer, R_ClassSymbol, mkString("data.frame"));
			setAttrib(answer, R_RowNamesSymbol, row_names);
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
