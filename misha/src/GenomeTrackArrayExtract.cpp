#include <cstdint>
#include <vector>

#include <fstream>
#include <iomanip>
#include <iostream>
#include "rdbinterval.h"
#include "rdbprogress.h"
#include "rdbutils.h"
#include "GenomeTrack.h"
#include "GIntervalsBigSet1D.h"
#include "TrackExpressionSparseIterator.h"

using namespace std;
using namespace rdb;

static SEXP build_rintervals_arrayextract(GIntervalsFetcher1D *out_intervals, const vector<float> &res_vals,
										  vector<unsigned> *interv_ids, int numcols, SEXP _colnames, IntervUtils &iu)
{
	SEXP answer = iu.convert_intervs(out_intervals, interv_ids ? GInterval::NUM_COLS + numcols + 1 : GInterval::NUM_COLS + numcols, false);
	uint64_t numvals = res_vals.size() / numcols;
	vector<SEXP> rvals(numcols);

	for (int icol = 0; icol < numcols; ++icol) {
		rprotect(rvals[icol] = RSaneAllocVector(REALSXP, numvals));
	}

	int rownum = 0;
	for (vector<float>::const_iterator ival = res_vals.begin(); ival != res_vals.end(); ++rownum) {
		for (vector<SEXP>::iterator irvals = rvals.begin(); irvals != rvals.end(); ++irvals) {
			REAL(*irvals)[rownum] = *ival;
			++ival;
		}
	}

	SEXP colnames = getAttrib(answer, R_NamesSymbol);
	for (int icol = 0; icol < numcols; ++icol){
		SET_STRING_ELT(colnames, GInterval::NUM_COLS + icol, STRING_ELT(_colnames, icol));
	}

	if (interv_ids) {
		SEXP ids;
		rprotect(ids = RSaneAllocVector(INTSXP, interv_ids->size()));
		for (vector<unsigned>::const_iterator iid = interv_ids->begin(); iid != interv_ids->end(); ++iid)
			INTEGER(ids)[iid - interv_ids->begin()] = *iid;
		SET_VECTOR_ELT(answer, GInterval::NUM_COLS + numcols, ids);
		SET_STRING_ELT(colnames, GInterval::NUM_COLS + numcols, mkChar("intervalID"));
	}

    for (int icol = 0; icol < numcols; ++icol) {
        SET_VECTOR_ELT(answer, GInterval::NUM_COLS + icol, rvals[icol]);
	}
	
	return answer;
}

extern "C" {

SEXP garrayextract(SEXP _track, SEXP _slice, SEXP _colnames, SEXP _file, SEXP _intervals, SEXP _intervals_set_out, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_track) || length(_track) != 1)
			verror("Track argument must be a string");

		if (!isString(_colnames))
			verror("Column names argument must be a vector of strings");

		if (!isNull(_file) && (!isString(_file) || length(_file) != 1))
			verror("File argument must be a string or NULL");

		if (!isNull(_intervals_set_out) && (!isString(_intervals_set_out) || length(_intervals_set_out) != 1))
			verror("intervals.set.out argument is not a string");

		if (!isNull(_file) && !isNull(_intervals_set_out))
			verror("Cannot use both file and intervals.set.out arguments");

		const char *filename = isNull(_file) ? NULL : CHAR(STRING_ELT(_file, 0));
		string intervset_out = isNull(_intervals_set_out) ? "" : CHAR(STRING_ELT(_intervals_set_out, 0));
		IntervUtils iu(_envir);
		const char *track_str = CHAR(STRING_ELT(_track, 0));
		string track_path = track2path(iu.get_env(), track_str);
		GenomeTrack::Type track_type = GenomeTrack::get_type(track_path.c_str(), iu.get_chromkey());
		vector<unsigned> slice;

		if (track_type != GenomeTrack::ARRAYS) 
			verror("Track %s is not of %s type", track_str, GenomeTrack::TYPE_NAMES[GenomeTrack::ARRAYS]);

		if (isInteger(_slice)) {
			if (length(_slice) != length(_colnames))
				verror("Column names do not match slice indices");
			for (int islice = 0; islice < length(_slice); ++islice) 
				slice.push_back((unsigned)INTEGER(_slice)[islice] - 1);
		}

		int numcols = length(_colnames);
		vector<float> res_vals;
		GIntervalsFetcher1D *intervals = NULL;
		GIntervals out_intervals;
		vector<unsigned> interv_ids;

		iu.convert_rintervs(_intervals, &intervals, NULL);
		unique_ptr<GIntervalsFetcher1D> intervals1d_guard(intervals);
		intervals->sort();

		Progress_reporter progress;
		progress.init(intervals->size(), 1);
		int scope_idx = 0;
		vector<float> vals;
		TrackExpressionSparseIterator sparse_itr(iu, GenomeTrack::ARRAYS);
		int last_chrom = -1;
		ofstream outfile;

		if (filename) {
			outfile.open(filename);
			if (outfile.fail())
				verror("Failed to open file %s for writing: %s\n", filename, strerror(errno));
			outfile << setprecision(15);

			for (int i = 0; i < GInterval::NUM_COLS; ++i) 
				outfile << GInterval::COL_NAMES[i] << "\t";

			for (int icol = 0; icol < numcols; ++icol) {
				if (icol) 
					outfile << "\t";
				outfile << CHAR(STRING_ELT(_colnames, icol));
			}

			outfile << "\n";
		}

		vector<GIntervalsBigSet1D::ChromStat> chromstats;
		char error_prefix[1000];
		strcpy(error_prefix, "Result");

		if (!intervset_out.empty())
			GIntervalsBigSet1D::begin_save(intervset_out.c_str(), iu, chromstats);

		for (sparse_itr.begin(track_path, *intervals); !sparse_itr.isend(); sparse_itr.next()) {
			const GInterval &interval = sparse_itr.last_interval();
			GenomeTrackArrays &track = sparse_itr.get_track_arrays();

			if (last_chrom != interval.chromid) {
				track.set_slice(slice);
				last_chrom = interval.chromid;
			}

			track.get_sliced_vals(sparse_itr.get_icur_interval(), vals, numcols);
			if (vals.size() != (uint64_t)numcols) 
				verror("Number of sliced values (%ld) does not match the number of columns (%d)", vals.size(), numcols);

			progress.report(max((int64_t)(sparse_itr.get_cur_scope_idx() - scope_idx), (int64_t)0));
			check_interrupt();
			scope_idx = sparse_itr.get_cur_scope_idx();
			const GInterval &scope_interval = sparse_itr.last_scope_interval();

			if (filename) {
				outfile << iu.id2chrom(interval.chromid) << "\t" << interval.start << "\t" << interval.end;
				for (vector<float>::const_iterator ival = vals.begin(); ival != vals.end(); ++ival) 
					outfile << "\t" << *ival;
				outfile << "\n";
			} else {
				if (!intervset_out.empty() && !out_intervals.empty() && out_intervals.back().chromid != interval.chromid) {
					SEXP rintervals = build_rintervals_arrayextract(&out_intervals, res_vals, NULL, numcols, _colnames, iu);
					GIntervalsBigSet1D::save_chrom(intervset_out.c_str(), &out_intervals, rintervals, iu, chromstats);
					out_intervals.clear();
					res_vals.clear();
				} else
					interv_ids.push_back(iu.get_orig_interv_idx(scope_interval) + 1);

				if (out_intervals.empty() && !intervset_out.empty()) 
					snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chrom %s", intervset_out.c_str(), iu.id2chrom(interval.chromid).c_str());

				out_intervals.push_back(interval);
				iu.verify_max_data_size(out_intervals.size() * numcols, error_prefix);

				for (vector<float>::const_iterator ival = vals.begin(); ival != vals.end(); ++ival) 
					res_vals.push_back(*ival);
			}
		}

		progress.report_last();

		if (out_intervals.empty())
			return R_NilValue;

		if (intervset_out.empty())
			return build_rintervals_arrayextract(&out_intervals, res_vals, &interv_ids, numcols, _colnames, iu);

		SEXP rintervals = build_rintervals_arrayextract(&out_intervals, res_vals, NULL, numcols, _colnames, iu);
		GIntervalsBigSet1D::save_chrom(intervset_out.c_str(), &out_intervals, rintervals, iu, chromstats);
		out_intervals.clear();
		res_vals.clear();
		SEXP zeroline = build_rintervals_arrayextract(&out_intervals, res_vals, NULL, numcols, _colnames, iu);
		GIntervalsBigSet1D::end_save(intervset_out.c_str(), zeroline, iu, chromstats);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

}

