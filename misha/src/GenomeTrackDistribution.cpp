/*
 * GenomeTrackDistribution.cpp
 *
 *  Created on: Mar 26, 2010
 *      Author: hoichman
 */

#include <cstdint>
#include "rdbinterval.h"
#include "rdbutils.h"
#include "BinsManager.h"
#include "TrackExpressionScanner.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP gtrackdist(SEXP _intervals, SEXP _track_exprs, SEXP _breaks, SEXP _include_lowest, SEXP _iterator_policy, SEXP _band, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		// check the arguments
		if (!isString(_track_exprs) || length(_track_exprs) < 1)
			verror("Track argument is not a string vector");

		unsigned numexpr = length(_track_exprs);
		BinsManager bins_manager(_breaks, _include_lowest);

		if (bins_manager.get_num_bin_finders() != numexpr)
			verror("Number of breaks sets must be equal to the number of tracks used");

		IntervUtils iu(_envir);
		TrackExprScanner scanner(iu);
		unsigned totalbins = bins_manager.get_total_bins();
		iu.verify_max_data_size(totalbins, "Result");
		vector<uint64_t> distribution(totalbins, 0);
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
			for (unsigned i = 0; i < numexpr; ++i)
				vals[i] = scanner.last_real(i);

			int index = bins_manager.vals2idx(vals);

			if (index >= 0)
				distribution[index]++;
		}

		// pack the answer
		SEXP answer, dim, dimnames;
		rprotect(answer = RSaneAllocVector(REALSXP, totalbins));
		double *panswer = REAL(answer);

		for (unsigned i = 0; i < totalbins; i++)
			panswer[i] = distribution[i];

		rprotect(dim = RSaneAllocVector(INTSXP, numexpr));
		rprotect(dimnames = RSaneAllocVector(VECSXP, numexpr));
		bins_manager.set_dims(dim, dimnames);
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

SEXP gtrackdist_multitask(SEXP _intervals, SEXP _track_exprs, SEXP _breaks, SEXP _include_lowest, SEXP _iterator_policy, SEXP _band, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		// check the arguments
		if (!isString(_track_exprs) || length(_track_exprs) < 1)
			verror("Track argument is not a string vector");

		unsigned numexpr = length(_track_exprs);
		BinsManager bins_manager(_breaks, _include_lowest);

		if (bins_manager.get_num_bin_finders() != numexpr)
			verror("Number of breaks sets must be equal to the number of tracks used");

		IntervUtils iu(_envir);
		unsigned totalbins = bins_manager.get_total_bins();
		iu.verify_max_data_size(totalbins, "Result");
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

		if (!iu.prepare4multitasking(_track_exprs, intervals1d, intervals2d, _iterator_policy, _band))
			rreturn(R_NilValue);

		if (iu.distribute_task(totalbins * sizeof(uint64_t), 0)) { // child process
			uint64_t *distribution = (uint64_t *)allocate_res(0);
			memset(distribution, 0, totalbins * sizeof(uint64_t));

			TrackExprScanner scanner(iu);

			for (scanner.begin(_track_exprs, iu.get_kid_intervals1d(), iu.get_kid_intervals2d(), _iterator_policy, _band); !scanner.isend(); scanner.next()) {
				for (unsigned i = 0; i < numexpr; ++i)
					vals[i] = scanner.last_real(i);

				int index = bins_manager.vals2idx(vals);

				if (index >= 0)
					distribution[index]++;
			}
		} else { // parent process
			// collect results from kids
			vector<uint64_t> distribution(totalbins, 0);
			for (int i = 0; i < get_num_kids(); ++i) {
				uint64_t *kid_distribution = (uint64_t *)get_kid_res(i);

				for (uint64_t ibin = 0; ibin < totalbins; ++ibin) 
					distribution[ibin] += kid_distribution[ibin];
			}

			// pack the answer
			SEXP answer, dim, dimnames;
			rprotect(answer = RSaneAllocVector(REALSXP, totalbins));
			double *panswer = REAL(answer);

			for (unsigned i = 0; i < totalbins; i++)
				panswer[i] = distribution[i];

			rprotect(dim = RSaneAllocVector(INTSXP, numexpr));
			rprotect(dimnames = RSaneAllocVector(VECSXP, numexpr));
			bins_manager.set_dims(dim, dimnames);
			setAttrib(answer, R_DimSymbol, dim);
			setAttrib(answer, R_DimNamesSymbol, dimnames);
			return answer;
		}
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	rreturn(R_NilValue);
}

}
