#include <algorithm>
#include <iterator>

#include "BinFinder.h"
#include "RandomShuffle.h"
#include "rdbinterval.h"
#include "rdbutils.h"
#include "TrackExpressionScanner.h"

using namespace std;
using namespace rdb;

typedef iterator_traits<double *> traits;

extern "C" {

SEXP C_gsample(SEXP _expr, SEXP _num_samples, SEXP _intervals, SEXP _iterator_policy, SEXP _band, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		// check the arguments
		if (!isString(_expr) || length(_expr) != 1)
			verror("Expression argument is not a string");

		if ((!isReal(_num_samples) && !isInteger(_num_samples)) || length(_num_samples) != 1)
			verror("Number of samples argument must be a number");

		if (isReal(_num_samples) && (double)REAL(_num_samples)[0] != (double)(int)REAL(_num_samples)[0]) 
			verror("Number of samples is not an integer");

		int num_samples = isReal(_num_samples) ? (int)REAL(_num_samples)[0] : INTEGER(_num_samples)[0];

		if (num_samples < 1) 
			verror("Number of samples must be greater than zero");

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
		StreamSampler<double> sampler(num_samples, true);

		for (scanner.begin(_expr, intervals1d, intervals2d, _iterator_policy, _band); !scanner.isend(); scanner.next()) {
			sampler.add(scanner.last_real(0), unif_rand);
			iu.verify_max_data_size(sampler.samples().size(), "Result");
		}

		if (!sampler.samples().size()) 
			return R_NilValue;

		SEXP answer;

		rprotect(answer = RSaneAllocVector(REALSXP, sampler.samples().size()));
		double *vals = REAL(answer);

		for (vector<double>::const_iterator ival = sampler.samples().begin(); ival != sampler.samples().end(); ++ival) {
			*vals = *ival;
			++vals;
		}

		// The samples need to be reshuffled since sampler does not guarantee random order.
        tgs_random_shuffle(REAL(answer), REAL(answer) + sampler.samples().size(), unif_rand);

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

}

