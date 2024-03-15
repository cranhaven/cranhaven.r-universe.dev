#include <cstdint>
#include "BinFinder.h"
#include "GenomeTrackFixedBin.h"

#include "rdbinterval.h"
#include "rdbutils.h"
#include "TrackExpressionFixedBinIterator.h"
#include "TrackExpressionScanner.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP gtrack_modify(SEXP _track, SEXP _track_expr, SEXP _intervals, SEXP _iterator_policy, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		// check the arguments
		if (!isString(_track) || length(_track) != 1)
			verror("Track argument is not a string");

		if (!isString(_track_expr) || length(_track_expr) != 1)
			verror("Track expression argument is not a string vector");

		IntervUtils iu(_envir);
		GIntervalsFetcher1D *intervals = NULL;
		iu.convert_rintervs(_intervals, &intervals, NULL);
		unique_ptr<GIntervalsFetcher1D> intervals1d_guard(intervals);
		intervals->sort();
		intervals->unify_overlaps(false);
		const char *track = CHAR(STRING_ELT(_track, 0));
		string trackpath = track2path(_envir, track);
		char filename[FILENAME_MAX];
		TrackExprScanner scanner(iu);
		GenomeTrackFixedBin gtrack;
		GInterval last_interval(-1, -1, -1, -1);

		if (GenomeTrack::get_type(trackpath.c_str(), iu.get_chromkey()) != GenomeTrack::FIXED_BIN)
			verror("Cannot modify track %s: modification is supported only for dense tracks", track);

		scanner.begin(_track_expr, intervals, NULL, _iterator_policy);

		if (scanner.get_iterator()->get_type() != TrackExpressionIteratorBase::FIXED_BIN)
			verror("gtrack.modify() requires the iterator policy to be fixed bin.\n");

		for (; !scanner.isend(); scanner.next()) {
			const GInterval &cur_interval = scanner.last_interval1d();

			if (last_interval.chromid != cur_interval.chromid) {
				snprintf(filename, sizeof(filename), "%s/%s", trackpath.c_str(), GenomeTrack::get_1d_filename(iu.get_chromkey(), cur_interval.chromid).c_str());
				gtrack.init_read(filename, cur_interval.chromid);
				if (gtrack.get_bin_size() != ((TrackExpressionFixedBinIterator *)scanner.get_iterator())->get_bin_size())
					verror("Cannot modify track %s: iterator policy must be set to the bin size of the track (%d).\n", track, gtrack.get_bin_size());

				gtrack.init_update(filename, cur_interval.chromid);
			}

			if (last_interval.chromid != cur_interval.chromid || last_interval.end != cur_interval.start)
				gtrack.goto_bin((uint64_t)(cur_interval.start / gtrack.get_bin_size()));

			double val = scanner.last_real(0);
			gtrack.write_next_bin(val);
			last_interval = cur_interval;
		}
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

}
