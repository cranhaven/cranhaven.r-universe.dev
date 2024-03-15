#include "rdbinterval.h"
#include "rdbprogress.h"
#include "rdbutils.h"

#include "GenomeTrackRects.h"
#include "GenomeTrackSparse.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP gtrack_create_track2d(SEXP _track, SEXP _intervs, SEXP _values, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_track) || length(_track) != 1)
			verror("Track argument is not a string");

		IntervUtils iu(_envir);
		GIntervals2D intervs;
		iu.convert_rintervs(_intervs, NULL, &intervs);
		intervs.sort();
		intervs.verify_no_overlaps(iu.get_chromkey());

		if (!isReal(_values) && !isInteger(_values))
			verror("Values argument is not numeric");

		if (length(_values) != (int)intervs.size())
			verror("Number of intervals (%ld) does not match the number of values (%d)", intervs.size(), (int)length(_values));

		const char *track = CHAR(STRING_ELT(_track, 0));

		string dirname = create_track_dir(_envir, track);
		int cur_chromid1 = -1;
		int cur_chromid2 = -1;
		char filename[FILENAME_MAX];

		Progress_reporter progress;
		progress.init(intervs.size(), 100000);

		GenomeTrackRectsRects gtrack(iu.get_track_chunk_size(), iu.get_track_num_chunks());
		RectsQuadTree qtree;

		for (GIntervals2D::const_iterator iinterv = intervs.begin(); iinterv != intervs.end(); ++iinterv) {
			if (cur_chromid1 != iinterv->chromid1() || cur_chromid2 != iinterv->chromid2()) {
				if (gtrack.opened())
					gtrack.write(qtree);

				cur_chromid1 = iinterv->chromid1();
				cur_chromid2 = iinterv->chromid2();
				snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_2d_filename(iu.get_chromkey(), cur_chromid1, cur_chromid2).c_str());

				qtree.reset(0, 0, iu.get_chromkey().get_chrom_size(cur_chromid1), iu.get_chromkey().get_chrom_size(cur_chromid2));
				gtrack.init_write(filename, cur_chromid1, cur_chromid2);
			}

			float val = isReal(_values) ? REAL(_values)[iu.get_orig_interv_idx(*iinterv)] : INTEGER(_values)[iu.get_orig_interv_idx(*iinterv)];
			qtree.insert(RectsQuadTree::ValueType(*iinterv, val));

			progress.report(1);
		}

		if (gtrack.opened())
			gtrack.write(qtree);

		progress.report_last();
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

}
