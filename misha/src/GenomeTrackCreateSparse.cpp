#include <stdio.h>

#include <set>
#include <string>
#include <vector>

#include "rdbinterval.h"
#include "rdbprogress.h"
#include "rdbutils.h"

#include "GenomeTrackSparse.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP gtrack_create_sparse(SEXP _track, SEXP _intervs, SEXP _values, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_track) || length(_track) != 1)
			verror("Track argument is not a string");

		IntervUtils iu(_envir);
		GIntervals intervs;
		iu.convert_rintervs(_intervs, &intervs, NULL);
		intervs.sort();
		intervs.verify_no_overlaps(iu.get_chromkey());

		if (!isReal(_values) && !isInteger(_values))
			verror("Values argument is not numeric");

		if (length(_values) != (int)intervs.size())
			verror("Number of intervals (%ld) does not match the number of values (%d)", intervs.size(), (int)length(_values));

		const char *track = CHAR(STRING_ELT(_track, 0));

		string dirname = create_track_dir(_envir, track);
		int cur_chromid = -1;
		set<int> created_chromids;
		GenomeTrackSparse gtrack;
		char filename[FILENAME_MAX];
		GIntervals all_genome_intervs;
		iu.get_all_genome_intervs(all_genome_intervs);

		Progress_reporter progress;
		progress.init(intervs.size(), 100000);

		for (GIntervals::const_iterator iinterv = intervs.begin(); iinterv != intervs.end(); ++iinterv) {
			if (cur_chromid != iinterv->chromid) {
				cur_chromid = iinterv->chromid;
				snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_1d_filename(iu.get_chromkey(), cur_chromid).c_str());
				gtrack.init_write(filename, cur_chromid);
				created_chromids.insert(cur_chromid);
			}

			if (isReal(_values))
				gtrack.write_next_interval(*iinterv, REAL(_values)[iu.get_orig_interv_idx(*iinterv)]);
			else
				gtrack.write_next_interval(*iinterv, INTEGER(_values)[iu.get_orig_interv_idx(*iinterv)]);

			progress.report(1);
			check_interrupt();
		}

		// some of the chromosome could be previously skipped; we still must create them even if they are empty
		for (GIntervals::const_iterator iinterv = all_genome_intervs.begin(); iinterv != all_genome_intervs.end(); ++iinterv) {
			if (created_chromids.find(iinterv->chromid) == created_chromids.end()) {
				snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_1d_filename(iu.get_chromkey(), iinterv->chromid).c_str());
				gtrack.init_write(filename, iinterv->chromid);
			}
		}

		progress.report_last();
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

}
