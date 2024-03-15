#include <cstdint>
#include "GenomeTrackComputed.h"
#include "HiCComputers.h"

#include "rdbinterval.h"
#include "rdbprogress.h"
#include "rdbutils.h"

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>

using namespace rdb;

extern "C" {

SEXP gcreate_test_computer2d_track(SEXP _track, SEXP _prob_skip_chrom, SEXP _max_rects, SEXP _max_rect_size, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_track) || length(_track) != 1)
			verror("Track argument is not a string");

		if (!isNumeric(_prob_skip_chrom) || length(_prob_skip_chrom) != 1)
			verror("Skip chromosome probability argument is not numeric");

		if (!isNumeric(_max_rects) || length(_max_rects) != 1)
			verror("Max rects argument is not numeric");

		if (!isNumeric(_max_rect_size) || length(_max_rect_size) != 1)
			verror("Max rect size argument is not numeric");

		const char *track = CHAR(STRING_ELT(_track, 0));
		const double prob_skip_chrom = REAL(_prob_skip_chrom)[0];
		const int max_rects = (int)(REAL(_max_rects)[0]);
		const int max_rect_size = (int)(REAL(_max_rect_size)[0]);

		string dirname = create_track_dir(_envir, track);
		IntervUtils iu(_envir);
		char filename[FILENAME_MAX];
		GIntervals2D all_genome_intervs2d;
		iu.get_all_genome_intervs(all_genome_intervs2d);

		Progress_reporter progress;
		progress.init(iu.get_chromkey().get_num_chroms() * iu.get_chromkey().get_num_chroms(), 1);

		for (uint64_t chromid1 = 0; chromid1 < iu.get_chromkey().get_num_chroms(); chromid1++) {
			for (uint64_t chromid2 = 0; chromid2 < iu.get_chromkey().get_num_chroms(); chromid2++) {
				if (unif_rand() < prob_skip_chrom) {
					progress.report(1);
					continue;
				}

				GenomeTrackComputed gtrack(get_groot(_envir), iu.get_track_chunk_size(), iu.get_track_num_chunks());
				snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid1, chromid2).c_str());
				gtrack.init_write(filename, chromid1, chromid2);
				gtrack.set_computer(new TestComputer2D(get_groot(_envir), chromid1, chromid2));

				int64_t maxx = iu.get_chromkey().get_chrom_size(chromid1);
				int64_t maxy = iu.get_chromkey().get_chrom_size(chromid2);
				ComputedQuadTree qtree;
				qtree.reset(0, 0, maxx, maxy);
				qtree.set_uptr(gtrack.get_computer());

				int num_rects = (int)(1 + unif_rand() * max_rects);
				for (int i = 0; i < num_rects; ++i) {
					while (1) {
						int64_t x1 = (int64_t)(unif_rand() * (maxx - 2));
						int64_t y1 = (int64_t)(unif_rand() * (maxy - 2));
						int64_t x2 = min((int64_t)(x1 + 1 + max_rect_size * unif_rand()), (int64_t)maxx);
						int64_t y2 = min((int64_t)(y1 + 1 + max_rect_size * unif_rand()), (int64_t)maxy);
						ComputedQuadTree::ValueType rect(x1, y1, x2, y2, (x1 + x2 + y1 + y2) % 10000000);
						if (!qtree.do_intersect(rect)) {
							qtree.insert(rect);
							break;
						}
					}
				}
				check_interrupt();
				gtrack.write(qtree);
				check_interrupt();
				progress.report(1);
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
