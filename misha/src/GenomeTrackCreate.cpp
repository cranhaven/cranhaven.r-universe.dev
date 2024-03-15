/*
 * GenomeTrackCreate.cpp
 *
 *  Created on: May 2, 2010
 *      Author: hoichman
 */

#include <stdio.h>

#include <set>
#include <string>
#include <vector>

#include "rdbinterval.h"
#include "rdbutils.h"

#include "GenomeTrack.h"
#include "GenomeTrackFixedBin.h"
#include "GenomeTrackRects.h"
#include "GenomeTrackSparse.h"
#include "TrackExpressionScanner.h"
#include "TrackExpressionIterator.h"
#include "TrackExpressionFixedBinIterator.h"
#include "TrackExpressionSparseIterator.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP gtrackcreate(SEXP track, SEXP expr, SEXP _iterator_policy, SEXP _band, SEXP envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(track) || length(track) != 1)
			verror("Track argument is not a string");

		if (!isString(expr) || length(expr) != 1)
			verror("Track expression argument is not a string");

		const char *track_str = CHAR(STRING_ELT(track, 0));

		string dirname = create_track_dir(envir, track_str);
		IntervUtils iu(envir);
		TrackExprScanner scanner(iu);
		char filename[FILENAME_MAX];
		GIntervals all_genome_intervs1d;
		GIntervals2D all_genome_intervs2d;
		iu.get_all_genome_intervs(all_genome_intervs1d);
		iu.get_all_genome_intervs(all_genome_intervs2d);

		scanner.begin(expr, &all_genome_intervs1d, &all_genome_intervs2d, _iterator_policy, _band);

		TrackExpressionIteratorBase::Type itr_type = scanner.get_iterator()->get_type();

		if (itr_type == TrackExpressionIteratorBase::FIXED_BIN) {
			int cur_chromid = -1;
			GenomeTrackFixedBin gtrack;

			for (; !scanner.isend(); scanner.next()) {
				if (cur_chromid != scanner.last_interval1d().chromid) {
					cur_chromid = scanner.last_interval1d().chromid;
					snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_1d_filename(iu.get_chromkey(), cur_chromid).c_str());
					gtrack.init_write(filename, ((TrackExpressionFixedBinIterator *)scanner.get_iterator())->get_bin_size(), cur_chromid);
				}
				gtrack.write_next_bin(scanner.last_real(0));
			}
		} else if (itr_type == TrackExpressionIteratorBase::INTERVALS1D) {
			int cur_chromid = -1;
			GenomeTrackSparse gtrack;
			set<int> created_chromids;

			for (; !scanner.isend(); scanner.next()) {
				if (cur_chromid != scanner.last_interval1d().chromid) {
					cur_chromid = scanner.last_interval1d().chromid;
					snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_1d_filename(iu.get_chromkey(), cur_chromid).c_str());
					gtrack.init_write(filename, cur_chromid);
					created_chromids.insert(cur_chromid);
				}
				gtrack.write_next_interval(scanner.last_interval1d(), scanner.last_real(0));
			}

			// some of the chromosome could be previously skipped; we still must create them even if they are empty
			for (GIntervals::const_iterator iinterv = all_genome_intervs1d.begin(); iinterv != all_genome_intervs1d.end(); ++iinterv) {
				if (created_chromids.find(iinterv->chromid) == created_chromids.end()) {
					snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_1d_filename(iu.get_chromkey(), iinterv->chromid).c_str());
					gtrack.init_write(filename, iinterv->chromid);
				}
			}
		} else if (itr_type == TrackExpressionIteratorBase::INTERVALS2D) {
			int cur_chromid1 = -1;
			int cur_chromid2 = -1;
			GenomeTrackRectsRects gtrack(iu.get_track_chunk_size(), iu.get_track_num_chunks());
			RectsQuadTree qtree;

			for (; !scanner.isend(); scanner.next()) {
				const GInterval2D &interv = scanner.last_interval2d();

				if (cur_chromid1 != interv.chromid1() || cur_chromid2 != interv.chromid2()) {
					if (gtrack.opened())
						gtrack.write(qtree);

					cur_chromid1 = interv.chromid1();
					cur_chromid2 = interv.chromid2();
					snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_2d_filename(iu.get_chromkey(), cur_chromid1, cur_chromid2).c_str());

					qtree.reset(0, 0, iu.get_chromkey().get_chrom_size(cur_chromid1), iu.get_chromkey().get_chrom_size(cur_chromid2));
					gtrack.init_write(filename, cur_chromid1, cur_chromid2);
				}

				qtree.insert(RectsQuadTree::ValueType(interv, scanner.last_real(0)));
			}

			if (gtrack.opened())
				gtrack.write(qtree);
		} else {
			if (itr_type >= 0 && itr_type < sizeof(TrackExpressionIteratorBase::TYPE_NAMES) / sizeof(TrackExpressionIteratorBase::TYPE_NAMES[0])) {
    			verror("Iterator type %s is not supported by the function", TrackExpressionIteratorBase::TYPE_NAMES[itr_type]);
			} else {
    			verror("Invalid iterator type encountered");
			}
		}
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}


SEXP gtrackcreate_multitask(SEXP track, SEXP expr, SEXP _iterator_policy, SEXP _band, SEXP envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(track) || length(track) != 1)
			verror("Track argument is not a string");

		if (!isString(expr) || length(expr) != 1)
			verror("Track expression argument is not a string");

		const char *track_str = CHAR(STRING_ELT(track, 0));

		string dirname = create_track_dir(envir, track_str);
		IntervUtils iu(envir);
		char filename[FILENAME_MAX];
		GIntervals all_genome_intervs1d;
		GIntervals2D all_genome_intervs2d;
		iu.get_all_genome_intervs(all_genome_intervs1d);
		iu.get_all_genome_intervs(all_genome_intervs2d);

		if (!iu.prepare4multitasking(expr, &all_genome_intervs1d, &all_genome_intervs2d, _iterator_policy, _band))
			rreturn(R_NilValue);

		if (iu.distribute_task(0, 0)) {  // child process
			TrackExprScanner scanner(iu);

			scanner.begin(expr, iu.get_kid_intervals1d(), iu.get_kid_intervals2d(), _iterator_policy, _band);

			TrackExpressionIteratorBase::Type itr_type = scanner.get_iterator()->get_type();

			if (itr_type == TrackExpressionIteratorBase::FIXED_BIN) {
				int cur_chromid = -1;
				GenomeTrackFixedBin gtrack;

				for (; !scanner.isend(); scanner.next()) {
                    if (cur_chromid != scanner.last_interval1d().chromid) {
                        cur_chromid = scanner.last_interval1d().chromid;
                        snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_1d_filename(iu.get_chromkey(), cur_chromid).c_str());
                        gtrack.init_write(filename, ((TrackExpressionFixedBinIterator *)scanner.get_iterator())->get_bin_size(), cur_chromid);
                    }
                    gtrack.write_next_bin(scanner.last_real(0));
				}
			} else if (itr_type == TrackExpressionIteratorBase::INTERVALS1D) {
				int cur_chromid = -1;
				GenomeTrackSparse gtrack;
				set<int> created_chromids;

				for (; !scanner.isend(); scanner.next()) {
					if (cur_chromid != scanner.last_interval1d().chromid) {
						cur_chromid = scanner.last_interval1d().chromid;
						snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_1d_filename(iu.get_chromkey(), cur_chromid).c_str());
						gtrack.init_write(filename, cur_chromid);
						created_chromids.insert(cur_chromid);
					}
					gtrack.write_next_interval(scanner.last_interval1d(), scanner.last_real(0));
				}

				// some of the chromosome could be previously skipped; we still must create them even if they are empty
				GIntervals *kid_intervals1d = (GIntervals *)iu.get_kid_intervals1d();
				for (GIntervals::const_iterator iinterv = kid_intervals1d->begin(); iinterv != kid_intervals1d->end(); ++iinterv) {
					if (created_chromids.find(iinterv->chromid) == created_chromids.end()) {
						snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_1d_filename(iu.get_chromkey(), iinterv->chromid).c_str());
						gtrack.init_write(filename, iinterv->chromid);
					}
				}
			} else if (itr_type == TrackExpressionIteratorBase::INTERVALS2D) {
				int cur_chromid1 = -1;
				int cur_chromid2 = -1;
				GenomeTrackRectsRects gtrack(iu.get_track_chunk_size(), iu.get_track_num_chunks());
				RectsQuadTree qtree;

				for (; !scanner.isend(); scanner.next()) {
					const GInterval2D &interv = scanner.last_interval2d();

					if (cur_chromid1 != interv.chromid1() || cur_chromid2 != interv.chromid2()) {
						if (gtrack.opened())
							gtrack.write(qtree);

						cur_chromid1 = interv.chromid1();
						cur_chromid2 = interv.chromid2();
						snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_2d_filename(iu.get_chromkey(), cur_chromid1, cur_chromid2).c_str());

						qtree.reset(0, 0, iu.get_chromkey().get_chrom_size(cur_chromid1), iu.get_chromkey().get_chrom_size(cur_chromid2));
						gtrack.init_write(filename, cur_chromid1, cur_chromid2);
					}

					qtree.insert(RectsQuadTree::ValueType(interv, scanner.last_real(0)));
				}

				if (gtrack.opened())
					gtrack.write(qtree);
			} else {
				if (itr_type >= 0 && itr_type < sizeof(TrackExpressionIteratorBase::TYPE_NAMES) / sizeof(TrackExpressionIteratorBase::TYPE_NAMES[0])) {
    				verror("Iterator type %s is not supported by the function", TrackExpressionIteratorBase::TYPE_NAMES[itr_type]);
				} else {
    				verror("Invalid iterator type encountered");
				}
			}				
		}
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	rreturn(R_NilValue);
}

}
