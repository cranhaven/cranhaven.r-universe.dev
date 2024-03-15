#include <cstdint>
#include "rdbinterval.h"
#include "rdbprogress.h"
#include "rdbutils.h"

#include "GenomeTrackComputed.h"
#include "GenomeTrackRects.h"
#include "GTrackIntervalsFetcher.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP gtrackconvert(SEXP _src_track, SEXP _tgt_track, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_src_track) || length(_src_track) != 1)
			verror("Source track argument is not a string");

		if (!isString(_tgt_track) || length(_tgt_track) != 1)
			verror("Target track argument is not a string");

		const char *src_track_str = CHAR(STRING_ELT(_src_track, 0));
		const char *tgt_track_str = CHAR(STRING_ELT(_tgt_track, 0));
		IntervUtils iu(_envir);
		string src_trackpath(track2path(_envir, src_track_str));
		string tgt_trackpath = create_track_dir(_envir, tgt_track_str);
		GenomeTrack::Type track_type = GenomeTrack::get_type(src_trackpath.c_str(), iu.get_chromkey(), true);

		if (track_type == GenomeTrack::FIXED_BIN || track_type == GenomeTrack::SPARSE  || track_type == GenomeTrack::ARRAYS ||
			track_type == GenomeTrack::RECTS || track_type == GenomeTrack::POINTS || track_type == GenomeTrack::COMPUTED)
			verror("Track %s does not require conversion", src_track_str);

		BufferedFile bfile;
		Progress_reporter progress;
		progress.init(iu.get_chromkey().get_num_chroms() * iu.get_chromkey().get_num_chroms(), 1);

		for (uint64_t chromid1 = 0; chromid1 < iu.get_chromkey().get_num_chroms(); chromid1++) {
			for (uint64_t chromid2 = 0; chromid2 < iu.get_chromkey().get_num_chroms(); chromid2++) {
				if (!bfile.open((src_trackpath + "/" + GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid1, chromid2)).c_str(), "rb")) {
					int format_signature;

					if (bfile.read(&format_signature, sizeof(format_signature)) != sizeof(format_signature)) {
						if (bfile.error())
							verror("Opening a track file %s: %s", bfile.file_name().c_str(), strerror(errno));
						verror("Invalid format of the track file %s", bfile.file_name().c_str());
					}

					if (format_signature != GenomeTrack::FORMAT_SIGNATURES[track_type])
						verror("Invalid format of track file %s", bfile.file_name().c_str());

					if (track_type == GenomeTrack::OLD_RECTS1) {
						RectsQuadTree qtree2;
						{
							StatQuadTree<Rectangle_val<float>, unsigned> qtree1;
							qtree1.unserialize(bfile);

							const Rectangle &arena = qtree1.get_arena();
							qtree2.init(arena.x1, arena.y1, arena.x2, arena.y2, qtree1.get_max_depth(), qtree1.get_max_node_objs());
							check_interrupt();

							const vector< Rectangle_val<float> > &objs = qtree1.get_objs();
							for (vector< Rectangle_val<float> >::const_iterator iobj = objs.begin(); iobj != objs.end(); ++iobj) 
								qtree2.insert(*iobj);
						}

						check_interrupt();

						GenomeTrackRectsRects gtrack(iu.get_track_chunk_size(), iu.get_track_num_chunks());
						gtrack.init_write((tgt_trackpath + "/" + GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid1, chromid2)).c_str(), chromid1, chromid2);
						gtrack.write(qtree2);
					} else if (track_type == GenomeTrack::OLD_RECTS2) {
						RectsQuadTree qtree2;
						{
							StatQuadTreeCached<Rectangle_val<float>, unsigned> qtree1;
							qtree1.unserialize(bfile);

							const Rectangle &arena = qtree1.get_arena();
							qtree2.init(arena.x1, arena.y1, arena.x2, arena.y2);

							StatQuadTreeCached<Rectangle_val<float>, unsigned>::Iterator icqtree(&qtree1);
							for (icqtree.begin(); !icqtree.is_end(); icqtree.next()) {
								qtree2.insert(*icqtree);
								check_interrupt();
							}
						}

						GenomeTrackRectsRects gtrack(iu.get_track_chunk_size(), iu.get_track_num_chunks());
						gtrack.init_write((tgt_trackpath + "/" + GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid1, chromid2)).c_str(), chromid1, chromid2);
						gtrack.write(qtree2);
					} else if (track_type == GenomeTrack::OLD_COMPUTED1) {
						Computer2D *computer = Computer2D::unserializeComputer2D(bfile, get_groot(_envir), chromid1, chromid2);
						ComputedQuadTree qtree2;
						qtree2.set_uptr(computer);

						{
							StatQuadTree<Computed_val<double>, unsigned> qtree1;
							qtree1.unserialize(bfile);

							const Rectangle &arena = qtree1.get_arena();
							qtree2.init(arena.x1, arena.y1, arena.x2, arena.y2, qtree1.get_max_depth(), qtree1.get_max_node_objs());

							const vector< Computed_val<double> > &objs = qtree1.get_objs();
							for (vector< Computed_val<double> >::const_iterator iobj = objs.begin(); iobj != objs.end(); ++iobj)
								qtree2.insert(Computed_val<float>(iobj->x1, iobj->y1, iobj->x2, iobj->y2, (float)iobj->v));
						}

						check_interrupt();

						GenomeTrackComputed gtrack(get_groot(_envir), iu.get_track_chunk_size(), iu.get_track_num_chunks());
						gtrack.set_computer(computer);
						gtrack.init_write((tgt_trackpath + "/" + GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid1, chromid2)).c_str(), chromid1, chromid2);
						gtrack.get_qtree().init(iu.get_track_chunk_size(), iu.get_track_num_chunks());
						gtrack.write(qtree2);
					} else if (track_type == GenomeTrack::OLD_COMPUTED2) {
						Computer2D *computer = Computer2D::unserializeComputer2D(bfile, get_groot(_envir), chromid1, chromid2);
						ComputedQuadTree qtree2;
						qtree2.set_uptr(computer);

						{
							StatQuadTreeCached<Computed_val<double>, unsigned> qtree1;
							qtree1.unserialize(bfile);

							Rectangle arena = qtree1.get_arena();
							qtree2.init(arena.x1, arena.y1, arena.x2, arena.y2);

							StatQuadTreeCached< Computed_val<double>, unsigned >::Iterator icqtree(&qtree1);
							for (icqtree.begin(); !icqtree.is_end(); icqtree.next()) {
								const Computed_val<double> &val = *icqtree;
								qtree2.insert(Computed_val<float>(val.x1, val.y1, val.x2, val.y2, (float)val.v));
								check_interrupt();
							}
						}

						GenomeTrackComputed gtrack(get_groot(_envir), iu.get_track_chunk_size(), iu.get_track_num_chunks());
						gtrack.set_computer(computer);
						gtrack.init_write((tgt_trackpath + "/" + GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid1, chromid2)).c_str(), chromid1, chromid2);
						gtrack.get_qtree().init(iu.get_track_chunk_size(), iu.get_track_num_chunks());
						gtrack.write(qtree2);
					}  else if (track_type == GenomeTrack::OLD_COMPUTED3) {
						Computer2D *computer = Computer2D::unserializeComputer2D(bfile, get_groot(_envir), chromid1, chromid2);
						ComputedQuadTree qtree2;
						qtree2.set_uptr(computer);

						{
							StatQuadTreeCached<Computed_val<float>, unsigned> qtree1;
							qtree1.unserialize(bfile);

							Rectangle arena = qtree1.get_arena();
							qtree2.init(arena.x1, arena.y1, arena.x2, arena.y2);

							StatQuadTreeCached< Computed_val<float>, unsigned >::Iterator icqtree(&qtree1);
							for (icqtree.begin(); !icqtree.is_end(); icqtree.next()) {
								const Computed_val<float> &val = *icqtree;
								qtree2.insert(Computed_val<float>(val.x1, val.y1, val.x2, val.y2, (float)val.v));
								check_interrupt();
							}
						}

						GenomeTrackComputed gtrack(get_groot(_envir), iu.get_track_chunk_size(), iu.get_track_num_chunks());
						gtrack.set_computer(computer);
						gtrack.init_write((tgt_trackpath + "/" + GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid1, chromid2)).c_str(), chromid1, chromid2);
						gtrack.get_qtree().init(iu.get_track_chunk_size(), iu.get_track_num_chunks());
						gtrack.write(qtree2);
					} else
						verror("Unrecognized format of track %s", src_track_str);
				}

				progress.report(1);
				check_interrupt();
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

SEXP gtrack_create_meta(SEXP _track, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_track) || length(_track) != 1)
			verror("Track argument is not a string");

		const char *track_str = CHAR(STRING_ELT(_track, 0));
		IntervUtils iu(_envir);
		GTrackIntervalsFetcher::create_track_meta(track_str, iu);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

}
