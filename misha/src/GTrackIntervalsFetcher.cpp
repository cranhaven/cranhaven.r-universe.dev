#include <cstdint>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "GenomeTrackArrays.h"
#include "GenomeTrackComputed.h"
#include "GenomeTrackRects.h"
#include "GenomeTrackSparse.h"
#include "GIntervalsMeta2D.h"
#include "GTrackIntervalsFetcher.h"
#include "GIntervalsBigSet1D.h"
#include "GIntervalsBigSet2D.h"
#include "rdbprogress.h"
#include "rdbutils.h"

bool GTrackIntervalsFetcher::isbig(const char *track_name, const IntervUtils &iu)
{
	string path = interv2path(iu.get_env(), track_name);
	SEXP gtracks;

	rprotect(gtracks = findVar(install("GTRACKS"), findVar(install(".misha"), iu.get_env())));
	for (int itrack = 0; itrack < length(gtracks); ++itrack) {
		const char *track = CHAR(STRING_ELT(gtracks, itrack));
		if (!strcmp(track_name, track))
			return true;
	}
	return false;
}

void GTrackIntervalsFetcher::init(const char *track_name, const IntervUtils &iu)
{
	m_track_name = track_name;
	m_iu = (IntervUtils *)&iu;
}

void GTrackIntervalsFetcher::create_track_meta(const char *track_name, const IntervUtils &iu)
{
	string trackpath(track2path(iu.get_env(), track_name));
	GenomeTrack::Type track_type = GenomeTrack::get_type(trackpath.c_str(), iu.get_chromkey(), true);

	if (track_type == GenomeTrack::FIXED_BIN)
		verror("Track %s is of type %s which cannot be used as a substitute for an intervals set", track_name, GenomeTrack::TYPE_NAMES[track_type]);

	REprintf("Preparing the track %s to be used as an intervals set...\n", track_name);

	if (track_type == GenomeTrack::SPARSE || track_type == GenomeTrack::ARRAYS) {
		vector<GIntervalsMeta1D::ChromStat> chromstats(iu.get_chromkey().get_num_chroms());
		Progress_reporter progress;
		progress.init(iu.get_chromkey().get_num_chroms(), 1);

		for (uint64_t chromid = 0; chromid < iu.get_chromkey().get_num_chroms(); chromid++) {
			string filename(trackpath + "/" + GenomeTrack::get_1d_filename(iu.get_chromkey(), chromid));

			if (access(filename.c_str(), R_OK) && errno == ENOENT) {
				progress.report(1);
				continue;
			}

			if (track_type == GenomeTrack::SPARSE) {
				GenomeTrackSparse track;
				track.init_read(filename.c_str(), chromid);
				GIntervals intervals(track.get_intervals());
				chromstats[chromid] = GIntervalsBigSet1D::get_chrom_stat(&intervals).second;
			} else if (track_type == GenomeTrack::ARRAYS) {
				GenomeTrackArrays track;
				track.init_read(filename.c_str(), chromid);
				GIntervals intervals(track.get_intervals());
				chromstats[chromid] = GIntervalsBigSet1D::get_chrom_stat(&intervals).second;
			}

			progress.report(1);
			check_interrupt();
		}
		GIntervalsMeta1D::save_plain_intervals_meta(trackpath.c_str(), chromstats, iu);
		progress.report_last();
	} else if (track_type == GenomeTrack::RECTS || track_type == GenomeTrack::POINTS || track_type == GenomeTrack::COMPUTED) {
		vector<GIntervalsMeta2D::ChromStat> chromstats(iu.get_chromkey().get_num_chroms() * iu.get_chromkey().get_num_chroms());
		BufferedFile bfile;
		Progress_reporter progress;
		progress.init(iu.get_chromkey().get_num_chroms() * iu.get_chromkey().get_num_chroms(), 1);

		for (uint64_t chromid1 = 0; chromid1 < iu.get_chromkey().get_num_chroms(); chromid1++) {
			for (uint64_t chromid2 = 0; chromid2 < iu.get_chromkey().get_num_chroms(); chromid2++) {
				string filename(trackpath + "/" + GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid1, chromid2));
				unique_ptr<GenomeTrack2D> track;

				if (track_type == GenomeTrack::RECTS) {
					track = unique_ptr<GenomeTrack2D>(new GenomeTrackRectsRects(iu.get_track_chunk_size(), iu.get_track_num_chunks()));
					((GenomeTrackRectsRects *)track.get())->init_read(filename.c_str(), chromid1, chromid2);
				} else if (track_type == GenomeTrack::POINTS) {
					track = unique_ptr<GenomeTrack2D>(new GenomeTrackRectsPoints(iu.get_track_chunk_size(), iu.get_track_num_chunks()));
					((GenomeTrackRectsPoints *)track.get())->init_read(filename.c_str(), chromid1, chromid2);
				} else if (track_type == GenomeTrack::COMPUTED) {
					track = unique_ptr<GenomeTrack2D>(new GenomeTrackComputed(get_groot(iu.get_env()), iu.get_track_chunk_size(), iu.get_track_num_chunks()));
					((GenomeTrackComputed *)track.get())->init_read(filename.c_str(), chromid1, chromid2);
				}

				track->read_interval(GInterval2D(chromid1, 0, iu.get_chromkey().get_chrom_size(chromid1),
												 chromid2, 0, iu.get_chromkey().get_chrom_size(chromid2)),
									 DiagonalBand());

				int idx = chromid1 * iu.get_chromkey().get_num_chroms() + chromid2;

				chromstats[idx].contains_overlaps = false;

				if (track_type == GenomeTrack::RECTS)
					chromstats[idx].size = ((GenomeTrackRectsRects *)track.get())->get_qtree().get_num_objs();
				else if (track_type == GenomeTrack::POINTS)
					chromstats[idx].size = ((GenomeTrackRectsPoints *)track.get())->get_qtree().get_num_objs();
				else if (track_type == GenomeTrack::COMPUTED)
					chromstats[idx].size = ((GenomeTrackComputed *)track.get())->get_qtree().get_num_objs();

				chromstats[idx].surface = track->last_occupied_area();
				progress.report(1);
				check_interrupt();
			}
		}

		GIntervalsMeta2D::save_plain_intervals_meta(trackpath.c_str(), chromstats, iu);
		progress.report_last();
	}
	REprintf("Track %s is modified and ready to be used as an intervals set.\n", track_name);
}
