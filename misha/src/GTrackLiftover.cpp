#include <cstdint>
#include "port.h"

#include <cmath>

#include "rdbinterval.h"
#include "rdbprogress.h"
#include "rdbutils.h"

#include "GenomeTrackFixedBin.h"
#include "GenomeTrackRects.h"
#include "GenomeTrackSparse.h"

using namespace std;
using namespace rdb;

//----------------------------------------- BufferedIntervals ---------------------------------------------

class BufferedIntervals {
public:
	BufferedIntervals() : m_last_val(numeric_limits<float>::quiet_NaN()) {}
	BufferedIntervals(const BufferedIntervals &) : m_last_val(numeric_limits<float>::quiet_NaN()) {}

	~BufferedIntervals() { close(); }

	void open(const char *path, const char *mode, int chromid) {
		if (m_bfile.open(path, mode))
			TGLError("Opening file %s: %s", path, strerror(errno));
		m_last_interval.chromid = chromid;
	}

	void close() {
		flush();
		m_bfile.close();
	}

	void write_interval(const GInterval &interval, float val) {
		if (((std::isnan(val) && std::isnan(m_last_val)) || val == m_last_val) && m_last_interval.end == interval.start)
			m_last_interval.end = interval.end;
		else {
			flush();
			m_last_interval = interval;
			m_last_val = val;
		}
	}

	bool read_interval() {
		if (m_bfile.read(&m_last_interval.start, sizeof(int64_t)) != sizeof(int64_t) ||
				m_bfile.read(&m_last_interval.end, sizeof(int64_t)) != sizeof(int64_t) ||
				m_bfile.read(&m_last_val, sizeof(float)) != sizeof(float))
		{
			if (m_bfile.eof())
				return false;
			if (m_bfile.error())
				TGLError("Failed to read a file %s: %s", m_bfile.file_name().c_str(), strerror(errno));
			TGLError("Invalid format of a file %s", m_bfile.file_name().c_str());
		}

		if (isinf(m_last_val))
			m_last_val = numeric_limits<float>::quiet_NaN();
		return true;
	}

	void flush() {
		if (m_last_interval.start != -1)
			write_last_interval();
	}

	int seek(long offset, int whence) { return m_bfile.seek(offset, whence); }

	const string &file_name() { return m_bfile.file_name(); }

	bool opened() const { return m_bfile.opened(); }

	int chromid() const { return m_last_interval.chromid; }

	const GInterval &last_interval() const { return m_last_interval; }
	float last_val() const { return m_last_val; }

private:
	BufferedFile m_bfile;
	GInterval    m_last_interval;
	float        m_last_val;

	void write_last_interval() {
		static const int RECORD_SIZE = sizeof(m_last_interval.start) + sizeof(m_last_interval.end) + sizeof(m_last_val);

		uint64_t size = 0;
		size += m_bfile.write(&m_last_interval.start, sizeof(m_last_interval.start));
		size += m_bfile.write(&m_last_interval.end, sizeof(m_last_interval.end));
		size += m_bfile.write(&m_last_val, sizeof(m_last_val));

		if ((int)size != RECORD_SIZE) {
			if (m_bfile.error())
				TGLError("Failed to write intervals to file %s: %s", m_bfile.file_name().c_str(), strerror(errno));
			TGLError("Failed to write intervals to file %s", m_bfile.file_name().c_str());
		}

		m_last_interval.start = -1;
	}
};

//----------------------------------------- BufferedIntervals2D ---------------------------------------------

class BufferedIntervals2D {
public:
	BufferedIntervals2D() : m_last_val(numeric_limits<float>::quiet_NaN()) {}
	BufferedIntervals2D(const BufferedIntervals2D &) : m_last_val(numeric_limits<float>::quiet_NaN()) {}

	~BufferedIntervals2D() { close(); }

	void open(const char *path, const char *mode, int chromid1, int chromid2) {
		if (m_bfile.open(path, mode))
			TGLError("Opening file %s: %s", path, strerror(errno));
		m_last_interval.chromid1() = chromid1;
		m_last_interval.chromid2() = chromid2;
	}

	void close() {
		m_bfile.close();
	}

	void write_interval(const GInterval &interval1, const GInterval &interval2, float val) {
		static const int RECORD_SIZE = sizeof(interval1.start) + sizeof(interval2.start) + sizeof(interval1.end) + sizeof(interval2.end) + sizeof(val);

		uint64_t size = 0;
		size += m_bfile.write(&interval1.start, sizeof(interval1.start));
		size += m_bfile.write(&interval1.end, sizeof(interval1.end));
		size += m_bfile.write(&interval2.start, sizeof(interval2.start));
		size += m_bfile.write(&interval2.end, sizeof(interval2.end));
		size += m_bfile.write(&val, sizeof(val));

		if ((int)size != RECORD_SIZE) {
			if (m_bfile.error())
				TGLError("Failed to write intervals to file %s: %s", m_bfile.file_name().c_str(), strerror(errno));
			TGLError("Failed to write intervals to file %s", m_bfile.file_name().c_str());
		}
	}

	bool read_interval() {
		if (m_bfile.read(&m_last_interval.start1(), sizeof(int64_t)) != sizeof(int64_t) ||
				m_bfile.read(&m_last_interval.end1(), sizeof(int64_t)) != sizeof(int64_t) ||
				m_bfile.read(&m_last_interval.start2(), sizeof(int64_t)) != sizeof(int64_t) ||
				m_bfile.read(&m_last_interval.end2(), sizeof(int64_t)) != sizeof(int64_t) ||
				m_bfile.read(&m_last_val, sizeof(float)) != sizeof(float))
		{
			if (m_bfile.eof())
				return false;
			if (m_bfile.error())
				TGLError("Failed to read a file %s: %s", m_bfile.file_name().c_str(), strerror(errno));
			TGLError("Invalid format of a file %s", m_bfile.file_name().c_str());
		}

		if (isinf(m_last_val))
			m_last_val = numeric_limits<float>::quiet_NaN();
		return true;
	}

	void flush() {}

	int seek(long offset, int whence) { return m_bfile.seek(offset, whence); }

	const string &file_name() { return m_bfile.file_name(); }

	bool opened() const { return m_bfile.opened(); }

	int chromid1() const { return m_last_interval.chromid1(); }
	int chromid2() const { return m_last_interval.chromid2(); }

	const GInterval2D &last_interval() const { return m_last_interval; }
	float last_val() const { return m_last_val; }

private:
	BufferedFile m_bfile;
	GInterval2D  m_last_interval;
	float        m_last_val;
};

struct GIntervalVal {
	GInterval interval;
	float     val;

	GIntervalVal(const GInterval &_interval, float _val) : interval(_interval), val(_val) {}
	bool operator<(const GIntervalVal &obj) const { return interval.start < obj.interval.start; }
};

extern "C" {

SEXP gtrack_liftover(SEXP _track, SEXP _src_track_dir, SEXP _chain, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_track) || length(_track) != 1)
			verror("Track argument is not a string");

		if (!isString(_src_track_dir) || length(_src_track_dir) != 1)
			verror("Track source directory argument is not a string");

		const char *track = CHAR(STRING_ELT(_track, 0));
		const char *src_track_dir = CHAR(STRING_ELT(_src_track_dir, 0));

		IntervUtils iu(_envir);
		ChainIntervals chain_intervs;
		vector<string> src_id2chrom;

		iu.convert_rchain_intervs(_chain, chain_intervs, src_id2chrom);
		chain_intervs.sort_by_tgt();
		chain_intervs.verify_no_tgt_overlaps(iu.get_chromkey(), src_id2chrom);
		chain_intervs.sort_by_src();
		chain_intervs.verify_no_src_overlaps(iu.get_chromkey(), src_id2chrom);

		GenomeChromKey src_chromkey;
		for (vector<string>::const_iterator ichrom = src_id2chrom.begin(); ichrom != src_id2chrom.end(); ++ichrom)
			src_chromkey.add_chrom(*ichrom, numeric_limits<int64_t>::max());

		string dirname = create_track_dir(_envir, track);

		GenomeTrack::Type src_track_type = GenomeTrack::get_type(src_track_dir, src_chromkey);

		if (GenomeTrack::is_1d(src_track_type)) {
			GIntervals all_genome_intervs;
			iu.get_all_genome_intervs(all_genome_intervs);
			vector<BufferedIntervals> buffered_intervs(iu.get_chromkey().get_num_chroms());
			char filename[FILENAME_MAX];
			GIntervals tgt_intervals;
			unsigned binsize = 0;

			Progress_reporter progress;
			progress.init(src_id2chrom.size() + iu.get_chromkey().get_num_chroms(), 1);

			// create temporary files that contain unsorted target intervals and corresponding values
			for (vector<BufferedIntervals>::iterator ibuffered_interv = buffered_intervs.begin(); ibuffered_interv != buffered_intervs.end(); ++ibuffered_interv) {
				int chromid = ibuffered_interv - buffered_intervs.begin();
				snprintf(filename, sizeof(filename), "%s/_%s", dirname.c_str(), iu.get_chromkey().id2chrom(chromid).c_str());
				ibuffered_interv->open(filename, "w+", chromid);
			}

			// write the target intervals + values to the temporary files
			if (src_track_type == GenomeTrack::FIXED_BIN) {
				for (vector<string>::const_iterator ichrom = src_id2chrom.begin(); ichrom != src_id2chrom.end(); ++ichrom) {
					GenomeTrackFixedBin src_track;
					int chromid = ichrom - src_id2chrom.begin();
					float val;

					try {
						snprintf(filename, sizeof(filename), "%s/%s", src_track_dir, ichrom->c_str());
						src_track.init_read(filename, chromid);
						if (binsize > 0 && binsize != src_track.get_bin_size()) {
							char filename2[FILENAME_MAX];
							snprintf(filename2, sizeof(filename2), "%s/%s", src_track_dir, (ichrom - 1)->c_str());
							TGLError("Binsize of track file %s differs from the binsize of track file %s (%d vs. %d)",
									filename, filename2, src_track.get_bin_size(), binsize);
						} else
							binsize = src_track.get_bin_size();
					} catch (TGLException &) {  // some of source chroms might be missing, this is normal
						progress.report(1);
						continue;
					}

					GInterval src_interval(chromid, 0, src_track.get_bin_size(), 0);
					ChainIntervals::const_iterator hint = chain_intervs.begin();

					for (int64_t i = 0; i < src_track.get_num_samples(); ++i) {
						src_track.read_next_bin(val);

						hint = chain_intervs.map_interval(src_interval, tgt_intervals, hint);
						for (GIntervals::const_iterator iinterv = tgt_intervals.begin(); iinterv != tgt_intervals.end(); ++iinterv)
							buffered_intervs[iinterv->chromid].write_interval(*iinterv, val);

						src_interval.start += src_track.get_bin_size();
						src_interval.end += src_track.get_bin_size();
						check_interrupt();
					}

					progress.report(1);
				}
			} else if (src_track_type == GenomeTrack::SPARSE) {
				for (vector<string>::const_iterator ichrom = src_id2chrom.begin(); ichrom != src_id2chrom.end(); ++ichrom) {
					GenomeTrackSparse src_track;
					int chromid = ichrom - src_id2chrom.begin();

					try {
						snprintf(filename, sizeof(filename), "%s/%s", src_track_dir, ichrom->c_str());
						src_track.init_read(filename, chromid);
					} catch (TGLException &) {  // some of source chroms might be missing, this is normal
						progress.report(1);
						continue;
					}

					const GIntervals &src_intervals = src_track.get_intervals();
					const vector<float> &vals = src_track.get_vals();
					ChainIntervals::const_iterator hint = chain_intervs.begin();

					for (uint64_t i = 0; i < src_intervals.size(); ++i) {
						hint = chain_intervs.map_interval(src_intervals[i], tgt_intervals, hint);
						for (GIntervals::const_iterator iinterv = tgt_intervals.begin(); iinterv != tgt_intervals.end(); ++iinterv)
							buffered_intervs[iinterv->chromid].write_interval(*iinterv, vals[i]);
						check_interrupt();
					}

					progress.report(1);
				}
			} else
				TGLError("Source track type %s is currently not supported in liftover", GenomeTrack::TYPE_NAMES[src_track_type]);

			// read the temporary files one by one into memory, sort the data and save it in corresponding track
			for (vector<BufferedIntervals>::iterator ibuffered_interv = buffered_intervs.begin(); ibuffered_interv != buffered_intervs.end(); ++ibuffered_interv) {
				int chromid = ibuffered_interv - buffered_intervs.begin();
				vector<GIntervalVal> interv_vals;

				ibuffered_interv->flush();
				ibuffered_interv->seek(0, SEEK_SET);

				while (ibuffered_interv->read_interval())
					interv_vals.push_back(GIntervalVal(ibuffered_interv->last_interval(), ibuffered_interv->last_val()));

				sort(interv_vals.begin(), interv_vals.end());

				snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_1d_filename(iu.get_chromkey(), chromid).c_str());

				if (src_track_type == GenomeTrack::FIXED_BIN) {
					GenomeTrackFixedBin gtrack;
					gtrack.init_write(filename, binsize, chromid);
					int64_t end_bin = (int64_t)ceil(iu.get_chromkey().get_chrom_size(chromid) / (double)binsize);
					int64_t coord1 = 0;
					int64_t coord2 = coord1 + binsize;
					vector<GIntervalVal>::const_iterator iinterv_val = interv_vals.begin();

					for (int64_t bin = 0; bin < end_bin; ++bin) {
						double sum = 0;
						int num_intervals = 0;
						bool intersect = false;

						for ( ; iinterv_val != interv_vals.end(); ++iinterv_val) {
							if (max(coord1, iinterv_val->interval.start) < min(coord2, iinterv_val->interval.end)) {
								intersect = true;
								if (!std::isnan(iinterv_val->val)) {
									sum += iinterv_val->val;
									++num_intervals;
								}
							} else if (iinterv_val->interval.end > coord1) {
								if (intersect && iinterv_val->interval.start > coord2)
									--iinterv_val;
								break;
							}
						}

						if (num_intervals)
							gtrack.write_next_bin(sum / num_intervals);
						else
							gtrack.write_next_bin(numeric_limits<float>::quiet_NaN());

						coord1 = coord2;
						coord2 += binsize;
						check_interrupt();
					}
				} else if (src_track_type == GenomeTrack::SPARSE) {
					GenomeTrackSparse gtrack;
					gtrack.init_write(filename, chromid);

					for (vector<GIntervalVal>::const_iterator iinterv_val = interv_vals.begin(); iinterv_val != interv_vals.end(); ++iinterv_val) {
						gtrack.write_next_interval(iinterv_val->interval, iinterv_val->val);
						check_interrupt();
					}
				}

				progress.report(1);
			}

			// remove the buffered intervals files
			for (vector<BufferedIntervals>::iterator ibuffered_interv = buffered_intervs.begin(); ibuffered_interv != buffered_intervs.end(); ++ibuffered_interv) {
				string filename = ibuffered_interv->file_name();
				ibuffered_interv->close();
				unlink(filename.c_str());
			}

			progress.report_last();
		} else {   // 2D tracks
			if (src_track_type != GenomeTrack::RECTS && src_track_type != GenomeTrack::POINTS)
				TGLError("Source track type %s is currently not supported in liftover", GenomeTrack::TYPE_NAMES[src_track_type]);

			GIntervals2D all_genome_intervs;
			iu.get_all_genome_intervs(all_genome_intervs);
			vector<BufferedIntervals2D> buffered_intervs(all_genome_intervs.size());
			char filename[FILENAME_MAX];
			GIntervals tgt_intervals[2];

			Progress_reporter progress;
			progress.init(src_id2chrom.size() * src_id2chrom.size() + all_genome_intervs.size(), 1);

			// convert source intervals and write them to files
			for (vector<string>::const_iterator ichrom1 = src_id2chrom.begin(); ichrom1 != src_id2chrom.end(); ++ichrom1) {
				for (vector<string>::const_iterator ichrom2 = src_id2chrom.begin(); ichrom2 != src_id2chrom.end(); ++ichrom2) {
					GenomeTrackRectsRects src_track_rects(iu.get_track_chunk_size(), iu.get_track_num_chunks());
					GenomeTrackRectsPoints src_track_points(iu.get_track_chunk_size(), iu.get_track_num_chunks());
					GenomeTrack2D *src_track;

					if (src_track_type == GenomeTrack::RECTS)
						 src_track = &src_track_rects;
					else
						src_track = &src_track_points;

					int chromid1 = ichrom1 - src_id2chrom.begin();
					int chromid2 = ichrom2 - src_id2chrom.begin();

					try {
						snprintf(filename, sizeof(filename), "%s/%s-%s", src_track_dir, ichrom1->c_str(), ichrom2->c_str());
						src_track->init_read(filename, chromid1, chromid2);
						if (!src_track->opened()) {
							progress.report(1);
							continue;
						}
					} catch (TGLException &) {  // some of source chroms might be missing, this is normal
						progress.report(1);
						continue;
					}

					ChainIntervals::const_iterator hints[2] = { chain_intervs.begin(), chain_intervs.begin() };
					GInterval src_intervals[2];

					src_intervals[0].chromid = chromid1;
					src_intervals[1].chromid = chromid2;

					if (src_track_type == GenomeTrack::RECTS) {
						src_track_rects.load();
						RectsQuadTreeCached &qtree = src_track_rects.get_qtree();
						RectsQuadTreeCached::Iterator iqtree(&qtree);

						for (iqtree.begin(); !iqtree.is_end(); iqtree.next()) {
							src_intervals[0].start = iqtree->x1;
							src_intervals[0].end = iqtree->x2;
							src_intervals[1].start = iqtree->y1;
							src_intervals[1].end = iqtree->y2;

							hints[0] = chain_intervs.map_interval(src_intervals[0], tgt_intervals[0], hints[0]);
							hints[1] = chain_intervs.map_interval(src_intervals[1], tgt_intervals[1], hints[1]);

							for (GIntervals::const_iterator iinterv1 = tgt_intervals[0].begin(); iinterv1 != tgt_intervals[0].end(); ++iinterv1) {
								for (GIntervals::const_iterator iinterv2 = tgt_intervals[1].begin(); iinterv2 != tgt_intervals[1].end(); ++iinterv2) {
									int idx = iinterv1->chromid * iu.get_chromkey().get_num_chroms() + iinterv2->chromid;
									if (!buffered_intervs[idx].opened()) {
										snprintf(filename, sizeof(filename), "%s/_%s-%s", dirname.c_str(), iu.get_chromkey().id2chrom(iinterv1->chromid).c_str(), iu.get_chromkey().id2chrom(iinterv2->chromid).c_str());
										buffered_intervs[idx].open(filename, "w+", iinterv1->chromid, iinterv2->chromid);
									}
									buffered_intervs[idx].write_interval(*iinterv1, *iinterv2, iqtree->v);
								}
							}
							check_interrupt();
						}
					} else {
						src_track_points.load();
						PointsQuadTreeCached &qtree = src_track_points.get_qtree();
						PointsQuadTreeCached::Iterator iqtree(&qtree);

						for (iqtree.begin(); !iqtree.is_end(); iqtree.next()) {
							src_intervals[0].start = iqtree->x;
							src_intervals[0].end = iqtree->x + 1;
							src_intervals[1].start = iqtree->y;
							src_intervals[1].end = iqtree->y + 1;

							hints[0] = chain_intervs.map_interval(src_intervals[0], tgt_intervals[0], hints[0]);
							hints[1] = chain_intervs.map_interval(src_intervals[1], tgt_intervals[1], hints[1]);

							for (GIntervals::const_iterator iinterv1 = tgt_intervals[0].begin(); iinterv1 != tgt_intervals[0].end(); ++iinterv1) {
								for (GIntervals::const_iterator iinterv2 = tgt_intervals[1].begin(); iinterv2 != tgt_intervals[1].end(); ++iinterv2) {
									int idx = iinterv1->chromid * iu.get_chromkey().get_num_chroms() + iinterv2->chromid;
									if (!buffered_intervs[idx].opened()) {
										snprintf(filename, sizeof(filename), "%s/_%s-%s", dirname.c_str(), iu.get_chromkey().id2chrom(iinterv1->chromid).c_str(), iu.get_chromkey().id2chrom(iinterv2->chromid).c_str());
										buffered_intervs[idx].open(filename, "w+", iinterv1->chromid, iinterv2->chromid);
									}
									buffered_intervs[idx].write_interval(*iinterv1, *iinterv2, iqtree->v);
								}
							}
							check_interrupt();
						}
					}

					progress.report(1);
				}
			}

			// read the temporary files one by one into memory, build the quad tree and save it in corresponding track
			for (vector<BufferedIntervals2D>::iterator ibuffered_interv = buffered_intervs.begin(); ibuffered_interv != buffered_intervs.end(); ++ibuffered_interv) {
				if (ibuffered_interv->opened()) {
					int chromid1 = ibuffered_interv->chromid1();
					int chromid2 = ibuffered_interv->chromid2();
					RectsQuadTree qtree;

					qtree.reset(0, 0, iu.get_chromkey().get_chrom_size(chromid1), iu.get_chromkey().get_chrom_size(chromid2));

					ibuffered_interv->flush();
					ibuffered_interv->seek(0, SEEK_SET);

					while (ibuffered_interv->read_interval())
						qtree.insert(RectsQuadTree::ValueType(ibuffered_interv->last_interval(), ibuffered_interv->last_val()));

					if (!qtree.empty()) {
						GenomeTrackRectsRects gtrack(iu.get_track_chunk_size(), iu.get_track_num_chunks());

						snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid1, chromid2).c_str());
						gtrack.init_write(filename, chromid1, chromid2);
						gtrack.write(qtree);
					}
				}

				progress.report(1);
			}

			// remove the buffered intervals files
			for (vector<BufferedIntervals2D>::iterator ibuffered_interv = buffered_intervs.begin(); ibuffered_interv != buffered_intervs.end(); ++ibuffered_interv) {
				if (ibuffered_interv->opened()) {
					string filename = ibuffered_interv->file_name();
					ibuffered_interv->close();
					unlink(filename.c_str());
				}
			}

			progress.report_last();
		}
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

}
