#include <cstdint>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fstream>
#include <unordered_map>

#include "rdbinterval.h"
#include "rdbprogress.h"
#include "rdbutils.h"
#include "strutil.h"

#include "GenomeTrackRects.h"
#include "GenomeTrackSparse.h"

using namespace std;
using namespace rdb;

typedef pair<uint32_t, uint32_t> Chrom_pair;

class BinIntervalsFiles : public unordered_map<Chrom_pair, BufferedFile *> {
public:
	BinIntervalsFiles() : unordered_map<Chrom_pair, BufferedFile *>() {}

	~BinIntervalsFiles() {
		for (iterator ifile = begin(); ifile != end(); ++ifile) 
			delete ifile->second;
	}
};

static bool read_interval(BufferedFile &f, int64_t &start1, int64_t &end1, int64_t &start2, int64_t &end2, float &val)
{
	f.read(&start1, sizeof(start1));
	f.read(&end1, sizeof(end1));
	f.read(&start2, sizeof(start2));
	f.read(&end2, sizeof(end2));
	f.read(&val, sizeof(val));

	if (f.eof()) 
		return false;

	if (f.error())
		verror("Reading file %s: %s\n", f.file_name().c_str(), strerror(errno));

	return true;
}

static void write_interval(BufferedFile &f, int64_t start1, int64_t end1, int64_t start2, int64_t end2, float val)
{
	f.write(&start1, sizeof(start1));
	f.write(&end1, sizeof(end1));
	f.write(&start2, sizeof(start2));
	f.write(&end2, sizeof(end2));
	f.write(&val, sizeof(val));
	if (f.error())
		verror("Writing file %s: %s\n", f.file_name().c_str(), strerror(errno));
}

extern "C" {

SEXP gtrack_2d_import(SEXP _track, SEXP _files, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_track) || length(_track) != 1)
			verror("Track argument is not a string");

		if (!isString(_files) || length(_files) < 1)
			verror("Files argument is not a vector of strings");

		IntervUtils iu(_envir);
		const char *track = CHAR(STRING_ELT(_track, 0));
		string dirname = create_track_dir(_envir, track);
		bool are_all_points = true;
		Progress_reporter progress;
		BinIntervalsFiles bin_intervals_files;
		int64_t start1, start2, end1, end2;
		float val;
		char filename[FILENAME_MAX];

		// The number of 2D intervals might be huge. We might not be even able to hold all the intervals of one chromosome pair in memory and to build the quad tree with it.
		// Our strategy is therefore that:
		// 1. Read the input files and split them into binary files each holding the intervals of specific chromosomes pair.
		// 2. Check what is the number of contacts in a chromosome pair and based on that number decide how many subtrees would be needed in StatQuadTreeCachedSerializer.
		//    (Creating a quad tree through seriaizer would be the only feasible method when only part of the intervals of a chromosome pair can be loaded into RAM.)
		// 3. Split the intervals of a chromosome pair to binary files - each holding contacts of a subtree. We assume that we will be able to hold in the memory the contacts of one subtree.
		// 4. Read the contacts of a subtree and insert them to StatQuadTreeCachedSerializer.

		REprintf("Reading input file(s)...\n");

		int64_t infiles_size_sum = 0;
		for (int ifile = 0; ifile < length(_files); ++ifile) {
			const char *fname = CHAR(STRING_ELT(_files, ifile));
			struct stat st;

			if (stat(fname, &st))
				verror("Accessing file %s: %s", fname, strerror(errno));
			infiles_size_sum += st.st_size;
		}

		// STEP 1: Read the input files and split them into binary files each holding the intervals of specific chromosomes pair.
		progress.init(infiles_size_sum, 10000000);
		for (int ifile = 0; ifile < length(_files); ++ifile) {
			vector<string> fields;
			long lineno = 0;
			BufferedFile infile;
			infile.open(CHAR(STRING_ELT(_files, ifile)), "r");

			lineno += split_line(infile, fields, '\t');

			if (fields.empty())
				continue;

			if (fields.size() < GInterval2D::NUM_COLS + 1)
				verror("File %s, line %ld: invalid format", infile.file_name().c_str(), lineno);

			for (int i = 0; i < GInterval2D::NUM_COLS; ++i) {
				if (fields[i] != GInterval2D::COL_NAMES[i]) 
					verror("File %s, line %ld: invalid format", infile.file_name().c_str(), lineno);
			}

			while (1) {
				int64_t fpos = infile.tell();
				int chromid1, chromid2;
				char *endptr;

				// read the interval from tab-delimited file
				lineno += split_line(infile, fields, '\t');

				progress.report(infile.tell() - fpos);
				check_interrupt();

				if (fields.empty())
					break;

				if (fields.size() < GInterval2D::NUM_COLS + 1)
					verror("File %s, line %ld: invalid format", infile.file_name().c_str(), lineno);

				try {
					chromid1 = iu.chrom2id(fields[GInterval2D::CHROM1]);
					chromid2 = iu.chrom2id(fields[GInterval2D::CHROM2]);
				} catch (TGLException &) {
					// there might be unrecognized chromosomes, ignore them
					continue;
				}

				start1 = strtoll(fields[GInterval2D::START1].c_str(), &endptr, 10);
				if (*endptr || start1 < 0) 
					verror("File %s, line %ld: invalid format of start1 coordinate", infile.file_name().c_str(), lineno);

				end1 = strtoll(fields[GInterval2D::END1].c_str(), &endptr, 10);
				if (*endptr || end1 < 0) 
					verror("File %s, line %ld: invalid format of end1 coordinate", infile.file_name().c_str(), lineno);

				if (start1 >= end1) 
					verror("File %s, line %ld: start1 coordinate exceeds or equals the end1 coordinate", infile.file_name().c_str(), lineno);

				if ((uint64_t)end1 > iu.get_chromkey().get_chrom_size(chromid1)) 
					verror("File %s, line %ld: end1 coordinate exceeds chromosome's size", infile.file_name().c_str(), lineno);

				start2 = strtoll(fields[GInterval2D::START2].c_str(), &endptr, 10);
				if (*endptr || start1 < 0) 
					verror("File %s, line %ld: invalid format of start2 coordinate", infile.file_name().c_str(), lineno);

				end2 = strtoll(fields[GInterval2D::END2].c_str(), &endptr, 10);
				if (*endptr || end2 < 0) 
					verror("File %s, line %ld: invalid format of end2 coordinate", infile.file_name().c_str(), lineno);

				if (start2 >= end2) 
					verror("File %s, line %ld: start2 coordinate exceeds or equals the end1 coordinate", infile.file_name().c_str(), lineno);

				if ((uint64_t)end2 > iu.get_chromkey().get_chrom_size(chromid2)) 
					verror("File %s, line %ld: end2 coordinate exceeds chromosome's size", infile.file_name().c_str(), lineno);

				val = strtod(fields[GInterval2D::NUM_COLS].c_str(), &endptr);
				if (*endptr) 
					verror("File %s, line %ld: invalid value", infile.file_name().c_str(), lineno);

				are_all_points &= start1 == end1 - 1 && start2 == end2 - 1;

				// Write down the interval + value in binary format
				BinIntervalsFiles::iterator ibin_intervals_file = bin_intervals_files.find(Chrom_pair(chromid1, chromid2));
				if (ibin_intervals_file == bin_intervals_files.end()) {
					ibin_intervals_file = bin_intervals_files.insert(make_pair(Chrom_pair(chromid1, chromid2), new BufferedFile())).first;
					snprintf(filename, sizeof(filename), "%s/.%s", dirname.c_str(), GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid1, chromid2).c_str());
					if (ibin_intervals_file->second->open(filename, "wb"))
						verror("Writing an intermediate file %s: %s\n", filename, strerror(errno));
				}

				BufferedFile *out_file = ibin_intervals_file->second;
				write_interval(*out_file, start1, end1, start2, end2, val);
			}
		}
		progress.report_last();

		// We might open in the next stage many new files to write the subtrees. Close the old files otherwise the user might exceed
		// the limit of maximal number of open files.
		for (BinIntervalsFiles::iterator ifile = bin_intervals_files.begin(); ifile != bin_intervals_files.end(); ++ifile)
			ifile->second->close();

		REprintf("Writing the track...\n");
		progress.init(bin_intervals_files.size(), 1);

		for (BinIntervalsFiles::iterator ifile = bin_intervals_files.begin(); ifile != bin_intervals_files.end(); ++ifile) {
			// STAGE 2. Check what is the number of contacts in a chromosome pair and based on that number decide how many subtrees would be needed in StatQuadTreeCachedSerializer.
			int chromid1 = ifile->first.first;
			int chromid2 = ifile->first.second;
			BufferedFile &infile = *ifile->second;

			if (infile.open(infile.file_name().c_str(), "r")) 
				verror("Opening an intermediate file %s: %s\n", infile.file_name().c_str(), strerror(errno));

			int64_t num_intervals = infile.file_size() / (sizeof(start1) + sizeof(end1) + sizeof(start2) + sizeof(end2) + sizeof(val));
			int64_t num_subtrees = max(num_intervals / (int64_t)iu.get_max_data_size(), (int64_t)1);
			num_subtrees = 1 << (2 * (int)(log2(num_subtrees) / 2));  // round the number of subtrees to the lowest power of 4

			GenomeTrackRectsPoints gtrack_points(iu.get_track_chunk_size(), iu.get_track_num_chunks());
			GenomeTrackRectsRects gtrack_rects(iu.get_track_chunk_size(), iu.get_track_num_chunks());
			PointsQuadTreeCachedSerializer points_serializer;
			RectsQuadTreeCachedSerializer rects_serializer;
			BufferedFiles subtrees_files(num_subtrees);

			snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid1, chromid2).c_str());

			if (are_all_points) {
				gtrack_points.init_write(filename, chromid1, chromid2);
				gtrack_points.init_serializer(points_serializer, 0, 0, iu.get_chromkey().get_chrom_size(chromid1), iu.get_chromkey().get_chrom_size(chromid2), num_subtrees, true);
			} else {
				gtrack_rects.init_write(filename, chromid1, chromid2);
				gtrack_rects.init_serializer(rects_serializer, 0, 0, iu.get_chromkey().get_chrom_size(chromid1), iu.get_chromkey().get_chrom_size(chromid2), num_subtrees, true);
			}

			// STAGE 3: Split the intervals of a chromosome pair to binary files - each holding contacts of a subtree.
			if (num_subtrees > 1) {
				const Rectangles &subarenas = are_all_points ? points_serializer.get_subarenas() : rects_serializer.get_subarenas();

				for (uint64_t i = 0; i < subtrees_files.size(); ++i) {
					snprintf(filename, sizeof(filename), "%s/.%ld", dirname.c_str(), (long)i);
					subtrees_files[i] = new BufferedFile();
					if (subtrees_files[i]->open(filename, "w+")) 
						verror("Opening an intermediate file %s: %s\n", filename, strerror(errno));
				}

				while (1) {
					if (!read_interval(infile, start1, end1, start2, end2, val))
						break;

					Rectangle rect(start1, start2, end1, end2);
					for (Rectangles::const_iterator isubarena = subarenas.begin(); isubarena != subarenas.end(); ++isubarena) {
						if (rect.do_intersect(*isubarena)) {
							write_interval(*subtrees_files[isubarena - subarenas.begin()], start1, end1, start2, end2, val);
							break;
						}
					}
					check_interrupt();
				}

				for (uint64_t i = 0; i < subtrees_files.size(); ++i)
					subtrees_files[i]->seek(0, SEEK_SET);

				infile.close();
				unlink(infile.file_name().c_str());
			}

			// Stage 4: Read the contacts of a subtree and insert them to StatQuadTreeCachedSerializer.
			for (uint64_t i = 0; i < (uint64_t)num_subtrees; ++i) {
				BufferedFile &subtree_file = num_subtrees == 1 ? infile : *subtrees_files[i];

				while (1) {
					if (!read_interval(subtree_file, start1, end1, start2, end2, val))
						break;

					if (are_all_points) 
						points_serializer.insert(PointsQuadTree::ValueType(start1, start2, val));
					else
						rects_serializer.insert(RectsQuadTree::ValueType(start1, start2, end1, end2, val));

					check_interrupt();
				}
				subtree_file.close();
				unlink(subtree_file.file_name().c_str());
			}

			if (are_all_points) 
				points_serializer.end();
			else
				rects_serializer.end();

			progress.report(1);
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

