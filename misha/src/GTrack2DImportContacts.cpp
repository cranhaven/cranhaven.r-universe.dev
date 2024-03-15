#include <cstdint>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fstream>
#include <unordered_map>

#include "HashFunc.h"
#include "rdbinterval.h"
#include "rdbprogress.h"
#include "rdbutils.h"
#include "strutil.h"

#include "GenomeTrackRects.h"
#include "GenomeTrackSparse.h"

using namespace std;
using namespace rdb;

typedef unordered_map<pair<uint64_t, uint64_t>, double> Contacts;
typedef pair<uint32_t, uint32_t> Chrom_pair;

class Contact_chrom_files : public unordered_map<Chrom_pair, BufferedFile *> {
public:
	Contact_chrom_files() : unordered_map<Chrom_pair, BufferedFile *>() {}

	~Contact_chrom_files() {
		for (iterator ifile = begin(); ifile != end(); ++ifile) 
			delete ifile->second;
	}
};

static unsigned read_header(BufferedFile &file, const char *fname, const char *ftype, const char *colnames[], int num_cols, vector<int> &fcol_idx)
{
	vector<string> fields;

	if (file.open(fname, "r"))
		verror("Failed to open %s file %s: %s", ftype, fname, strerror(errno));

	fcol_idx.resize(num_cols, -1);

	split_line(file, fields, '\t');
	if (fields.empty())
		verror("Invalid format of %s file %s", ftype, fname);

	if (file.error())
		verror("Error while reading %s file %s: %s", ftype, fname, strerror(errno));

	for (int i = 0; i < num_cols; ++i) {
		for (vector<string>::const_iterator ifield = fields.begin(); ifield != fields.end(); ++ifield) {
			if (*ifield == colnames[i]) {
				if (fcol_idx[i] == -1)
					fcol_idx[i] = ifield - fields.begin();
				else
					verror("Invalid format of %s file %s: column %s appears more than once", ftype, fname, colnames[i]);
			}
		}

		if (fcol_idx[i] < 0)
			verror("Invalid format of %s file %s: column %s was not found", ftype, fname, colnames[i]);
	}

	return fields.size();
}

static void write_contact(BufferedFile &file, int64_t coord1, int64_t coord2, float value)
{
	file.write(&coord1, sizeof(coord1));
	file.write(&coord2, sizeof(coord2));
	file.write(&value, sizeof(value));
	if (file.error())
		verror("Writing file %s: %s\n", file.file_name().c_str(), strerror(errno));
}

static void process_contacts_as_intervals(IntervUtils &iu, SEXP _files, Contact_chrom_files &contact_chrom_files, const char *dirname)
{
	Progress_reporter progress;
	int64_t start1, start2, end1, end2;
	float val;

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

			int64_t coord1 = (start1 + end1) / 2;
			int64_t coord2 = (start2 + end2) / 2;

			if (chromid1 > chromid2 || (chromid1 == chromid2 && coord1 > coord2)) {
				swap(chromid1, chromid2);
				swap(coord1, coord2);
			}


			// Write down the interval + value in binary format
			Chrom_pair chrom_pair(chromid1, chromid2);
			Contact_chrom_files::iterator icontact_chrom_file = contact_chrom_files.find(chrom_pair);

			if (icontact_chrom_file == contact_chrom_files.end()) {
				char filename[FILENAME_MAX];

				snprintf(filename, sizeof(filename), "%s/.%s", dirname, GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid1, chromid2).c_str());
				icontact_chrom_file = contact_chrom_files.insert(make_pair(chrom_pair, new BufferedFile())).first;
				if (icontact_chrom_file->second->open(filename, "wb"))
					verror("Writing an intermediate file %s: %s\n", filename, strerror(errno));
			}

			write_contact(*icontact_chrom_file->second, coord1, coord2, val);
//if ((start1 + end1) / 2 == 183256 && (start2 + end2) / 2 == 214179)
//REprintf("writing %g to %s\n", val, GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid1, chromid2).c_str());
		}
	}
	progress.report_last();
}

static void process_contacts_as_fends(IntervUtils &iu, SEXP _contacts, SEXP _fends, Contact_chrom_files &contact_chrom_files, const char *dirname)
{
	enum { COL_FEND, COL_CHR, COL_COORD, NUM_FENDS_COLS };
	const char *FENDS_COLS[] = { "fend", "chr", "coord" };
	vector<int> fends_col_idx;
	unsigned fends_cols_total;
	const char *fends_fname = CHAR(STRING_ELT(_fends, 0));

	enum { COL_FEND1, COL_FEND2, COL_COUNT, NUM_CONTACTS_COLS };
	const char *CONTACTS_COLS[] = { "fend1", "fend2", "count" };

	vector<string> fields;
	int64_t contacts_files_size_sum = 0;
	for (int icontact_file = 0; icontact_file < length(_contacts); ++icontact_file) {
		const char *contacts_fname = CHAR(STRING_ELT(_contacts, icontact_file));
		struct stat st;

		if (stat(contacts_fname, &st))
			verror("Accessing file %s: %s", contacts_fname, strerror(errno));
		contacts_files_size_sum += st.st_size;
	}

	Progress_reporter progress;
	BufferedFile fends_file;
	fends_cols_total = read_header(fends_file, fends_fname, "fragment ends", FENDS_COLS, NUM_FENDS_COLS, fends_col_idx);

	// read fends file
	REprintf("Reading fends file...\n");

	vector<int> chromids;
	vector<int64_t> coords;
	vector<bool> defined;
	int64_t lineno;
	char *endptr;
	int64_t fendid;
	int64_t coord;
	char chromname[1000];
	const char *CHROM_PREFIX = "chr";
	const int CHROM_PREFIX_LEN = strlen(CHROM_PREFIX);
	strcpy(chromname, "chr");

	progress.init(fends_file.file_size(), 10000000);
	lineno = 1;
	while (1) {
		int64_t fpos = fends_file.tell();

		lineno++;
		split_line(fends_file, fields, '\t');
		if (fields.empty())
			break;

		if (fields.size() != fends_cols_total)
			verror("Error reading fragment ends file %s, line %ld: invalid file format", fends_fname, lineno);

		const char *fendid_str = fields[fends_col_idx[COL_FEND]].c_str();
		const char *chr_str = fields[fends_col_idx[COL_CHR]].c_str();
		const char *coord_str = fields[fends_col_idx[COL_COORD]].c_str();

		fendid = strtoll(fendid_str, &endptr, 10);
		if (*endptr)
			verror("Error reading fragment ends file %s, line %ld: invalid %s", fends_fname, lineno, FENDS_COLS[COL_FEND]);

		chromids.resize(fendid + 1);
		coords.resize(fendid + 1);
		defined.resize(fendid + 1);
		defined[fendid] = true;

		if (strncmp(chr_str, CHROM_PREFIX, CHROM_PREFIX_LEN)) {
			strcpy(chromname + CHROM_PREFIX_LEN, chr_str);
			chromids[fendid] = iu.get_chromkey().chrom2id(chromname);
		} else
			chromids[fendid] = iu.get_chromkey().chrom2id(chr_str);

		coord = strtoll(coord_str, &endptr, 10);
		if (*endptr)
			verror("Error reading fragment ends file %s, line %ld: invalid %s", fends_fname, lineno, FENDS_COLS[COL_COORD]);

		if (coord < 0 || coord >= (int64_t)iu.get_chromkey().get_chrom_size(chromids[fendid]))
			verror("Error reading fragment ends file %s, line %ld: %s is out of range", fends_fname, lineno, FENDS_COLS[COL_COORD]);

		coords[fendid] = coord;
		progress.report(fends_file.tell() - fpos);
		check_interrupt();
	}
	progress.report_last();

	// STEP 1: Read the contacts and split them into binary files each holding the contacts of specific pair of chromosomes.
	REprintf("Reading contacts...\n");

	progress.init(contacts_files_size_sum, 10000000);

	float value;

	for (int icontact_file = 0; icontact_file < length(_contacts); ++icontact_file) {
		int64_t fend1, fend2;
		GIntervals2D intervs;
		const char *contacts_fname = CHAR(STRING_ELT(_contacts, icontact_file));
		vector<int> contacts_col_idx;
		unsigned contacts_cols_total;
		BufferedFile contacts_file;

		contacts_cols_total = read_header(contacts_file, contacts_fname, "contacts", CONTACTS_COLS, NUM_CONTACTS_COLS, contacts_col_idx);

		lineno = 1;
		while (1) {
			int64_t fpos = contacts_file.tell();

			lineno++;
			split_line(contacts_file, fields, '\t');
			if (fields.empty())
				break;

			if (fields.size() != contacts_cols_total)
				verror("Error reading contacts ends file %s, line %ld: invalid file format", contacts_fname, lineno);

			const char *fend1_str = fields[contacts_col_idx[COL_FEND1]].c_str();
			const char *fend2_str = fields[contacts_col_idx[COL_FEND2]].c_str();
			const char *value_str = fields[contacts_col_idx[COL_COUNT]].c_str();

			fend1 = strtoll(fend1_str, &endptr, 10);
			if (*endptr)
				verror("Error reading contacts file %s, line %ld: invalid %s", contacts_fname, lineno, CONTACTS_COLS[COL_FEND1]);
			if (fend1 < 0 || fend1 >= (int64_t)defined.size() || !defined[fend1])
				continue;

			fend2 = strtoll(fend2_str, &endptr, 10);
			if (*endptr)
				verror("Error reading contacts file %s, line %ld: invalid %s", contacts_fname, lineno, CONTACTS_COLS[COL_FEND2]);
			if (fend2 < 0 || fend2 >= (int64_t)defined.size() || !defined[fend2])
				continue;

			value = strtod(value_str, &endptr);
			if (*endptr)
				verror("Error reading contacts file %s, line %ld: invalid %s", contacts_fname, lineno, CONTACTS_COLS[COL_COUNT]);

			if (chromids[fend1] > chromids[fend2] || (chromids[fend1] == chromids[fend2] && coords[fend1] > coords[fend2]))
				swap(fend1, fend2);

			Chrom_pair chrom_pair(chromids[fend1], chromids[fend2]);
			Contact_chrom_files::iterator icontact_chrom_file = contact_chrom_files.find(chrom_pair);

			if (icontact_chrom_file == contact_chrom_files.end()) {
				char filename[FILENAME_MAX];

				snprintf(filename, sizeof(filename), "%s/.%s", dirname, GenomeTrack::get_2d_filename(iu.get_chromkey(), chromids[fend1], chromids[fend2]).c_str());
				icontact_chrom_file = contact_chrom_files.insert(make_pair(chrom_pair, new BufferedFile())).first;
				if (icontact_chrom_file->second->open(filename, "wb"))
					verror("Writing an intermediate file %s: %s\n", filename, strerror(errno));
			}

			write_contact(*icontact_chrom_file->second, coords[fend1], coords[fend2], value);
			progress.report(contacts_file.tell() - fpos);
			check_interrupt();
		}
	}
	progress.report_last();
}

extern "C" {

SEXP gtrack_import_contacts(SEXP _track, SEXP _contacts, SEXP _fends, SEXP _allow_duplicates, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_track) || length(_track) != 1)
			verror("Track argument is not a string");

		if (!isNull(_fends) && (!isString(_fends) || length(_fends) != 1))
			verror("Fends argument is not a string");

		if (!isString(_contacts) || length(_contacts) < 1)
			verror("Contacts argument is not a string");

		if (!isLogical(_allow_duplicates) || length(_allow_duplicates) != 1)
			verror("Allow duplicates argument is not a boolean");

		IntervUtils iu(_envir);
		const char *track = CHAR(STRING_ELT(_track, 0));
		string dirname = create_track_dir(_envir, track);
		Contact_chrom_files contact_chrom_files;
		int64_t coord1, coord2;
		float value;
		Progress_reporter progress;

		// The number of contacts might be huge. We might not be even able to hold the contacts of one chromosome in the memory and to build the quad tree with it.
		// Our strategy is therefore that:
		// 1. Read the contacts and split them into binary files each holding the contacts of specific pair of chromosomes.
		// 2. Check what is the number of contacts in a chromosome pair and based on that number decide how many subtrees would be needed in StatQuadTreeCachedSerializer.
		//    (Creating a quad tree through seriaizer would be the only feasible method considering the enormous number of contacts.)
		// 3. Split contacts of a chromosome pair to binary files - each holding contacts of a subtree. We assume that we will be able to hold in the memory the contacts of one subtree.
		// 4. Read the contacts of a subtree, sum up the duplicates and insert them to StatQuadTreeCachedSerializer.

		// STAGE 1: Read the contacts and split them into binary files each holding the contacts of specific pair of chromosomes.
		if (isNull(_fends))
			process_contacts_as_intervals(iu, _contacts, contact_chrom_files, dirname.c_str());
		else
			process_contacts_as_fends(iu, _contacts, _fends, contact_chrom_files, dirname.c_str());

		// We might open in the next stage many new files to write the subtrees. Close the old files otherwise the user might exceed
		// the limit of maximal number of open files.
		for (Contact_chrom_files::iterator ifile = contact_chrom_files.begin(); ifile != contact_chrom_files.end(); ++ifile)
			ifile->second->close();

		REprintf("Writing the track...\n");
		progress.init(contact_chrom_files.size(), 1);

		for (Contact_chrom_files::iterator icontact_chrom_file = contact_chrom_files.begin(); icontact_chrom_file != contact_chrom_files.end(); ++icontact_chrom_file) {
			uint64_t chromid1 = icontact_chrom_file->first.first;
			uint64_t chromid2 = icontact_chrom_file->first.second;
			BufferedFile &infile = *icontact_chrom_file->second;

			// STAGE 2: Check what is the number of contacts in a chromosome pair and based on that number decide how many subtrees would be needed in StatQuadTreeCachedSerializer.
			//          (Creating a quad tree through seriaizer would be the only feasible method considering the enormous number of contacts.)
			if (infile.open(infile.file_name().c_str(), "r")) 
				verror("Opening an intermediate file %s: %s\n", infile.file_name().c_str(), strerror(errno));

			int64_t num_contacts = infile.file_size() / (sizeof(coord1) + sizeof(coord2) + sizeof(value));

			if (chromid1 == chromid2) 
				num_contacts *= 2;   // we are going to mirror the coordinates around the diagonal

			int64_t num_subtrees = max(num_contacts / (int64_t)iu.get_max_data_size(), (int64_t)1);
			num_subtrees = 1 << (2 * (int)(log2(num_subtrees) / 2));  // round the number of subtrees to the lowest power of 4

			char filename[FILENAME_MAX];
			GenomeTrackRectsPoints gtrack1(iu.get_track_chunk_size(), iu.get_track_num_chunks());
			GenomeTrackRectsPoints gtrack2(iu.get_track_chunk_size(), iu.get_track_num_chunks());
			PointsQuadTreeCachedSerializer qtree_serializer1, qtree_serializer2;
			BufferedFiles subtrees_files(num_subtrees);

			snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid1, chromid2).c_str());
			gtrack1.init_write(filename, chromid1, chromid2);
			gtrack1.init_serializer(qtree_serializer1, 0, 0, iu.get_chromkey().get_chrom_size(chromid1), iu.get_chromkey().get_chrom_size(chromid2), num_subtrees, false);

			if (chromid1 != chromid2) {
				snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid2, chromid1).c_str());
				gtrack2.init_write(filename, chromid2, chromid1);
				gtrack2.init_serializer(qtree_serializer2, 0, 0, iu.get_chromkey().get_chrom_size(chromid2), iu.get_chromkey().get_chrom_size(chromid1), num_subtrees, false);
			}

			// 3. STAGE 3: Split contacts of a chromosome pair to binary files (if needed) - each holding contacts of a subtree.
			if (num_subtrees > 1) {
				const Rectangles &subarenas = qtree_serializer1.get_subarenas();

				for (uint64_t i = 0; i < subtrees_files.size(); ++i) {
					snprintf(filename, sizeof(filename), "%s/.%ld", dirname.c_str(), (long)i);
					subtrees_files[i] = new BufferedFile();
					if (subtrees_files[i]->open(filename, "w+")) 
						verror("Opening an intermediate file %s: %s\n", filename, strerror(errno));
				}

				while (1) {
					infile.read(&coord1, sizeof(coord1));
					infile.read(&coord2, sizeof(coord2));
					infile.read(&value, sizeof(value));

					if (infile.eof()) 
						break;

					if (infile.error()) 
						verror("Reading file %s: %s\n", infile.file_name().c_str(), strerror(errno));

					Point point(coord1, coord2);
					for (Rectangles::const_iterator isubarena = subarenas.begin(); isubarena != subarenas.end(); ++isubarena) {
						if (point.do_intersect(*isubarena)) {
							write_contact(*subtrees_files[isubarena - subarenas.begin()], coord1, coord2, value);
							break;
						}
					}

					if (chromid1 == chromid2 && point.x != point.y) {
						// if it's a pair of identical chromosomes, mirror the contact around the diagonal
						Point point(coord2, coord1);
						for (Rectangles::const_iterator isubarena = subarenas.begin(); isubarena != subarenas.end(); ++isubarena) {
							if (point.do_intersect(*isubarena)) {
								write_contact(*subtrees_files[isubarena - subarenas.begin()], coord2, coord1, value);
								break;
							}
						}
					}
					check_interrupt();
				}

				for (uint64_t i = 0; i < subtrees_files.size(); ++i)
					subtrees_files[i]->seek(0, SEEK_SET);

				infile.close();
				unlink(infile.file_name().c_str());
			}

			for (uint64_t i = 0; i < (uint64_t)num_subtrees; ++i) {
				BufferedFile &subtree_file = num_subtrees == 1 ? infile : *subtrees_files[i];
				Contacts contacts;

				while (1) {
					subtree_file.read(&coord1, sizeof(coord1));
					subtree_file.read(&coord2, sizeof(coord2));
					subtree_file.read(&value, sizeof(value));

					if (subtree_file.eof()) 
						break;

					if (subtree_file.error()) 
						verror("Reading file %s: %s\n", subtree_file.file_name().c_str(), strerror(errno));

					pair<uint64_t, uint64_t> key(coord1, coord2);
					Contacts::iterator icontact = contacts.find(key);

					if (icontact == contacts.end())
						contacts[key] = value;
					else {
						if (!LOGICAL(_allow_duplicates)[0]) 
							verror("Duplicated contact (%s, %ld)-(%s, %ld)", iu.id2chrom(chromid1).c_str(), coord1, iu.id2chrom(chromid2).c_str(), coord2);
						icontact->second += value;
					}
					check_interrupt();
				}

				for (Contacts::const_iterator icontact = contacts.begin(); icontact != contacts.end(); ++icontact) {
					coord1 = icontact->first.first;
					coord2 = icontact->first.second;
					value = icontact->second;

					qtree_serializer1.insert(PointsQuadTree::ValueType(coord1, coord2, value));

					if (chromid1 != chromid2) 
						qtree_serializer2.insert(PointsQuadTree::ValueType(coord2, coord1, value));
					else if (num_subtrees == 1 && coord1 != coord2)
						qtree_serializer1.insert(PointsQuadTree::ValueType(coord2, coord1, value));

					check_interrupt();
				}

				subtree_file.close();
				unlink(subtree_file.file_name().c_str());

				check_interrupt();
			}

			qtree_serializer1.end();

			if (chromid1 != chromid2) 
				qtree_serializer2.end();

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
