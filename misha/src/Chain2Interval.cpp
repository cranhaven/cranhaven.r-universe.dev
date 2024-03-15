#include <errno.h>
#include <set>
#include <string>
#include <vector>
#include <unordered_map>

#include "HashFunc.h"
#include "BufferedFile.h"
#include "rdbinterval.h"
#include "rdbutils.h"
#include "strutil.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP gchain2interv(SEXP _chainfile, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_chainfile) || length(_chainfile) != 1)
			verror("Chain file argument is not a string");

		IntervUtils iu(_envir);
		const char *chainfname = CHAR(STRING_ELT(_chainfile, 0));
		BufferedFile chainfile;

		if (chainfile.open(chainfname, "r"))
			TGLError("Error opening chain file %s: %s\n", chainfname, strerror(errno));

		enum { CHAIN, SCORE, CHROM1, CHROM_SIZE1, STRAND1, START1, END1, CHROM2, CHROM_SIZE2, STRAND2, START2, END2, ID, NUM_FIELDS };
		enum { SIZE, DT, DQ };
		enum { SRC, TGT };

		ChainIntervals chain_intervs;
		vector<string> fields(NUM_FIELDS);
		unordered_map<string, int> chrom2id; // used only for source chroms
		vector<string> id2chrom;        // used only for source chroms
		vector<int64_t> chrom_sizes;    // used only for source chroms
		int chrom[2] = { -1, -1 };
		int64_t start[2] = { -1, -1 };
		int64_t end[2] = { -1, -1 };
		int strand[2] = { -1, -1 };
		int64_t lineno = 0;
		char *endptr;
		int64_t num;

		while (1) {
			lineno += split_line_by_space_chars(chainfile, fields, NUM_FIELDS);

			if (fields.size() == NUM_FIELDS) {
				if (chrom[TGT] >= 0) {
					if (start[SRC] != end[SRC])
						TGLError("Chain file %s, line %ld: new chain is defined before the previous one finished to map reference sequence", chainfname, lineno);

					if (start[TGT] != end[TGT])
						TGLError("Chain file %s, line %ld: new chain is defined before the previous one finished to map query sequence", chainfname, lineno);
				}

				// CHAIN
				if (strcmp(fields[CHAIN].c_str(), "chain"))
					TGLError("Chain file %s, line %ld: invalid file format", chainfname, lineno);

				// CHROM1
				unordered_map<string, int>::const_iterator ichrom2id = chrom2id.find(fields[CHROM1]);
				if (ichrom2id == chrom2id.end()) {
					chrom[SRC] = id2chrom.size();
					chrom2id[fields[CHROM1]] = chrom[SRC];
					id2chrom.push_back(fields[CHROM1]);
					chrom_sizes.push_back(0);
				} else
					chrom[SRC] = ichrom2id->second;

				// CHROM_SIZE1
				num = strtoll(fields[CHROM_SIZE1].c_str(), &endptr, 10);
				if (*endptr || num <= 0)
					TGLError("Chain file %s, line %ld: invalid format of reference chrom size", chainfname, lineno);
				if (!chrom_sizes[chrom[SRC]])
					chrom_sizes[chrom[SRC]] = num;
				else if (chrom_sizes[chrom[SRC]] != num)
					TGLError("Chain file %s, line %ld: reference chrom size (%ld) differs from previous value (%ld)",
							chainfname, lineno, num, chrom_sizes[chrom[SRC]]);

				// STRAND1
				if (!strcmp(fields[STRAND1].c_str(), "+"))
					strand[SRC] = 0;
				else if (!strcmp(fields[STRAND1].c_str(), "-"))
					strand[SRC] = 1;
				else
					TGLError("Chain file %s, line %ld: invalid format of reference strand", chainfname, lineno);

				// START1
				num = strtoll(fields[START1].c_str(), &endptr, 10);
				if (*endptr || num < 0)
					TGLError("Chain file %s, line %ld: invalid value of reference start coordinate", chainfname, lineno);
				if (num >= chrom_sizes[chrom[SRC]])
					TGLError("Chain file %s, line %ld: reference start coordinate exceeds chromosome size", chainfname, lineno);
				start[SRC] = num;

				// END1
				num = strtoll(fields[END1].c_str(), &endptr, 10);
				if (*endptr)
					TGLError("Chain file %s, line %ld: invalid value of reference end coordinate", chainfname, lineno);
				if (num <= start[SRC])
					TGLError("Chain file %s, line %ld: reference end coordinate is less or equal than the start coordinate", chainfname, lineno);
				if (num > chrom_sizes[chrom[SRC]])
					TGLError("Chain file %s, line %ld: reference end coordinate exceeds chromosome size", chainfname, lineno);
				end[SRC] = num;

				// CHROM2
				try {
					chrom[TGT] = iu.get_chromkey().chrom2id(fields[CHROM2]);
				} catch (TGLException &) { // target chromosome might not exist (random chroms, etc.) => skip the mapping
					chrom[TGT] = -1;
					continue;
				}

				// CHROM_SIZE2
				num = strtoll(fields[CHROM_SIZE2].c_str(), &endptr, 10);
				if (*endptr)
					TGLError("Chain file %s, line %ld: invalid format of query chrom size", chainfname, lineno);
				if ((int64_t)iu.get_chromkey().get_chrom_size(chrom[TGT]) != num)
					TGLError("Chain file %s, line %ld: query chrom size (%ld) differs from what have been defined in the database (%ld)",
							chainfname, lineno, num, iu.get_chromkey().get_chrom_size(chrom[TGT]));

				// STRAND2
				if (!strcmp(fields[STRAND2].c_str(), "+"))
					strand[TGT] = 0;
				else if (!strcmp(fields[STRAND2].c_str(), "-"))
					strand[TGT] = 1;
				else
					TGLError("Chain file %s, line %ld: invalid format of query strand", chainfname, lineno);

				// START2
				num = strtoll(fields[START2].c_str(), &endptr, 10);
				if (*endptr || num < 0)
					TGLError("Chain file %s, line %ld: invalid value of query start coordinate", chainfname, lineno);
				if (num >= (int64_t)iu.get_chromkey().get_chrom_size(chrom[TGT]))
					TGLError("Chain file %s, line %ld: query start coordinate exceeds chromosome size", chainfname, lineno);
				start[TGT] = num;

				// END2
				num = strtoll(fields[END2].c_str(), &endptr, 10);
				if (*endptr)
					TGLError("Chain file %s, line %ld: invalid value of reference end coordinate", chainfname, lineno);
				if (num <= start[TGT])
					TGLError("Chain file %s, line %ld: reference end coordinate is less or equal than the start coordinate", chainfname, lineno);
				if (num > (int64_t)iu.get_chromkey().get_chrom_size(chrom[TGT]))
					TGLError("Chain file %s, line %ld: reference end coordinate exceeds chromosome size", chainfname, lineno);
				end[TGT] = num;
			} else if (fields.size() == 3 || fields.size() == 1) {
				if (chrom[SRC] < 0)
					TGLError("Chain file %s, line %ld: invalid file format", chainfname, lineno);

				if (chrom[TGT] < 0)
					continue;

				int64_t size = strtoll(fields[SIZE].c_str(), &endptr, 10);
				if (*endptr || size <= 0)
					TGLError("Chain file %s, line %ld: invalid size", chainfname, lineno);
				if (start[SRC] + size > end[SRC])
					TGLError("Chain file %s, line %ld: block exceeds chain size of the reference genome", chainfname, lineno);
				if (start[TGT] + size > end[TGT])
					TGLError("Chain file %s, line %ld: block exceeds chain size of the query genome", chainfname, lineno);

				chain_intervs.push_back(ChainInterval(
					chrom[TGT],
					strand[TGT] ? iu.get_chromkey().get_chrom_size(chrom[TGT]) - start[TGT] - size : start[TGT],
					strand[TGT] ? iu.get_chromkey().get_chrom_size(chrom[TGT]) - start[TGT] : start[TGT] + size,
					chrom[SRC],
					strand[SRC] ? chrom_sizes[chrom[SRC]] - start[SRC] - size - 1 : start[SRC]));

				if (fields.size() == 3) {
					int64_t dt = strtoll(fields[DT].c_str(), &endptr, 10);
					int64_t dq = strtoll(fields[DQ].c_str(), &endptr, 10);

					if (dt < 0 || dq < 0 || (!dt && !dq))
						TGLError("Chain file %s, line %ld: invalid block gaps", chainfname, lineno);

					start[SRC] += size + dt;
					start[TGT] += size + dq;
				} else {
					start[SRC] += size;
					start[TGT] += size;

					if (start[SRC] != end[SRC])
						TGLError("Chain file %s, line %ld: reference chain was not fully mapped", chainfname, lineno);

					if (start[TGT] != end[TGT])
						TGLError("Chain file %s, line %ld: query chain was not fully mapped", chainfname, lineno);
				}
			} else if (fields.empty()) {
#ifdef error  // R redefines "error" which interferes with BufferedFile::error function
    #define tmp_error error
    #undef error
				if (chainfile.error())
					TGLError("Reading chain file %s: %s", chainfname, strerror(errno));
    #define error tmp_error
#endif
				break;
			} else
				TGLError("Chain file %s, line %ld: invalid file format", chainfname, lineno);
		}

		if (chain_intervs.empty())
			return R_NilValue;

		// check for overlaps in source
		chain_intervs.sort_by_src();
		chain_intervs.verify_no_src_overlaps(iu.get_chromkey(), id2chrom);

		// check for overlaps in target: overlaps might exist, remove them
		set<ChainInterval, ChainInterval::SetCompare> sorted_intervs;    // we need a set as we're going to insert on the fly new objects
		for (ChainIntervals::iterator iinterv = chain_intervs.begin() + 1; iinterv != chain_intervs.end(); ++iinterv)
			sorted_intervs.insert(*iinterv);

		set<ChainInterval>::iterator iinterv2 = sorted_intervs.begin();
		set<ChainInterval>::iterator iinterv1 = iinterv2++;

		while (iinterv2 != sorted_intervs.end()) {
			// the intervals overlap

			if (iinterv1->chromid == iinterv2->chromid && iinterv1->end > iinterv2->start) {
				// wipe the overlapping part 
				int64_t tgt_end1 = iinterv1->end;

				// concatenate interv1
				((ChainInterval &)*iinterv1).end = iinterv2->start; // that's an ugly hack: iinterv1 points to a const object, but changing the "end" is harmless

				// create interv2 that does not contain the overlapping part
				if (tgt_end1 < iinterv2->end) { // the two intervals intersect
					ChainInterval interv(iinterv2->chromid, tgt_end1, iinterv2->end,
							iinterv2->chromid_src, iinterv2->start_src + tgt_end1 - iinterv2->start);
					sorted_intervs.erase(iinterv2);

					if (interv.start != interv.end)
						sorted_intervs.insert(interv);
				} else { // interval1 contains interval2 => split interval1
					ChainInterval interv(iinterv1->chromid, iinterv1->end + iinterv2->end - iinterv2->start, tgt_end1,
							iinterv1->chromid_src, iinterv1->start_src + iinterv2->end - iinterv1->start);
					sorted_intervs.erase(iinterv2);
					if (interv.start != interv.end)
						sorted_intervs.insert(interv);
				}

				if (iinterv1->start == iinterv1->end) {
					iinterv2 = iinterv1;
					--iinterv2;
					sorted_intervs.erase(iinterv1);
				} else
					iinterv2 = iinterv1;
			}
			iinterv1 = iinterv2;
			++iinterv2;
		}

		// pack the answer
		if (sorted_intervs.empty())
			return R_NilValue;

		chain_intervs.clear();
		for (set<ChainInterval>::const_iterator iinterval = sorted_intervs.begin(); iinterval != sorted_intervs.end(); ++iinterval)
			chain_intervs.push_back(*iinterval);

		return iu.convert_chain_intervs(chain_intervs, id2chrom);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

}
