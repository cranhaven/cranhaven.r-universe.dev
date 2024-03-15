#include <cstdint>
#include <unordered_map>

#include "port.h"

#include "rdbinterval.h"
#include "rdbprogress.h"
#include "rdbutils.h"
#include "strutil.h"
#include "BufferedFile.h"

using namespace std;
using namespace rdb;

typedef vector<string> Annots;
typedef unordered_map<string, Annots> Id2Annots;

void read_annots_file(const char *annots_fname, Id2Annots &id2annots, int num_annots)
{
	BufferedFile annots_file;
	vector<string> fields;
	int lineno = 0;

	if (annots_file.open(annots_fname, "r"))
		verror("Failed to open file %s: %s", annots_file.file_name().c_str(), strerror(errno));

	while (1) {
		lineno += split_line(annots_file, fields, '\t', num_annots + 1);

		if (annots_file.error())
			verror("Failed to read file %s: %s", annots_file.file_name().c_str(), strerror(errno));

		if (fields.empty())
			break;

		if (fields.size() < 1 || fields[0].empty())
			verror("Annotation file %s, line %d: invalid format\n", annots_file.file_name().c_str(), lineno);

		if (fields.size() != (uint64_t)num_annots)
			verror("Annotation file %s, line %d: number of annotations in file (%ld)\n"
				   "does not match the number of annotations in annots.name argument (%d)",
				   annots_file.file_name().c_str(), lineno, fields.size(), num_annots);

		 if (id2annots.find(fields[0]) != id2annots.end())
			 verror("Annotation file %s: id %s appears more than once", annots_file.file_name().c_str(), fields[0].c_str());

		 id2annots.insert(Id2Annots::value_type(fields[0], vector<string>(fields.begin(), fields.end())));
	}
}

void read_genes_file(const char *genes_fname, GIntervals &tss, GIntervals &exons, GIntervals &utr3, GIntervals &utr5,
					 const Id2Annots &id2annots, const GenomeChromKey &chromkey)
{
	enum { ID, CHROM, STRAND, TXSTART, TXEND, CDSSTART, CDSEND, EXONCOUNT, EXONSTARTS, EXONENDS, PROTEINID, ALIGNID, NUM_COLS };

	BufferedFile genes_file;
	vector<string> fields;
	vector<int64_t> exon_starts;
	vector<int64_t> exon_ends;
	int lineno = 0;
	char *endptr;

	if (genes_file.open(genes_fname, "r"))
		verror("Failed to open file %s: %s", genes_file.file_name().c_str(), strerror(errno));

	while (1) {
		lineno += split_line(genes_file, fields, '\t', NUM_COLS);

		if (genes_file.error())
			verror("Failed to read file %s: %s", genes_file.file_name().c_str(), strerror(errno));

		if (fields.empty())
			break;

		if (fields.size() != NUM_COLS || fields[ID].empty() || fields[CHROM].empty() || fields[STRAND].empty() || fields[EXONCOUNT].empty() ||
			fields[TXSTART].empty() || fields[TXEND].empty()) 
			verror("Genes file %s, line %d: invalid file format", genes_file.file_name().c_str(), lineno);

		// CHROM
		int chromid;

		try {
			chromid = chromkey.chrom2id(fields[CHROM]); 
		} catch (TGLException &) {
			continue;    // chromosome might not be presented: it's normal
		}

		// STRAND
		char strand = 0;
		if (fields[STRAND] == "+")
			strand = 1;
		else if (fields[STRAND] == "-")
			strand = -1;
		else
			verror("Genes file %s, line %d: invalid strand value", genes_file.file_name().c_str(), lineno);

		// TXSTART
		int64_t txstart = strtoll(fields[TXSTART].c_str(), &endptr, 10);
		if (*endptr)
			verror("Genes file %s, line %d: invalid txStart value", genes_file.file_name().c_str(), lineno);

		// TXEND
		int64_t txend = strtoll(fields[TXEND].c_str(), &endptr, 10);
		if (*endptr)
			verror("Genes file %s, line %d: invalid txEnd value", genes_file.file_name().c_str(), lineno);

		// EXONCOUNT
		int exoncount = strtol(fields[EXONCOUNT].c_str(), &endptr, 10);
		if (*endptr || exoncount < 0)
			verror("Genes file %s, line %d: invalid exonCount value", genes_file.file_name().c_str(), lineno);

		// EXONSTARTS
		exon_starts.resize(0);
		endptr = (char *)fields[EXONSTARTS].c_str();
		for (int i = 0; i < exoncount; ++i) {
			exon_starts.push_back(strtoll(endptr, &endptr, 10));
			if (*endptr != ',') 
				verror("Genes file %s, line %d: invalid exonStarts value", genes_file.file_name().c_str(), lineno);
			endptr++;
		}
		if (exon_starts.size() != (uint64_t)exoncount) 
			verror("Genes file %s, line %d: number of exonStarts values does not match exonCount", genes_file.file_name().c_str(), lineno);

		// EXONENDS
		exon_ends.resize(0);
		endptr = (char *)fields[EXONENDS].c_str();
		for (int i = 0; i < exoncount; ++i) {
			exon_ends.push_back(strtoll(endptr, &endptr, 10));
			if (*endptr != ',') 
				verror("Genes file %s, line %d: invalid exonEnds value", genes_file.file_name().c_str(), lineno);
			endptr++;
		}
		if (exon_ends.size() != (uint64_t)exoncount) 
			verror("Genes file %s, line %d: number of exonEnds values does not match exonCount", genes_file.file_name().c_str(), lineno);

		const Id2Annots::const_iterator itr = id2annots.find(fields[ID]);
		const Annots *annots = itr == id2annots.end() ? NULL : &itr->second;
		GInterval interv(chromid, -1, -1, strand, (void *)annots);

		// add tss
		if (strand == 1) {
			interv.start = txstart;
			interv.end = txstart + 1;
		} else {
			interv.start = txend - 1;
			interv.end = txend;
		}
		interv.verify(chromkey);
		tss.push_back(interv);

		// add exons
		for (int i = 0; i < exoncount; ++i) {
			interv.start = exon_starts[i];
			interv.end = exon_ends[i];
			interv.verify(chromkey);
			exons.push_back(interv);
		}

		// add 3utr
		if (txend >= 0 && exoncount > 0) {
			if (strand == 1) {
				interv.start = exon_ends[exoncount - 1] - 1;
				interv.end = txend;
			} else {
				interv.start = txstart;
				interv.end = exon_starts[0] + 1;
			}
			interv.verify(chromkey);
			utr3.push_back(interv);
		}

		// add 5utr
		if (txstart >= 0 && exoncount > 0) {
			if (strand == 1) {
				interv.start = txstart;
				interv.end = exon_starts[0] + 1;
			} else {
				interv.start = exon_ends[exoncount - 1] - 1;
				interv.end = txend;
			}
			interv.verify(chromkey);
			utr5.push_back(interv);
		}
	}
}

extern "C" {

SEXP gintervals_import_genes(SEXP _genes_fname, SEXP _annots_fname, SEXP _annots_names, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_genes_fname) || length(_genes_fname) != 1)
			verror("genes.file argument is not a string");

		if (!isNull(_annots_fname) && (!isString(_annots_fname) || length(_annots_fname) != 1))
			verror("annots.file argument is not a string");

		if (isNull(_annots_names) && !isNull(_annots_fname))
			verror("annots.names argument cannot be NULL if annots.file is specified");

		if (!isNull(_annots_names) && !isString(_annots_names))
			verror("annots.names argument must be a vector strings");

		const char *genes_fname = CHAR(STRING_ELT(_genes_fname, 0));
		const char *annots_fname = isNull(_annots_fname) ? NULL : CHAR(STRING_ELT(_annots_fname, 0));
		int num_annots = isNull(_annots_names) ? 0 : length(_annots_names);

		IntervUtils iu(_envir);
		Id2Annots id2annots;
		GIntervals tss;
		GIntervals exons;
		GIntervals utr3;
		GIntervals utr5;

		if (annots_fname)
			read_annots_file(annots_fname, id2annots, length(_annots_names));

		read_genes_file(genes_fname, tss, exons, utr3, utr5, id2annots, iu.get_chromkey());

		// assemble the answer
		enum { TSS, EXONS, UTR3, UTR5, NUM_INTERVS_SETS };
		const char *intervs_sets_names[NUM_INTERVS_SETS] = { "tss", "exons", "utr3", "utr5" };
		GIntervals *intervs_sets[NUM_INTERVS_SETS] = { &tss, &exons, &utr3, &utr5 };

		SEXP answer;
		SEXP answer_col_names;
		rprotect(answer = RSaneAllocVector(VECSXP, NUM_INTERVS_SETS));
        rprotect(answer_col_names = RSaneAllocVector(STRSXP, NUM_INTERVS_SETS));

		for (int iintervs_set = 0; iintervs_set < NUM_INTERVS_SETS; ++iintervs_set) {
			GIntervals &intervs = *intervs_sets[iintervs_set];

            if (intervs.empty()) {
                SET_VECTOR_ELT(answer, iintervs_set, R_NilValue);
                continue;
            }

            intervs.sort();

			// determine the number of intervals after unification (but do not unify them)
			int intervs_size = 1;
			GInterval last_interv = intervs.front();

            for (GIntervals::const_iterator iinterv = intervs.begin() + 1; iinterv < intervs.end(); ++iinterv) {
                if (last_interv.chromid != iinterv->chromid || last_interv.end <= iinterv->start) {
                    intervs_size++;
                    last_interv = *iinterv;
                } else if (last_interv.end < iinterv->end)
                    last_interv.end = iinterv->end;
            }

			// add the intervals to the answer
			unsigned num_chroms = iu.get_chromkey().get_num_chroms();
            SEXP rintervs;
            SEXP chroms, chroms_idx, starts, ends, strands;
            SEXP row_names;
            SEXP col_names;
            vector<SEXP> rannots(num_annots);

            rprotect(rintervs = RSaneAllocVector(VECSXP, GInterval::NUM_COLS + num_annots + 1));
            rprotect(chroms_idx = RSaneAllocVector(INTSXP, intervs_size));
            rprotect(starts = RSaneAllocVector(REALSXP, intervs_size));
            rprotect(ends = RSaneAllocVector(REALSXP, intervs_size));
            rprotect(strands = RSaneAllocVector(REALSXP, intervs_size));

            for (int iannot = 0; iannot < num_annots; ++iannot)
                rprotect(rannots[iannot] = RSaneAllocVector(STRSXP, intervs_size));

            rprotect(chroms = RSaneAllocVector(STRSXP, num_chroms));
            rprotect(col_names = RSaneAllocVector(STRSXP, GInterval::NUM_COLS + num_annots + 1));
            rprotect(row_names = RSaneAllocVector(INTSXP, intervs_size));

            intervs_size = 0;
			last_interv = intervs.front();
			GIntervals::const_iterator first_overlap = intervs.begin();
			vector< set<string> > annots_set(num_annots); // used to eliminate identical values in overlapping intervals

			for (int iannot = 0; iannot < num_annots; ++iannot) {
                if (first_overlap->udata && !(*(const Annots *)first_overlap->udata)[iannot].empty())
                    annots_set[iannot].insert((*(const Annots *)first_overlap->udata)[iannot]);
            }

			for (GIntervals::const_iterator iinterv = intervs.begin() + 1; ; ++iinterv) {
				if (iinterv >= intervs.end() || last_interv.chromid != iinterv->chromid || last_interv.end <= iinterv->start) {
                    INTEGER(chroms_idx)[intervs_size] = last_interv.chromid + 1;
                    REAL(starts)[intervs_size] = last_interv.start;
                    REAL(ends)[intervs_size] = last_interv.end;
                    REAL(strands)[intervs_size] = last_interv.strand;
                    INTEGER(row_names)[intervs_size] = intervs_size + 1;

					for (int iannot = 0; iannot < num_annots; ++iannot) {
						set<string> &annots = annots_set[iannot];
						string annot;

                        for (set<string>::const_iterator itr = annots.begin(); itr != annots.end(); ++itr) {
                            if (itr == annots.begin())
                                annot = *itr;
                            else {
                                annot += ";";
                                annot += *itr;
                            }
                        }

                        SET_STRING_ELT(rannots[iannot], intervs_size, mkChar(annot.c_str()));
					}

					intervs_size++;

					if (iinterv >= intervs.end()) 
						break;

                    last_interv = *iinterv;
                    first_overlap = iinterv;

                    for (int iannot = 0; iannot < num_annots; ++iannot) {
                        set<string> &annots = annots_set[iannot];
                        annots.clear();
                        if (iinterv->udata && !(*(const Annots *)iinterv->udata)[iannot].empty())
                            annots.insert((*(const Annots *)iinterv->udata)[iannot]);
                    }
				} else {
                    if (last_interv.strand != iinterv->strand)
                        last_interv.strand = 0;

                    if (last_interv.end < iinterv->end)
                        last_interv.end = iinterv->end;

                    for (int iannot = 0; iannot < num_annots; ++iannot) {
                        if (iinterv->udata && !(*(const Annots *)iinterv->udata)[iannot].empty())
                            annots_set[iannot].insert((*(const Annots *)iinterv->udata)[iannot]);
                    }
				}
			}

            for (unsigned id = 0; id < (unsigned)num_chroms; ++id)
                SET_STRING_ELT(chroms, id, mkChar(iu.id2chrom(id).c_str()));

            for (int i = 0; i < GInterval::NUM_COLS; i++)
                SET_STRING_ELT(col_names, i, mkChar(GInterval::COL_NAMES[i]));

            SET_STRING_ELT(col_names, GInterval::NUM_COLS, mkChar("strand"));

            for (int i = 0; i < num_annots; ++i)
                SET_STRING_ELT(col_names, i + GInterval::NUM_COLS + 1, STRING_ELT(_annots_names, i));

            SET_STRING_ELT(answer_col_names, iintervs_set, mkChar(intervs_sets_names[iintervs_set]));

            SET_VECTOR_ELT(rintervs, GInterval::CHROM, chroms_idx);
            SET_VECTOR_ELT(rintervs, GInterval::START, starts);
            SET_VECTOR_ELT(rintervs, GInterval::END, ends);
            SET_VECTOR_ELT(rintervs, GInterval::NUM_COLS, strands);
            for (int iannot = 0; iannot < num_annots; ++iannot)
                SET_VECTOR_ELT(rintervs, GInterval::NUM_COLS + 1 + iannot, rannots[iannot]);

            setAttrib(chroms_idx, R_LevelsSymbol, chroms);
            setAttrib(chroms_idx, R_ClassSymbol, mkString("factor"));

            setAttrib(rintervs, R_NamesSymbol, col_names);
            setAttrib(rintervs, R_ClassSymbol, mkString("data.frame"));
            setAttrib(rintervs, R_RowNamesSymbol, row_names);

            SET_VECTOR_ELT(answer, iintervs_set, rintervs);
        }
        setAttrib(answer, R_NamesSymbol, answer_col_names);

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

}
