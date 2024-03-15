#include <cstdint>
#include <stdio.h>

#include <unordered_map>
#include <string>
#include <vector>

#include "rdbinterval.h"
#include "rdbutils.h"
#include "rdbprogress.h"

#include "BufferedFile.h"
#include "HashFunc.h"

#include "GenomeTrack.h"
#include "GenomeTrackFixedBin.h"
#include "GenomeTrackSparse.h"
#include "TrackExpressionScanner.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP C_gcompute_strands_autocorr(SEXP _infile, SEXP _chrom, SEXP _binsize, SEXP _maxread, SEXP _cols_order, SEXP _min_coord, SEXP _max_coord, SEXP _envir)
{
	enum { SEQ_COL, CHROM_COL, COORD_COL, STRAND_COL, NUM_COLS };
	const char *COL_NAMES[NUM_COLS] = { "sequence", "chromosome", "coordinate", "strand" };
	const int MAX_COV = 10;

	try {
		RdbInitializer rdb_init;

		if (!isString(_infile) || length(_infile) != 1)
			verror("file argument is not a string");

		if (!isString(_chrom) || length(_chrom) != 1)
			verror("chrom argument is not a string");

		if (length(_binsize) != 1 || ((!isReal(_binsize) || REAL(_binsize)[0] != (int)REAL(_binsize)[0]) && !isInteger(_binsize)))
			verror("binsize argument is not an integer");

		if (length(_maxread) != 1 || ((!isReal(_maxread) || REAL(_maxread)[0] != (int)REAL(_maxread)[0]) && !isInteger(_maxread)))
			verror("maxread argument is not an integer");

		if (length(_cols_order) != NUM_COLS || (!isReal(_cols_order) && !isInteger(_cols_order)))
			verror("cols.order argument must be a vector with %d numeric values", NUM_COLS);

		if (length(_min_coord) != 1 || ((!isReal(_min_coord) || REAL(_min_coord)[0] != (int)REAL(_min_coord)[0]) && !isInteger(_min_coord)))
			verror("min_coord argument is not an integer");

		if (length(_max_coord) != 1 || ((!isReal(_max_coord) || REAL(_max_coord)[0] != (int)REAL(_max_coord)[0]) && !isInteger(_max_coord)))
			verror("max_coord argument is not an integer");

		if (isReal(_cols_order)) {
			for (int i = 0; i < NUM_COLS; i++) {
				if (REAL(_cols_order)[i] != (int)REAL(_cols_order)[i])
					verror("cols.order is not an integer");
			}
		}

		const char *infilename = CHAR(STRING_ELT(_infile, 0));
		const char *chrom = CHAR(STRING_ELT(_chrom, 0));
		double binsize = isReal(_binsize) ? (int)REAL(_binsize)[0] : INTEGER(_binsize)[0];
		double maxread = isReal(_maxread) ? (int)REAL(_maxread)[0] : INTEGER(_maxread)[0];
		int cols_order[NUM_COLS];
		int64_t min_coord;
		int64_t max_coord;

		for (int i = 0; i < NUM_COLS; i++)
			cols_order[i] = isReal(_cols_order) ? (int)REAL(_cols_order)[i] : INTEGER(_cols_order)[i];

		if (binsize <= 0)
			verror("Invalid binsize value %g", binsize);

		if (maxread <= 0)
			verror("Invalid maxread value %g", binsize);

		for (int i = 0; i < NUM_COLS; i++) {
			if (cols_order[i] <= 0)
				verror("Invalid columns order: %s column's order is %d", COL_NAMES[i]);

			for (int j = i + 1; j < NUM_COLS; j++) {
				if (cols_order[i] == cols_order[j]) {
					verror("Invalid columns order: %s column has the same order as %s column", COL_NAMES[i], COL_NAMES[j]);
				}
			}
		}

		min_coord = (int64_t)(isReal(_min_coord) ? REAL(_min_coord)[0] : INTEGER(_min_coord)[0]);
		max_coord = (int64_t)(isReal(_max_coord) ? REAL(_max_coord)[0] : INTEGER(_max_coord)[0]);

		IntervUtils iu(_envir);
		int chromid = iu.chrom2id(chrom);
		int64_t chromsize = 0;
		GIntervals all_genome_intervs;

		iu.get_all_genome_intervs(all_genome_intervs);
		for (GIntervals::const_iterator iinterv = all_genome_intervs.begin(); iinterv != all_genome_intervs.end(); ++iinterv) {
			if (iinterv->chromid == chromid) {
				chromsize = iinterv->end;
				break;
			}
		}

		if (min_coord < 0)
			min_coord = 0;
		if (max_coord < 0 || max_coord > chromsize)
			max_coord = chromsize;

		vector<int> forward((uint64_t)ceil(chromsize / binsize), 0);
		vector<int> reverse((uint64_t)ceil(chromsize / binsize), 0);

		BufferedFile infile;
		infile.open(infilename, "r");

		if (infile.error())
			verror("Failed to open file %s: %s", infilename, strerror(errno));

		int col = 1;
		int active_col_idx = -1;
		int c;
		string str[NUM_COLS];
		int min_off = (int)(-maxread / binsize);
		int max_off = (int)(maxread / binsize);
		uint64_t min_idx = (uint64_t)(max_off + min_coord / binsize);
		uint64_t max_idx = (uint64_t)(max_coord / binsize - max_off - 1);

		if (min_idx >= forward.size() || (int64_t)max_idx < 0)
			verror("Not enough data to calculate auto correlation.");

		Progress_reporter progress;
		progress.init(infile.file_size() + max_idx - min_idx, 1000000);

		for (int i = 0; i < NUM_COLS; i++) {
			if (cols_order[i] == 1) {
				active_col_idx = i;
				break;
			}
		}

		while (1) {
			c = infile.getc();
			if (c == '\n' || c == EOF || c == '\t') {
				if (c == '\n' || c == EOF) {
					int num_nonempty_strs = 0;

					for (int i = 0; i < NUM_COLS; i++) {
						if (!str[i].empty())
							num_nonempty_strs++;
					}

					while (num_nonempty_strs == NUM_COLS) {
						unordered_map<string, int>::iterator istr2chrom;						
						int64_t coord;
						char *endptr;

						if (strcmp(str[CHROM_COL].c_str(), chrom))
							break;

						coord = strtoll(str[COORD_COL].c_str(), &endptr, 10);
						if (*endptr || coord < 0 || coord >= chromsize)
							break;

						if (coord < min_coord || coord > max_coord)
							break;

						if (str[STRAND_COL] == "+" || str[STRAND_COL] == "F") {
							uint64_t idx = (uint64_t)(coord / binsize);
							forward[idx] = min(MAX_COV, forward[idx] + 1);
						}
						else if (str[STRAND_COL] == "-" || str[STRAND_COL] == "R") {
							uint64_t idx = (uint64_t)((coord + str[SEQ_COL].size()) / binsize);
							reverse[idx] = min(MAX_COV, reverse[idx] + 1);
						}

						break;
					}

					if (c == EOF)
						break;

					if (num_nonempty_strs > 0) {
						for (int i = 0; i < NUM_COLS; i++)
							str[i].clear();
					}
					col = 1;
				} else
					col++;

				active_col_idx = -1;
				for (int i = 0; i < NUM_COLS; i++) {
					if (cols_order[i] == col) {
						active_col_idx = i;
						break;
					}
				}
			} else if (active_col_idx >= 0)
				str[active_col_idx].push_back(c);

			check_interrupt();
			progress.report(1);
		}

		if (infile.error())
			verror("Error while reading file %s: %s", infilename, strerror(errno));

		int64_t count = 0;
		int64_t tot_f = 0;
		int64_t tot_r = 0;
		int64_t tot_ff = 0;
		int64_t tot_rr = 0;
		vector<double> tot_fr(max_off - min_off);

		for (uint64_t i = min_idx; i < max_idx; i++) {
			int cur_fr = forward[i];
			int cur_rv = reverse[i];
			tot_f += cur_fr;
			tot_r += cur_rv;
			count++;
			tot_rr += cur_rv * cur_rv;
			tot_ff += cur_fr * cur_fr;
			for (int off = min_off; off < max_off; off++)
				tot_fr[off - min_off] += cur_fr * reverse[i + off];
			progress.report(1);
		}

		progress.report_last();

		double mean_f = tot_f / (double)count;
		double mean_r = tot_r / (double)count;
		double std_f = sqrt(tot_ff / (double)count - mean_f * mean_f);
		double std_r = sqrt(tot_rr / (double)count - mean_r * mean_r);

		SEXP total_stat;
		SEXP bin_stat;
		SEXP bin_idx;
		SEXP corr;
        SEXP row_names;
        SEXP total_stat_names;
        SEXP rnames;
		SEXP answer;

		rprotect(total_stat = RSaneAllocVector(REALSXP, 4));
		REAL(total_stat)[0] = mean_f;
		REAL(total_stat)[1] = std_f;
		REAL(total_stat)[2] = mean_r;
		REAL(total_stat)[3] = std_r;

        rprotect(total_stat_names = RSaneAllocVector(STRSXP, 4));
        SET_STRING_ELT(total_stat_names, 0, mkChar("Forward mean"));
        SET_STRING_ELT(total_stat_names, 1, mkChar("Forward stdev"));
        SET_STRING_ELT(total_stat_names, 2, mkChar("Reverse mean"));
        SET_STRING_ELT(total_stat_names, 3, mkChar("Reverse stdev"));
        setAttrib(total_stat, R_NamesSymbol, total_stat_names);

		rprotect(bin_stat = RSaneAllocVector(VECSXP, 2));
        rprotect(bin_idx = RSaneAllocVector(REALSXP, max_off - min_off));
        rprotect(corr = RSaneAllocVector(REALSXP, max_off - min_off));
        rprotect(row_names = RSaneAllocVector(INTSXP, max_off - min_off));
        rprotect(rnames = RSaneAllocVector(STRSXP, 2));
        rprotect(answer = RSaneAllocVector(VECSXP, 2));

		for (int off = min_off; off < max_off; off++) {
			REAL(bin_idx)[off - min_off] = off;
			REAL(corr)[off - min_off] = (tot_fr[off - min_off] / (double)count - mean_f * mean_r) / (std_f * std_r);
			INTEGER(row_names)[off - min_off] = off - min_off + 1;
		}

        SET_VECTOR_ELT(bin_stat, 0, bin_idx);
        SET_VECTOR_ELT(bin_stat, 1, corr);

        SET_STRING_ELT(rnames, 0, mkChar("bin"));
        SET_STRING_ELT(rnames, 1, mkChar("corr"));

        setAttrib(bin_stat, R_RowNamesSymbol, row_names);
		setAttrib(bin_stat, R_NamesSymbol, rnames);
        setAttrib(bin_stat, R_ClassSymbol, mkString("data.frame"));

		SET_VECTOR_ELT(answer, 0, total_stat);
		SET_VECTOR_ELT(answer, 1, bin_stat);

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

}
