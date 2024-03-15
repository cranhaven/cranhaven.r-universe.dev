#include "port.h"

#include "rdbinterval.h"
#include "rdbprogress.h"
#include "rdbutils.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP gintervs_liftover(SEXP _src_intervs, SEXP _chain, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

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

		GIntervals src_intervs1d;
		GIntervals2D src_intervs2d;
		iu.convert_rintervs(_src_intervs, &src_intervs1d, &src_intervs2d, false, &src_chromkey);
		src_intervs1d.sort();
		src_intervs2d.sort();

		vector<int> src_indices;
		GIntervals tgt_intervs1d;
		GIntervals2D tgt_intervs2d;

		// 1D intervals
		if (!src_intervs1d.empty()) {
			GIntervals tmp_tgt_intervs;
			ChainIntervals::const_iterator hint = chain_intervs.begin();

			for (GIntervals::const_iterator isrc_interv = src_intervs1d.begin(); isrc_interv != src_intervs1d.end(); ++isrc_interv) {
				hint = chain_intervs.map_interval(*isrc_interv, tmp_tgt_intervs, hint);
				if (!tmp_tgt_intervs.empty()) {
					tgt_intervs1d.insert(tgt_intervs1d.end(), tmp_tgt_intervs.begin(), tmp_tgt_intervs.end());
					src_indices.insert(src_indices.end(), tmp_tgt_intervs.size(), iu.get_orig_interv_idx(*isrc_interv) + 1);
					iu.verify_max_data_size(tgt_intervs1d.size(), "Result");
				}
			}
		}
		// 2D intervals
		else {
			GInterval src_intervs[2];
			GIntervals tmp_tgt_intervs[2];
			ChainIntervals::const_iterator hints[2] = { chain_intervs.begin(), chain_intervs.begin() };

			for (GIntervals2D::const_iterator isrc_interv = src_intervs2d.begin(); isrc_interv != src_intervs2d.end(); ++isrc_interv) {
				src_intervs[0].chromid = isrc_interv->chromid1();
				src_intervs[0].start = isrc_interv->start1();
				src_intervs[0].end = isrc_interv->end1();
				src_intervs[1].chromid = isrc_interv->chromid2();
				src_intervs[1].start = isrc_interv->start2();
				src_intervs[1].end = isrc_interv->end2();

				hints[0] = chain_intervs.map_interval(src_intervs[0], tmp_tgt_intervs[0], hints[0]);
				hints[1] = chain_intervs.map_interval(src_intervs[1], tmp_tgt_intervs[1], hints[1]);

				for (GIntervals::const_iterator iinterv1 = tmp_tgt_intervs[0].begin(); iinterv1 != tmp_tgt_intervs[0].end(); ++iinterv1) {
					for (GIntervals::const_iterator iinterv2 = tmp_tgt_intervs[1].begin(); iinterv2 != tmp_tgt_intervs[1].end(); ++iinterv2) {
						tgt_intervs2d.push_back(GInterval2D(iinterv1->chromid, iinterv1->start, iinterv1->end, iinterv2->chromid, iinterv2->start, iinterv2->end));
						src_indices.push_back(iu.get_orig_interv_idx(*isrc_interv) + 1);
						iu.verify_max_data_size(tgt_intervs2d.size(), "Result");
					}
				}
			}
		}

		// assemble the answer
		SEXP answer;
		unsigned num_interv_cols;

		if (!tgt_intervs1d.empty()) {
			answer = iu.convert_intervs(&tgt_intervs1d, GInterval::NUM_COLS + 1);
			num_interv_cols = GInterval::NUM_COLS;
		} else if (!tgt_intervs2d.empty()) {
			answer = iu.convert_intervs(&tgt_intervs2d, GInterval2D::NUM_COLS + 1);
			num_interv_cols = GInterval2D::NUM_COLS;
		} else
			return R_NilValue;

		SEXP rsrc_indices;
		SEXP col_names = getAttrib(answer, R_NamesSymbol);

        rprotect(rsrc_indices = RSaneAllocVector(INTSXP, src_indices.size()));

		for (vector<int>::const_iterator iindex = src_indices.begin(); iindex != src_indices.end(); ++iindex)
			INTEGER(rsrc_indices)[iindex - src_indices.begin()] = *iindex;

		SET_STRING_ELT(col_names, num_interv_cols, mkChar("intervalID"));
        SET_VECTOR_ELT(answer, num_interv_cols, rsrc_indices);

		return answer;

	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

}
