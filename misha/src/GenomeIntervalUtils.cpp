/*
 * GenomeIntervalUtils.cpp
 *
 *  Created on: Aug 5, 2010
 *      Author: hoichman
 */

#include <cstdint>
#include <string>
#include <vector>

#include "GenomeTrack.h"
#include "GenomeTrackArrays.h"
#include "GenomeTrackComputed.h"
#include "GenomeTrackRects.h"
#include "GenomeTrackSparse.h"
#include "GIntervalsMeta2D.h"
#include "GIntervalsBigSet1D.h"
#include "GIntervalsBigSet2D.h"
#include "rdbinterval.h"
#include "rdbutils.h"
#include "StatQuadTree.h"

using namespace std;
using namespace rdb;

struct ImportedInterval {
	GInterval       interv;
	vector<int64_t> origin_ids;

	ImportedInterval(const GInterval &_interv, const vector<int64_t> &_origin_ids) : interv(_interv), origin_ids(_origin_ids) {}

	bool operator<(const ImportedInterval &obj) const { return interv.chromid < obj.interv.chromid || (interv.chromid == obj.interv.chromid && interv.start < obj.interv.start); }
};

typedef std::vector<ImportedInterval> ImportedIntervals;


extern "C" {

SEXP grbind(SEXP _objs, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		IntervUtils iu(_envir);

		if (!isVector(_objs))
			verror("Argument for grbind is not a list");

		SEXP answer;
		uint64_t numrows = 0;
		uint64_t numcols = 0;

		for (uint64_t i = 0; i < (uint64_t)length(_objs); ++i) {
			SEXP obj = VECTOR_ELT(_objs, i);
			SEXP src_class = getAttrib(obj, R_ClassSymbol);

			if (isNull(src_class) || !isString(src_class) || length(src_class) != 1 || strcmp(CHAR(STRING_ELT(src_class, 0)), "data.frame"))
				verror("Object for grbind is not a data frame");

			if (i) {
				if (numcols != (uint64_t)length(obj)) 
					verror("Data frames for grbind differ in the number of columns");
			} else
				numcols = length(obj);

			if (numcols) 
				numrows += length(VECTOR_ELT(obj, 0));
		}

		// In complience with R additional attributes are copied from the first object in rbind
		answer = iu.create_data_frame(numrows, numcols, length(_objs) ? VECTOR_ELT(_objs, 0) : R_NilValue);
		vector<SEXP> src_cols;
		vector<SEXP> tgt_cols;
		iu.define_data_frame_cols(VECTOR_ELT(_objs, 0), src_cols, answer, tgt_cols, 0);

		uint64_t tgtrow = 0;

		for (int i = 0; i < length(_objs); ++i) {
			SEXP obj = VECTOR_ELT(_objs, i);
			uint64_t srcnumrows = length(VECTOR_ELT(obj, 0));

			for (int j = 0; j < length(obj); ++j) 
				src_cols[j] = VECTOR_ELT(obj, j);

			iu.copy_data_frame_rows(src_cols, 0, srcnumrows, tgt_cols, tgtrow, 0);
			tgtrow += srcnumrows;
		}

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP gintervsort(SEXP _intervs, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		IntervUtils iu(_envir);
		GIntervals intervs;
		GIntervals2D intervs2d;
		iu.convert_rintervs(_intervs, &intervs, &intervs2d);

		if (!intervs.empty()) {
			SEXP rcolnames = getAttrib(_intervs, R_NamesSymbol);
			int strand_col;

			for (strand_col = 0; strand_col < length(rcolnames); ++strand_col) {
				if (!strcmp(CHAR(STRING_ELT(rcolnames, strand_col)), "strand"))
					break;
			}
			intervs.sort();

			if (strand_col == length(_intervs))
				return iu.convert_intervs(&intervs);

			SEXP answer = iu.convert_intervs(&intervs, GInterval::NUM_COLS + 1);
			SEXP strands;

			rprotect(strands = RSaneAllocVector(INTSXP, intervs.size()));
			for (GIntervals::const_iterator interv = intervs.begin(); interv != intervs.end(); ++interv)
				INTEGER(strands)[interv - intervs.begin()] = interv->strand;

			SET_VECTOR_ELT(answer, GInterval::NUM_COLS, strands);
			SET_STRING_ELT(getAttrib(answer, R_NamesSymbol), GInterval::NUM_COLS, mkChar("strand"));
			return answer;
		}

		intervs2d.sort();
		return iu.convert_intervs(&intervs2d);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

// intervals must be in canonic form
SEXP gintervunion(SEXP _intervs1, SEXP _intervs2, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		IntervUtils iu(_envir);
		GIntervals res_intervs;
		GIntervals intervs[2];
		iu.convert_rintervs(_intervs1, &intervs[0], NULL);
		iu.convert_rintervs(_intervs2, &intervs[1], NULL);
		intervs[0].sort();
		intervs[1].sort();
		intervs[0].unify_overlaps();
		intervs[1].unify_overlaps();
		GIntervals::unify(intervs[0], intervs[1], res_intervs);
		return iu.convert_intervs(&res_intervs);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

// intervals must be in canonic form
SEXP gintervintersect(SEXP _intervs1, SEXP _intervs2, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		IntervUtils iu(_envir);
		GIntervals intervs[2];
		GIntervals2D intervs2d[2];
		iu.convert_rintervs(_intervs1, &intervs[0], &intervs2d[0]);
		iu.convert_rintervs(_intervs2, &intervs[1], &intervs2d[1]);
		intervs[0].sort();
		intervs[1].sort();
		intervs[0].unify_overlaps();
		intervs[1].unify_overlaps();
		intervs2d[0].sort();
		intervs2d[1].sort();
		intervs2d[0].verify_no_overlaps(iu.get_chromkey());
		intervs2d[1].verify_no_overlaps(iu.get_chromkey());

		if ((intervs[0].empty() && intervs2d[0].empty()) || (intervs[1].empty() && intervs2d[1].empty()))
			return R_NilValue;

		if ((!intervs[0].empty() && intervs[1].empty()) || (intervs[0].empty() && !intervs[1].empty()) ||
            (!intervs2d[0].empty() && intervs2d[1].empty()) || (intervs2d[0].empty() && !intervs2d[1].empty()))
			verror("Cannot intersect 1D intervals with 2D intervals");

		if (!intervs[0].empty()) {
			GIntervals res_intervs;
			GIntervals::intersect(intervs[0], intervs[1], res_intervs);

			if (res_intervs.empty())
				return R_NilValue;

			return iu.convert_intervs(&res_intervs);
		}

		// 2D
		GIntervals2D res_intervs;

		// build the minimal quad tree
		if (intervs2d[0].size() > intervs2d[1].size())
			intervs2d[0].swap(intervs2d[1]);

		GIntervals2D::const_iterator interv1 = intervs2d[0].begin();
		GIntervals2D::const_iterator interv2 = intervs2d[1].begin();
		Rectangles       intersection;
		vector<uint64_t> intersected_objs_indices;

		while (interv1 != intervs2d[0].end() && interv2 != intervs2d[1].end()) {
			RectsQuadTree qtree(0, 0, iu.get_chromkey().get_chrom_size(interv1->chromid1()), iu.get_chromkey().get_chrom_size(interv1->chromid2()));

			do {
				qtree.insert(*interv1);
				++interv1;
			} while (interv1 != intervs2d[0].end() && interv1->is_same_chrom(*(interv1 - 1)));

			while (interv2 != intervs2d[1].end() && *interv2 < *(interv1 - 1))
				++interv2;

			for (; interv2 != intervs2d[1].end() && interv2->is_same_chrom(*(interv1 - 1)); ++interv2) {
				qtree.intersect(*interv2, intersection, intersected_objs_indices);
				for (Rectangles::const_iterator iintersection = intersection.begin(); iintersection != intersection.end(); ++iintersection)
					res_intervs.push_back(GInterval2D(interv2->chromid1(), interv2->chromid2(), *iintersection));
			}
		}

		if (res_intervs.empty())
			return R_NilValue;

		return iu.convert_intervs(&res_intervs);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

// intervals must be in canonic form
SEXP gintervdiff(SEXP _intervs1, SEXP _intervs2, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		IntervUtils iu(_envir);
		GIntervals res_intervs;
		GIntervals intervs[2];
		iu.convert_rintervs(_intervs1, &intervs[0], NULL);
		iu.convert_rintervs(_intervs2, &intervs[1], NULL);
		intervs[0].sort();
		intervs[1].sort();
		intervs[0].unify_overlaps();
		intervs[1].unify_overlaps();
		GIntervals::diff(intervs[0], intervs[1], res_intervs);
		return iu.convert_intervs(&res_intervs);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP gintervcanonic(SEXP _intervs, SEXP _unify_touching_intervals, SEXP _envir)
{
	try {
        RdbInitializer rdb_init;

        if (!isLogical(_unify_touching_intervals) || xlength(_unify_touching_intervals) != 1)
            verror("unify_touching_intervals argument is not logical");

        bool unify_touching_intervals = LOGICAL(_unify_touching_intervals)[0] == 1;
        IntervUtils iu(_envir);
        GIntervals intervs;
        GIntervals2D intervs2d;
        iu.convert_rintervs(_intervs, &intervs, &intervs2d);

        if (!intervs2d.empty()) {
            intervs2d.sort();
            intervs2d.verify_no_overlaps(iu.get_chromkey());

            SEXP old2new_mapping;

            rprotect(old2new_mapping = RSaneAllocVector(REALSXP, intervs2d.size()));

            for (GIntervals2D::const_iterator interv = intervs2d.begin(); interv != intervs2d.end(); ++interv)
                REAL(old2new_mapping)[interv - intervs2d.begin()] = ((int64_t)interv->udata()) + 1;

			SEXP answer = iu.convert_intervs(&intervs2d);
			setAttrib(answer, install("mapping"), old2new_mapping);
			return answer;
		}

        if (intervs.empty())
            return R_NilValue;

        ImportedIntervals imported_intervs;
        vector<int64_t> origin_ids(1);
        imported_intervs.reserve(intervs.size());
        for (uint64_t interv = 0; interv < intervs.size(); ++interv) {
            origin_ids[0] = interv;
            imported_intervs.push_back(ImportedInterval(intervs[interv], origin_ids));
        }

        sort(imported_intervs.begin(), imported_intervs.end());
        uint64_t cur_idx = 0;

        for (uint64_t i = 1; i < imported_intervs.size(); i++) {
            if (imported_intervs[cur_idx].interv.chromid != imported_intervs[i].interv.chromid ||
                    imported_intervs[cur_idx].interv.end < imported_intervs[i].interv.start ||
                    (!unify_touching_intervals && imported_intervs[cur_idx].interv.end == imported_intervs[i].interv.start))
                imported_intervs[++cur_idx] = imported_intervs[i];
            // unite overlapping intervals
            else {
                if (imported_intervs[cur_idx].interv.end < imported_intervs[i].interv.end)
                    imported_intervs[cur_idx].interv.end = imported_intervs[i].interv.end;
                imported_intervs[cur_idx].origin_ids.push_back(imported_intervs[i].origin_ids.front());
            }
        }
        imported_intervs.erase(imported_intervs.begin() + cur_idx + 1, imported_intervs.end());

        // pack the result
        SEXP old2new_mapping;

        rprotect(old2new_mapping = RSaneAllocVector(REALSXP, intervs.size()));
        intervs.clear();

        for (ImportedIntervals::const_iterator iimported_interv = imported_intervs.begin(); iimported_interv != imported_intervs.end(); ++iimported_interv) {
            intervs.push_back(iimported_interv->interv);
            for (vector<int64_t>::const_iterator iid = iimported_interv->origin_ids.begin(); iid != iimported_interv->origin_ids.end(); ++iid)
                REAL(old2new_mapping)[*iid] = (iimported_interv - imported_intervs.begin()) + 1;
        }

        SEXP answer = iu.convert_intervs(&intervs);
        setAttrib(answer, install("mapping"), old2new_mapping);
        return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}


SEXP ginterv_intersectband(SEXP _intervs, SEXP _band, SEXP _intervals_set_out, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isNull(_intervals_set_out) && (!isString(_intervals_set_out) || length(_intervals_set_out) != 1))
			verror("intervals.set.out argument is not a string");

		string intervset_out = isNull(_intervals_set_out) ? "" : CHAR(STRING_ELT(_intervals_set_out, 0));

		IntervUtils iu(_envir);
		GIntervalsFetcher2D *intervs = NULL;
		iu.convert_rintervs(_intervs, NULL, &intervs);
		unique_ptr<GIntervalsFetcher2D> intervs_guard(intervs);
		intervs->sort();
		intervs->verify_no_overlaps(iu.get_chromkey());

		DiagonalBand band(iu.convert_band(_band));

		if (!band.is_non_empty_area())
			return _intervs;

		GIntervals2D res_intervs;
		int chromid1 = 0;
		int chromid2 = 0;
		bool go_on;
		vector<GIntervalsBigSet2D::ChromStat> chromstats;
		char error_prefix[1000];

		if (!intervset_out.empty())
			GIntervalsBigSet2D::begin_save(intervset_out.c_str(), iu, chromstats);

		while (1) {
			for (intervs->begin_chrom_iter(chromid1, chromid2); !intervs->isend_chrom(); intervs->next_in_chrom()) {
				const GInterval2D &interv = intervs->cur_interval();

				if (band.do_intersect(interv)) {
					if (!intervset_out.empty()) {
						if (res_intervs.empty())
							snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chroms (%s, %s)",
									 intervset_out.c_str(), iu.id2chrom(interv.chromid1()).c_str(), iu.id2chrom(interv.chromid2()).c_str());
						else if (!interv.is_same_chrom(res_intervs.front()))
							GIntervalsBigSet2D::save_chrom_plain_intervals(intervset_out.c_str(), res_intervs, iu, chromstats);
					}

					if (band.do_contain(interv))
						res_intervs.push_back(interv);
					else {
						Rectangle intersection(interv.x1, interv.y1, interv.x2, interv.y2);
						band.shrink2intersected(intersection);
						res_intervs.push_back(GInterval2D(interv.chromid1(), interv.chromid2(), intersection, interv.udata()));
					}
					iu.verify_max_data_size(res_intervs.size(), intervset_out.empty() ? "Result" : error_prefix);
				}
			}

			while ((go_on = intervs->get_next_chroms(&chromid1, &chromid2))) {
				if (chromid1 == chromid2) 
					break;
			}

			if (!go_on) 
				break;
		}

		if (intervset_out.empty())
			return iu.convert_intervs(&res_intervs);

		GIntervalsBigSet2D::save_chrom_plain_intervals(intervset_out.c_str(), res_intervs, iu, chromstats);
		GIntervalsBigSet2D::end_save_plain_intervals(intervset_out.c_str(), iu, chromstats);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP gintervals_stats(SEXP _intervs, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		IntervUtils iu(_envir);
		GIntervals intervs1d;
		GIntervals2D intervs2d;
		iu.convert_rintervs(_intervs, &intervs1d, &intervs2d);
		intervs1d.sort();
		intervs2d.sort();

		SEXP answer;
		SEXP colnames;
		SEXP rownames;

		if (intervs1d.size()) {
			GIntervalsBigSet1D::ChromStat chromstat = GIntervalsBigSet1D::get_chrom_stat(&intervs1d).second;
			rprotect(answer = RSaneAllocVector(VECSXP, GIntervalsBigSet1D::NUM_STAT_COLS - 1));
            rprotect(colnames = RSaneAllocVector(STRSXP, GIntervalsBigSet1D::NUM_STAT_COLS - 1));

			vector<int> idx2ridx(GIntervalsBigSet1D::NUM_STAT_COLS);
			int colidx = 0;

			for (int i = 0; i < GIntervalsBigSet1D::NUM_STAT_COLS; i++) {
				if (i != GIntervalsBigSet1D::CHROM_COL) {
					idx2ridx[i] = colidx;
					SET_STRING_ELT(colnames, colidx++, mkChar(GIntervalsBigSet1D::STAT_COL_NAMES[i]));
				}
			}

            {
                SEXP rexp;
                rprotect(rexp = ScalarReal(chromstat.size));
    			SET_VECTOR_ELT(answer, idx2ridx[GIntervalsBigSet1D::SIZE_COL], rexp);
            }

            {
                SEXP rexp;
                rprotect(rexp = ScalarReal(chromstat.unified_overlap_size));
                SET_VECTOR_ELT(answer, idx2ridx[GIntervalsBigSet1D::UNIFIED_OVERLAP_SIZE_COL], rexp);
            }

            {
                SEXP rexp;
                rprotect(rexp = ScalarReal(chromstat.unified_touching_size));
                SET_VECTOR_ELT(answer, idx2ridx[GIntervalsBigSet1D::UNIFIED_TOUCHING_SIZE_COL], rexp);
            }

            {
                SEXP rexp;
                rprotect(rexp = ScalarReal(chromstat.range));
                SET_VECTOR_ELT(answer, idx2ridx[GIntervalsBigSet1D::RANGE_COL], rexp);
            }

            {
                SEXP rexp;
                rprotect(rexp = ScalarReal(chromstat.unified_overlap_range));
                SET_VECTOR_ELT(answer, idx2ridx[GIntervalsBigSet1D::UNIFIED_OVERLAP_RANGE_COL], rexp);
            }

            {
                SEXP rexp;
                rprotect(rexp = ScalarReal(chromstat.contains_overlaps));
                SET_VECTOR_ELT(answer, idx2ridx[GIntervalsBigSet1D::CONTAINS_OVERLAPS_COL], RSaneAllocVector(LGLSXP, 1));
            }

            setAttrib(answer, R_NamesSymbol, colnames);
            setAttrib(answer, R_ClassSymbol, mkString("data.frame"));
		} else {
			GIntervalsBigSet2D::ChromStat chromstat = GIntervalsBigSet2D::get_chrom_stat(&intervs2d, iu).second;

			rprotect(answer = RSaneAllocVector(VECSXP, GIntervalsBigSet2D::NUM_STAT_COLS - 2));
            rprotect(colnames = RSaneAllocVector(STRSXP, GIntervalsBigSet2D::NUM_STAT_COLS - 2));

			vector<int> idx2ridx(GIntervalsBigSet2D::NUM_STAT_COLS);
			int colidx = 0;

			for (int i = 0; i < GIntervalsBigSet2D::NUM_STAT_COLS; i++) {
				if (i != GIntervalsBigSet2D::CHROM1_COL && i != GIntervalsBigSet2D::CHROM2_COL) {
					idx2ridx[i] = colidx;
					SET_STRING_ELT(colnames, colidx++, mkChar(GIntervalsBigSet2D::STAT_COL_NAMES[i]));
				}
			}

            {
                SEXP rexp;
                rprotect(rexp = ScalarReal(chromstat.size));
                SET_VECTOR_ELT(answer, idx2ridx[GIntervalsBigSet2D::SIZE_COL], rexp);
            }

            {
                SEXP rexp;
                rprotect(rexp = ScalarReal(chromstat.surface));
                SET_VECTOR_ELT(answer, idx2ridx[GIntervalsBigSet2D::SURFACE_COL], rexp);
            }

            {
                SEXP rexp;
                rprotect(rexp = ScalarReal(chromstat.contains_overlaps));
                SET_VECTOR_ELT(answer, idx2ridx[GIntervalsBigSet2D::CONTAINS_OVERLAPS_COL], rexp);
            }

            setAttrib(answer, R_NamesSymbol, colnames);
            setAttrib(answer, R_ClassSymbol, mkString("data.frame"));
		}

        rprotect(rownames = RSaneAllocVector(INTSXP, 1));
        INTEGER(rownames)[0] = 1;
		setAttrib(answer, R_RowNamesSymbol, rownames);

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP gintervals_chrom_sizes(SEXP _intervals, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		IntervUtils iu(_envir);
		GIntervals intervals1d;
		GIntervals2D intervals2d;
		iu.convert_rintervs(_intervals, &intervals1d, &intervals2d, false, NULL, "", NULL, false);
		intervals1d.sort();
		intervals2d.sort();

		unsigned intervs_type_mask = iu.get_rintervs_type_mask(_intervals);

		if (intervs_type_mask & IntervUtils::INTERVS1D && intervs_type_mask & IntervUtils::INTERVS2D) 
			verror("Dual intervals are not supported");

		vector<uint64_t> chrom_sizes;
		uint64_t num_chroms = iu.get_chromkey().get_num_chroms();
		int num_non_zero_chroms = 0;
		SEXP answer;

		if (intervs_type_mask & IntervUtils::INTERVS1D) { // do not use intervals1d.size() as the intervals might be empty and
														  // we still want to return chrom sizes that are not NULL
			chrom_sizes.resize(num_chroms);
			for (uint64_t chromid = 0; chromid < num_chroms; ++chromid) {
				uint64_t chrom_size = intervals1d.size(chromid);
				if (chrom_size) {
					chrom_sizes[chromid] = chrom_size;
					++num_non_zero_chroms;
				}
			}

			enum { CHROM, SIZE, NUM_COLS };

			SEXP chroms, chroms_idx, sizes, col_names;

			rprotect(answer = RSaneAllocVector(VECSXP, NUM_COLS));
            rprotect(chroms_idx = RSaneAllocVector(INTSXP, num_non_zero_chroms));
            rprotect(sizes = RSaneAllocVector(INTSXP, num_non_zero_chroms));
            rprotect(col_names = RSaneAllocVector(STRSXP, NUM_COLS));
            rprotect(chroms = RSaneAllocVector(STRSXP, num_chroms));

			uint64_t idx = 0;

			for (uint64_t chromid = 0; chromid < num_chroms; ++chromid) {
				SET_STRING_ELT(chroms, chromid, mkChar(iu.get_chromkey().id2chrom(chromid).c_str()));

				if (chrom_sizes[chromid]) {
					INTEGER(chroms_idx)[idx] = chromid + 1;
					INTEGER(sizes)[idx] = chrom_sizes[chromid];
					++idx;
				}
			}

			SET_STRING_ELT(col_names, CHROM, mkChar("chrom"));
			SET_STRING_ELT(col_names, SIZE, mkChar("size"));

            setAttrib(answer, R_NamesSymbol, col_names);
            setAttrib(chroms_idx, R_LevelsSymbol, chroms);
            setAttrib(chroms_idx, R_ClassSymbol, mkString("factor"));

            SET_VECTOR_ELT(answer, CHROM, chroms_idx);
            SET_VECTOR_ELT(answer, SIZE, sizes);
		} else {
			chrom_sizes.resize(num_chroms * num_chroms);
			for (uint64_t chromid1 = 0; chromid1 < num_chroms; ++chromid1) {
				for (uint64_t chromid2 = 0; chromid2 < num_chroms; ++chromid2) {
					uint64_t chrom_size = intervals2d.size(chromid1, chromid2);
					if (chrom_size) {
						chrom_sizes[chromid1 * num_chroms + chromid2] = chrom_size;
						++num_non_zero_chroms;
					}
				}
			}

			enum { CHROM1, CHROM2, SIZE, NUM_COLS };

			SEXP chroms1, chroms2, chroms_idx1, chroms_idx2, sizes, col_names;

			rprotect(answer = RSaneAllocVector(VECSXP, NUM_COLS));
            rprotect(chroms_idx1 = RSaneAllocVector(INTSXP, num_non_zero_chroms));
            rprotect(chroms_idx2 = RSaneAllocVector(INTSXP, num_non_zero_chroms));
            rprotect(sizes = RSaneAllocVector(INTSXP, num_non_zero_chroms));
            rprotect(col_names = RSaneAllocVector(STRSXP, NUM_COLS));
            rprotect(chroms1 = RSaneAllocVector(STRSXP, num_chroms));
            rprotect(chroms2 = RSaneAllocVector(STRSXP, num_chroms));

			uint64_t idx = 0;

			for (uint64_t chromid1 = 0; chromid1 < num_chroms; ++chromid1) {
				SET_STRING_ELT(chroms1, chromid1, mkChar(iu.get_chromkey().id2chrom(chromid1).c_str()));
				SET_STRING_ELT(chroms2, chromid1, mkChar(iu.get_chromkey().id2chrom(chromid1).c_str()));

				for (uint64_t chromid2 = 0; chromid2 < num_chroms; ++chromid2) {
					uint64_t chrom_size = chrom_sizes[chromid1 * num_chroms + chromid2];
					if (chrom_size) {
						INTEGER(chroms_idx1)[idx] = chromid1 + 1;
						INTEGER(chroms_idx2)[idx] = chromid2 + 1;
						INTEGER(sizes)[idx] = chrom_size;
						++idx;
					}
				}
			}

			SET_STRING_ELT(col_names, CHROM1, mkChar("chrom1"));
			SET_STRING_ELT(col_names, CHROM2, mkChar("chrom2"));
			SET_STRING_ELT(col_names, SIZE, mkChar("size"));

            setAttrib(answer, R_NamesSymbol, col_names);
            setAttrib(chroms_idx1, R_LevelsSymbol, chroms1);
            setAttrib(chroms_idx1, R_ClassSymbol, mkString("factor"));
            setAttrib(chroms_idx2, R_LevelsSymbol, chroms2);
            setAttrib(chroms_idx2, R_ClassSymbol, mkString("factor"));

            SET_VECTOR_ELT(answer, CHROM1, chroms_idx1);
            SET_VECTOR_ELT(answer, CHROM2, chroms_idx2);
            SET_VECTOR_ELT(answer, SIZE, sizes);
		}

		SEXP row_names;
        rprotect(row_names = RSaneAllocVector(INTSXP, num_non_zero_chroms));

		for (int i = 0; i < num_non_zero_chroms; ++i) 
			INTEGER(row_names)[i] = i + 1;

        setAttrib(answer, R_ClassSymbol, mkString("data.frame"));
        setAttrib(answer, R_RowNamesSymbol, row_names);

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP gtrack_intervals_load(SEXP _track, SEXP _chrom, SEXP _chrom1, SEXP _chrom2, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_track) || length(_track) != 1)
			verror("Track argument is not a string");

		const char *track_str = CHAR(STRING_ELT(_track, 0));
		IntervUtils iu(_envir);
		string trackpath(track2path(_envir, track_str));
		GenomeTrack::Type track_type = GenomeTrack::get_type(trackpath.c_str(), iu.get_chromkey(), true);

		if (track_type == GenomeTrack::FIXED_BIN)
			verror("Track of type %s cannot be used in place of an intervals set", GenomeTrack::TYPE_NAMES[track_type]);

		if (track_type == GenomeTrack::SPARSE  || track_type == GenomeTrack::ARRAYS) {
			if ((!isString(_chrom) && !isFactor(_chrom)) || length(_chrom) != 1)
				verror("Chromosome argument is not a string");

			unique_ptr<GenomeTrack1D> track;
			GIntervals *intervals = NULL;

			SEXP chrom_levels = getAttrib(_chrom, R_LevelsSymbol);
			const char *chrom = isString(_chrom) ? CHAR(STRING_ELT(_chrom, 0)) : CHAR(STRING_ELT(chrom_levels, INTEGER(_chrom)[0] - 1));
			int chromid = iu.chrom2id(chrom);

			string filename(trackpath + "/" + GenomeTrack::get_1d_filename(iu.get_chromkey(), chromid));

			if (track_type == GenomeTrack::SPARSE) {
				track = unique_ptr<GenomeTrack1D>(new GenomeTrackSparse());
				((GenomeTrackSparse *)track.get())->init_read(filename.c_str(), chromid);
				intervals = (GIntervals *)&((GenomeTrackSparse *)track.get())->get_intervals();
			} else if (track_type == GenomeTrack::ARRAYS) {
				track = unique_ptr<GenomeTrack1D>(new GenomeTrackArrays());
				((GenomeTrackArrays *)track.get())->init_read(filename.c_str(), chromid);
				intervals = (GIntervals *)&((GenomeTrackArrays *)track.get())->get_intervals();
			}

			return iu.convert_intervs(intervals);
		} else if (track_type == GenomeTrack::RECTS || track_type == GenomeTrack::POINTS || track_type == GenomeTrack::COMPUTED) {
			if ((!isString(_chrom1) && !isFactor(_chrom1)) || length(_chrom1) != 1 || (!isString(_chrom2) && !isFactor(_chrom2)) || length(_chrom2) != 1)
				verror("Chromosome argument is not a string");

			unique_ptr<GenomeTrack2D> track;
			GIntervals2D intervals;
			uint64_t size = 0;

			SEXP chrom_levels1 = getAttrib(_chrom1, R_LevelsSymbol);
			SEXP chrom_levels2 = getAttrib(_chrom2, R_LevelsSymbol);
			const char *chrom1 = isString(_chrom1) ? CHAR(STRING_ELT(_chrom1, 0)) : CHAR(STRING_ELT(chrom_levels1, INTEGER(_chrom1)[0] - 1));
			const char *chrom2 = isString(_chrom2) ? CHAR(STRING_ELT(_chrom2, 0)) : CHAR(STRING_ELT(chrom_levels2, INTEGER(_chrom2)[0] - 1));
			int chromid1 = iu.chrom2id(chrom1);
			int chromid2 = iu.chrom2id(chrom2);

			string filename(trackpath + "/" + GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid1, chromid2));

			if (track_type == GenomeTrack::RECTS) {
				track = unique_ptr<GenomeTrack2D>(new GenomeTrackRectsRects(iu.get_track_chunk_size(), iu.get_track_num_chunks()));
				((GenomeTrackRectsRects *)track.get())->init_read(filename.c_str(), chromid1, chromid2);
				size = ((GenomeTrackRectsRects *)track.get())->get_qtree().get_num_objs();
			} else if (track_type == GenomeTrack::POINTS) {
				track = unique_ptr<GenomeTrack2D>(new GenomeTrackRectsPoints(iu.get_track_chunk_size(), iu.get_track_num_chunks()));
				((GenomeTrackRectsPoints *)track.get())->init_read(filename.c_str(), chromid1, chromid2);
				size = ((GenomeTrackRectsPoints *)track.get())->get_qtree().get_num_objs();
			} else if (track_type == GenomeTrack::COMPUTED) {
				track = unique_ptr<GenomeTrack2D>(new GenomeTrackComputed(get_groot(_envir), iu.get_track_chunk_size(), iu.get_track_num_chunks()));
				((GenomeTrackComputed *)track.get())->init_read(filename.c_str(), chromid1, chromid2);
				size = ((GenomeTrackComputed *)track.get())->get_qtree().get_num_objs();
			}

			intervals.reserve(size);

			for (track->begin_interval(); !track->is_end_interval(); track->next_interval())
				intervals.push_back(track->cur_interval());

			return iu.convert_intervs(&intervals);
		}
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

}
