/*
 * GenomeTrackFindNeighbors.cpp
 *
 *  Created on: Nov 9, 2010
 *      Author: hoichman
 */

#include <cstdint>
#include "rdbinterval.h"
#include "rdbprogress.h"
#include "rdbutils.h"
#include "SegmentFinder.h"
#include "StatQuadTree.h"

using namespace std;
using namespace rdb;

struct IntervNeighbor {
	int64_t        id1;
	int64_t        id2;
	int64_t        dist;

	IntervNeighbor(int64_t _id1, int64_t _id2, int64_t _dist) : id1(_id1), id2(_id2), dist(_dist) {}

	bool operator<(const IntervNeighbor &o) const {
		return
			id1 < o.id1 ||
			(id1 == o.id1 && llabs(dist) < llabs(o.dist)) ||
			(id1 == o.id1 && llabs(dist) == llabs(o.dist) && id2 < o.id2);
	}
};

struct IntervNeighbor2D {
	int64_t        id1;
	int64_t        id2;
	int64_t        dist1;
	int64_t        dist2;

	IntervNeighbor2D(int64_t _id1, int64_t _id2, int64_t _dist1, int64_t _dist2) : id1(_id1), id2(_id2), dist1(_dist1), dist2(_dist2) {}

	bool operator<(const IntervNeighbor2D &o) const {
		return
			id1 < o.id1 ||
			(id1 == o.id1 && llabs(dist1 + dist2) < llabs(o.dist1 + o.dist2)) ||
			(id1 == o.id1 && llabs(dist1 + dist2) == llabs(o.dist1 + o.dist2) && id2 == o.id2);
	}
};

extern "C" {

SEXP gfind_neighbors(SEXP _intervs1, SEXP _intervs2, SEXP _maxneighbors, SEXP _distrange_start, SEXP _distrange_end,
					 SEXP _distrange_start1, SEXP _distrange_end1, SEXP _distrange_start2, SEXP _distrange_end2,
					 SEXP _na_if_notfound, SEXP _report_progress, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;
		GIntervals intervals1d[2];
		GIntervals2D intervals2d[2];

		IntervUtils iu(_envir);
		_intervs1 = iu.convert_rintervs(_intervs1, &intervals1d[0], &intervals2d[0]);
		_intervs2 = iu.convert_rintervs(_intervs2, &intervals1d[1], &intervals2d[1]);

		unsigned type_mask1 = iu.get_rintervs_type_mask(_intervs1);
		unsigned type_mask2 = iu.get_rintervs_type_mask(_intervs2);

		if (type_mask1 == (IntervUtils::INTERVS1D | IntervUtils::INTERVS2D) ||
			type_mask2 == (IntervUtils::INTERVS1D | IntervUtils::INTERVS2D))
			verror("Intervals cannot be of dual intervals type");

		if (type_mask1 != type_mask2) 
			verror("Cannot intermix 1D and 2D intervals");

		if (!isInteger(_maxneighbors) && !isReal(_maxneighbors)) 
			verror("maxneighbors argument is not numeric");
		int maxneighbors = isInteger(_maxneighbors) ? INTEGER(_maxneighbors)[0] : (int)REAL(_maxneighbors)[0];
		if (maxneighbors < 1) 
			verror("maxneighbors must be greater or equal to 1");

		if (!isLogical(_na_if_notfound))
			verror("na.if.notfound argument is not logical");
		bool na_if_notfound = LOGICAL(_na_if_notfound)[0];

		Progress_reporter progress;

		if (type_mask1 &= IntervUtils::INTERVS1D) {
			if (!isReal(_distrange_start))
				verror("mindist argument is not numeric");
			int64_t distance_start = (int64_t)REAL(_distrange_start)[0];

			if (!isReal(_distrange_end))
				verror("maxdist argument is not numeric");
			int64_t distance_end = (int64_t)REAL(_distrange_end)[0];

			if (distance_start > distance_end)
				verror("mindist exceeds maxdist");

			int64_t maxdist = max(llabs(distance_start), llabs(distance_end));
			int64_t mindist = min(llabs(distance_start), llabs(distance_end));

			if (intervals1d[0].empty() || (intervals1d[1].empty() && !na_if_notfound))
				return R_NilValue;

			vector<IntervNeighbor> result;

			intervals1d[0].sort();
			intervals1d[1].sort();

			int chromid = -1;
			SegmentFinder<GInterval> segment_finder;
			SegmentFinder<GInterval>::NNIterator inn(&segment_finder);

			if (LOGICAL(_report_progress)[0])
				progress.init(intervals1d[0].size(), 100);

			for (GIntervals::const_iterator iinterv = intervals1d[0].begin(); iinterv < intervals1d[0].end(); ++iinterv) {
				if (iinterv->chromid != chromid) {
					chromid = iinterv->chromid;
					segment_finder.reset(0, iu.get_chromkey().get_chrom_size(chromid));

					for (intervals1d[1].begin_chrom_iter(chromid); !intervals1d[1].isend_chrom(); intervals1d[1].next_in_chrom()) {
						const GInterval &interv = intervals1d[1].cur_interval();
						segment_finder.insert(interv);
					}
				}

				if ((distance_start > 0 && distance_end > 0) || (distance_start < 0 && distance_end < 0)) {
					Segment excluded_area(iinterv->start - mindist + 1, iinterv->end + mindist - 1);
					inn.begin(*iinterv, excluded_area);
				} else
					inn.begin(*iinterv);

				int num_neighbors = 0;
				while (!inn.is_end()) {
					int64_t dist = iinterv->dist2interv(*inn);
					if (llabs(dist) > maxdist)
						break;
					if (dist >= distance_start && dist <= distance_end) {
						result.push_back(IntervNeighbor(iu.get_orig_interv_idx(*iinterv), iu.get_orig_interv_idx(*inn), dist));
						iu.verify_max_data_size(result.size());
						num_neighbors++;
						if (num_neighbors >= maxneighbors) 
							break;
					}
					inn.next();
				}

				if (na_if_notfound && !num_neighbors) {
					result.push_back(IntervNeighbor(iu.get_orig_interv_idx(*iinterv), -1, 0));
					iu.verify_max_data_size(result.size());
				}

				if (LOGICAL(_report_progress)[0])
					progress.report(1);
				check_interrupt();
			}
			if (LOGICAL(_report_progress)[0])
				progress.report_last();

			if (result.empty())
				return R_NilValue;

			sort(result.begin(), result.end());

			SEXP answer = iu.create_data_frame(result.size(), length(_intervs1) + length(_intervs2) + 1);
			SEXP rdists;
			vector<SEXP> src_cols1;
			vector<SEXP> src_cols2;
			vector<SEXP> tgt_cols;

			iu.define_data_frame_cols(_intervs1, src_cols1, answer, tgt_cols, 0);
			iu.define_data_frame_cols(_intervs2, src_cols2, answer, tgt_cols, length(_intervs1));
			rprotect(rdists = RSaneAllocVector(REALSXP, result.size()));

			for (uint64_t i = 0; i < result.size(); ++i) {
				const IntervNeighbor &r = result[i];
				iu.copy_data_frame_row(src_cols1, r.id1, tgt_cols, i, 0);

				if (r.id2 >= 0) {
					iu.copy_data_frame_row(src_cols2, r.id2, tgt_cols, i, length(_intervs1));
					REAL(rdists)[i] = (double)r.dist;
				} else {
					for (int j = 0; j < length(_intervs2); ++j)
						iu.set_data_frame_val_nan(tgt_cols, i, length(_intervs1) + j);
					REAL(rdists)[i] = NA_REAL;
				}
			}
			SET_VECTOR_ELT(answer, length(_intervs1) + length(_intervs2), rdists);
			SEXP colnames = getAttrib(answer, R_NamesSymbol);
			SET_STRING_ELT(colnames, length(_intervs1) + length(_intervs2), mkChar("dist"));

			return answer;
		} else {   // 2D
			if (!isReal(_distrange_start1))
				verror("mindist1 argument is not numeric");
			int64_t distance_start1 = (int64_t)REAL(_distrange_start1)[0];

			if (!isReal(_distrange_end1))
				verror("maxdist1 argument is not numeric");
			int64_t distance_end1 = (int64_t)REAL(_distrange_end1)[0];

			if (distance_start1 > distance_end1)
				verror("mindist1 exceeds maxdist1");

			if (!isReal(_distrange_start2))
				verror("mindist2 argument is not numeric");
			int64_t distance_start2 = (int64_t)REAL(_distrange_start2)[0];

			if (!isReal(_distrange_end2))
				verror("maxdist2 argument is not numeric");
			int64_t distance_end2 = (int64_t)REAL(_distrange_end2)[0];

			if (distance_start2 > distance_end2)
				verror("mindist2 exceeds maxdist2");

			if (intervals2d[0].empty() || (intervals2d[1].empty() && !na_if_notfound) || distance_end1 < 0 || distance_end2 < 0)
				return R_NilValue;

			vector<IntervNeighbor2D> result;

			intervals2d[0].sort();
			intervals2d[1].sort();
			intervals2d[1].verify_no_overlaps(iu.get_chromkey());

			int chromid1 = -1;
			int chromid2 = -1;
			StatQuadTree<Rectangle_val<uint64_t>, uint64_t> qtree;
			StatQuadTree<Rectangle_val<uint64_t>, uint64_t>::NNIterator inn(&qtree);

			if (LOGICAL(_report_progress)[0])
				progress.init(intervals2d[0].size(), 100);

			for (GIntervals2D::const_iterator iinterv = intervals2d[0].begin(); iinterv < intervals2d[0].end(); ++iinterv) {
				if (iinterv->chromid1() != chromid1 || iinterv->chromid2() != chromid2) {
					chromid1 = iinterv->chromid1();
					chromid2 = iinterv->chromid2();
					qtree.reset(0, 0, iu.get_chromkey().get_chrom_size(chromid1), iu.get_chromkey().get_chrom_size(chromid2));

					for (intervals2d[1].begin_chrom_iter(chromid1, chromid2); !intervals2d[1].isend_chrom(); intervals2d[1].next_in_chrom()) {
						const GInterval2D &interv = intervals2d[1].cur_interval();
						StatQuadTree<Rectangle_val<uint64_t>, uint64_t>::ValueType rect(interv, iu.get_orig_interv_idx(interv));
						qtree.insert(rect);
					}
				}

				if (distance_start1 > 0 && distance_start2 > 0) {
					Rectangle excluded_area(iinterv->x1 - distance_start1, iinterv->y1 - distance_start2,
											iinterv->x2 + distance_start1, iinterv->y2 + distance_start2);
					inn.begin(*iinterv, excluded_area);
				} else
					inn.begin(*iinterv);

				int num_neighbors = 0;
				while (!inn.is_end()) {
					int64_t dist1 = iinterv->xdist(*inn);
					int64_t dist2 = iinterv->ydist(*inn);

					if (dist1 > distance_end1 && dist2 > distance_end2) 
						break;

					if (dist1 >= distance_start1 && dist1 <= distance_end1 && dist2 >= distance_start2 && dist2 <= distance_end2) {
						result.push_back(IntervNeighbor2D(iu.get_orig_interv_idx(*iinterv), inn->v, dist1, dist2));
						iu.verify_max_data_size(result.size());
                        num_neighbors++;
                        if (num_neighbors >= maxneighbors) 
                            break;
					}

					inn.next();
				}

				if (na_if_notfound && !num_neighbors) {
					result.push_back(IntervNeighbor2D(iu.get_orig_interv_idx(*iinterv), -1, 0, 0));
					iu.verify_max_data_size(result.size());
				}

				if (LOGICAL(_report_progress)[0])
					progress.report(1);
				check_interrupt();
			}
			if (LOGICAL(_report_progress)[0])
				progress.report_last();

			if (result.empty())
				return R_NilValue;

			sort(result.begin(), result.end());

			SEXP answer = iu.create_data_frame(result.size(), length(_intervs1) + length(_intervs2) + 2);
			SEXP rdists1, rdists2;
			vector<SEXP> src_cols1;
			vector<SEXP> src_cols2;
			vector<SEXP> tgt_cols;

			iu.define_data_frame_cols(_intervs1, src_cols1, answer, tgt_cols, 0);
			iu.define_data_frame_cols(_intervs2, src_cols2, answer, tgt_cols, length(_intervs1));
			rprotect(rdists1 = RSaneAllocVector(REALSXP, result.size()));
			rprotect(rdists2 = RSaneAllocVector(REALSXP, result.size()));

			for (uint64_t i = 0; i < result.size(); ++i) {
				const IntervNeighbor2D &r = result[i];
				iu.copy_data_frame_row(src_cols1, r.id1, tgt_cols, i, 0);

				if (r.id2 >= 0) {
					iu.copy_data_frame_row(src_cols2, r.id2, tgt_cols, i, length(_intervs1));
					REAL(rdists1)[i] = (double)r.dist1;
					REAL(rdists2)[i] = (double)r.dist2;
				} else {
					for (int j = 0; j < length(_intervs2); ++j)
						iu.set_data_frame_val_nan(tgt_cols, i, length(_intervs1) + j);
					REAL(rdists1)[i] = NA_REAL;
					REAL(rdists2)[i] = NA_REAL;
				}
			}
			SET_VECTOR_ELT(answer, length(_intervs1) + length(_intervs2), rdists1);
			SET_VECTOR_ELT(answer, length(_intervs1) + length(_intervs2) + 1, rdists2);

			SEXP colnames = getAttrib(answer, R_NamesSymbol);
			SET_STRING_ELT(colnames, length(_intervs1) + length(_intervs2), mkChar("dist1"));
			SET_STRING_ELT(colnames, length(_intervs1) + length(_intervs2) + 1, mkChar("dist2"));

			return answer;
		}
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

}
