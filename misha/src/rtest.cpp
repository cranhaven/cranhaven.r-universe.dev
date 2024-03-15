#include <cstdint>
// /*
//  * rtest.cpp
//  *
//  *  Created on: Aug 11, 2010
//  *      Author: hoichman
//  */

// #include <time.h>
// #include <fstream>
// #include <signal.h>
// #include <unistd.h>
// #include <sys/types.h>
// #include <sys/wait.h>

// #include "rdbinterval.h"

// #include "BinsManager.h"
// #include "GenomeTrackArrays.h"
// #include "GenomeTrackRects.h"
// #include "Point.h"
// #include "SegmentFinder.h"
// #include "StreamPercentiler.h"
// #include "StatQuadTree.h"
// #include "StatQuadTreeCached.h"
// #include "StatQuadTreeCachedSerializer.h"
// #include "strutil.h"
// #include "TrackExpressionScanner.h"

// #include "rdbinterval.h"
// #include "rdbprogress.h"
// #include "rdbutils.h"
// #include "TrackExpressionFixedBinIterator.h"
// #include "TrackExpressionIntervals1DIterator.h"

// #include <R.h>
// #include <Rinternals.h>
// #include <R_ext/Parse.h>

// using namespace rdb;

// struct NNRes {
// 	Rectangle_val<float> obj;
// 	int64_t              dist;

// 	bool operator==(const NNRes &o) const {
// 		return obj.x1 == o.obj.x1 && obj.x2 == o.obj.x2 && obj.y1 == o.obj.y1 && obj.y2 == o.obj.y2 && obj.v == o.obj.v;
// 	}

// 	bool operator<(const NNRes &o) const {
// 		return dist < o.dist ||
// 			(dist == o.dist && obj.v < o.obj.v);
// //		(obj.x1 < o.obj.x1 ||
// //							   obj.x1 == o.obj.x1 && (obj.x2 < o.obj.x2 ||
// //													  (obj.x2 == o.obj.x2 && obj.y1 < o.obj.y1 ||
// //													   obj.y1 == o.obj.y1 && (obj.y2 < o.obj.y2 ||
// //																			  (obj.y2 == o.obj.y2 && obj.v < o.obj.v)))));
// 	}
// };

// static bool sort_rects(const Rectangle &r1, const Rectangle &r2)
// {
// 	return r1.x1 + 3 * r1.x2 + 17 * r1.y1 + 23 * r1.y2 < r2.x1 + 3 * r2.x2 + 17 * r2.y1 + 23 * r2.y2;
// }

// struct SegmentVal : public Segment{
// 	SegmentVal() {}
// 	SegmentVal(const Segment &segment, uint64_t _val) : Segment(segment), v(_val) {}

// //	Segment &operator=(const SegmentVal &o) {
// //		*(Segment *)this = o;
// //		v = o.v;
// //		return *this;
// //	}
// //
// 	uint64_t v;

// 	const char *debug_str() const {
// 		static char buf[100];
// 		snprintf(buf, sizeof(buf), "%s %ld", Segment::debug_str(), v);
// 		return buf;
// 	}
// };

// struct NNSegmentRes {
// 	SegmentVal obj;
// 	int64_t    dist;

// 	bool operator==(const NNSegmentRes &o) const {
// 		return obj.start == o.obj.start && obj.end == o.obj.end && obj.v == o.obj.v;
// 	}

// 	bool operator<(const NNSegmentRes &o) const {
// 		return dist < o.dist ||
// 			(dist == o.dist && obj.v < o.obj.v);
// //		(obj.start < o.obj.start ||
// //							   obj.start == o.obj.start && (obj.end < o.obj.end ||
// //															(obj.end == o.obj.end && obj.v < o.obj.v)));
// 	}
// };

// extern "C" {

// SEXP gvectors(SEXP _n, SEXP _variant, SEXP _envir) {
// 	try {
// 		RdbInitializer rdb_init;
// 		IntervUtils iu(_envir);

// 		int n = isReal(_n) ? REAL(_n)[0] : INTEGER(_n)[0];
// 		int variant = isReal(_variant) ? REAL(_variant)[0] : INTEGER(_variant)[0];
// 		SEXP answer;

// 		rprotect(answer = RSaneAllocVector(REALSXP, n));

// 		switch (variant) {
// 		case 0:
// 			{
// 				for (int i = 0; i < n; ++i) 
// 					REAL(answer)[i] = i;
// 			}
// 			break;
// 		case 1:
// 			{
// 				double *ar = REAL(answer);
// 				for (int i = 0; i < n; ++i) 
// 					ar[i] = i;
// 			}
// 			break;
// 		case 2:
// 			{
// 				for (int i = 0; i < n; ++i) 
// 					if (isReal(answer))
// 						;
// 			}
// 			break;
// 		case 3:
// 			{
// 				for (int i = 0; i < n; ++i) 
// 					if (TYPEOF(answer) == REALSXP)
// 						;
// 			}
// 			break;
// 		case 4:
// 			{
// 				double *ar = REAL(answer);
// 				for (int i = 0; i < n; ++i) 
// 					if (isReal(answer))
// 						ar[i] = i;
// 			}
// 			break;
// 		}
// //		return answer;
// 	} catch (TGLException &e) {
// 		rerror("%s", e.msg());
//     } catch (const bad_alloc &e) {
//         rerror("Out of memory");
//     }
// 	return R_NilValue;
// }


// //SEXP gtest_cbind(SEXP df1, SEXP df2, SEXP _envir) {
// //	try {
// //		RdbInitializer rdb_init;
// //		IntervUtils iu(_envir);
// //
// //		SEXP res = iu.create_data_frame(2, length(df1) + length(df2));
// //		iu.define_data_frame_cols(df1, res, 0);
// //		iu.define_data_frame_cols(df2, res, length(df1));
// //		iu.copy_data_frame_row(df1, 1, res, 0, 0);
// //		iu.copy_data_frame_row(df1, 3, res, 1, 0);
// //		iu.copy_data_frame_row(df2, 0, res, 0, length(df1));
// //		iu.copy_data_frame_row(df2, 2, res, 1, length(df1));
// //		return res;
// //	} catch (TGLException &e) {
// //		rerror("%s", e.msg());
// //	}
// //	return R_NilValue;
// //}
// //
// SEXP gtest_segment_finder(SEXP _envir) {
// 	try {
// 		RdbInitializer rdb_init;

// 		int start, end;

// 		int maxcoord = 100000000;
// 		int max_segment_size = 10000;
// 		int num_segments = 150000;
// 		int num_queries = 1500000;
// 		int num_nn_queries = min(2000, num_queries);
// 		int max_query_size = 2000;//00;
// 		int max_nearest_neighbors = 10;
// 		Segments queries;
// 		vector<SegmentVal> segments;
// 		SegmentFinder<SegmentVal> sf(0, maxcoord, 20, 3);

// 		clock_t cl = clock();

// 		REprintf("Generating queries\n");
// 		for (int i = 0; i < num_queries; i++) {
// 			start = (int64_t)(unif_rand() * maxcoord);
// 			end = (int64_t)min(start + 1 + max_query_size * unif_rand(), (double)maxcoord);
// 			queries.push_back(Segment(start, end));
// 		}

// //queries.push_back(Segment(49992229, 50000329));
// //queries.push_back(Segment(49992220, 49992240));
// //queries.push_back(Segment(49999999, 50000001));
// //REprintf("Distance: %ld\n", queries.back().dist2segment(Segment(49992229, 50000329)));

// 		REprintf("Generating segments\n");
// 		for (int i = 0; i < num_segments; i++) {
// 			start = (int64_t)(unif_rand() * maxcoord);
// 			end = min((int64_t)(start + 1 + max_segment_size * unif_rand()), (int64_t)maxcoord);
// 			SegmentVal sv(Segment(start, end), i);
// 			sf.insert(sv);
// 			segments.push_back(sv);

// 			if (i && i % 10000 == 0) {
// 				REprintf("%d segments generated\n", i);
// 				check_interrupt();
// 			}
// 		}
// 		REprintf(" %2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);

// //		sf.debug_print_tree();

// 		cl = clock();

// 		REprintf("Running nearest neighbor queries on segment finder\n");
// 		{
// 			cl = clock();
// 			vector<NNSegmentRes> r1;
// 			vector<NNSegmentRes> r2;

// 			for (int i = 0; i < num_nn_queries; ++i) {
// 				Segments::const_iterator iquery = queries.begin() + i;
// 				SegmentFinder<SegmentVal>::NNIterator inn(&sf);
// 				int nearest_neighbors = 0;

// 				r1.clear();
// //				REprintf("\tQuery: %s\n", iquery->debug_str());
// 				for (inn.begin(*iquery); !inn.is_end(); inn.next()) {
// 					NNSegmentRes res;
// 					res.obj = *inn;
// 					res.dist = iquery->dist2segment(res.obj);
// 					if (r1.size() && res.dist != r1.back().dist) {
// 						nearest_neighbors++;
// 						if (nearest_neighbors >= max_nearest_neighbors)
// 							break;
// 					}
// 					r1.push_back(res);
// //					REprintf("\tr size %ld, %s, dist: %ld\n",
// //						   r1.size(), res.obj.debug_str(), res.obj.dist2segment(*iquery));
// 				}

// 				r2.clear();
// 				r2.reserve(segments.size());
// 				for (vector<SegmentVal>::const_iterator isgm = segments.begin(); isgm < segments.end(); ++isgm) {
// 					NNSegmentRes res;
// 					res.obj = *isgm;
// 					res.dist = iquery->dist2segment(res.obj);
// 					r2.push_back(res);
// 				}

// 				sort(r1.begin(), r1.end());
// 				sort(r2.begin(), r2.end());
// 				if (r1.size() < max_nearest_neighbors) {
// 					REprintf("\t%ld Query: %s\n", iquery - queries.begin(), iquery->debug_str());
// 					for (uint64_t j = 0; j < min(r1.size(), (uint64_t)max_nearest_neighbors); ++j)
// 						REprintf("\tSegment finder: %s, dist: %lld\n", r1[j].obj.debug_str(), r1[j].dist);
// 					for (uint64_t j = 0; j < min(r2.size(), (uint64_t)max_nearest_neighbors); ++j)
// 						REprintf("\tPlain:          %s, dist: %lld\n", r2[j].obj.debug_str(), r2[j].dist);
// 					getchar();
// 					check_interrupt();
// 				}

// 				for (uint64_t j = 0; j < r1.size(); ++j) {
// 					if (!(r1[j] == r2[j])) {
// 						REprintf("\t%ld Query: %s\n", iquery - queries.begin(), iquery->debug_str());
// 						for (uint64_t j = 0; j < min(r1.size(), (uint64_t)max_nearest_neighbors); ++j) {
// 							REprintf("\tSegment finder: %s, dist: %lld\n", r1[j].obj.debug_str(), r1[j].dist);
// 							REprintf("\tPlain:          %s, dist: %lld\n", r2[j].obj.debug_str(), r2[j].dist);
// 						}
// 						getchar();
// 						check_interrupt();
// 					}
// 				}

// 				if (i && i % 100 == 0) {
// 					REprintf("%d nearest neighbor queries done\n", i);
// 					check_interrupt();
// 				}
// 			}

// 			REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);
// 		}
//  	} catch (TGLException &e) {
// 		rerror("%s", e.msg());
//     } catch (const bad_alloc &e) {
//         rerror("Out of memory");
//     }
// 	return R_NilValue;

// }

// SEXP gtest_cached_qtree(SEXP _envir) {
// 	try {
// 		RectsQuadTree qtree;
// 		BufferedFile file;
// 		RectsQuadTreeCached cqtree;
// 		RectsQuadTree::Stat s;

// 		cqtree.serialize(file, qtree);
// 		cqtree.unserialize(file);
// 		cqtree.get_stat(Rectangle(0, 0, 10, 10), s);
// 		cqtree.debug_print_tree();
//  	} catch (TGLException &e) {
// 		rerror("%s", e.msg());
//     } catch (const bad_alloc &e) {
//         rerror("Out of memory");
//     }
// 	return R_NilValue;

// }

// SEXP gtest_eitan(SEXP _envir) {

// 	try {
// 		RdbInitializer rdb_init;

// 		IntervUtils iu(_envir);

// 		const GenomeChromKey &chromkey = iu.get_chromkey();
// 		unsigned num_chroms = chromkey.get_num_chroms();
// 		RectsQuadTree qtree[num_chroms * num_chroms];

// 		for (unsigned i = 0; i < num_chroms; i++) {
// 			for (unsigned j = 0; j < num_chroms; j++)
// 				qtree[i * num_chroms + j].init(0, 0, chromkey.get_chrom_size(i), chromkey.get_chrom_size(j));
// 		}

// 		ifstream file("/home/eitany/storage/o3c/results/wtb_s0.mat");
// //		ifstream file("/net/mraid04/export/users/lubling/scell/results/for_misha/lib5_gc_200.mat");
// 		if (!file)
// 			verror("Failed to open file");

// 		vector<string> fields;

// 		clock_t cl = clock();
// 		REprintf("Inserting rects to quad tree\n");
// 		string chrstr("chr");
// 		split_line(file, fields);
// 		for (unsigned i = 0; ; i++) {
// 			split_line(file, fields);

// 			if (fields.empty())
// 				break;

// 			int chrom1 = chromkey.chrom2id(chrstr + fields[1]);
// 			int chrom2 = chromkey.chrom2id(chrstr + fields[4]);
// 			uint64_t coord1 = atoll(fields[2].c_str());
// 			uint64_t coord2 = atoll(fields[5].c_str());

// 			if (chrom1 <= chrom2)
// 				qtree[chrom1 * num_chroms + chrom2].insert(RectsQuadTree::ValueType(coord1, coord1 + 1, coord2, coord2 + 1, 0));
// 			else
// 				qtree[chrom2 * num_chroms + chrom1].insert(RectsQuadTree::ValueType(coord1, coord1 + 1, coord2, coord2 + 1, 0));

// 			if (i && i % 100000 == 0) {
// 				REprintf("%d rects inserted to quad tree\n", i);
// 				check_interrupt();
// 			}
// 		}
// 		REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);

// 		REprintf("Serializing quad tree\n");
// 		cl = clock();
// 		for (unsigned chrom1 = 0; chrom1 < num_chroms; chrom1++) {
// 			for (unsigned chrom2 = chrom1; chrom2 < num_chroms; chrom2++) {
// 				BufferedFile file;
// 				char buf[1000];

// 				snprintf(buf, sizeof(buf), "2dtrack %d-%d", chrom1, chrom2);
// 				file.open("2dtrack", "w");
// 				qtree[chrom1 * num_chroms + chrom2].serialize(file);
// 				file.close();
// 			}
// 		}
// 		REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);

// 		REprintf("Unserializing quad tree\n");
// 		cl = clock();
// 		for (unsigned chrom1 = 0; chrom1 < num_chroms; chrom1++) {
// 			for (unsigned chrom2 = chrom1; chrom2 < num_chroms; chrom2++) {
// 				RectsQuadTree qtree2;
// 				BufferedFile file;
// 				char buf[1000];

// 				snprintf(buf, sizeof(buf), "2dtrack %d-%d", chrom1, chrom2);
// 				file.open("2dtrack", "r");
// 				qtree2.unserialize(file);
// 			}
// 		}
// 		REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);

// 		const unsigned query_size = 50000;
// 		RectsQuadTree::Stat s;
// 		uint64_t num_queries = 0;

// 		cl = clock();
// 		for (unsigned chrom1 = 0; chrom1 < num_chroms; chrom1++) {
// 			uint64_t chrom1_size = chromkey.get_chrom_size(chrom1);
// 			for (unsigned chrom2 = chrom1; chrom2 < num_chroms; chrom2++) {
// 				uint64_t chrom2_size = chromkey.get_chrom_size(chrom2);
// 				const RectsQuadTree &qt = qtree[chrom1 * num_chroms + chrom2];

// 				REprintf("%s vs %s\t%ld objs\t%ld queries\n", chromkey.id2chrom(chrom1).c_str(), chromkey.id2chrom(chrom2).c_str(), qt.get_num_objs(), num_queries);

// 				for (uint64_t x = 0; x < chrom1_size; x += query_size) {
// 					for (uint64_t y = 0; y < chrom2_size; y += query_size) {
// 						qt.get_stat(Rectangle(x, y, x + query_size, y + query_size), s);
// 						num_queries++;
// 					}
// 				}
// 			}
// 		}
// 		REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);
// 		REprintf("Num queries: %ld\n", num_queries);

// //		REprintf("Running quad tree queries\n");
// //		cl = clock();
// //		for (int i = 0; i < num_queries; i++) {
// //			StatQuadTree<Point_n_val>::Stat &s = results2[i];
// //			qtree.get_stat(queries[i], s);
// //			if (i && i % 10000 == 0) {
// //				REprintf("%d quad tree queries done\n", i);
// //				check_interrupt();
// //			}
// //		}
// //		REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);
// 	} catch (TGLException &e) {
// 		rerror("%s", e.msg());
//     } catch (const bad_alloc &e) {
//         rerror("Out of memory");
//     }
// 	return R_NilValue;
// }

// // .Call("gtest_quadtree_rect", .misha_env(), silent = TRUE)

// SEXP gtest_bands(SEXP _envir) {
// 	try {
// 		RdbInitializer rdb_init;

// 		DiagonalBand band(-20, -10);
// 		Rectangle r(0, 0, 100, 100);
// 		REprintf("intersects: %d\n", band.do_intersect(r));
// 		REprintf("contains: %d\n", band.do_contain(r));
// 		band.shrink2intersected(r);
// 		REprintf("shrinked rect: %s\n", r.debug_str());
// 		REprintf("intersected area: %ld\n", band.intersected_area(r));
// 	} catch (TGLException &e) {
// 		rerror("%s", e.msg());
//     } catch (const bad_alloc &e) {
//         rerror("Out of memory");
//     }
// 	return R_NilValue;
// }

// SEXP gtest_create_2d(SEXP _track, SEXP _max_chrom_rects, SEXP _max_rect_size, SEXP _envir)
// {
// 	try {
// 		RdbInitializer rdb_init;

// 		if (!isString(_track) || length(_track) != 1)
// 			verror("Track argument is not a string");

// 		IntervUtils iu(_envir);

// 		const char *track = CHAR(STRING_ELT(_track, 0));
// 		string dirname = create_track_dir(_envir, track);
// 		char filename[FILENAME_MAX];
// 		GIntervals2D all_genome_intervs;
// 		iu.get_all_genome_intervs(all_genome_intervs);

// 		Progress_reporter progress;
// 		progress.init(all_genome_intervs.size(), 1);

// 		GenomeTrackRectsRects gtrack(iu.get_track_chunk_size(), iu.get_track_num_chunks());
// 		RectsQuadTree qtree;
// 		int chromid1 = -1;
// 		int chromid2 = -1;
// 		int max_num_rects = isReal(_max_chrom_rects) ? REAL(_max_chrom_rects)[0] : INTEGER(_max_chrom_rects)[0]; // 20000
// 		int max_rect_size = isReal(_max_rect_size) ? REAL(_max_rect_size)[0] : INTEGER(_max_rect_size)[0]; // 100

// 		for (GIntervals2D::const_iterator igenome = all_genome_intervs.begin(); igenome != all_genome_intervs.end(); ++igenome) {
// 			if (gtrack.opened())
// 				gtrack.write(qtree);

// 			if (unif_rand() > 0.2) { // skip creation some of the chromosome pairs
// 				chromid1 = igenome->chromid1();
// 				chromid2 = igenome->chromid2();

// 				uint64_t maxx = iu.get_chromkey().get_chrom_size(chromid1);
// 				uint64_t maxy = iu.get_chromkey().get_chrom_size(chromid2);

// 				qtree.reset(0, 0, maxx, maxy);
// 				snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_2d_filename(iu.get_chromkey(), chromid1, chromid2).c_str());
// 				gtrack.init_write(filename, chromid1, chromid2);

// 				int num_rects = (int)(unif_rand() * max_num_rects) + 1;
// 				int64_t x1, x2, y1, y2;

// 				for (int i = 0; i < num_rects; i++) {
// 					while (1) {
// 						check_interrupt();
// 						x1 = (int64_t)(unif_rand() * maxx);
// 						y1 = (int64_t)(unif_rand() * maxy);
// 						x2 = min((int64_t)(x1 + 1 + max_rect_size * unif_rand()), (int64_t)maxx);
// 						y2 = min((int64_t)(y1 + 1 + max_rect_size * unif_rand()), (int64_t)maxy);
// 						RectsQuadTree::ValueType rect(x1, y1, x2, y2, i);
// 						if (!qtree.do_intersect(rect)) {
// 							qtree.insert(rect);
// 							break;
// 						}
// 					}
// 				}
// 			}

// 			progress.report(1);
// 		}

// 		if (gtrack.opened())
// 			gtrack.write(qtree);

// 		progress.report_last();
// 	} catch (TGLException &e) {
// 		rerror("%s", e.msg());
//     } catch (const bad_alloc &e) {
//         rerror("Out of memory");
//     }

// 	return R_NilValue;
// }


// SEXP gtest_quadtree_rect(SEXP _envir) {

// 	try {
// 		RdbInitializer rdb_init;

// 		RectsQuadTree qtree;
// 		RectsQuadTreeCached cqtree;
// 		RectsQuadTreeCached cqtree2;
// 		RectsQuadTreeCachedSerializer cqtree_serializer;
// 		BufferedFile cqtree_file, cqtree_file2;

// 		int x1, x2, y1, y2;
// 		int64_t d1, d2;

// 		int maxcoord = 100000000;
// 		int max_rect_size = 10000;
// 		int num_rects =   150000;
// 		int num_queries = 1500000;
// 		int num_nn_queries = min(2000, num_queries);
// 		int max_query_size = 200000;
// 		int max_nearest_neighbors = 10;

// 		qtree.init(0, 0, maxcoord, maxcoord, 9, 20);
// 		cqtree.init(100000, 100);

// 		vector<RectsQuadTree::ValueType> rects;

// //qtree.insert(RectsQuadTree::ValueType(1000, 2000, 4000, 6000, 17));
// //RectsQuadTree::Stat ss;
// //qtree.get_stat(Rectangle(1100, 2100, 3900, 5900), ss);
// //REprintf("area %ld, avg %g\n", ss.occupied_area, ss.avg_val);
// //qtree.get_stat(Rectangle(3000, 3000, 5000, 7000), ss);
// //REprintf("area %ld, avg %g\n", ss.occupied_area, ss.avg_val);
// //verror("stamn");

// 		vector<Rectangle> queries;
// 		vector<DiagonalBand> bands;
// 		vector<RectsQuadTree::Stat> results1(num_queries);
// 		vector<RectsQuadTree::Stat> results2(num_queries);
// 		vector<RectsQuadTreeCached::Stat> results3(num_queries);
// 		vector<RectsQuadTreeCached::Stat> results4(num_queries);

// 		REprintf("Generating queries\n");
// 		for (int i = 0; i < num_queries; i++) {
// 			x1 = (int64_t)(unif_rand() * maxcoord);
// 			y1 = (int64_t)(unif_rand() * maxcoord);
// 			x2 = (int64_t)min(x1 + 1 + max_query_size * unif_rand(), (double)maxcoord);
// 			y2 = (int64_t)min(y1 + 1 + max_query_size * unif_rand(), (double)maxcoord);
// 			queries.push_back(Rectangle(x1, y1, x2, y2));

// 			d1 = (int64_t)(x1 - y1 + max_query_size * (2 * unif_rand() - 1));
// 			d2 = (int64_t)(x2 - y2 + max_query_size * (2 * unif_rand() - 1));
// 			bands.push_back(DiagonalBand(min(d1, d2), max(d1, d2)));
// 		}

// 		clock_t cl = clock();
// 		REprintf("Generating rects\n");
// 		for (int i = 0; i < num_rects; i++) {
// 			while (1) {
// 				x1 = (int64_t)(unif_rand() * maxcoord);
// 				y1 = (int64_t)(unif_rand() * maxcoord);
// 				x2 = min((int64_t)(x1 + 1 + max_rect_size * unif_rand()), (int64_t)maxcoord);
// 				y2 = min((int64_t)(y1 + 1 + max_rect_size * unif_rand()), (int64_t)maxcoord);
// 				RectsQuadTree::ValueType rect(x1, y1, x2, y2, i);
// 				if (!qtree.do_intersect(rect)) {
// 					qtree.insert(rect);
// 					rects.push_back(rect);
// 					break;
// 				}
// //
// //				vector<RectsQuadTree::ValueType>::const_iterator irect = rects.begin();
// //				for (; irect != rects.end(); ++irect) {
// //					if (irect->do_intersect(rect))
// //						break;
// //				}
// //				if (irect == rects.end()) {
// //					rects.push_back(rect);
// //					break;
// //				}
// 			}

// 			if (i && i % 10000 == 0) {
// 				REprintf("%d rects generated\n", i);
// 				check_interrupt();
// 			}
// 		}
// 		REprintf(" %2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);

// //		REprintf("Inserting rects to quad tree\n");
// //		cl = clock();
// //		for (int i = 0; i < rects.size(); ++i) {
// ////REprintf("\tRECT %d: %s\n", i, rects[i].debug_str());
// //			qtree.insert(rects[i]);
// //			if (i && i % 10000 == 0) {
// //				REprintf("%d rects inserted to quad tree\n", i);
// //				check_interrupt();
// //			}
// //		}
// //		REprintf(" %2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);

// 		REprintf("Rects: %llu\n", qtree.get_num_objs());
// 		check_interrupt();
// //
// //		REprintf("Converting quad tree to cached quad tree\n");
// //////REprintf("===============QUAD TREE================\n");
// //////qtree.debug_print_tree();
// //////REprintf("\n\n");
// //		cl = clock();
// //		int signature = 0x12345678;
// //		cqtree_file.open("2dtrack_cached", "w");
// //		cqtree_file.write(&signature, sizeof(signature));
// //		cqtree.serialize(cqtree_file, qtree);
// //		cqtree_file.close();
// //		REprintf(" %2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);
// //		check_interrupt();
// ////
// //////return R_NilValue;
// //		cqtree_file.open("2dtrack_cached", "r");
// //		int s;
// //		cqtree_file.read(&s, sizeof(signature));
// //		REprintf("signature OK: %d\n", signature == s);
// //		cqtree.unserialize(cqtree_file);

// // .Call("gtest_quadtree_rect", .misha_env(), silent = TRUE)
// //		REprintf("Constructing cached HUGE quad tree via serializer\n");
// //		cl = clock();
// //		int num_subtrees = 64;
// //		int dim = sqrt(num_subtrees);
// //		cqtree_file2.open("2dtrack_cached_serializer", "w");
// //		cqtree_file2.write(&signature, sizeof(signature));
// //		cqtree_serializer.begin(cqtree_file2, 0, 0, dim * maxcoord, dim * maxcoord, num_subtrees, 100000, 100, 9, 20);
// //
// //		for (uint64_t i = 0; i < dim; ++i) {
// //			for (uint64_t j = 0; j < dim; ++j) {
// //				for (vector<RectsQuadTree::ValueType>::const_iterator irect = rects.begin(); irect < rects.end(); ++irect) {
// //					RectsQuadTree::ValueType r(irect->x1 + maxcoord * i, irect->y1 + maxcoord * j,
// //											   irect->x2 + maxcoord * i, irect->y2 + maxcoord * j,
// //											   irect->v);
// //					cqtree_serializer.insert(r);
// //				}
// //check_interrupt();
// //REprintf("Subtree [%ld, %ld] generated\n", i, j);
// //			}
// //		}
// //
// //		cqtree_serializer.end();
// //
// //		cqtree_file2.close();
// //		REprintf(" %2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);
// //		check_interrupt();
// //
// //		cqtree_file2.open("2dtrack_cached_serializer", "r");
// //		cqtree_file2.read(&s, sizeof(signature));
// //		REprintf("signature OK: %d\n", signature == s);
// //		cqtree2.unserialize(cqtree_file2);
// //return R_NilValue;


// //		REprintf("Constructing cached quad tree via serializer\n");
// //////REprintf("===============QUAD TREE================\n");
// //////qtree.debug_print_tree();
// //////REprintf("\n\n");
// //		cl = clock();
// //		int num_subtrees = 64;
// //		cqtree_file2.open("2dtrack_cached_serializer", "w");
// //		cqtree_file2.write(&signature, sizeof(signature));
// //		cqtree_serializer.begin(cqtree_file2, 0, 0, maxcoord, maxcoord, num_subtrees, 100000, 100, 9, 20);
// //		vector< vector<RectsQuadTree::ValueType> > subarenas_rects(num_subtrees);
// //
// //		const Rectangles &subarenas = cqtree_serializer.get_subarenas();
// //		for (uint64_t i = 0; i < rects.size(); ++i) {
// //			for (uint64_t j = 0; j < num_subtrees; ++j) {
// //				if (rects[i].do_intersect(subarenas[j])) {
// //					 subarenas_rects[j].push_back(rects[i]);
// //					 break;
// //				}
// //			}
// //		}
// //		for (uint64_t i = 0; i < num_subtrees; ++i) {
// //			for (vector<RectsQuadTree::ValueType>::const_iterator irect = subarenas_rects[i].begin(); irect != subarenas_rects[i].end(); ++irect)
// //				cqtree_serializer.insert(*irect);
// //		}
// //
// //		cqtree_serializer.end();
// //
// //		cqtree_file2.close();
// //		REprintf(" %2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);
// //		check_interrupt();
// //
// //		cqtree_file2.open("2dtrack_cached_serializer", "r");
// //		cqtree_file2.read(&s, sizeof(signature));
// //		REprintf("signature OK: %d\n", signature == s);
// //		cqtree2.unserialize(cqtree_file2);



// //cqtree.debug_print_tree();

// //		{
// //			REprintf("Cached quad tree1:\n");
// //			RectsQuadTreeCached::Iterator icqtree(&cqtree);
// //			int num_objs = 0;
// //			for (icqtree.begin(); !icqtree.is_end(); icqtree.next()) {
// //				num_objs++;
// //				REprintf("  %s\n", icqtree->debug_str());
// //				REprintf("  Containing quad: %s\n", icqtree.containing_quad().debug_str());
// //			}
// //			REprintf("Num objs: %d\n", num_objs);
// //		}
// //
// //		{
// //			REprintf("Cached quad tree2:\n");
// //			RectsQuadTreeCached::Iterator icqtree(&cqtree2);
// //			int num_objs = 0;
// //			for (icqtree.begin(); !icqtree.is_end(); icqtree.next()) {
// //				num_objs++;
// //				REprintf("  %s\n", icqtree->debug_str());
// //				REprintf("  Containing quad: %s\n", icqtree.containing_quad().debug_str());
// //			}
// //			REprintf("Num objs: %d\n", num_objs);
// //		}


// //		REprintf("Running intersect queries\n");
// //		cl = clock();
// //		vector<unsigned> v1;
// //		vector<unsigned> v2;
// //		vector<Rectangle> intersection;
// //		for (int i = 0; i < num_queries; i++) {
// //			v1.clear();
// //			for (vector<RectsQuadTree::ValueType>::const_iterator irect = rects.begin(); irect != rects.end(); ++irect) {
// //				if (irect->do_intersect(queries[i]))
// //					v1.push_back(irect - rects.begin());
// //			}
// //
// //			qtree.intersect(queries[i], intersection, v2);
// //
// //			sort(v1.begin(), v1.end());
// //			sort(v2.begin(), v2.end());
// //			if (v1 != v2) {
// //				REprintf("Query %d %s\n", i, queries[i].debug_str());
// //				REprintf("Plain:\n");
// //				for (unsigned i = 0; i < v1.size(); i++)
// //					REprintf("  %d. %s\n", v1[i], rects[v1[i]].debug_str());
// //				REprintf("Quad-tree:\n");
// //				for (unsigned i = 0; i < v2.size(); i++)
// //					REprintf("  %d. %s\n", v2[i], rects[v2[i]].debug_str());
// //				getchar();
// //				check_interrupt();
// //			}
// //
// //			if (i && i % 10000 == 0) {
// //				REprintf("%d intersect queries done\n", i);
// //				check_interrupt();
// //			}
// //		}
// //		REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);
// //
// //		REprintf("Running intersect with bands queries\n");
// //		cl = clock();
// //		vector<unsigned> v1;
// //		vector<unsigned> v2;
// //		vector<Rectangle> intersection1;
// //		vector<Rectangle> intersection2;
// //		for (int i = 0; i < num_queries; i++) {
// ////for (int i=2014762; i < num_queries; i++) {
// //			v1.clear();
// //			intersection1.clear();
// //			for (vector<RectsQuadTree::ValueType>::const_iterator irect = rects.begin(); irect != rects.end(); ++irect) {
// //				if (irect->do_intersect(queries[i])) {
// //					Rectangle intersection(irect->intersect(queries[i]));
// //					if (bands[i].do_intersect(intersection)) {
// //						bands[i].shrink2intersected(intersection);
// //						v1.push_back(irect - rects.begin());
// //						intersection1.push_back(intersection);
// //					}
// //				}
// //			}
// //
// //			qtree.intersect(queries[i], bands[i], intersection2, v2);
// //
// //			sort(v1.begin(), v1.end());
// //			sort(v2.begin(), v2.end());
// //			sort(intersection1.begin(), intersection1.end(), sort_rects);
// //			sort(intersection2.begin(), intersection2.end(), sort_rects);
// //			if (v1 != v2 || intersection1 != intersection2) {
// //				REprintf("Query %d %s\n", i, queries[i].debug_str());
// //				REprintf("Bands %ld, %ld\n", bands[i].d1, bands[i].d2);
// //				REprintf("Plain (%ld, %ld):\n", v1.size(), intersection1.size());
// //				for (unsigned idx = 0; idx < v1.size(); idx++) {
// //					REprintf("  %d. %s\n", idx, intersection1[idx].debug_str());
// //					REprintf("  %d. %s. Intersects with band? %d\n", v1[idx], rects[v1[idx]].debug_str(), bands[i].do_intersect(rects[v1[idx]]));
// //				}
// //				REprintf("Quad-tree (%ld, %ld):\n", v2.size(), intersection2.size());
// //				for (unsigned idx = 0; idx < v2.size(); idx++) {
// //					REprintf("  %d. %s\n", idx, intersection2[idx].debug_str());
// //					REprintf("  %d. %s\n", v2[idx], rects[v2[idx]].debug_str());
// //					Rectangle r(queries[i].intersect(rects[v2[idx]]));
// //					bands[i].shrink2intersected(r);
// //					REprintf("shrinked: %s\n", r.debug_str());
// //				}
// //				getchar();
// //				check_interrupt();
// //			}
// //
// //			if (i && i % 10000 == 0) {
// //				REprintf("%d intersect queries done\n", i);
// //				check_interrupt();
// //			}
// //		}
// //		REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);
// //
// //		REprintf("Running do intersect queries\n");
// //		cl = clock();
// //		for (int i = 0; i < num_queries; i++) {
// //			bool r1 = false;
// //			for (vector<RectsQuadTree::ValueType>::const_iterator irect = rects.begin(); irect != rects.end(); ++irect) {
// //				if (irect->do_intersect(queries[i])) {
// //					r1 = true;
// //					break;
// //				}
// //			}
// //
// //			bool r2 = qtree.do_intersect(queries[i]);

// //			if (r1 != r2) {
// //				REprintf("Query %s: %d vs. %d\n", queries[i].debug_str(), r1, r2);
// //				getchar();
// //				check_interrupt();
// //			}
// //
// //			bool r3 = cqtree.do_intersect(queries[i]);
// //
// //			if (r2 != r3) {
// //				REprintf("Query %s: %d vs. %d\n", queries[i].debug_str(), r2, r3);
// //				getchar();
// //				check_interrupt();
// //			}
// //
// //			bool r4 = cqtree2.do_intersect(queries[i]);
// //
// //			if (r3 != r4) {
// //				REprintf("Query %s: %d vs. %d\n", queries[i].debug_str(), r3, r4);
// //				getchar();
// //				check_interrupt();
// //			}
// //
// //			if (i && i % 10000 == 0) {
// //				REprintf("%d do intersect queries done\n", i);
// //				check_interrupt();
// //			}
// //		}
// //		REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);

// 		REprintf("Running nearest neighbor queries\n");
// 		{
// 			cl = clock();
// 			vector<NNRes> r1;
// 			vector<NNRes> r2;

// 			for (int i = 0; i < num_nn_queries; ++i) {
// 				Rectangles::const_iterator iquery = queries.begin() + i;
// 				RectsQuadTree::NNIterator inn(&qtree);
// 				int nearest_neighbors = 0;

// 				r1.clear();
// //				REprintf("\tQuery: %s\n", iquery->debug_str());
// 				for (inn.begin(*iquery); !inn.is_end(); inn.next()) {
// 					NNRes res;
// 					res.obj = *inn;
// 					res.dist = iquery->manhattan_dist(res.obj);
// 					if (r1.size() && res.dist != r1.back().dist) {
// 						nearest_neighbors++;
// 						if (nearest_neighbors >= max_nearest_neighbors)
// 							break;
// 					}
// 					r1.push_back(res);
// //					REprintf("\tr size %ld, %s, xdist: %ld, ydist: %ld, dist: %ld\n",
// //						   r1.size(), res.obj.debug_str(), res.obj.xdist(*iquery), res.obj.ydist(*iquery), res.obj.manhattan_dist(*iquery));
// 				}

// 				r2.clear();
// 				r2.reserve(rects.size());
// 				for (vector<RectsQuadTree::ValueType>::const_iterator irect = rects.begin(); irect < rects.end(); ++irect) {
// 					NNRes res;
// 					res.obj = *irect;
// 					res.dist = iquery->manhattan_dist(res.obj);
// 					r2.push_back(res);
// 				}

// 				sort(r1.begin(), r1.end());
// 				sort(r2.begin(), r2.end());
// 				if (r1.size() < max_nearest_neighbors) {
// 					for (uint64_t j = 0; j < min(r1.size(), (uint64_t)max_nearest_neighbors); ++j)
// 						REprintf("\tQuad-tree: %s, dist: %lld\n", r1[j].obj.debug_str(), r1[j].dist);
// 					for (uint64_t j = 0; j < min(r2.size(), (uint64_t)max_nearest_neighbors); ++j)
// 						REprintf("\tPlain:     %s, dist: %lld\n", r2[j].obj.debug_str(), r2[j].dist);
// 					getchar();
// 					check_interrupt();
// 				}

// 				for (uint64_t j = 0; j < r1.size(); ++j) {
// 					if (!(r1[j] == r2[j])) {
// 						REprintf("\tQuad-tree: %s, dist: %lld\n", r1[j].obj.debug_str(), r1[j].dist);
// 						REprintf("\tPlain: %s, dist: %lld\n", r2[j].obj.debug_str(), r2[j].dist);
// 						getchar();
// 						check_interrupt();
// 					}
// 				}

// 				if (i && i % 100 == 0) {
// 					REprintf("%d nearest neighbor queries done\n", i);
// 					check_interrupt();
// 				}
// 			}

// 			REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);
// 		}

// //		REprintf("Running stat plain queries\n");
// //		cl = clock();
// //		for (int i = 0; i < num_queries; i++) {
// //			RectsQuadTree::Stat &s = results1[i];
// //			s.occupied_area = 0;
// //			s.weighted_sum = 0.;
// //			s.min_val = numeric_limits<double>::max();
// //			s.max_val = -numeric_limits<double>::max();
// //			for (vector<RectsQuadTree::ValueType>::const_iterator irect = rects.begin(); irect != rects.end(); ++irect) {
// //				int64_t intersected_area = irect->intersected_area(queries[i]);
// //				if (intersected_area > 0) {
// //					s.weighted_sum += irect->v * (double)intersected_area;
// //					s.max_val = max(s.max_val, (double)irect->v);
// //					s.min_val = min(s.min_val, (double)irect->v);
// //					s.occupied_area += intersected_area;
// //				}
// //			}
// //			if (i && i % 10000 == 0) {
// //				REprintf("%d plain queries done\n", i);
// //				check_interrupt();
// //			}
// //		}
// //		REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);
// //
// //		REprintf("Running stat quad tree queries\n");
// //		cl = clock();
// //		for (int i = 0; i < num_queries; i++) {
// //			RectsQuadTree::Stat &s = results2[i];
// //			qtree.get_stat(queries[i], s);
// //			if (i && i % 10000 == 0) {
// //				REprintf("%d quad tree queries done\n", i);
// //				check_interrupt();
// //			}
// //		}
// //		REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);
// //
// //		REprintf("Running stat cached quad tree queries\n");
// //		cl = clock();
// //		for (int i = 0; i < num_queries; i++) {
// //			RectsQuadTreeCached::Stat &s = results3[i];
// //			cqtree.get_stat(queries[i], s);
// //			if (i && i % 10000 == 0) {
// //				REprintf("%d cached quad tree queries done\n", i);
// //				check_interrupt();
// //			}
// //		}
// //		REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);
// //
// //		REprintf("Running stat cached quad tree + serializer queries\n");
// //		cl = clock();
// //		for (int i = 0; i < num_queries; i++) {
// //			RectsQuadTreeCached::Stat &s = results4[i];
// //			cqtree2.get_stat(queries[i], s);
// //			if (i && i % 10000 == 0) {
// //				REprintf("%d cached quad tree queries + serializer done\n", i);
// //				check_interrupt();
// //			}
// //		}
// //		REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);
// //
// //		REprintf("Running stat plain queries with bands\n");
// //		cl = clock();
// //		for (int i = 0; i < num_queries; i++) {
// //			RectsQuadTree::Stat &s = results1[i];
// //			s.occupied_area = 0;
// //			s.weighted_sum = 0.;
// //			s.min_val = numeric_limits<double>::max();
// //			s.max_val = -numeric_limits<double>::max();
// //			for (vector<RectsQuadTree::ValueType>::const_iterator irect = rects.begin(); irect != rects.end(); ++irect) {
// //				if (irect->do_intersect(queries[i])) {
// //					Rectangle intersection = irect->intersect(queries[i]);
// //					if (bands[i].do_intersect(intersection)) {
// //						bands[i].shrink2intersected(intersection);
// //						int64_t intersected_area = bands[i].intersected_area(intersection);
// //						s.weighted_sum += irect->v * (double)intersected_area;
// //						s.max_val = max(s.max_val, (double)irect->v);
// //						s.min_val = min(s.min_val, (double)irect->v);
// //						s.occupied_area += intersected_area;
// //					}
// //				}
// //			}
// //			if (i && i % 10000 == 0) {
// //				REprintf("%d plain queries done\n", i);
// //				check_interrupt();
// //			}
// //		}
// //		REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);
// //
// //		REprintf("Running stat cached quad tree queries with bands\n");
// //		cl = clock();
// //		for (int i = 0; i < num_queries; i++) {
// //			RectsQuadTreeCached::Stat &s = results3[i];
// //			cqtree.get_stat(queries[i], bands[i], s);
// ////if (i == 232)
// ////REprintf("(%d) cached %ld %g, %g, %g\n", i, s.occupied_area, s.weighted_sum, s.max_val, s.min_val);
// //			if (i && i % 100000 == 0) {
// //				REprintf("%d cached quad tree queries done\n", i);
// //				check_interrupt();
// //			}
// //		}
// //		REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);


// //		REprintf("Serializing quad tree\n");
// //		cl = clock();
// //		BufferedFile file;
// //
// //		file.open("2dtrack", "w");
// //		qtree.serialize(file);
// //		file.close();
// //		RectsQuadTree qtree2;
// //		file.open("2dtrack", "r");
// //		qtree2.unserialize(file);
// //		REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);
// //
// //		REprintf("Running unserialized quad tree queries\n");
// //		cl = clock();
// //
// //		for (int i = 0; i < num_queries; i++) {
// //			RectsQuadTree::Stat &s = results1[i];
// //			qtree2.get_stat(queries[i], s);
// //			if (i && i % 10000 == 0) {
// //				REprintf("%d quad tree queries done\n", i);
// //				check_interrupt();
// //			}
// //		}
// //		REprintf(" %.2g secs\n", (clock() - cl) / (double)CLOCKS_PER_SEC);
// //

// //		REprintf("Comparing results\n");
// //int num_nans = 0;
// //		for (int i = 0; i < num_queries; i++) {
// //			RectsQuadTree::Stat &s1 = results1[i];
// //			RectsQuadTree::Stat &s2 = results2[i];
// //			RectsQuadTreeCached::Stat &s3 = results3[i];
// //			RectsQuadTreeCached::Stat &s4 = results4[i];
// //			if (s1.occupied_area != s2.occupied_area ||
// //					(s1.occupied_area || s2.occupied_area) &&
// //					(fabs(s1.weighted_sum - s2.weighted_sum) > 0.0000001 || s1.min_val != s2.min_val || s1.max_val != s2.max_val || s1.occupied_area != s2.occupied_area))
// //			{
// //				check_interrupt();
// //				REprintf("(%d) Not equal %ld %g, %g, %g   vs.     %ld %g, %g, %g\n", i, s1.occupied_area, s1.weighted_sum, s1.max_val, s1.min_val, s2.occupied_area, s2.weighted_sum, s2.max_val, s2.min_val);
// //				getchar();
// //			}
// //
// //if (s1.occupied_area == 0)
// //num_nans++;
// //			if (s1.occupied_area != s3.occupied_area ||
// //					(s1.occupied_area || s3.occupied_area) &&
// //					(fabs(s1.weighted_sum - s3.weighted_sum) > 0.0000001 || s1.min_val != s3.min_val || s1.max_val != s3.max_val || s1.occupied_area != s3.occupied_area))
// //			{
// //				check_interrupt();
// //				REprintf("(%d) Not equal %ld %g, %g, %g   vs.     %ld %g, %g, %g\n", i, s1.occupied_area, s1.weighted_sum, s1.max_val, s1.min_val, s3.occupied_area, s3.weighted_sum, s3.max_val, s3.min_val);
// //				getchar();
// //			}

// //			if (memcmp(&s2, &s3, sizeof(s3))) {
// //				check_interrupt();
// //				REprintf("(%d) Not equal %ld %g, %g, %g   vs.     %ld %g, %g, %g\n", i, s2.occupied_area, s2.weighted_sum, s2.max_val, s2.min_val, s3.occupied_area, s3.weighted_sum, s3.max_val, s3.min_val);
// //				getchar();
// //			}

// //			if (s4.occupied_area != s3.occupied_area ||
// //					(s4.occupied_area || s3.occupied_area) &&
// //					(fabs(s4.weighted_sum - s3.weighted_sum) > 0.0000001 || s4.min_val != s3.min_val || s4.max_val != s3.max_val || s4.occupied_area != s3.occupied_area))
// //			{
// //				check_interrupt();
// //				REprintf("(%d) Not equal %ld %g, %g, %g   vs.     %ld %g, %g, %g\n", i, s4.occupied_area, s4.weighted_sum, s4.max_val, s4.min_val, s3.occupied_area, s3.weighted_sum, s3.max_val, s3.min_val);
// //				getchar();
// //			}
// //		}
// //REprintf("Nans: %d (%d)\n", num_nans, int(100 * num_nans / (double)num_queries + .5));


// 		REprintf("Comparison done\n");
// 	} catch (TGLException &e) {
// 		rerror("%s", e.msg());
//     } catch (const bad_alloc &e) {
//         rerror("Out of memory");
//     }
// 	return R_NilValue;
// }

// SEXP gtest_percentiles(SEXP _percentile, SEXP _vals, SEXP _rnd_sampling_buf_size, SEXP _lowest_vals_buf_size, SEXP _highest_vals_buf_size, SEXP _envir) {
// 	try {
// 		RdbInitializer rdb_init;

// //		StreamPercentiler<double> sp(REAL(_rnd_sampling_buf_size)[0], REAL(_lowest_vals_buf_size)[0], REAL(_highest_vals_buf_size)[0]);
// //		for (int i = 0; i < length(_vals); i++)
// //			sp.add(REAL(_vals)[i]);
// //
// //		bool estimation;
// //
// //		REprintf("Percentile: %g\n", sp.get_percentile(REAL(_percentile)[0], estimation));
// //		REprintf("Is estimation: %d\n", estimation);
// //		REprintf("\n");
// //		REprintf("Stream size: %ld\n", sp.m_stream_sampler.stream_size());
// //		REprintf("Reserv size: %ld\n", sp.m_stream_sampler.reservoir_size());
// //		for (uint64_t i = 0; i < sp.m_stream_sampler.samples().size(); i++) {
// //			REprintf("%g ", sp.m_stream_sampler.samples()[i]);
// //		}
// //		REprintf("\n\n");
// //
// //		REprintf("Lowest heap size: %ld\n", sp.m_extreme_vals[0].size());
// //		for (uint64_t i = 0; i < sp.m_extreme_vals[0].size(); i++) {
// //			REprintf("%g ", sp.m_extreme_vals[0][i]);
// //		}
// //		REprintf("\n\n");
// //
// //
// //		REprintf("Highest heap size: %ld\n", sp.m_extreme_vals[1].size());
// //		for (uint64_t i = 0; i < sp.m_extreme_vals[1].size(); i++) {
// //			REprintf("%g ", sp.m_extreme_vals[1][i]);
// //		}
// //		REprintf("\n\n");
// 	} catch (TGLException &e) {
// 		rerror("%s", e.msg());
//     } catch (const bad_alloc &e) {
//         rerror("Out of memory");
//     }
// 	return R_NilValue;
// }

// SEXP gcreate_arrays_track(SEXP _track, SEXP _minsize, SEXP _maxsize, SEXP _expr, SEXP _scope, SEXP _iterator_policy, SEXP _envir)
// {
// 	try {
// 		RdbInitializer rdb_init;

// 		if (!isString(_track) || length(_track) != 1)
// 			verror("Track argument is not a string");

// 		if (!isReal(_minsize) || length(_minsize) != 1) 
// 			verror("Min size argment is not a number");

// 		if (!isReal(_maxsize) || length(_maxsize) != 1) 
// 			verror("Max size argment is not a number");

// 		if (!isString(_expr) || length(_expr) != 1)
// 			verror("Tracks expression argument must be a string");

// 		const char *track_str = CHAR(STRING_ELT(_track, 0));
// 		string dirname = create_track_dir(_envir, track_str);
// 		int minsize = (int)REAL(_minsize)[0];
// 		int maxsize = (int)REAL(_maxsize)[0];

// 		IntervUtils iu(_envir);
// 		GIntervals scope1d;
// 		GIntervals all_genome_intervs1d;

// 		iu.convert_rintervs(_scope, &scope1d, NULL);
// 		scope1d.sort();
// 		scope1d.unify_overlaps();

// 		int cur_chromid = -1;
// 		GenomeTrackArrays gtrack;
// 		set<int> created_chromids;
// 		char filename[FILENAME_MAX];

// 		TrackExprScanner scanner(iu);
// 		iu.get_all_genome_intervs(all_genome_intervs1d);
// 		scanner.begin(_expr, &scope1d, NULL, _iterator_policy, R_NilValue);
// 		GenomeTrackArrays::ArrayVals array_vals(maxsize);
// 		int counter = 0;

// 		for (GenomeTrackArrays::ArrayVals::iterator iarray_val = array_vals.begin(); iarray_val != array_vals.end(); ++iarray_val)
// 			iarray_val->idx = iarray_val - array_vals.begin();

// 		for (; !scanner.isend(); scanner.next()) {
// 			if (cur_chromid != scanner.last_interval1d().chromid) {
// 				cur_chromid = scanner.last_interval1d().chromid;
// 				snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_1d_filename(iu.get_chromkey(), cur_chromid).c_str());
// 				gtrack.init_write(filename, cur_chromid);
// 				created_chromids.insert(cur_chromid);
// 			}

// 			unsigned array_size = minsize + (int)(unif_rand() * (maxsize - minsize + 1));
// 			for (unsigned i = 0; i < array_size; ++i) {
// 				unsigned idx = (unsigned)(unif_rand() * (maxsize - i));
// 				swap(array_vals[idx + i], array_vals[i]);
// 				array_vals[i].val = array_vals[i].idx + counter * 100;
// 			}
// 			sort(array_vals.begin(), array_vals.begin() + array_size);
// if (counter < 10) {
// for (GenomeTrackArrays::ArrayVals::const_iterator iarray_val = array_vals.begin(); iarray_val != array_vals.begin() + array_size; ++iarray_val)
// REprintf("%d-%d ", iarray_val->idx, (int)iarray_val->val);
// REprintf("\n");
// }
// 			gtrack.write_next_interval(scanner.last_interval1d(), array_vals.begin(), array_vals.begin() + array_size);

// 			++counter;
// 		}

// 		// some of the chromosome could be previously skipped; we still must create them even if they are empty
// 		for (GIntervals::const_iterator iinterv = all_genome_intervs1d.begin(); iinterv != all_genome_intervs1d.end(); ++iinterv) {
// 			if (created_chromids.find(iinterv->chromid) == created_chromids.end()) {
// 				snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_1d_filename(iu.get_chromkey(), iinterv->chromid).c_str());
// 				gtrack.init_write(filename, iinterv->chromid);
// 			}
// 		}

// 		SEXP colnames = RSaneAllocVector(STRSXP, maxsize);
// 		for (int i = 0; i < maxsize; i++) {
// 			char buf[100];
// 			snprintf(buf, sizeof(buf), "col%d", i);
// 			SET_STRING_ELT(colnames, i, mkChar(buf));
// 		}

// 		return colnames;
// 	} catch (TGLException &e) {
// 		rerror("%s", e.msg());
//     } catch (const bad_alloc &e) {
//         rerror("Out of memory");
//     }

// 	return R_NilValue;
// }


// SEXP _gtest_error_recovery(SEXP _fname, SEXP _envir)
// {
// 	try {
// 		RdbInitializer rdb_init;

// 		SEXP test_val = RSaneAllocVector(REALSXP, 2);
// 		REAL(test_val)[0] = 35;
// 		REAL(test_val)[1] = 73;
// 		RSaneSerialize(test_val, "aaaa4");

// 		SEXP rval = RSaneUnserialize(CHAR(STRING_ELT(_fname, 0)));

// 		REprintf("After2...\n");
// 		if (isReal(rval))
// 			REprintf("Val = %g\n", REAL(rval)[0]);

// 		return R_NilValue;
// 	} catch (TGLException &e) {
// 		rerror("%s", e.msg());
//     } catch (const bad_alloc &e) {
//         rerror("Out of memory");
//     }

// 	return R_NilValue;
// }

// }
