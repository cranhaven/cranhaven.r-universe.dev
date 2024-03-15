#include <cstdint>
#include <cmath>

#include "BinFinder.h"

#include "GenomeTrackFixedBin.h"
#include "GenomeTrackRects.h"
#include "GenomeTrackSparse.h"
#include "GIntervalsBigSet1D.h"
#include "GIntervalsBigSet2D.h"

#include "rdbinterval.h"
#include "rdbutils.h"
#include "TrackExpressionFixedBinIterator.h"
#include "TrackExpressionScanner.h"

using namespace std;
using namespace rdb;

static double get_bin_tranformed_value(unsigned numexpr, bool force_binning, SEXP lookup_table, const TrackExprScanner &scanner, vector<BinFinder> &bin_finders, const vector<unsigned> &track_mult)
{
	bool nan = false;
	unsigned index = 0;

	for (unsigned i = 0; i < numexpr; ++i) {
		double val = scanner.last_real(i);

		if (std::isnan(val)) {
			nan = true;
			break;
		} else {
			int bin = bin_finders[i].val2bin(val);

			if (bin < 0 && force_binning)
				bin = val <= bin_finders[i].get_breaks().front() ? 0 : bin_finders[i].get_numbins() - 1;

			if (bin >= 0)
				index += bin * track_mult[i];
			else {
				nan = true;
				break;
			}
		}
	}

	double value;

	if (nan)
		value = numeric_limits<double>::quiet_NaN();
	else {
		if ((int)index >= length(lookup_table))
			verror("Internal error: index %d is out of range", index);

		value = isReal(lookup_table) ? REAL(lookup_table)[index] : INTEGER(lookup_table)[index];
	}

	return value;
}

static SEXP build_rintervals_bintransform(GIntervalsFetcher1D *out_intervals1d, GIntervalsFetcher2D *out_intervals2d, const vector<double> &values,
										  vector<unsigned> *interv_ids, SEXP _exprs, IntervUtils &iu)
{
	enum { VALUE, ID, NUM_COLS };

	SEXP answer;
	unsigned num_interv_cols;

	if (out_intervals1d) {
		answer = iu.convert_intervs(out_intervals1d, interv_ids ? GInterval::NUM_COLS + NUM_COLS : GInterval::NUM_COLS + NUM_COLS - 1, false);
		num_interv_cols = GInterval::NUM_COLS;
	} else {
		answer = iu.convert_intervs(out_intervals2d, interv_ids ? GInterval2D::NUM_COLS + NUM_COLS : GInterval2D::NUM_COLS + NUM_COLS - 1, false);
		num_interv_cols = GInterval2D::NUM_COLS;
	}

	SEXP rvals;
	rprotect(rvals = RSaneAllocVector(REALSXP, values.size()));

    for (unsigned i = 0; i < values.size(); ++i)
        REAL(rvals)[i] = values[i];

    SET_VECTOR_ELT(answer, num_interv_cols + VALUE, rvals);

	SEXP col_names = getAttrib(answer, R_NamesSymbol);
	SET_STRING_ELT(col_names, num_interv_cols + VALUE, mkChar("value"));

	if (interv_ids) {
		SEXP ids;
		rprotect(ids = RSaneAllocVector(INTSXP, interv_ids->size()));
		for (vector<unsigned>::const_iterator iid = interv_ids->begin(); iid != interv_ids->end(); ++iid)
			INTEGER(ids)[iid - interv_ids->begin()] = *iid;
		SET_VECTOR_ELT(answer, num_interv_cols + ID, ids);

		SET_STRING_ELT(col_names, num_interv_cols + ID, mkChar("intervalID"));
	}

	return answer;
}


extern "C" {

SEXP gbintransform(SEXP _intervals, SEXP _track_exprs, SEXP _breaks, SEXP _include_lowest, SEXP _force_binning, SEXP _lookup_table,
				   SEXP _iterator_policy, SEXP _band, SEXP _intervals_set_out, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		// check the arguments
		if (!isString(_track_exprs) || length(_track_exprs) < 1)
			verror("Track argument is not a string vector");

		unsigned numexpr = length(_track_exprs);

		if (!isVector(_breaks))
			verror("Breaks argument must be a vector");

		if (length(_breaks) != (int)numexpr)
			verror("Number of breaks sets must be equal to the number of tracks used");

		if (!isLogical(_include_lowest) || length(_include_lowest) != 1)
			verror("include.lowest argument is not logical");

		if (!isLogical(_force_binning) || length(_force_binning) != 1)
			verror("include.lowest argument is not logical");

		if (!isNumeric(_lookup_table))
			verror("Lookup table argument must be numeric");

		if (!isNull(_intervals_set_out) && (!isString(_intervals_set_out) || length(_intervals_set_out) != 1))
			verror("intervals.set.out argument is not a string");

		string intervset_out = isNull(_intervals_set_out) ? "" : CHAR(STRING_ELT(_intervals_set_out, 0));
		bool include_lowest = LOGICAL(_include_lowest)[0];
		bool force_binning = LOGICAL(_force_binning)[0];
		vector<BinFinder> bin_finders;
		vector<unsigned> track_mult(numexpr);
		unsigned totalbins = 1;

		bin_finders.reserve(numexpr);
		for (unsigned i = 0; i < numexpr; ++i) {
			SEXP breaks = VECTOR_ELT(_breaks, i);

			if (!isReal(breaks))
				verror("Breaks[%d] is not numeric", i);

			bin_finders.push_back(BinFinder());
			bin_finders.back().init(REAL(breaks), length(breaks), include_lowest);

			totalbins *= bin_finders.back().get_numbins();
			track_mult[i] = !i ? 1 : track_mult[i - 1] * bin_finders[i - 1].get_numbins();
		}

		if ((int)totalbins != length(_lookup_table))
			verror("Lookup table length (%d) must match the range of the bins (%d)", length(_lookup_table), totalbins);

		IntervUtils iu(_envir);
		GIntervalsFetcher1D *intervals1d = NULL;
		GIntervalsFetcher2D *intervals2d = NULL;
		iu.convert_rintervs(_intervals, &intervals1d, &intervals2d);
		unique_ptr<GIntervalsFetcher1D> intervals1d_guard(intervals1d);
		unique_ptr<GIntervalsFetcher2D> intervals2d_guard(intervals2d);
		intervals1d->sort();
		intervals2d->sort();
		intervals2d->verify_no_overlaps(iu.get_chromkey());

		GIntervals out_intervals1d;
		GIntervals2D out_intervals2d;
		vector<unsigned> ids;
		vector<double> out_values;
		vector<GIntervalsBigSet1D::ChromStat> chromstats1d;
		vector<GIntervalsBigSet2D::ChromStat> chromstats2d;
		GInterval last_scope_interval1d;
		GInterval2D last_scope_interval2d;
		uint64_t size;
		char error_prefix[1000];

		TrackExprScanner scanner(iu);

		bool is_1d_iterator = iu.is_1d_iterator(_track_exprs, intervals1d, intervals2d, _iterator_policy);

		if (iu.get_multitasking() && !iu.prepare4multitasking(_track_exprs, intervals1d, intervals2d, _iterator_policy, _band))
			rreturn(R_NilValue);

		if (!intervset_out.empty()) {
			if (is_1d_iterator)
				GIntervalsBigSet1D::begin_save(intervset_out.c_str(), iu, chromstats1d);
			else
				GIntervalsBigSet2D::begin_save(intervset_out.c_str(), iu, chromstats2d);
		}

		if (iu.get_multitasking()) {
			if (!intervset_out.empty()) {
				if (iu.distribute_task(is_1d_iterator ?
									   sizeof(GIntervalsBigSet1D::ChromStat) * chromstats1d.size() :
									   sizeof(GIntervalsBigSet2D::ChromStat) * chromstats2d.size(),
									   0))
				{ // child process
					GIntervalsFetcher1D *kid_intervals1d = iu.get_kid_intervals1d();
					GIntervalsFetcher2D *kid_intervals2d = iu.get_kid_intervals2d();
					TrackExprScanner scanner(iu);

					scanner.begin(_track_exprs, kid_intervals1d, kid_intervals2d, _iterator_policy, _band);

					while (!scanner.isend()) {
						out_values.push_back(get_bin_tranformed_value(numexpr, force_binning, _lookup_table, scanner, bin_finders, track_mult));

						if (is_1d_iterator) {
							if (last_scope_interval1d.chromid != scanner.last_scope_interval1d().chromid) {
								last_scope_interval1d = scanner.last_scope_interval1d();
								snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chrom %s",
										intervset_out.c_str(), iu.id2chrom(last_scope_interval1d.chromid).c_str());
							}
							out_intervals1d.push_back(scanner.last_interval1d());
							size = out_intervals1d.size();
						} else {
							if (!last_scope_interval2d.is_same_chrom(scanner.last_scope_interval2d())) {
								last_scope_interval2d = scanner.last_scope_interval2d();
								snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chroms (%s, %s)",
										intervset_out.c_str(), iu.id2chrom(last_scope_interval2d.chromid1()).c_str(), iu.id2chrom(last_scope_interval2d.chromid2()).c_str());
							}
							out_intervals2d.push_back(scanner.last_interval2d());
							size = out_intervals2d.size();
						}

						iu.verify_max_data_size(size, error_prefix, false);

						scanner.next();

						if (is_1d_iterator) {
							if (scanner.isend() || last_scope_interval1d.chromid != scanner.last_scope_interval1d().chromid) {
								SEXP rintervals = build_rintervals_bintransform(&out_intervals1d, NULL, out_values, NULL, _track_exprs, iu);
								GIntervalsBigSet1D::save_chrom(intervset_out.c_str(), &out_intervals1d, rintervals, iu, chromstats1d);
								out_intervals1d.clear();
								out_values.clear();
							}
						} else {
							if (scanner.isend() || !last_scope_interval2d.is_same_chrom(scanner.last_scope_interval2d())) {
								SEXP rintervals = build_rintervals_bintransform(NULL, &out_intervals2d, out_values, NULL, _track_exprs, iu);
								GIntervalsBigSet2D::save_chrom(intervset_out.c_str(), &out_intervals2d, rintervals, iu, chromstats2d);
								out_intervals2d.clear();
								out_values.clear();
							}
						}
					}

					// pack the result into shared memory
					void *ptr = allocate_res(0);

					if (is_1d_iterator) 
						pack_data(ptr, chromstats1d.front(), chromstats1d.size());
					else
						pack_data(ptr, chromstats2d.front(), chromstats2d.size());
				} else { // parent process
					vector<GIntervalsBigSet1D::ChromStat> kid_chromstats1d(chromstats1d.size());
					vector<GIntervalsBigSet2D::ChromStat> kid_chromstats2d(chromstats2d.size());

					for (int i = 0; i < get_num_kids(); ++i) {
						void *ptr = get_kid_res(i);

						if (is_1d_iterator) {
							unpack_data(ptr, kid_chromstats1d.front(), kid_chromstats1d.size());
							for (vector<GIntervalsBigSet1D::ChromStat>::const_iterator istat = kid_chromstats1d.begin(); istat < kid_chromstats1d.end(); ++istat) {
								if (istat->size)
									chromstats1d[istat - kid_chromstats1d.begin()] = *istat;
							}
						} else {
							unpack_data(ptr, kid_chromstats2d.front(), kid_chromstats2d.size());
							for (vector<GIntervalsBigSet2D::ChromStat>::const_iterator istat = kid_chromstats2d.begin(); istat < kid_chromstats2d.end(); ++istat) {
								if (istat->size)
									chromstats2d[istat - kid_chromstats2d.begin()] = *istat;
							}
						}
					}

					// finish saving (write meta)
					if (is_1d_iterator) {
						SEXP zeroline = build_rintervals_bintransform(&out_intervals1d, NULL, out_values, NULL, _track_exprs, iu);
						GIntervalsBigSet1D::end_save(intervset_out.c_str(), zeroline, iu, chromstats1d);
					} else {
						SEXP zeroline = build_rintervals_bintransform(NULL, &out_intervals2d, out_values, NULL, _track_exprs, iu);
						GIntervalsBigSet2D::end_save(intervset_out.c_str(), zeroline, iu, chromstats2d);
					}
				}
				rreturn(R_NilValue);
			}

			if (iu.distribute_task(0,
								   (is_1d_iterator ? sizeof(GInterval) : sizeof(GInterval2D)) + // interval
							       sizeof(unsigned) +                                           // interval id
							       sizeof(double)))                                             // values
			{  // child process
				for (scanner.begin(_track_exprs, iu.get_kid_intervals1d(), iu.get_kid_intervals2d(), _iterator_policy, _band); !scanner.isend(); scanner.next()) {
					if (scanner.get_iterator()->is_1d()) {
						out_intervals1d.push_back(scanner.last_interval1d());
						ids.push_back(iu.get_orig_interv_idx(scanner.last_scope_interval1d()) + 1);
					} else {
						out_intervals2d.push_back(scanner.last_interval2d());
						ids.push_back(iu.get_orig_interv_idx(scanner.last_scope_interval2d()) + 1);
					}

					out_values.push_back(get_bin_tranformed_value(numexpr, force_binning, _lookup_table, scanner, bin_finders, track_mult));
					iu.verify_max_data_size(out_values.size(), "Result");
				}

				uint64_t num_intervals = scanner.get_iterator()->is_1d() ? out_intervals1d.size() : out_intervals2d.size();

				void *result = allocate_res(num_intervals);

				if (num_intervals) {
					if (scanner.get_iterator()->is_1d())
						pack_data(result, out_intervals1d.front(), num_intervals);
					else
						pack_data(result, out_intervals2d.front(), num_intervals);

					pack_data(result, ids.front(), num_intervals);

					pack_data(result, out_values.front(), num_intervals);
				}
				rreturn(R_NilValue);
			}

			// parent process
			// collect results from kids
			for (int i = 0; i < get_num_kids(); ++i) {
				void *ptr = get_kid_res(i);
				uint64_t num_intervals = get_kid_res_size(i);

				if (!num_intervals)
					continue;

				if (is_1d_iterator) {
					out_intervals1d.insert(out_intervals1d.end(), (GInterval *)ptr, (GInterval *)ptr + num_intervals);
					ptr = (GInterval *)ptr + num_intervals;
				} else {
					out_intervals2d.insert(out_intervals2d.end(), (GInterval2D *)ptr, (GInterval2D *)ptr + num_intervals);
					ptr = (GInterval2D *)ptr + num_intervals;
				}

				ids.insert(ids.end(), (unsigned *)ptr, (unsigned *)ptr + num_intervals);
				ptr = (unsigned *)ptr + num_intervals;

				out_values.insert(out_values.end(), (double *)ptr, (double *)ptr + num_intervals);
				ptr = (double *)ptr + num_intervals;
			}
		} else { // no multitasking
			if (!intervset_out.empty()) {
				scanner.begin(_track_exprs, intervals1d, intervals2d, _iterator_policy, _band);

				while (!scanner.isend()) {
					out_values.push_back(get_bin_tranformed_value(numexpr, force_binning, _lookup_table, scanner, bin_finders, track_mult));

					if (is_1d_iterator) {
						if (last_scope_interval1d.chromid != scanner.last_scope_interval1d().chromid) {
							last_scope_interval1d = scanner.last_scope_interval1d();
							snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chrom %s",
									intervset_out.c_str(), iu.id2chrom(last_scope_interval1d.chromid).c_str());
						}
						out_intervals1d.push_back(scanner.last_interval1d());
						size = out_intervals1d.size();
					} else {
						if (!last_scope_interval2d.is_same_chrom(scanner.last_scope_interval2d())) {
							last_scope_interval2d = scanner.last_scope_interval2d();
							snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chroms (%s, %s)",
									intervset_out.c_str(), iu.id2chrom(last_scope_interval2d.chromid1()).c_str(), iu.id2chrom(last_scope_interval2d.chromid2()).c_str());
						}
						out_intervals2d.push_back(scanner.last_interval2d());
						size = out_intervals2d.size();
					}

					iu.verify_max_data_size(size, error_prefix, false);

					scanner.next();

					if (is_1d_iterator) {
						if (scanner.isend() || last_scope_interval1d.chromid != scanner.last_scope_interval1d().chromid) {
							SEXP rintervals = build_rintervals_bintransform(&out_intervals1d, NULL, out_values, NULL, _track_exprs, iu);
							GIntervalsBigSet1D::save_chrom(intervset_out.c_str(), &out_intervals1d, rintervals, iu, chromstats1d);
							out_intervals1d.clear();
							out_values.clear();
						}
					} else {
						if (scanner.isend() || !last_scope_interval2d.is_same_chrom(scanner.last_scope_interval2d())) {
							SEXP rintervals = build_rintervals_bintransform(NULL, &out_intervals2d, out_values, NULL, _track_exprs, iu);
							GIntervalsBigSet2D::save_chrom(intervset_out.c_str(), &out_intervals2d, rintervals, iu, chromstats2d);
							out_intervals2d.clear();
							out_values.clear();
						}
					}
				}

				// finish saving (write meta)
				if (is_1d_iterator) {
					SEXP zeroline = build_rintervals_bintransform(&out_intervals1d, NULL, out_values, NULL, _track_exprs, iu);
					GIntervalsBigSet1D::end_save(intervset_out.c_str(), zeroline, iu, chromstats1d);
				} else {
					SEXP zeroline = build_rintervals_bintransform(NULL, &out_intervals2d, out_values, NULL, _track_exprs, iu);
					GIntervalsBigSet2D::end_save(intervset_out.c_str(), zeroline, iu, chromstats2d);
				}

				return R_NilValue;
			}

			for (scanner.begin(_track_exprs, intervals1d, intervals2d, _iterator_policy, _band); !scanner.isend(); scanner.next()) {
				if (scanner.get_iterator()->is_1d()) {
					out_intervals1d.push_back(scanner.last_interval1d());
					ids.push_back(iu.get_orig_interv_idx(scanner.last_scope_interval1d()) + 1);
				} else {
					out_intervals2d.push_back(scanner.last_interval2d());
					ids.push_back(iu.get_orig_interv_idx(scanner.last_scope_interval2d()) + 1);
				}

				out_values.push_back(get_bin_tranformed_value(numexpr, force_binning, _lookup_table, scanner, bin_finders, track_mult));
				iu.verify_max_data_size(out_values.size(), "Result");
			}
		}

		if (out_intervals1d.empty() && out_intervals2d.empty())
			rreturn(R_NilValue);

		// assemble the answer
		SEXP answer;

		if (!out_intervals1d.empty())
			answer = build_rintervals_bintransform(&out_intervals1d, NULL, out_values, &ids, _track_exprs, iu);
		else
			answer = build_rintervals_bintransform(NULL, &out_intervals2d, out_values, &ids, _track_exprs, iu);

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP gtrack_bintransform(SEXP _track, SEXP _track_exprs, SEXP _breaks, SEXP _include_lowest, SEXP _force_binning, SEXP _lookup_table, SEXP _iterator_policy, SEXP _band, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		// check the arguments
		if (!isString(_track) || length(_track) != 1)
			verror("Track argument is not a string");

		if (!isString(_track_exprs) || length(_track_exprs) < 1)
			verror("Track expression argument is not a string vector");

		unsigned numexpr = length(_track_exprs);

		if (!isVector(_breaks))
			verror("Breaks argument must be a vector");

		if (length(_breaks) != (int)numexpr)
			verror("Number of breaks sets must be equal to the number of tracks used");

		if (!isLogical(_include_lowest) || length(_include_lowest) != 1)
			verror("include.lowest argument is not logical");

		if (!isLogical(_force_binning) || length(_force_binning) != 1)
			verror("include.lowest argument is not logical");

		if (!isNumeric(_lookup_table))
			verror("Lookup table argument must be numeric");

		bool include_lowest = LOGICAL(_include_lowest)[0];
		bool force_binning = LOGICAL(_force_binning)[0];
		vector<BinFinder> bin_finders;
		vector<unsigned> track_mult(numexpr);
		unsigned totalbins = 1;

		bin_finders.reserve(numexpr);
		for (unsigned i = 0; i < numexpr; ++i) {
			SEXP breaks = VECTOR_ELT(_breaks, i);

			if (!isReal(breaks))
				verror("Breaks[%d] is not numeric", i);

			bin_finders.push_back(BinFinder());
			bin_finders.back().init(REAL(breaks), length(breaks), include_lowest);

			totalbins *= bin_finders.back().get_numbins();
			track_mult[i] = !i ? 1 : track_mult[i - 1] * bin_finders[i - 1].get_numbins();
		}

		if ((int)totalbins != length(_lookup_table))
			verror("Lookup table length (%d) must match the range of the bins (%d)", length(_lookup_table), totalbins);

		const char *track = CHAR(STRING_ELT(_track, 0));
		string dirname = create_track_dir(_envir, track);

		int cur_chromid = -1;
		char filename[FILENAME_MAX];
		IntervUtils iu(_envir);
		TrackExprScanner scanner(iu);
		GIntervals all_genome_intervs1d;
		GIntervals2D all_genome_intervs2d;
		iu.get_all_genome_intervs(all_genome_intervs1d);
		iu.get_all_genome_intervs(all_genome_intervs2d);

		GenomeTrackFixedBin fixed_bin_track;
		GenomeTrackSparse sparse_track;
		set<int> created_chromids;

		try {
			scanner.begin(_track_exprs, &all_genome_intervs1d, &all_genome_intervs2d, _iterator_policy, _band);
			TrackExpressionIteratorBase::Type itr_type = scanner.get_iterator()->get_type();

			if (scanner.get_iterator()->is_1d()) {
				for (; !scanner.isend(); scanner.next()) {
					if (cur_chromid != scanner.last_interval1d().chromid) {
						cur_chromid = scanner.last_interval1d().chromid;
						created_chromids.insert(cur_chromid);
						snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), iu.id2chrom(cur_chromid).c_str());

						if (itr_type == TrackExpressionIteratorBase::FIXED_BIN)
							fixed_bin_track.init_write(filename, ((TrackExpressionFixedBinIterator *)scanner.get_iterator())->get_bin_size(), cur_chromid);
						else if (itr_type == TrackExpressionIteratorBase::INTERVALS1D)
							sparse_track.init_write(filename, cur_chromid);
					}

					double value = get_bin_tranformed_value(numexpr, force_binning, _lookup_table, scanner, bin_finders, track_mult);

					if (itr_type == TrackExpressionIteratorBase::FIXED_BIN)
						fixed_bin_track.write_next_bin(value);
					else
						sparse_track.write_next_interval(scanner.last_interval1d(), value);
				}

				if (itr_type == TrackExpressionIteratorBase::INTERVALS1D) {
					// some of the chromosome could be previously skipped; we still must create them even if they are empty
					for (GIntervals::const_iterator iinterv = all_genome_intervs1d.begin(); iinterv != all_genome_intervs1d.end(); ++iinterv) {
						if (created_chromids.find(iinterv->chromid) == created_chromids.end()) {
							snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), iu.id2chrom(iinterv->chromid).c_str());
							sparse_track.init_write(filename, iinterv->chromid);
						}
					}
				}
			} else if (itr_type == TrackExpressionIteratorBase::INTERVALS2D) {
				int cur_chromid1 = -1;
				int cur_chromid2 = -1;
				GenomeTrackRectsRects gtrack(iu.get_track_chunk_size(), iu.get_track_num_chunks());
				RectsQuadTree qtree;

				for (; !scanner.isend(); scanner.next()) {
					const GInterval2D &interv = scanner.last_interval2d();

					if (cur_chromid1 != interv.chromid1() || cur_chromid2 != interv.chromid2()) {
						if (gtrack.opened())
							gtrack.write(qtree);

						cur_chromid1 = interv.chromid1();
						cur_chromid2 = interv.chromid2();
						snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_2d_filename(iu.get_chromkey(), cur_chromid1, cur_chromid2).c_str());

						qtree.reset(0, 0, iu.get_chromkey().get_chrom_size(cur_chromid1), iu.get_chromkey().get_chrom_size(cur_chromid2));
						gtrack.init_write(filename, cur_chromid1, cur_chromid2);
					}

					double value = get_bin_tranformed_value(numexpr, force_binning, _lookup_table, scanner, bin_finders, track_mult);
					qtree.insert(RectsQuadTree::ValueType(interv, value));
				}

				if (gtrack.opened())
					gtrack.write(qtree);
			} else {
				size_t numTypeNames = sizeof(TrackExpressionIteratorBase::TYPE_NAMES) / sizeof(TrackExpressionIteratorBase::TYPE_NAMES[0]);
				if (itr_type >= 0 && itr_type < numTypeNames) {
					verror("Iterator type %s is not supported by the function", TrackExpressionIteratorBase::TYPE_NAMES[itr_type]);
				} else {
					verror("Invalid iterator type encountered");
				}
			}
				
		} catch (TGLException &e) {
			verror("Error writing %s: %s", filename, e.msg());
		}
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

}
