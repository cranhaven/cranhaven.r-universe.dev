/*
 * rdbinterval.h
 *
 *  Created on: Jun 3, 2010
 *      Author: hoichman
 */

#ifndef RDBINTERVAL_H_
#define RDBINTERVAL_H_

#include <cstdint>
#include <inttypes.h>
#include <sys/types.h>

#include <vector>

using namespace std;

#include <list>
#include <memory>
#include <set>

#include "DiagonalBand.h"
#include "GenomeChromKey.h"
#include "GIntervals.h"
#include "GIntervals2D.h"
#include "TrackExpressionIterator.h"

#include <R.h>
#include <Rinternals.h>

#include "GenomeTrack.h"

namespace rdb {

//------------------------------------- IntervalPval ----------------------------------------------

struct IntervalPval : public GInterval {
	enum { PVAL = GInterval::NUM_COLS, NUM_COLS };

	static const char *COL_NAMES[NUM_COLS];

	double     minpval;

	IntervalPval(int _chromid, int64_t _start, int64_t _end, char _strand, double _minpval) :
		GInterval(_chromid, _start, _end, _strand), minpval(_minpval) {}
};


//------------------------------------- ChainInterval ---------------------------------------------

struct ChainInterval : public GInterval {
	enum Errors { BAD_INTERVAL };
	enum { CHROM_SRC = GInterval::NUM_COLS, START_SRC, NUM_COLS };

	struct SrcCompare {
		bool operator()(const ChainInterval &obj1, const ChainInterval &obj2) const;
	};	

	struct SetCompare {
		bool operator()(const ChainInterval &obj1, const ChainInterval &obj2) const;
	};

	bool operator<(const ChainInterval &interv) const {
		return chromid < interv.chromid || (chromid == interv.chromid && start < interv.start) || (chromid == interv.chromid && start == interv.start && end < interv.end);
	}

	static const char *COL_NAMES[NUM_COLS];

	int64_t start_src;
	int     chromid_src;

	ChainInterval() : GInterval(), start_src(-1), chromid_src(-1) {}

	ChainInterval(int _chromid, int64_t _start, int64_t _end, int _chromid_src, int64_t _start_src) :
		GInterval(_chromid, _start, _end, 0), start_src(_start_src), chromid_src(_chromid_src) {}

	string tostring(const GenomeChromKey &chromkey, const vector<string> &src_id2chrom) const;

	void verify(const GenomeChromKey &chromkey, const vector<string> &src_id2chrom, bool check_chrom_boundary = true) const;

	// returns true if source interval overlaps the given interval
	bool do_overlap_src(const GInterval &interv) const {
		return chromid_src == interv.chromid && max(start_src, interv.start) < min(start_src + end - start, interv.end);
	}
};


//------------------------------------- ChainIntervals --------------------------------------------

class ChainIntervals : public std::vector<ChainInterval> {
public:
	enum Errors { OVERLAPPING_INTERVAL, UNSORTED_INTERVALS };

	ChainIntervals() : std::vector<ChainInterval>() {}
	ChainIntervals(size_type n) : std::vector<ChainInterval>(n) {}
	ChainIntervals(const std::vector<ChainInterval> &v) : std::vector<ChainInterval>(v) {}

	void sort_by_src() { sort(begin(), end(), ChainInterval::SrcCompare()); }
	void sort_by_tgt() { sort(begin(), end()); }

	// Verifies that there are no overlaps between the source intervals; if intervals overlap an exception is thrown.
	// Intervals are expected to be already sorted by source.
	void verify_no_src_overlaps(const GenomeChromKey &chromkey, const vector<string> &src_id2chrom) const;

	// Verifies that there are no overlaps between the target intervals; if intervals overlap an exception is thrown.
	// Intervals are expected to be already sorted by target.
	void verify_no_tgt_overlaps(const GenomeChromKey &chromkey, const vector<string> &src_id2chrom) const;

	const_iterator map_interval(const GInterval &src_interval, GIntervals &tgt_intervs, const_iterator hint);

private:
	bool check_first_overlap_src(const const_iterator &iinterval1, const GInterval &interval2) {
		return iinterval1->do_overlap_src(interval2) && (iinterval1 == begin() || !(iinterval1 - 1)->do_overlap_src(interval2));
	}

	const_iterator add2tgt(const_iterator hint, const GInterval &src_interval, GIntervals &tgt_intervs);
};


//------------------------------------- IntervUtils -----------------------------------------------

class IntervUtils {
public:
	enum TrackType { TRACK1D, TRACK2D };
	enum IntervsType { INTERVS1D = 0x1, INTERVS2D = 0x2 };

	IntervUtils(SEXP envir);
	~IntervUtils();

	SEXP get_env() const { return m_envir; }

	// Get all the intervals that cover the whole genome
	void get_all_genome_intervs(GIntervals &intervals) const;

	// Get all 2d intervals that cover the whole genome
	void get_all_genome_intervs(GIntervals2D &intervals) const;

	GIntervalsFetcher1D *get_kid_intervals1d();

	GIntervalsFetcher2D *get_kid_intervals2d();

	unsigned get_rintervs_type_mask(SEXP rintervals, const char *error_msg_prefix = "") const;

	// Converts R intervals (data frame with 4 columns or a string marking the filename) to a vector of Intervals.
	// Returns R intervals in the form of data frame.
	SEXP convert_rintervs(SEXP rintervals, GIntervals *intervals, GIntervals2D *intervals2d, bool null_if_interv_nonexist = false,
						  const GenomeChromKey *chromkey = NULL, const char *error_msg_prefix = "", unsigned *pintervs_type_mask = NULL, bool verify = true) const;

	// Returns intervals type mask
	unsigned convert_rintervs(SEXP rintervals, GIntervalsFetcher1D **intervals, GIntervalsFetcher2D **intervals2d, bool null_if_interv_nonexist = false,
							  const GenomeChromKey *chromkey = NULL, const char *error_msg_prefix = "", bool verify = true) const;

	// Converts a vector of Intervals to R (data frame with 3 columns: chrom, start, end)
	SEXP convert_intervs(GIntervalsFetcher1D *intervals, unsigned num_cols = GInterval::NUM_COLS, bool null_if_empty = true, bool use_original_index = false) const;

	// Converts a vector of Intervals to R (data frame with 6 columns: chrom1, start1, end1, chrom2, start2, end2)
	SEXP convert_intervs(GIntervalsFetcher2D *intervals, unsigned num_cols = GInterval2D::NUM_COLS, bool null_if_empty = true, bool use_original_index = false) const;

	// Converts R chain intervals to a vector of ChainIntervals
	void convert_rchain_intervs(SEXP chain, ChainIntervals &chain_intervs, vector<string> &src_id2chrom);

	// Converts a vector of ChainIntervals to R
	SEXP convert_chain_intervs(const ChainIntervals &chain_intervs, vector<string> &src_id2chrom);

	DiagonalBand convert_band(SEXP rband);

	// Creates a data frame with given number or rows and columns. The data frame returned is still half baked.
	// Column names and the columns themselves must be defined later manually or via define_data_frame_cols.
	// If attrs_src is not R_NilValue, the attributes of the new data frame are copied from it.
	SEXP create_data_frame(int numrows, int numcols, SEXP attrs_src = R_NilValue);

	// Copies columns definitions from src (must be a data frame) to tgt starting from column 'tgt_col_offset'.
	// tgt must be created by create_data_frame().
	// No column values are copied though. This function creates only the vectors of columns and copies column names.
	void define_data_frame_cols(SEXP src, vector<SEXP> &src_cols, SEXP tgt, vector<SEXP> &tgt_cols, int tgt_col_offset);

	// Copies a row (values, not the definition) from src data frame to tgt data frame.
	// Before calling this function src columns must be defined in tgt by calling define_data_frame_cols().
	void copy_data_frame_row(const vector<SEXP> &src_cols, int src_row, const vector<SEXP> &tgt_cols, int tgt_row, int tgt_col_offset);

	void copy_data_frame_rows(const vector<SEXP> &src_cols, int src_row, int num_rows, const vector<SEXP> &tgt_cols, int tgt_row, int tgt_col_offset);

	// Sets NAN at the given row and column of a data frame
	void set_data_frame_val_nan(const vector<SEXP> &tgt_cols, int tgt_row, int tgt_col);

	// Verifies that the number of bins in each interval does not exceed the limit
	void restrict_bins(int64_t maxbins, GIntervals &intervals, unsigned binsize) const;

	// Returns true if multitasking is switched on
	bool get_multitasking() const;

	// Returns absolute maximal number of concurrently opened processes for parallel computation
	uint64_t get_max_processes() const;

	// Returns the maximal number of concurrently opened processes per core for parallel computation
	uint64_t get_max_processes2core() const;

	// Returns minimal scope range per process for parallel computation
	uint64_t get_min_scope4process() const;

	// Returns the upper limit for data size
	uint64_t get_max_data_size() const;

	// Returns the upper limit for memory usage
	uint64_t get_max_mem_usage() const;

	// Returns the threshold for creating a big intervals set
	uint64_t get_big_intervals_size() const;

	// Returns the size of the buffer used to store highest/lowest values for high-precision computation of quantiles
	uint64_t get_quantile_edge_data_size() const;

	// Returns the chunk size of 2D track
	uint64_t get_track_chunk_size() const;

	// Returns the chunk size of 2D track
	uint64_t get_track_num_chunks() const;

	// Returns true if iterator is 1D
	bool is_1d_iterator(SEXP rtrack_expr, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, SEXP riterator);

	// Verifies that the data size does not exceed the maximum allowed.
	// If check_all_kids == true then the limit is checked cumulatively for all child processes.
	void verify_max_data_size(uint64_t data_size, const char *data_name = "Result", bool check_all_kids = true);

	// returns true if the intervals set should be saved in a big set
	bool needs_bigset(uint64_t num_intervs) { return num_intervs > get_big_intervals_size(); }

	uint64_t get_orig_interv_idx(const GInterval &interval) const { return (uint64_t)interval.udata; }
	uint64_t get_orig_interv_idx(const GInterval2D &interval) const { return (uint64_t)interval.udata(); }

	int chrom2id(const string &chrom) const { return m_chrom_key.chrom2id(chrom); }
	const string &id2chrom(int id) const { return m_chrom_key.id2chrom(id); }

	const GenomeChromKey &get_chromkey() const { return m_chrom_key; }

	bool track_exists(const char *track_name);

	// returns the number of parallel processes that would be open by distribute_task or 0 if scope is empty
	int prepare4multitasking(SEXP track_exprs, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, SEXP iterator_policy, SEXP band = R_NilValue);

	// same as the previous function but designated for cases when there's no track expression
	int prepare4multitasking(GIntervalsFetcher1D *scope1D, GIntervalsFetcher2D *scope2d);

	// multi-tasking: divides intervals and forks child processes each with its own kid_intervals.
	// Returns true if a child, false if a parent.
	bool distribute_task(uint64_t res_const_size,    // data size in bytes for all the result
						 uint64_t res_record_size);  // size in bytes per datum in the result

private:
	GenomeChromKey                m_chrom_key;
	SEXP                          m_envir;
	SEXP                          m_allgenome;
	int                           m_num_planned_kids;
	vector<GIntervalsFetcher1D *> m_kids_intervals1d;
	vector<GIntervalsFetcher2D *> m_kids_intervals2d;
	GIntervalsFetcher1D          *m_kid_intervals1d;
	GIntervalsFetcher2D          *m_kid_intervals2d;
	mutable int                   m_multitasking;
	mutable uint64_t              m_max_data_size{0};
	mutable uint64_t              m_max_mem_usage{0};
	mutable uint64_t              m_big_intervals_size{0};
	mutable uint64_t              m_max_processes{0};
	mutable uint64_t              m_max_processes2core{0};
	mutable uint64_t              m_min_scope4process{0};
	mutable uint64_t              m_quantile_edge_data_size{0};
	mutable uint64_t              m_track_chunk_size{0};
	mutable uint64_t              m_track_num_chunks{0};

	SEXP get_rallgenome1d() const { return VECTOR_ELT(m_allgenome, 0); }
	SEXP get_rallgenome2d() const { return VECTOR_ELT(m_allgenome, 1); }
};

}


//-------------------------------------- IMPLEMENTATION --------------------------------------------------

inline bool rdb::ChainInterval::SrcCompare::operator()(const ChainInterval &obj1, const ChainInterval &obj2) const
{
	return obj1.chromid_src < obj2.chromid_src || (obj1.chromid_src == obj2.chromid_src && obj1.start_src < obj2.start_src);
}

inline bool rdb::ChainInterval::SetCompare::operator()(const ChainInterval &obj1, const ChainInterval &obj2) const
{
	return obj1.chromid < obj2.chromid || (obj1.chromid == obj2.chromid && obj1.start < obj2.start) ||
		(obj1.chromid == obj2.chromid && obj1.start == obj2.start && obj1.end < obj2.end);
}

inline string rdb::ChainInterval::tostring(const GenomeChromKey &chromkey, const vector<string> &src_id2chrom) const
{
    char buf[1000];
    snprintf(buf, sizeof(buf), "(%s, %" PRId64 ", %" PRId64 ") <- (%s, %" PRId64 ")", chromkey.id2chrom(chromid).c_str(), start, end, src_id2chrom[chromid_src].c_str(), start_src);
    return string(buf);
}


inline void rdb::ChainInterval::verify(const GenomeChromKey &chromkey, const vector<string> &src_id2chrom, bool check_chrom_boundary) const
{
	if (start < 0)
		TGLError<ChainInterval>(BAD_INTERVAL, "Chain interval %s: start coordinate must be greater or equal than zero", tostring(chromkey, src_id2chrom).c_str());
	if (start >= end)
		TGLError<ChainInterval>(BAD_INTERVAL, "Interval %s: start coordinate must be lesser than end coordinate", tostring(chromkey, src_id2chrom).c_str());
	if (check_chrom_boundary && (uint64_t)end > chromkey.get_chrom_size(chromid))
		TGLError<ChainInterval>(BAD_INTERVAL, "Interval %s: end coordinate exceeds chromosome boundaries", tostring(chromkey, src_id2chrom).c_str());
	if (start_src < 0)
		TGLError<ChainInterval>(BAD_INTERVAL, "Chain interval %s: source start coordinate must be greater or equal than zero", tostring(chromkey, src_id2chrom).c_str());
}

#endif /* GENOMETRACKTYPES_H_ */
