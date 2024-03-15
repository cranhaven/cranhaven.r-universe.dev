#ifndef TRACKEXPRESSIONSCANNER_H_
#define TRACKEXPRESSIONSCANNER_H_

#include <cstdint>
#include <ctype.h>

#include <vector>
#include <string>

#include "GenomeTrack.h"
#include "GenomeTrackFixedBin.h"
#include "TrackExpressionVars.h"
#include "rdbinterval.h"
#include "rdbutils.h"

#include "TrackExpressionIterator.h"

using namespace std;
using namespace rdb;

class TrackExprScanner {
public:
	TrackExprScanner(rdb::IntervUtils &iu);
	~TrackExprScanner();

	const vector<string> &get_track_exprs() const { return m_track_exprs; }
	const vector<string> &get_chroms() const { return m_chroms; }

	void check(string &track_expr, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, SEXP iterator_policy, SEXP band = R_NilValue);
	void check(const vector<string> &track_exprs, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, SEXP iterator_policy, SEXP band = R_NilValue);
	void check(SEXP track_exprs, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, SEXP iterator_policy, SEXP band = R_NilValue);
	bool begin(string &track_expr, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, SEXP iterator_policy, SEXP band = R_NilValue);
	bool begin(const vector<string> &track_exprs, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, SEXP iterator_policy, SEXP band = R_NilValue);
	bool begin(SEXP track_exprs, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, SEXP iterator_policy, SEXP band = R_NilValue);
	bool next();
	bool isend() { return m_isend; }

	void report_progress(bool do_report_progress) { m_do_report_progress = do_report_progress; }

	// returns the result of last evaluation of track expression as REAL
	double last_real(int track_expr_idx) const;

	// returns the result of last evaluation of track expression as LOGICAL
	// 1 = true, 0 = false, -1 = NA
	int    last_logical(int track_expr_idx) const;

	const GInterval   &last_interval1d() const { return m_1d.expr_itr_intervals[m_eval_buf_idx]; }
	const GInterval2D &last_interval2d() const { return m_2d.expr_itr_intervals[m_eval_buf_idx]; }

	const GInterval   &last_scope_interval1d() const { return m_1d.expr_itr_scope_intervals[m_eval_buf_idx]; }
	const GInterval2D &last_scope_interval2d() const { return m_2d.expr_itr_scope_intervals[m_eval_buf_idx]; }

	int64_t last_scope_idx() const { return m_expr_itr_scope_idx[m_eval_buf_idx]; }

	int64_t last_scope_chrom_idx() const { return m_expr_itr_scope_chrom_idx[m_eval_buf_idx]; }

	const TrackExpressionIteratorBase *get_iterator() { return m_expr_itr; }

	// Gets the tracks names that appear in the track expression, returns a TrackExpressionIterator after begin is being called.
	// In addition returns the intervals that are used by the iterator (if the iterator is of INTERVALS type).
	TrackExpressionIteratorBase *create_expr_iterator(SEXP rtrack_exprs, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d,
													  SEXP iterator_policy, SEXP band, bool call_begin = true);

private:
	rdb::IntervUtils &m_iu;

	vector<string>   m_track_exprs;	

	vector<SEXP>     m_eval_exprs;
	vector<SEXP>     m_eval_bufs;
	vector<double *> m_eval_doubles;
	vector<int *>    m_eval_ints;
	unsigned         m_eval_buf_idx;
	unsigned         m_eval_buf_limit;
	unsigned         m_eval_buf_size;

	vector<string>  m_chroms;
	int             m_last_progress_reported;
	uint64_t          m_num_evals;
	int             m_report_step;
	uint64_t        m_last_report_clock;
	SEXP            m_rexpr_itr_intervals;
	vector<int64_t> m_expr_itr_scope_idx;
	vector<int64_t> m_expr_itr_scope_chrom_idx;
	bool            m_isend;
	bool            m_do_report_progress;

	struct {
		GIntervals   intervals;  // iterator intervals
		int          cur_chromid;
		GIntervals   expr_itr_intervals;
		GIntervals   expr_itr_scope_intervals;
		int          *expr_itr_intervals_chroms;
		double       *expr_itr_intervals_starts;
		double       *expr_itr_intervals_ends;
	} m_1d;

	struct {
		GIntervals2D  intervals;  // iterator intervals
		int           cur_chromid1;
		int           cur_chromid2;
		GIntervals2D  expr_itr_intervals;
		GIntervals2D  expr_itr_scope_intervals;
		int          *expr_itr_intervals_chroms1;
		double       *expr_itr_intervals_starts1;
		double       *expr_itr_intervals_ends1;
		int          *expr_itr_intervals_chroms2;
		double       *expr_itr_intervals_starts2;
		double       *expr_itr_intervals_ends2;
	} m_2d;

	DiagonalBand                 m_band;
	TrackExpressionIteratorBase *m_expr_itr;
	TrackExpressionVars          m_expr_vars;

	static const int INIT_REPORT_STEP;
	static const int REPORT_INTERVAL; // report interval in milliseconds
	static const int MIN_REPORT_INTERVAL; // report interval in milliseconds

	void convert_rtrack_exprs(SEXP rtrack_exprs, vector<string> &track_exprs);
	void define_r_vars(unsigned eval_buf_limit);

	bool eval_next();
	void report_progress();

	void verify_1d_iter(GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d) const;
	void verify_2d_iter(GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d) const;

	TrackExpressionIteratorBase *create_expr_iterator(SEXP giterator, const TrackExpressionVars &vars, const vector<string> &track_exprs,
													  GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d,
													  GIntervals &intervals1d, GIntervals2D &intervals2d, const DiagonalBand &band, bool call_begin = true);
};


//------------------------------ IMPLEMENTATION ----------------------------------------

inline double TrackExprScanner::last_real(int idx) const
{
    if (m_eval_exprs[idx] != R_NilValue && !isReal(m_eval_bufs[idx])) {
        if (RdbInitializer::is_kid()) 
            verror("Expression \"%s\" does not produce a numeric result.", m_track_exprs[idx].c_str());
        else {
            defineVar(install("GERROR_EXPR"), m_eval_exprs[idx], findVar(install(".misha"), m_iu.get_env()));
            verror("Expression \"%s\" does not produce a numeric result.\n"
                    "The result of the last expression evaluation was saved in GERROR_EXPR variable.", m_track_exprs[idx].c_str());
        }
    }
	return m_eval_doubles[idx][m_eval_buf_idx];
}

inline int TrackExprScanner::last_logical(int idx) const
{
    if (m_eval_exprs[idx] == R_NilValue)
        verror("Expression \"%s\" does not produce a logical result", m_track_exprs[idx].c_str());

    if (!isLogical(m_eval_bufs[idx])) {
        if (RdbInitializer::is_kid()) 
            verror("Expression \"%s\" does not produce a logical result\n", m_track_exprs[idx].c_str());
        else {
            defineVar(install("GERROR_EXPR"), m_eval_exprs[idx], findVar(install(".misha"), m_iu.get_env()));
            verror("Expression \"%s\" does not produce a logical result\n"
                    "The result of the last expression evaluation was saved in GERROR_EXPR variable.", m_track_exprs[idx].c_str());
        }
    }
    return m_eval_ints[idx][m_eval_buf_idx];
}

#endif /* TRACKEXPRESSIONSCANNER_H_ */
