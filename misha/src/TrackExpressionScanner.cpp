/*
 * TrackExpressionScanner.cpp
 *
 *  Created on: Apr 28, 2010
 *      Author: hoichman
 */

#include <cstdint>
#include <errno.h>
#include <sys/time.h>

#include <set>
#include <unordered_set>

#include "port.h"

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>

#include "HashFunc.h"

#include "GenomeTrack.h"
#include "GenomeTrackFixedBin.h"
#include "TrackExpressionBigSet1DIterator.h"
#include "TrackExpressionBigSet2DIterator.h"
#include "TrackExpressionCartesianGridIterator.h"
#include "TrackExpressionFixedBinIterator.h"
#include "TrackExpressionFixedRectIterator.h"
#include "TrackExpressionIntervals1DIterator.h"
#include "TrackExpressionIntervals2DIterator.h"
#include "TrackExpressionIterator.h"
#include "TrackExpressionSparseIterator.h"
#include "TrackExpressionScanner.h"
#include "TrackExpressionTrackRectsIterator.h"
#include "TrackExpressionVars.h"
#include "rdbutils.h"

const int TrackExprScanner::INIT_REPORT_STEP = 10000;
const int TrackExprScanner::REPORT_INTERVAL = 3000;
const int TrackExprScanner::MIN_REPORT_INTERVAL = 1000;

using namespace rdb;

static uint64_t get_cur_clock()
{
	struct timeval tv;
	gettimeofday(&tv, NULL);
	return tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

TrackExprScanner::TrackExprScanner(rdb::IntervUtils &iu) :
	m_iu(iu),	
	m_isend(true),
	m_expr_itr(NULL),
	m_expr_vars(iu)
{
	m_1d.cur_chromid = -1;
	m_2d.cur_chromid1 = -1;
	m_2d.cur_chromid2 = -1;
	m_do_report_progress = true;
}

TrackExprScanner::~TrackExprScanner()
{
	delete m_expr_itr;
}

void TrackExprScanner::convert_rtrack_exprs(SEXP rtrack_exprs, vector<string> &track_exprs)
{
	track_exprs.clear();

	if (!isString(rtrack_exprs) || length(rtrack_exprs) < 1)
		verror("Tracks expressions argument must be a vector of strings");

	unsigned num_track_exprs = (unsigned)length(rtrack_exprs);
	track_exprs.resize(num_track_exprs);

	for (unsigned iexpr = 0; iexpr < num_track_exprs; ++iexpr)
		track_exprs[iexpr] = CHAR(STRING_ELT(rtrack_exprs, iexpr));
}

void TrackExprScanner::define_r_vars(unsigned eval_buf_limit)
{
	m_eval_buf_limit = eval_buf_limit;
	m_expr_vars.define_r_vars(eval_buf_limit);

	if (m_expr_itr->is_1d()) {
		m_1d.cur_chromid = -1;
		m_1d.expr_itr_intervals.resize(m_eval_buf_limit);
		m_1d.expr_itr_scope_intervals.resize(m_eval_buf_limit);
		m_rexpr_itr_intervals = m_iu.convert_intervs(&m_1d.expr_itr_intervals);
		m_1d.expr_itr_intervals_chroms = INTEGER(VECTOR_ELT(m_rexpr_itr_intervals, GInterval::CHROM));
		m_1d.expr_itr_intervals_starts = REAL(VECTOR_ELT(m_rexpr_itr_intervals, GInterval::START));
		m_1d.expr_itr_intervals_ends = REAL(VECTOR_ELT(m_rexpr_itr_intervals, GInterval::END));
		for (unsigned i = 0; i < m_eval_buf_limit; i++)
			m_1d.expr_itr_intervals_chroms[i] = 1;
	} else {
		m_2d.cur_chromid1 = -1;
		m_2d.cur_chromid2 = -1;
		m_2d.expr_itr_intervals.resize(m_eval_buf_limit);
		m_2d.expr_itr_scope_intervals.resize(m_eval_buf_limit);
		m_rexpr_itr_intervals = m_iu.convert_intervs(&m_2d.expr_itr_intervals);
		m_2d.expr_itr_intervals_chroms1 = INTEGER(VECTOR_ELT(m_rexpr_itr_intervals, GInterval2D::CHROM1));
		m_2d.expr_itr_intervals_starts1 = REAL(VECTOR_ELT(m_rexpr_itr_intervals, GInterval2D::START1));
		m_2d.expr_itr_intervals_ends1 = REAL(VECTOR_ELT(m_rexpr_itr_intervals, GInterval2D::END1));
		m_2d.expr_itr_intervals_chroms2 = INTEGER(VECTOR_ELT(m_rexpr_itr_intervals, GInterval2D::CHROM2));
		m_2d.expr_itr_intervals_starts2 = REAL(VECTOR_ELT(m_rexpr_itr_intervals, GInterval2D::START2));
		m_2d.expr_itr_intervals_ends2 = REAL(VECTOR_ELT(m_rexpr_itr_intervals, GInterval2D::END2));
		for (unsigned i = 0; i < m_eval_buf_limit; i++)
			m_2d.expr_itr_intervals_chroms1[i] = m_2d.expr_itr_intervals_chroms2[i] = 1;
	}
	defineVar(install("GITERATOR.INTERVALS"), m_rexpr_itr_intervals, findVar(install(".misha"), m_iu.get_env()));

    for (unsigned iexpr = 0; iexpr < m_track_exprs.size(); ++iexpr) {
        const TrackExpressionVars::Track_var *var = m_expr_vars.var(m_track_exprs[iexpr].c_str());
        if (var)  // track expression is a virtual track
            m_eval_doubles[iexpr] = REAL(var->rvar);
    }
}

void TrackExprScanner::check(string &track_expr, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, SEXP iterator_policy, SEXP band)
{
	check(vector<string>(1, track_expr), scope1d, scope2d, iterator_policy, band);
}

void TrackExprScanner::check(SEXP track_exprs, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, SEXP iterator_policy, SEXP band)
{
	vector<string> track_expr_strs;
	convert_rtrack_exprs(track_exprs, track_expr_strs);
	check(track_expr_strs, scope1d, scope2d, iterator_policy, band);
}

void TrackExprScanner::check(const vector<string> &track_exprs, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, SEXP iterator_policy, SEXP band)
{
	m_band = m_iu.convert_band(band);

    runprotect(m_eval_bufs);
    runprotect(m_eval_exprs);

    m_track_exprs.reserve(track_exprs.size());
    for (vector<string>::const_iterator iexpr = track_exprs.begin(); iexpr != track_exprs.end(); ++iexpr) {
        // trim spaces from the track expression
        string::const_iterator istr_start, istr_end;

        for (istr_start = iexpr->begin(); istr_start < iexpr->end(); ++istr_start) {
            if (!isspace(*istr_start))
                break;
        }

        for (istr_end = iexpr->end() - 1; istr_end >= iexpr->begin(); --istr_end) {
            if (!isspace(*istr_end))
                break;
        }

        m_track_exprs.push_back(iexpr->substr(istr_start - iexpr->begin(), istr_end - istr_start + 1));
    }

    m_eval_exprs.resize(m_track_exprs.size(), R_NilValue);
    m_eval_bufs.resize(m_track_exprs.size(), R_NilValue);
    m_eval_doubles.resize(m_track_exprs.size(), NULL);
    m_eval_ints.resize(m_track_exprs.size(), NULL);

	m_expr_vars.parse_exprs(m_track_exprs);

	// initiate the expression iterator
	delete m_expr_itr;
	m_expr_itr = create_expr_iterator(iterator_policy, m_expr_vars, m_track_exprs, scope1d, scope2d, m_1d.intervals, m_2d.intervals, m_band);
	m_expr_vars.init(*m_expr_itr);

	for (unsigned iexpr = 0; iexpr < m_track_exprs.size(); ++iexpr) {
        if (!m_expr_vars.var(m_track_exprs[iexpr].c_str())) {   // track expression is not a virtual track
    		SEXP expr = R_NilValue;
			SEXPCleaner expr_cleaner(expr);

    		rprotect(expr = RSaneAllocVector(STRSXP, 1));
    		SET_STRING_ELT(expr, 0, mkChar(m_track_exprs[iexpr].c_str()));

    		// parse R expression
    		ParseStatus status;
    		SEXP parsed_expr;
    		rprotect(parsed_expr = R_ParseVector(expr, -1, &status, R_NilValue));
    		if (status != PARSE_OK)
    			verror("R parsing of expression \"%s\" failed", m_track_exprs[iexpr].c_str());
    		m_eval_exprs[iexpr] = VECTOR_ELT(parsed_expr, 0);
        }
	}
}

bool TrackExprScanner::begin(string &track_expr, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, SEXP iterator_policy, SEXP band)
{
	return begin(vector<string>(1, track_expr), scope1d, scope2d, iterator_policy, band);
}

bool TrackExprScanner::begin(SEXP track_exprs, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, SEXP iterator_policy, SEXP band)
{
	vector<string> track_expr_strs;
	convert_rtrack_exprs(track_exprs, track_expr_strs);
	return begin(track_expr_strs, scope1d, scope2d, iterator_policy, band);
}

bool TrackExprScanner::begin(const vector<string> &track_exprs, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, SEXP iterator_policy, SEXP band)
{
	check(track_exprs, scope1d, scope2d, iterator_policy, band);

	// check whether m_eval_buf_limit == 1000 will correctly work for the expression
	SEXP gbufsize = GetOption(install("gbuf.size"), R_NilValue);
	if (!isReal(gbufsize) || REAL(gbufsize)[0] < 1)
		define_r_vars(1000);
	else
		define_r_vars((unsigned)REAL(gbufsize)[0]);

	for (unsigned iexpr = 0; iexpr < m_track_exprs.size(); ++iexpr) {
        if (m_eval_exprs[iexpr] != R_NilValue) {
            SEXP res = eval_in_R(m_eval_exprs[iexpr], m_iu.get_env());

            if (length(res) != (int)m_eval_buf_limit) {
                runprotect(res);
                define_r_vars(1);
                break;
            }
            runprotect(res);
        }
	}

	m_num_evals = 0;
	m_last_progress_reported = -1;
	m_report_step = INIT_REPORT_STEP;
	m_last_report_clock = get_cur_clock();

	m_isend = false;
	m_eval_buf_idx = m_eval_buf_limit;
	m_eval_buf_size = 0;
	m_expr_itr_scope_idx.resize(m_eval_buf_limit, -1);
	m_expr_itr_scope_chrom_idx.resize(m_eval_buf_limit, -1);

	return next();
}

bool TrackExprScanner::next()
{
	monitor_memusage();

	if (isend())
		return false;

	if (eval_next())
		return true;

	// did we start reporting progress?
	if (m_last_progress_reported >= 0) {
		if (m_last_progress_reported != 100)
			REprintf("100%%\n");
		else
			REprintf("\n");
	}
	update_progress(100);

	runprotect(m_eval_bufs);

	return false;
}

bool TrackExprScanner::eval_next()
{
	m_eval_buf_idx++;

	// do we need to read more track samples to the buffer?
	if (m_eval_buf_idx >= m_eval_buf_limit) {
		m_eval_buf_idx = 0;
		if (m_expr_itr->is_1d()) {
			for (m_eval_buf_size = 0; m_eval_buf_size < m_eval_buf_limit; ++m_eval_buf_size) {
                if (m_expr_itr->isend()) {
                    for (unsigned i = m_eval_buf_size; i < m_eval_buf_limit; ++i) {
                        m_1d.expr_itr_intervals_chroms[i] = 1;
                        m_1d.expr_itr_intervals_starts[i] = -1.;
                        m_1d.expr_itr_intervals_ends[i] = -1.;
                    }
                    break;
                }

                const GInterval &interval = ((TrackExpression1DIterator *)m_expr_itr)->last_interval();

                m_1d.expr_itr_intervals[m_eval_buf_size] = interval;
                m_1d.expr_itr_scope_intervals[m_eval_buf_size] = ((TrackExpression1DIterator *)m_expr_itr)->last_scope_interval();
                m_1d.expr_itr_intervals_chroms[m_eval_buf_size] = interval.chromid + 1;
                m_1d.expr_itr_intervals_starts[m_eval_buf_size] = (double)interval.start;
                m_1d.expr_itr_intervals_ends[m_eval_buf_size] = (double)interval.end;
                m_1d.cur_chromid = interval.chromid;

                m_expr_itr_scope_idx[m_eval_buf_size] = ((TrackExpression1DIterator *)m_expr_itr)->get_cur_scope_idx();
                m_expr_itr_scope_chrom_idx[m_eval_buf_size] = ((TrackExpression1DIterator *)m_expr_itr)->get_cur_scope_chrom_idx();
                m_expr_vars.set_vars(interval, m_eval_buf_size);
                m_expr_itr->next();
			}
		}
		else {
			for (m_eval_buf_size = 0; m_eval_buf_size < m_eval_buf_limit; ++m_eval_buf_size) {
				if (m_expr_itr->isend()) {
					for (unsigned i = m_eval_buf_size; i < m_eval_buf_limit; ++i) {
						m_2d.expr_itr_intervals_chroms1[i] = 1;
						m_2d.expr_itr_intervals_starts1[i] = -1.;
						m_2d.expr_itr_intervals_ends1[i] = -1.;
						m_2d.expr_itr_intervals_chroms2[i] = 1;
						m_2d.expr_itr_intervals_starts2[i] = -1.;
						m_2d.expr_itr_intervals_ends2[i] = -1.;
					}
					break;
				}

				const GInterval2D &interval = ((TrackExpression2DIterator *)m_expr_itr)->last_interval();

				m_2d.expr_itr_intervals[m_eval_buf_size] = interval;
				m_2d.expr_itr_scope_intervals[m_eval_buf_size] = ((TrackExpression2DIterator *)m_expr_itr)->last_scope_interval();
				m_2d.expr_itr_intervals_chroms1[m_eval_buf_size] = interval.chromid1() + 1;
				m_2d.expr_itr_intervals_starts1[m_eval_buf_size] = (double)interval.start1();
				m_2d.expr_itr_intervals_ends1[m_eval_buf_size] = (double)interval.end1();
				m_2d.expr_itr_intervals_chroms2[m_eval_buf_size] = interval.chromid2() + 1;
				m_2d.expr_itr_intervals_starts2[m_eval_buf_size] = (double)interval.start2();
				m_2d.expr_itr_intervals_ends2[m_eval_buf_size] = (double)interval.end2();
				m_2d.cur_chromid1 = interval.chromid1();
				m_2d.cur_chromid2 = interval.chromid2();

				m_expr_itr_scope_idx[m_eval_buf_size] = ((TrackExpression2DIterator *)m_expr_itr)->get_cur_scope_idx();
				m_expr_itr_scope_chrom_idx[m_eval_buf_size] = ((TrackExpression2DIterator *)m_expr_itr)->get_cur_scope_chrom_idx();
				m_expr_vars.set_vars(interval, m_band, m_eval_buf_size);
				m_expr_itr->next();
			}
		}

        check_interrupt();

        for (unsigned iexpr = 0; iexpr < m_eval_exprs.size(); ++iexpr) {
            if (m_eval_exprs[iexpr] != R_NilValue) {
                runprotect(m_eval_bufs[iexpr]);
                m_eval_bufs[iexpr] = eval_in_R(m_eval_exprs[iexpr], m_iu.get_env());
                if (length(m_eval_bufs[iexpr]) != (int)m_eval_buf_limit)
                    verror("Evaluation of expression \"%s\" produces a vector of size %d while expecting size %d",
                            m_track_exprs[iexpr].c_str(), length(m_eval_bufs[iexpr]), m_eval_buf_limit);
                if (isReal(m_eval_bufs[iexpr]))
                    m_eval_doubles[iexpr] = REAL(m_eval_bufs[iexpr]);
                else if (isLogical(m_eval_bufs[iexpr]))
                    m_eval_ints[iexpr] = LOGICAL(m_eval_bufs[iexpr]);
                else
                    verror("Evaluation of expression \"%s\" produces a vector of unsupported type %s",
                            m_track_exprs[iexpr].c_str(), type2char(TYPEOF(m_eval_bufs[iexpr])));
            }
        }

		report_progress();
	}

	if (m_eval_buf_idx >= m_eval_buf_size) {
		m_eval_buf_idx = m_eval_buf_limit;
		m_isend = true;
	}

	return !m_isend;
}

void TrackExprScanner::report_progress()
{
	m_num_evals += m_eval_buf_size;
	if (m_num_evals > (uint64_t)m_report_step && m_do_report_progress) {
        uint64_t curclock = get_cur_clock();
        double delta = curclock - m_last_report_clock;

        if (delta)
            m_report_step = (int)(m_report_step * (REPORT_INTERVAL / delta) + .5);
        else
            m_report_step *= 10;

        if (delta > MIN_REPORT_INTERVAL) {
            if (m_last_progress_reported < 0 && m_eval_buf_limit == 1)
                REprintf("Warning: track expression(s) cannot be evaluated as a vector. Run-times might be slow.\n");

            int progress;

            if (m_expr_itr->is_1d())
                progress = !((TrackExpression1DIterator *)m_expr_itr)->get_scope()->size() ? 0 :
                    (int)(100. * (((TrackExpression1DIterator *)m_expr_itr)->get_cur_scope_idx()) / ((TrackExpression1DIterator *)m_expr_itr)->get_scope()->size());
            else
                progress = !((TrackExpression2DIterator *)m_expr_itr)->get_scope()->size() ? 0 :
                    (int)(100. * (((TrackExpression2DIterator *)m_expr_itr)->get_cur_scope_idx()) / ((TrackExpression2DIterator *)m_expr_itr)->get_scope()->size());

            progress = max(progress, m_last_progress_reported);  // in 2D scope idx can sometimes go backward
            if (progress != 100) {
                if (progress != m_last_progress_reported) {
                    REprintf("%d%%...", progress);
                    update_progress(progress);
                } else
                    REprintf(".");
                m_last_progress_reported = progress;
            }
            m_num_evals = 0;
            m_last_report_clock = curclock;
        }
	}
}

void TrackExprScanner::verify_1d_iter(GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d) const
{
	if (!scope1d)
		verror("The function does not support 1D iterators");

	if (scope1d && scope2d && !scope1d->size() && scope2d->size())
		verror("1D iterator is used along with 2D intervals");
}

void TrackExprScanner::verify_2d_iter(GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d) const
{
	if (!scope2d)
		verror("The function does not support 2D iterators");

	if (scope1d && scope2d && scope1d->size() && !scope2d->size())
		verror("2D iterator is used along with 1D intervals");
}

TrackExpressionIteratorBase *TrackExprScanner::create_expr_iterator(SEXP rtrack_exprs, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d,
																	SEXP iterator_policy, SEXP band, bool call_begin)
{
	m_track_exprs.resize(length(rtrack_exprs));
	for (int i = 0; i < length(rtrack_exprs); ++i) 
		m_track_exprs[i] = CHAR(STRING_ELT(rtrack_exprs, i));

	m_band = m_iu.convert_band(band);
	m_expr_vars.parse_exprs(m_track_exprs);

	// initiate the expression iterator
	delete m_expr_itr;
	m_expr_itr = create_expr_iterator(iterator_policy, m_expr_vars, m_track_exprs, scope1d, scope2d, m_1d.intervals, m_2d.intervals, m_band, call_begin);
    return m_expr_itr;
}

TrackExpressionIteratorBase *TrackExprScanner::create_expr_iterator(SEXP giterator, const TrackExpressionVars &vars, const vector<string> &track_exprs,
		GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, GIntervals &intervals1d, GIntervals2D &intervals2d, const DiagonalBand &band, bool call_begin)
{
	monitor_memusage();

	TrackExpressionIteratorBase *expr_itr = NULL;
	SEXP class_name = getAttrib(giterator, R_ClassSymbol);

	if ((isReal(giterator) || isInteger(giterator)) && length(giterator) == 1) {            // giterator == binsize
		verify_1d_iter(scope1d, scope2d);
		expr_itr = new TrackExpressionFixedBinIterator;
		if (call_begin) 
			((TrackExpressionFixedBinIterator *)expr_itr)->begin(isReal(giterator) ? (int64_t)REAL(giterator)[0] : INTEGER(giterator)[0], *scope1d);
	} else if ((isReal(giterator) || isInteger(giterator)) && length(giterator) == 2) {     // giterator == fixed rectangle
		verify_2d_iter(scope1d, scope2d);
		expr_itr = new TrackExpressionFixedRectIterator;
		if (call_begin) {
			if (isReal(giterator))
				((TrackExpressionFixedRectIterator *)expr_itr)->begin((int64_t)REAL(giterator)[0], (int64_t)REAL(giterator)[1], *scope2d, band);
			else
				((TrackExpressionFixedRectIterator *)expr_itr)->begin(INTEGER(giterator)[0], INTEGER(giterator)[1], *scope2d, band);
		}
	} else if (isVector(giterator) && !isNull(class_name) && !strcmp(CHAR(STRING_ELT(class_name, 0)), "cartesian.grid")) {
		GIntervals intervals1;
		GIntervals intervals2;

		m_iu.convert_rintervs(VECTOR_ELT(giterator, 0), &intervals1, NULL, false);
		if (!isNull(VECTOR_ELT(giterator, 1)))
			m_iu.convert_rintervs(VECTOR_ELT(giterator, 1), &intervals2, NULL, false);

		SEXP rexpansion1 = VECTOR_ELT(giterator, 2);
		SEXP rexpansion2 = VECTOR_ELT(giterator, 3);

		if ((!isReal(rexpansion1) && !isInteger(rexpansion1)) || (!isNull(rexpansion2) && !isReal(rexpansion2) && !isInteger(rexpansion2)))
			verror("Invalid format of cartesian grid iterator");

		vector<int64_t> expansion1(length(rexpansion1));
		for (int i = 0; i < length(rexpansion1); ++i)
			expansion1[i] = isReal(rexpansion1) ? (int64_t)REAL(rexpansion1)[i] : INTEGER(rexpansion1)[i];

		vector<int64_t> expansion2;
		if (isNull(rexpansion2))
			expansion2 = expansion1;
		else {
			expansion2.resize(length(rexpansion2));
			for (int i = 0; i < length(rexpansion2); ++i)
				expansion2[i] = isReal(rexpansion2) ? (int64_t)REAL(rexpansion2)[i] : INTEGER(rexpansion2)[i];
		}

		SEXP rband_idx = VECTOR_ELT(giterator, 4);

		if ((!isReal(rband_idx) && !isInteger(rband_idx)) || length(rband_idx) != 3)
			verror("Invalid format of cartesian grid iterator");

		int64_t min_band_idx = isReal(rband_idx) ? (int64_t)REAL(rband_idx)[0] : INTEGER(rband_idx)[0];
		int64_t max_band_idx = isReal(rband_idx) ? (int64_t)REAL(rband_idx)[1] : INTEGER(rband_idx)[1];
		bool use_band_idx_limit = isReal(rband_idx) ? (bool)REAL(rband_idx)[2] : (bool)INTEGER(rband_idx)[2];

		expr_itr = new TrackExpressionCartesianGridIterator;
		if (call_begin) 
			((TrackExpressionCartesianGridIterator *)expr_itr)->begin(m_iu.get_chromkey(), &intervals1, isNull(VECTOR_ELT(giterator, 1)) ? NULL : &intervals2,
																	  expansion1, expansion2, min_band_idx, max_band_idx, use_band_idx_limit, *scope2d, band,
																	  m_iu.get_max_data_size());
	} else if (isString(giterator) && length(giterator) == 1 && GIntervalsBigSet::isbig(CHAR(STRING_ELT(giterator, 0)), m_iu)) {
		const char *intervset = CHAR(STRING_ELT(giterator, 0));
		SEXP meta = GIntervalsMeta::load_meta(interv2path(m_iu.get_env(), intervset).c_str());

		if (GIntervalsBigSet1D::is1d(meta)) {
			expr_itr = new TrackExpressionBigSet1DIterator(m_iu);
			if (call_begin) 
				((TrackExpressionBigSet1DIterator *)expr_itr)->begin(intervset, meta, *scope1d);
		} else if (GIntervalsBigSet2D::is2d(meta)) {
			expr_itr = new TrackExpressionBigSet2DIterator(m_iu);
			if (call_begin) 
				((TrackExpressionBigSet2DIterator *)expr_itr)->begin(intervset, meta, *scope2d, band, m_iu.get_max_data_size());
		}
		runprotect(meta);
	} else if ((isVector(giterator) && !isString(giterator)) ||
			   (isString(giterator) && length(giterator) == 1 && !m_iu.track_exists(CHAR(STRING_ELT(giterator, 0)))))
	{   // giterator == intervals
		unsigned intervs_type_mask;
		if (m_iu.convert_rintervs(giterator, &intervals1d, &intervals2d, true, NULL, "", &intervs_type_mask)) {
			if (intervs_type_mask == (IntervUtils::INTERVS1D | IntervUtils::INTERVS2D))
				verror("Dual (1D and 2D) intervals cannot be used as an iterator");

			if (intervs_type_mask == IntervUtils::INTERVS1D) {
				verify_1d_iter(scope1d, scope2d);
				intervals1d.sort();
				intervals1d.unify_overlaps(false);
				expr_itr = new TrackExpressionIntervals1DIterator();
				if (call_begin) 
					((TrackExpressionIntervals1DIterator *)expr_itr)->begin(intervals1d, *scope1d);
			} else if (intervs_type_mask == IntervUtils::INTERVS2D) {
				verify_2d_iter(scope1d, scope2d);
				intervals2d.sort();
				intervals2d.verify_no_overlaps(m_iu.get_chromkey());
				expr_itr = new TrackExpressionIntervals2DIterator();
				if (call_begin) 
					((TrackExpressionIntervals2DIterator *)expr_itr)->begin(m_iu.get_chromkey(), intervals2d, *scope2d, band, m_iu.get_max_data_size());
			}
		}
	}

	int common_track_type = -1;
	int common_binsize = -1;

	GIntervals all_genome_intervs;
	m_iu.get_all_genome_intervs(all_genome_intervs);

	unsigned num_tracks = vars.get_num_track_vars();
	vector<string> track_names;
	vector<GenomeTrack::Type> track_types;

	for (unsigned ivar = 0; ivar < vars.get_num_track_vars(); ++ivar) {
		track_names.push_back(vars.get_track_name(ivar));
		track_types.push_back(vars.get_track_type(ivar));
	}

	if (isString(giterator) && !expr_itr) {
		string iter_val(CHAR(STRING_ELT(giterator, 0)));

		if (find(track_names.begin(), track_names.end(), iter_val) == track_names.end()) {
			if (isString(giterator)) {
				SEXP all_track_names;
				SEXPCleaner all_track_names_cleaner(all_track_names);

				rprotect(all_track_names = findVar(install("GTRACKS"), findVar(install(".misha"), m_iu.get_env())));
				if (isString(all_track_names)) {
					int i;
					for (i = 0; i < length(all_track_names); ++i) {
						if (iter_val == CHAR(STRING_ELT(all_track_names, i)))
							break;
					}
					if (i >= length(all_track_names)) 
						verror("Invalid iterator: %s is neither a name of a track nor a name of an intervals set", iter_val.c_str());
				}
                        }
			track_names.push_back(iter_val);
			track_types.push_back(GenomeTrack::get_type(track2path(m_iu.get_env(), iter_val).c_str(), m_iu.get_chromkey()));
		}
	}

	try {
		for (vector<string>::const_iterator itrack_name = track_names.begin(); itrack_name != track_names.end(); ++itrack_name) {
			string trackpath(track2path(m_iu.get_env(), *itrack_name));
			GenomeTrack::Type track_type = track_types[itrack_name - track_names.begin()];
			vector<string> filenames;
			unsigned binsize = 0;

			// read the list of chrom files
			get_chrom_files(trackpath.c_str(), filenames);
			sort(filenames.begin(), filenames.end());

			if (GenomeTrack::is_1d(track_type)) {
				set<int> chromids;

				for (vector<string>::const_iterator ifilename = filenames.begin(); ifilename != filenames.end(); ++ifilename) {										
					int chromid = -1;
					GenomeTrackFixedBin gtrack_fbin;

					try {
						 chromid = GenomeTrack::get_chromid_1d(m_iu.get_chromkey(), *ifilename);
					} catch (TGLException &e) {
						verror("Track %s: %s\n", itrack_name->c_str(), e.msg());
					}

					chromids.insert(chromid);
					if (track_type == GenomeTrack::FIXED_BIN) {
                        gtrack_fbin.init_read((trackpath + "/" + *ifilename).c_str(), chromid);
                        int64_t expected_num_bins = (int64_t)ceil(all_genome_intervs[chromid].end / (double)gtrack_fbin.get_bin_size());

                        if (gtrack_fbin.get_num_samples() != expected_num_bins){
                            verror("Number of bins in track %s, chrom %s do not match the chromosome size (expecting: %ld, reading: %ld)",
                                    itrack_name->c_str(), m_iu.get_chromkey().id2chrom(chromid).c_str(), expected_num_bins, gtrack_fbin.get_num_samples());
						}
					}

					if (ifilename == filenames.begin()) {
						if (isString(giterator)) {
							if (*itrack_name == CHAR(STRING_ELT(giterator, 0)))
								common_track_type = track_type;
						} else {
							if (itrack_name == track_names.begin())
								common_track_type = track_type;
							else if (common_track_type != track_type)
								common_track_type = -1;
						}

						if (track_type == GenomeTrack::FIXED_BIN) {
							binsize = gtrack_fbin.get_bin_size();

							if (isString(giterator)) {
								if (*itrack_name == CHAR(STRING_ELT(giterator, 0)))
									common_binsize = binsize;
							} else {
								if (itrack_name == track_names.begin())
									common_binsize = binsize;
								else if (common_binsize != (int)binsize)
									common_binsize = -1;
							}
						}
					} else if (track_type == GenomeTrack::FIXED_BIN && binsize != gtrack_fbin.get_bin_size())
						verror("Track %s: bin size of chroms %s and %s differ (%d and %d respectively)",
								itrack_name->c_str(), m_iu.id2chrom(GenomeTrack::get_chromid_1d(m_iu.get_chromkey(), *(ifilename - 1))).c_str(),
								m_iu.id2chrom(chromid).c_str(), binsize, gtrack_fbin.get_bin_size());
				}

				for (GIntervals::const_iterator iinterv = all_genome_intervs.begin(); iinterv != all_genome_intervs.end(); ++iinterv) {
					if (chromids.find(iinterv->chromid) == chromids.end())
						verror("Chrom %s presented in the global chrom list is missing in track %s", m_iu.id2chrom(iinterv->chromid).c_str(), itrack_name->c_str());
				}
			} else if (GenomeTrack::is_2d(track_type)) {
				for (vector<string>::const_iterator ifilename = filenames.begin(); ifilename != filenames.end(); ++ifilename) {
					try {
						 GenomeTrack::get_chromid_2d(m_iu.get_chromkey(), *ifilename);
					} catch (TGLException &e) {
						verror("Track %s: %s\n", itrack_name->c_str(), e.msg());
					}

					if (ifilename == filenames.begin()) {
						if (isString(giterator)) {
							if (*itrack_name == CHAR(STRING_ELT(giterator, 0)))
								common_track_type = track_type;
						} else {
							if (itrack_name == track_names.begin())
								common_track_type = track_type;
							else if (common_track_type != track_type)
								common_track_type = -1;
						}
					}
				}
			} else
				verror("Tracks of type %s are not supported in track expression", GenomeTrack::TYPE_NAMES[track_type]);
		}
	} catch (TGLException &e) {
		verror("%s\n", e.msg());
	}

	// if the iterator type is track or NULL (set implicitly), the iterator should be initialized now
	if (!expr_itr) {
		if (common_track_type < 0) {
			if (num_tracks) {
				if (track_exprs.size() == 1)
					verror("Cannot implicitly determine iterator policy:\ntrack expression \"%s\" contains tracks in different formats.\n", track_exprs.front().c_str());
				verror("Cannot implicitly determine iterator policy: track expressions contain tracks in different formats.\n");
			}
			if (track_exprs.size() == 1)
				verror("Cannot implicitly determine iterator policy:\ntrack expression \"%s\" does not contain any tracks.\n", track_exprs.front().c_str());
			verror("Cannot implicitly determine iterator policy: track expressions do not contain any tracks.\n");
		}

		if (common_track_type == GenomeTrack::FIXED_BIN && common_binsize < 0) {
			if (track_exprs.size() == 1)
				verror("Cannot implicitly determine iterator policy:\ntrack expression \"%s\" contains tracks with different bin sizes.\n", track_exprs.front().c_str());
			verror("Cannot implicitly determine iterator policy: track expressions contain tracks with different bin sizes.\n");
		}

		if (!isString(giterator) &&
			(common_track_type == GenomeTrack::SPARSE || common_track_type == GenomeTrack::ARRAYS ||
			 common_track_type == GenomeTrack::POINTS || common_track_type == GenomeTrack::RECTS ||
			 common_track_type == GenomeTrack::COMPUTED) &&
			num_tracks > 1)
		{
			for (unsigned ivar = 1; ivar < vars.get_num_track_vars(); ++ivar) {
				if (vars.get_track_name(ivar) != vars.get_track_name(ivar - 1)) {
					if (track_exprs.size() == 1)
						verror("Cannot implicitly determine iterator policy: track expression \"%s\" contains more than one %s track.\n",
								track_exprs.front().c_str(), GenomeTrack::TYPE_NAMES[common_track_type]);
					verror("Cannot implicitly determine iterator policy: track expressions contain more than one %s track.\n", GenomeTrack::TYPE_NAMES[common_track_type]);
				}
			}
		}

		if (common_track_type == GenomeTrack::FIXED_BIN) {
			verify_1d_iter(scope1d, scope2d);
			expr_itr = new TrackExpressionFixedBinIterator();
			if (call_begin) 
				((TrackExpressionFixedBinIterator *)expr_itr)->begin(common_binsize, *scope1d);
		} else if (common_track_type == GenomeTrack::SPARSE || common_track_type == GenomeTrack::ARRAYS) {
			verify_1d_iter(scope1d, scope2d);
			expr_itr = new TrackExpressionSparseIterator(m_iu, (GenomeTrack::Type)common_track_type);
			if (call_begin) {
				if (isString(giterator))
					((TrackExpressionSparseIterator *)expr_itr)->begin(track2path(m_iu.get_env(), CHAR(STRING_ELT(giterator, 0))), *scope1d);
				else
					((TrackExpressionSparseIterator *)expr_itr)->begin(track2path(m_iu.get_env(), vars.get_track_name(0)), *scope1d);
			}
		} else if (common_track_type == GenomeTrack::RECTS || common_track_type == GenomeTrack::POINTS || common_track_type == GenomeTrack::COMPUTED) {
			verify_2d_iter(scope1d, scope2d);
			expr_itr = new TrackExpressionTrackRectsIterator(m_iu);
			if (call_begin) {
				if (isString(giterator))
					((TrackExpressionTrackRectsIterator *)expr_itr)->begin(track2path(m_iu.get_env(), CHAR(STRING_ELT(giterator, 0))), (GenomeTrack::Type)common_track_type, *scope2d, band, m_iu.get_max_data_size());
				else
					((TrackExpressionTrackRectsIterator *)expr_itr)->begin(track2path(m_iu.get_env(), vars.get_track_name(0)), (GenomeTrack::Type)common_track_type, *scope2d, band, m_iu.get_max_data_size());
			}
		} else
		verror("Unrecognized type of iterator");
	}

	return expr_itr;
}
