/*
 * rdbinterval.cpp
 *
 *  Created on: Sep 7, 2010
 *      Author: hoichman
 */

#include <cstdint>
#include <cmath>
#include <fstream>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>

#include "rdbinterval.h"
#include "rdbutils.h"
#include "TrackExpressionScanner.h"
#include "GIntervalsBigSet1D.h"
#include "GIntervalsBigSet2D.h"
#include "GTrackIntervalsFetcher1D.h"
#include "GTrackIntervalsFetcher2D.h"

using namespace std;

#include "strutil.h"

using namespace rdb;

const char *IntervalPval::COL_NAMES[IntervalPval::NUM_COLS] = { "chrom", "start", "end", "pval" };

const char *ChainInterval::COL_NAMES[ChainInterval::NUM_COLS] = { "chrom", "start", "end", "chromsrc", "startsrc" };

IntervUtils::IntervUtils(SEXP envir)
{
	m_envir = envir;
	m_num_planned_kids = 0;
	m_multitasking = -1;
	m_kid_intervals1d = NULL;
	m_kid_intervals2d = NULL;

	m_kids_intervals1d.clear();
	m_kids_intervals2d.clear();

	
	m_allgenome = findVar(install("ALLGENOME"), findVar(install(".misha"), m_envir));

	if (isNull(m_allgenome))
		verror("ALLGENOME variable does not exist");

	if (!isVector(m_allgenome) || length(m_allgenome) != 2)
		verror("ALLGENOME variable has invalid type");

	SEXP chroms = VECTOR_ELT(get_rallgenome1d(), GInterval::CHROM);
	SEXP chrom_sizes = VECTOR_ELT(get_rallgenome1d(), GInterval::END);
	SEXP chrom_levels = getAttrib(chroms, R_LevelsSymbol);
	unsigned num_intervals = (unsigned)length(chroms);

	for (unsigned i = 0; i < num_intervals; i++) {
		const char *chrom = isString(chroms) ? CHAR(STRING_ELT(chroms, i)) : CHAR(STRING_ELT(chrom_levels, INTEGER(chroms)[i] - 1));
		double chrom_size = isReal(chrom_sizes) ? REAL(chrom_sizes)[i] : INTEGER(chrom_sizes)[i];
		try {
			m_chrom_key.add_chrom(chrom, (uint64_t)chrom_size);
		} catch (TGLException &e) {
			verror("Reading ALLGENOME: %s", e.msg());
		}
	}

    GenomeTrack::set_rnd_func(unif_rand);
}

IntervUtils::~IntervUtils()
{
	for (vector<GIntervalsFetcher1D *>::iterator iinterv = m_kids_intervals1d.begin(); iinterv != m_kids_intervals1d.end(); ++iinterv) 
		delete *iinterv;
	for (vector<GIntervalsFetcher2D *>::iterator iinterv = m_kids_intervals2d.begin(); iinterv != m_kids_intervals2d.end(); ++iinterv) 
		delete *iinterv;
}

bool IntervUtils::track_exists(const char *track_name)
{
	SEXP all_track_names;

	rprotect(all_track_names = findVar(install("GTRACKS"), findVar(install(".misha"), get_env())));
	if (isString(all_track_names)) {
		for (int i = 0; i < length(all_track_names); ++i) {
			if (!strcmp(track_name, CHAR(STRING_ELT(all_track_names, i))))
				return true;
		}
	}
	return false;
}

void IntervUtils::get_all_genome_intervs(GIntervals &intervals) const
{
	intervals.clear();
	convert_rintervs(get_rallgenome1d(), &intervals, NULL);
	intervals.sort();
}

void IntervUtils::get_all_genome_intervs(GIntervals2D &intervals) const
{
	intervals.clear();
	convert_rintervs(get_rallgenome2d(), NULL, &intervals);
	intervals.sort();
}

GIntervalsFetcher1D *IntervUtils::get_kid_intervals1d()
{
	if (!m_kid_intervals1d && !m_kids_intervals1d.empty()) 
		return m_kids_intervals1d[RdbInitializer::get_kid_idx()];

	return NULL;
}

GIntervalsFetcher2D *IntervUtils::get_kid_intervals2d()
{
	if (!m_kid_intervals2d && !m_kids_intervals2d.empty())
		return m_kids_intervals2d[RdbInitializer::get_kid_idx()];

	return NULL;
}

unsigned IntervUtils::get_rintervs_type_mask(SEXP rintervals, const char *error_msg_prefix) const
{
	if (!isVector(rintervals))
		verror("%sInvalid format of intervals argument", error_msg_prefix);

	if (length(rintervals) == 2) {
		if (get_rintervs_type_mask(VECTOR_ELT(rintervals, 0), error_msg_prefix) != INTERVS1D || get_rintervs_type_mask(VECTOR_ELT(rintervals, 1), error_msg_prefix) != INTERVS2D)
			verror("%sInvalid format of intervals argument", error_msg_prefix);
		return INTERVS1D | INTERVS2D;
	}

	SEXP colnames = getAttrib(rintervals, R_NamesSymbol);

	if (!isString(colnames) || length(colnames) < GInterval::NUM_COLS)
		verror("%sInvalid format of intervals argument", error_msg_prefix);

	IntervUtils::IntervsType type = INTERVS1D;

	for (unsigned i = 0; i < GInterval::NUM_COLS; i++) {
		if (strcmp(CHAR(STRING_ELT(colnames, i)), GInterval::COL_NAMES[i])) {
			type = INTERVS2D;
			break;
		}
	}

	if (type == INTERVS2D) {
		for (unsigned i = 0; i < GInterval2D::NUM_COLS; i++) {
			if (strcmp(CHAR(STRING_ELT(colnames, i)), GInterval2D::COL_NAMES[i]))
				verror("Invalid format of intervals: column names do not match neither 1d nor 2d intervals");
		}
	}

	if (type == INTERVS1D) {
		SEXP starts = VECTOR_ELT(rintervals, GInterval::START);
		SEXP ends = VECTOR_ELT(rintervals, GInterval::END);
		SEXP strands = R_NilValue;
		SEXP colnames = getAttrib(rintervals, R_NamesSymbol);

		for (int i = 0; i < length(rintervals); i++) {
			if (!strcmp(CHAR(STRING_ELT(colnames, i)), "strand")) {
				if (length(VECTOR_ELT(rintervals, i)) != length(VECTOR_ELT(rintervals, GInterval::CHROM)))
					verror("%sNumber of rows in column %s differs than the number of rows in column strand", error_msg_prefix, GInterval::COL_NAMES[GInterval::CHROM]);
				break;
			}
		}

		for (unsigned i = 0; i < GInterval::NUM_COLS; i++) {
			if (i != 0 && length(VECTOR_ELT(rintervals, i)) != length(VECTOR_ELT(rintervals, i - 1)))
				verror("%sNumber of rows in column %s differs than the number of rows in column %s", error_msg_prefix, GInterval::COL_NAMES[i - 1], GInterval::COL_NAMES[i]);
		}

		if ((!isReal(starts) && !isInteger(starts)) || (!isReal(ends) && !isInteger(ends)) || (strands != R_NilValue && !isReal(strands) && !isInteger(strands)))
			verror("%sInvalid format of intervals argument", error_msg_prefix);

	} else if (type == INTERVS2D) {
		SEXP starts1 = VECTOR_ELT(rintervals, GInterval2D::START1);
		SEXP ends1 = VECTOR_ELT(rintervals, GInterval2D::END1);
		SEXP starts2 = VECTOR_ELT(rintervals, GInterval2D::START2);
		SEXP ends2 = VECTOR_ELT(rintervals, GInterval2D::END2);

		for (unsigned i = 0; i < GInterval2D::NUM_COLS; i++) {
			if (i != 0 && length(VECTOR_ELT(rintervals, i)) != length(VECTOR_ELT(rintervals, i - 1)))
				verror("%sNumber of rows in column %s differs than the number of rows in column %s", error_msg_prefix, GInterval2D::COL_NAMES[i - 1], GInterval2D::COL_NAMES[i]);
		}

		if ((!isReal(starts1) && !isInteger(starts1)) || (!isReal(ends1) && !isInteger(ends1)) || (!isReal(starts2) && !isInteger(starts2)) || (!isReal(ends2) && !isInteger(ends2)))
			verror("%sInvalid format of intervals argument", error_msg_prefix);
	} else
		verror("%sUnexpected intervals type", error_msg_prefix);

	return type;
}

unsigned IntervUtils::convert_rintervs(SEXP rintervals, GIntervalsFetcher1D **intervals1d, GIntervalsFetcher2D **intervals2d, bool null_if_interv_nonexist,
									   const GenomeChromKey *chromkey, const char *error_msg_prefix, bool verify) const
{
	monitor_memusage();
	if (intervals1d) 
		*intervals1d = NULL;
	if (intervals2d) 
		*intervals2d = NULL;
	try {
		if (isString(rintervals) && length(rintervals) == 1) {
			const char *full_interv_name = CHAR(STRING_ELT(rintervals, 0));

			if (GIntervalsBigSet::isbig(full_interv_name, *this)) { // big intervals set?
				SEXP meta = GIntervalsMeta::load_meta(interv2path(get_env(), full_interv_name).c_str());

				if (GIntervalsMeta1D::is1d(meta)) {
					if (intervals1d) {
						*intervals1d = new GIntervalsBigSet1D(full_interv_name, meta, *this);
						if (intervals2d)
							*intervals2d = new GIntervals2D();  // create an empty intervals set for convenience
						return IntervUtils::INTERVS1D;
					}
					verror("%sExpecting 2D intervals while receiving 1D intervals set %s", error_msg_prefix, full_interv_name);
				} else {
					if (intervals2d) {
						*intervals2d = new GIntervalsBigSet2D(full_interv_name, meta, *this);
						if (intervals1d)
							*intervals1d = new GIntervals();  // create an empty intervals set for convenience
						return IntervUtils::INTERVS2D;
					}
					verror("%sExpecting 1D intervals while receiving 2D intervals set %s", error_msg_prefix, full_interv_name);
				}
			} else if (GTrackIntervalsFetcher::isbig(full_interv_name, *this)) {
				string trackpath = track2path(get_env(), full_interv_name);

				if (access((trackpath + "/.meta").c_str(), R_OK) && errno == ENOENT)
					GTrackIntervalsFetcher::create_track_meta(full_interv_name, *this);

				SEXP meta = GIntervalsMeta::load_meta(trackpath.c_str());

				if (GIntervalsMeta1D::is1d(meta)) {
					if (intervals1d) {
						GenomeTrack::Type track_type = GenomeTrack::get_type(trackpath.c_str(), get_chromkey(), true);

						if (track_type == GenomeTrack::FIXED_BIN)
							verror("%s is track of %s type which cannot be used as a replacement for intervals",
								   full_interv_name, GenomeTrack::TYPE_NAMES[track_type]);
						else if (track_type == GenomeTrack::SPARSE)
							*intervals1d = new GTrackIntervalsFetcher1D<GenomeTrackSparse>(full_interv_name, meta, *this);
						else if (track_type == GenomeTrack::ARRAYS)
							*intervals1d = new GTrackIntervalsFetcher1D<GenomeTrackArrays>(full_interv_name, meta, *this);

						if (intervals2d)
							*intervals2d = new GIntervals2D();  // create an empty intervals set for convenience
						return IntervUtils::INTERVS1D;
					}
					verror("%sExpecting 2D intervals while receiving 1D intervals set %s", error_msg_prefix, full_interv_name);
				} else {
					if (intervals2d) {
						GenomeTrack::Type track_type = GenomeTrack::get_type(trackpath.c_str(), get_chromkey(), true);

						if (track_type == GenomeTrack::RECTS)
							*intervals2d = new GTrackIntervalsFetcher2D<GenomeTrackRectsRects>(full_interv_name, meta, *this);
						else if (track_type == GenomeTrack::POINTS)
							*intervals2d = new GTrackIntervalsFetcher2D<GenomeTrackRectsPoints>(full_interv_name, meta, *this);
						else if (track_type == GenomeTrack::COMPUTED)
							*intervals2d = new GTrackIntervalsFetcher2D<GenomeTrackComputed>(full_interv_name, meta, *this);

						if (intervals1d)
							*intervals1d = new GIntervals();  // create an empty intervals set for convenience
						return IntervUtils::INTERVS2D;
					}
					verror("%sExpecting 1D intervals while receiving 2D intervals set %s", error_msg_prefix, full_interv_name);
				}
			}
		}

		if (intervals1d)
			*intervals1d = new GIntervals();
		if (intervals2d)
			*intervals2d = new GIntervals2D();

		unsigned intervs_type_mask;
		convert_rintervs(rintervals, intervals1d ? (GIntervals *)*intervals1d : NULL, intervals2d ? (GIntervals2D *)*intervals2d : NULL,
						 null_if_interv_nonexist, chromkey, error_msg_prefix, &intervs_type_mask, verify);
		return intervs_type_mask;
	} catch (...) {
		if (intervals1d) {
			delete *intervals1d;
			*intervals1d = NULL;
		}
		if (intervals2d) {
			delete *intervals2d;
			*intervals2d = NULL;
		}
		throw;
	}
}

SEXP IntervUtils::convert_rintervs(SEXP rintervals, GIntervals *intervals, GIntervals2D *intervals2d, bool null_if_interv_nonexist,
								   const GenomeChromKey *chromkey, const char *error_msg_prefix, unsigned *pintervs_type_mask, bool verify) const
{
	monitor_memusage();

	if (intervals)
		intervals->clear();

	if (intervals2d)
		intervals2d->clear();

	if (pintervs_type_mask) 
		*pintervs_type_mask = 0;

	bool loaded_from_file = isString(rintervals);

	// rintervals is the name of the intervals file
	if (isString(rintervals) && length(rintervals) == 1) {
		const char *full_interv_name = CHAR(STRING_ELT(rintervals, 0));
		SEXP gintervs;
		bool interv_found = false;

		rprotect(gintervs = findVar(install("GINTERVS"), findVar(install(".misha"),m_envir)));
		if (isString(gintervs)) {
			for (int iinterv = 0; iinterv < length(gintervs); ++iinterv) {
				const char *interv = CHAR(STRING_ELT(gintervs, iinterv));
				if (!strcmp(full_interv_name, interv)) {
					interv_found = true;
					break;
				}
			}
		}

		if (!interv_found) {
			if (GTrackIntervalsFetcher::isbig(full_interv_name, *this))
				verror("Tracks cannot be used as a substitute for intervals in this function");

			if (null_if_interv_nonexist)
				return R_NilValue;

			verror("%sInterval %s does not exist", error_msg_prefix, full_interv_name);
		}

		string path = interv2path(m_envir, full_interv_name);
		struct stat stat_res;
		if (!stat(path.c_str(), &stat_res) && S_ISDIR(stat_res.st_mode))
			verror("%s is a big intervals set. Big intervals sets are not supported by the function.", full_interv_name);
		rprotect(rintervals = RSaneUnserialize(path.c_str()));
        }

	if (TYPEOF(rintervals) == PROMSXP) {
		if (PRENV(rintervals) == R_NilValue)
			rintervals = PRVALUE(rintervals);
		else
			rintervals = eval_in_R(PRCODE(rintervals), PRENV(rintervals));
	}

	SEXP _rintervals = rintervals;
	unsigned intervs_type_mask = get_rintervs_type_mask(rintervals, error_msg_prefix);

	if (pintervs_type_mask) 
		*pintervs_type_mask = intervs_type_mask;

	if (intervs_type_mask == (INTERVS1D | INTERVS2D))
		rintervals = VECTOR_ELT(_rintervals, 0);

	if (intervs_type_mask == INTERVS1D && !intervals)
		verror("%sExpecting 2D intervals while receiving 1D intervals", error_msg_prefix);

	if (intervs_type_mask == INTERVS2D && !intervals2d)
		verror("%sExpecting 1D intervals while receiving 2D intervals", error_msg_prefix);

	if ((intervs_type_mask & INTERVS1D) && intervals) {
		SEXP chroms = VECTOR_ELT(rintervals, GInterval::CHROM);
		SEXP chrom_levels = getAttrib(chroms, R_LevelsSymbol);
		SEXP starts = VECTOR_ELT(rintervals, GInterval::START);
		SEXP ends = VECTOR_ELT(rintervals, GInterval::END);
		SEXP strands = R_NilValue;
		SEXP colnames = getAttrib(rintervals, R_NamesSymbol);
		unsigned num_intervals = (unsigned)length(starts);

		for (int i = 0; i < length(rintervals); i++) {
			if (!strcmp(CHAR(STRING_ELT(colnames, i)), "strand")) {
				strands = VECTOR_ELT(rintervals, i);
				break;
			}
		}

		for (unsigned i = 0; i < num_intervals; i++) {
			if ((isFactor(chroms) && INTEGER(chroms)[i] < 0) ||
				(isReal(starts) && std::isnan(REAL(starts)[i])) || (isReal(ends) && std::isnan(REAL(ends)[i])) ||
				(strands != R_NilValue && isReal(strands) && std::isnan(REAL(strands)[i])))
				verror("%sInvalid format of interval at index %d", error_msg_prefix, i + 1);

			const char *chrom = isString(chroms) ? CHAR(STRING_ELT(chroms, i)) : CHAR(STRING_ELT(chrom_levels, INTEGER(chroms)[i] - 1));
			int chromid = chromkey ? chromkey->chrom2id(chrom) : chrom2id(chrom);
			int64_t start = (int64_t)(isReal(starts) ? REAL(starts)[i] : INTEGER(starts)[i]);
			int64_t end = (int64_t)(isReal(ends) ? REAL(ends)[i] : INTEGER(ends)[i]);
			char strand = 0;

			if (strands != R_NilValue)
				strand = (char)(isReal(strands) ? REAL(strands)[i] : INTEGER(strands)[i]);

			GInterval interval(chromid, start, end, strand, (void *)(intptr_t)i);

			if (loaded_from_file && interval.start == interval.end) {
				interval.end++;
				if (isReal(ends))
					REAL(ends)[i]++;
				else
					INTEGER(ends)[i]++;
			}

			if (verify) 
				interval.verify(chromkey ? *chromkey : m_chrom_key);
			intervals->push_back(interval);
		}
	}

	if (intervs_type_mask == (INTERVS1D | INTERVS2D))
		rintervals = VECTOR_ELT(_rintervals, 1);

	if ((intervs_type_mask & INTERVS2D) && intervals2d) {
		SEXP chroms1 = VECTOR_ELT(rintervals, GInterval2D::CHROM1);
		SEXP chrom_levels1 = getAttrib(chroms1, R_LevelsSymbol);
		SEXP starts1 = VECTOR_ELT(rintervals, GInterval2D::START1);
		SEXP ends1 = VECTOR_ELT(rintervals, GInterval2D::END1);
		SEXP chroms2 = VECTOR_ELT(rintervals, GInterval2D::CHROM2);
		SEXP chrom_levels2 = getAttrib(chroms2, R_LevelsSymbol);
		SEXP starts2 = VECTOR_ELT(rintervals, GInterval2D::START2);
		SEXP ends2 = VECTOR_ELT(rintervals, GInterval2D::END2);
		unsigned num_intervals = (unsigned)length(starts1);

		for (unsigned i = 0; i < num_intervals; i++) {
			if ((isFactor(chroms1) && INTEGER(chroms1)[i] < 0) ||
				(isReal(starts1) && std::isnan(REAL(starts1)[i])) || (isReal(ends1) && std::isnan(REAL(ends1)[i])) ||
				(isFactor(chroms2) && INTEGER(chroms2)[i] < 0) ||
				(isReal(starts2) && std::isnan(REAL(starts2)[i])) || (isReal(ends2) && std::isnan(REAL(ends2)[i])))
				verror("%sInvalid format of interval at index %d", error_msg_prefix, i + 1);

			const char *chrom1 = isString(chroms1) ? CHAR(STRING_ELT(chroms1, i)) : CHAR(STRING_ELT(chrom_levels1, INTEGER(chroms1)[i] - 1));
			const char *chrom2 = isString(chroms2) ? CHAR(STRING_ELT(chroms2, i)) : CHAR(STRING_ELT(chrom_levels2, INTEGER(chroms2)[i] - 1));
			int chromid1 = chromkey ? chromkey->chrom2id(chrom1) : chrom2id(chrom1);
			int chromid2 = chromkey ? chromkey->chrom2id(chrom2) : chrom2id(chrom2);
			int64_t start1 = (int64_t)(isReal(starts1) ? REAL(starts1)[i] : INTEGER(starts1)[i]);
			int64_t start2 = (int64_t)(isReal(starts2) ? REAL(starts2)[i] : INTEGER(starts2)[i]);
			int64_t end1 = (int64_t)(isReal(ends1) ? REAL(ends1)[i] : INTEGER(ends1)[i]);
			int64_t end2 = (int64_t)(isReal(ends2) ? REAL(ends2)[i] : INTEGER(ends2)[i]);

			GInterval2D interval(chromid1, start1, end1, chromid2, start2, end2, (void *)(intptr_t)i);

			if (verify) 
				interval.verify(chromkey ? *chromkey : m_chrom_key);
			intervals2d->push_back(interval);
		}
	}

	return _rintervals;
}

SEXP IntervUtils::convert_intervs(GIntervalsFetcher1D *intervals, unsigned num_cols, bool null_if_empty, bool use_original_index) const
{
	monitor_memusage();

	if (null_if_empty && !intervals->size())
		return R_NilValue;

	unsigned num_chroms = m_chrom_key.get_num_chroms();

	SEXP answer;
	SEXP chroms, chroms_idx, starts, ends;
	SEXP row_names;
	SEXP col_names;

	rprotect(answer = RSaneAllocVector(VECSXP, num_cols));
    rprotect(chroms_idx = RSaneAllocVector(INTSXP, intervals->size()));
    rprotect(starts = RSaneAllocVector(REALSXP, intervals->size()));
    rprotect(ends = RSaneAllocVector(REALSXP, intervals->size()));
    rprotect(chroms = RSaneAllocVector(STRSXP, num_chroms));
    rprotect(col_names = RSaneAllocVector(STRSXP, num_cols));
    rprotect(row_names = RSaneAllocVector(INTSXP, intervals->size()));

	for (intervals->begin_iter(); !intervals->isend(); intervals->next()) {
		const GInterval &interval = intervals->cur_interval();
		uint64_t index = use_original_index ? get_orig_interv_idx(interval) : intervals->iter_index();

		INTEGER(chroms_idx)[index] = interval.chromid + 1;
		REAL(starts)[index] = interval.start;
		REAL(ends)[index] = interval.end;
		INTEGER(row_names)[index] = index + 1;
	}

	for (unsigned id = 0; id < (unsigned)num_chroms; ++id)
		SET_STRING_ELT(chroms, id, mkChar(m_chrom_key.id2chrom(id).c_str()));

	for (int i = 0; i < GInterval::NUM_COLS; i++)
		SET_STRING_ELT(col_names, i, mkChar(GInterval::COL_NAMES[i]));

    setAttrib(chroms_idx, R_LevelsSymbol, chroms);
    setAttrib(chroms_idx, R_ClassSymbol, mkString("factor"));

    SET_VECTOR_ELT(answer, GInterval::CHROM, chroms_idx);
    SET_VECTOR_ELT(answer, GInterval::START, starts);
    SET_VECTOR_ELT(answer, GInterval::END, ends);

    setAttrib(answer, R_NamesSymbol, col_names);
    setAttrib(answer, R_ClassSymbol, mkString("data.frame"));
    setAttrib(answer, R_RowNamesSymbol, row_names);

	return answer;
}

SEXP IntervUtils::convert_intervs(GIntervalsFetcher2D *intervals, unsigned num_cols, bool null_if_empty, bool use_original_index) const
{
	monitor_memusage();

	if (null_if_empty && !intervals->size())
		return R_NilValue;

	unsigned num_chroms = m_chrom_key.get_num_chroms();

	SEXP answer;
	SEXP chroms1, chroms2, chroms_idx1, chroms_idx2, starts1, starts2, ends1, ends2;
	SEXP row_names;
	SEXP col_names;

	rprotect(answer = RSaneAllocVector(VECSXP, num_cols));
    rprotect(chroms1 = RSaneAllocVector(STRSXP, num_chroms));
    rprotect(starts1 = RSaneAllocVector(REALSXP, intervals->size()));
    rprotect(ends1 = RSaneAllocVector(REALSXP, intervals->size()));
    rprotect(chroms_idx1 = RSaneAllocVector(INTSXP, intervals->size()));
    rprotect(chroms_idx2 = RSaneAllocVector(INTSXP, intervals->size()));
    rprotect(starts2 = RSaneAllocVector(REALSXP, intervals->size()));
    rprotect(ends2 = RSaneAllocVector(REALSXP, intervals->size()));
    rprotect(chroms1 = RSaneAllocVector(STRSXP, num_chroms));
    rprotect(chroms2 = RSaneAllocVector(STRSXP, num_chroms));
    rprotect(col_names = RSaneAllocVector(STRSXP, num_cols));
    rprotect(row_names = RSaneAllocVector(INTSXP, intervals->size()));

	for (intervals->begin_iter(); !intervals->isend(); intervals->next()) {
		const GInterval2D &interval = intervals->cur_interval();
		uint64_t index = use_original_index ? get_orig_interv_idx(interval) : intervals->iter_index();

		INTEGER(chroms_idx1)[index] = interval.chromid1() + 1;
		REAL(starts1)[index] = interval.start1();
        REAL(ends1)[index] = interval.end1();
        INTEGER(chroms_idx2)[index] = interval.chromid2() + 1;
        REAL(starts2)[index] = interval.start2();
        REAL(ends2)[index] = interval.end2();
		INTEGER(row_names)[index] = index + 1;
	}

    for (unsigned id = 0; id < (unsigned)num_chroms; ++id) {
        SET_STRING_ELT(chroms1, id, mkChar(m_chrom_key.id2chrom(id).c_str()));
        SET_STRING_ELT(chroms2, id, mkChar(m_chrom_key.id2chrom(id).c_str()));
    }

    for (int i = 0; i < GInterval2D::NUM_COLS; i++)
        SET_STRING_ELT(col_names, i, mkChar(GInterval2D::COL_NAMES[i]));


    setAttrib(chroms_idx1, R_LevelsSymbol, chroms1);
    setAttrib(chroms_idx1, R_ClassSymbol, mkString("factor"));
    setAttrib(chroms_idx2, R_LevelsSymbol, chroms2);
    setAttrib(chroms_idx2, R_ClassSymbol, mkString("factor"));

    SET_VECTOR_ELT(answer, GInterval2D::CHROM1, chroms_idx1);
    SET_VECTOR_ELT(answer, GInterval2D::START1, starts1);
    SET_VECTOR_ELT(answer, GInterval2D::END1, ends1);
    SET_VECTOR_ELT(answer, GInterval2D::CHROM2, chroms_idx2);
    SET_VECTOR_ELT(answer, GInterval2D::START2, starts2);
    SET_VECTOR_ELT(answer, GInterval2D::END2, ends2);

    setAttrib(answer, R_NamesSymbol, col_names);
    setAttrib(answer, R_ClassSymbol, mkString("data.frame"));
    setAttrib(answer, R_RowNamesSymbol, row_names);

    return answer;
}

void IntervUtils::convert_rchain_intervs(SEXP rchain, ChainIntervals &chain_intervs, vector<string> &src_id2chrom)
{
	if (!isVector(rchain) || length(rchain) != ChainInterval::NUM_COLS)
		TGLError("Invalid format of chain argument");

	SEXP colnames = getAttrib(rchain, R_NamesSymbol);

	if (!isString(colnames) || length(colnames) != ChainInterval::NUM_COLS)
		verror("Invalid format of chain argument");

	for (unsigned i = 0; i < ChainInterval::NUM_COLS; i++) {
		if (strcmp(CHAR(STRING_ELT(colnames, i)), ChainInterval::COL_NAMES[i]))
			verror("Invalid format of chain argument");
	}

	// convert the first 3 columns of the data frame
	GIntervals intervs;
	convert_rintervs(rchain, &intervs, NULL);

	// convert the rest of the columns
	SEXP src_chroms = VECTOR_ELT(rchain, ChainInterval::CHROM_SRC);
	SEXP src_chrom_levels = getAttrib(src_chroms, R_LevelsSymbol);
	SEXP src_starts = VECTOR_ELT(rchain, ChainInterval::START_SRC);

	for (unsigned i = 0; i < ChainInterval::NUM_COLS; i++) {
		if (i != 0 && length(VECTOR_ELT(rchain, i)) != length(VECTOR_ELT(rchain, i - 1)))
			verror("Number of rows in column %s differs than the number of rows in column %s", ChainInterval::COL_NAMES[i - 1], ChainInterval::COL_NAMES[i]);
	}

	if (!isReal(src_starts) && !isInteger(src_starts))
		verror("Invalid format of intervals argument");

	unordered_map<string, int> src_chrom2id;

	for (unsigned i = 0; i < intervs.size(); i++) {
		if ((isFactor(src_chroms) && INTEGER(src_chroms)[i] < 0) || (isReal(src_starts) && std::isnan(REAL(src_starts)[i])))
			verror("Invalid format of interval at index %d", i + 1);

		const char *src_chrom = isString(src_chroms) ? CHAR(STRING_ELT(src_chroms, i)) : CHAR(STRING_ELT(src_chrom_levels, INTEGER(src_chroms)[i] - 1));
		unordered_map<string, int>::const_iterator isrc_chrom2id = src_chrom2id.find(src_chrom);
		int src_chromid;

		if (isrc_chrom2id == src_chrom2id.end()) {
			src_chromid = src_id2chrom.size();
			src_id2chrom.push_back(src_chrom);
			src_chrom2id[src_chrom] = src_chromid;
		} else
			src_chromid = isrc_chrom2id->second;

		int64_t src_start = (int64_t)(isReal(src_starts) ? REAL(src_starts)[i] : INTEGER(src_starts)[i]);

		ChainInterval interval(intervs[i].chromid, intervs[i].start, intervs[i].end, src_chromid, src_start);

		interval.verify(m_chrom_key, src_id2chrom);
		chain_intervs.push_back(interval);
	}
}

SEXP IntervUtils::convert_chain_intervs(const ChainIntervals &chain_intervs, vector<string> &src_id2chrom)
{
	GIntervals tmp_intervals;
	tmp_intervals.reserve(chain_intervs.size());
	for (ChainIntervals::const_iterator iinterval = chain_intervs.begin(); iinterval != chain_intervs.end(); ++iinterval)
		tmp_intervals.push_back((GInterval)*iinterval);

	SEXP answer = convert_intervs(&tmp_intervals, ChainInterval::NUM_COLS);
	SEXP src_chroms, src_chroms_idx, src_starts;
	SEXP col_names = getAttrib(answer, R_NamesSymbol);
	unsigned num_src_chroms = src_id2chrom.size();

    rprotect(src_chroms_idx = RSaneAllocVector(INTSXP, chain_intervs.size()));
    rprotect(src_starts = RSaneAllocVector(REALSXP, chain_intervs.size()));
    rprotect(src_chroms = RSaneAllocVector(STRSXP, num_src_chroms));

	for (ChainIntervals::const_iterator iinterval = chain_intervs.begin(); iinterval != chain_intervs.end(); ++iinterval) {
		INTEGER(src_chroms_idx)[iinterval - chain_intervs.begin()] = iinterval->chromid_src + 1;
		REAL(src_starts)[iinterval - chain_intervs.begin()] = iinterval->start_src;
	}

	for (unsigned id = 0; id < num_src_chroms; ++id)
		SET_STRING_ELT(src_chroms, id, mkChar(src_id2chrom[id].c_str()));

	for (int i = 0; i < ChainInterval::NUM_COLS; i++)
		SET_STRING_ELT(col_names, i, mkChar(ChainInterval::COL_NAMES[i]));

    setAttrib(src_chroms_idx, R_LevelsSymbol, src_chroms);
    setAttrib(src_chroms_idx, R_ClassSymbol, mkString("factor"));

    SET_VECTOR_ELT(answer, ChainInterval::CHROM_SRC, src_chroms_idx);
    SET_VECTOR_ELT(answer, ChainInterval::START_SRC, src_starts);

	return answer;
}

DiagonalBand IntervUtils::convert_band(SEXP rband)
{
	if (isNull(rband))
		return DiagonalBand();

	if ((!isReal(rband) && !isInteger(rband)) || length(rband) != 2)
		verror("Invalid format of band argument");

	int d1 = isReal(rband) ? (int)(REAL(rband)[0] > 0 ? REAL(rband)[0] + 0.5 : REAL(rband)[0] - 0.5) : INTEGER(rband)[0];
	int d2 = isReal(rband) ? (int)(REAL(rband)[1] > 0 ? REAL(rband)[1] + 0.5 : REAL(rband)[1] - 0.5) : INTEGER(rband)[1];

	if (d1 >= d2)
		verror("Invalid band argument: distance1 exceeds distance2");

	return DiagonalBand(d1, d2);
}

SEXP IntervUtils::create_data_frame(int numrows, int numcols, SEXP attrs_src)
{
	SEXP answer, row_names, col_names;

	rprotect(answer = RSaneAllocVector(VECSXP, numcols));
    rprotect(col_names = RSaneAllocVector(STRSXP, numcols));
    rprotect(row_names = RSaneAllocVector(INTSXP, numrows));

	for (int i = 0; i < numrows; ++i)
		INTEGER(row_names)[i] = i + 1;

    if (attrs_src != R_NilValue) 
        copyMostAttrib(attrs_src, answer);

    setAttrib(answer, R_NamesSymbol, col_names);
    setAttrib(answer, R_ClassSymbol, mkString("data.frame"));
    setAttrib(answer, R_RowNamesSymbol, row_names);

	return answer;
}

void IntervUtils::define_data_frame_cols(SEXP src, vector<SEXP> &src_cols, SEXP tgt, vector<SEXP> &tgt_cols, int tgt_col_offset)
{
	SEXP src_class = getAttrib(src, R_ClassSymbol);

	if (isNull(src_class) || !isString(src_class) ||
		(!(length(src_class) == 1 && !strcmp(CHAR(STRING_ELT(src_class, 0)), "data.frame")) &&
		!(length(src_class) == 3 && !strcmp(CHAR(STRING_ELT(src_class, 0)), "tbl_df")  &&
          !strcmp(CHAR(STRING_ELT(src_class, 1)), "tbl") && !strcmp(CHAR(STRING_ELT(src_class, 2)), "data.frame"))))
		verror("Copied object is not a data frame or tibble");

	if (length(tgt) < length(src) + tgt_col_offset)
		verror("Attempt to copy data frame columns beyond the valid size");

	int numrows = length(getAttrib(tgt, R_RowNamesSymbol));
	SEXP src_colnames = getAttrib(src, R_NamesSymbol);
	SEXP tgt_colnames = getAttrib(tgt, R_NamesSymbol);

	if (isNull(src_colnames) || !isString(src_colnames))
		verror("Invalid source data frame for a copy");

	src_cols.resize(length(src));
	if (tgt_cols.size() < (uint64_t)(length(tgt) + tgt_col_offset)){ 
		tgt_cols.resize(length(tgt) + tgt_col_offset);
	}

	for (int col = 0; col < length(src); ++col) {
		SEXP src_col = VECTOR_ELT(src, col);
		SEXP tgt_col;

        rprotect(tgt_col = RSaneAllocVector(TYPEOF(src_col), numrows));

		if (!isInteger(src_col) && !isReal(src_col) && !isLogical(src_col) && !isString(src_col) && !isFactor(src_col))
			verror("Unsupported type found in a data frame: %s", type2char(TYPEOF(src_col)));

		copyMostAttrib(src_col, tgt_col);
		SET_STRING_ELT(tgt_colnames, col + tgt_col_offset, STRING_ELT(src_colnames, col));
		src_cols[col] = src_col;
		tgt_cols[col + tgt_col_offset] = tgt_col;

        SET_VECTOR_ELT(tgt, col + tgt_col_offset, tgt_col);
    }
}

void IntervUtils::copy_data_frame_row(const vector<SEXP> &src_cols, int src_row, const vector<SEXP> &tgt_cols, int tgt_row, int tgt_col_offset)
{
	for (uint64_t col = 0; col < src_cols.size(); ++col) {
		SEXP src_col = src_cols[col];
		SEXP tgt_col = tgt_cols[col + tgt_col_offset];

		if (isInteger(src_col) || isFactor(src_col))
			INTEGER(tgt_col)[tgt_row] = INTEGER(src_col)[src_row];
		else if (isReal(src_col))
			REAL(tgt_col)[tgt_row] = REAL(src_col)[src_row];
		else if (isLogical(src_col))
			LOGICAL(tgt_col)[tgt_row] = LOGICAL(src_col)[src_row];
		else if (isString(src_col))
			SET_STRING_ELT(tgt_col, tgt_row, mkChar(CHAR(STRING_ELT(src_col, src_row))));
	}
}

void IntervUtils::copy_data_frame_rows(const vector<SEXP> &src_cols, int src_row, int num_rows, const vector<SEXP> &tgt_cols, int tgt_row, int tgt_col_offset)
{
	for (uint64_t col = 0; col < src_cols.size(); ++col) {
		SEXP src_col = src_cols[col];
		SEXP tgt_col = tgt_cols[col + tgt_col_offset];

		if (isInteger(src_col) || isFactor(src_col)) {
			int *src_vals = INTEGER(src_col);
			int *tgt_vals = INTEGER(tgt_col);
			for (int i = 0; i < num_rows; ++i) 
				tgt_vals[tgt_row + i] = src_vals[src_row + i];
		} else if (isReal(src_col)) {
			double *src_vals = REAL(src_col);
			double *tgt_vals = REAL(tgt_col);
			for (int i = 0; i < num_rows; ++i) 
				tgt_vals[tgt_row + i] = src_vals[src_row + i];
		} else if (isLogical(src_col)) {
			int *src_vals = LOGICAL(src_col);
			int *tgt_vals = LOGICAL(tgt_col);
			for (int i = 0; i < num_rows; ++i) 
				tgt_vals[tgt_row + i] = src_vals[src_row + i];
		} else if (isString(src_col)) {
			for (int i = 0; i < num_rows; ++i) 
				SET_STRING_ELT(tgt_col, tgt_row + i, mkChar(CHAR(STRING_ELT(src_col, src_row + i))));
		}
	}
}

void IntervUtils::set_data_frame_val_nan(const vector<SEXP> &tgt_cols, int tgt_row, int tgt_col)
{
	SEXP rtgt_col = tgt_cols[tgt_col];

	if (isInteger(rtgt_col) || isFactor(rtgt_col))
		INTEGER(rtgt_col)[tgt_row] = NA_INTEGER;
	else if (isReal(rtgt_col))
		REAL(rtgt_col)[tgt_row] = NA_REAL;
	else if (isLogical(rtgt_col))
		LOGICAL(rtgt_col)[tgt_row] = NA_LOGICAL;
	else if (isString(rtgt_col))
		SET_STRING_ELT(rtgt_col, tgt_row, NA_STRING);
}

void IntervUtils::restrict_bins(int64_t maxbins, GIntervals &intervals, unsigned binsize) const
{
	for (GIntervals::const_iterator iinterval = intervals.begin(); iinterval != intervals.end(); ++iinterval) {
		int64_t bins = max((int64_t)0, (int64_t)ceil(iinterval->end / binsize) - (int64_t)(iinterval->start / binsize));

		if (bins > maxbins)
			verror("The interval %s [%ld, %ld) covers too wide range of samples that might cause memory allocation failure.\n"
					"(bins covered: %ld, bins limit: %ld)\n", id2chrom(iinterval->chromid).c_str(), iinterval->start, iinterval->end, bins, maxbins);
	}
}

bool IntervUtils::get_multitasking() const
{
	if (m_multitasking < 0) {
		SEXP r_multitasking = GetOption(install("gmultitasking"), R_NilValue);

		if (isLogical(r_multitasking))
			m_multitasking = (int)LOGICAL(r_multitasking)[0];
		else
			m_multitasking = false;
	}
	return (bool)m_multitasking;
}

uint64_t IntervUtils::get_max_processes() const
{
	if (!m_max_processes) {
		SEXP r_max_processes = GetOption(install("gmax.processes"), R_NilValue);

		if (isReal(r_max_processes))
			m_max_processes = (uint64_t)REAL(r_max_processes)[0];
		else if (isInteger(r_max_processes))
			m_max_processes = INTEGER(r_max_processes)[0];
		else
			m_max_processes = 64;
		if (m_max_processes < 1) 
			m_max_processes = 64;
	}
	return m_max_processes;
}

uint64_t IntervUtils::get_max_processes2core() const
{
	if (!m_max_processes2core) {
		SEXP r_max_processes2core = GetOption(install("gmax.processes2core"), R_NilValue);

		if (isReal(r_max_processes2core))
			m_max_processes2core = (uint64_t)REAL(r_max_processes2core)[0];
		else if (isInteger(r_max_processes2core))
			m_max_processes2core = INTEGER(r_max_processes2core)[0];
		else
			m_max_processes2core = 4;
		if (m_max_processes2core < 1) 
			m_max_processes2core = 4;
	}
	return m_max_processes2core;
}

uint64_t IntervUtils::get_min_scope4process() const
{
	if (!m_min_scope4process) {
		SEXP r_min_scope4process = GetOption(install("gmin.scope4process"), R_NilValue);

		if (isReal(r_min_scope4process))
			m_min_scope4process = (uint64_t)REAL(r_min_scope4process)[0];
		else if (isInteger(r_min_scope4process))
			m_min_scope4process = INTEGER(r_min_scope4process)[0];
		else
			m_min_scope4process = 10000;
	}
	return m_min_scope4process;
}

uint64_t IntervUtils::get_max_data_size() const
{
	if (!m_max_data_size) {
		SEXP r_max_data_size = GetOption(install("gmax.data.size"), R_NilValue);

		if (isReal(r_max_data_size))
			m_max_data_size = (uint64_t)REAL(r_max_data_size)[0];
		else if (isInteger(r_max_data_size))
			m_max_data_size = INTEGER(r_max_data_size)[0];
		else
			m_max_data_size = numeric_limits<uint64_t>::max();
	}
	return m_max_data_size;
}

uint64_t IntervUtils::get_max_mem_usage() const
{
	if (!m_max_mem_usage) {
		SEXP r_max_mem_usage = GetOption(install("gmax.mem.usage"), R_NilValue);

		if (isReal(r_max_mem_usage))
			m_max_mem_usage = (uint64_t)REAL(r_max_mem_usage)[0] * 1000;
		else if (isInteger(r_max_mem_usage))
			m_max_mem_usage = INTEGER(r_max_mem_usage)[0] * 1000;
		else
			m_max_mem_usage = numeric_limits<uint64_t>::max();
	}
	return m_max_mem_usage;
}

uint64_t IntervUtils::get_big_intervals_size() const
{
	if (!m_big_intervals_size) {
		SEXP r_big_intervals_size = GetOption(install("gbig.intervals.size"), R_NilValue);

		if (isReal(r_big_intervals_size))
			m_big_intervals_size = (uint64_t)REAL(r_big_intervals_size)[0];
		else if (isInteger(r_big_intervals_size))
			m_big_intervals_size = INTEGER(r_big_intervals_size)[0];
		else
			m_big_intervals_size = numeric_limits<uint64_t>::max();
		m_big_intervals_size = min(m_big_intervals_size, get_max_data_size());
	}
	return m_big_intervals_size;
}

uint64_t IntervUtils::get_quantile_edge_data_size() const
{
	if (!m_quantile_edge_data_size) {
		SEXP r_quantile_edge_data_size = GetOption(install("gquantile.edge.data.size"), R_NilValue);

		if (isReal(r_quantile_edge_data_size))
			m_quantile_edge_data_size = (uint64_t)REAL(r_quantile_edge_data_size)[0];
		else if (isInteger(r_quantile_edge_data_size))
			m_quantile_edge_data_size = INTEGER(r_quantile_edge_data_size)[0];
		else
			m_quantile_edge_data_size = 0;
	}
	return m_quantile_edge_data_size;
}

uint64_t IntervUtils::get_track_chunk_size() const
{
	if (!m_track_chunk_size) {
		SEXP r_track_chunk_size = GetOption(install("gtrack.chunk.size"), R_NilValue);

		if (isReal(r_track_chunk_size))
			m_track_chunk_size = (uint64_t)REAL(r_track_chunk_size)[0];
		else if (isInteger(r_track_chunk_size))
			m_track_chunk_size = INTEGER(r_track_chunk_size)[0];
		else
			m_track_chunk_size = 100000;
	}
	return m_track_chunk_size;
}

uint64_t IntervUtils::get_track_num_chunks() const
{
	if (!m_track_num_chunks) {
		SEXP r_track_num_chunks = GetOption(install("gtrack.num.chunks"), R_NilValue);

		if (isReal(r_track_num_chunks))
			m_track_num_chunks = (uint64_t)REAL(r_track_num_chunks)[0];
		else if (isInteger(r_track_num_chunks))
			m_track_num_chunks = INTEGER(r_track_num_chunks)[0];
		else
			m_track_num_chunks = 0;
	}
	return m_track_num_chunks;
}

bool IntervUtils::is_1d_iterator(SEXP rtrack_exprs, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, SEXP riterator)
{
	TrackExprScanner scanner(*this);
	TrackExpressionIteratorBase *itr = scanner.create_expr_iterator(rtrack_exprs, scope1d, scope2d, riterator, R_NilValue, false);
	return itr->is_1d();
}

void IntervUtils::verify_max_data_size(uint64_t data_size, const char *data_name, bool check_all_kids)
{
	if (data_size > get_max_data_size())
		verror("%s size exceeded the maximal allowed (%ld).\n"
				"Try to bound the scope of the function.\n"
				"Note: the maximum data size is controlled via gmax.data.size option (see options, getOptions).",
			   data_name, get_max_data_size());

	if (check_all_kids) 
		update_res_data_size(data_size);
}

int IntervUtils::prepare4multitasking(SEXP track_exprs, GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d, SEXP iterator_policy, SEXP band)
{
	TrackExprScanner scanner(*this);

	scanner.check(track_exprs, scope1d, scope2d, iterator_policy, band);
	if (scanner.get_iterator()->is_1d()) {
		if (scope2d && dynamic_cast<GIntervals2D *>(scope2d))
			((GIntervals2D *)scope2d)->clear();
	} else {
		if (scope1d && dynamic_cast<GIntervals *>(scope1d)) 
			((GIntervals *)scope1d)->clear();
	}

	return prepare4multitasking(scope1d, scope2d);
}

int IntervUtils::prepare4multitasking(GIntervalsFetcher1D *scope1d, GIntervalsFetcher2D *scope2d)
{
	if (scope1d && !scope1d->size() && scope2d && !scope2d->size()) 
		return 0;

	if ((scope1d && scope1d->size() && scope2d && scope2d->size()) || (!scope1d && !scope2d)) 
		verror("Cannot determine iterator policy");

	int num_cores = max(1, (int)sysconf(_SC_NPROCESSORS_ONLN));
	int max_num_pids = min(get_max_processes2core() * num_cores, get_max_processes());
	int num_chroms = scope1d && scope1d->size() ? scope1d->num_chroms() : scope2d->num_chrom_pairs();

	m_kids_intervals1d.clear();
	m_kids_intervals2d.clear();

	if (scope1d && scope1d->size()) {
		int64_t range = scope1d->range();
		int64_t kid_range = 0;
		int num_avail_kids = min(max_num_pids, num_chroms) - 1;
		int num_remaining_chroms = num_chroms - 1;

		// GIntervals::range(chromid) and GIntervals::create_masked_copy() have complexity of O(n) (and only O(1) for GIntervalsBigSet).
		// Calling these functions for each chromosome might be painful - O(m x n) (m = num chromosomes)
		// So we are forced to write a designated code for GIntervals for the sake of efficiency. The total complexity for GIntervals is O(n).
		if (dynamic_cast<GIntervals *>(scope1d)) {
			GIntervals *intervals = (GIntervals *)scope1d;

			m_kids_intervals1d.push_back(new GIntervals());

			for (GIntervals::const_iterator iinterv = intervals->begin(); iinterv != intervals->end(); ++iinterv) {
				if (iinterv != intervals->begin() && iinterv->chromid != (iinterv - 1)->chromid && num_avail_kids) {
					int64_t chrom_range = 0;
					for (GIntervals::const_iterator iinterv2 = iinterv; iinterv2 != intervals->end(); ++iinterv2) {
						if (iinterv->chromid != iinterv2->chromid) 
							break;
						chrom_range += iinterv2->range();
					}

					// should we allocate a new process?
					if ((uint64_t)kid_range > get_min_scope4process() && (uint64_t)range > get_min_scope4process() && (uint64_t)(kid_range + chrom_range) >= (uint64_t)(range / num_avail_kids)) {
						--num_avail_kids;
						kid_range = 0;
						m_kids_intervals1d.push_back(new GIntervals());
					}

					--num_remaining_chroms;
					num_avail_kids = min(num_avail_kids, num_remaining_chroms);
				}

				int64_t interv_range = iinterv->range();
				kid_range += interv_range;
				range -= interv_range;
				((GIntervals *)m_kids_intervals1d.back())->push_back(*iinterv);
			}
		} else {
			set<int> chromids_mask;
			GIntervals all_genome;
			get_all_genome_intervs(all_genome);

			for (GIntervals::const_iterator iinterv = all_genome.begin(); iinterv != all_genome.end(); ++iinterv) {
				int64_t chrom_range = scope1d->range(iinterv->chromid);

				if (!chrom_range) 
					continue;

				if (kid_range && num_avail_kids) {
					// should we allocate a new process?
					if ((uint64_t)kid_range > get_min_scope4process() && (uint64_t)range > get_min_scope4process() && kid_range + chrom_range >= range / num_avail_kids) {
						--num_avail_kids;
						kid_range = 0;
						m_kids_intervals1d.push_back(scope1d->create_masked_copy(chromids_mask));
						chromids_mask.clear();
					}

					--num_remaining_chroms;
					num_avail_kids = min(num_avail_kids, num_remaining_chroms);
				}

				kid_range += chrom_range;
				range -= chrom_range;
				chromids_mask.insert(iinterv->chromid);
			}

			if (!chromids_mask.empty()) 
				m_kids_intervals1d.push_back(scope1d->create_masked_copy(chromids_mask));
		}

//int idx = 0;
//for (vector<GIntervalsFetcher1D *>::iterator i = m_kids_intervals1d.begin(); i != m_kids_intervals1d.end(); ++i){
//GIntervalsFetcher1D *fetcher = *i;
//REprintf("Kid %d:\n", ++idx);
//GIntervals all_genome;
//get_all_genome_intervs(all_genome);
//
//int64_t total_range = 0;
//for (GIntervals::const_iterator iinterv = all_genome.begin(); iinterv != all_genome.end(); ++iinterv) {
//int64_t chrom_range = fetcher->range(iinterv->chromid);
//if (chrom_range) {
//REprintf("\t%s\trange %ld\n", id2chrom(iinterv->chromid).c_str(), chrom_range);
//total_range += chrom_range;
//}
//}
//REprintf("total range: %ld\n\n", total_range);
//}

	} else if (scope2d && scope2d->size()) {
		double surface = scope2d->surface();
		double kid_surface = 0;
		int num_avail_kids = min(max_num_pids, num_chroms) - 1;
		int num_remaining_chroms = num_chroms - 1;

		// GIntervals2D::surface(chromid1, chromid2) and GIntervals2D::create_masked_copy() have complexity of O(n) (and only O(1) for GIntervalsBigSet2D).
		// Calling these functions for each chromosome might be painful - O(m^2 x n) (m = num chromosomes)
		// So we are forced to write a designated code for GIntervals2D for the sake of efficiency. The total complexity for GIntervals is O(n).
		if (dynamic_cast<GIntervals2D *>(scope2d)) {
			GIntervals2D *intervals = (GIntervals2D *)scope2d;

			m_kids_intervals2d.push_back(new GIntervals2D());

			for (GIntervals2D::const_iterator iinterv = intervals->begin(); iinterv != intervals->end(); ++iinterv) {
				if (iinterv != intervals->begin() && !iinterv->is_same_chrom(*(iinterv - 1)) && num_avail_kids) {
					double chrom_surface = 0;
					for (GIntervals2D::const_iterator iinterv2 = iinterv; iinterv2 != intervals->end(); ++iinterv2) {
						if (!iinterv->is_same_chrom(*iinterv2)) 
							break;
						chrom_surface += iinterv2->surface();
					}

					// should we allocate a new process?
					if (kid_surface > get_min_scope4process() && surface > get_min_scope4process() && kid_surface + chrom_surface >= surface / num_avail_kids) {
						--num_avail_kids;
						kid_surface = 0;
						m_kids_intervals2d.push_back(new GIntervals2D());
					}

					--num_remaining_chroms;
					num_avail_kids = min(num_avail_kids, num_remaining_chroms);
				}

				double interv_surface = iinterv->surface();
				kid_surface += interv_surface;
				surface -= interv_surface;
				((GIntervals2D *)m_kids_intervals2d.back())->push_back(*iinterv);
			}
		} else {
			set<ChromPair> chrompairs_mask;
			GIntervals2D all_genome;
			get_all_genome_intervs(all_genome);

			for (GIntervals2D::const_iterator iinterv = all_genome.begin(); iinterv != all_genome.end(); ++iinterv) {
				double chrom_surface = scope2d->surface(iinterv->chromid1(), iinterv->chromid2());

				if (!chrom_surface) 
					continue;

				if (kid_surface && num_avail_kids) {
					// should we allocate a new process?
					if (kid_surface > get_min_scope4process() && kid_surface > get_min_scope4process() && kid_surface + chrom_surface >= surface / num_avail_kids) {
						--num_avail_kids;
						kid_surface = 0;
						m_kids_intervals2d.push_back(scope2d->create_masked_copy(chrompairs_mask));
						chrompairs_mask.clear();
					}

					--num_remaining_chroms;
					num_avail_kids = min(num_avail_kids, num_remaining_chroms);
				}

				kid_surface += chrom_surface;
				surface -= chrom_surface;
				chrompairs_mask.insert(ChromPair(iinterv->chromid1(), iinterv->chromid2()));
			}

			if (!chrompairs_mask.empty())
				m_kids_intervals2d.push_back(scope2d->create_masked_copy(chrompairs_mask));
		}

//int idx = 0;
//for (vector<GIntervalsFetcher2D *>::iterator i = m_kids_intervals2d.begin(); i != m_kids_intervals2d.end(); ++i){
//GIntervalsFetcher2D *fetcher = *i;
//REprintf("Kid %d:\n", ++idx);
//GIntervals2D all_genome;
//get_all_genome_intervs(all_genome);
//
//double total_surface = 0;
//for (GIntervals2D::const_iterator iinterv = all_genome.begin(); iinterv != all_genome.end(); ++iinterv) {
//double chrom_surface = fetcher->surface(iinterv->chromid1(), iinterv->chromid2());
//if (chrom_surface) {
//total_surface += chrom_surface;
//REprintf("\t%s\t%s\tsurface %g\n", id2chrom(iinterv->chromid1()).c_str(), id2chrom(iinterv->chromid2()).c_str(), chrom_surface);
//}
//}
//REprintf("total_surface: %g\n\n", total_surface);
//}
	}

	m_num_planned_kids = max(m_kids_intervals1d.size(), m_kids_intervals2d.size());
	return m_num_planned_kids;
}

bool IntervUtils::distribute_task(uint64_t res_const_size,    // data size in bytes for all the result
								  uint64_t res_record_size)   // size in bytes per datum in the result
{
	uint64_t max_res_size = get_max_data_size() * res_record_size + m_num_planned_kids * res_const_size;

	rdb::prepare4multitasking(res_const_size, res_record_size, max_res_size, get_max_mem_usage(), m_num_planned_kids);

	for (vector<GIntervalsFetcher1D *>::const_iterator ikid_intervs = m_kids_intervals1d.begin(); ikid_intervs != m_kids_intervals1d.end(); ++ikid_intervs) {
		if (!launch_process())
			return true;
	}

	for (vector<GIntervalsFetcher2D *>::const_iterator ikid_intervs = m_kids_intervals2d.begin(); ikid_intervs != m_kids_intervals2d.end(); ++ikid_intervs) {
		if (!launch_process())
			return true;
	}

	wait_for_kids(*this);
	return false;
}

void ChainIntervals::verify_no_src_overlaps(const GenomeChromKey &chromkey, const vector<string> &src_id2chrom) const
{
	for (const_iterator iinterv = begin() + 1; iinterv < end(); ++iinterv) {
		if (ChainInterval::SrcCompare()(*iinterv, *(iinterv - 1)))
			TGLError<ChainIntervals>(UNSORTED_INTERVALS, "To verify overlaps chain intervals must be sorted by source");

		if (iinterv->chromid_src == (iinterv - 1)->chromid_src && (iinterv - 1)->start_src + (iinterv - 1)->end - (iinterv - 1)->start > iinterv->start_src)
			TGLError<ChainIntervals>(OVERLAPPING_INTERVAL, "Source of chain intervals %s and %s overlap",
					(iinterv - 1)->tostring(chromkey, src_id2chrom).c_str(), iinterv->tostring(chromkey, src_id2chrom).c_str());
	}
}

void ChainIntervals::verify_no_tgt_overlaps(const GenomeChromKey &chromkey, const vector<string> &src_id2chrom) const
{
	for (const_iterator iinterv = begin() + 1; iinterv < end(); ++iinterv) {
		if (*iinterv < *(iinterv - 1))
			TGLError<ChainIntervals>(UNSORTED_INTERVALS, "To verify overlaps chain intervals must be sorted by target");

		if (iinterv->chromid == (iinterv - 1)->chromid && (iinterv - 1)->end > iinterv->start)
			TGLError<ChainIntervals>(OVERLAPPING_INTERVAL, "Target of chain intervals %s and %s overlap",
					(iinterv - 1)->tostring(chromkey, src_id2chrom).c_str(), iinterv->tostring(chromkey, src_id2chrom).c_str());
	}
}

ChainIntervals::const_iterator ChainIntervals::map_interval(const GInterval &src_interval, GIntervals &tgt_intervs, ChainIntervals::const_iterator hint)
{
	tgt_intervs.clear();

//for (auto i = begin(); i < end(); ++i)
//REprintf("chrom %d, coord %ld\n", i->chromid_src, i->start_src);

	if (empty())
		return end();

	if (front().chromid_src > src_interval.chromid || (front().chromid_src == src_interval.chromid && front().start_src >= src_interval.end))
		return begin();

	if (back().chromid_src < src_interval.chromid || (back().chromid_src == src_interval.chromid && back().start_src + back().end - back().start <= src_interval.start))
		return end() - 1;

	if (check_first_overlap_src(hint, src_interval))
		return add2tgt(hint, src_interval, tgt_intervs);

	if (hint + 1 < end() && check_first_overlap_src(hint + 1, src_interval))
		return add2tgt(hint + 1, src_interval, tgt_intervs);

	// run the binary search
	const_iterator istart_interval = begin();
	const_iterator iend_interval = end();

	while (iend_interval - istart_interval > 1) {
		const_iterator imid_interval = istart_interval + (iend_interval - istart_interval) / 2;

		if (check_first_overlap_src(imid_interval, src_interval))
			return add2tgt(imid_interval, src_interval, tgt_intervs);

		// is mid_interval < interval?
		if (imid_interval->chromid_src < src_interval.chromid || (imid_interval->chromid_src == src_interval.chromid && imid_interval->start_src < src_interval.start))
			istart_interval = imid_interval;
		else
			iend_interval = imid_interval;
	}

	return begin();
}

ChainIntervals::const_iterator ChainIntervals::add2tgt(const_iterator hint, const GInterval &src_interval, GIntervals &tgt_intervs)
{
	while (hint != end()) {
		if (hint->do_overlap_src(src_interval)) {
			int64_t common_start = max(hint->start_src, src_interval.start);
			int64_t common_end = min(hint->start_src + hint->end - hint->start, src_interval.end);

			tgt_intervs.push_back(GInterval(hint->chromid, hint->start + common_start - hint->start_src, hint->start + common_end - hint->start_src, 0));
			++hint;
		} else
			break;
	}
	return hint - 1;
}

