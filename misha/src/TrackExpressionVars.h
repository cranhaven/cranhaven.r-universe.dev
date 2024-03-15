/*
 * TrackExpressionVars.h
 *
 *  Created on: Feb 9, 2012
 *      Author: hoichman
 */

#ifndef TRACKEXPRESSIONVARS_H_
#define TRACKEXPRESSIONVARS_H_

#include <cstdint>
#include <vector>
#include <string>
#include <string.h>

#include <R.h>
#include <Rinternals.h>

#include "BinFinder.h"
#include "GenomeTrack.h"
#include "GenomeTrackArrays.h"
#include "GInterval.h"
#include "GInterval2D.h"
#include "TrackExpressionIteratorBase.h"
#include "rdbinterval.h"
#include "rdbutils.h"

using namespace std;

class TrackExpressionVars {
public:
    struct Binned_pv {
   		vector<double> bins;
        BinFinder  binfinder;
    };

    struct Iterator_modifier1D {
        enum Dimension { DIM_NONE, DIM1, DIM2 };

        Dimension dim;
        int64_t   sshift;
        int64_t   eshift;
        GInterval interval;
        bool      out_of_range;

        Iterator_modifier1D() : dim(DIM_NONE), sshift(0), eshift(0), out_of_range(false) {}
        bool operator==(const Iterator_modifier1D &o) const;

        void transform(const GInterval &interv, const GenomeChromKey &chromkey);
        void transform(const GInterval2D &interv, const GenomeChromKey &chromkey);
    };

    typedef vector<Iterator_modifier1D> Iterator_modifiers1D;

    struct Iterator_modifier2D {
        int64_t     sshift1;
        int64_t     eshift1;
        int64_t     sshift2;
        int64_t     eshift2;
        GInterval2D interval;
        bool        out_of_range;

        Iterator_modifier2D() : sshift1(0), eshift1(0), sshift2(0), eshift2(0), out_of_range(false) {}
        bool operator==(const Iterator_modifier2D &o) const;

        void transform(const GInterval2D &interv, const GenomeChromKey &chromkey);
    };

    typedef vector<Iterator_modifier2D> Iterator_modifiers2D;

    struct Track_n_imdf {
        string               name;
        GenomeTrack         *track{NULL};
        GenomeTrack::Type    type;
        vector<unsigned>     slice;
        GenomeTrackArrays::SliceFunctions slice_func;
        double               slice_percentile;
        Iterator_modifier1D *imdf1d{NULL};
        Iterator_modifier2D *imdf2d{NULL};
    };

    typedef vector<Track_n_imdf> Track_n_imdfs;

    struct Track_var {
        enum Val_func { REG, REG_MIN, REG_MAX, REG_NEAREST, STDDEV, SUM, QUANTILE, PV, PV_MIN, PV_MAX, WEIGHTED_SUM, OCCUPIED_AREA, NUM_FUNCS };

        static const char *FUNC_NAMES[NUM_FUNCS];

        string              var_name;
        SEXP                rvar{R_NilValue};
        double             *var;
        Val_func            val_func;
        double              percentile;
        bool                requires_pv;
        Binned_pv           pv_binned;
        Track_n_imdf       *track_n_imdf;
    };

    typedef vector<Track_var> Track_vars;

    struct Interv_var {
        enum Val_func { DIST, DIST_CENTER, NUM_FUNCS };

        static const char *FUNC_NAMES[NUM_FUNCS];

        string                     var_name;
        SEXP                       rvar{R_NilValue};
        double                    *var;
        Iterator_modifier1D       *imdf1d;
        Val_func                   val_func;

        GIntervals                 sintervs;
        GIntervals                 eintervs;
        GIntervals::const_iterator siinterv;
        GIntervals::const_iterator eiinterv;
        double                     dist_margin;
    };

    typedef vector<Interv_var> Interv_vars;

	TrackExpressionVars(rdb::IntervUtils &iu);
	~TrackExpressionVars();

	unsigned get_num_track_vars() const { return m_track_vars.size(); }
	unsigned get_num_interv_vars() const { return m_interv_vars.size(); }

	const string &get_track_name(unsigned ivar) const { return m_track_vars[ivar].track_n_imdf->name; }
	GenomeTrack::Type get_track_type(unsigned ivar) const { return m_track_vars[ivar].track_n_imdf->type; }

	void parse_exprs(const vector<string> &track_exprs);
	void init(const TrackExpressionIteratorBase &expr_itr);
	void define_r_vars(unsigned size);
    const Track_var *var(const char *var_name) const;

	void set_vars(const GInterval &interval, unsigned idx);
	void set_vars(const GInterval2D &interval, const DiagonalBand &band, unsigned idx);

private:
	rdb::IntervUtils       &m_iu;
	string                  m_groot;
	Track_vars              m_track_vars;
	Interv_vars             m_interv_vars;
	Track_n_imdfs           m_track_n_imdfs;
	Iterator_modifiers1D    m_imdfs1d;
	Iterator_modifiers2D    m_imdfs2d;
	GInterval               m_interval1d;
	GInterval2D             m_interval2d;
	DiagonalBand            m_band;

	void                 parse_imdf(SEXP rvtrack, const string &vtrack, Iterator_modifier1D *imdf1d, Iterator_modifier2D *imdf2d);
	Iterator_modifier1D *add_imdf(const Iterator_modifier1D &imdf1d);
	Iterator_modifier2D *add_imdf(const Iterator_modifier2D &imdf2d);
	Track_n_imdf        &add_track_n_imdf(const string &track, GenomeTrack::Type track_type,
										  const vector<unsigned> &slice, GenomeTrackArrays::SliceFunctions slice_func, double slice_percentile,
										  const Iterator_modifier1D &imdf1d, const Iterator_modifier2D &imdf2d);
	Track_var           &add_track_var(const string &track);
	void                 add_vtrack_var(const string &track, SEXP rvtrack);
	Track_var           &add_vtrack_var_src_track(SEXP rvtrack, const string &vtrack, const string &track);
	Interv_var          &add_vtrack_var_src_interv(SEXP rvtrack, const string &vtrack, GIntervals &intervs1d, GIntervals2D &intervs2d);
	void                 register_track_functions();

	void start_chrom(const GInterval &interval);
	void start_chrom(const GInterval2D &interval);
	void set_vars(unsigned idx);

	bool is_var(const string &str, uint64_t start, uint64_t end) const { return (!start || !rdb::is_R_var_char(str[start - 1])) && (end == str.size() || !rdb::is_R_var_char(str[end])); }
};


// -------------------------------------------------- IMPLEMENTATION -------------------------------------------------------

inline bool TrackExpressionVars::Iterator_modifier1D::operator==(const Iterator_modifier1D &o) const
{
	return dim == o.dim && sshift == o.sshift && eshift == o.eshift;
}

inline void TrackExpressionVars::Iterator_modifier1D::transform(const GInterval &interv, const GenomeChromKey &chromkey)
{
	interval.chromid = interv.chromid;
	interval.start = max(interv.start + sshift, (decltype(interv.start + sshift))0);
	interval.end = min(interv.end + eshift, (int64_t)chromkey.get_chrom_size(interv.chromid));
	interval.strand = interv.strand;
	out_of_range = interval.start >= interval.end;
}

inline void TrackExpressionVars::Iterator_modifier1D::transform(const GInterval2D &interv, const GenomeChromKey &chromkey)
{
	return dim == DIM1 ?
			transform(GInterval(interv.chromid1(), interv.start1(), interv.end1(), 0), chromkey) :
				transform(GInterval(interv.chromid2(), interv.start2(), interv.end2(), 0), chromkey);
}

inline bool TrackExpressionVars::Iterator_modifier2D::operator==(const Iterator_modifier2D &o) const
{
	return sshift1 == o.sshift1 && eshift1 == o.eshift1 && sshift2 == o.sshift2 && eshift2 == o.eshift2;
}

inline void TrackExpressionVars::Iterator_modifier2D::transform(const GInterval2D &interv, const GenomeChromKey &chromkey)
{
	int64_t start1 = max(interv.start1() + sshift1, (decltype(interv.start1() + sshift1))0);
	int64_t end1 = min(interv.end1() + eshift1, (int64_t)chromkey.get_chrom_size(interv.chromid1()));
	int64_t start2 = max(interv.start2() + sshift2, (decltype(interv.start2() + sshift2))0);
	int64_t end2 = min(interv.end2() + eshift2, (int64_t)chromkey.get_chrom_size(interv.chromid2()));
	interval.set(interv.chromid1(), start1, end1, interv.chromid2(), start2, end2);
	out_of_range = start1 >= end1 || start2 >= end2;
}

inline const TrackExpressionVars::Track_var *TrackExpressionVars::var(const char *var_name) const
{
    for (Track_vars::const_iterator ivar = m_track_vars.begin(); ivar != m_track_vars.end(); ivar++) {
        if (ivar->var_name == var_name)
            return &*ivar;
    }
    return NULL;
}

#endif /* TRACKEXPRESSIONVARS_H_ */
