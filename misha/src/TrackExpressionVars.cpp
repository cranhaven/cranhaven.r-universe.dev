#include <cstdint>
#include <cmath>
#include <unistd.h>

#include "rdbutils.h"

#include <R.h>
#include <Rinternals.h>

#include "GenomeTrack.h"
#include "GenomeTrackArrays.h"
#include "GenomeTrackComputed.h"
#include "GenomeTrackFixedBin.h"
#include "GenomeTrackRects.h"
#include "GenomeTrackSparse.h"
#include "TrackExpressionVars.h"

const char *TrackExpressionVars::Track_var::FUNC_NAMES[TrackExpressionVars::Track_var::NUM_FUNCS] = {
		"avg", "min", "max", "nearest", "stddev", "sum", "quantile", "global.percentile", "global.percentile.min", "global.percentile.max", "weighted.sum", "area"
};
const char *TrackExpressionVars::Interv_var::FUNC_NAMES[TrackExpressionVars::Interv_var::NUM_FUNCS] = { "distance", "distance.center" };

using namespace rdb;


TrackExpressionVars::TrackExpressionVars(rdb::IntervUtils &iu) :
	m_iu(iu)
{
	m_imdfs1d.reserve(10000);
	m_imdfs2d.reserve(10000);
	m_track_n_imdfs.reserve(10000);
	m_groot = get_groot(m_iu.get_env());
}

TrackExpressionVars::~TrackExpressionVars()
{
	for (Track_vars::iterator ivar = m_track_vars.begin(); ivar != m_track_vars.end(); ivar++)
		runprotect(ivar->rvar);
	for (Interv_vars::iterator ivar = m_interv_vars.begin(); ivar != m_interv_vars.end(); ivar++)
		runprotect(ivar->rvar);
	for (Track_n_imdfs::iterator itrack_n_imdf = m_track_n_imdfs.begin(); itrack_n_imdf != m_track_n_imdfs.end(); ++itrack_n_imdf)
		delete itrack_n_imdf->track;
}

void TrackExpressionVars::parse_exprs(const vector<string> &track_exprs)
{
	enum { TRACK, VTRACK, NUM_VAR_TYPES };

	SEXP rtracknames[NUM_VAR_TYPES] = { R_NilValue, R_NilValue };
	SEXP gvtracks = R_NilValue;
	SEXP vtracks = R_NilValue;

	// retrieve track names
	rprotect(rtracknames[TRACK] = findVar(install("GTRACKS"), findVar(install(".misha"), m_iu.get_env())));
	SEXPCleaner rtracknames_track_cleaner(rtracknames[TRACK]);

	// retrieve virtual track names (it's more complex since virtual track names are burried in a list of lists)
	rtracknames[VTRACK] = R_NilValue;
	rprotect(gvtracks = findVar(install("GVTRACKS"), findVar(install(".misha"), m_iu.get_env())));
	SEXPCleaner gvtracks_cleaner(gvtracks);

	if (!isNull(gvtracks) && !isSymbol(gvtracks)) { 
		SEXP gwds = getAttrib(gvtracks, R_NamesSymbol);

		if (!isVector(gvtracks) || (length(gvtracks) && !isString(gwds)) || length(gwds) != length(gvtracks))
			verror("Invalid format of GVTRACKS variable.\n"
				"To continue working with virtual tracks please remove this variable from the environment.");

		const char *gwd = get_gwd(m_iu.get_env());

		for (int i = 0; i < length(gwds); ++i) {
			if (!strcmp(gwd, CHAR(STRING_ELT(gwds, i)))) {
				vtracks = VECTOR_ELT(gvtracks, i);
				SEXP vtracknames = getAttrib(vtracks, R_NamesSymbol);

				if (!isVector(vtracks) || (length(vtracks) && !isString(vtracknames)) || length(vtracknames) != length(vtracks))
					verror("Invalid format of GVTRACKS variable.\n"
						"To continue working with virtual tracks please remove this variable from the environment.");

				rtracknames[VTRACK] = vtracknames;
			}
		}
	}

	for (vector<string>::const_iterator iexpr = track_exprs.begin(); iexpr != track_exprs.end(); ++iexpr) {
		for (int var_type = 0; var_type < NUM_VAR_TYPES; ++var_type) {
			if (!isString(rtracknames[var_type]))
				continue;

			for (int itrack = 0; itrack < length(rtracknames[var_type]); ++itrack) {
				string track = CHAR(STRING_ELT(rtracknames[var_type], itrack));
				uint64_t pos = 0;

				while ((pos = iexpr->find(track, pos)) != string::npos) {
					if (is_var(*iexpr, pos, pos + track.size())) {
						if (var_type == TRACK)
							add_track_var(track);
						else
							add_vtrack_var(track, VECTOR_ELT(vtracks, itrack));
						break;
					}
					pos += track.size();
				}
			}
		}
	}

	for (Interv_vars::iterator ivar = m_interv_vars.begin(); ivar != m_interv_vars.end(); ++ivar) {
		ivar->siinterv = ivar->sintervs.begin();
		if (ivar->val_func == Interv_var::DIST)
			ivar->eiinterv = ivar->eintervs.begin();
	}
}

void TrackExpressionVars::parse_imdf(SEXP rvtrack, const string &vtrack, Iterator_modifier1D *imdf1d, Iterator_modifier2D *imdf2d)
{
	SEXP rimdf = get_rvector_col(rvtrack, "itr", vtrack.c_str(), false);

	if (isNull(rimdf))
		return;

	string vname = vtrack + "$itr";

	SEXP rtype = get_rvector_col(rimdf, "type", vname.c_str(), true);

	if (!isString(rtype) || length(rtype) != 1)
		verror("Invalid format of virtual track %s", vtrack.c_str());

	string type(CHAR(STRING_ELT(rtype, 0)));
	transform(type.begin(), type.end(), type.begin(), ::tolower);

	if (type == "1d") {
		if (!imdf1d)
			verror("Virtual track %s: 1D iterator modifier cannot be used with source that supports only 2D iterators", vtrack.c_str());

		SEXP rdim = get_rvector_col(rimdf, "dim", vname.c_str(), false);

		if (isNull(rdim))
			imdf1d->dim = Iterator_modifier1D::DIM_NONE;
		else {
			if (!(isReal(rdim) || isInteger(rdim)) || length(rdim) != 1)
				verror("Virtual track %s: invalid dimension projection of iterator modifier", vtrack.c_str());

			double dim = isReal(rdim) ? REAL(rdim)[0] : INTEGER(rdim)[0];

			if (!dim)
				imdf1d->dim = Iterator_modifier1D::DIM_NONE;
			else if (dim == 1)
				imdf1d->dim = Iterator_modifier1D::DIM1;
			else if (dim == 2)
				imdf1d->dim = Iterator_modifier1D::DIM2;
			else
				verror("Virtual track %s: invalid dimension projection of iterator modifier", vtrack.c_str());
		}

		enum { SSHIFT, ESHIFT, NUM_SHIFTS };
		static const char *shift_col_names[NUM_SHIFTS] = { "sshift", "eshift" };

		for (int i = 0; i < NUM_SHIFTS; i++) {
			SEXP rshift = get_rvector_col(rimdf, shift_col_names[i], vname.c_str(), false);

			if (!(isReal(rshift) || isInteger(rshift)) || length(rshift) != 1)
				verror("Virtual track %s: %s must be an integer", vtrack.c_str(), shift_col_names[i]);

			int64_t shift = isReal(rshift) ? (int64_t)REAL(rshift)[0] : INTEGER(rshift)[0];

			if (i == SSHIFT)
				imdf1d->sshift = shift;
			else if (i == ESHIFT)
				imdf1d->eshift = shift;
		}
	} else if (type == "2d") {
		if (!imdf2d)
			verror("Virtual track %s: 2D iterator modifier cannot be used with source that supports only 1D iterators", vtrack.c_str());

		enum { SSHIFT1, ESHIFT1, SSHIFT2, ESHIFT2, NUM_SHIFTS };
		static const char *shift_col_names[NUM_SHIFTS] = { "sshift1", "eshift1", "sshift2", "eshift2" };

		for (int i = 0; i < NUM_SHIFTS; i++) {
			SEXP rshift = get_rvector_col(rimdf, shift_col_names[i], vname.c_str(), false);

			if (!(isReal(rshift) || isInteger(rshift)) || length(rshift) != 1)
				verror("Virtual track %s: %s must be an integer", vtrack.c_str(), shift_col_names[i]);

			int64_t shift = isReal(rshift) ? (int64_t)REAL(rshift)[0] : INTEGER(rshift)[0];

			if (i == SSHIFT1)
				imdf2d->sshift1 = shift;
			else if (i == ESHIFT1)
				imdf2d->eshift1 = shift;
			else if (i == SSHIFT2)
				imdf2d->sshift2 = shift;
			else if (i == ESHIFT2)
				imdf2d->eshift2 = shift;
		}
	} else
		verror("Virtual track %s: invalid type of iterator modifier", vtrack.c_str());
}

TrackExpressionVars::Iterator_modifier1D *TrackExpressionVars::add_imdf(const Iterator_modifier1D &imdf)
{
	if (imdf == Iterator_modifier1D())
		return NULL;

	for (Iterator_modifiers1D::iterator iimdf = m_imdfs1d.begin(); iimdf != m_imdfs1d.end(); ++iimdf) {
		if (*iimdf == imdf)
			return &*iimdf;
	}

	if (m_imdfs1d.size() == m_imdfs1d.capacity())
		verror("Reached the limit of maximal number of tracks");

	m_imdfs1d.push_back(imdf);
	return &m_imdfs1d.back();
}

TrackExpressionVars::Iterator_modifier2D *TrackExpressionVars::add_imdf(const Iterator_modifier2D &imdf)
{
	if (imdf == Iterator_modifier2D())
		return NULL;

	for (Iterator_modifiers2D::iterator iimdf = m_imdfs2d.begin(); iimdf != m_imdfs2d.end(); ++iimdf) {
		if (*iimdf == imdf)
			return &*iimdf;
	}

	if (m_imdfs2d.size() == m_imdfs2d.capacity())
		verror("Reached the limit of maximal number of tracks");

	m_imdfs2d.push_back(imdf);
	return &m_imdfs2d.back();
}

TrackExpressionVars::Track_n_imdf &TrackExpressionVars::add_track_n_imdf(const string &track, GenomeTrack::Type track_type,
																		 const vector<unsigned> &slice, GenomeTrackArrays::SliceFunctions slice_func, double slice_percentile,
																		 const Iterator_modifier1D &imdf1d, const Iterator_modifier2D &imdf2d)
{
	Iterator_modifier1D *pimdf1d = add_imdf(imdf1d);
	Iterator_modifier2D *pimdf2d = add_imdf(imdf2d);

	for (Track_n_imdfs::iterator itrack_n_imdf = m_track_n_imdfs.begin(); itrack_n_imdf != m_track_n_imdfs.end(); ++itrack_n_imdf) {
		if (itrack_n_imdf->name == track &&
			itrack_n_imdf->slice == slice && itrack_n_imdf->slice_func == slice_func && itrack_n_imdf->slice_percentile == slice_percentile &&
			itrack_n_imdf->imdf1d == pimdf1d && itrack_n_imdf->imdf2d == pimdf2d)
			return *itrack_n_imdf;
	}

	if (m_track_n_imdfs.size() == m_track_n_imdfs.capacity())
		verror("Reached the limit of maximal number of tracks");

	m_track_n_imdfs.push_back(Track_n_imdf());
	Track_n_imdf &track_n_imdf = m_track_n_imdfs.back();
	track_n_imdf.name = track;
	track_n_imdf.track = NULL;
	track_n_imdf.type = track_type;
	track_n_imdf.slice = slice;
	track_n_imdf.slice_func = slice_func;
	track_n_imdf.slice_percentile = slice_percentile;
	track_n_imdf.imdf1d = pimdf1d;
	track_n_imdf.imdf2d = pimdf2d;
	return track_n_imdf;
}

void TrackExpressionVars::add_vtrack_var(const string &vtrack, SEXP rvtrack)
{
	for (Track_vars::const_iterator ivar = m_track_vars.begin(); ivar != m_track_vars.end(); ++ivar) {
		if (ivar->var_name == vtrack)
			return;
	}

	for (Interv_vars::const_iterator ivar = m_interv_vars.begin(); ivar != m_interv_vars.end(); ++ivar) {
		if (ivar->var_name == vtrack)
			return;
	}

	SEXP rsrc = get_rvector_col(rvtrack, "src", vtrack.c_str(), true);

	if (isString(rsrc) && length(rsrc) == 1) {
		string track(CHAR(STRING_ELT(rsrc, 0)));

		SEXP gtracks = findVar(install("GTRACKS"), findVar(install(".misha"), m_iu.get_env()));
		if (isString(gtracks)) {
			for (int itrack = 0; itrack < length(gtracks); itrack++) {
				if (!strcmp(CHAR(STRING_ELT(gtracks, itrack)), track.c_str())) {
					add_vtrack_var_src_track(rvtrack, vtrack, track);
					return;
				}
			}
		}
        }

	GIntervals intervs1d;
	GIntervals2D intervs2d;

	try {
		m_iu.convert_rintervs(rsrc, &intervs1d, &intervs2d);
	} catch (TGLException &e) {
		verror("Source of virtual track %s was not recognized neither as a track nor as intervals", vtrack.c_str());
	}
	add_vtrack_var_src_interv(rvtrack, vtrack, intervs1d, intervs2d);
}

TrackExpressionVars::Track_var &TrackExpressionVars::add_track_var(const string &track)
{
	for (Track_vars::iterator ivar = m_track_vars.begin(); ivar != m_track_vars.end(); ++ivar) {
		if (ivar->var_name == track)
			return *ivar;
	}

	GenomeTrack::Type track_type = GenomeTrack::get_type(track2path(m_iu.get_env(), track).c_str(), m_iu.get_chromkey());
	Track_n_imdf &track_n_imdf = add_track_n_imdf(track, track_type, vector<unsigned>(), GenomeTrackArrays::S_AVG, 0, Iterator_modifier1D(), Iterator_modifier2D());

	m_track_vars.push_back(Track_var());
	Track_var &var = m_track_vars.back();
	var.var_name = track;
	var.val_func = Track_var::REG;
	var.percentile = numeric_limits<double>::quiet_NaN();
	var.requires_pv = false;
	var.track_n_imdf = &track_n_imdf;
	return var;
}

TrackExpressionVars::Track_var &TrackExpressionVars::add_vtrack_var_src_track(SEXP rvtrack, const string &vtrack, const string &track)
{
	GenomeTrack::Type track_type = GenomeTrack::get_type(track2path(m_iu.get_env(), track).c_str(), m_iu.get_chromkey());
	Iterator_modifier1D imdf1d;
	Iterator_modifier2D imdf2d;

	if (GenomeTrack::is_1d(track_type))
		parse_imdf(rvtrack, vtrack, &imdf1d, NULL);
	else
		parse_imdf(rvtrack, vtrack, NULL, &imdf2d);

	SEXP rslice = get_rvector_col(rvtrack, "slice", vtrack.c_str(), false);
	vector<unsigned> slice;
	GenomeTrackArrays::SliceFunctions slice_func = GenomeTrackArrays::S_AVG;
	double slice_percentile = 0;

	if (!isNull(rslice)) {
		if (track_type != GenomeTrack::ARRAYS)
			verror("Slices are not supported by %s tracks", GenomeTrack::TYPE_NAMES[track_type]);

		SEXP rslice_func = get_rvector_col(rslice, "func", vtrack.c_str(), false);

    	if (!isNull(rslice_func)) {
    		string slice_func_str;

    		if (track_type != GenomeTrack::ARRAYS)
    			verror("Slices are not supported by %s tracks", GenomeTrack::TYPE_NAMES[track_type]);

    		if (!isString(rslice_func))
    			verror("slice function argument must be a string");

    		SEXP rslice_params = get_rvector_col(rslice, "params", vtrack.c_str(), false);

    		slice_func_str = CHAR(STRING_ELT(rslice_func, 0));
    		transform(slice_func_str.begin(), slice_func_str.end(), slice_func_str.begin(), ::tolower);

    		int ifunc;
    		for (ifunc = 0; ifunc < GenomeTrackArrays::NUM_S_FUNCS; ++ifunc) {
    			if (!strcmp(slice_func_str.c_str(), GenomeTrackArrays::SLICE_FUNCTION_NAMES[ifunc])) {
    				slice_func = (GenomeTrackArrays::SliceFunctions)ifunc;
					if (slice_func == GenomeTrackArrays::S_QUANTILE) {
						if (isNull(rslice_params))
							verror("Virtual track %s: slice function %s requires an additional parameter (percentile) to be specified", vtrack.c_str(), slice_func_str.c_str());
						if (!isReal(rslice_params) || length(rslice_params) != 1)
							verror("Virtual track %s: invalid parameters used for function %s", vtrack.c_str(), slice_func_str.c_str());
						slice_percentile = REAL(rslice_params)[0];
						if (slice_percentile < 0 || slice_percentile > 1)
							verror("Virtual track %s: parameter (percentile) used for function %s is out of range", vtrack.c_str(), slice_func_str.c_str());
					} else if (!isNull(rslice_params))
    					verror("Virtual track %s: slice function %s does not accept any parameters", vtrack.c_str(), slice_func_str.c_str());
    				break;
    			}
    		}

    		if (ifunc >= GenomeTrackArrays::NUM_S_FUNCS)
    			verror("Virtual track %s: invalid function %s used with a track", vtrack.c_str(), slice_func_str.c_str());
    	}

		SEXP rslice_idx = get_rvector_col(rslice, "slice", vtrack.c_str(), false);
		if (!isNull(rslice_idx)) {
			if (!isReal(rslice_idx) && !isInteger(rslice_idx)) 
				verror("Virtual track %s: invalid slice parameters", vtrack.c_str());

			for (int i = 0; i < length(rslice_idx); ++i) {
				double idx = isReal(rslice_idx) ? REAL(rslice_idx)[i] : INTEGER(rslice_idx)[i];
				if (idx < 1 || idx != (double)(int)idx)
					verror("Virtual track %s: slice indices must be positive integers", vtrack.c_str());
				slice.push_back((unsigned)(idx - 1));
			}

			sort(slice.begin(), slice.end());
			vector<unsigned>::iterator new_end = unique(slice.begin(), slice.end());
			slice.resize(new_end - slice.begin());
		}
	}

	Track_n_imdf &track_n_imdf = add_track_n_imdf(track, track_type, slice, slice_func, slice_percentile, imdf1d, imdf2d);

	m_track_vars.push_back(Track_var());
	Track_var &var = m_track_vars.back();
	var.var_name = vtrack;
	var.track_n_imdf = &track_n_imdf;

	SEXP rfunc = get_rvector_col(rvtrack, "func", vtrack.c_str(), false);
	SEXP rparams = get_rvector_col(rvtrack, "params", vtrack.c_str(), false);
	string func;

	if (isNull(rfunc))
		func = Track_var::FUNC_NAMES[Track_var::REG];
	else {
		if (!isString(rfunc))
			verror("Function argument must be a string");

		func = CHAR(STRING_ELT(rfunc, 0));
		transform(func.begin(), func.end(), func.begin(), ::tolower);
	}

	int ifunc;
	for (ifunc = 0; ifunc < Track_var::NUM_FUNCS; ++ifunc) {
		if (!strcmp(func.c_str(), Track_var::FUNC_NAMES[ifunc])) {
			if ((GenomeTrack::is_1d(track_type) && (ifunc == Track_var::WEIGHTED_SUM || ifunc == Track_var::OCCUPIED_AREA)) ||
					(GenomeTrack::is_2d(track_type) && (ifunc == Track_var::REG_NEAREST || ifunc == Track_var::STDDEV || ifunc == Track_var::SUM || ifunc == Track_var::QUANTILE)) ||
					(track_type != GenomeTrack::FIXED_BIN && (ifunc == Track_var::PV || ifunc == Track_var::PV_MIN || ifunc == Track_var::PV_MAX)))
				verror("Virtual track %s: function %s is not supported by %s tracks", vtrack.c_str(), func.c_str(), GenomeTrack::TYPE_NAMES[track_type]);

			if (ifunc == Track_var::QUANTILE) {
				if (isNull(rparams))
					verror("Virtual track %s: function %s requires an additional parameter (percentile) to be specified", vtrack.c_str(), func.c_str());
				if (!isReal(rparams) || length(rparams) != 1)
					verror("Virtual track %s: invalid parameters used for function %s", vtrack.c_str(), func.c_str());
				var.percentile = REAL(rparams)[0];
				if (var.percentile < 0 || var.percentile > 1)
					verror("Virtual track %s: parameter (percentile) used for function %s is out of range", vtrack.c_str(), func.c_str());
			} else {
				var.percentile = numeric_limits<double>::quiet_NaN();
				if (!isNull(rparams))
					verror("Virtual track %s: function %s does not accept any parameters", vtrack.c_str(), func.c_str());
			}

			var.val_func = (Track_var::Val_func)ifunc;
			var.requires_pv = ifunc == Track_var::PV || ifunc == Track_var::PV_MIN || ifunc == Track_var::PV_MAX;
			break;
		}
	}

	if (ifunc >= Track_var::NUM_FUNCS)
        verror("Virtual track %s: invalid function %s used for a virtual track", vtrack.c_str(), func.c_str());

	return var;
}

TrackExpressionVars::Interv_var &TrackExpressionVars::add_vtrack_var_src_interv(SEXP rvtrack, const string &vtrack, GIntervals &intervs1d, GIntervals2D &intervs2d)
{
	if (intervs1d.empty() && !intervs2d.empty())
		verror("Virtual track %s: virtual tracks do not support 2D intervals as a source", vtrack.c_str());

	m_interv_vars.push_back(Interv_var());
	Interv_var &var = m_interv_vars.back();

	var.var_name = vtrack;
	Iterator_modifier1D imdf1d;
	parse_imdf(rvtrack, vtrack, &imdf1d, NULL);
	var.imdf1d = add_imdf(imdf1d);

	SEXP rfunc = get_rvector_col(rvtrack, "func", vtrack.c_str(), false);
	SEXP rparams = get_rvector_col(rvtrack, "params", vtrack.c_str(), false);
	string func;

	if (isNull(rfunc))
		func = Interv_var::FUNC_NAMES[Interv_var::DIST];
	else {
		func = CHAR(STRING_ELT(rfunc, 0));
		transform(func.begin(), func.end(), func.begin(), ::tolower);
	}

	if (!strcmp(func.c_str(), Interv_var::FUNC_NAMES[Interv_var::DIST])) {
		var.val_func = Interv_var::DIST;

		double dist_margin = 0;

		if (!isNull(rparams)) {
			if (isReal(rparams) && length(rparams) == 1)
				dist_margin = REAL(rparams)[0];
			else if (isInteger(rparams) && length(rparams) == 1)
				dist_margin = INTEGER(rparams)[0];
			else
				verror("Virtual track %s: invalid parameters used for function %s", vtrack.c_str(), func.c_str());
		}
		if (dist_margin < 0)
			verror("Virtual track %s, function %s: margin cannot be a negative number", vtrack.c_str(), func.c_str());

		var.dist_margin = dist_margin * 0.5;
		var.sintervs.swap(intervs1d);
		var.sintervs.sort();
		var.eintervs = var.sintervs;
		var.eintervs.sort(GIntervals::compare_by_end_coord);
		// cannot not set var.siinterv and var.eiinterv now because these iterators will be invalidated one a new var is added to m_interv_vars

		if (var.dist_margin) {
			// if dist_margin is not zero => we are measuring distances from the centers of the interval, therefore the intervals cannot overlap
			for (GIntervals::const_iterator iinterv = var.sintervs.begin() + 1; iinterv < var.sintervs.end(); ++iinterv) {
				if (iinterv->do_touch(*(iinterv - 1)))
					verror("Virtual track %s: intervals are overlapping and hence incompatible with %s function having non-zero (%g) margin", vtrack.c_str(), func.c_str(), var.dist_margin);
			}
		}
	} else if (!strcmp(func.c_str(), Interv_var::FUNC_NAMES[Interv_var::DIST_CENTER])) {
		var.val_func = Interv_var::DIST_CENTER;

		if (!isNull(rparams))
			verror("Virtual track %s: function %s does not accept any parameters", vtrack.c_str(), func.c_str());

		var.dist_margin = 0.;
		var.sintervs.swap(intervs1d);
		var.sintervs.sort();
		// cannot not set var.siinterv and var.eiinterv now because these iterators will be invalidated one a new var is added to m_interv_vars

		for (GIntervals::const_iterator iinterv = var.sintervs.begin() + 1; iinterv < var.sintervs.end(); ++iinterv) {
			if (iinterv->do_touch(*(iinterv - 1)))
				verror("Virtual track %s: intervals are overlapping and hence incompatible with %s function", vtrack.c_str(), func.c_str());
		}
	} else
		verror("Virtual track %s: invalid function %s used with intervals", vtrack.c_str(), func.c_str());

	return var;
}

void TrackExpressionVars::register_track_functions()
{
	for (Track_vars::iterator ivar = m_track_vars.begin(); ivar != m_track_vars.end(); ++ivar) {
		GenomeTrack1D *track1d = GenomeTrack::is_1d(ivar->track_n_imdf->type) ? (GenomeTrack1D *)ivar->track_n_imdf->track : NULL;
		GenomeTrack2D *track2d = GenomeTrack::is_2d(ivar->track_n_imdf->type) ? (GenomeTrack2D *)ivar->track_n_imdf->track : NULL;

		switch (ivar->val_func) {
		case Track_var::REG:
		case Track_var::PV:
			if (track1d)
				track1d->register_function(GenomeTrack1D::AVG);
			else
				track2d->register_function(GenomeTrack2D::AVG);
			break;
		case Track_var::REG_MIN:
		case Track_var::PV_MIN:
			if (track1d)
				track1d->register_function(GenomeTrack1D::MIN);
			else
				track2d->register_function(GenomeTrack2D::MIN);
			break;
		case Track_var::REG_MAX:
		case Track_var::PV_MAX:
			if (track1d)
				track1d->register_function(GenomeTrack1D::MAX);
			else
				track2d->register_function(GenomeTrack2D::MAX);
			break;
		case Track_var::REG_NEAREST:
			track1d->register_function(GenomeTrack1D::NEAREST);
			break;
		case Track_var::STDDEV:
			track1d->register_function(GenomeTrack1D::STDDEV);
			break;
		case Track_var::SUM:
			track1d->register_function(GenomeTrack1D::SUM);
			break;
		case Track_var::QUANTILE:
			track1d->register_quantile(m_iu.get_max_data_size(), m_iu.get_quantile_edge_data_size(), m_iu.get_quantile_edge_data_size());
			break;
		case Track_var::WEIGHTED_SUM:
			track2d->register_function(GenomeTrack2D::WEIGHTED_SUM);
			break;
		case Track_var::OCCUPIED_AREA:
			track2d->register_function(GenomeTrack2D::OCCUPIED_AREA);
			break;
		default:
			verror("Unrecognized virtual track function");
		}

		if (ivar->track_n_imdf->type == GenomeTrack::ARRAYS) {
			GenomeTrackArrays *track = (GenomeTrackArrays *)ivar->track_n_imdf->track;
			if (ivar->track_n_imdf->slice_func == GenomeTrackArrays::S_QUANTILE) 
				track->set_slice_quantile(ivar->track_n_imdf->slice_percentile, m_iu.get_max_data_size(), m_iu.get_quantile_edge_data_size(), m_iu.get_quantile_edge_data_size(),
										  ivar->track_n_imdf->slice);
			else 
				track->set_slice_function(ivar->track_n_imdf->slice_func, ivar->track_n_imdf->slice);
		}
	}
}

void TrackExpressionVars::init(const TrackExpressionIteratorBase &expr_itr)
{
	for (Track_vars::const_iterator itrack_var = m_track_vars.begin(); itrack_var != m_track_vars.end(); ++itrack_var) {
		if (expr_itr.is_1d()) {
			if (GenomeTrack::is_1d(itrack_var->track_n_imdf->type) && itrack_var->track_n_imdf->imdf1d && itrack_var->track_n_imdf->imdf1d->dim != Iterator_modifier1D::DIM_NONE)
				verror("Virtual track %s: 1D iterator is used for a virtual track that specifies dimension projection and hence expects 2D iterators", itrack_var->var_name.c_str());
			if (GenomeTrack::is_2d(itrack_var->track_n_imdf->type)) {
				if (itrack_var->var_name == itrack_var->track_n_imdf->name)
					verror("1D iterator is applied to a 2D track %s", itrack_var->track_n_imdf->name.c_str());
				verror("Virtual track %s: 1D iterator is applied to a 2D track %s", itrack_var->var_name.c_str(), itrack_var->track_n_imdf->name.c_str());
			}
		} else if (expr_itr.is_2d() && GenomeTrack::is_1d(itrack_var->track_n_imdf->type) && (!itrack_var->track_n_imdf->imdf1d || itrack_var->track_n_imdf->imdf1d->dim == Iterator_modifier1D::DIM_NONE)) {
			if (itrack_var->var_name == itrack_var->track_n_imdf->name)
				verror("2D iterator is applied to a 1D track %s without explicit dimension projection", itrack_var->track_n_imdf->name.c_str());
			verror("Virtual track %s: 2D iterator is applied to a 1D track %s without explicit dimension projection", itrack_var->var_name.c_str(), itrack_var->track_n_imdf->name.c_str());
		}
	}

	for (Interv_vars::const_iterator iinterv_var = m_interv_vars.begin(); iinterv_var != m_interv_vars.end(); ++iinterv_var) {
		if (expr_itr.is_1d() && iinterv_var->imdf1d && iinterv_var->imdf1d->dim != Iterator_modifier1D::DIM_NONE)
			verror("Virtual track %s: 1D iterator is used for a virtual track that specifies dimension projection and hence expects 2D iterators", iinterv_var->var_name.c_str());
		else if (expr_itr.is_2d() && (!iinterv_var->imdf1d || iinterv_var->imdf1d->dim == Iterator_modifier1D::DIM_NONE))
			verror("Virtual track %s: 2D iterator is used without explicit dimension projection", iinterv_var->var_name.c_str());
	}

	for (Track_vars::iterator ivar = m_track_vars.begin(); ivar != m_track_vars.end(); ++ivar) {
		if (ivar->requires_pv) {
			// load binned_pvals
			SEXP val;
			Binned_pv &pv = ivar->pv_binned;
			string pv_fname(track2path(m_iu.get_env(), ivar->track_n_imdf->name) + "/vars/pv.percentiles");

			if (access(pv_fname.c_str(), R_OK) < 0 && errno == ENOENT) {
				char command[1000];
				SEXP rretv;

				REprintf("Preparing track %s for percentiles queries\n", ivar->track_n_imdf->name.c_str());
				snprintf(command, sizeof(command),
						"{ "
						"	.ginteractive = getOption(\".ginteractive\")\n"
						"	tryCatch({\n"
						"			options(.ginteractive = FALSE)\n"
						"			misha:::.gtrack.prepare.pvals(\"%s\")\n"
						"		},\n"
						"		finally = { options(.ginteractive = .ginteractive) })"
						" }",
						ivar->track_n_imdf->name.c_str());
				runprotect(rretv = run_in_R(command, m_iu.get_env()));
			}

			rprotect(val = RSaneUnserialize(pv_fname.c_str()));
			SEXPCleaner val_cleaner(val);
			SEXP breaks = getAttrib(val, install("breaks"));

			if (breaks == R_NilValue || !isReal(breaks) || length(breaks) != length(val))
				verror("File %s is in invalid format.", pv_fname.c_str());

			pv.bins.assign(REAL(val), REAL(val) + length(val));
			pv.binfinder.init(REAL(breaks), length(breaks));
		}
	}
}

void TrackExpressionVars::define_r_vars(unsigned size)
{
	for (Track_vars::iterator ivar = m_track_vars.begin(); ivar != m_track_vars.end(); ivar++) {
		rprotect(ivar->rvar = RSaneAllocVector(REALSXP, size));
		defineVar(install(ivar->var_name.c_str()), ivar->rvar, m_iu.get_env());
		ivar->var = REAL(ivar->rvar);
	}
	for (Interv_vars::iterator ivar = m_interv_vars.begin(); ivar != m_interv_vars.end(); ivar++) {
		rprotect(ivar->rvar = RSaneAllocVector(REALSXP, size));
		defineVar(install(ivar->var_name.c_str()), ivar->rvar, m_iu.get_env());
		ivar->var = REAL(ivar->rvar);
	}
}

void TrackExpressionVars::start_chrom(const GInterval &interval)
{
	for (Track_n_imdfs::iterator itrack_n_imdf = m_track_n_imdfs.begin(); itrack_n_imdf != m_track_n_imdfs.end(); ++itrack_n_imdf) {
		try {
			string filename(track2path(m_iu.get_env(), itrack_n_imdf->name) + "/" + GenomeTrack::get_1d_filename(m_iu.get_chromkey(), interval.chromid));

            delete itrack_n_imdf->track;
			if (itrack_n_imdf->type == GenomeTrack::FIXED_BIN) {
                itrack_n_imdf->track = new GenomeTrackFixedBin;
                ((GenomeTrackFixedBin *)itrack_n_imdf->track)->init_read(filename.c_str(), interval.chromid);
			} else if (itrack_n_imdf->type == GenomeTrack::SPARSE) {
				itrack_n_imdf->track = new GenomeTrackSparse;
				((GenomeTrackSparse *)itrack_n_imdf->track)->init_read(filename.c_str(), interval.chromid);
			} else if (itrack_n_imdf->type == GenomeTrack::ARRAYS) {
				itrack_n_imdf->track = new GenomeTrackArrays;

				// Optimization step:
				// Normally if two virtual tracks share the same source track, slice and iterator modifiers then only one real track
				// (i.e. Track_n_imdfs) is created. Thus if two virtual tracks based on a common source array track differ only by slice
				// still two Track_n_imdfs and subsequently two GenomeTrackArrays are created. These two GenomeTrackArrays objects will
				// open exactly the same file, make exactly the same "seek" and read the same "file values" each time read_interval is called.
				// Once "file values" are achieved each object will calculate its own "track value" based on its unique slice.
				// We seek to prevent opening and reading the same file multiple times.
				// set_master_obj() allows an object to use "file values" of another "master" object.
				// Thus multiple reading of the same file is prevented, yet still the "track value" is calculated differently by each object.
				for (Track_n_imdfs::iterator itrack_n_imdf2 = m_track_n_imdfs.begin(); itrack_n_imdf2 != itrack_n_imdf; ++itrack_n_imdf2) {
					if (itrack_n_imdf->name == itrack_n_imdf2->name &&
						itrack_n_imdf->imdf1d == itrack_n_imdf2->imdf1d && itrack_n_imdf->imdf2d == itrack_n_imdf2->imdf2d) 
					{
						((GenomeTrackArrays *)itrack_n_imdf->track)->set_master_obj((GenomeTrackArrays *)itrack_n_imdf2->track);
						break;
					}
				}

				((GenomeTrackArrays *)itrack_n_imdf->track)->init_read(filename.c_str(), interval.chromid);
			} else
				verror("Internal error: track %s of type %s is not supported by 1D iterators", itrack_n_imdf->name.c_str(), GenomeTrack::TYPE_NAMES[itrack_n_imdf->type]);
		} catch (TGLException &e) {
			verror("%s\n", e.msg());
		}
	}
    register_track_functions();
}

void TrackExpressionVars::start_chrom(const GInterval2D &interval)
{
	for (Track_n_imdfs::iterator itrack_n_imdf = m_track_n_imdfs.begin(); itrack_n_imdf != m_track_n_imdfs.end(); ++itrack_n_imdf) {
		try {
			if (GenomeTrack::is_1d(itrack_n_imdf->type)) {
				int chromid = 0;

				if (itrack_n_imdf->imdf1d->dim == Iterator_modifier1D::DIM1)
					chromid = interval.chromid1();
				else if (itrack_n_imdf->imdf1d->dim == Iterator_modifier1D::DIM2)
					chromid = interval.chromid2();
				else
					verror("Internal error: no 2D to 1D conversion for track %s", itrack_n_imdf->name.c_str());

				if (chromid != itrack_n_imdf->imdf1d->interval.chromid) {
					string filename(track2path(m_iu.get_env(), itrack_n_imdf->name) + "/" + GenomeTrack::get_1d_filename(m_iu.get_chromkey(), chromid));

					delete itrack_n_imdf->track;
					if (itrack_n_imdf->type == GenomeTrack::FIXED_BIN) {
						itrack_n_imdf->track = new GenomeTrackFixedBin;
						((GenomeTrackFixedBin *)itrack_n_imdf->track)->init_read(filename.c_str(), chromid);
					} else if (itrack_n_imdf->type == GenomeTrack::SPARSE) {
						itrack_n_imdf->track = new GenomeTrackSparse;
						((GenomeTrackSparse *)itrack_n_imdf->track)->init_read(filename.c_str(), chromid);
					} else if (itrack_n_imdf->type == GenomeTrack::ARRAYS) {
						itrack_n_imdf->track = new GenomeTrackArrays;
						((GenomeTrackArrays *)itrack_n_imdf->track)->init_read(filename.c_str(), chromid);
					} else
						verror("Internal error: track %s of type %s is not supported by 1D iterators (projected from 2D)", itrack_n_imdf->name.c_str(), GenomeTrack::TYPE_NAMES[itrack_n_imdf->type]);
				}
			} else if (!m_interval2d.is_same_chrom(interval)) {
				string filename(track2path(m_iu.get_env(), itrack_n_imdf->name) + "/" + GenomeTrack::get_2d_filename(m_iu.get_chromkey(), interval.chromid1(), interval.chromid2()));

				delete itrack_n_imdf->track;
				if (itrack_n_imdf->type == GenomeTrack::RECTS) {
					itrack_n_imdf->track = new GenomeTrackRectsRects(m_iu.get_track_chunk_size(), m_iu.get_track_num_chunks());
					((GenomeTrackRectsRects *)itrack_n_imdf->track)->init_read(filename.c_str(), interval.chromid1(), interval.chromid2());
				} else if (itrack_n_imdf->type == GenomeTrack::POINTS) {
					itrack_n_imdf->track = new GenomeTrackRectsPoints(m_iu.get_track_chunk_size(), m_iu.get_track_num_chunks());
					((GenomeTrackRectsPoints *)itrack_n_imdf->track)->init_read(filename.c_str(), interval.chromid1(), interval.chromid2());
				} else if (itrack_n_imdf->type == GenomeTrack::COMPUTED) {
					itrack_n_imdf->track = new GenomeTrackComputed(m_groot, m_iu.get_track_chunk_size(), m_iu.get_track_num_chunks());
					((GenomeTrackComputed *)itrack_n_imdf->track)->init_read(filename.c_str(), interval.chromid1(), interval.chromid2());
				} else
					verror("Internal error: track %s of type %s is not supported by 2D iterators", itrack_n_imdf->name.c_str(), GenomeTrack::TYPE_NAMES[itrack_n_imdf->type]);
			}
		} catch (TGLException &e) {
			verror("%s\n", e.msg());
		}
	}
	register_track_functions();
}

void TrackExpressionVars::set_vars(const GInterval &interval, unsigned idx)
{
    if (m_interval1d.chromid != interval.chromid) {
        start_chrom(interval);
	}

	m_interval1d = interval;

    for (Iterator_modifiers1D::iterator iimdf = m_imdfs1d.begin(); iimdf != m_imdfs1d.end(); ++iimdf)
        iimdf->transform(interval, m_iu.get_chromkey());

    set_vars(idx);
}

void TrackExpressionVars::set_vars(const GInterval2D &interval, const DiagonalBand &band, unsigned idx)
{
	if (!m_interval2d.is_same_chrom(interval))
		start_chrom(interval);

	m_interval2d = interval;
	m_band = band;

	for (Iterator_modifiers1D::iterator iimdf = m_imdfs1d.begin(); iimdf != m_imdfs1d.end(); ++iimdf)
		iimdf->transform(interval, m_iu.get_chromkey());

	for (Iterator_modifiers2D::iterator iimdf = m_imdfs2d.begin(); iimdf != m_imdfs2d.end(); ++iimdf)
		iimdf->transform(interval, m_iu.get_chromkey());

	set_vars(idx);
}

void TrackExpressionVars::set_vars(unsigned idx)
{
	for (Track_n_imdfs::iterator itrack_n_imdf = m_track_n_imdfs.begin(); itrack_n_imdf != m_track_n_imdfs.end(); ++itrack_n_imdf) {
		try {
			if (GenomeTrack::is_2d(itrack_n_imdf->type)) {
				if (itrack_n_imdf->imdf2d) {
					if (!itrack_n_imdf->imdf2d->out_of_range)
						((GenomeTrack2D *)itrack_n_imdf->track)->read_interval(itrack_n_imdf->imdf2d->interval, m_band);
				} else
					((GenomeTrack2D *)itrack_n_imdf->track)->read_interval(m_interval2d, m_band);
			} else {
				if (itrack_n_imdf->imdf1d) {
					if (!itrack_n_imdf->imdf1d->out_of_range)
						((GenomeTrack1D *)itrack_n_imdf->track)->read_interval(itrack_n_imdf->imdf1d->interval);
				} else
					((GenomeTrack1D *)itrack_n_imdf->track)->read_interval(m_interval1d);
			}
		} catch (TGLException &e) {
			verror("%s", e.msg());
		}
	}

	for (Track_vars::iterator ivar = m_track_vars.begin(); ivar != m_track_vars.end(); ++ivar) {
		if (GenomeTrack::is_1d(ivar->track_n_imdf->type)) {
			GenomeTrack1D &track = *(GenomeTrack1D *)ivar->track_n_imdf->track;

			if (ivar->track_n_imdf->imdf1d && ivar->track_n_imdf->imdf1d->out_of_range)
				ivar->var[idx] = numeric_limits<double>::quiet_NaN();
			else {
				switch (ivar->val_func) {
				case Track_var::REG:
				case Track_var::PV:
					ivar->var[idx] = track.last_avg();
					break;
				case Track_var::REG_MIN:
				case Track_var::PV_MIN:
					ivar->var[idx] = track.last_min();
					break;
				case Track_var::REG_MAX:
				case Track_var::PV_MAX:
					ivar->var[idx] = track.last_max();
					break;
				case Track_var::REG_NEAREST:
					ivar->var[idx] = track.last_nearest();
					break;
				case Track_var::STDDEV:
					ivar->var[idx] = track.last_stddev();
					break;
				case Track_var::SUM:
					ivar->var[idx] = track.last_sum();
					break;
				case Track_var::QUANTILE:
					ivar->var[idx] = track.last_quantile(ivar->percentile);
					break;
				default:
					verror("Internal error: unsupported function %d", ivar->val_func);
				}

				if (ivar->requires_pv) {
					double val = ivar->var[idx];
					if (!std::isnan(val)) {
						int bin = ivar->pv_binned.binfinder.val2bin(val);
						if (bin < 0) {
							if (val <= ivar->pv_binned.binfinder.get_breaks().front())
								ivar->var[idx] = ivar->pv_binned.bins[0];
							else
								ivar->var[idx] = 1.;
						} else
							ivar->var[idx] = ivar->pv_binned.bins[bin];
					}
				}
			}
		} else {
			GenomeTrack2D &track = *(GenomeTrack2D *)ivar->track_n_imdf->track;

			if (ivar->track_n_imdf->imdf2d && ivar->track_n_imdf->imdf2d->out_of_range)
				ivar->var[idx] = numeric_limits<double>::quiet_NaN();
			else {
				switch (ivar->val_func) {
				case Track_var::REG:
					ivar->var[idx] = track.last_avg();
					break;
				case Track_var::REG_MIN:
					ivar->var[idx] = track.last_min();
					break;
				case Track_var::REG_MAX:
					ivar->var[idx] = track.last_max();
					break;
				case Track_var::WEIGHTED_SUM:
					ivar->var[idx] = track.last_weighted_sum();
					break;
				case Track_var::OCCUPIED_AREA:
					ivar->var[idx] = track.last_occupied_area();
					break;
				default:
					verror("Internal error: unsupported function %d", ivar->val_func);
				}
			}
		}
	}

	// set intervals variables
	for (Interv_vars::iterator ivar = m_interv_vars.begin(); ivar != m_interv_vars.end(); ++ivar) {
		if (ivar->val_func == Interv_var::DIST) {
			// if iterator modifier exists, iterator intervals might not come sorted => perform a binary search
			if (ivar->imdf1d) {
				const GInterval &interval = ivar->imdf1d->interval;
				double min_dist = numeric_limits<double>::max();
				double dist;
				int64_t coord = (interval.start + interval.end) / 2;
				GIntervals::const_iterator iinterv = lower_bound(ivar->sintervs.begin(), ivar->sintervs.end(), interval, GIntervals::compare_by_start_coord);

				if (iinterv != ivar->sintervs.end() && iinterv->chromid == interval.chromid)
					min_dist = iinterv->dist2coord(coord, ivar->dist_margin);

				if (iinterv != ivar->sintervs.begin() && (iinterv - 1)->chromid == interval.chromid) {
					dist = (iinterv - 1)->dist2coord(coord, ivar->dist_margin);
					if (fabs(min_dist) > fabs(dist))
						min_dist = dist;
				}

				// if min_dist == double_max then we haven't found an interval with the same chromosome as the iterator interval =>
				// we can skip the second binary search
				if (min_dist == numeric_limits<double>::max())
					ivar->var[idx] = numeric_limits<double>::quiet_NaN();
				else {
					iinterv = lower_bound(ivar->eintervs.begin(), ivar->eintervs.end(), interval, GIntervals::compare_by_end_coord);

					if (iinterv != ivar->eintervs.end() && iinterv->chromid == interval.chromid) {
						dist = iinterv->dist2coord(coord, ivar->dist_margin);
						if (fabs(min_dist) > fabs(dist))
							min_dist = dist;
					}

					if (iinterv != ivar->eintervs.begin() && (iinterv - 1)->chromid == interval.chromid) {
						dist = (iinterv - 1)->dist2coord(coord, ivar->dist_margin);
						if (fabs(min_dist) > fabs(dist))
							min_dist = dist;
					}

					ivar->var[idx] = min_dist;
				}
			} else {
				const GIntervals *pintervs[2] = { &ivar->sintervs, &ivar->eintervs };
				GIntervals::const_iterator *piinterv[2] = { &ivar->siinterv, &ivar->eiinterv };
				double dist[2] = { 0, 0 };
				const GInterval &interval = m_interval1d;

				for (int i = 0; i < 2; ++i) {
					const GIntervals &intervs = *pintervs[i];
					GIntervals::const_iterator &iinterv = *piinterv[i];

					while (iinterv != intervs.end() && iinterv->chromid < interval.chromid)
						++iinterv;

					if (iinterv == intervs.end() || iinterv->chromid != interval.chromid)
						dist[i] = numeric_limits<double>::quiet_NaN();
					else {
						int64_t coord = (interval.start + interval.end) / 2;
						dist[i] = (double)iinterv->dist2coord(coord, ivar->dist_margin);
						GIntervals::const_iterator iinterv_next = iinterv + 1;

						while (iinterv_next != intervs.end() && iinterv_next->chromid == interval.chromid) {
							double dist_next = iinterv_next->dist2coord(coord, ivar->dist_margin);

							if (fabs(dist[i]) < fabs(dist_next))
								break;

							iinterv = iinterv_next;
							dist[i] = dist_next;
							++iinterv_next;
						}
					}
				}

				ivar->var[idx] = fabs(dist[0]) < fabs(dist[1]) ? dist[0] : dist[1];
			}
		} else if (ivar->val_func == Interv_var::DIST_CENTER) {
			// if iterator modifier exists, iterator intervals might not come sorted => perform a binary search
			if (ivar->imdf1d) {
				int64_t coord = (ivar->imdf1d->interval.start + ivar->imdf1d->interval.end) / 2;
				GInterval interval(ivar->imdf1d->interval.chromid, coord, coord + 1, 0);
				GIntervals::const_iterator iinterv = lower_bound(ivar->sintervs.begin(), ivar->sintervs.end(), interval, GIntervals::compare_by_start_coord);
				double dist = numeric_limits<double>::quiet_NaN();

				ivar->var[idx] = numeric_limits<double>::quiet_NaN();

				if (iinterv != ivar->sintervs.end() && iinterv->chromid == interval.chromid)
					dist = iinterv->dist2center(coord);

				if (dist != numeric_limits<double>::quiet_NaN() && iinterv != ivar->sintervs.begin() && (iinterv - 1)->chromid == interval.chromid)
					dist = (iinterv - 1)->dist2center(coord);

				ivar->var[idx] = dist;
			} else {
				int64_t coord = (m_interval1d.start + m_interval1d.end) / 2;
				GIntervals::const_iterator &iinterv = ivar->siinterv;
				double dist = numeric_limits<double>::quiet_NaN();

				while (iinterv != ivar->sintervs.end() && ivar->siinterv->chromid < m_interval1d.chromid)
					++iinterv;

				while (iinterv != ivar->sintervs.end() && iinterv->chromid == m_interval1d.chromid && iinterv->start <= coord) {
					if (iinterv->end > coord)
						dist = iinterv->dist2center(coord);
					++iinterv;
				}

				ivar->var[idx] = dist;
			}
		}
	}
}

