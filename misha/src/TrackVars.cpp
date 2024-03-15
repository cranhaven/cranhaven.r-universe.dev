#include <cstdint>
#include "GenomeTrack.h"
#include "rdbinterval.h"
#include "rdbprogress.h"
#include "rdbutils.h"

using namespace rdb;

struct TrackIdxVal {
	int trackidx;
	string val;

	TrackIdxVal(int _trackidx, const char *_val) : trackidx(_trackidx), val(_val) {}
};

typedef list<TrackIdxVal> TrackIdxVals;

typedef unordered_map<string, TrackIdxVals> AttrsMap;

struct SortAttrs {
	bool operator()(AttrsMap::const_iterator iattr1,  AttrsMap::const_iterator iattr2) {
		return iattr1->second.size() > iattr2->second.size() || (iattr1->second.size() == iattr2->second.size() && iattr1->first < iattr2->first);
	}
};

extern "C" {

SEXP gget_tracks_attrs(SEXP _tracks, SEXP _attrs, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_tracks))
			verror("Tracks argument must be a vector of strings");

		if (!isNull(_attrs) && (!isString(_attrs) || length(_attrs) < 1))
			verror("Attributes names argument must be a string");

		IntervUtils iu(_envir);
		AttrsMap attrs;

		if (!isNull(_attrs)) {
			for (int i = 0; i < length(_attrs); ++i) 
				attrs[CHAR(STRING_ELT(_attrs, i))] = TrackIdxVals();
		}

		Progress_reporter progress;
		progress.init(length(_tracks), 1);
		GenomeTrack::TrackAttrs track_attrs;

		for (int itrack = 0; itrack < length(_tracks); ++itrack) {
			const char *trackname = CHAR(STRING_ELT(_tracks, itrack));
			GenomeTrack::load_attrs(trackname, track2attrs_path(_envir, trackname).c_str(), track_attrs); 

			for (GenomeTrack::TrackAttrs::const_iterator itrack_attr = track_attrs.begin(); itrack_attr != track_attrs.end(); ++itrack_attr) {
				AttrsMap::iterator iattr = attrs.find(itrack_attr->first);

				if (iattr == attrs.end()) {
					if (isNull(_attrs)) 
						iattr = attrs.insert(pair<string, TrackIdxVals>(itrack_attr->first, TrackIdxVals())).first;
					else
						continue;
				}

				iattr->second.push_back(TrackIdxVal(itrack, itrack_attr->second.c_str()));
			}
			progress.report(1);
			check_interrupt();
		}
		progress.report_last();

		if (attrs.empty()) 
			return R_NilValue;

		// if all attributes should be returned, then sort them by popularity
		vector<AttrsMap::const_iterator> sorted_attrs;

		if (isNull(_attrs)) {
			for (unordered_map<string, TrackIdxVals>::const_iterator iattr = attrs.begin(); iattr != attrs.end(); ++iattr) 
				sorted_attrs.push_back(iattr);
			sort(sorted_attrs.begin(), sorted_attrs.end(), SortAttrs());
		} else {
			for (int i = 0; i < length(_attrs); ++i) 
				sorted_attrs.push_back(attrs.find(CHAR(STRING_ELT(_attrs, i))));
		}

		SEXP rattrs, rattr;
		SEXP rrow_names, rcol_names;

		rprotect(rattrs = RSaneAllocVector(VECSXP, sorted_attrs.size()));
		setAttrib(rattrs, R_ClassSymbol, mkString("data.frame"));
		setAttrib(rattrs, R_NamesSymbol, (rcol_names = RSaneAllocVector(STRSXP, sorted_attrs.size())));
		setAttrib(rattrs, R_RowNamesSymbol, (rrow_names = RSaneAllocVector(STRSXP, length(_tracks))));

		for (int itrack = 0; itrack < length(_tracks); ++itrack)
			SET_STRING_ELT(rrow_names, itrack, STRING_ELT(_tracks, itrack));

		for (uint64_t i = 0; i < sorted_attrs.size(); ++i) {
            rprotect(rattr = RSaneAllocVector(STRSXP, length(_tracks)));

			for (int itrack = 0; itrack < length(_tracks); ++itrack) 
				SET_STRING_ELT(rattr, itrack, mkChar(""));

			for (TrackIdxVals::const_iterator iattr = sorted_attrs[i]->second.begin(); iattr != sorted_attrs[i]->second.end(); ++iattr) 
				SET_STRING_ELT(rattr, iattr->trackidx, mkChar(iattr->val.c_str()));

			SET_STRING_ELT(rcol_names, i, mkChar(sorted_attrs[i]->first.c_str()));
            SET_VECTOR_ELT(rattrs, i, rattr);
		}

		return rattrs;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP gset_tracks_attrs(SEXP _attrs, SEXP _replace, SEXP _read_only_attrs, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isLogical(_replace) || length(_replace) != 1)
			verror("remove.others argument must be logical"); 

		if (!isVector(_attrs))
			verror("Invalid format of attributes");

		if (!isNull(_read_only_attrs) && !isString(_read_only_attrs))
			verror("Invalid format of read-only attributes");
		
		SEXP rattr, rattr_names, rtracknames;

		rattr_names = getAttrib(_attrs, R_NamesSymbol);
		rtracknames = getAttrib(_attrs, R_RowNamesSymbol);

		if (!isString(rattr_names) || !isString(rtracknames) || length(rattr_names) != length(_attrs))
			verror("Invalid format of attributes");

		IntervUtils iu(_envir);
		GenomeTrack::TrackAttrs track_attrs;
		Progress_reporter progress;
		progress.init(length(rtracknames), 1);
		set<string> read_only_attrs;
		bool replace_all = (bool)LOGICAL(_replace)[0];

		if (!isNull(_read_only_attrs)) {
			for (int i = 0; i < length(_read_only_attrs); ++i) 
				read_only_attrs.insert(CHAR(STRING_ELT(_read_only_attrs, i)));
		}

		for (int itrack = 0; itrack < length(rtracknames); ++itrack) {
			const char *trackname = CHAR(STRING_ELT(rtracknames, itrack));
			bool attrs_changed = false;

			track_attrs.clear();

			if (!replace_all || !read_only_attrs.empty()) { // do not replace the whole table of attributes
				GenomeTrack::load_attrs(trackname, track2attrs_path(_envir, trackname).c_str(), track_attrs);

				if (replace_all) { // if all attributes should be replaced then leave just read-only attributes
					for (GenomeTrack::TrackAttrs::iterator itrack_attr = track_attrs.begin(); itrack_attr != track_attrs.end(); ) {
						if (read_only_attrs.find(itrack_attr->first) == read_only_attrs.end()) {
							GenomeTrack::TrackAttrs::iterator icur = itrack_attr;
							++itrack_attr;
							track_attrs.erase(icur);
						} else
							++itrack_attr;
					}
				}
			}
				 
			for (int iattr = 0; iattr < length(rattr_names); ++iattr) {
				const char *attrname = CHAR(STRING_ELT(rattr_names, iattr));
				GenomeTrack::TrackAttrs::iterator itrack_attr = track_attrs.find(attrname);
				
				rattr = VECTOR_ELT(_attrs, iattr);
				if (!isString(rattr) || length(rattr) != length(rtracknames))
					verror("Invalid format of attributes");
				
				const char *val = CHAR(STRING_ELT(rattr, itrack));

				if (read_only_attrs.find(attrname) != read_only_attrs.end()) {  // read-only attribute
					const char *track_attr_val = itrack_attr == track_attrs.end() ? "" : itrack_attr->second.c_str();

					if (strcmp(track_attr_val, val))
						verror("Attempt to change read-only attribute %s for track %s", attrname, trackname);
				} else {                                                         // not read-only attribute
					if (itrack_attr != track_attrs.end()) {
						if (itrack_attr->second != val) {
							if (*val) 
								itrack_attr->second = val;
							else
								track_attrs.erase(itrack_attr);
							attrs_changed = true;
						}
					} else {
						track_attrs[attrname] = val;
						attrs_changed = true;
					}
				}
			}

			if (attrs_changed || replace_all)
				GenomeTrack::save_attrs(trackname, track2attrs_path(_envir, trackname).c_str(), track_attrs);

			progress.report(1);
			check_interrupt();
		}
		progress.report_last();
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

}

