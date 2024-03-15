#include <cstdint>
#include <fts.h>

#include "rdbutils.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP gfind_tracks_n_intervals(SEXP _dir, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

        // check the arguments
		if (!isString(_dir) || length(_dir) != 1)
			verror("Dir argument argument is not a string");

		const char *dir = CHAR(STRING_ELT(_dir, 0));
		IntervUtils iu(_envir);

        vector<string> tracks;
        vector<string> intervs;

        FTS *tree;
        FTSENT *f;
        char *dirs[] = { (char *)dir, NULL };

        tree = fts_open(dirs, FTS_LOGICAL | FTS_NOSTAT, NULL);
        if (tree == NULL)
            verror("Opening %s: %s", dir, strerror(errno));

        while ((f = fts_read(tree))) {
            if (f->fts_info == FTS_DP)   // skip directories in post order
                continue;

            short len_wo_track_ext = f->fts_namelen - rdb::TRACK_FILE_EXT.size();
            if (f->fts_namelen > rdb::TRACK_FILE_EXT.size() && !strcmp(f->fts_name + len_wo_track_ext, rdb::TRACK_FILE_EXT.c_str())) {
                if (find(f->fts_name, f->fts_name + len_wo_track_ext, '.') == f->fts_name + len_wo_track_ext)
                    tracks.push_back(f->fts_path);
                fts_set(tree, f, FTS_SKIP);
                continue;
            }

            short len_wo_interv_ext = f->fts_namelen - rdb::INTERV_FILE_EXT.size();
            if (f->fts_namelen > rdb::INTERV_FILE_EXT.size() && !strcmp(f->fts_name + f->fts_namelen - rdb::INTERV_FILE_EXT.size(), rdb::INTERV_FILE_EXT.c_str())) {
                if (find(f->fts_name, f->fts_name + len_wo_interv_ext, '.') == f->fts_name + len_wo_interv_ext)
                    intervs.push_back(f->fts_path);
                fts_set(tree, f, FTS_SKIP);
                continue;
            }
            
            // skip directories that have "." in their names (if not skipped these names will make a mess while trying to resolve a track/interv name)
            if (find(f->fts_name, f->fts_name + f->fts_namelen, '.') != f->fts_name + f->fts_namelen) {
                fts_set(tree, f, FTS_SKIP);
                continue;
            }
        }
        fts_close(tree);

		SEXP answer;
		SEXP rtracks;
		SEXP rintervs;

		rprotect(rtracks = RSaneAllocVector(STRSXP, tracks.size()));
		for (vector<string>::iterator itrack = tracks.begin(); itrack < tracks.end(); ++itrack) {
			itrack->resize(itrack->size() - rdb::TRACK_FILE_EXT.size());
			for (uint64_t pos = itrack->find_first_of('/'); pos != string::npos; pos = itrack->find_first_of('/', pos + 1))
				itrack->at(pos) = '.';
			SET_STRING_ELT(rtracks, itrack - tracks.begin(), mkChar(itrack->c_str() + strlen(dir) + 1));
		}

		rprotect(rintervs = RSaneAllocVector(STRSXP, intervs.size()));
		for (vector<string>::iterator iinterv = intervs.begin(); iinterv < intervs.end(); ++iinterv) {
			iinterv->resize(iinterv->size() - rdb::INTERV_FILE_EXT.size());
			for (uint64_t pos = iinterv->find_first_of('/'); pos != string::npos; pos = iinterv->find_first_of('/', pos + 1))
				iinterv->at(pos) = '.';
			SET_STRING_ELT(rintervs, iinterv - intervs.begin(), mkChar(iinterv->c_str() + strlen(dir) + 1));
		}

		rprotect(answer = RSaneAllocVector(VECSXP, 2));
		SET_VECTOR_ELT(answer, 0, rtracks);
		SET_VECTOR_ELT(answer, 1, rintervs);

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

}
