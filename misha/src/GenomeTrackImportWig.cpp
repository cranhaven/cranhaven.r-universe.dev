#include <cstdint>
#include <cmath>
#include <cstring>

#include "rdbinterval.h"
#include "rdbprogress.h"
#include "rdbutils.h"

#include "GenomeArraysCsv.h"
#include "GenomeTrackFixedBin.h"
#include "GenomeTrackSparse.h"
#include "Wig.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP gtrackimportwig(SEXP _track, SEXP _wig, SEXP _binsize, SEXP _defvalue, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_track) || length(_track) != 1)
			verror("Track argument is not a string");

		if (!isString(_wig) || length(_wig) != 1)
			verror("Wig argument is not a string");

		if ((!isReal(_binsize) && !isInteger(_binsize)) || length(_binsize) != 1)
			verror("Binsize argument is not a number");

		if ((!isReal(_defvalue) && !isInteger(_defvalue)) || length(_defvalue) != 1)
			verror("Defvalue argument is not a number");

		const char *track = CHAR(STRING_ELT(_track, 0));
		const char *fname = CHAR(STRING_ELT(_wig, 0));
		double dbinsize = isReal(_binsize) ? REAL(_binsize)[0] : INTEGER(_binsize)[0];
		unsigned binsize = (unsigned)dbinsize;
		double defvalue = isReal(_defvalue) ? REAL(_defvalue)[0] : INTEGER(_defvalue)[0];

		if (dbinsize < 0 || binsize != dbinsize)
			verror("Invalid value of binsize argument: %g\n", dbinsize);

		string dirname = create_track_dir(_envir, track);
		IntervUtils iu(_envir);
		Wig wig;
		GenomeArraysCsv csv;
		GIntervals data;
		GIntervals::const_iterator iinterv_begin;
		GIntervals::const_iterator iinterv_end;
		vector<float> vals;
		char filename[FILENAME_MAX];
		bool is_csv = false;

		try {
			wig.init(iu.get_chromkey(), fname, true); 
		} catch (TGLException &e) {
			if (e.type() != typeid(Wig) || e.code() == Wig::FILE_ERROR) 
				throw e;
			is_csv = true;
		}

		if (is_csv) {
			try {
				csv.init(fname, iu.get_chromkey()); 
				if (csv.get_colnames().size() != 1) 
					verror("More than one value column appears in file %s", fname);
			} catch (TGLException &e) {
				if (e.type() != typeid(GenomeArraysCsv) || e.code() == GenomeArraysCsv::FILE_ERROR) 
					throw e;
				verror("Unrecognized format of file %s", fname);
			}
		}

		GIntervals all_genome_intervs;
		iu.get_all_genome_intervs(all_genome_intervs);

		Progress_reporter progress;
		progress.init(iu.get_chromkey().get_num_chroms(), 1);

		for (int chromid = 0; chromid < (int)iu.get_chromkey().get_num_chroms(); ++chromid) {
			uint64_t chromsize = iu.get_chromkey().get_chrom_size(chromid);

			check_interrupt();

			if (is_csv) {
				const GIntervals &intervals = csv.get_intervals(chromid);
				iinterv_begin = intervals.begin();
				iinterv_end = intervals.end();
			} else {
				wig.get_data(chromid, data);
				iinterv_begin = data.begin();
				iinterv_end = data.end();
			}

			snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_1d_filename(iu.get_chromkey(), chromid).c_str());

			if (binsize) {  // Fixed-bin track
				GenomeTrackFixedBin gtrack;
				gtrack.init_write(filename, binsize, chromid);
				GIntervals::const_iterator iinterval = iinterv_begin;

				for (uint64_t start_coord = 0; start_coord < chromsize; start_coord += binsize) {
					double sum = 0;
					uint64_t num_non_nans = 0;
					uint64_t end_coord = min(start_coord + binsize, chromsize);

					for (uint64_t coord = start_coord; coord < end_coord; ++coord) {
						float v;

						if (iinterval != iinterv_end && coord >= (uint64_t)iinterval->start && coord < (uint64_t)iinterval->end) {
							if (is_csv) {
								csv.get_sliced_vals(iinterval, vals);
								v = vals.front();
							} else {
								memcpy(&v, &iinterval->udata, sizeof(float));
							}

							if (coord == (uint64_t)iinterval->end - 1) 
								++iinterval;
						} else
							v = defvalue;

						if (!std::isnan(v)) {
							sum += v;
							++num_non_nans;
						}
					}


					gtrack.write_next_bin(num_non_nans ? sum / num_non_nans : numeric_limits<float>::quiet_NaN());
					progress.report(0);
					check_interrupt();
				}
			} else { // Sparse track
				GenomeTrackSparse gtrack;
				gtrack.init_write(filename, chromid);

				for (GIntervals::const_iterator iinterval = iinterv_begin; iinterval != iinterv_end; ++iinterval) {
					float v;

					if (is_csv) {
						csv.get_sliced_vals(iinterval, vals);
						v = vals.front();
					} else {
						memcpy(&v, &iinterval->udata, sizeof(float));
					}

					gtrack.write_next_interval(*iinterval, v);
					progress.report(0);
					check_interrupt();
				}
			}
			progress.report(1);
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
