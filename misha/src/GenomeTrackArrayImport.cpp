#include <cmath>
#include <list>
#include <vector>

#include "rdbinterval.h"
#include "rdbprogress.h"
#include "rdbutils.h"

#include "GenomeArraysCsv.h"
#include "GenomeTrackArrays.h"

using namespace std;
using namespace rdb;

class Source {
public:
	Source(const GenomeChromKey &chromkey, const char *src) : m_chromkey(chromkey) {
		m_src = src;
		m_csv.init(m_src.c_str(), m_chromkey);
		m_dependencies.resize(get_colnames().size());
		m_array_idxs.resize(get_colnames().size());
	}

	Source(const GenomeChromKey &chromkey, const char *src, const char *dir, SEXP colnames) : m_chromkey(chromkey) {
		m_src = src;
		m_dir = dir;
		m_colnames.reserve(length(colnames));
		for (int i = 0; i < length(colnames); ++i) 
			m_colnames.push_back(CHAR(STRING_ELT(colnames, i)));
		m_dependencies.resize(get_colnames().size());
		m_array_idxs.resize(get_colnames().size());
	}

	const char *get_src() const { return m_src.c_str(); }
	const char *get_src_typename() const { return is_csv() ? "file" : "track"; }
	bool is_csv() const { return m_colnames.empty(); }

	const vector<string> &get_colnames() const { return is_csv() ? m_csv.get_colnames() : m_colnames; }

	const GIntervals &get_intervals() const { return *m_intervals; }

	void add_dependency(int slice1, const Source *src, int slice2) {
		m_dependencies[slice1] = Dependency(src, slice2);
		m_array_idxs[slice1] = src->m_array_idxs[slice2];
	}

	void start_chrom(int chromid) {
		if (is_csv())
			m_intervals = (GIntervals *)&m_csv.get_intervals(chromid);
		else {
			char filename[FILENAME_MAX];

			snprintf(filename, sizeof(filename), "%s/%s", m_dir.c_str(), GenomeTrack::get_1d_filename(m_chromkey, chromid).c_str());
			m_track.init_read(filename, chromid);
			m_intervals = (GIntervals *)&m_track.get_intervals();
		}

		m_last_interval.start = m_last_interval.end = 0;
	}

	const vector<float> &get_vals(GIntervals::const_iterator iinterval) {
		if (is_csv()) 
			m_csv.get_sliced_vals(iinterval, m_vals);
		else
			m_track.get_sliced_vals(iinterval, m_vals, m_colnames.size());
		m_last_interval = *iinterval;
		return m_vals;
	}

	bool is_dependent(int slice) const { return m_dependencies[slice].src != NULL; }

	bool check_writability(int slice) {
		Source *src1 = this;
		int slice1 = slice;

		while (src1->is_dependent(slice1)) {
			Source *src2 = (Source *)src1->m_dependencies[slice1].src;
			int slice2 = src1->m_dependencies[slice1].slice;
			const GInterval &interval2 = src2->m_last_interval;

			if (m_last_interval.start == interval2.start && m_last_interval.end == interval2.end) {
				float v1 = m_vals[slice];
				float v2 = src2->m_vals[slice2];

				if (v1 != v2 && !(std::isnan(v1) && std::isnan(v2)))
					verror("Non matching values (%g and %g) in column %s, interval (%s, %ld, %ld) contained in a %s %s and a %s %s",
						   v2, v1, get_colnames()[slice].c_str(),
						   m_chromkey.id2chrom(m_last_interval.chromid).c_str(), m_last_interval.start, m_last_interval.end,
						   src2->get_src_typename(), src2->get_src(),
						   get_src_typename(), get_src());
				return false;
			}

			src1 = src2;
			slice1 = slice2; 
		}

		return true;
	}

	unsigned get_array_idx(int slice) const { return m_array_idxs[slice]; }

	void set_array_idx(int slice, unsigned val) { m_array_idxs[slice] = val; }

private:
	struct Dependency {
		const Source *src;
		int           slice;

		Dependency() : src(NULL), slice(-1) {}
		Dependency(const Source *_src, int _slice) : src(_src), slice(_slice) {}
	};

	typedef vector<Dependency> Dependencies;

	const GenomeChromKey &m_chromkey;
	GenomeTrackArrays    m_track;
	GenomeArraysCsv      m_csv;
	string               m_src;
	string               m_dir;
	vector<string>       m_colnames;
	vector<unsigned>     m_array_idxs;
	GIntervals          *m_intervals;
	GInterval            m_last_interval;
	Dependencies         m_dependencies;
	vector<float>        m_vals;
};

class Sources : public std::vector<Source *> {
public:
	Sources() : std::vector<Source *>() {}
	Sources(size_type n) : std::vector<Source *>(n) {}

	~Sources() {
		for (iterator isrc = begin(); isrc != end(); ++isrc) 
			delete *isrc;
	}
};

extern "C" {

SEXP garrays_import(SEXP _track, SEXP _src, SEXP _colnames, SEXP _envir)
{
	Sources sources;

	try {
		RdbInitializer rdb_init;

		if (!isString(_track) || length(_track) != 1)
			verror("Track argument is not a string");

		if (!isString(_src)) 
			verror("Invalid source argument");

		if (!isVector(_colnames)) 
			verror("Invalid column names argument");

		if (length(_src) != length(_colnames)) 
			verror("Sources size does not match the columns names set size");

		IntervUtils iu(_envir);
		unsigned array_idx = 0;

		for (int i = 0; i < length(_src); ++i) {
			SEXP rcolnames = VECTOR_ELT(_colnames, i);
			const char *src = CHAR(STRING_ELT(_src, i));

			if (!isString(rcolnames)) 
				verror("Invalid column names argument");

			if (length(rcolnames))
				sources.push_back(new Source(iu.get_chromkey(), src, track2path(_envir, src).c_str(), rcolnames));
			else
				sources.push_back(new Source(iu.get_chromkey(), src));

			Source *source = sources.back();
			const vector<string> &colnames1 = source->get_colnames();

			for (unsigned slice1 = 0; slice1 < colnames1.size(); ++slice1) {
				// Add dependencies in the opposite order so that a chain of dependencies is created: i.e.
				// src3 -> src2 -> src1 (vs. src3 -> src1 AND src2 -> src1)
				// We rely on chain dependencies when we check the writability of a value.
				for (int j = i - 1; j >= 0; --j) {
					const vector<string> &colnames2 = sources[j]->get_colnames();

					for (unsigned slice2 = 0; slice2 < colnames2.size(); ++slice2) {
						if (colnames1[slice1] == colnames2[slice2]) {
							source->add_dependency(slice1, sources[j], slice2);
							break;
						}
					}

					if (source->is_dependent(slice1))
						break;
				}

				if (!source->is_dependent(slice1))
					source->set_array_idx(slice1, array_idx++);
			}
		}

		const char *track_str = CHAR(STRING_ELT(_track, 0));
		string dirname = create_track_dir(_envir, track_str);
		GenomeTrackArrays track;
		char filename[FILENAME_MAX];
		vector<GIntervals::const_iterator> iintervals(sources.size());
		int max_chromid = iu.get_chromkey().get_num_chroms();
		Progress_reporter progress;

		progress.init(max_chromid, 1);

		for (int chromid = 0; chromid < max_chromid; ++chromid) {
			snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), GenomeTrack::get_1d_filename(iu.get_chromkey(), chromid).c_str());
			track.init_write(filename, chromid);

			for (Sources::iterator isrc = sources.begin(); isrc != sources.end(); ++isrc) {
				(*isrc)->start_chrom(chromid);
				iintervals[isrc - sources.begin()] = (*isrc)->get_intervals().begin();
			}

			GenomeTrackArrays::ArrayVals array_vals;

			while (1) {
				// pick up the interval with the smallest start coordinate
				unsigned idx2write = 0;

				for (unsigned idx = 1; idx < sources.size(); ++idx) {
					if (iintervals[idx2write] == sources[idx2write]->get_intervals().end() ||
						(iintervals[idx] != sources[idx]->get_intervals().end() && iintervals[idx2write]->start > iintervals[idx]->start))
						idx2write = idx;
				}

				if (iintervals[idx2write] == sources[idx2write]->get_intervals().end()) 
					break;

				array_vals.clear();

				// check whether the interval fully overlaps intervals from other sources
				for (unsigned idx = 0; idx < sources.size(); ++idx) {

					if (iintervals[idx] != sources[idx]->get_intervals().end()) {
						// write the interval
						if (idx2write == idx || (iintervals[idx2write]->start == iintervals[idx]->start && iintervals[idx2write]->end == iintervals[idx]->end)) {
							const vector<float> &vals = sources[idx]->get_vals(iintervals[idx]);
							for (unsigned i = 0; i < vals.size(); ++i) {
								if (sources[idx]->check_writability(i) && !std::isnan(vals[i]))
									array_vals.push_back(GenomeTrackArrays::ArrayVal(vals[i], sources[idx]->get_array_idx(i)));
							}

							if (idx2write != idx) 
								++iintervals[idx];
						} else if (iintervals[idx]->do_overlap(*iintervals[idx2write]))
							verror("Interval (%s, %ld, %ld) contained in a %s %s overlaps interval (%s, %ld, %ld) contained in a %s %s",
								   iu.get_chromkey().id2chrom(iintervals[idx2write]->chromid).c_str(), iintervals[idx2write]->start, iintervals[idx2write]->end,
								   sources[idx2write]->get_src_typename(), sources[idx2write]->get_src(),
								   iu.get_chromkey().id2chrom(iintervals[idx]->chromid).c_str(), iintervals[idx]->start, iintervals[idx]->end,
								   sources[idx2write]->get_src_typename(), sources[idx]->get_src());
					}
				}

				if (array_vals.size()) 
					track.write_next_interval(*iintervals[idx2write], array_vals.begin(), array_vals.end());
				++iintervals[idx2write];

				progress.report(0);
				check_interrupt();
			}
			progress.report(1);
		}

		unsigned totcols = 0;
		for (Sources::const_iterator isrc = sources.begin(); isrc != sources.end(); ++isrc) {
			for (unsigned i = 0; i < (*isrc)->get_colnames().size(); ++i) {
				if (!(*isrc)->is_dependent(i)) 
					++totcols;
			}
		}
		progress.report_last();

		SEXP answer;
		unsigned colidx = 0;
		rprotect(answer = RSaneAllocVector(STRSXP, totcols));
		for (Sources::const_iterator isrc = sources.begin(); isrc != sources.end(); ++isrc) {
			const vector<string> &colnames = (*isrc)->get_colnames();

			for (unsigned i = 0; i < colnames.size(); ++i) {
				if (!(*isrc)->is_dependent(i)) 
					SET_STRING_ELT(answer, colidx++, mkChar(colnames[i].c_str()));
			}
		}

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

}

