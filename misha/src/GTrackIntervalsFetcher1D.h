#ifndef GTRACKINTERVALSFETCHER1D_H_INCLUDED
#define GTRACKINTERVALSFETCHER1D_H_INCLUDED

#include <cstdint>
#include "GenomeTrackArrays.h"
#include "GenomeTrackSparse.h"
#include "GIntervalsMeta1D.h"
#include "GTrackIntervalsFetcher.h"

template <class Track>
class GTrackIntervalsFetcher1D : public GTrackIntervalsFetcher, public GIntervalsMeta1D {
public:
	GTrackIntervalsFetcher1D() {}
	GTrackIntervalsFetcher1D(const char *track_name, SEXP meta, const IntervUtils &iu) { init(track_name, meta, iu); }

	virtual ~GTrackIntervalsFetcher1D() {}

	void init(const char *intervset, SEXP meta, const IntervUtils &iu);

	void load_chrom(int chromid);

	const GIntervals &get_chrom_intervals() { return m_intervals; }

	//-------------------------------- GIntervalsFetcher1D interface -----------------------------------

	virtual GIntervalsFetcher1D *create_masked_copy(const set<int> &chromids_mask) const;

	virtual void seal() {}

	virtual int64_t range(int chromid) const { return m_chrom2range[chromid]; } // complexity: O(1)

	virtual void begin_iter();

	virtual void begin_chrom_iter(int chromid);

	virtual bool next();
	virtual bool next_in_chrom();

	virtual bool isend() const { return m_iter_index < 0 || m_iter_index >= m_size; }
	virtual bool isend_chrom() const { return m_iinterval >= m_intervals.end() || m_cur_chromid != m_iter_chrom; }

	virtual GIntervals::const_iterator get_chrom_begin() const { return m_intervals.begin(); }
	virtual GIntervals::const_iterator get_chrom_end() const { return m_intervals.end(); }

	virtual uint64_t iter_index() const { return m_iter_index; }

	virtual uint64_t iter_chrom_index() const { return m_iter_chrom_index; }

	virtual const GInterval &cur_interval() const { return *m_iinterval; }

	virtual void sort(Compare_t compare = compare_by_start_coord);

	virtual void unify_overlaps(bool unify_touching_intervals = true);

	virtual void verify_no_overlaps(const GenomeChromKey &chromkey, const char *error_prefix = "") const {} // there are no overlaps in track intervals

protected:
	GIntervals                 m_intervals;
	GIntervals::const_iterator m_iinterval;
	int                        m_cur_chromid;
	int                        m_iter_chrom;
	uint64_t                     m_iter_index;
	uint64_t                     m_iter_chrom_index;
	Compare_t                  m_compare;
	bool                       m_do_sort;
	bool                       m_unify_touching_intervals;
};


//------------------------------------------- IMPLEMENTATION -------------------------------------------------------

template <class Track>
void GTrackIntervalsFetcher1D<Track>::init(const char *intervset, SEXP meta, const IntervUtils &iu)
{
	GTrackIntervalsFetcher::init(intervset, iu);
	GIntervalsMeta1D::init(intervset, meta, iu.get_chromkey());

	m_cur_chromid = m_chrom2size.size();
	m_iter_chrom = -1;
	m_iter_index = -1;
	m_iter_chrom_index = 0;
	m_do_sort = false;
	m_unify_touching_intervals = false;
	m_iinterval = m_intervals.end();
}

template <class Track>
void GTrackIntervalsFetcher1D<Track>::load_chrom(int chromid)
{
	m_iter_chrom_index = 0;
	if (get_num_intervals(chromid)) {
		if (m_intervals.empty() || m_intervals.front().chromid != chromid) {
			string filename(track2path(m_iu->get_env(), m_track_name) + "/" + GenomeTrack::get_1d_filename(*m_chromkey, chromid));
			Track track;
			track.init_read(filename.c_str(), chromid);
			m_intervals = track.get_intervals();

			// set udata
			uint64_t offset = 0;
			for (int i = 0; i < chromid; ++i)
				offset += m_orig_chrom2size[i];
			for (GIntervals::iterator iinterval = m_intervals.begin(); iinterval < m_intervals.end(); ++iinterval) 
				iinterval->udata = (void *)(intptr_t)(iinterval - m_intervals.begin() + offset);

			if (m_do_sort) 
				m_intervals.sort(m_compare);

			if (m_unify_touching_intervals) 
				m_intervals.unify_overlaps(m_unify_touching_intervals);
		}
	} else
		m_intervals.clear();
}

template <class Track>
GIntervalsFetcher1D *GTrackIntervalsFetcher1D<Track>::create_masked_copy(const set<int> &chromids_mask) const
{
	GTrackIntervalsFetcher1D<Track> *obj = new GTrackIntervalsFetcher1D<Track>();

	init_masked_copy(obj, chromids_mask);

	obj->m_track_name = m_track_name;
	obj->m_iu = m_iu;
	obj->m_cur_chromid = obj->m_chrom2size.size();
	obj->m_iter_chrom = -1;
	obj->m_iter_index = -1;
	obj->m_iter_chrom_index = 0;
	obj->m_do_sort = false;
	obj->m_unify_touching_intervals = false;
	obj->m_iinterval = obj->m_intervals.end();

	if (m_do_sort)
		obj->sort(m_compare);

	if (m_unify_touching_intervals) 
		obj->unify_overlaps(m_unify_touching_intervals);

	return obj;
}

template <class Track>
void GTrackIntervalsFetcher1D<Track>::begin_iter()
{
	m_iter_chrom = -1;
	m_iter_index = 0;
	m_iter_chrom_index = 0;
	m_intervals.clear();
	for (m_cur_chromid = 0; m_cur_chromid < (int)m_chrom2size.size(); ++m_cur_chromid) {
		if (get_num_intervals(m_cur_chromid)) {
			load_chrom(m_cur_chromid);
			m_iinterval = m_intervals.begin();
			return;
		}
	}
}

template <class Track>
void GTrackIntervalsFetcher1D<Track>::begin_chrom_iter(int chromid)
{
	m_iter_chrom = chromid;
	m_iter_index = 0;
	m_iter_chrom_index = 0;
	for (m_cur_chromid = 0; m_cur_chromid < (int)m_chrom2size.size(); ++m_cur_chromid) {
		if (m_cur_chromid == chromid) {
			if (get_num_intervals(m_cur_chromid)) {
				load_chrom(m_cur_chromid);
				m_iinterval = m_intervals.begin();
				return;
			}
			break;
		}
		m_iter_index += get_num_intervals(m_cur_chromid);
	}
	m_intervals.clear();
	m_iinterval = m_intervals.end();
}

template <class Track>
bool GTrackIntervalsFetcher1D<Track>::next()
{
	++m_iinterval;
	++m_iter_index;
	++m_iter_chrom_index;
	if (m_iinterval >= m_intervals.end()) {
		++m_cur_chromid;
		for (; m_cur_chromid < (int)m_chrom2size.size(); ++m_cur_chromid) {
			if (get_num_intervals(m_cur_chromid)) {
				load_chrom(m_cur_chromid);
				m_iinterval = m_intervals.begin();
				break;
			}
		}
	}
	return !isend();
}

template <class Track>
bool GTrackIntervalsFetcher1D<Track>::next_in_chrom()
{
	if (isend_chrom()) 
		return false;

	++m_iinterval;
	++m_iter_index;
	++m_iter_chrom_index;
	return !isend_chrom();
}

template <class Track>
void GTrackIntervalsFetcher1D<Track>::sort(Compare_t compare)
{
	if (compare != compare_by_start_coord) {    // track intervals are already sorted by start coordinate
		m_do_sort = true;
		m_compare = compare;
		if (m_intervals.size())
			m_intervals.sort(m_compare);
	}
}

template <class Track>
void GTrackIntervalsFetcher1D<Track>::unify_overlaps(bool unify_touching_intervals)
{
	// track intervals never overlap, so no need to do anything unless unify_touching_intervals is required
	if (!unify_touching_intervals || m_unify_touching_intervals == unify_touching_intervals) 
		return;

	m_unify_touching_intervals = true;
	m_size = 0;
	m_user_chrom2size = &m_chrom2unified_touching_size;
	for (vector<int64_t>::const_iterator isize = m_chrom2unified_touching_size.begin(); isize < m_chrom2unified_touching_size.end(); ++isize)
		m_size += *isize;
	m_intervals.unify_overlaps(m_unify_touching_intervals);
}

#endif

