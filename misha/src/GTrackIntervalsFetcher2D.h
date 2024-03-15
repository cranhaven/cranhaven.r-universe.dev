#ifndef GTRACKINTERVALSFETCHER2D_H_INCLUDED
#define GTRACKINTERVALSFETCHER2D_H_INCLUDED

#include <cstdint>
#include "GenomeTrackComputed.h"
#include "GenomeTrackRects.h"
#include "GIntervalsMeta2D.h"
#include "GTrackIntervalsFetcher.h"

template <class Track>
class GTrackIntervalsFetcher2D : public GTrackIntervalsFetcher, public GIntervalsMeta2D {
public:
	GTrackIntervalsFetcher2D() : m_track(NULL) {}
	GTrackIntervalsFetcher2D(const char *track_name, SEXP meta, const IntervUtils &iu) : m_track(NULL) { init(track_name, meta, iu); }

	virtual ~GTrackIntervalsFetcher2D() { clear(); }

	//-------------------------------- GIntervalsFetcher2D interface -----------------------------------

	virtual GIntervalsFetcher2D *create_masked_copy(const set<ChromPair> &chrompairs_mask) const;

	virtual void seal() {}

	virtual void begin_iter();
	virtual void begin_chrom_iter(int chromid1, int chromid2);

	virtual bool next();
	virtual bool next_in_chrom();

	virtual bool isend() const { return m_iter_index < 0 || m_iter_index >= m_size; }
	virtual bool isend_chrom() const { return !m_track || m_track->is_end_interval() || m_cur_chromid != m_iter_chromid; }

	virtual GIntervals2D::const_iterator get_chrom_begin() const;
	virtual GIntervals2D::const_iterator get_chrom_end() const;

	virtual uint64_t iter_index() const { return m_iter_index; }
	virtual uint64_t iter_chrom_index() const { return m_iter_chrom_index; }

	virtual const GInterval2D &cur_interval() const {
		m_cur_interval = m_track->cur_interval();
		m_cur_interval.udata() = (void *)(intptr_t)(m_iter_orig_index);
		return m_cur_interval;
	}

	virtual void sort(Compare_t compare = compare_for_sort);

	virtual void verify_no_overlaps(const GenomeChromKey &chromkey, const char *error_prefix = "") const {}

protected:
	Track              *m_track;
	int                 m_cur_chromid;
	int                 m_iter_chromid;
	uint64_t              m_iter_index;
	uint64_t              m_iter_chrom_index;
	uint64_t              m_iter_orig_index;
    mutable GInterval2D m_cur_interval;

	void clear();
	void init(const char *track_name, SEXP meta, const IntervUtils &iu);
	void load_chrom(int chromid);
};


//------------------------------------------- IMPLEMENTATION -------------------------------------------------------

template <class Track>
void GTrackIntervalsFetcher2D<Track>::init(const char *track_name, SEXP meta, const IntervUtils &iu)
{
	GTrackIntervalsFetcher::init(track_name, iu);
	GIntervalsMeta2D::init(track_name, meta, iu.get_chromkey());

	clear();

	if (typeid(Track) == typeid(GenomeTrackRectsRects))
		m_track = (Track *)(void *)(new GenomeTrackRectsRects(m_iu->get_track_chunk_size(), m_iu->get_track_num_chunks()));
	else if (typeid(Track) == typeid(GenomeTrackRectsPoints))
		m_track = (Track *)(void *)new GenomeTrackRectsPoints(m_iu->get_track_chunk_size(), m_iu->get_track_num_chunks());
	else if (typeid(Track) == typeid(GenomeTrackComputed))
		m_track = (Track *)(void *)new GenomeTrackComputed(get_groot(m_iu->get_env()), m_iu->get_track_chunk_size(), m_iu->get_track_num_chunks());
	else
		verror("This track type cannot currently be used a substitute of intervals");

	m_cur_chromid = m_chroms2size.size();
	m_iter_chromid = -1;
	m_iter_index = 0;
	m_iter_chrom_index = 0;
	m_iter_orig_index = 0;
}

template <class Track>
GIntervalsFetcher2D *GTrackIntervalsFetcher2D<Track>::create_masked_copy(const set<ChromPair> &chrompairs_mask) const
{
	GTrackIntervalsFetcher2D<Track> *obj = new GTrackIntervalsFetcher2D<Track>();

	init_masked_copy(obj, chrompairs_mask);

	obj->m_track_name = m_track_name;
	obj->m_iu = m_iu;
	obj->m_cur_chromid = obj->m_chroms2size.size();
	obj->m_iter_chromid = -1;
	obj->m_iter_index = 0;
	obj->m_iter_chrom_index = 0;
	obj->m_iter_orig_index = 0;
	obj->m_orig_chroms2size = m_orig_chroms2size;

	if (typeid(Track) == typeid(GenomeTrackRectsRects))
		obj->m_track = (Track *)(void *)(new GenomeTrackRectsRects(m_iu->get_track_chunk_size(), m_iu->get_track_num_chunks()));
	else if (typeid(Track) == typeid(GenomeTrackRectsPoints))
		obj->m_track = (Track *)(void *)new GenomeTrackRectsPoints(m_iu->get_track_chunk_size(), m_iu->get_track_num_chunks());
	else if (typeid(Track) == typeid(GenomeTrackComputed))
		obj->m_track = (Track *)(void *)new GenomeTrackComputed(get_groot(m_iu->get_env()), m_iu->get_track_chunk_size(), m_iu->get_track_num_chunks());
	else
		verror("This track type cannot currently be used a substitute of intervals");

	return obj;
}

template <class Track>
void GTrackIntervalsFetcher2D<Track>::begin_iter()
{
	m_iter_chromid = -1;
	m_iter_index = 0;
	m_iter_chrom_index = 0;
	m_iter_orig_index = 0;
	for (int cur_chromid = 0; cur_chromid < (int)m_chroms2size.size(); ++cur_chromid) {
		if (m_chroms2size[cur_chromid]) {
			load_chrom(cur_chromid);
			m_track->begin_interval();
			return;
		}
	}
	m_cur_chromid = m_chroms2size.size();
}

template <class Track>
void GTrackIntervalsFetcher2D<Track>::begin_chrom_iter(int chromid1, int chromid2)
{
	int target_chromid = chroms2idx(chromid1, chromid2);

	m_iter_chromid = target_chromid;
	m_iter_index = 0;
	m_iter_chrom_index = 0;
	m_iter_orig_index = 0;
	for (int cur_chromid = 0; cur_chromid < (int)m_chroms2size.size(); ++cur_chromid) {
		if (cur_chromid == target_chromid) {
			if (m_chroms2size[cur_chromid]) {
				load_chrom(cur_chromid);
				m_track->begin_interval();
			} else
				m_cur_chromid = m_chroms2size.size();
			return;
		}
		m_iter_index += m_chroms2size[cur_chromid];
		m_iter_orig_index += m_orig_chroms2size[cur_chromid];
	}
	m_cur_chromid = m_chroms2size.size();
}

template <class Track>
bool GTrackIntervalsFetcher2D<Track>::next()
{
	if (isend()) 
		return false;

	m_track->next_interval();
	++m_iter_index;
	++m_iter_chrom_index;
	++m_iter_orig_index;
	if (m_track->is_end_interval()) {
		int cur_chromid = m_cur_chromid + 1;
		for (; cur_chromid < (int)m_chroms2size.size(); ++cur_chromid) {
			if (m_chroms2size[cur_chromid]) {
				load_chrom(cur_chromid);
				m_track->begin_interval();
				break;
			}
			m_iter_orig_index += m_orig_chroms2size[cur_chromid];
		}
		if (cur_chromid >= (int)m_chroms2size.size()) 
			m_cur_chromid = m_chroms2size.size();
	}
	return !isend();
}

template <class Track>
bool GTrackIntervalsFetcher2D<Track>::next_in_chrom()
{
	if (isend_chrom()) 
		return false;

	m_track->next_interval();
	++m_iter_index;
	++m_iter_chrom_index;
	++m_iter_orig_index;
	return !isend_chrom();
}

template <class Track>
GIntervals2D::const_iterator GTrackIntervalsFetcher2D<Track>::get_chrom_begin() const
{
	verror("Using a track in place of intervals is not supported by this function");
	return GIntervals2D().end();
}

template <class Track>
GIntervals2D::const_iterator GTrackIntervalsFetcher2D<Track>::get_chrom_end() const
{
	verror("Using a track in place of intervals is not supported by this function");
	return GIntervals2D().end();
}

template <class Track>
void GTrackIntervalsFetcher2D<Track>::sort(Compare_t compare)
{
	if (compare != compare_for_sort)
		verror("Using a track in place of intervals is not supported by this function");
}

template <class Track>
void GTrackIntervalsFetcher2D<Track>::clear()
{
	delete m_track;
	m_track = NULL;
}

template <class Track>
void GTrackIntervalsFetcher2D<Track>::load_chrom(int chromid)
{
	m_iter_chrom_index = 0;
	if (m_cur_chromid != chromid) {
		int chromid1 = idx2chrom1(chromid);
		int chromid2 = idx2chrom2(chromid);

		string filename(track2path(m_iu->get_env(), m_track_name) + "/" + GenomeTrack::get_2d_filename(*m_chromkey, chromid1, chromid2));
		m_track->init_read(filename.c_str(), chromid1, chromid2);
		m_cur_chromid = chromid;
	}
}

#endif

