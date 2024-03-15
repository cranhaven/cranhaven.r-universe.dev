#include <cstdint>
#include <sys/stat.h>
#include <sys/types.h>

#include "GIntervalsBigSet1D.h"
#include "rdbutils.h"

//------------------------------------- GIntervalsBigSet1D --------------------------------------

void GIntervalsBigSet1D::init(const char *intervset, SEXP meta, const IntervUtils &iu)
{
	GIntervalsBigSet::init(intervset, iu);
	GIntervalsMeta1D::init(intervset, meta, iu.get_chromkey());

	m_cur_chromid = m_chrom2size.size();
	m_iter_chrom = -1;
	m_iter_index = -1;
	m_iter_chrom_index = 0;
	m_do_sort = false;
	m_do_unify_overlaps = false;
	m_iinterval = m_intervals.end();
}

void GIntervalsBigSet1D::load_chrom(int chromid)
{
	m_iter_chrom_index = 0;
	if (get_num_intervals(chromid)) {
		if (m_intervals.empty() || m_intervals.front().chromid != chromid) {
			string filename = interv2path(m_iu->get_env(), m_intervset);
			filename += "/";
			filename += m_iu->id2chrom(chromid);
			SEXP rintervals = RSaneUnserialize(filename.c_str());
			rprotect(rintervals);
			m_iu->convert_rintervs(rintervals, &m_intervals, NULL);
			runprotect(rintervals);
			
			// set udata
			uint64_t offset = 0;
			for (int i = 0; i < chromid; ++i)
				offset += m_orig_chrom2size[i];
			for (GIntervals::iterator iinterval = m_intervals.begin(); iinterval < m_intervals.end(); ++iinterval) 
				iinterval->udata = (void *)(intptr_t)(iinterval - m_intervals.begin() + offset);
			
			if (m_do_sort) 
				m_intervals.sort(m_compare);
			
			if (m_do_unify_overlaps) 
				m_intervals.unify_overlaps(m_unify_touching_intervals);
		}
	} else
		m_intervals.clear();
}

void GIntervalsBigSet1D::begin_save(const char *intervset, const IntervUtils &iu, vector<ChromStat> &chromstats)
{
	string path = interv2path(iu.get_env(), intervset);
	if (mkdir(path.c_str(), 0777))
		verror("Cannot create intervals directory at %s: %s", path.c_str(), strerror(errno));

	init_chromstats(chromstats, iu);
}

void GIntervalsBigSet1D::save_chrom_plain_intervals(const char *intervset, GIntervals &intervals, const IntervUtils &iu, vector<ChromStat> &chromstats)
{
	if (intervals.size()) {
		SEXP rintervals = iu.convert_intervs(&intervals);
		save_chrom(intervset, &intervals, rintervals, iu, chromstats);
		intervals.clear();
	}
}

void GIntervalsBigSet1D::save_chrom(const char *intervset, GIntervalsFetcher1D *intervals, SEXP rintervals, const IntervUtils &iu, vector<ChromStat> &chromstats)
{
	if (!intervals->size()) 
		return;

	pair<int, ChromStat> res = get_chrom_stat(intervals);
	int &chromid = res.first;
	ChromStat &chromstat = res.second;
	chromstats[chromid] = chromstat;

	string filename = interv2path(iu.get_env(), intervset);
	filename += "/";
	filename += iu.id2chrom(chromid);
	RSaneSerialize(rintervals, filename.c_str());
}

void GIntervalsBigSet1D::end_save_plain_intervals(const char *intervset, const IntervUtils &iu, const vector<ChromStat> &chromstats)
{
	save_plain_intervals_meta(interv2path(iu.get_env(), intervset).c_str(), chromstats, iu);
}

void GIntervalsBigSet1D::end_save(const char *intervset, SEXP zeroline, const IntervUtils &iu, const vector<ChromStat> &chromstats)
{
	save_meta(interv2path(iu.get_env(), intervset).c_str(), zeroline, chromstats, iu);
}

GIntervalsFetcher1D *GIntervalsBigSet1D::create_masked_copy(const set<int> &chromids_mask) const
{
	GIntervalsBigSet1D *obj = new GIntervalsBigSet1D();

	init_masked_copy(obj, chromids_mask);

	obj->m_intervset = m_intervset;
	obj->m_iu = m_iu;
	obj->m_cur_chromid = obj->m_chrom2size.size();
	obj->m_iter_chrom = -1;
	obj->m_iter_index = -1;
	obj->m_iter_chrom_index = 0;
	obj->m_do_sort = false;
	obj->m_do_unify_overlaps = false;
	obj->m_iinterval = obj->m_intervals.end();

	if (m_do_sort)
		obj->sort(m_compare);

	if (m_do_unify_overlaps) 
		obj->unify_overlaps(m_unify_touching_intervals);

	return obj;
}

void GIntervalsBigSet1D::begin_iter()
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

void GIntervalsBigSet1D::begin_chrom_iter(int chromid)
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

void GIntervalsBigSet1D::sort(Compare_t compare)
{
	m_do_sort = true;
	m_compare = compare;
	if (m_intervals.size())
		m_intervals.sort(m_compare); 
}

void GIntervalsBigSet1D::unify_overlaps(bool unify_touching_intervals)
{
	if (m_do_unify_overlaps && m_unify_touching_intervals == unify_touching_intervals) 
		return;

	m_do_unify_overlaps = true;
	m_unify_touching_intervals = unify_touching_intervals;

	m_size = 0;
	m_range = 0;
	if (m_unify_touching_intervals) {
		m_user_chrom2size = &m_chrom2unified_touching_size;
		for (vector<int64_t>::const_iterator isize = m_chrom2unified_touching_size.begin(); isize < m_chrom2unified_touching_size.end(); ++isize)
			m_size += *isize;
	} else {
		m_user_chrom2size = &m_chrom2unified_overlap_size;
		for (vector<int64_t>::const_iterator isize = m_chrom2unified_overlap_size.begin(); isize < m_chrom2unified_overlap_size.end(); ++isize) 
			m_size += *isize;
	}
	for (vector<int64_t>::const_iterator irange = m_chrom2unified_overlap_range.begin(); irange < m_chrom2unified_overlap_range.end(); ++irange) 
		m_range += *irange;

	if (m_intervals.size())
		m_intervals.unify_overlaps(m_unify_touching_intervals);
}

void GIntervalsBigSet1D::verify_no_overlaps(const GenomeChromKey &chromkey, const char *error_prefix) const
{
	if (m_contains_overlaps)
		TGLError<GIntervalsFetcher1D>(OVERLAPPING_INTERVAL, "%sIntervals set %s contains overlapping intervals", error_prefix, m_intervset.c_str());
}

