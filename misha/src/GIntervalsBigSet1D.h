#ifndef _GINTERVALSBIGSET1D_H_INCLUDED_
#define _GINTERVALSBIGSET1D_H_INCLUDED_

#include <cstdint>
#include "GInterval.h"
#include "GIntervalsBigSet.h"
#include "GIntervalsMeta1D.h"
#include "rdbinterval.h"

//------------------------------------- GIntervalsBigSet1D --------------------------------------
// !!!!!!!!! IN CASE OF ERROR THIS CLASS THROWS TGLException  !!!!!!!!!!!!!!!!

class GIntervalsBigSet1D : public GIntervalsBigSet, public GIntervalsMeta1D {
public:
	GIntervalsBigSet1D() {}
	GIntervalsBigSet1D(const char *intervset, SEXP meta, const IntervUtils &iu) { init(intervset, meta, iu); }
	virtual ~GIntervalsBigSet1D() {}

	void init(const char *intervset, SEXP meta, const IntervUtils &iu);

	void load_chrom(int chromid);

	const GIntervals &get_chrom_intervals() { return m_intervals; }

	static void begin_save(const char *intervset, const IntervUtils &iu, vector<ChromStat> &chromstats);

	// saves generic intervals with optional additional columns
	static void save_chrom(const char *intervset, GIntervalsFetcher1D *intervals, SEXP rintervals, const IntervUtils &iu, vector<ChromStat> &chromstats);

	// ends save for generic intervals with optional additional columns
	static void end_save(const char *intervset, SEXP zeroline, const IntervUtils &iu, const vector<ChromStat> &chromstats);

	// saves plain intervals
	static void save_chrom_plain_intervals(const char *intervset, GIntervals &intervals, const IntervUtils &iu, vector<ChromStat> &chromstats);

	// ends save for plain intervals
	static void end_save_plain_intervals(const char *intervset, const IntervUtils &iu, const vector<ChromStat> &chromstats);

	//-------------------------------- GIntervalsFetcher1D interface -----------------------------------

	virtual GIntervalsFetcher1D *create_masked_copy(const set<int> &chromids_mask) const;

	virtual void seal() {}

	virtual int64_t range(int chromid) const { return m_do_unify_overlaps ? m_chrom2unified_overlap_range[chromid] : m_chrom2range[chromid]; } // complexity: O(1)

	virtual void begin_iter();

	virtual void begin_chrom_iter(int chromid);

	virtual bool next();
	virtual bool next_in_chrom();

	virtual bool isend() const { return (int)m_iter_index == -1 || m_iter_index >= m_size; }
	virtual bool isend_chrom() const { return m_iinterval >= m_intervals.end() || m_cur_chromid != m_iter_chrom; }

	virtual GIntervals::const_iterator get_chrom_begin() const { return m_intervals.begin(); }
	virtual GIntervals::const_iterator get_chrom_end() const { return m_intervals.end(); }

	virtual uint64_t iter_index() const { return m_iter_index; }

	virtual uint64_t iter_chrom_index() const { return m_iter_chrom_index; }

	virtual const GInterval &cur_interval() const { return *m_iinterval; }

	virtual void sort(Compare_t = compare_by_start_coord);

	virtual void unify_overlaps(bool unify_touching_intervals = true);

	virtual void verify_no_overlaps(const GenomeChromKey &chromkey, const char *error_prefix = "") const;

private:
	GIntervals                 m_intervals;
	GIntervals::const_iterator m_iinterval;
	int                        m_cur_chromid;
	int                        m_iter_chrom;
	uint64_t                     m_iter_index;
	uint64_t                     m_iter_chrom_index;
	Compare_t                  m_compare;
	bool                       m_do_sort;
	bool                       m_do_unify_overlaps;
	bool                       m_unify_touching_intervals;
};

//------------------------------------- IMPLEMENTATION ------------------------------------------

inline bool GIntervalsBigSet1D::next()
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

inline bool GIntervalsBigSet1D::next_in_chrom()
{
	if (isend_chrom()) 
		return false;

	++m_iinterval;
	++m_iter_index;
	++m_iter_chrom_index;
	return !isend_chrom();
}

#endif

