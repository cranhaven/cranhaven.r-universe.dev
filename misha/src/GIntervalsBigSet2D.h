#ifndef _GINTERVALSBIGSET2D_H_INCLUDED_
#define _GINTERVALSBIGSET2D_H_INCLUDED_

#include <cstdint>
#include "GIntervals2D.h"
#include "GIntervalsBigSet.h"
#include "GIntervalsFetcher2D.h"
#include "GIntervalsMeta2D.h"
#include "rdbinterval.h"

//------------------------------------- GIntervalsBigSet2D --------------------------------------
// !!!!!!!!! IN CASE OF ERROR THIS CLASS THROWS TGLException  !!!!!!!!!!!!!!!!

class GIntervalsBigSet2D : public GIntervalsBigSet, public GIntervalsMeta2D {
public:
	GIntervalsBigSet2D() {}
	GIntervalsBigSet2D(const char *intervset, SEXP meta, const IntervUtils &iu) { init(intervset, meta, iu); }
	virtual ~GIntervalsBigSet2D() {}

	void init(const char *intervset, SEXP meta, const IntervUtils &iu);

	int64_t get_num_intervals(int chromid1, int chromid2) const { return m_chroms2size[chroms2idx(chromid1, chromid2)]; }

	void load_chrom(int chromid1, int chromid2);

	const GIntervals2D &get_chrom_intervals() { return m_intervals; }

	static pair<ChromPair, ChromStat> get_chrom_stat(GIntervalsFetcher2D *intervals, const IntervUtils &iu);

	static void begin_save(const char *intervset, const IntervUtils &iu, vector<ChromStat> &chromstats);

	// saves generic intervals with optional additional columns
	static void save_chrom(const char *intervset, GIntervalsFetcher2D *intervals, SEXP rintervals, const IntervUtils &iu, vector<ChromStat> &chromstats);

	// ends save for generic intervals with optional additional columns
	static void end_save(const char *intervset, SEXP zeroline, const IntervUtils &iu, const vector<ChromStat> &chromstats);

	// saves plain intervals
	static void save_chrom_plain_intervals(const char *intervset, GIntervals2D &intervals, const IntervUtils &iu, vector<ChromStat> &chromstats);

	// ends save for plain intervals
	static void end_save_plain_intervals(const char *intervset, const IntervUtils &iu, const vector<ChromStat> &chromstats);

	static int chroms2idx(int chromid1, int chromid2, int num_chroms) { return chromid1 * num_chroms + chromid2; }
	static int idx2chrom1(int idx, int num_chroms) { return idx / num_chroms; }
	static int idx2chrom2(int idx, int num_chroms) { return idx % num_chroms; }

	//-------------------------------- GIntervalsFetcher2D interface -----------------------------------

	virtual GIntervalsFetcher2D *create_masked_copy(const set<ChromPair> &chrompairs_mask) const;

	virtual void seal() {}

	virtual void begin_iter();
	virtual void begin_chrom_iter(int chromid1, int chromid2);

	virtual bool next();
	virtual bool next_in_chrom();

	virtual bool isend() const { return (int)m_iter_index == -1 || m_iter_index >= m_size; }
	virtual bool isend_chrom() const { return m_iinterval >= m_intervals.end() || m_cur_chromid != m_iter_chromid; }

	virtual GIntervals2D::const_iterator get_chrom_begin() const { return m_intervals.begin(); }
	virtual GIntervals2D::const_iterator get_chrom_end() const { return m_intervals.end(); }

	virtual uint64_t iter_index() const { return m_iter_index; }
	virtual uint64_t iter_chrom_index() const { return m_iter_chrom_index; }

	virtual const GInterval2D &cur_interval() const { return *m_iinterval; }

	virtual void sort(Compare_t compare = compare_for_sort);

	virtual void verify_no_overlaps(const GenomeChromKey &chromkey, const char *error_prefix = "") const;

private:
	GIntervals2D                 m_intervals;
	GIntervals2D::const_iterator m_iinterval;
	int                          m_cur_chromid;
	int                          m_iter_chromid;
	uint64_t                       m_iter_index;
	uint64_t                       m_iter_chrom_index;
	Compare_t                    m_compare;
	bool                         m_do_sort;

	int chroms2idx(int chromid1, int chromid2) const { return GIntervalsMeta2D::chroms2idx(chromid1, chromid2); }
	int idx2chrom1(int idx) const { return GIntervalsMeta2D::idx2chrom1(idx); }
	int idx2chrom2(int idx) const { return GIntervalsMeta2D::idx2chrom2(idx); }
};


//------------------------------------- IMPLEMENTATION ------------------------------------------

inline bool GIntervalsBigSet2D::next()
{
	++m_iinterval;
	++m_iter_index;
	++m_iter_chrom_index;
	if (m_iinterval >= m_intervals.end()) {
		m_cur_chromid = min(m_cur_chromid + 1, (int)m_chroms2size.size());
		for (; m_cur_chromid < (int)m_chroms2size.size(); ++m_cur_chromid) {
			if (m_chroms2size[m_cur_chromid]) {
				int chromid1 = idx2chrom1(m_cur_chromid);
				int chromid2 = idx2chrom2(m_cur_chromid);

				load_chrom(chromid1, chromid2);
				m_iinterval = m_intervals.begin();
				break;
			}
		}
	}
	return !isend();
}

inline bool GIntervalsBigSet2D::next_in_chrom()
{
	if (isend_chrom()) 
		return false;

	++m_iinterval;
	++m_iter_index;
	++m_iter_chrom_index;
	return !isend_chrom();
}

#endif

