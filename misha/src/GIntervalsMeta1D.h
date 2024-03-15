#ifndef GINTERVALSMETA1D_H_INCLUDED
#define GINTERVALSMETA1D_H_INCLUDED

#include <cstdint>
#include "GIntervalsFetcher1D.h"
#include "GIntervalsMeta.h"

class GIntervalsMeta1D : public GIntervalsMeta, public GIntervalsFetcher1D {
public:
	enum StatCols {
		CHROM_COL, CONTAINS_OVERLAPS_COL, SIZE_COL, UNIFIED_OVERLAP_SIZE_COL, UNIFIED_TOUCHING_SIZE_COL,
		RANGE_COL, UNIFIED_OVERLAP_RANGE_COL, NUM_STAT_COLS
	};

	static const char *STAT_COL_NAMES[NUM_STAT_COLS];

	struct ChromStat {
		bool    contains_overlaps;
		uint64_t  size;
		uint64_t  unified_overlap_size;
		uint64_t  unified_touching_size;
		int64_t range;
		int64_t unified_overlap_range;

		ChromStat() : contains_overlaps(false), size(0), unified_overlap_size(0), unified_touching_size(0), range(0), unified_overlap_range(0) {}
	};

	GIntervalsMeta1D() : GIntervalsFetcher1D(BIGSET1D) {}

	virtual ~GIntervalsMeta1D() {}

	// same as size(chromid) but faster
	int64_t get_num_intervals(int chromid) const { return (*m_user_chrom2size)[chromid]; }

	static bool is1d(SEXP meta) { return length(VECTOR_ELT(meta, 0)) == NUM_STAT_COLS; }

	static pair<int, ChromStat> get_chrom_stat(GIntervalsFetcher1D *intervals);
	static void init_chromstats(vector<ChromStat> &chromstats, const IntervUtils &iu);
	static void save_plain_intervals_meta(const char *path, const vector<ChromStat> &chromstats, const IntervUtils &iu);
	static void save_meta(const char *path, SEXP zeroline, const vector<ChromStat> &chromstats, const IntervUtils &iu);

	//-------------------------------- GIntervalsFetcher1D interface -----------------------------------

	virtual uint64_t size() const { return m_size; }

	virtual uint64_t size(int chromid) const { return (*m_user_chrom2size)[chromid]; }

	virtual int num_chroms() const;

	virtual int64_t range() const { return m_range; }    // complexity: O(1)
	virtual int64_t range(int chromid) const = 0;   // complexity: O(n)

protected:
	vector<int64_t>            m_chrom2size;
	vector<int64_t>            m_orig_chrom2size;
	vector<int64_t>           *m_user_chrom2size;
	vector<int64_t>            m_chrom2unified_overlap_size;
	vector<int64_t>            m_chrom2unified_touching_size;
	vector<int64_t>            m_chrom2range;
	vector<int64_t>            m_chrom2unified_overlap_range;
	uint64_t                     m_size;
	int64_t                    m_range;
	bool                       m_contains_overlaps;
	GenomeChromKey            *m_chromkey;

	void init(const char *name, SEXP meta, const GenomeChromKey &chromkey);
	void init_masked_copy(GIntervalsMeta1D *obj, const set<int> &chromids_mask) const;

	int chroms2idx(int chromid1, int chromid2) const { return chromid1 * m_chromkey->get_num_chroms() + chromid2; }
	int idx2chrom1(int idx) const { return idx / m_chromkey->get_num_chroms(); }
	int idx2chrom2(int idx) const { return idx % m_chromkey->get_num_chroms(); }
};

//------------------------------------- IMPLEMENTATION ------------------------------------------

inline int GIntervalsMeta1D::num_chroms() const
{
	int res = 0;
	for (vector<int64_t>::const_iterator ichrom2size = m_user_chrom2size->begin(); ichrom2size < m_user_chrom2size->end(); ++ichrom2size) {
		if (*ichrom2size) 
			++res;
	}
	return res;
}

#endif

