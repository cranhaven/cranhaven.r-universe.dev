#ifndef GINTERVALSMETA2D_H_INCLUDED
#define GINTERVALSMETA2D_H_INCLUDED

#include <cstdint>
#include "GIntervalsFetcher2D.h"
#include "GIntervalsMeta.h"

class GIntervalsMeta2D : public GIntervalsMeta, public GIntervalsFetcher2D {
public:
	enum StatCols {
		CHROM1_COL, CHROM2_COL, CONTAINS_OVERLAPS_COL, SIZE_COL, SURFACE_COL, NUM_STAT_COLS
	};

	static const char *STAT_COL_NAMES[NUM_STAT_COLS];

	struct ChromStat {
		bool    contains_overlaps;
		uint64_t  size;
		double  surface;

		ChromStat() : contains_overlaps(false), size(0), surface(0.) {}
	};

	GIntervalsMeta2D() : GIntervalsFetcher2D(BIGSET2D) {}

	virtual ~GIntervalsMeta2D() {}

	static bool is2d(SEXP meta) { return length(VECTOR_ELT(meta, 0)) == NUM_STAT_COLS; }

	static void init_chromstats(vector<ChromStat> &chromstats, const IntervUtils &iu);
	static void save_plain_intervals_meta(const char *path, const vector<ChromStat> &chromstats, const IntervUtils &iu);
	static void save_meta(const char *path, SEXP zeroline, const vector<ChromStat> &chromstats, const IntervUtils &iu);

	//-------------------------------- GIntervalsFetcher2D interface -----------------------------------

	virtual uint64_t size() const { return m_size; }

	virtual uint64_t size(int chromid1, int chromid2) const { return m_chroms2size[chroms2idx(chromid1, chromid2)]; }

	virtual int num_chrom_pairs() const;

	virtual double surface() const { return m_surface; } // complexity: O(1)
	virtual double surface(int chromid1, int chromid2) const { return m_surfaces[chroms2idx(chromid1, chromid2)]; } // complexity: O(1)

	virtual bool get_next_chroms(int *chromid1, int *chromid2);

protected:
	vector<int64_t>              m_chroms2size;
	vector<int64_t>              m_orig_chroms2size;
	vector<double>               m_surfaces;
	vector<bool>                 m_contains_overlaps;
	uint64_t                       m_size;
	double                       m_surface;
	GenomeChromKey              *m_chromkey;

	void init(const char *name, SEXP meta, const GenomeChromKey &chromkey);
	void init_masked_copy(GIntervalsMeta2D *obj, const set<ChromPair> &chrompairs_mask) const;

	int chroms2idx(int chromid1, int chromid2) const { return chromid1 * m_chromkey->get_num_chroms() + chromid2; }
	int idx2chrom1(int idx) const { return idx / m_chromkey->get_num_chroms(); }
	int idx2chrom2(int idx) const { return idx % m_chromkey->get_num_chroms(); }
};

//------------------------------------- IMPLEMENTATION ------------------------------------------

inline int GIntervalsMeta2D::num_chrom_pairs() const
{
	int res = 0;
	for (vector<int64_t>::const_iterator ichroms2size = m_chroms2size.begin(); ichroms2size < m_chroms2size.end(); ++ichroms2size) {
		if (*ichroms2size) 
			++res;
	}
	return res;
}

inline bool GIntervalsMeta2D::get_next_chroms(int *chromid1, int *chromid2)
{
	if ((uint64_t)*chromid2 < m_chromkey->get_num_chroms() - 1)
		++*chromid2;
	else {
		++*chromid1;
		*chromid2 = 0;
	}
	return (uint64_t)*chromid1 < m_chromkey->get_num_chroms() && (uint64_t)*chromid2 < m_chromkey->get_num_chroms();
}

#endif

