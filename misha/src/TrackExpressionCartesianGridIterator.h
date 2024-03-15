#ifndef TRACKEXPRESSIONCARTESIANGRIDITERATOR_H_
#define TRACKEXPRESSIONCARTESIANGRIDITERATOR_H_

#include <cstdint>
#include "GenomeChromKey.h"
#include "GenomeTrackRects.h"
#include "StatQuadTree.h"
#include "TrackExpression2DIterator.h"

//------------------------------ TrackExpressionCartesianGridIterator ---------------------------------

class TrackExpressionCartesianGridIterator : public TrackExpression2DIterator {
public:
	TrackExpressionCartesianGridIterator() : TrackExpression2DIterator(INTERVALS2D) {}

	bool begin(const GenomeChromKey &chromkey, const GIntervals *intervals1, const GIntervals *intervals2,
			   const vector<int64_t> &expansion1, const vector<int64_t> &expansion2,
			   int min_band_idx, int max_band_idx, bool use_band_idx_limit, GIntervalsFetcher2D &scope, const DiagonalBand &band, int max_data_size);

	virtual bool next();

	virtual int64_t get_cur_scope_idx() const { return m_scope_idx; }
	virtual int64_t get_cur_scope_chrom_idx() const { return m_scope_chrom_idx; }

protected:
	struct GridPoint {
		int     chromid;
		int64_t coord;
		int64_t min_expansion;
		int64_t max_expansion;

		GridPoint() : chromid(-1), coord(-1), min_expansion(0), max_expansion(0) {}
		GridPoint(int _chromid, int64_t _coord) : chromid(_chromid), coord(_coord), min_expansion(0), max_expansion(0) {}

		bool operator<(const GridPoint &o) const { return chromid < o.chromid || (chromid == o.chromid && coord < o.coord); }
		bool operator==(const GridPoint &o) const { return chromid == o.chromid && coord == o.coord; }
	};

	typedef vector<GridPoint> GridPoints;

	vector<int64_t>                    m_expansion[2];
	int                                m_min_band_idx;
	int                                m_max_band_idx;
	bool                               m_use_band_idx_limit;
	GenomeChromKey                    *m_chromkey;
	uint64_t                           m_max_data_size;
	int                                m_chromid[2];
	int                                m_scope_chromid[2];
	GridPoints                         m_gpoints[2];
	GridPoints::const_iterator         m_igpoint[2];
	GridPoints::const_iterator         m_igpoint_start[2]; // iterators that point to the start of chromosomal subset
	vector<int64_t>::const_iterator    m_iexpansion[2];
	IntervalsQuadTree                  m_scope_qtree;
	Rectangles                         m_intersection;
	vector<uint64_t>                   m_intersected_objs_indices;
	Rectangles::const_iterator         m_iintersection;
	int64_t                            m_scope_idx;
	int64_t                            m_scope_chrom_idx;
	int64_t                            m_start_scope_idx;
};

#endif /* TRACKEXPRESSIONCARTESIANGRIDITERATOR_H_ */
