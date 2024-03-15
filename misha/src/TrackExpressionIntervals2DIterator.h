#ifndef TRACKEXPRESSIONINTERVALS2DITERATOR_H_
#define TRACKEXPRESSIONINTERVALS2DITERATOR_H_

#include <cstdint>
#include "GenomeChromKey.h"
#include "GIntervals2D.h"
#include "StatQuadTree.h"
#include "TrackExpression2DIterator.h"

//------------------------------ TrackExpressionIntervals2DIterator ---------------------------------

class TrackExpressionIntervals2DIterator : public TrackExpression2DIterator {
public:
	TrackExpressionIntervals2DIterator() : TrackExpression2DIterator(INTERVALS2D) {}

	bool begin(const GenomeChromKey &chromkey, const GIntervals2D &intervals, GIntervalsFetcher2D &scope, const DiagonalBand &band, uint64_t max_data_size);
	virtual bool next();

	virtual int64_t get_cur_scope_idx() const { return m_scope_idx; }
	virtual int64_t get_cur_scope_chrom_idx() const { return m_scope_chrom_idx; }

protected:
	GIntervals2D                 *m_intervals;
	GenomeChromKey               *m_chromkey;
	uint64_t                      m_max_data_size;
	GIntervals2D::const_iterator  m_icur_interval;
	int64_t                       m_scope_idx;
	int64_t                       m_scope_chrom_idx;
	int64_t                       m_start_scope_idx;
	IntervalsQuadTree             m_scope_qtree;
	Rectangles                    m_intersection;
	vector<uint64_t>              m_intersected_objs_indices;
	Rectangles::const_iterator    m_iintersection;
};

#endif /* TRACKEXPRESSIONINTERVALS2DITERATOR_H_ */
