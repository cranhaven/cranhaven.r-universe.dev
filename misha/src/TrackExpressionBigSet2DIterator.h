#ifndef TRACKEXPRESSIONBIGSET2DITERATOR_H_
#define TRACKEXPRESSIONBIGSET2DITERATOR_H_

#include <cstdint>
#include "GIntervalsBigSet2D.h"
#include "rdbinterval.h"
#include "TrackExpression2DIterator.h"

//------------------------------ TrackExpressionBigSet2DIterator ----------------------------------

using namespace rdb;

class TrackExpressionBigSet2DIterator : public TrackExpression2DIterator {
public:
	TrackExpressionBigSet2DIterator(const IntervUtils &iu) : TrackExpression2DIterator(INTERVALS2D), m_iu(iu) {}

	bool begin(const char *intervset, SEXP meta, GIntervalsFetcher2D &scope, const DiagonalBand &band, int max_data_size);
	virtual bool next();

private:
	const IntervUtils            &m_iu;
	GIntervalsBigSet2D            m_bigset;
	uint64_t                      m_max_data_size;
	int64_t                       m_scope_idx;
	int64_t                       m_scope_chrom_idx;
	int64_t                       m_start_scope_idx;
	IntervalsQuadTree             m_scope_qtree;
	bool                          m_scope_started;
	bool                          m_bigset_started;
	Rectangles                    m_intersection;
	vector<uint64_t>              m_intersected_objs_indices;
	Rectangles::const_iterator    m_iintersection;
	int                           m_chromid1;
	int                           m_chromid2;
};

#endif


