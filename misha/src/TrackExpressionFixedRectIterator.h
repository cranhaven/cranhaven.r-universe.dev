#ifndef TRACKEXPRESSIONFIXEDRECTITERATOR_H_
#define TRACKEXPRESSIONFIXEDRECTITERATOR_H_

#include "TrackExpression2DIterator.h"

//------------------------------ TrackExpressionFixedRectIterator ---------------------------------

class TrackExpressionFixedRectIterator : public TrackExpression2DIterator {
public:
	TrackExpressionFixedRectIterator() : TrackExpression2DIterator(INTERVALS2D), m_width(0), m_height(0) {}

	bool begin(int64_t width, int64_t height, GIntervalsFetcher2D &scope, const DiagonalBand &band);
	virtual bool next();

	int64_t get_width() const { return m_width; }
	int64_t get_height() const { return m_height; }

private:
	int64_t     m_width;
	int64_t     m_height;
	int64_t     m_cur_xbin;
	int64_t     m_cur_ybin;
	int64_t     m_start_xbin;
	int64_t     m_end_xbin;
	int64_t     m_end_ybin;
	int64_t     m_minx;
	int64_t     m_maxx;
	int64_t     m_miny;
	int64_t     m_maxy;
	bool        m_using_band;
	bool        m_starting_iteration;
	GInterval2D m_scope_interv;
};

#endif /* TRACKEXPRESSIONFIXEDRECTITERATOR_H_ */
