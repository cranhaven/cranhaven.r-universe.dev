#ifndef TRACKEXPRESSION2DITERATOR_H_
#define TRACKEXPRESSION2DITERATOR_H_

#include <cstdint>
#include "StatQuadTree.h"
#include "TrackExpressionIterator.h"

class TrackExpression2DIterator : public TrackExpressionIterator<GInterval2D, GIntervalsFetcher2D> {
public:
	TrackExpression2DIterator(Type type) : TrackExpressionIterator<GInterval2D, GIntervalsFetcher2D>(type) {}

	const DiagonalBand &get_band() const { return m_band; }

protected:
	template<typename T>
	struct GInterval2DVal : public GInterval2D {
		T v;

		GInterval2DVal() {}
		GInterval2DVal(const GInterval2D &interval, const T &_v = T()) : GInterval2D(interval), v(_v) {}

		double val(const Rectangle &, void *) const { return v; }
		double val(const Rectangle &, const DiagonalBand &, void *) const { return v; }
	};

	typedef StatQuadTree<GInterval2DVal<float>, uint64_t> IntervalsQuadTree;

	DiagonalBand m_band;

	void begin(GIntervalsFetcher2D &scope, const DiagonalBand &band);
};


//--------------------------------- IMPLEMENTATION -------------------------------------

inline void TrackExpression2DIterator::begin(GIntervalsFetcher2D &scope, const DiagonalBand &band)
{
	m_band = band;
	TrackExpressionIterator<GInterval2D, GIntervalsFetcher2D>::begin(scope);
}

#endif /* TRACKEXPRESSION2DITERATOR_H_ */
