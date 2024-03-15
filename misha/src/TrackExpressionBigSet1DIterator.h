#ifndef TRACKEXPRESSIONBIGSET1DITERATOR_H_
#define TRACKEXPRESSIONBIGSET1DITERATOR_H_

#include "GIntervalsBigSet1D.h"
#include "rdbinterval.h"
#include "TrackExpressionIterator.h"
#include "TrackExpressionIntervals1DIterator.h"

//------------------------------ TrackExpressionBigSet1DIterator ----------------------------------

using namespace rdb;

class TrackExpressionBigSet1DIterator : public TrackExpressionIntervals1DIterator {
public:
	TrackExpressionBigSet1DIterator(const IntervUtils &iu) : m_iu(iu) {}

	bool begin(const char *intervset, SEXP meta, GIntervalsFetcher1D &scope);
	virtual bool next();

private:
	const IntervUtils  &m_iu;
	GIntervalsBigSet1D  m_bigset;
	int                 m_chromid;
};

#endif

