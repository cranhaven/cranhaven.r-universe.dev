#ifndef TRACKEXPRESSIONFIXEDBINITERATOR_H_
#define TRACKEXPRESSIONFIXEDBINITERATOR_H_

#include "TrackExpressionIterator.h"

//------------------------------ TrackExpressionFixedBinIterator ---------------------------------

class TrackExpressionFixedBinIterator : public TrackExpression1DIterator {
public:
	TrackExpressionFixedBinIterator() : TrackExpression1DIterator(FIXED_BIN), m_binsize(0) {}

	bool begin(int64_t binsize, GIntervalsFetcher1D &scope);
	virtual bool next();

	int64_t get_bin_size() const { return m_binsize; }

private:
	int64_t     m_binsize;
	int64_t     m_cur_bin;
	int64_t     m_end_bin;
};

#endif /* TRACKEXPRESSIONFIXEDBINITERATOR_H_ */
