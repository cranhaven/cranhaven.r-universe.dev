#ifndef TRACKEXPRESSIONITERATOR_H_
#define TRACKEXPRESSIONITERATOR_H_

#include "GIntervals.h"
#include "GInterval2D.h"
#include "GIntervalsFetcher1D.h"
#include "GIntervalsFetcher2D.h"

#include "TrackExpressionIteratorBase.h"

//------------------------------ TrackExpressionIterator -----------------------------------------

template <typename Interval, typename Intervals>
class TrackExpressionIterator : public TrackExpressionIteratorBase {
public:
	TrackExpressionIterator(Type type) : TrackExpressionIteratorBase(type), m_scope(NULL) {}
	virtual ~TrackExpressionIterator() {}

	virtual int64_t get_cur_scope_idx() const { return m_scope ? m_scope->iter_index() : -1; }
	virtual int64_t get_cur_scope_chrom_idx() const { return m_scope ? m_scope->iter_chrom_index() : -1; }
	const Interval &last_scope_interval() const { return m_last_scope_interval; }
	const Interval &last_interval() const { return m_last_interval; }

	Intervals *get_scope() const { return m_scope; }

protected:
	Interval      m_last_interval;
	Interval      m_last_scope_interval;
	Intervals    *m_scope;

	void begin(Intervals &scope);
	void end();
};

typedef TrackExpressionIterator<GInterval, GIntervalsFetcher1D> TrackExpression1DIterator;
//typedef TrackExpressionIterator<GInterval2D, GIntervals2D> TrackExpression2DIterator;

//------------------------------ IMPLEMENTATION --------------------------------------------------

template <typename Interval, typename Intervals>
void TrackExpressionIterator<Interval, Intervals>::begin(Intervals &scope)
{
	m_scope = (Intervals *)&scope;
	m_isend = false;
}

template <typename Interval, typename Intervals>
void TrackExpressionIterator<Interval, Intervals>::end()
{
	m_isend = true;
	m_last_interval = Interval();
}

#endif
