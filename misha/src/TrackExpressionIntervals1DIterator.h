#ifndef TRACKEXPRESSIONINTERVALSITERATOR_H_
#define TRACKEXPRESSIONINTERVALSITERATOR_H_

#include "TrackExpressionIterator.h"

//------------------------------ TrackExpressionIntervalsIterator ---------------------------------

class TrackExpressionIntervals1DIterator : public TrackExpression1DIterator {
public:
	TrackExpressionIntervals1DIterator() : TrackExpression1DIterator(INTERVALS1D) {}

	bool begin(const GIntervals &intervals, GIntervalsFetcher1D &scope);
	virtual bool next();

	const GIntervals *get_intervals() const { return m_intervals; }
	GIntervals::const_iterator get_icur_interval() const { return m_icur_interval; }

protected:
	bool                       m_scope_by_chrom;
	GIntervals                *m_intervals;
	GIntervals::const_iterator m_icur_interval;

	bool check_overlap(GIntervals::const_iterator iinterval, const GInterval &scope_interv);
	bool check_first_overlap(GIntervals::const_iterator iinterval, const GInterval &scope_interv);
};


//------------------------------ IMPLEMENTATION ---------------------------------------------------

inline bool TrackExpressionIntervals1DIterator::check_overlap(GIntervals::const_iterator iinterval, const GInterval &scope_interv)
{
	if (scope_interv.chromid == iinterval->chromid) {
		int64_t overlap_start = max(scope_interv.start, iinterval->start);
		int64_t overlap_end = min(scope_interv.end, iinterval->end);

		if (overlap_start < overlap_end) {
			m_last_interval.start = overlap_start;
			m_last_interval.end = overlap_end;
			m_last_interval.udata = scope_interv.udata;
			m_last_scope_interval = scope_interv;
			return true;
		}
	}
	return false;
}

inline bool TrackExpressionIntervals1DIterator::check_first_overlap(GIntervals::const_iterator iinterval, const GInterval &scope_interv)
{
	if (scope_interv.chromid == iinterval->chromid) {
		int64_t overlap_start = max(scope_interv.start, iinterval->start);
		int64_t overlap_end = min(scope_interv.end, iinterval->end);

		if (overlap_start < overlap_end && (iinterval == m_intervals->begin() || !(iinterval - 1)->do_overlap(scope_interv))) {
			m_last_interval.chromid = iinterval->chromid;
			m_last_interval.start = overlap_start;
			m_last_interval.end = overlap_end;
			m_last_interval.udata = scope_interv.udata;
			m_last_scope_interval = scope_interv;
			return true;
		}
	}
	return false;
}

#endif /* TRACKEXPRESSIONINTERVALSITERATOR_H_ */
