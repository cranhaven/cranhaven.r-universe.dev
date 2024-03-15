#include "TrackExpressionIntervals1DIterator.h"

bool TrackExpressionIntervals1DIterator::begin(const GIntervals &intervals, GIntervalsFetcher1D &scope)
{
	TrackExpression1DIterator::begin(scope);

	m_intervals = (GIntervals *)&intervals;
	m_icur_interval = m_intervals->begin() - 1;
	m_scope->begin_iter();
	m_scope_by_chrom = false;

	if (m_intervals->empty() || m_scope->isend())
		end();
	else
		m_last_interval.chromid = m_scope->cur_interval().chromid;

	return next();
}

bool TrackExpressionIntervals1DIterator::next()
{
	if (isend())
		return false;

	// check whether the next interval overlaps the old scope
	if (m_icur_interval + 1 < m_intervals->end() && check_overlap(m_icur_interval + 1, m_scope->cur_interval())) {
		++m_icur_interval;
		return true;
	}

	if (m_icur_interval < m_intervals->begin())
		m_icur_interval = m_intervals->begin();
	else {
		// the next interval does not overlap the old scope: since the intervals are in canonic form, we must move to the next scope
		if (m_scope_by_chrom) 
			m_scope->next_in_chrom();
		else
			m_scope->next();
	}

	for (; !m_scope->isend_chrom() || (!m_scope->isend() && !m_scope_by_chrom); m_scope_by_chrom ? m_scope->next_in_chrom() : m_scope->next()) {
		// before launching the binary search try the last interval with the new scope, maybe we're lucky
		if (check_first_overlap(m_icur_interval, m_scope->cur_interval()))
			return true;

		GIntervals::const_iterator istart_interval = m_intervals->begin();
		GIntervals::const_iterator iend_interval = m_intervals->end();

		// scope is sorted; if chromid exceeds the end interval => stop
		if (m_scope->cur_interval().chromid > (iend_interval - 1)->chromid) 
			break;

		// if scope precedes the start interval => give up the search
		if (!istart_interval->do_overlap(m_scope->cur_interval()) && GIntervals::compare_by_start_coord(m_scope->cur_interval(), *istart_interval))
			continue;

		// if scope exceeds the end interval => give up the search
		if (!(iend_interval - 1)->do_overlap(m_scope->cur_interval()) && GIntervals::compare_by_start_coord(*(iend_interval - 1), m_scope->cur_interval()))
			continue;

		// perform binary search
		while (iend_interval - istart_interval > 1) {
			GIntervals::const_iterator imid_interval = istart_interval + (iend_interval - istart_interval) / 2;

			if (check_first_overlap(imid_interval, m_scope->cur_interval())) {
				m_icur_interval = imid_interval;
				return true;
			}

			// is mid_interval < cur_scope?
			if (GIntervals::compare_by_start_coord(*imid_interval, m_scope->cur_interval()))
				istart_interval = imid_interval;
			else
				iend_interval = imid_interval;
		}

		if (check_first_overlap(istart_interval, m_scope->cur_interval())) {
			m_icur_interval = istart_interval;
			return true;
		}
	}

	end();
	return false;
}
