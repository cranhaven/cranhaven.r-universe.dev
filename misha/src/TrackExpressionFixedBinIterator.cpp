#include <math.h>

#include "rdbutils.h"
#include "TrackExpressionFixedBinIterator.h"

using namespace rdb;

bool TrackExpressionFixedBinIterator::begin(int64_t binsize, GIntervalsFetcher1D &scope)
{
	TrackExpression1DIterator::begin(scope);

	if (binsize <= 0)
		verror("Bin size of a fixed bin iterator (%ld) must be positive", binsize);

	m_binsize = binsize;
	m_cur_bin = m_end_bin = -1;
	m_scope->begin_iter();

	if (m_scope->isend())
		end();
	else
		m_last_scope_interval = m_scope->cur_interval();

	return next();
}

bool TrackExpressionFixedBinIterator::next()
{
	if (isend())
		return false;

	if (m_cur_bin != m_end_bin)
		m_cur_bin++;

	if (m_cur_bin == m_end_bin) {
		if (m_cur_bin >= 0) {
			m_scope->next();
			if (!m_scope->isend()) 
				m_last_scope_interval = m_scope->cur_interval();
		}

		if (m_scope->isend()) {
			end();
			return false;
		}

		m_cur_bin = (int64_t)(m_last_scope_interval.start / (double)m_binsize);
		m_end_bin = (int64_t)ceil(m_last_scope_interval.end / (double)m_binsize);

		m_last_interval.chromid = m_last_scope_interval.chromid;
	}

	int64_t coord = m_cur_bin * m_binsize;
	m_last_interval.start = max(coord, m_last_scope_interval.start);
	m_last_interval.end = min(coord + m_binsize, m_last_scope_interval.end);

	return true;
}
