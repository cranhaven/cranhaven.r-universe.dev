#include <cstdint>
#include "TrackExpressionBigSet1DIterator.h"

bool TrackExpressionBigSet1DIterator::begin(const char *intervset, SEXP meta, GIntervalsFetcher1D &scope)
{
	TrackExpression1DIterator::begin(scope);
	m_bigset.init(intervset, meta, m_iu);
	m_bigset.sort();
	m_bigset.unify_overlaps(false);
	m_intervals = NULL;
	m_chromid = 0;
	m_scope_by_chrom = true;

	return next();
}

bool TrackExpressionBigSet1DIterator::next()
{
	if (isend())
		return false;

	while ((uint64_t)m_chromid < m_iu.get_chromkey().get_num_chroms()) {
		if (!m_intervals || m_intervals->empty()) {
			if (!m_scope->size(m_chromid) || !m_bigset.size(m_chromid)) {
				++m_chromid;
				continue;
			}

			m_scope->begin_chrom_iter(m_chromid);
			m_bigset.load_chrom(m_chromid);
			m_intervals = (GIntervals *)&m_bigset.get_chrom_intervals();
			m_icur_interval = m_intervals->begin() - 1;
			m_last_interval.chromid = m_chromid;
		}

		if (TrackExpressionIntervals1DIterator::next())
			return true;

		m_isend = false;
		m_intervals = NULL;
		++m_chromid;
	}

	end();
	return false;
}
