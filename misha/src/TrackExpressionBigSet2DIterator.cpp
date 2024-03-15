#include "TrackExpressionBigSet2DIterator.h"
#include "rdbutils.h"

bool TrackExpressionBigSet2DIterator::begin(const char *intervset, SEXP meta, GIntervalsFetcher2D &scope, const DiagonalBand &band, int max_data_size)
{
	TrackExpression2DIterator::begin(scope, band);

	m_max_data_size = max_data_size;

	m_intersection.clear();
	m_intersected_objs_indices.clear();
	m_iintersection = m_intersection.end();

	m_isend = false;
	m_scope_idx = 0;
	m_start_scope_idx = 0;
	m_scope_started = false;
	m_bigset_started = false;
	m_chromid1 = -1;
	m_chromid2 = -1;

	if (m_scope->size()) {
		m_bigset.init(intervset, meta, m_iu);
		m_bigset.verify_no_overlaps(m_iu.get_chromkey(), "Initializing iterator ");
	}

	return next();
}

bool TrackExpressionBigSet2DIterator::next()
{
	if (isend())
		return false;

	while (1) {
		if (m_iintersection != m_intersection.end()) {
			m_last_interval = GInterval2D(m_chromid1, m_chromid2, *m_iintersection);
			m_scope_chrom_idx = m_intersected_objs_indices[m_iintersection - m_intersection.begin()];
			m_last_scope_interval = m_scope_qtree.get_objs()[m_scope_chrom_idx];
			m_scope_idx = m_start_scope_idx + m_scope_chrom_idx;
			++m_iintersection;
			return true;
		}

		if (m_bigset_started && !m_bigset.isend_chrom()) {
			if (m_band.is_non_empty_area())
				m_scope_qtree.intersect(m_bigset.cur_interval(), m_band, m_intersection, m_intersected_objs_indices);
			else
				m_scope_qtree.intersect(m_bigset.cur_interval(), m_intersection, m_intersected_objs_indices);

			if (m_intersection.empty())
				m_iintersection = m_intersection.end();
			else
				m_iintersection = m_intersection.begin();

			m_bigset.next_in_chrom();
			continue;
		}

		if (m_scope_started && m_scope->isend())
			break;

		if (m_chromid1 < 0)
			m_chromid1 = m_chromid2 = 0;
		else if (!m_scope->get_next_chroms(&m_chromid1, &m_chromid2))
			break;

		if ((m_band.is_non_empty_area() && m_chromid1 != m_chromid2) || !m_scope->size(m_chromid1, m_chromid2) || !m_bigset.size(m_chromid1, m_chromid2)) {
			m_bigset_started = false;
			continue;
		}

		m_bigset.begin_chrom_iter(m_chromid1, m_chromid2);
		m_scope->begin_chrom_iter(m_chromid1, m_chromid2);
		m_bigset_started = true;
		m_scope_started = true;

		m_scope_qtree.reset(0, 0, m_iu.get_chromkey().get_chrom_size(m_chromid1), m_iu.get_chromkey().get_chrom_size(m_chromid2));
		for (; !m_scope->isend_chrom(); m_scope->next_in_chrom()) {
			if (m_scope_qtree.get_num_objs() >= m_max_data_size)
				verror("2D iterator run out of memory limit for storing scope intevals for (%s, %s).\n"
					   "To reduce memory usage please choose a smaller intervals set.\n"
					   "Note: the memory limit is controlled via gmax.data.size option (see options, getOptions).",
					   m_iu.id2chrom(m_scope->cur_interval().chromid1()).c_str(),
					   m_iu.id2chrom(m_scope->cur_interval().chromid2()).c_str());
			m_scope_qtree.insert(m_scope->cur_interval());
		}
	}

	end();
	return false;
}
