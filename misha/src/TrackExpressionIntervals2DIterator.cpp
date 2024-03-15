#include <cstdint>
#include "rdbutils.h"
#include "TrackExpressionIntervals2DIterator.h"

using namespace rdb;

bool TrackExpressionIntervals2DIterator::begin(const GenomeChromKey &chromkey, const GIntervals2D &intervals, GIntervalsFetcher2D &scope, const DiagonalBand &band, uint64_t max_data_size)
{
	TrackExpression2DIterator::begin(scope, band);

	m_intervals = (GIntervals2D *)&intervals;
	m_chromkey = (GenomeChromKey *)&chromkey;
	m_max_data_size = max_data_size;

	m_icur_interval = m_intervals->begin() - 1;
	m_scope_idx = 0;
	m_start_scope_idx = 0;

	m_intersection.clear();
	m_intersected_objs_indices.clear();
	m_iintersection = m_intersection.end();

    if (m_intervals->empty() || !m_scope->size())
		end();

	return next();
}

bool TrackExpressionIntervals2DIterator::next()
{
	if (isend())
		return false;

	if (m_iintersection != m_intersection.end()) {
		m_last_interval = GInterval2D(m_icur_interval->chromid1(), m_icur_interval->chromid2(), *m_iintersection);
		m_scope_chrom_idx = m_intersected_objs_indices[m_iintersection - m_intersection.begin()];
		m_last_scope_interval = m_scope_qtree.get_objs()[m_scope_chrom_idx];
		m_scope_idx = m_start_scope_idx + m_scope_chrom_idx;
		++m_iintersection;
		return true;
	}

	for (++m_icur_interval; m_icur_interval != m_intervals->end(); ++m_icur_interval) {
		if (m_band.is_non_empty_area() && m_icur_interval->chromid1() != m_icur_interval->chromid2())
			continue;

		if (m_icur_interval == m_intervals->begin() || !m_icur_interval->is_same_chrom(*(m_icur_interval - 1))) {
			int chromid1 = m_icur_interval->chromid1();
			int chromid2 = m_icur_interval->chromid2();

			m_scope->begin_chrom_iter(chromid1, chromid2);

			if (m_scope->isend())
				break;

			m_start_scope_idx = m_scope->iter_index();
			if (m_scope->isend_chrom())
				m_scope_qtree.reset();
			else {
				m_scope_qtree.reset(0, 0, m_chromkey->get_chrom_size(chromid1), m_chromkey->get_chrom_size(chromid2));
				for (; !m_scope->isend_chrom(); m_scope->next_in_chrom()) {
					if (m_scope_qtree.get_num_objs() >= m_max_data_size)
						verror("2D iterator run out of memory limit for storing scope intevals for (%s, %s).\n"
							   "To reduce memory usage please choose a smaller intervals set.\n"
							   "Note: the memory limit is controlled via gmax.data.size option (see options, getOptions).",
							   m_chromkey->id2chrom(m_scope->cur_interval().chromid1()).c_str(),
							   m_chromkey->id2chrom(m_scope->cur_interval().chromid2()).c_str());
					m_scope_qtree.insert(m_scope->cur_interval());
				}
			}
		}

		if (!m_scope_qtree.empty()) {
			if (m_band.is_non_empty_area())
				m_scope_qtree.intersect(*m_icur_interval, m_band, m_intersection, m_intersected_objs_indices);
			else
				m_scope_qtree.intersect(*m_icur_interval, m_intersection, m_intersected_objs_indices);

			if (!m_intersection.empty()) {
				m_iintersection = m_intersection.begin();
				m_last_interval = GInterval2D(m_icur_interval->chromid1(), m_icur_interval->chromid2(), *m_iintersection);
				m_scope_chrom_idx = m_intersected_objs_indices[m_iintersection - m_intersection.begin()];
				m_last_scope_interval = m_scope_qtree.get_objs()[m_scope_chrom_idx];
				m_scope_idx = m_start_scope_idx + m_scope_chrom_idx;
				++m_iintersection;
				return true;
			}
			m_iintersection = m_intersection.end();
		}
	}

	end();
	return false;
}
