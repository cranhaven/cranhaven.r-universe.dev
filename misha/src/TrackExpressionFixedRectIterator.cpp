#include <math.h>

#include "rdbutils.h"
#include "TrackExpressionFixedRectIterator.h"

using namespace rdb;

bool TrackExpressionFixedRectIterator::begin(int64_t width, int64_t height, GIntervalsFetcher2D &scope, const DiagonalBand &band)
{
	TrackExpression2DIterator::begin(scope, band);

	if (width <= 0)
		verror("Width of a fixed rectangle iterator (%ld) must be positive", width);

	if (height <= 0)
		verror("Height of a fixed rectangle iterator (%ld) must be positive", height);

	m_width = width;
	m_height = height;

	m_cur_xbin = m_start_xbin = m_end_xbin = 0;
	m_cur_ybin = m_end_ybin = 0;
	m_minx = m_maxx = m_miny = m_maxy = 0;
	m_using_band = false;
	m_starting_iteration = true;

	m_scope->begin_iter();
	if (m_scope->isend()) 
		end();
	else
		m_last_scope_interval = m_scope->cur_interval();

	return next();
}

bool TrackExpressionFixedRectIterator::next()
{
	if (isend())
		return false;

	while (1) {
		if (m_cur_xbin < m_end_xbin) {
			int64_t coord = m_cur_xbin * m_width;

			m_last_interval.start1() = max(coord, m_minx);
			m_last_interval.end1() = min(coord + m_width, m_maxx);

			if (m_using_band) {
				m_last_interval.start2() = m_miny;
				m_last_interval.end2() = m_maxy;

				if (!m_band.do_contain(m_last_interval))
					m_band.shrink2intersected(m_last_interval);
			}

			++m_cur_xbin;
			return true;
		}

		if (m_cur_ybin < m_end_ybin) {
			int64_t coord = m_cur_ybin * m_height;
			m_last_interval.start2() = max(coord, m_scope_interv.start2());
			m_last_interval.end2() = min(coord + m_height, m_scope_interv.end2());

			// if using band start/end xbins must be recalculated for each new row
			if (m_using_band) {
				Rectangle r(m_scope_interv.start1(), m_last_interval.start2(), m_scope_interv.end1(), m_last_interval.end2());
				m_band.shrink2intersected(r);
				m_minx = r.x1;
				m_maxx = r.x2;
				m_miny = m_last_interval.start2();
				m_maxy = m_last_interval.end2();
				m_start_xbin = (int64_t)(m_minx / (double)m_width);
				m_end_xbin = (int64_t)ceil(m_maxx / (double)m_width);
			}

			m_cur_xbin = m_start_xbin;
			++m_cur_ybin;
			continue;
		}

		if (m_starting_iteration) 
			m_starting_iteration = false;
		else {
			m_scope->next();
			if (!m_scope->isend()) 
				m_last_scope_interval = m_scope->cur_interval();
		}

		if (m_band.is_non_empty_area() && (m_scope->cur_interval().chromid1() != m_scope->cur_interval().chromid2() || !m_band.do_intersect(m_scope->cur_interval())))
			continue;

		if (m_scope->isend())
			break;

		m_last_interval.chromid1() = m_scope->cur_interval().chromid1();
		m_last_interval.chromid2() = m_scope->cur_interval().chromid2();
		m_last_interval.udata() = m_scope->cur_interval().udata();

		m_scope_interv = m_scope->cur_interval();

		if (m_band.is_non_empty_area())
			m_band.shrink2intersected(m_scope_interv);

		m_minx = m_scope_interv.start1();
		m_maxx = m_scope_interv.end1();
		m_miny = m_scope_interv.start2();
		m_maxy = m_scope_interv.end2();

		m_start_xbin = (int64_t)(m_minx / (double)m_width);
		m_cur_xbin = m_end_xbin = (int64_t)ceil(m_maxx / (double)m_width);
		m_cur_ybin = (int64_t)(m_miny / (double)m_height);
		m_end_ybin = (int64_t)ceil(m_maxy / (double)m_height);

		m_using_band = m_band.is_non_empty_area() && !m_band.do_contain(m_scope_interv);
	}

	end();
	return false;
}
