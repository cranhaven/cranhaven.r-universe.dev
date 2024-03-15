#include "rdbutils.h"
#include "TrackExpressionCartesianGridIterator.h"

using namespace rdb;

bool TrackExpressionCartesianGridIterator::begin(const GenomeChromKey &chromkey, const GIntervals *intervals1, const GIntervals *intervals2,
												 const vector<int64_t> &expansion1, const vector<int64_t> &expansion2,
												 int min_band_idx, int max_band_idx, bool use_band_idx_limit, GIntervalsFetcher2D &scope, const DiagonalBand &band,
												 int max_data_size)
{
	TrackExpression2DIterator::begin(scope, band);

	m_chromkey = (GenomeChromKey *)&chromkey;

	m_max_data_size = max_data_size;
	m_min_band_idx = min_band_idx;
	m_max_band_idx = max_band_idx;
	m_use_band_idx_limit = use_band_idx_limit;

	m_scope->begin_iter();
	m_scope_idx = 0;
	m_scope_chrom_idx = 0;
	m_start_scope_idx = 0;

	m_intersection.clear();
	m_intersected_objs_indices.clear();
	m_iintersection = m_intersection.end();

	// copy expansion, sort and remove duplicates
	m_expansion[0] = expansion1;
	m_expansion[1] = expansion2;
	for (int i = 0; i < 2; ++i) {
		sort(m_expansion[i].begin(), m_expansion[i].end());
		m_expansion[i].resize(unique(m_expansion[i].begin(), m_expansion[i].end()) - m_expansion[i].begin());

		if (m_expansion[i].size() < 2)
			verror("Iterator grid expansion must contain at least two unique values");

		m_iexpansion[i] = m_expansion[i].begin() + 1;
	}

	m_chromid[0] = m_chromid[1] = -1;

	// set grid points
	for (int i = 0; i < 2; ++i) {
		const GIntervals &intervals = !i || !intervals2 ? *intervals1 : *intervals2;
		GridPoints &gpoints = m_gpoints[i];

		gpoints.clear();

		for (GIntervals::const_iterator iinterv = intervals.begin(); iinterv != intervals.end(); ++iinterv)
			gpoints.push_back(GridPoint(iinterv->chromid, (int64_t)((iinterv->end + iinterv->start) * 0.5)));

		// sort gpoints and remove duplicates
		sort(gpoints.begin(), gpoints.end());
		gpoints.resize(unique(gpoints.begin(), gpoints.end()) - gpoints.begin());
		m_igpoint[i] = gpoints.begin();
		m_igpoint_start[i] = gpoints.begin();

		// set min/max expansion
		for (GridPoints::iterator igpoint = gpoints.begin(); igpoint != gpoints.end(); ++igpoint) {
			// check whether two points with their maximal expansions overlap
			if (igpoint != gpoints.begin() && igpoint->chromid == (igpoint - 1)->chromid &&
				igpoint->coord + m_expansion[i].front() < (igpoint - 1)->coord + m_expansion[i].back())
			{
				int64_t mid_coord = (int64_t)((igpoint->coord + (igpoint - 1)->coord) * 0.5);

				if (igpoint->coord + m_expansion[i].front() < mid_coord) {
					if ((igpoint - 1)->coord + m_expansion[i].back() > mid_coord) {
						(igpoint - 1)->max_expansion = mid_coord - (igpoint - 1)->coord;
						igpoint->min_expansion = mid_coord - igpoint->coord;
					} else {
						(igpoint - 1)->max_expansion = m_expansion[i].back();
						igpoint->min_expansion = (igpoint - 1)->coord - igpoint->coord + m_expansion[i].back();
					}
				} else {
					(igpoint - 1)->max_expansion = igpoint->coord - (igpoint - 1)->coord + m_expansion[i].front();
					igpoint->min_expansion = m_expansion[i].front();
				}
			} else
				igpoint->min_expansion = -igpoint->coord;

			igpoint->max_expansion = m_chromkey->get_chrom_size(igpoint->chromid) - igpoint->coord;
		}
	}

	if (m_gpoints[0].empty() || m_gpoints[1].empty() || m_scope->isend())
		end();
	else
		m_scope_chromid[0] = m_scope_chromid[1] = -1;

	return next();
}

bool TrackExpressionCartesianGridIterator::next()
{
	if (isend())
		return false;

	while (1) {
		check_interrupt();

		if (m_iintersection != m_intersection.end()) {
			m_last_interval = GInterval2D(m_igpoint[0]->chromid, m_igpoint[1]->chromid, *m_iintersection);
			m_scope_chrom_idx = m_intersected_objs_indices[m_iintersection - m_intersection.begin()];
			m_last_scope_interval = m_scope_qtree.get_objs()[m_scope_chrom_idx];
			m_scope_idx = m_start_scope_idx + m_scope_chrom_idx;

			++m_iintersection;
			if (m_iintersection == m_intersection.end())
				++m_iexpansion[1];
			return true;
		}

		if (m_igpoint[1] == m_gpoints[1].end() || m_igpoint[1]->chromid != m_igpoint_start[1]->chromid) {
			++m_igpoint[0];

			if (m_igpoint[0] != m_gpoints[0].end() && m_igpoint[0]->chromid == m_igpoint_start[0]->chromid)
				m_igpoint[1] = m_igpoint_start[1];
			else {
				if (m_igpoint[1] == m_gpoints[1].end()) {
					if (m_igpoint[0] == m_gpoints[0].end()) {
						end();
						return false;
					}
					m_igpoint_start[0] = m_igpoint[0];
					m_igpoint[1] = m_igpoint_start[1] = m_gpoints[1].begin();
				} else {
					m_igpoint[0] = m_igpoint_start[0];
					m_igpoint_start[1] = m_igpoint[1];
				}
			}
			continue;
		}

		int delta_idx = (m_igpoint[0] - m_gpoints[0].begin()) - (m_igpoint[1] - m_gpoints[1].begin());
		if (m_iexpansion[0] == m_expansion[0].end() ||
				(m_igpoint[0]->chromid != m_igpoint[1]->chromid && (m_band.is_non_empty_area() || m_use_band_idx_limit)) ||
				(m_use_band_idx_limit && (delta_idx < m_min_band_idx || delta_idx > m_max_band_idx)))
		{
			m_iexpansion[0] = m_expansion[0].begin() + 1;
			m_iexpansion[1] = m_expansion[1].begin() + 1;
			++m_igpoint[1];
			continue;
		}

		int64_t start1 = m_igpoint[0]->coord + max(*(m_iexpansion[0] - 1), m_igpoint[0]->min_expansion);
		int64_t end1 = m_igpoint[0]->coord + min(*m_iexpansion[0], m_igpoint[0]->max_expansion);

		if (m_iexpansion[1] == m_expansion[1].end() || start1 == end1) {
			m_iexpansion[1] = m_expansion[1].begin() + 1;
			++m_iexpansion[0];
			continue;
		}

		int64_t start2 = m_igpoint[1]->coord + max(*(m_iexpansion[1] - 1), m_igpoint[1]->min_expansion);
		int64_t end2 = m_igpoint[1]->coord + min(*m_iexpansion[1], m_igpoint[1]->max_expansion);

		if (start2 == end2) {
			++m_iexpansion[1];
			continue;
		}

		if (m_igpoint[0]->chromid != m_chromid[0] || m_igpoint[1]->chromid != m_chromid[1]) {
			m_chromid[0] = m_igpoint[0]->chromid;
			m_chromid[1] = m_igpoint[1]->chromid;

			if (m_scope_chromid[0] < m_chromid[0] || (m_scope_chromid[0] == m_chromid[0] && m_scope_chromid[1] < m_chromid[1])) {
				m_scope_chromid[0] = m_chromid[0];
				m_scope_chromid[1] = m_chromid[1];
				m_scope->begin_chrom_iter(m_scope_chromid[0], m_scope_chromid[1]);
				if (m_scope->isend()) {
					end();
					return false;
				}
				m_start_scope_idx = m_scope->iter_index();
			}

			if (!m_scope->isend_chrom() && (m_igpoint[0]->chromid == m_igpoint[1]->chromid || (!m_band.is_non_empty_area() && !m_use_band_idx_limit))) {
				m_scope_qtree.reset(0, 0, m_chromkey->get_chrom_size(m_scope_chromid[0]), m_chromkey->get_chrom_size(m_scope_chromid[1]));
				for (; !m_scope->isend_chrom(); m_scope->next_in_chrom()) {
					if (m_scope_qtree.get_num_objs() >= m_max_data_size)
						verror("Cartesian iterator run out of memory limit for storing scope intevals for (%s, %s).\n"
							   "To reduce memory usage please choose a smaller intervals set.\n"
							   "Note: the memory limit is controlled via gmax.data.size option (see options, getOptions).",
							   m_chromkey->id2chrom(m_scope_chromid[0]).c_str(), m_chromkey->id2chrom(m_scope_chromid[1]).c_str());
					m_scope_qtree.insert(m_scope->cur_interval());
				}
			} else
				m_scope_qtree.reset();

			if (m_scope_qtree.empty()) {
				m_iexpansion[0] = m_expansion[0].begin() + 1;
				m_iexpansion[1] = m_expansion[1].begin() + 1;
				// skip the chromosome
				while (m_igpoint[1] != m_gpoints[1].end() && m_igpoint[1]->chromid == m_chromid[1])
					++m_igpoint[1];
				m_igpoint_start[1] = m_igpoint[1];
				continue;
			}
		}

		Rectangle r(start1, start2, end1, end2);

		if (!m_band.is_non_empty_area() || m_band.do_contain(r))
			m_scope_qtree.intersect(r, m_intersection, m_intersected_objs_indices);
		else if (m_band.do_intersect(r))
			m_scope_qtree.intersect(r, m_band, m_intersection, m_intersected_objs_indices);
		else
			m_intersection.clear();

		if (m_intersection.empty()) {
			m_iintersection = m_intersection.end();
			++m_iexpansion[1];
		} else
			m_iintersection = m_intersection.begin();
	}

	end();
	return false;
}
