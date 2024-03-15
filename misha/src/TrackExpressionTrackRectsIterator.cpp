#include <cstdint>
#include <unistd.h>

#include "TrackExpressionTrackRectsIterator.h"
#include "rdbutils.h"

TrackExpressionTrackRectsIterator::TrackExpressionTrackRectsIterator(IntervUtils &iu) :
	TrackExpression2DIterator(INTERVALS2D),
	m_chromkey((GenomeChromKey *)&iu.get_chromkey()),
	m_track_rects(iu.get_track_chunk_size(), iu.get_track_num_chunks()),
	m_track_points(iu.get_track_chunk_size(), iu.get_track_num_chunks()),
	m_track_computed(rdb::get_groot(iu.get_env()), iu.get_track_chunk_size(), iu.get_track_num_chunks())
{}

bool TrackExpressionTrackRectsIterator::begin(const string &track_dir, GenomeTrack::Type track_type, GIntervalsFetcher2D &scope, const DiagonalBand &band, uint64_t max_data_size)
{
	m_track = NULL;
	m_track_dir = track_dir + "/";
	m_track_type = track_type;

	TrackExpression2DIterator::begin(scope, band);

	m_max_data_size = max_data_size;

	m_intersection.clear();
	m_intersected_objs_indices.clear();
	m_iintersection = m_intersection.end();

	m_isend = false;
	m_scope_idx = 0;
	m_start_scope_idx = 0;
	m_scope_started = false;
	m_chromid1 = -1;
	m_chromid2 = -1;

	return next();
}

bool TrackExpressionTrackRectsIterator::next()
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

		if (m_track && !m_track->is_end_interval()) {
			if (m_band.is_non_empty_area())
				m_scope_qtree.intersect(m_track->cur_interval(), m_band, m_intersection, m_intersected_objs_indices);
			else
				m_scope_qtree.intersect(m_track->cur_interval(), m_intersection, m_intersected_objs_indices);

			if (m_intersection.empty())
				m_iintersection = m_intersection.end();
			else
				m_iintersection = m_intersection.begin();

			m_track->next_interval();
			continue;
		}

		if (m_scope_started && m_scope->isend())
			break;

		if (m_chromid1 < 0)
			m_chromid1 = m_chromid2 = 0;
		else if (!m_scope->get_next_chroms(&m_chromid1, &m_chromid2))
			break;

		string track_filename = m_track_dir + "/" + GenomeTrack::get_2d_filename(*m_chromkey, m_chromid1, m_chromid2);

		if ((m_band.is_non_empty_area() && m_chromid1 != m_chromid2) || !m_scope->size(m_chromid1, m_chromid2) ||
			(access(track_filename.c_str(), R_OK) < 0 && errno == ENOENT))
		{
			m_track = NULL;
			continue;
		}

		if (m_track_type == GenomeTrack::RECTS) {
			m_track_rects.init_read(track_filename.c_str(), m_chromid1, m_chromid2);
			m_track = &m_track_rects;
		} else if (m_track_type == GenomeTrack::POINTS) {
			m_track_points.init_read(track_filename.c_str(), m_chromid1, m_chromid2);
			m_track = &m_track_points;
		} else if (m_track_type == GenomeTrack::COMPUTED) {
			m_track_computed.init_read(track_filename.c_str(), m_chromid1, m_chromid2);
			m_track = &m_track_computed;
		} else
			verror("Invalid track type %d used in TrackExpressionTrackRectsIterator", (int)m_track_type);

		if (m_track->begin_interval()) {
			m_scope->begin_chrom_iter(m_chromid1, m_chromid2);
			m_scope_started = true;
		}
		else {
			m_track = NULL;
			continue;
		}

		m_scope_qtree.reset(0, 0, m_chromkey->get_chrom_size(m_chromid1), m_chromkey->get_chrom_size(m_chromid2));
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

	end();
	return false;
}

