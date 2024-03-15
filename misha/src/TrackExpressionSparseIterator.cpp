#include <cstdint>
#include "TrackExpressionSparseIterator.h"

bool TrackExpressionSparseIterator::begin(const string &track_dir, GIntervalsFetcher1D &scope)
{
	TrackExpression1DIterator::begin(scope);

	m_track_dir = track_dir + "/";
	m_intervals = NULL;
	m_chromid = 0;
	m_scope_by_chrom = true;

	return next();
}

bool TrackExpressionSparseIterator::next()
{
	if (isend())
		return false;

	while ((uint64_t)m_chromid < (uint64_t)m_iu.get_chromkey().get_num_chroms()) {
		if (!m_intervals || m_intervals->empty()) {
			if (!m_scope->size(m_chromid)) {
				++m_chromid;
				continue;
			}

			// const GInterval &scope_interval = m_scope->cur_interval();
			string track_filename = m_track_dir + "/" + GenomeTrack::get_1d_filename(m_iu.get_chromkey(), m_chromid);

			if (m_track_type == GenomeTrack::ARRAYS) {
				m_track_arrays.init_read(track_filename.c_str(), m_chromid);
				m_intervals = (GIntervals *)&m_track_arrays.get_intervals();
			} else if (m_track_type == GenomeTrack::SPARSE) {
				m_track_sparse.init_read(track_filename.c_str(), m_chromid);
				m_intervals = (GIntervals *)&m_track_sparse.get_intervals();
			} else
				TGLError<TrackExpressionSparseIterator>("Unrecognized track type for sparse iterator");

			if (m_intervals->empty()) {
				// No intervals in the track => skip all the scope with the same chromid.
				++m_chromid;
				continue;
			}

			m_scope->begin_chrom_iter(m_chromid);
			m_last_interval.chromid = m_chromid;
			m_icur_interval = m_intervals->begin() - 1;
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
