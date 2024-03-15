#ifndef TRACKEXPRESSIONSPARSEITERATOR_H_
#define TRACKEXPRESSIONSPARSEITERATOR_H_

#include "GenomeTrackArrays.h"
#include "GenomeTrackSparse.h"
#include "rdbinterval.h"
#include "TrackExpressionIterator.h"
#include "TrackExpressionIntervals1DIterator.h"

//------------------------------ TrackExpressionIntervalsIterator ---------------------------------

using namespace rdb;

class TrackExpressionSparseIterator : public TrackExpressionIntervals1DIterator {
public:
	TrackExpressionSparseIterator(const IntervUtils &iu, GenomeTrack::Type track_type) : m_iu(iu), m_track_type(track_type) {}

	bool begin(const string &track_dir, GIntervalsFetcher1D &scope);
	virtual bool next();

	GenomeTrackArrays &get_track_arrays() { return m_track_arrays; }
	GenomeTrackSparse &get_track_sparse() { return m_track_sparse; }

private:
	string             m_track_dir;
	const IntervUtils &m_iu;
	GenomeTrack::Type  m_track_type;
	GenomeTrackArrays  m_track_arrays;
	GenomeTrackSparse  m_track_sparse;
	int                m_chromid;
};

#endif /* TRACKEXPRESSIONSPARSEITERATOR_H_ */
