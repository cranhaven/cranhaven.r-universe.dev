#ifndef TRACKEXPRESSIONTRACK2DRECTSITERATOR_H_
#define TRACKEXPRESSIONTRACK2DRECTSITERATOR_H_

#include <cstdint>
#include "GenomeChromKey.h"
#include "GIntervals2D.h"
#include "GenomeTrackComputed.h"
#include "GenomeTrackRects.h"
#include "rdbinterval.h"
#include "StatQuadTree.h"
#include "TrackExpression2DIterator.h"

//------------------------------ TrackExpressionTrackRectsIterator ---------------------------------

using namespace rdb;

class TrackExpressionTrackRectsIterator : public TrackExpression2DIterator {
public:
	TrackExpressionTrackRectsIterator(IntervUtils &iu);

	bool begin(const string &track_dir, GenomeTrack::Type track_type, GIntervalsFetcher2D &scope, const DiagonalBand &band, uint64_t max_data_size);
	virtual bool next();

protected:
	string                        m_track_dir;
	GenomeChromKey               *m_chromkey;
	uint64_t                      m_max_data_size;
	GenomeTrack2D                *m_track;
	GenomeTrackRectsRects         m_track_rects;
	GenomeTrackRectsPoints        m_track_points;
	GenomeTrackComputed           m_track_computed;
	GenomeTrack::Type             m_track_type;
	int64_t                       m_scope_idx;
	int64_t                       m_scope_chrom_idx;
	int64_t                       m_start_scope_idx;
	IntervalsQuadTree             m_scope_qtree;
	bool                          m_scope_started;
	Rectangles                    m_intersection;
	vector<uint64_t>              m_intersected_objs_indices;
	Rectangles::const_iterator    m_iintersection;
	int                           m_chromid1;
	int                           m_chromid2;
};

#endif /* TRACKEXPRESSIONTRACK2DRECTSITERATOR_H_ */
