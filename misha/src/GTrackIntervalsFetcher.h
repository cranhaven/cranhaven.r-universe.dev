#ifndef GTRACK_INTERVALS_FETCHER_H_INCLUDED
#define GTRACK_INTERVALS_FETCHER_H_INCLUDED

#include "rdbinterval.h"

using namespace rdb;

//------------------------------------- GTrackIntervalsFetcher ----------------------------------------
// !!!!!!!!! IN CASE OF ERROR THIS CLASS THROWS TGLException  !!!!!!!!!!!!!!!!

class GTrackIntervalsFetcher {
public:
	virtual ~GTrackIntervalsFetcher() {}

	static bool isbig(const char *track_name, const IntervUtils &iu);

	static void create_track_meta(const char *track_name, const IntervUtils &iu);

protected:
	GTrackIntervalsFetcher() : m_iu(NULL) {}
	GTrackIntervalsFetcher(const GTrackIntervalsFetcher &) = delete;
	GTrackIntervalsFetcher &operator=(const GTrackIntervalsFetcher &) = delete;

	IntervUtils *m_iu;
	string       m_track_name;

	void init(const char *track_name, const IntervUtils &iu);
};

#endif

