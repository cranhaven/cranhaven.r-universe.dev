#ifndef _GINTERVALSBIGSET_H_INCLUDED_
#define _GINTERVALSBIGSET_H_INCLUDED_

#include "rdbinterval.h"
#include "GInterval.h"

using namespace rdb;

//------------------------------------- GIntervalsBigSet ----------------------------------------
// !!!!!!!!! IN CASE OF ERROR THIS CLASS THROWS TGLException  !!!!!!!!!!!!!!!!

class GIntervalsBigSet {
public:
	virtual ~GIntervalsBigSet() {}

	static bool isbig(const char *intervset, const IntervUtils &iu);

protected:
	GIntervalsBigSet() : m_iu(NULL) {}
	GIntervalsBigSet(const GIntervalsBigSet &) = delete;
	GIntervalsBigSet &operator=(const GIntervalsBigSet &) = delete;

	IntervUtils *m_iu;
	string       m_intervset;

	void init(const char *intervset, const IntervUtils &iu);
};

#endif

