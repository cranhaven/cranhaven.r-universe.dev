#ifndef _GINTERVALSSET_H_INCLUDED_
#define _GINTERVALSSET_H_INCLUDED_

#include "GInterval.h"

//------------------------------------- GIntervalsSet -------------------------------------------
// !!!!!!!!! IN CASE OF ERROR THIS CLASS THROWS TGLException  !!!!!!!!!!!!!!!!

class GIntervalsSet {
public:
	virtual ~GIntervalsSet() {}
};


//------------------------------------- GIntervalsSetSmall --------------------------------------

class GIntervalsSetSmall : public GIntervalsSet {
public:
	GIntervalsSetSmall() {}

	virtual ~GIntervalsSetSmall() {}



private:
	GIntervals m_intervals;
};

#endif
