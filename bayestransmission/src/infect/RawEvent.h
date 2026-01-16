// infect/RawEvent.h
#ifndef ALUN_INFECT_RAWEVENT_H
#define ALUN_INFECT_RAWEVENT_H

#include "../util/util.h"

class RawEvent : public Object
{
private:

	double time;
	int facility;
	int unit;
	int pat;
	int type;

public:
	RawEvent(int f, int u, double t, int p, int tp);

	// RawEvents are sorted first by patient then by time
	// then by unit. This ordering is used by EventData
	// to create the checked lists of Events and Episodes.
	int compare (Object const * const e) const;

	void write(ostream &os) const override;


	inline double getTime() const
	{
		return time;
	}

	inline int getFacilityId() const
	{
		return facility;
	}

	inline int getUnitId() const
	{
		return unit;
	}

	inline int getPatientId() const
	{
		return pat;
	}

	inline int getTypeId() const
	{
		return type;
	}
};

#endif // ALUN_INFECT_RAWEVENT_H
