// infect/facility.h
#ifndef ALUN_INFECT_FACILITY_H
#define ALUN_INFECT_FACILITY_H

#include "../util/util.h"
#include "Unit.h"

class Facility : public Object
{
private:

	int number;
	IntMap *unit;

public:
	Facility(int id);
	~Facility();
	void write(ostream &os) const override;

	inline void addUnit(Unit *u)
	{
		unit->put(u->getId(),u);
	}

	inline Unit *getUnit(int i) const
	{
		return (Unit *) unit->get(i);
	}

	inline IntMap *getUnits()
	{
		unit->init();
		return unit;
	}

	inline int getId() const
	{
		return number;
	}
};

#endif // ALUN_INFECT_FACILITY_H
