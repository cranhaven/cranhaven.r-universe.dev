// infect/Unit.h
#ifndef ALUN_INFECT_UNIT_H
#define ALUN_INFECT_UNIT_H

#include "../util/util.h"

class Unit : public Object
{
private:

	int number;
	Object *f;

public:
	Unit(Object *fac, int id)
	{
		number = id;
		f = fac;
	}

	inline int getId() const
	{
		return number;
	}

	inline string getName() const
	{
		stringstream ss;
		ss << f << ":" << number;
		return ss.str();
	}

	void write(ostream &os) const override
	{
        	os << f << ":" << number;
	}
};

#endif // ALUN_INFECT_UNIT_H
