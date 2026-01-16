// infect/Patient.h
#ifndef ALUN_INFECT_PATIENT_H
#define ALUN_INFECT_PATIENT_H

#include "../util/util.h"

class Patient : public Object
{
private:

	int name;
	int group;
	const static int thou = 1000;

public:
	Patient(int id);

	~Patient(){}

	inline int getId() const
	{
		return name;
	}

	inline void setGroup(int i)
	{
		group = i-thou;
	}

	inline int getGroup() const
	{
		return group+thou;
	}

	void write(ostream &os) const override;
};

#endif // ALUN_INFECT_PATIENT_H
