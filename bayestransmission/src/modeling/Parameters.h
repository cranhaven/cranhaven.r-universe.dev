#ifndef ALUN_MODELING_PARAMETERS_H
#define ALUN_MODELING_PARAMETERS_H

#include "../infect/infect.h"
#include <string>
#include <vector>

namespace models {

class Parameters : public Object, public infect::EventCoding, public infect::InfectionCoding
{
public:
	virtual string header() const = 0;
	virtual std::vector<std::string> paramNames() const = 0;
	virtual std::vector<double> getValues() const = 0;

	virtual double logProb(infect::HistoryLink *h) = 0;
	virtual double logProbGap(infect::HistoryLink *g, infect::HistoryLink *h) { return 0; }

	virtual void initCounts() = 0;
	virtual void count(infect::HistoryLink *h) = 0;
	virtual void countGap(infect::HistoryLink *g, infect::HistoryLink *h) { }

	virtual void update(Random *r, bool max) = 0;
	virtual void update(Random *r);


	virtual int getNStates() const = 0;
	//virtual int nParam() const = 0;

	virtual int eventIndex(EventCode e);
	virtual int stateIndex(InfectionStatus s) const;
	virtual int testResultIndex(EventCode e) const;
};

} // namespace models

#endif // ALUN_MODELING_PARAMETERS_H
