#ifndef ALUN_MODELING_ABXPARAMS_H
#define ALUN_MODELING_ABXPARAMS_H

#include "Parameters.h"

namespace models {

class AbxParams : public Parameters
{
private:

	int n;
	int nstates;
	double *rates;
	double *ratepar;
	double *shapepar;
	double *priorrate;
	double *priorshape;
	int *doit;

public:

	AbxParams(int k);

	~AbxParams();
	virtual string header() const override;
    virtual std::vector<std::string> paramNames() const override;
	virtual int getNStates() const override;
    virtual int nParam() const;

// Implement Parameters.

	virtual double logProb(infect::HistoryLink *h) override;
	virtual double logProbGap(infect::HistoryLink *g, infect::HistoryLink *h) override;
	virtual void count(infect::HistoryLink *h) override;
	virtual void countGap(infect::HistoryLink *g, infect::HistoryLink *h) override;
	virtual void initCounts() override;
	virtual void update(Random *r, bool max) override;

// Personal accessors.
	virtual void set(int i, double value, int update, double prival, double prin);
    virtual std::vector<double> getValues() const override;
	virtual void write(ostream &os) const override;
};

} // namsepace models

#endif // ALUN_MODELING_ABXPARAMS_H
