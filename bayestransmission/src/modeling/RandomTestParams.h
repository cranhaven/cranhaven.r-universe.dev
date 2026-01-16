#ifndef ALUN_MODELING_RANDOMTESTPARAMS_H
#define ALUN_MODELING_RANDOMTESTPARAMS_H

#include "TestParams.h"

namespace models{

class RandomTestParams : public TestParams
{
protected:

	int n;
	double *rates;
	double *shapepar;
	double *ratepar;
	double *rateprior;
	double *shapeprior;
	int *updaterate;

public:

	RandomTestParams(int nst);
	~RandomTestParams();
	virtual string header() const override;
// Implement Parameters.

	inline double logProb(infect::HistoryLink *h) override
	{
		double x = TestParams::logProb(h);
		x += log(rates[stateIndex(h->getPState()->infectionStatus())]);
		return x;
	}

	virtual double logProbGap(infect::HistoryLink *g, infect::HistoryLink *h) override;
	inline void initCounts() override
	{
		TestParams::initCounts();
		for (int i=0; i<n; i++)
		{
			ratepar[i] = rateprior[i];
			shapepar[i] = shapeprior[i];
		}
	}

	inline void count(infect::HistoryLink *h) override
	{
		TestParams::count(h);
		shapepar[stateIndex(h->getPState()->infectionStatus())] += 1;
	}

	virtual void countGap(infect::HistoryLink *g, infect::HistoryLink *h) override;
	virtual void update(Random *r, bool max) override;

// Personal accessors.

	virtual void set(int i, double value, int update, double prival, double prin) override;
	virtual void set(bool israte, int i, double value, int update, double prival, double prin);
	virtual int nParam() const override;
    virtual std::vector<std::string> paramNames() const override;
    virtual std::vector<double> getValues() const override;
	virtual void write (ostream &os) const override;
};

} // namespace models

#endif // ALUN_MODELING_RANDOMTESTPARAMS_H
