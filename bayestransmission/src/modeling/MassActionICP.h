#ifndef ALUN_MODELING_MASSACTIONICP_H
#define ALUN_MODELING_MASSACTIONICP_H

#include "InColParams.h"

namespace models {

class MassActionICP : public InColParams
{
private:

	int n;
	double *rates;
	double *logrates;
	double *ratepar;
	double *shapepar;
	double *priorrate;
	double *priorshape;
	int *doit;

	// value 0 == frequency dependent; 1 == density dependent; 2 == constant;
	int isDensity;

	// The following function should be the only thing depending on
	// isDensity in the whole class.
	virtual double acquisitionFactor(int c, int n)
	{
		switch(isDensity)
		{
		case 2: return 1;
		case 1: return c;
		case 0: return (n > 0 ? c / (double) n : 0);
		default:
			return 0;
		}
	}

protected:

	virtual void set(int i, double value);

public:

	MassActionICP(int k, int isdens);
	~MassActionICP();
	virtual string header() const override;
// Implement InColParams.

	virtual double *acquisitionRates(double time, infect::PatientState *p, infect::LocationState *s) override;
	virtual double eventRate(double time, EventCode c, infect::PatientState *p, infect::LocationState *s) override;
	virtual double **rateMatrix(double time, infect::PatientState *p, infect::LocationState *u) override;
// Implement Parameters.

	virtual double logProb(infect::HistoryLink *h) override;
/*
	This will fail if eventRate depends on PatientStatus.
*/
	virtual double logProbGap(infect::HistoryLink *g, infect::HistoryLink *h) override;
	virtual void initCounts() override;
	virtual void count(infect::HistoryLink *h) override;
	virtual void countGap(infect::HistoryLink *g, infect::HistoryLink *h) override;
	virtual void update(Random *r, bool max) override;

// Personal accessors.
	virtual void set(int i, double value, int update, double prival, double prin);
	virtual int nParam() const;
	virtual std::vector<std::string> paramNames() const override;
    virtual std::vector<double> getValues() const override;
	virtual void write (ostream &os) override;
};

} // namespace models

#endif // ALUN_MODELING_MASSACTIONICP_H
