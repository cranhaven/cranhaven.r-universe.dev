#ifndef ALUN_LOGNORMAL_LOGNORMALCP_H
#define ALUN_LOGNORMAL_LOGNORMALCP_H

#include "../modeling/modeling.h"

class LogNormalICP : public models::InColParams
{
protected:

	// Parameters are log rates.
	// Acquisition parameters are par[0][*].
	// Progression parameters are par[1][*].
	// Clearance parameters are par[2][*].

	static const int ns = 3;
	int *n;
	double **par;  //< Log parameters.
	double **epar; //< Exponential parameters.
	double **primean;  //< Prior mean.
	double **pristdev; //< Prior standard deviation.
	double **sigmaprop; //< Proposal standard deviation.
	int **doit; //< Update flag.
	double tOrigin; //< Time origin.
	int nmetro; //< Number of Metropolis-Hastings iterations.
	Map *m;

	string **pnames;

	inline void setNormal(int i, int j, double x)
	{
		par[i][j] = x;
		epar[i][j] = unTransform(i,j);
	}

	virtual void initParameterNames();

public:

	LogNormalICP(int k, int napar, int nppar, int ncpar, int nmet = 10);
	~LogNormalICP();
	virtual int nParam2(int i) const;
// For models that have a time trend.

	void setTimeOrigin(double t);
	double getTimeOrigin();

	virtual double logpost(Random *r, int max);

// Sufficient to implement LogNormalICP.

	virtual double logAcquisitionRate(double time, PatientState *p, LocationState *s) = 0;
	virtual double logAcquisitionGap(double t0, double t1, LocationState *s) = 0;
	virtual double* acquisitionRates(double time, PatientState *p, LocationState *s) override = 0;

	virtual double logProgressionRate(double time, PatientState *p, LocationState *s) = 0;
	virtual double logProgressionGap(double t0, double t1, LocationState *s) = 0;

	virtual double logClearanceRate(double time, PatientState *p, LocationState *s) = 0;
	virtual double logClearanceGap(double t0, double t1, LocationState *s) = 0;

	virtual double unTransform(int i, int j);

// Personal accessors.

	virtual void set(int i, int j, double value, int update, double prival, double priorn);
	virtual void setWithLogTransform(int i, int j, double value, int update, double prival, double priorn, double sig = 0.1);
	virtual void setWithLogitTransform(int i, int j, double value, int update, double prival, double priorn, double sig = 0.1);
	virtual void setNormal(int i, int j, double value, int update, double prim, double privar, double sig = 0.1);


//implement Object
	virtual void write (ostream &os) override;

	virtual int nParam() const;

// Implement InColParams.

    virtual double eventRate(double time, EventCode c, PatientState *p, LocationState *s) override;
	virtual double **rateMatrix(double time, PatientState *p, LocationState *u) override;

// Implement Parameters.
	virtual std::vector<std::string> paramNames() const override;
	virtual std::vector<double> getValues() const override;
    virtual double logProb(HistoryLink *h) override;
    virtual double logProbGap(HistoryLink *g, HistoryLink *h) override;
	virtual void initCounts() override;
	virtual void count(HistoryLink *h) override;
	virtual void countGap(HistoryLink *g, HistoryLink *h) override;
	virtual void update(Random *r, bool max) override;
};
#endif // ALUN_LOGNORMAL_LOGNORMALCP_H
