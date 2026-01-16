#ifndef ALUN_MODELING_OUTCOLPARAMS_H
#define ALUN_MODELING_OUTCOLPARAMS_H

#include "Parameters.h"
#include <stdexcept>

namespace models {

class OutColParams : public Parameters
{
private:

	int nstates;
	int nmetro;

	Map *admits;
	int countscount;

	double *rates;
	double *priorshape;
	double *priorrate;
	double *P;
	double **I;
	double **Q;
	double **QQ;

	int *doit;

	double sumrates;
	complex<double> l2;
	complex<double> l3;

	double logpost(Random *r, int x);
	/**
	 * Probability of transitioning from state i to state j in time t.
	 *
	 * @param i Previous patient infection state.
	 * @param j Current patient infection state.
	 * @param t Time between points.
	 */
	virtual double prob(int i, int j, double t);
	/// Normalize probability across groups
	void resetPQ();
	virtual void update(Random *r, int nsteps, int max);
protected:

	virtual void set(double *x);
	virtual void set(double a, double b, double c);
	virtual void set(double a, double b);
public:
    using Parameters::update;

	OutColParams(int nst, int nmet);
	~OutColParams();
	virtual double transitionProb(InfectionStatus p, InfectionStatus c, double t);
	double *equilibriumProbs();
	double **rateMatrix();
	virtual void setNMetro(int n);



// Implement Parameters.
	virtual int stateIndex(InfectionStatus s) const override;
	virtual int getNStates() const override;
	virtual double logProb(infect::HistoryLink *h) override;
	virtual void initCounts() override;
	virtual void count(infect::HistoryLink *h) override;
	virtual void update(Random *r, bool max) override;
    virtual std::vector<double> getValues() const override;
	virtual std::vector<std::string> paramNames() const override;

// Personal accessors.
	virtual void set(int i, double value, int update, double prival, double prin);
	virtual int nParam() const;

// Implement Object
	virtual string header() const override;
	void write(ostream &os) const override;
};

} // namespace models

#endif // ALUN_MODELING_OUTCOLPARAMS_H
