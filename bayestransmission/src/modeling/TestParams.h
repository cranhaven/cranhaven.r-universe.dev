#ifndef ALUN_MODELING_TESTPARAMS_H
#define ALUN_MODELING_TESTPARAMS_H

#include "Parameters.h"
#include <vector>
#include <string>
#include <stdexcept>

namespace models {

class TestParams : public Parameters
{
protected:

	int nstates;
	int n;
	int m;

	// nxm matrix with probs[i][j] = P(test result == j | colonization status = i)
	double** probs;
	double** logprobs;
	double** counts;
	double** priors;
	int* doit;

	virtual void set(int i, double value)
	{
		probs[i][0] = 1-value;
		probs[i][1] = value;
		logprobs[i][0] = log(probs[i][0]);
		logprobs[i][1] = log(probs[i][1]);
	}

public:

	TestParams(int nst);
	~TestParams();
	virtual string header() const override;
	virtual int getNStates() const override;
	virtual double eventProb(InfectionStatus s, int onabx, EventCode e) const;
	virtual double* resultProbs(int onabx, EventCode e) const;

// Implement Parameters.

	virtual double logProb(infect::HistoryLink *h) override;
	virtual void initCounts() override;
	virtual void count(infect::HistoryLink *h) override;
	virtual void update(Random *r, bool max = false) override;
	virtual void update_max(Random *r);

// Personal accessors.

	// Set value, update and Beta prior.
	virtual void set(int i, double value, int update, double prival, double prin);
	virtual std::vector<std::string> paramNames() const override;
    virtual std::vector<double> getValues() const override;
	virtual void write (ostream &os) const override;

	virtual int nParam() const
	{
	    return nstates;
	}

	// Accessors for counts matrix (for testing)
	virtual double getCount(int i, int j) const
	{
		if (i < 0 || i >= n || j < 0 || j >= m)
			throw std::out_of_range("Index out of range");
		return counts[i][j];
	}

	virtual void setCount(int i, int j, double value)
	{
		if (i < 0 || i >= n || j < 0 || j >= m)
			throw std::out_of_range("Index out of range");
		counts[i][j] = value;
	}

};

} // namespace models

#endif // ALUN_MODELING_TESTPARAMS_H
