#ifndef ALUN_MODELING_INSITUPARAMS_H
#define ALUN_MODELING_INSITUPARAMS_H

#include "Parameters.h"
#include <vector>
#include <string>

namespace models {

class InsituParams : public Parameters
{
private:

	int nstates;
	double *probs;
	double *logprobs;
	double *counts;
	double *priors;
	bool *doit;

	virtual double prob(InfectionStatus s) const;

public:

	InsituParams(int nst);
	InsituParams();
    InsituParams(std::vector<double> probs, std::vector<double> priors, std::vector<bool> doit);
	~InsituParams();
	virtual int nParam() const;
	virtual std::vector<std::string> paramNames() const override;
    virtual std::vector<double> getValues() const override;
	virtual string header() const override;
	virtual double *statusProbs() const;
// Implement Parameters.

	virtual double logProb(infect::HistoryLink *h) override;
	virtual void initCounts() override;
	virtual void count(infect::HistoryLink *h) override;
	virtual void update(Random *r, bool max) override;
// Personal accessors.

	inline int getNStates() const override
	{
	    return nstates;
	}

	virtual void set(double u, double l, double c);
    /// Set the prior probabilities for the states.
	inline void setPriors(
	        double pu, /// Prior probability of uncolonized
	        double pl, /// Prior probability of latent
	        double pc) /// Prior probability of colonized
	{
        // Relative values of probs, sum is equivalent number of obs.
        priors[0] = pu;
        priors[1] = pl;
        priors[2] = pc;
    }
	inline void setUpdate(bool u, bool l, bool c)
	{
	    doit[0] = u;
	    doit[1] = l;
	    doit[2] = c;
	}
	virtual void write(ostream &os) const override;
    virtual void init(double p, bool up,
                      double q, bool uq,
                      double r, bool ur);
    static void skipLine(istream &is);
    virtual void read(istream &is);

	// Accessors for counts array (for testing)
	virtual std::vector<double> getCounts() const
	{
		std::vector<double> res(nstates);
		for (int i = 0; i < nstates; i++)
			res[i] = counts[i];
		return res;
	}

	virtual void setCounts(std::vector<double> newCounts)
	{
		if ((int)newCounts.size() != nstates)
			throw std::runtime_error("counts vector size must match nstates");
		for (int i = 0; i < nstates; i++)
			counts[i] = newCounts[i];
	}
};


} // namespace models

#endif // ALUN_MODELING_INSITUPARAMS_H
