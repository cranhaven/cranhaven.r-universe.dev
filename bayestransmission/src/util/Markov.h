// util/Markoi.h
#ifndef ALUN_UTIL_MARKOV_H
#define ALUN_UTIL_MARKOV_H

#include <vector>
#include <iostream>
using std::vector;
using std::ostream;

#include "Object.h"
#include "Random.h"


namespace util{

struct timepoint
{
	double time;
	int state;
	bool restart;

	timepoint(double t, int s, bool r)
	{
		time = t;
		state = s;
		restart = r;
	}
};

struct checkpoint
{
	double time;
	int state;
	double **P;
	double *S;
	double **Q;
	double *R;
	double **RR;
	bool doit;

	checkpoint(double t, int st, double **p, double *s, double **q, bool d)
	{
		time = t;
		state = st;
		P = p;
		S = s;
		Q = q;
		doit = d;

		R = 0;
		RR = 0;
	}

	void alloc(int ns)
	{
		R = new double[ns];
		RR = new double*[ns];
		for (int i=0; i<ns; i++)
			RR[i] = new double[ns];
	}

	void dealloc(int ns)
	{
		delete [] R;
		R = 0;
		for (int i=0; i<ns; i++)
			delete [] RR[i];
		delete [] RR;
		RR = 0;
	}

	void clear(int ns)
	{
		for (int i=0; i<ns; i++)
		{
			R[i] = 0;
			for (int j=0; j<ns; j++)
				RR[i][j] = 0;
		}
	}
};

class Markov : public Object
{
private:
	int n;
	int ns;
	checkpoint **x;
	Random *rand;
	double logtot;

	void expQt(int n, double **Q, double t, double **etQ);

	inline double logpexp(double x, double l) const
	{
		return -l*x;
	}

	inline void append(vector<timepoint> *v, vector<timepoint> vv)
	{
		v->insert(v->end(),vv.begin(),vv.end());
	}

	inline void append(vector<timepoint> *v, checkpoint *x)
	{
		v->push_back(timepoint(x->time,x->state,true));
	}

public:

	Markov (int nstates, int npoints, double *t, double ***Q, double **S, bool *d, Random *r);
	~Markov();
	vector<timepoint> simulateProcess(double **Q, checkpoint *y, checkpoint *z) const;
	vector<timepoint> simulateProcess();
	double simulateProcess(int segs, int *ec, double **et, int **es);
	double logProcessProb(int segs, int *ec, double **et, int **es) const;
	double logChainProb();
	double logProb(vector<timepoint> v) const;
	void collect();
	void simulateChain();

// Implement Object
	void write(ostream &os) const override;
};
} // namespace util
#endif // ALUN_UTIL_MARKOV_H
