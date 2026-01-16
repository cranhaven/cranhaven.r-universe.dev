#include "util/Markov.h"

#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <math.h>
#include <stdexcept>
#include <vector>
using namespace std;

#include <RcppArmadillo.h>
using namespace arma;


namespace util{

void Markov::expQt(int n, double **Q, double t, double **etQ)
{
	arma::mat M(n,n);
	for (int i=0; i<n; i++)
		for (int j=0; j<n; j++)
			M(i,j) = Q[i][j];
	M *= t;

	M = arma::expmat(M);

	for (int i=0; i<n; i++)
		for (int j=0; j<n; j++)
			etQ[i][j] = M(i,j);
}


Markov::Markov (int nstates, int npoints, double *t, double ***Q, double **S, bool *d, Random *r)
{
    rand = r;
    n = npoints;
    ns = nstates;

    x = new checkpoint*[n];
    for (int i=0; i<n; i++)
        x[i] = new checkpoint(i,0,0,0,0,true);
    for (int i=0; i<n-1; i++)
        x[i]->alloc(ns);


    for (int i=0; i<n; i++)
    {
        x[i]->time = t[i];
        x[i]->S = S[i];
        x[i]->Q = Q[i];
        x[i]->doit = d[i];
    }

    double **q = x[0]->Q;

    for (int i=0; i<n-1; i++)
    {
        if (x[i]->Q)
            q = x[i]->Q;

        x[i]->P = new double*[ns];
        for (int j=0; j<ns; j++)
            x[i]->P[j] = new double[ns];
        expQt(ns,q,x[i+1]->time - x[i]->time,x[i]->P);
    }

    collect();
}

Markov::~Markov()
{
    for (int i=0; i<n; i++)
        if (x[i]->P)
        {
            for (int j=0; j<ns; j++)
                delete [] x[i]->P[j];
            delete [] x[i]->P;
        }

    for (int i=0; i<n-1; i++)
        x[i]->dealloc(ns);
    for (int i=0; i<n; i++)
        delete x[i];
    delete [] x;
}

vector<timepoint> Markov::simulateProcess(double **Q, checkpoint *y, checkpoint *z) const
{
    vector<timepoint> v;

    for (int s = -1; s != z->state; )
    {
        v.clear();
        double t = 0;

        for (t = y->time, s = y->state; (t += rand->rexp(-Q[s][s])) <= z->time; )
        {
            s = rand->rint(ns,Q[s]);
            v.push_back(timepoint(t,s,false));
        }
    }

    return v;
}

vector<timepoint> Markov::simulateProcess()
{
    vector<timepoint> v;

    double **Q = x[0]->Q;
    append(&v,x[0]);

    for (int i=1; i<n; i++)
    {
        if (x[i-1]->doit)
        {
            append(&v,simulateProcess(Q,x[i-1],x[i]));
        }
        else
            append(&v,x[i]);
        if (x[i]->Q)
            Q = x[i]->Q;
    }

    return v;
}

double Markov::simulateProcess(int segs, int *ec, double **et, int **es)
{
    simulateChain();

    vector<timepoint> vv = simulateProcess();

    for (int c=0, l=0, i=0, k=0; l<n; c++, l=k+1)
    {
        for (k=l; k<n-1; k++)
            if (!x[k]->doit)
                break;

        vector<timepoint> v;
        v.push_back(vv[i++]);
        while (i < (int) vv.size() && !vv[i].restart)
            v.push_back(vv[i++]);

        ec[c] = (int) v.size();
        et[c] = new double[ec[c]];
        es[c] = new int[ec[c]];

        for (int j=0; j<ec[c]; j++)
        {
            et[c][j] = v[j].time;
            es[c][j] = v[j].state;
        }
    }

    return logProb(vv);
}

double Markov::logProcessProb(int segs, int *ec, double **et, int **es) const
{
    vector<timepoint> v;

    for (int i=0; i<segs; i++)
    {
        v.push_back(timepoint(et[i][0],es[i][0],true));

        for (int j=1; j<ec[i]; j++)
            v.push_back(timepoint(et[i][j],es[i][j],false));
    }

    return logProb(v);
}

double Markov::logChainProb()
{
    double l = log(x[0]->R[x[0]->state]);
    for (int i=1; i<n; i++)
        l += log(x[i-1]->RR[x[i-1]->state][x[i]->state]);
    return l;
}

double Markov::logProb(vector<timepoint> v) const
{
    double l = -logtot;

    double **Q = x[0]->Q;
    double t = v[0].time;
    int s = v[0].state;

    if (x[0]->S)
        l += log(x[0]->S[s]);

    for (int i=1, j=1; i<n; i++)
    {
        if (x[i-1]->doit)
        {
            while (j < (int) v.size() && v[j].time <= x[i]->time && !v[j].restart)
            {
                l +=  rand->logdexp(v[j].time-t,-Q[s][s]);
                t = v[j].time;
                s = v[j].state;
                j++;
            }

            l +=  logpexp(x[i]->time-t,-Q[s][s]);
            t = x[i]->time;
        }
        else
        {
            l += log(x[i-1]->P[s][v[j].state]);
            t = v[j].time;
            s = v[j].state;
            j++;
        }

        if (x[i]->S)
        {
            l += log(x[i]->S[s]);
        }

        if (x[i]->Q)
            Q = x[i]->Q;
    }

    return l;
}

void Markov::collect()
{
    logtot = 0;

    x[n-1]->R = x[n-1]->S;

    for (int i=n-1; i>0; i--)
    {
        double tot = 0;
        x[i-1]->clear(ns);

        for (int j=0; j<ns; j++)
            for (int k = 0; k<ns; k++)
            {
                double z = x[i-1]->P[j][k];

                if (x[i]->R != 0)
                    z *= x[i]->R[k];

                if (x[i-1]->S != 0)
                    z *= x[i-1]->S[j];

                x[i-1]->RR[j][k] = z;
                x[i-1]->R[j] += z;
                tot += z;
            }

        for (int j=0; j<ns; j++)
        {
            for (int k=0; k<ns; k++)
                x[i-1]->RR[j][k] /= x[i-1]->R[j];
            x[i-1]->R[j] /= tot;
        }

        logtot += log(tot);
    }
}

void Markov::simulateChain()
{
    x[0]->state = rand->rint(ns,x[0]->R);
    for (int i=1; i<n; i++)
        x[i]->state = rand->rint(ns,x[i-1]->RR[x[i-1]->state]);
}

void Markov::write(ostream &os) const
{
    for (int i=0; i<n; i++)
    {
        os << x[i]->time << "\t" << x[i]->state << "\n";
        os << "\t" << x[i]->doit << "\n";
        os << "\t" << x[i]->S << "\n";
        os << "\t" << x[i]->Q << "\n";
    }
}



} // namespace util
