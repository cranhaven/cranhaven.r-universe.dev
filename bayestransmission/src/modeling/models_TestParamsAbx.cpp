#include "modeling/modeling.h"

namespace models {
TestParamsAbx::TestParamsAbx(int nst, bool abx) : TestParams(nst)
{
    useabx = abx;

    nstates = nst;
    l = 3;
    m = 2;
    n = 2;

    probs = cleanAlloc(l,m,n);
    logprobs = cleanAlloc(l,m,n);
    counts = cleanAlloc(l,m,n);
    priors = cleanAlloc(l,m,n);
    doit = cleanAllocInt(l,m);

    set(0,0,0,0,0.5,2);
    set(1,0,0,0,0.5,2);
    set(2,0,0.8,1,0.5,2);

    set(0,1,0,0,0.5,2);
    set(1,1,0,0,0.5,2);
    set(2,1,0.8,1,0.5,2);

    initCounts();
}

TestParamsAbx::~TestParamsAbx()
{
    cleanFree(&probs,l,m);
    cleanFree(&logprobs,l,m);
    cleanFree(&counts,l,m);
    cleanFree(&priors,l,m);
    cleanFree(&doit,l);
}

std::vector<std::string> TestParamsAbx::paramNames() const
{
    std::vector<std::string> res(2*nstates);

    if (nstates == 3)
    {
        res[0] = "ATest.P(+|unc-)";
        res[1] = "ATest.P(+|lat-)";
        res[2] = "ATest.P(+|col-)";
        res[3] = "ATest.P(+|unc+)";
        res[4] = "ATest.P(+|lat+)";
        res[5] = "ATest.P(+|col+)";
    }

    if (nstates == 2)
    {
        res[0] = "ATest.P(+|unc-)";
        res[1] = "ATest.P(+|col-)";
        res[2] = "ATest.P(+|unc+)";
        res[3] = "ATest.P(+|col+)";
    }

    return res;
}

double TestParamsAbx::eventProb(InfectionStatus s, int onabx, EventCode e) const
{
    int i = stateIndex(s);
    int j = ( useabx && onabx ? 1 : 0) ;
    int k = testResultIndex(e);
    return ( i < 0 || k < 0 ? 0 : probs[i][j][k] );
}

double* TestParamsAbx::resultProbs(int onabx, EventCode e) const
{
    double *P = cleanAlloc(nstates);

    if (nstates == 2)
    {
        P[0] = eventProb(uncolonized,onabx,e);
        P[1] = eventProb(colonized,onabx,e);
    }

    if (nstates == 3)
    {
        P[0] = eventProb(uncolonized,onabx,e);
        P[1] = eventProb(latent,onabx,e);
        P[2] = eventProb(colonized,onabx,e);
    }

    return P;
}
double TestParamsAbx::logProb(infect::HistoryLink *const h) const
{
    int i = stateIndex(h->getPState()->infectionStatus());
    int j = ( useabx && h->getPState()->onAbx() ? 1 : 0) ;
    int k = testResultIndex(h->getEvent()->getType());

    return ( i < 0 || k < 0 ? 0 : logprobs[i][j][k] );
}

void TestParamsAbx::initCounts()
{
    for (int i=0; i<l; i++)
        for (int j=0; j<m; j++)
            for (int k=0; k<n; k++)
                counts[i][j][k] = priors[i][j][k];
}

void TestParamsAbx::count(infect::HistoryLink * const h)
{
    int i = stateIndex(h->getPState()->infectionStatus());
    int j = ( useabx && h->getPState()->onAbx() ? 1 : 0) ;
    int k = testResultIndex(h->getEvent()->getType());

    if (i >= 0 && k >= 0)
        counts[i][j][k] += 1;
}

void TestParamsAbx::update(Random *r, bool max)
{
    double **newpos = cleanAlloc(l,m);

    for (int i=0; i<l; i++)
        for (int j=0; j<m; j++)
        {
            if (doit[i][j])
            {
                if (max)
                    newpos[i][j] = (counts[i][j][1]-1) / (counts[i][j][1] + counts[i][j][0]-2);
                else
                    newpos[i][j] = r->rbeta(counts[i][j][1],counts[i][j][0]);
            }
            else
                newpos[i][j] = probs[i][j][1];
        }

    for (int i=0; i<l; i++)
        for (int j=0; j<m; j++)
            set(i,j,newpos[i][j]);

    cleanFree(&newpos,l);
}

// Personal accessors.

// Set value, update, and Beta priors.
void TestParamsAbx::set(int i, int j, double value, int update, double prival, double prin)
{
    if (value < 0 || value > 1)
    {
        Rcpp::stop("Can't set probability value outside (0,1): " + std::to_string(value));
    }
    if (prival < 0 || prival > 1)
    {
        Rcpp::stop("Can't set probability prior value outside (0,1): " + std::to_string(prival));
    }
    if (prin < 0)
    {
        Rcpp::stop("Can't set prior observation count negative: " + std::to_string(prin));
    }

    set(i,j,value);

    doit[i][j] = (update != 0);

    priors[i][j][0] = (1-prival)*prin;
    priors[i][j][1] = prival*prin;
}

int TestParamsAbx::nParam() const
{
    return 2*nstates;
}


void TestParamsAbx::write (ostream &os) const
{
    char *buffer = new char[100];

    snprintf(buffer, 100, "%12.10f\t",probs[0][0][1]);
    os << buffer;
    if (nstates == 3)
    {
        snprintf(buffer, 100, "%12.10f\t",probs[1][0][1]);
        os << buffer;
    }
    snprintf(buffer, 100, "%12.10f",probs[2][0][1]);
    os << buffer;

    if (useabx)
    {
        snprintf(buffer, 100, "\t%12.10f\t",probs[0][1][1]);
        os << buffer;
        if (nstates == 3)
        {
            snprintf(buffer, 100, "%12.10f\t",probs[1][1][1]);
            os << buffer;
        }
        snprintf(buffer, 100, "%12.10f",probs[2][1][1]);
        os << buffer;
    }

    delete [] buffer;
}

std::vector<double> TestParamsAbx::getValues() const
{
    std::vector<double> res(2*nstates);

    if (nstates == 3)
    {
        // Return probabilities in order matching paramNames():
        // [P(+|unc-), P(+|lat-), P(+|col-), P(+|unc+), P(+|lat+), P(+|col+)]
        // where probs[i][j][k] = i:state(0=unc,1=lat,2=col), j:abx(0=off,1=on), k:result(0=neg,1=pos)
        res[0] = probs[0][0][1];  // P(+|unc,off)
        res[1] = probs[1][0][1];  // P(+|lat,off)
        res[2] = probs[2][0][1];  // P(+|col,off)
        res[3] = probs[0][1][1];  // P(+|unc,on)
        res[4] = probs[1][1][1];  // P(+|lat,on)
        res[5] = probs[2][1][1];  // P(+|col,on)
    }

    if (nstates == 2)
    {
        // Return probabilities in order matching paramNames():
        // [P(+|unc-), P(+|col-), P(+|unc+), P(+|col+)]
        // where probs[i][j][k] = i:state(0=unc,2=col), j:abx(0=off,1=on), k:result(0=neg,1=pos)
        res[0] = probs[0][0][1];  // P(+|unc,off)
        res[1] = probs[2][0][1];  // P(+|col,off) - NOTE: index 2, not 1!
        res[2] = probs[0][1][1];  // P(+|unc,on)
        res[3] = probs[2][1][1];  // P(+|col,on) - NOTE: index 2, not 1!
    }

    return res;

}

} // namespace models
