#include "modeling/modeling.h"

namespace models {

TestParams::TestParams(int nst)
{
    nstates = nst;
    n = 3;
    m = 2;

    probs = cleanAlloc(n,m);
    logprobs = cleanAlloc(n,m);
    counts = cleanAlloc(n,m);
    priors = cleanAlloc(n,m);
    doit = cleanAllocInt(n);

    set(0,0,0,0.5,2);
    set(1,0,0,0.5,2);
    set(2,0.8,1,0.5,2);

    initCounts();
}

TestParams::~TestParams()
{
    cleanFree(&probs,n);
    cleanFree(&logprobs,n);
    cleanFree(&counts,n);
    cleanFree(&priors,n);
    cleanFree(&doit);
}

string TestParams::header() const
{
    stringstream s;
    s <<  "Test.Punc";
    if (nstates == 3)
        s << "\t" << "Test.Plat";
    s << "\t" << "Test.Pcol";
    return s.str();
}

int TestParams::getNStates() const
{
    return nstates;
}

double TestParams::eventProb(InfectionStatus s, int onabx, EventCode e) const
{
    int i = stateIndex(s);
    int j = testResultIndex(e);
    return ( i < 0 || j < 0 ? 0 : probs[i][j] );
}

double* TestParams::resultProbs(int onabx, EventCode e) const
{
    double *P = cleanAlloc(nstates);

    if (nstates == 2)
    {
        P[0] = eventProb(uncolonized,0,e);
        P[1] = eventProb(colonized,0,e);
    }

    if (nstates == 3)
    {
        P[0] = eventProb(uncolonized,0,e);
        P[1] = eventProb(latent,0,e);
        P[2] = eventProb(colonized,0,e);
    }

    return P;
}

// Implement Parameters.

double TestParams::logProb(infect::HistoryLink *h)
{
    int i = stateIndex(h->getPState()->infectionStatus());
    int j = testResultIndex(h->getEvent()->getType());
    return ( i < 0 || j < 0 ? 0 : logprobs[i][j] );
}

void TestParams::initCounts()
{
    for (int i=0; i<n; i++)
        for (int j=0; j<m; j++)
            counts[i][j] = priors[i][j];
}

void TestParams::count(infect::HistoryLink *h)
{
    int i = stateIndex(h->getPState()->infectionStatus());
    int j = testResultIndex(h->getEvent()->getType());
    if (i < 0 || j < 0)
        return;

    counts[i][j] += 1;
}

void TestParams::update(Random *r, bool max)
{
    double *newpos = new double[n];

    if (max)
    {
        for (int i=0; i<n; i++)
        {
            if (doit[i])
            {
                double mode = (counts[i][1]-1) / (counts[i][0]+counts[i][1]-2);
                if (counts[i][1] < 1)
                    mode = 0;
                if (counts[i][0] < 1)
                    mode = 1;
                newpos[i] = mode;
            }
            else
            {
                newpos[i] = probs[i][1];
            }
        }
    }
    else
    {
        for (int i=0; i<n; i++)
            newpos[i] = ( doit[i] ? r->rbeta(counts[i][1],counts[i][0]) : probs[i][1] );
    }

    for (int i=0; i<n; i++)
        set(i,newpos[i]);

    delete [] newpos;
}

void TestParams::update_max(Random *r)
{
    update(r, true);
}

// Personal accessors.

// Set value, update and Beta prior.
void TestParams::set(int i, double value, int update, double prival, double prin)
{
    if (value < 0 || value > 1)
    {
        throw std::runtime_error("Error: can't set probablilty value outside of (0,1)\t" + std::to_string(value));
        // cerr << "Error: can't set probablilty value outside of (0,1)\t" << value << "\n";
        // exit(1);
    }
    if (prival < 0 || prival > 1)
    {
        throw std::runtime_error("Error: can't set probablilty value outside of (0,1)\t" + std::to_string(prival));
        // cerr << "Error: can't set probablilty value outside of (0,1)\t" << prival << "\n";
        // exit(1);
    }
    if (prin < 0)
    {
        throw std::runtime_error("Error: can't set prior observations less than 0\t" + std::to_string(prin));
        // cerr << "Error: can't set prior observations less than 0\t" << prin << "\n";
        // exit(1);
    }

    set(i,value);
    //set(i,0.5);

    doit[i] = (update != 0);

    priors[i][0] = (1-prival) * prin;
    priors[i][1] = prival * prin;
}

std::vector<std::string> TestParams::paramNames() const
{
    std::vector<std::string> res(nstates);

    if (nstates == 3)
    {
        res[0] = "Test.P(+|unc)";
        res[1] = "Test.P(+|lat)";
        res[2] = "Test.P(+|col)";
    }

    if (nstates == 2)
    {
        res[0] = "Test.P(+|unc)";
        res[1] = "Test.P(+|col)";
    }

    return res;
}
std::vector<double> TestParams::getValues() const
{
    std::vector<double> res;
    res.push_back(probs[0][1]);
    if(nstates == 3)
        res.push_back(probs[1][1]);
    res.push_back(probs[2][1]);
    return res;

}
void TestParams::write (ostream &os) const
{
    char *buffer = new char[100];
    snprintf(buffer, 100, "%12.10f\t",probs[0][1]);
    os << buffer;
    if (nstates == 3)
    {
        snprintf(buffer, 100, "%12.10f\t",probs[1][1]);
        os << buffer;
    }
    snprintf(buffer, 100, "%12.10f",probs[2][1]);
    os << buffer;
    delete [] buffer;
}

} // namespace models
