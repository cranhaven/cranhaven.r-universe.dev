#include "modeling/modeling.h"

namespace models {

double InsituParams::prob(InfectionStatus s) const
{
    int i = stateIndex(s);
    return ( i >=0 ? probs[i] : 0 );
}

InsituParams::InsituParams(int nst)
{
    nstates = nst;

    probs = new double[3];
    logprobs = new double[3];
    counts = new double[3];
    priors = new double[3];
    doit = new bool[3];

    switch(nstates)
    {
    case 2:
        setUpdate(1,0,1);
        set(0.9,0,0.1);
        break;
    case 3:
        set(0.98,0.01,0.01);
        setUpdate(1,1,1);
        break;
    }

    setPriors(1,1,1);
    initCounts();
}

InsituParams::InsituParams():nstates(0)
{
    probs = 0;
    logprobs = 0;
    counts = 0;
    priors = 0;
    doit = 0;
}

InsituParams::InsituParams(std::vector<double> probs, std::vector<double> priors, std::vector<bool> doit)
{
    nstates = probs.size();

    this->probs = new double[nstates];
    this->logprobs = new double[nstates];
    this->counts = new double[nstates];
    this->priors = new double[nstates];
    this->doit = new bool[3];

    // Check if probabilities sum to one and normalize if not
    double sum = 0.0;
    for (int i = 0; i < 3; i++)
    {
        sum += probs[i];
    }

    for (int i = 0; i < 3; i++)
    {
        this->probs[i] = probs[i] / sum;
        this->logprobs[i] = log(this->probs[i]);
        this->priors[i] = priors[i];
        this->doit[i] = doit[i];
    }

    initCounts();
}

InsituParams::~InsituParams()
{
    if (probs)
        delete [] probs;
    if (logprobs)
        delete [] logprobs;
    if (counts)
        delete [] counts;
    if (priors)
        delete [] priors;
    if (doit)
        delete [] doit;
}

int InsituParams::nParam() const
{
    return nstates;
}

std::vector<std::string> InsituParams::paramNames() const
{
    std::vector<std::string> res(nstates);

    if (nstates == 3)
    {
        res[0] = "Insit.P(unc)";
        res[1] = "Insit.P(lat)";
        res[2] = "Insit.P(col)";
    }
    if (nstates == 2)
    {
        res[0] = "Insit.P(unc)";
        res[1] = "Insit.P(col)";
    }

    return res;
}
std::vector<double> InsituParams::getValues() const
{
    std::vector<double> vals;
    vals.push_back(probs[0]);
    if(nstates == 3)
        vals.push_back(probs[1]);
    vals.push_back(probs[2]);
    return vals;
}

string InsituParams::header() const
{
    auto names = paramNames();
    stringstream s;
    s << names[0] << "\t";
    if (nstates == 3)
        s << names[1] << "\t";
    s << names[2];

    return s.str();
}

double* InsituParams::statusProbs() const
{
    double *P = new double[nstates];

    if (nstates == 2)
    {
        P[0] = prob(uncolonized);
        P[1] = prob(colonized);
    }

    if (nstates == 3)
    {
        P[0] = prob(uncolonized);
        P[1] = prob(latent);
        P[2] = prob(colonized);
    }

    return P;
}

// Implement Parameters.

double InsituParams::logProb(infect::HistoryLink *h)
{
    int i = stateIndex(h->getPState()->infectionStatus());
    return ( i >= 0 ? logprobs[i] : 0);
}

void InsituParams::initCounts()
{
    for (int i=0; i<3; i++)
        counts[i] = priors[i];
}

void InsituParams::count(infect::HistoryLink *h)
{
    int i = stateIndex(h->getPState()->infectionStatus());
    if (i >= 0)
        counts[i] += 1;
}

void InsituParams::update(Random *r, bool max)
{
    double t = 0;
    double *cc = new double[3];

    for (int i=0; i<3; i++)
        cc[i] = probs[i];

    if (max)
    {
        for (int i=0; i<3; i++)
        {
            if (doit[i])
                cc[i] = counts[i] - 1;
            t += cc[i];
        }
    }
    else
    {
        for (int i=0; i<3; i++)
        {
            if (doit[i])
                cc[i] = r->rgamma(counts[i],1);
            t += cc[i];
        }
    }

    set(cc[0]/t,cc[1]/t,cc[2]/t);

    delete [] cc;
}

// Personal accessors.

void InsituParams::set(double u, double l, double c)
{
    double tot = (u+l+c);
    probs[0] = u/tot;
    probs[1] = l/tot;
    probs[2] = c/tot;
    // Clamp probabilities to avoid log(0) = -inf
    // For 2-state models, latent probability will be 0, but we still need a finite logprobs[1]
    const double eps = 1e-300;  // Very small but still > 0
    for (int i=0; i<3; i++)
        logprobs[i] = log(std::max(probs[i], eps));
}

void InsituParams::write(ostream &os) const
{
    char *buffer = new char[100];
    snprintf(buffer, 100, "%12.10f\t",probs[0]);
    os << buffer;

    if (nstates == 3)
    {
        snprintf(buffer, 100, "%12.10f\t",probs[1]);
        os << buffer;
    }

    snprintf(buffer, 100, "%12.10f",probs[2]);
    os << buffer;

    delete [] buffer;
}

void InsituParams::init(double p, bool up,
                  double q, bool uq,
                  double r, bool ur)
{
    double tot = p+q+r;
    r = r/tot;
    q = q/tot;
    p = 1-q-r;
    //up = ur;

    set(p,q,r);
    setPriors(up?p:0, uq?q:0, ur?r:0);
    setUpdate((up>0),(uq>0),(ur>0));
}
void InsituParams::skipLine(istream &is)
{
    char c;
    do
    {
        c = is.get();
    }
    while (c != '\n' && c != EOF);
}
void InsituParams::read(istream &is)
{
    string sdump;
    double p,q=0,r;
    double up, uq=0, ur;
    is >> sdump >> p >> up;
    skipLine(is);

    if (nstates == 3)
    {
        is >> sdump >> q >> uq;
        skipLine(is);
    }
    is >> sdump >> r >> ur;
    skipLine(is);

    init(p, up, q, uq, r, ur);
}


} // namespace models
