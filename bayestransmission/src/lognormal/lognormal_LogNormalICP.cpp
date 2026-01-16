#include "lognormal/lognormal.h"

namespace lognormal{

void LogNormalICP::initParameterNames()
{
    pnames = new string*[ns];
    pnames[0] = new string[n[0]];
    pnames[1] = new string[n[1]];
    pnames[2] = new string[n[2]];

    for (int i=0; i<ns; i++)
    {
        pnames[i] = new string[n[i]];
        for (int j=0; j<n[i]; j++)
            pnames[i][j] = "";
    }

    pnames[0][0] = "NOT";
}

LogNormalICP::LogNormalICP(int k, int napar, int nppar, int ncpar, int nmet) : InColParams(k)
{
    tOrigin = 0;

    nmetro = nmet;

    m = new Map();
    initCounts();

    n = cleanAllocInt(ns);
    n[0] = napar;
    n[1] = nppar;
    n[2] = ncpar;

    initParameterNames();

    par = new double*[ns];
    epar = new double*[ns];
    primean = new double*[ns];
    pristdev = new double*[ns];
    doit = new int*[ns];
    sigmaprop = new double*[ns];

    for (int i=0; i<ns; i++)
    {
        par[i] = cleanAlloc(n[i]);
        epar[i] = cleanAlloc(n[i]);
        primean[i] = cleanAlloc(n[i]);
        pristdev[i] = cleanAlloc(n[i]);
        doit[i] = cleanAllocInt(n[i]);
        for (int j=0; j<n[i]; j++)
            doit[i][j] = 1;
        sigmaprop[i] = cleanAlloc(n[i]);
        for (int j=0; j<n[i]; j++)
            sigmaprop[i][j] = 0.1;
    }
}

LogNormalICP::~LogNormalICP()
{
    delete [] n;

    for (int i=0; i<ns; i++)
    {
        delete [] par[i];
        delete [] epar[i];
        delete [] primean[i];
        delete [] pristdev[i];
        delete [] doit[i];
        delete [] sigmaprop[i];
    }

    delete [] par;
    delete [] epar;
    delete [] primean;
    delete [] pristdev;
    delete [] doit;
    delete [] sigmaprop;
    delete m;
}

int LogNormalICP::nParam2(int i) const
{
    if (i >= 0 && i <=2)
        return n[i];
    else
        return -1;
}

// For models that have a time trend.

void LogNormalICP::setTimeOrigin(double t)
{
    tOrigin = t;
}

double LogNormalICP::getTimeOrigin()
{
    return tOrigin;
}

// Personal accessors.

void LogNormalICP::set(int i, int j, double value, int update, double prival, double priorn)
{
    setWithLogTransform(i,j,value,update,prival,priorn,0.1);
}

double LogNormalICP::unTransform(int i, int j)
{
    return exp(par[i][j]);
}

void LogNormalICP::setWithLogTransform(int i, int j, double value, int update, double prival, double priorn, double sig)
{
    // For rate parameters.
    //
    // The prior is specified as having positive mean prival that was worth priorn observations.
    //
    // A Gamma(a,b) prior can be specified with these properties if
    //	a = prival*priorn
    //	b = priorn.
    //
    // If we transform the variable with a log transformation
    // the mean and variance are
    //	digamma(a) + log(b)
    //and
    // 	trigamma(a)
    //
    // Assume that the transformed variable has a Gaussian distribution with this mean and variance.

    double prin = priorn > 1 ? priorn : 1;
    double a = prival*prin;
    double b = prin;

    double mu = digamma(a) - log(b);
    double s2 = trigamma(a);
    setNormal(i,j,log(value),update,mu,s2,sig);
}

void LogNormalICP::setWithLogitTransform(int i, int j, double value, int update, double prival, double priorn, double sig)
{
    // For probability parameters.
    //
    // The prior is specified as having mean prival, in (0,1), that was worth priorn observations.
    //
    // A Beta(a,b) prior can be specified with these properties if
    //	a = prival * priorn,
    //	b = priorn * (1 - prival)
    //
    // If we transform the variable with a logit transformation
    // the mean and variance are
    //	digamma(a) - digamma(b)
    // 	trigamma(a) + trigamma(b)
    //
    // Assume that the transformed variable has a Gaussian distribution with this mean and variance.
    // Except, use the raw transform as the Gaussian mean!

    // Clamp value to avoid infinity from logit at 0 or 1
    // Use a small epsilon to keep values strictly in (0, 1)
    const double eps = 1e-10;
    double value_safe = value;
    if (value_safe <= eps) value_safe = eps;
    if (value_safe >= 1.0 - eps) value_safe = 1.0 - eps;

    // Clamp prival to avoid NaN from digamma/trigamma at 0
    // Must be strictly in (0, 1)
    double prival_safe = prival;
    if (prival_safe <= eps) prival_safe = eps;
    if (prival_safe >= 1.0 - eps) prival_safe = 1.0 - eps;

    double prin = priorn > 1 ? priorn : 1;
    double a = prival_safe*prin;
    double b = (1-prival_safe)*prin;

    double mu = digamma(a) - digamma(b);
    double s2 = trigamma(a) + trigamma(b);
    setNormal(i,j,logit(value_safe),update,mu,s2,sig);
}

void LogNormalICP::setNormal(int i, int j, double value, int update, double prim, double privar, double sig)
{
    par[i][j] = value;
    epar[i][j] = unTransform(i,j);
    doit[i][j] = update;
    primean[i][j] = prim;
    if (privar < 0)
    {
        Rcpp::stop("Error: Cannot set prior variance to be negative");
    }
    pristdev[i][j] = sqrt(privar);
    sigmaprop[i][j] = sig;
}

int LogNormalICP::nParam() const
{
    if (pnames[0][0] == "NOT")
        return 1;

    int t = 0;

    for (int i=0; i<ns; i++)
    {
        if (i == 1 && nstates != 3)
            continue;
        t += n[i];
    }

    return t;
}

std::vector<std::string> LogNormalICP::paramNames() const
{
    std::vector<std::string> res(nParam());

    if (pnames[0][0] == "NOT")
    {
        res[0] = "PARAMETER NAMES NOT IMPLEMENTED";
        return res;
    }

    int t = 0;

    for (int i=0; i<ns; i++)
    {
        if (i == 1 && nstates != 3)
            continue;

        for (int j=0; j<n[i]; j++)
        {
            res[t++] = pnames[i][j];
        }
    }

    return res;
}

void LogNormalICP::write (ostream &os)
{
    char *buffer = new char[100];

    for (int i=0; i<ns; i++)
    {
        if (i == 1 && nstates != 3)
            continue;

        for (int j=0; j<n[i]; j++)
        {
            snprintf(buffer, 100, "%12.10f",epar[i][j]);
            os << buffer;
            if (j != n[i]-1)
                os << "\t";
        }
        if (i != ns-1)
            os << "\t";
    }

    delete[] buffer;
}
std::vector<double> LogNormalICP::getValues() const
{
    std::vector<double> vals;

    for (int i=0; i<ns; i++)
    {
        if (i == 1 && nstates != 3)
            continue;

        for (int j=0; j<n[i]; j++)
        {
            vals.push_back(epar[i][j]);
        }
    }
    return vals;
}

// Implement InColParams.

/// Compute Rate for the given event
///
///
///
double LogNormalICP::eventRate(double time, EventCode c, PatientState *p, LocationState *s)
{
    switch(c)
    {
    case progression:
        return exp(logProgressionRate(time,p,s));
    case clearance:
        return exp(logClearanceRate(time,p,s));
    case acquisition:
        return exp(logAcquisitionRate(time,p,s));
    default:
        return 0;
    }
}

/// Matrix of Rates
///
/// For a given time and patient and location states, return the rate matrix
/// of transition rates
///
///
///
double** LogNormalICP::rateMatrix(double time, PatientState *p, LocationState *u)
{
    double **Q = cleanAlloc(nstates,nstates);

    if (nstates == 2)
    {
        Q[0][1] = eventRate(time,acquisition,p,u);
        Q[0][0] = -Q[0][1];
        Q[1][0] = eventRate(time,clearance,p,u);
        Q[1][1] = -Q[1][0];
    }

    if (nstates == 3)
    {
        Q[0][1] = eventRate(time,acquisition,p,u);
        Q[0][0] = -Q[0][1];
        Q[1][2] = eventRate(time,progression,p,u);
        Q[1][1] = -Q[1][2];
        Q[2][0] = eventRate(time,clearance,p,u);
        Q[2][2] = -Q[2][0];
    }

    return Q;
}

// Implement Parameters.

/// Log probability of an event given in the HistoryLink
double LogNormalICP::logProb(HistoryLink *h)
{
    switch(h->getEvent()->getType())
    {
    case progression:
        return logProgressionRate(h->getEvent()->getTime(),h->pPrev()->getPState(),h->uPrev()->getUState());
    case clearance:
        return logClearanceRate(h->getEvent()->getTime(),h->pPrev()->getPState(),h->uPrev()->getUState());
    case acquisition:
        return logAcquisitionRate(h->getEvent()->getTime(),h->pPrev()->getPState(),h->uPrev()->getUState());
    default:
        return 0;
    }
}

double LogNormalICP::logProbGap(HistoryLink *g, HistoryLink *h)
{
    LocationState *s = h->uPrev()->getUState();
    double t0 = g->getEvent()->getTime();
    double t1 = h->getEvent()->getTime();

    double x = 0;

    x += logProgressionGap(t0,t1,s);
    x += logClearanceGap(t0,t1,s);
    x += logAcquisitionGap(t0,t1,s);

    return x;
}

void LogNormalICP::initCounts()
{
    m->clear();
}

void LogNormalICP::count(HistoryLink *h)
{
}

void LogNormalICP::countGap(HistoryLink *g, HistoryLink *h)
{
    m->put(h,g);
}

double LogNormalICP::logpost(Random *r, int max)
{
    double x = 0;

    if (!max)
        for (int i=0; i<ns; i++)
            for (int j=0; j<n[i]; j++)
                if (doit[i][j])
                    x += r->logdnorm(par[i][j],primean[i][j],pristdev[i][j]);

    for (m->init(); m->hasNext(); )
    {
        HistoryLink *h = (HistoryLink *) m->next();
        HistoryLink *g = (HistoryLink *) m->get(h);
        x += logProb(h) + logProbGap(g,h);
    }

    return x;
}

void LogNormalICP::update(Random *r, bool max)
{
    double oldlogpost = logpost(r,max);

    for (int its = 0; its < nmetro; its++)
    {
        for (int i=0; i<ns; i++)
            for (int j=0; j<n[i]; j++)
            {
                if (doit[i][j])
                {
                    double oldone = par[i][j];
                    double newone = oldone + r->rnorm(0,sigmaprop[i][j]);
                    setNormal(i,j,newone);
                    double newlogpost = logpost(r,max);

                    if ( (max ? 0 : log(r->runif()) ) <= newlogpost - oldlogpost)
                    {
                        oldlogpost = newlogpost;
                    }
                    else
                    {
                        setNormal(i,j,oldone);
                    }
                }
            }
    }
}
} // namespace lognormal
