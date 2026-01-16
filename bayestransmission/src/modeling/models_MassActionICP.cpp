#include "modeling/modeling.h"

namespace models {

void MassActionICP::set(int i, double value)
{
    rates[i] = value;
    logrates[i] = log(rates[i]);
}

MassActionICP::MassActionICP(int k, int isdens) : InColParams(k)
{
    isDensity = isdens;

    n = 3;
    rates = new double[n];
    logrates = new double[n];
    shapepar = new double[n];
    ratepar = new double[n];
    priorshape = new double[n];
    priorrate = new double[n];
    doit = new int[n];

    switch(nstates)
    {
    case 2:
        set(0,0.001,1,1,1);
        set(1,0,0,1,1);
        set(2,0.01,1,1,1);
        break;
    case 3:
        set(0,0.001,1,1,1);
        set(1,0.01,1,1,1);
        set(2,0.01,1,1,1);
        break;
    }

    initCounts();
}

MassActionICP::~MassActionICP()
{
    if (rates)
        delete [] rates;
    if (logrates)
        delete [] logrates;
    if (shapepar)
        delete [] shapepar;
    if (ratepar)
        delete [] ratepar;
    if (priorshape)
        delete [] priorshape;
    if (priorrate)
        delete [] priorrate;
    if (doit)
        delete [] doit;
}

string MassActionICP::header() const
{
    stringstream s;
    s <<  "MAICP.acq";
    if (nstates == 3)
        s << "\t" << "MAICP.pro";
    s << "\t" << "MAICP.clr";
    return s.str();
}

// Implement InColParams.

double* MassActionICP::acquisitionRates(double time, infect::PatientState *p, infect::LocationState *s)
{
    double *P = new double[nstates];

    if (nstates == 2)
    {
        P[0] = rates[0] * acquisitionFactor(s->getColonized(),s->getTotal());
        P[1] = rates[0] * acquisitionFactor(1+s->getColonized(),s->getTotal());
    }

    if (nstates == 3)
    {
        P[0] = rates[0] * acquisitionFactor(s->getColonized(),s->getTotal());
        P[1] = rates[0] * acquisitionFactor(s->getColonized(),s->getTotal());
        P[2] = rates[0] * acquisitionFactor(1+s->getColonized(),s->getTotal());
    }

    return P;
}

double MassActionICP::eventRate(double time, EventCode c, infect::PatientState *p, infect::LocationState *s)
{
    switch(c)
    {
    case acquisition:
        return rates[0] * acquisitionFactor(s->getColonized(),s->getTotal());
    case progression:
        return rates[1];
    case clearance:
        return rates[2];
    default:
        return 0;
    }
}

double** MassActionICP::rateMatrix(double time, infect::PatientState *p, infect::LocationState *u)
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

double MassActionICP::logProb(infect::HistoryLink *h)
{
    switch(h->getEvent()->getType())
    {
    case acquisition:
    case progression:
    case clearance:
        return log(eventRate(h->getEvent()->getTime(),h->getEvent()->getType(),h->pPrev()->getPState(),h->uPrev()->getUState()));
    default:
        return 0;
    }
}

/*
 This will fail if eventRate depends on PatientStatus.
 */
double MassActionICP::logProbGap(infect::HistoryLink *g, infect::HistoryLink *h)
{
    infect::LocationState *s = h->uPrev()->getUState();
    return - (h->getEvent()->getTime() - g->getEvent()->getTime()) *
        (
                (s->getSusceptible() == 0 ? 0 : s->getSusceptible() * eventRate(g->getEvent()->getTime(),acquisition,0,s)) +
                    (s->getLatent() == 0 ? 0 : s->getLatent() * eventRate(g->getEvent()->getTime(),progression,0,s)) +
                    (s->getColonized() == 0 ? 0 : s->getColonized() * eventRate(g->getEvent()->getTime(),clearance,0,s))
        );
}

void MassActionICP::initCounts()
{
    for (int i=0; i<n; i++)
    {
        shapepar[i] = priorshape[i];
        ratepar[i] = priorrate[i];
    }
}

void MassActionICP::count(infect::HistoryLink *h)
{
    shapepar[eventIndex(h->getEvent()->getType())] += 1;
}

void MassActionICP::countGap(infect::HistoryLink *g, infect::HistoryLink *h)
{
    double time = h->getEvent()->getTime() - g->getEvent()->getTime();
    infect::LocationState *s = h->uPrev()->getUState();
    ratepar[0] += time * s->getSusceptible() * acquisitionFactor(s->getColonized(),s->getTotal());
    ratepar[1] += time * s->getLatent();
    ratepar[2] += time * s->getColonized();
}

void MassActionICP::update(Random *r, bool max)
{
    double *newrates = new double[n];

    if (max)
    {
        for (int i=0; i<n; i++)
            newrates[i] = (doit[i] ? (shapepar[i]-1)/ratepar[i] : rates[i]);
    }
    else
    {
        for (int i=0; i<n; i++)
            newrates[i] = (doit[i] ? r->rgamma(shapepar[i],ratepar[i]) : rates[i]);
    }

    for (int i=0; i<n; i++)
        set(i,newrates[i]);

    delete [] newrates;
}

// Personal accessors.

void MassActionICP::set(int i, double value, int update, double prival, double prin)
{
    if (value < 0)
    {
        Rcpp::stop("Can't set rate value negative: " + std::to_string(value));
    }
    if (prival < 0)
    {
        Rcpp::stop("Can't set rate prior value negative: " + std::to_string(prival));
    }
    if (prin < 0)
    {
        Rcpp::stop("Can't set prior observation count negative: " + std::to_string(prin));
    }

    set(i,value);

    doit[i] = update;
    double n = prin > 1 ? prin : 1;
    priorshape[i] = prival * n;
    priorrate[i] = n;
}


int MassActionICP::nParam() const
{
    return nstates;
}

std::vector<std::string> MassActionICP::paramNames() const
{
    std::vector<std::string> res(nstates);

    if (nstates == 3)
    {
        res[0] = "MAICP.acq";
        res[1] = "MAICP.pro";
        res[2] = "MAICP.clr";
    }

    if (nstates == 2)
    {
        res[0] = "MAICP.acq";
        res[1] = "MAICP.clr";
    }

    return res;
}
std::vector<double> MassActionICP::getValues() const
{
    std::vector<double> vals;
    vals.push_back(rates[0]);
    if(nstates == 3)
        vals.push_back(rates[1]);
    vals.push_back(rates[2]);
    return vals;
}

void MassActionICP::write (ostream &os)
{
    char *buffer = new char[100];
    snprintf(buffer, 100, "%12.10f\t",rates[0]);
    os << buffer;
    if (nstates == 3)
    {
        snprintf(buffer, 100, "%12.10f\t",rates[1]);
        os << buffer;
    }
    snprintf(buffer, 100, "%12.10f",rates[2]);
    os << buffer;
    delete[] buffer;
}
} // namespace models
