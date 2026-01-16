#include "modeling/modeling.h"

namespace models {

AbxParams::AbxParams(int k)
{
    nstates = k;
    n = 3;
    rates = new double[n];
    shapepar = new double[n];
    ratepar = new double[n];
    priorshape = new double[n];
    priorrate = new double[n];
    doit = new int[n];

    switch(nstates)
    {
    case 2:
        set(0,1,1,1,1);
        set(1,1,0,1,1);
        set(2,1,1,1,1);
        break;
    case 3:
        set(0,1,1,1,1);
        set(1,1,1,1,1);
        set(2,1,1,1,1);
        break;
    }

    initCounts();
}

AbxParams::~AbxParams()
{
    if (rates)
        delete [] rates;
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

string AbxParams::header() const
{
    stringstream s;
    s << "Abx.rateUnc";
    if (nstates == 3)
        s << "\t" << "Abx.rateLat";
    s << "\t" << "Abx.rateCol";
    return s.str();
}
std::vector<std::string> AbxParams::paramNames() const
{
    std::vector<std::string> nms;
    nms.push_back("Abx.rateUnc");
    if (nstates == 3)
        nms.push_back("Abx.rateLat");
    nms.push_back("Abx.rateCol");
    return nms;
}

int AbxParams::getNStates() const
{
    return nstates;
}

int AbxParams::nParam() const
{
    return nstates;
}

// Implement Parameters.

double AbxParams::logProb(infect::HistoryLink* h)
{
    if (h->getEvent()->getType() == abxon)
    {
        infect::AbxPatientState *ps = (infect::AbxPatientState *) h->getPState();
        if (ps->onAbx() == 1)
        {
            return  log (rates[stateIndex(ps->infectionStatus())]);
        }
    }
    return 0;
}

double AbxParams::logProbGap(infect::HistoryLink *g, infect::HistoryLink *h)
{
    infect::AbxLocationState *s = (infect::AbxLocationState *) h->uPrev()->getUState();
    return - (h->getEvent()->getTime() - g->getEvent()->getTime()) *
        (
                s->getNoAbxSusceptible() * rates[0] +
                    s->getNoAbxLatent() * rates[1] +
                    s->getNoAbxColonized() * rates[2]
        );
}

void AbxParams::count(infect::HistoryLink *h)
{
    if (h->getEvent()->getType() == abxon)
    {
        infect::AbxPatientState *ps = (infect::AbxPatientState *) h->getPState();
        if (ps->onAbx() == 1)
            shapepar[stateIndex(ps->infectionStatus())] += 1;
    }
}

void AbxParams::countGap(infect::HistoryLink *g, infect::HistoryLink *h)
{
    double time = h->getEvent()->getTime() - g->getEvent()->getTime();
    infect::AbxLocationState *s = (infect::AbxLocationState *) h->uPrev()->getUState();
    ratepar[0] += time * s->getNoAbxSusceptible();
    ratepar[1] += time * s->getNoAbxLatent();
    ratepar[2] += time * s->getNoAbxColonized();
}

void AbxParams::initCounts()
{
    for (int i=0; i<n; i++)
    {
        shapepar[i] = priorshape[i];
        ratepar[i] = priorrate[i];
    }
}

void AbxParams::update(Random *r, bool max)
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
        rates[i] = newrates[i];

    delete [] newrates;
}

// Personal accessors.

void AbxParams::set(int i, double value, int update, double prival, double prin)
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

    rates[i] = value;
    doit[i] = update;
    double n = prin > 1 ? prin : 1;
    priorshape[i] = prival * n;
    priorrate[i] = n;
}
std::vector<double> AbxParams::getValues() const
{
    std::vector<double> vals;
    vals.push_back(rates[0]);
    if(nstates == 3)
        vals.push_back(rates[1]);
    vals.push_back(rates[2]);
    return vals;
}
void AbxParams::write(ostream &os) const
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
