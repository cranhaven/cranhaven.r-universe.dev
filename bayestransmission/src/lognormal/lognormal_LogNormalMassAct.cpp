#include "lognormal/lognormal.h"

namespace lognormal{

double LogNormalMassAct::logAcquisitionRate(double ncol, double tot)
{
    double x =  par[0][0] + par[0][2] * log(tot);
    if (par[0][1] > 0)
        x += par[0][1] * log(ncol);
    return x;
}

LogNormalMassAct::LogNormalMassAct(int k, int isDen, int nmet) : LogNormalICP(k,3,1,1,nmet)
{
    isDensity = isDen;

    setNormal(1,0,log(0.01),1,0,1);
    setNormal(2,0,log(0.01),1,0,1);

    setNormal(0,0,log(0.001),1,0,1);

    switch(isDensity)
    {
    case 2: setNormal(0,1,0,0,0,1);
        setNormal(0,2,0,0,0,1);
        break;

    case 1: setNormal(0,1,1,0,0,1);
        setNormal(0,2,0,0,0,1);
        break;

    case 0: setNormal(0,1,1,0,0,1);
        setNormal(0,2,-1,0,0,1);
        break;
    }

    pnames[1][0] = "LNMA.pro";
    pnames[2][0] = "LNMA.clr";

    pnames[0][0] = "LNMA.base";
    pnames[0][1] = "LNMA.ncol";
    pnames[0][2] = "LNMA.ntot";
}


// Implement LogNormalICP.

double LogNormalMassAct::logProgressionRate(double time, PatientState *p, LocationState *s)
{
    return par[1][0];
}

double LogNormalMassAct::logProgressionGap(double t0, double t1, LocationState *s)
{
    return ( s->getLatent() == 0 ?  0 : -(t1 - t0) * s->getLatent() * exp(par[1][0]) );
}

double LogNormalMassAct::logClearanceRate(double time, PatientState *p, LocationState *s)
{
    return par[2][0];
}

double LogNormalMassAct::logClearanceGap(double t0, double t1, LocationState *s)
{
    return ( s->getColonized() == 0 ? 0 :  -(t1 - t0) * s->getColonized() * exp(par[2][0]) );
}

double LogNormalMassAct::logAcquisitionRate(double time, PatientState *p, LocationState *s)
{
    return logAcquisitionRate(s->getColonized(),s->getTotal());
}

double LogNormalMassAct::logAcquisitionGap(double t0, double t1, LocationState *s)
{
    if (s->getSusceptible() > 0)
        return - (t1 - t0) * s->getSusceptible() * exp(logAcquisitionRate(s->getColonized(),s->getTotal()));
    else
        return 0;
}

double* LogNormalMassAct::acquisitionRates(double time, PatientState *p, LocationState *s)
{
    double *P = new double[nstates];

    if (nstates == 2)
    {
        P[0] = exp(logAcquisitionRate(s->getColonized(),s->getTotal()));
        P[1] = exp(logAcquisitionRate(1+s->getColonized(),s->getTotal()));
    }

    if (nstates == 3)
    {
        P[0] = exp(logAcquisitionRate(s->getColonized(),s->getTotal()));
        P[1] = exp(logAcquisitionRate(s->getColonized(),s->getTotal()));
        P[2] = exp(logAcquisitionRate(1+s->getColonized(),s->getTotal()));
    }

    return P;
}

} // namespace lognormal
