#include "lognormal/lognormal.h"

namespace lognormal{

double MultiUnitAbxICP::acqRate(int unit, int onabx, double ncolabx, double ncol, double tot)
{
    //double x = par[0][1] + par[0][2] * log(tot) + par[0][4] * ncol + par[0][5] * ncolabx + par[0][6] * onabx;

    double x = par[0][unit] + par[0][2] * log(tot) + par[0][4] * ncol + par[0][5] * ncolabx + par[0][6] * onabx;

    if (par[0][3] > 0.000001)
        x += par[0][3] * log(ncol);
    return exp(x);
}

int MultiUnitAbxICP::index(Object *u)
{
    return ((Integer *)units->get(u))->intValue();
}


MultiUnitAbxICP::MultiUnitAbxICP(List *u, int nst, int isDensity, int nmet) : LogNormalAbxICP(nst,isDensity,nmet,7+u->size())
{
    setNormal(0,0,0,0,0,1,0.001);
    set(0,1,0.001,1,0,1);

    switch(isDensity)
    {
    case 0: // Frequency dependent.
        setNormal(0,2,-1,0,0,1);
        setNormal(0,3,1,0,0,1);
        break;

    case 1: // Density dependent.
        setNormal(0,2,0,0,0,1);
        setNormal(0,3,1,0,0,1);
        break;

    case 2: // Constant.
        setNormal(0,2,0,0,0,1);
        setNormal(0,3,0,0,0,1);
        break;
    }

    setNormal(0,4,0,0,0,1);
    setNormal(0,5,0,0,0,1);
    setNormal(0,6,0,0,0,1);

    units = new Map();
    u->init();
    for (int i = 7; i<n[0]; i++)
    {
        Unit *v = (Unit *) u->next();
        units->put(v,new Integer(i));
        setNormal(0,i,0,1,0,1);

        stringstream ss;
        ss << "MUABX." << v->getName();
        pnames[0][i] = ss.str();
    }

    setNormal(1,0,log(0.01),1,log(0.01),1);
    setNormal(1,1,0,0,0,1);

    setNormal(2,0,log(0.05),1,log(0.05),1);
    setNormal(2,1,0,0,0,1);
}

void MultiUnitAbxICP::setUnit(int i, Object *u, double value, int update, double prival, double priorn, double sig)
{
    set(i,index(u),value,update,prival,priorn);
    // Note: sig parameter is ignored - set() calls use fixed sigma values internally
}

// Implement LogNormalICP.

double MultiUnitAbxICP::logAcquisitionRate(double time, PatientState *p, LocationState *ls)
{
    AbxLocationState *as = (AbxLocationState *) ls;
    int onabx = as->onAbx((Patient *)p->getOwner());
    int unit = index(ls->getOwner());
    return log(acqRate(unit,onabx,as->getAbxColonized(),as->getColonized(),as->getTotal())) + (time-tOrigin)*par[0][0];
}

double MultiUnitAbxICP::logAcquisitionGap(double u, double v, LocationState *ls)
{
    double t = (abs(par[0][0]) < 0.0000001 ? (v-u) : (exp(par[0][0]*(v-tOrigin)) - exp(par[0][0]*(u-tOrigin))) / par[0][0] ) ;
    double x = 0;
    AbxLocationState *as = (AbxLocationState *) ls;
    int unit = index(ls->getOwner());

    if (as->getSusceptible() > 0)
    {
        if (as->getAbxSusceptible() > 0)
            x +=  as->getAbxSusceptible() * acqRate(unit,1,as->getAbxColonized(),as->getColonized(),as->getTotal());
        if (as->getSusceptible() - as->getAbxSusceptible() > 0)
            x +=  (as->getSusceptible() - as->getAbxSusceptible()) * acqRate(unit,0,as->getAbxColonized(),as->getColonized(),as->getTotal());
    }

    return -t*x;
}

double* MultiUnitAbxICP::acquisitionRates(double time, PatientState *p, LocationState *ls)
{
    AbxLocationState *as = (AbxLocationState *) ls;
    int onabx = as->onAbx((Patient *)p->getOwner());
    int unit = index(ls->getOwner());

    double *P = new double[nstates];

    if (nstates == 2)
    {
        P[0] = acqRate(unit,onabx,as->getAbxColonized(),as->getColonized(),as->getTotal()) * exp((time-tOrigin)*par[0][0]);
        P[1] = acqRate(unit,onabx,as->getAbxColonized(),1+as->getColonized(),as->getTotal()) * exp((time-tOrigin)*par[0][0]);
    }

    if (nstates == 3)
    {
        P[0] = acqRate(unit,onabx,as->getAbxColonized(),as->getColonized(),as->getTotal()) * exp((time-tOrigin)*par[0][0]);
        P[1] = P[0];
        P[2] = acqRate(unit,onabx,as->getAbxColonized(),1+as->getColonized(),as->getTotal()) * exp((time-tOrigin)*par[0][0]);
    }

    return P;
}

} // namespace lognormal
