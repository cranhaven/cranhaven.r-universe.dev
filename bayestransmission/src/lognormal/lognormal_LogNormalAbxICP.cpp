#include "lognormal/lognormal.h"

namespace lognormal{

void LogNormalAbxICP::setParameterNames()
{
    pnames[0][0] = "LNAX.time";
    pnames[0][1] = "LNAX.base";
    pnames[0][2] = "LNAX.ltot";
    pnames[0][3] = "LNAX.lcol";
    pnames[0][4] = "LNAX.col";
    pnames[0][5] = "LNAX.colabx";
    pnames[0][6] = "LNAX.susabx";
    pnames[0][7] = "LNAX.susever";

    pnames[1][0] = "LNAX.pro";
    pnames[1][1] = "LNAX.proAbx";
    pnames[1][2] = "LNAX.proEver";

    pnames[2][0] = "LNAX.clr";
    pnames[2][1] = "LNAX.clrAbx";
    pnames[2][2] = "LNAX.clrEver";
}

string LogNormalAbxICP::header() const
{
    stringstream s;

    s << "LNAX.time";
    s << "\t" "LNAX.base";
    s << "\t" << "LNAX.ltot";
    s << "\t" << "LNAX.lcol";
    s << "\t" << "LNAX.col";
    s << "\t" << "LNAX.colabx";
    s << "\t" << "LNAX.susabx";
    s << "\t" << "LNAX.susever";

    if (nstates == 3)
    {
        s << "\t" << "LNAX.pro";
        s << "\t" << "LNAX.proAbx";
        s << "\t" << "LNAX.proEver";
    }

    s << "\t" << "LNAX.clr";
    s << "\t" << "LNAX.clrAbx";
    s << "\t" << "LNAX.clrEver";

    return s.str();
}

/// Log Acquisition Rate
/// \param time Time of acquisition.
/// \param onabx If patient is on antibiotics at the time.
/// \param everabx If patient has ever been on antibiotics.
/// \param ncolabx Number of colonized patients on antibiotics.
/// \param ncol Number of colonized patients.
/// \param tot Total number of patients.
double LogNormalAbxICP::logAcqRate(int onabx, int everabx, int ncolabx, int ncol, int tot, double time)
{
    double x = 0;

    if (ncol > 0)
        x = logbeta_acq_log_col() * log((double) ncol) + logbeta_acq_tot_inpat() * log((double) tot);
    else
        x = log((double) 0);

    x +=
        logbeta_acq_constant() +
        logbeta_acq_time() * (time - tOrigin) +
        logbeta_acq_col() * ncol +
        logbeta_acq_abx_col() * ncolabx +
        logbeta_acq_onabx() * onabx +
        logbeta_acq_everabx() * everabx;
    return x;
}

double LogNormalAbxICP::acqRate(double time, int onabx, int everabx, double ncolabx, double ncol, double tot)
{
    return exp(logAcqRate(onabx,everabx,ncolabx,ncol,tot,time));
}

double LogNormalAbxICP::progRate(int onabx, int ever)
{
    double x = exp(par[1][0]);
    if (onabx)
        x *= exp(par[1][1]);
    if (ever)
        x *= exp(par[1][2]);
    return x;
}

double LogNormalAbxICP::logProgRate(int onabx, int ever)
{
    double x = par[1][0];
    if (onabx)
        x += par[1][1];
    if (ever)
        x += par[1][2];
    return x;
}

double LogNormalAbxICP::LogNormalAbxICP::clearRate(int onabx, int ever)
{
    double x = exp(par[2][0]);
    if (onabx)
        x *= exp(par[2][1]);
    if (ever)
        x *= exp(par[2][2]);
    return x;
}

double LogNormalAbxICP::logClearRate(int onabx, int ever)
{
    double x = par[2][0];
    if (onabx)
        x += par[2][1];
    if (ever)
        x += par[2][2];
    return x;
}

LogNormalAbxICP::LogNormalAbxICP(int nst, int isDensity, int nmet, int cap) : LogNormalICP(nst,cap,3,3,nmet)
{
    // Density model

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
    setNormal(0,7,0,0,0,1);

    setNormal(1,0,log(0.01),1,log(0.01),1);
    setNormal(1,1,0,0,0,1);
    setNormal(1,2,0,0,0,1);

    setNormal(2,0,log(0.05),1,log(0.05),1);
    setNormal(2,1,0,0,0,1);
    setNormal(2,2,0,0,0,1);

    setParameterNames();
}

double LogNormalAbxICP::timePar(){return par[0][0];}

double LogNormalAbxICP::logProgressionRate(double time, PatientState *p, LocationState *s)
{
    int onabx = ((AbxLocationState *)s)->onAbx((Patient *)p->getOwner());
    int everabx = ((AbxLocationState *)s)->everAbx((Patient *)p->getOwner());
    return logProgRate(onabx,everabx);
}

double LogNormalAbxICP::logProgressionGap(double t0, double t1, LocationState *s)
{
    AbxLocationState *as = (AbxLocationState *) s;
    double x = 0;
    x += progRate(0,0) * as->getNeverAbxLatent();
    x += progRate(0,1) * (as->getEverAbxLatent() - as->getAbxLatent());
    x += progRate(1,1) * as->getAbxLatent();
    return -(t1-t0) * x;
}

double LogNormalAbxICP::logClearanceRate(double time, PatientState *p, LocationState *s)
{
    int onabx = ((AbxLocationState *)s)->onAbx((Patient *)p->getOwner());
    int everabx = ((AbxLocationState *)s)->everAbx((Patient *)p->getOwner());
    return logClearRate(onabx,everabx);
}

double LogNormalAbxICP::logClearanceGap(double t0, double t1, LocationState *s)
{
    AbxLocationState *as = (AbxLocationState *) s;
    double x = 0;
    x += clearRate(0,0) * as->getNeverAbxColonized();
    x += clearRate(0,1) * (as->getEverAbxColonized() - as->getAbxColonized());
    x += clearRate(1,1) * as->getAbxColonized();
    return -(t1-t0) * x;
}

double LogNormalAbxICP::logAcquisitionRate(double time, PatientState *p, LocationState *ls)
{
    AbxLocationState *as = (AbxLocationState *) ls;
    int onabx = as->onAbx((Patient *)p->getOwner());
    int everabx = as->everAbx((Patient *)p->getOwner());
    return log(acqRate(time,onabx,everabx,as->getAbxColonized(),as->getColonized(),as->getTotal()));
}

double LogNormalAbxICP::logAcquisitionGap(double u, double v, LocationState *ls)
{
    double t = (abs(timePar()) < 0.0000001 ? (v-u) : (exp(timePar()*(v-tOrigin)) - exp(timePar()*(u-tOrigin))) / timePar() ) ;
    double x = 0;

    AbxLocationState *as = (AbxLocationState *) ls;

    if (as->getSusceptible() > 0)
    {
        int inx = as->getNeverAbxSusceptible();
        int ipx = as->getEverAbxSusceptible() - as->getAbxSusceptible();
        int icx = as->getAbxSusceptible();
        if (inx > 0)
            x += inx * acqRate(tOrigin,0,0,as->getAbxColonized(),as->getColonized(),as->getTotal());
        if (ipx > 0)
            x += (ipx) * acqRate(tOrigin,0,1,as->getAbxColonized(),as->getColonized(),as->getTotal());
        if (icx > 0)
            x += icx * acqRate(tOrigin,1,1,as->getAbxColonized(),as->getColonized(),as->getTotal());
    }

    return -t*x;
}

double* LogNormalAbxICP::acquisitionRates(double time, PatientState *p, LocationState *ls)
{
    AbxLocationState *as = (AbxLocationState *) ls;
    int onabx = as->onAbx((Patient *)p->getOwner());
    int everabx = as->everAbx((Patient *)p->getOwner());

    double *P = new double[nstates];

    if (nstates == 2)
    {
        P[0] = acqRate(time,onabx,everabx,as->getAbxColonized(),as->getColonized(),as->getTotal());
        P[1] = acqRate(time,onabx,everabx,as->getAbxColonized(),1+as->getColonized(),as->getTotal());
    }

    if (nstates == 3)
    {
        P[0] = acqRate(time,onabx,everabx,as->getAbxColonized(),as->getColonized(),as->getTotal());
        P[1] = P[0];
        P[2] = acqRate(time,onabx,everabx,as->getAbxColonized(),1+as->getColonized(),as->getTotal());
    }

    return P;
}

std::vector<std::string> LogNormalAbxICP::paramNames() const {
    std::vector<std::string> names(14);  // Pre-allocate for 14 parameters
    names[0] = "LNAX.time";
    names[1] = "LNAX.base";
    names[2] = "LNAX.ltot";
    names[3] = "LNAX.lcol";
    names[4] = "LNAX.col";
    names[5] = "LNAX.colabx";
    names[6] = "LNAX.susabx";
    names[7] = "LNAX.susever";

    names[8] = "LNAX.pro";
    names[9] = "LNAX.proAbx";
    names[10] = "LNAX.proEver";

    names[11] = "LNAX.clr";
    names[12] = "LNAX.clrAbx";
    names[13] = "LNAX.clrEver";
    return names;
}

} // namespace lognormal
