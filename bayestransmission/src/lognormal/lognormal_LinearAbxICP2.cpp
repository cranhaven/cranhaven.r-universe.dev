#include "lognormal/lognormal.h"

namespace lognormal{

// LinearAbxICP2 uses log transform for ALL parameters (not logit for positions 2,3)
// and has a different acqRate formula (additive not multiplicative)

LinearAbxICP2::LinearAbxICP2(int nst, int nmet, int nacqpar) : LogNormalICP(nst,nacqpar,3,3,nmet)
{
}

string LinearAbxICP2::header() const
{
    stringstream s;
    s << "LABX.base";
    s << "\t" << "LABX.time";
    s << "\t" << "LABX.dens";
    s << "\t" << "LABX.freq";
    s << "\t" << "LABX.colabx";
    s << "\t" << "LABX.susabx";
    s << "\t" << "LABX.susever";
    if (nstates == 3)
    {
        s << "\t" << "LABX.pro";
        s << "\t" << "LABX.proAbx";
        s << "\t" << "LABX.proEver";
    }
    s << "\t" << "LABX.clr";
    s << "\t" << "LABX.clrAbx";
    s << "\t" << "LABX.clrEver";
    return s.str();
}

double LinearAbxICP2::getRate(int i, int risk, int ever, int cur) const
{
    return epar[i][0] * ( risk-ever + epar[i][2] * (ever-cur + epar[i][1] * cur));
}

double LinearAbxICP2::logProgressionRate(double time, PatientState *p, LocationState *s)
{
    int everabx =((AbxLocationState *)s)->everAbx((Patient *)p->getOwner());
    int onabx = ((AbxLocationState *)s)->onAbx((Patient *)p->getOwner());
    return log(getRate(1,1,everabx,onabx));
}

double LinearAbxICP2::logProgressionGap(double t0, double t1, LocationState *s)
{
    AbxLocationState *as = (AbxLocationState *) s;
    return -(t1-t0) * getRate(1,as->getLatent(),as->getEverAbxLatent(),as->getAbxLatent());
}

double LinearAbxICP2::logClearanceRate(double time, PatientState *p, LocationState *s)
{
    int everabx =((AbxLocationState *)s)->everAbx((Patient *)p->getOwner());
    int onabx = ((AbxLocationState *)s)->onAbx((Patient *)p->getOwner());
    return log(getRate(2,1,everabx,onabx));
}

double LinearAbxICP2::logClearanceGap(double t0, double t1, LocationState *s)
{
    AbxLocationState *as = (AbxLocationState *) s;
    return -(t1-t0) * getRate(2,as->getColonized(),as->getEverAbxColonized(),as->getAbxColonized());
}

double LinearAbxICP2::acqRate(int nsus, int onabx, int everabx, int ncolabx, int ncol, int tot, double time)
{
    static constexpr double timepartol = 0.000000001;
    double x = (abs(par[0][1]) < timepartol ? 1 : exp(par[0][1] * (time-tOrigin)));

    double c = (ncol-ncolabx) + epar[0][4]*ncolabx;
    double y =  epar[0][0] + c * epar[0][2] + c * (tot > 0 ? epar[0][3]/tot : 0);
    
    double z = (nsus-everabx) + epar[0][6] * ( (everabx-onabx) + onabx * epar[0][5]);

    return x * y * z;
}

double LinearAbxICP2::logAcquisitionRate(double time, PatientState *p, LocationState *ls)
{
    AbxLocationState *as = (AbxLocationState *) ls;
    int everabx = as->everAbx((Patient *)p->getOwner());
    int onabx = as->onAbx((Patient *)p->getOwner());
    return log(acqRate(1,onabx,everabx,as->getAbxColonized(),as->getColonized(),as->getTotal(),time));
}

double LinearAbxICP2::logAcquisitionGap(double u, double v, LocationState *ls)
{
    static constexpr double timepartol = 0.000000001;
    AbxLocationState *as = (AbxLocationState *) ls;
    int nsus = as->getSusceptible();
    int neve = as->getEverAbxSusceptible();
    int ncur = as->getAbxSusceptible();
    int ncol = as->getColonized();
    int ntot = as->getTotal();
    int ncax = as->getAbxColonized();

    if (abs(par[0][1]) < timepartol)
    {
        return -(v-u) * acqRate(nsus,ncur,neve,ncax,ncol,ntot,tOrigin);
    }
    else
    {
        return - (acqRate(nsus,ncur,neve,ncax,ncol,ntot,v)-acqRate(nsus,ncur,neve,ncax,ncol,ntot,u)) / par[0][1];
    }
}

double* LinearAbxICP2::acquisitionRates(double time, PatientState *p, LocationState *ls)
{
    AbxLocationState *as = (AbxLocationState *) ls;
    int onabx = as->onAbx((Patient *)p->getOwner());
    int everabx = as->everAbx((Patient *)p->getOwner());

    double *P = new double[nstates];

    if (nstates == 2)
    {
        P[0] = acqRate(1,onabx,everabx,as->getAbxColonized(),as->getColonized(),as->getTotal(),time);
        P[1] = acqRate(1,onabx,everabx,as->getAbxColonized(),1+as->getColonized(),as->getTotal(),time);
    }

    if (nstates == 3)
    {
        P[0] = acqRate(1,onabx,everabx,as->getAbxColonized(),as->getColonized(),as->getTotal(),time);
        P[1] = P[0];
        P[2] = acqRate(1,onabx,everabx,as->getAbxColonized(),1+as->getColonized(),as->getTotal(),time);
    }

    return P;
}

double LinearAbxICP2::unTransform(int i, int j)
{
    // LinearAbxICP2 uses log transform for ALL parameters
    return exp(par[i][j]);
}

void LinearAbxICP2::set(int i, int j, double value, int update, double prival, double priorn)
{
    // LinearAbxICP2 uses log transform for ALL parameters (not logit for 2,3)
    setWithLogTransform(i,j,value,update,prival,priorn);
}

} // namespace lognormal

