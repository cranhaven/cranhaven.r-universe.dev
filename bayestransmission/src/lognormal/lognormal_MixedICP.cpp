#include "lognormal/lognormal.h"

namespace lognormal{

MixedICP::MixedICP(int nst, int isDensity, int nmet, int cap) : LogNormalAbxICP(nst, isDensity, nmet, cap)
{
    pnames[0][0] = "MICP.base";
    pnames[0][1] = "MICP.mix";
    pnames[0][2] = "MICP.ncol";
    pnames[0][3] = "MICP.ntot";
    pnames[0][4] = "MICP.time";
    pnames[0][5] = "MICP.colabx";
    pnames[0][6] = "MICP.susabx";
    pnames[0][7] = "MICP.susever";
}

// Acquisition model mixes constant and mass action terms.
// Constant parameter is par[0][0]
// Mixing parameter is par[0][1]
// log number colonized parameter is par[0][2]
// log total in-patients parameter is par[0][3]
// Time parameter is par[0][4].
// Number abx colonized parameter is par[0][5]
// Susceptible patient on Abx effect on colonizeation is par[0][6].
// Susceptible patient ever on Abx effect on colonizeation is par[0][7].

double MixedICP::acqRate(double time, int onabx, int everabx, double ncolabx, double ncol, double tot)
{
    double gamma = logistic(par[0][1]);
    double y = 0;
    if (ncol > 0)
        y += exp( par[0][2] * log(ncol) + par[0][3] * log(tot) );
    y = (1-gamma) + gamma * y;

    double x = par[0][0] +
        par[0][4] * (time -tOrigin) +
        par[0][5] * ncolabx +
        par[0][6] * onabx +
        par[0][7] * everabx;

    return y * exp(x);
}

double MixedICP::timePar()
{
    return par[0][4];
}

double MixedICP::unTransform(int i, int j)
{
    if (i == 0 && j == 1)
        return logistic(par[i][j]);
    return exp(par[i][j]);
}

void MixedICP::set(int i, int j, double value, int update, double prival, double priorn)
{
    if (i == 0 && j == 1)
        setWithLogitTransform(i,j,value,update,prival,priorn,0.1);
    else
        setWithLogTransform(i,j,value,update,prival,priorn,0.1);
}
} // namespace lognormal
