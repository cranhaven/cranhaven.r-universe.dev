#ifndef ALUN_LOGNORMAL_LINEARABXICP_H
#define ALUN_LOGNORMAL_LINEARABXICP_H

#include "LogNormalICP.h"

class LinearAbxICP: public LogNormalICP
{
protected:

    static constexpr double timepartol = 0.000000001;

public:
    using LogNormalICP::set;

    LinearAbxICP(int nst, int nmet, int nacqpar = 7);
    virtual string header() const  override;
    virtual std::vector<std::string> paramNames() const override;
    virtual double getRate(int i, int risk, int ever, int cur) const;
    virtual double acqRate(int nsus, int onabx, int everabx, int ncolabx, int ncol, int tot, double time);

    virtual void set(int i, int j, double value, int update, double prival, double priorn) override;

// LogNormalICP Implementation
    virtual double logProgressionRate(double time, PatientState *p, LocationState *s) override;
    virtual double logProgressionGap(double t0, double t1, LocationState *s) override;
    virtual double logClearanceRate(double time, PatientState *p, LocationState *s) override;
    virtual double logClearanceGap(double t0, double t1, LocationState *s) override;
    virtual double logAcquisitionRate(double time, PatientState *p, LocationState *ls) override;
    virtual double logAcquisitionGap(double u, double v, LocationState *ls) override;
    virtual double *acquisitionRates(double time, PatientState *p, LocationState *ls) override;

    virtual double unTransform(int i, int j) override;
};


#endif // ALUN_LOGNORMAL_LINEARABXICP_H
