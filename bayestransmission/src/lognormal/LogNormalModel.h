#ifndef ALUN_LOGNORMAL_LOGNORMALMODEL_H
#define ALUN_LOGNORMAL_LOGNORMALMODEL_H

#include "../modeling/modeling.h"
#include "LogNormalAbxICP.h"
// #include "MultiUnitAbxICP.h"


class LogNormalModel : public BasicModel
{
protected:

    bool abxbyonoff;

    LogNormalModel(int nst, int fw, int ch);

public:

	LogNormalModel(int nst, int abxtest, int nmetro, int fw = 0, int ch = 0);
    LogNormalModel(List *l, int nst, int abxtest, int nmetro, int fw = 0, int ch = 0);
    ~LogNormalModel();

    inline RandomTestParams* getClinicalTestParams() const
    {
        return (RandomTestParams *)clintsp;
    }

    LogNormalAbxICP * getInColParams() const;
    virtual int needEventType(EventCode e) override;
    virtual PatientState *makePatientState(Patient *p) override;
    virtual LocationState *makeUnitState(Unit *u) override;
    virtual void setAbx(bool onoff, double delay, double life);
    virtual void handleAbxDoses(HistoryLink *shead) override;

	virtual void read(istream &is);
protected:
	static void skipLine(istream &is);
	virtual void readInColParams(LogNormalICP *icp, istream &is);
	virtual void readInsituParams(InsituParams *isp, istream &is);
	virtual void readTestParams(TestParams *stsp, istream &is);
	virtual void readRandomTestParams(RandomTestParams *ctsp, istream &is);
	virtual void readOutColParams(OutColParams *ocp, istream &is);
	virtual void readAbxParams(AbxParams *abxp, istream &is);

private:

	List *dumpers;


public:
	virtual void write (ostream &os) const override;
/*
	virtual void writeHeader(ostream &os)
	{
		isp->writeHeader(os);
		os << "\t";
		survtsp->writeHeader(os);
		os << "\t";
		clintsp->writeHeader(os);
		os << "\t";
		ocp->writeHeader(os);
		os << "\t";
		icp->writeHeader(os);
		os << "\t";
		abxp->writeHeader(os);
		os << "\t";
	}
 */
};
#endif // ALUN_LOGNORMAL_LOGNORMALMODEL_H
