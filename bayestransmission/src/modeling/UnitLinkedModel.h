#ifndef ALUN_MODELING_UNITLINKEDMODEL_H
#define ALUN_MODELING_UNITLINKEDMODEL_H

#include "../infect/infect.h"
#include "InsituParams.h"
#include "OutColParams.h"
#include "TestParams.h"
#include "InColParams.h"
#include "AbxParams.h"

/*
	Manages models where it is only required that the
	augmented data events are linked into the Patient and Unit
	history lists, not Facility or System lists.
*/
namespace models {

class UnitLinkedModel : public infect::Model
{
protected:

	int nstates;
	int forwardEnabled;

	InsituParams *isp;
	OutColParams *ocp;
	TestParams *survtsp;
	TestParams *clintsp;
	InColParams *icp;
	AbxParams *abxp;

public:

	UnitLinkedModel(int ns, int fw, int ch);
	virtual double logLikelihood(infect::SystemHistory *hist) override;

public:

	inline int isForwardEnabled() const {return forwardEnabled;}
	inline int getNStates() const {return nstates;}

	// Accessors
	inline InsituParams* getInsituParams() const {return isp;}
	inline OutColParams* getOutColParams() const {return ocp;}
	inline TestParams*   getSurveillanceTestParams() const {return survtsp;}
	inline TestParams*   getClinicalTestParams() const {return clintsp;}
	inline InColParams*  getInColParams() const {return icp;}
	inline AbxParams*    getAbxParams() const {return abxp;}
	// Setters
	inline void setInColParams(InColParams *p) {icp = p;}
	inline void setAbxParams(AbxParams *p) {abxp = p;}

	virtual infect::HistoryLink* makeHistLink(infect::Facility *f, infect::Unit *u, infect::Patient *p, double time, EventCode type, int linked);

// Object
	virtual string header() const override;
    virtual void write (ostream &os) const override;
    /*
	virtual void writeHeader(ostream &os)
	{
		isp->writeHeader(os);
		os << "\t";
		survtsp->writeHeader(os);
		os << "\t";
		if (clintsp != survtsp)
		{
			clintsp->writeHeader(os);
			os << "\t";
		}

		ocp->writeHeader(os);
		os << "\t";
		icp->writeHeader(os);
		os << "\t";
		if (abxp != 0)
		{
			abxp->writeHeader(os);
			os << "\t";
		}
	}
    */
//protected:

// Model
	virtual int needEventType(EventCode e) override;
	virtual infect::LocationState *makeSystemState() override;
	virtual infect::LocationState *makeFacilityState(infect::Facility *f) override;
	virtual infect::LocationState *makeUnitState(infect::Unit *u) override;
	virtual infect::PatientState *makePatientState(infect::Patient *p) override;
	virtual infect::EpisodeHistory *makeEpisodeHistory(infect::HistoryLink *a, infect::HistoryLink *d) override;


	void countUnitStats(infect::HistoryLink *l);
	virtual double logLikelihood(infect::EpisodeHistory *h);
	virtual double logLikelihood(infect::EpisodeHistory *h, int opt);
	virtual double logLikelihood(infect::Patient *pat, infect::HistoryLink *h);
	virtual double logLikelihood(infect::Patient *pat, infect::HistoryLink *h, int opt);
	virtual double logLikelihood(infect::HistoryLink *h);
	virtual double logLikelihood(infect::HistoryLink *h, int dogap);
	void update(infect::SystemHistory *hist, Random *r);
	void update(infect::SystemHistory *hist, Random *r, int max)  override;
	
	// Diagnostic function to get individual log likelihoods
	std::vector<double> getHistoryLinkLogLikelihoods(infect::SystemHistory *hist);
};

} // namespace models
#endif // ALUN_MODELING_UNITLINKEDMODEL_H
