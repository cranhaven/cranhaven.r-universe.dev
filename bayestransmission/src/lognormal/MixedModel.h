#ifndef ALUN_LOGNORMAL_MIXEDMODEL_H
#define ALUN_LOGNORMAL_MIXEDMODEL_H

#include "LogNormalModel.h"
#include "MixedICP.h"

class MixedModel : public LogNormalModel
{
public:

	MixedModel(int nst, int nmetro, int fw = 0, int ch = 0) : LogNormalModel(nst,nmetro,fw,ch)
	{
		InColParams *icp = getInColParams();
		if (icp != 0) delete icp;
		icp = new MixedICP(nst, 0,nmetro);
		setInColParams(icp);
	}
};
#endif // ALUN_LOGNORMAL_MIXEDMODEL_H
