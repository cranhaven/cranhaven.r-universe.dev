// infect/Sampler.h
#ifndef ALUN_INFECT_SAMPLER_H
#define ALUN_INFECT_SAMPLER_H

#include "../util/util.h"
#include "SystemHistory.h"

class Sampler : public Object
{
private:

	SystemHistory *hist;
	Model *model;
	Random *rand;

public:

	Sampler(SystemHistory *h, Model *m, Random *r);

	virtual void sampleModel();
	virtual void sampleModel(int max);
	virtual void sampleEpisodes();
	virtual void sampleEpisodes(int max);
	void initializeEpisodes();
};

#endif // ALUN_INFECT_SAMPLER_H
