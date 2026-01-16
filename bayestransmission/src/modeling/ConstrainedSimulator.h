#ifndef ALUN_MODELING_CONTRAINTEDSIMULATOR_H
#define ALUN_MODELING_CONTRAINTEDSIMULATOR_H

#include "../infect/infect.h"
#include "UnitLinkedModel.h"

namespace models {

class ConstrainedSimulator : public Object, infect::EventCoding, infect::InfectionCoding
{
protected:

	static inline int stateAfterEvent(int nstates, EventCode ev)
	{
		if (nstates == 2)
		{
			switch(ev)
			{
			case acquisition: return 1;
			case clearance: return 0;
			default:
				return -1;
			}
		}
		if (nstates == 3)
		{
			switch(ev)
			{
			case acquisition: return 1;
			case progression: return 2;
			case clearance: return 0;
			default:
				return -1;
			}
		}
		return -1;
	}

	static inline EventCode eventOutOfState(int nstates, int i)
	{
		if (nstates == 2)
		{
			switch(i)
			{
			case 0: return acquisition;
			case 1: return clearance;
			default:
				return nullevent;
			}
		}
		if (nstates == 3)
		{
			switch(i)
			{
			case 0: return acquisition;
			case 1: return progression;
			case 2: return clearance;
			default:
				return nullevent;
			}
		}
		return nullevent;
	}

	static void getProposal(UnitLinkedModel *mod, infect::EpisodeHistory *h, int *nsim, double **times, int **states);
	static void putProposal(UnitLinkedModel *mod, infect::EpisodeHistory *h, int nsim, double *times, int *states);
	static Markov *getMarkovProcess(UnitLinkedModel *mod, infect::HistoryLink *p, Random *rand, int *nalloc, double **mytime, bool **mydoit, double ***myS, double ****myQ);

public:
	static void sampleEpisodes(UnitLinkedModel *mod, infect::SystemHistory *h, int max, Random *rand);
	static void sampleHistory(UnitLinkedModel *mod, infect::SystemHistory *hist, infect::HistoryLink *plink, int max, Random *rand);
	static void initEpisodeHistory(UnitLinkedModel *mod, infect::EpisodeHistory *eh, bool haspostest);
	static void cheatInitEpisodeHistory(UnitLinkedModel *mod, infect::EpisodeHistory *eh);
};

} // namespace models

#endif //ALUN_MODELING_CONTRAINTEDSIMULATOR_H
