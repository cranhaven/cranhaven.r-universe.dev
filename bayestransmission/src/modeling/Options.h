#ifndef ALUN_MODELING_OPTIONS_H
#define ALUN_MODELING_OPTIONS_H

#include "../infect/infect.h"
using namespace infect;

namespace models{

class Options : public util::Object
{
public:

	int nstates;
	int clinical;
	int densityModel;
	int seed;
	int nburn;
	int nsims;
	int nmetro;
	int allout;
	int verbose;
	int errors;

	Options(int argc, char *argv[]);
	Options();
	Options(int nstates, int clinical, int densityModel, int seed, int nburn, int nsims, int nmetro, int allout, int verbose);

};
} // namespace models
#endif // ALUN_MODELING_OPTIONS_H
