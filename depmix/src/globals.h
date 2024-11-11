
#ifndef GLOBALSLOGL
#define GLOBALSLOGL 1

#include "mgdmm.h"
#include "mmts.h"

extern "C" {

	int totalMemory;
	int printlevel;
	double crit=1000000;
		
	mmts ngdat;
	mmts ngcov;
	mgdmm models;
	mgdmm covpars;

} //end extern "C"

#endif
