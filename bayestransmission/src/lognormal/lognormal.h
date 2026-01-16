
#ifndef _LOGNORMAL_DEFINED

	#define _LOGNORMAL_DEFINED

	#include <stdlib.h>
	#include <limits>
	#include <math.h>
	#include <sstream>
	#include <exception>
    #include <stdexcept>
    #include <Rcpp.h>

    using namespace std;

    #include "../util/util.h"
    #include "../infect/infect.h"
    #include "../modeling/modeling.h"

	namespace lognormal
	{
		#include "LogNormalICP.h"
		#include "LogNormalMassAct.h"
		#include "LogNormalAbxICP.h"
		#include "MultiUnitAbxICP.h"
		#include "MixedICP.h"

	#include "LogNormalModel.h"
	#include "MixedModel.h"

#include "LinearAbxModel.h"
#include "LinearAbxModel2.h"
	}
#endif
