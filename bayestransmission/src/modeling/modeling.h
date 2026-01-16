
#ifndef _MODELS_DEFINED

	#define _MODELS_DEFINED

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

	// Model parameter classes.
	#include "Parameters.h"
	#include "TestParams.h"
	#include "TestParamsAbx.h"
	#include "InsituParams.h"
	#include "RandomTestParams.h"
	#include "OutColParams.h"
	#include "InColParams.h"
	#include "MassActionICP.h"
	#include "AbxParams.h"
	#include "UnitLinkedModel.h"

	// Models.
	#include "ForwardSimulator.h"
	#include "ConstrainedSimulator.h"
	#include "BasicModel.h"
	#include "DummyModel.h"
	#include "MassActionModel.h"

	// Command line options handling.
	#include "Options.h"

	using namespace models;

#endif
