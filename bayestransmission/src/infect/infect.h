
#ifndef _INFECT_DEFINED

	#define _INFECT_DEFINED

	#include <stdlib.h>
	#include <limits>
	#include <math.h>
	#include <sstream>
	#include <exception>
        #include <stdexcept>
	#include <complex>
	#include <memory>
        using namespace std;

    #include "../util/util.h"

	// Enumeration defining event types.

	namespace infect
	{
	    #include "EventCoding.h"
		#include "InfectionCoding.h"
		#include "AbxCoding.h"

	// Classes to define state of system history, including updates given model.
	// Classes that read and sort raw data.

		#include "RawEvent.h"
		#include "RawEventList.h"

	// Basic data classes.

		#include "Patient.h"
		#include "Unit.h"
		#include "Facility.h"
		#include "Event.h"
		#include "Episode.h"
		#include "System.h"

	// Class to define Model, including updates given history.

		#include "Model.h"

	// SystemHistory is the manager class, HistoryLink and the EpisodeHistory
	// classes store and maintain linked lists of the history.
	// State stores the current states of patients, units, facilities, or
	// the system.

		#include "State.h"
		#include "LocationState.h"
		#include "SetLocationState.h"
		#include "PatientState.h"
		#include "AbxLocationState.h"
		#include "AbxPatientState.h"

		#include "HistoryLink.h"
		#include "EpisodeHistory.h"
		#include "SystemEpisodeHistory.h"
		#include "FacilityEpisodeHistory.h"
		#include "UnitEpisodeHistory.h"
		#include "SystemHistory.h"

	// Class to run MCMC sampler.

		#include "Sampler.h"
	}
#endif
