/*
 * globalConstants.cpp
 *
 *  Created on: Jul 30, 2020
 *      Author: phaentu
 */

#include "coretools/Main/globalConstants.h"

namespace coretools {

//---------------------------------------------------------------------------
// GLOBAL constants
// These constants are set by TMain upon construction
// There are accessible throughout the program BUT SHOULD NEVER BE CHANGED
//---------------------------------------------------------------------------
std::string __GLOBAL_APPLICATION_NAME__;
std::string __GLOBAL_APPLICATION_VERSION__;
std::string __GLOBAL_APPLICATION_COMMIT__;

}; // namespace coretools
