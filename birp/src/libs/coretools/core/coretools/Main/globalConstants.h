/*
 * globalConstants.h
 *
 *  Created on: Jun 8, 2020
 *      Author: phaentu
 */

#ifndef COMMONUTILITIES_GLOBALCONSTANTS_H_
#define COMMONUTILITIES_GLOBALCONSTANTS_H_

#include <string>

namespace coretools {

//---------------------------------------------------------------------------
// GLOBAL constants
// These constants are set by TMain upon construction
// There are accessible throughout the program BUT SHOULD NEVER BE CHANGED
//---------------------------------------------------------------------------
extern std::string __GLOBAL_APPLICATION_NAME__;
extern std::string __GLOBAL_APPLICATION_VERSION__;
extern std::string __GLOBAL_APPLICATION_COMMIT__;

}; // namespace coretools

#endif /* COMMONUTILITIES_GLOBALCONSTANTS_H_ */
