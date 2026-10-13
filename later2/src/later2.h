#ifndef _LATER2_H_
#define _LATER2_H_

#include "callback_registry.h"
#include <memory>

// This should be kept in sync with LATER2_H_API_VERSION in
// inst/include/later2_api.h. Whenever the interface between
// inst/include/later2_api.h and the code in src/ changes, these values
// should be incremented.
#define LATER2_DLL_API_VERSION_MAJOR 0
#define LATER2_DLL_API_VERSION_MINOR 1
#define LATER2_DLL_API_VERSION_PATCH 0
#define LATER2_DLL_API_VERSION                                                 \
  (LATER2_DLL_API_VERSION_MAJOR * 10000 + LATER2_DLL_API_VERSION_MINOR * 100 + \
   LATER2_DLL_API_VERSION_PATCH)

#define GLOBAL_LOOP 0

std::shared_ptr<CallbackRegistry> getGlobalRegistry();

bool execCallbacksForTopLevel();
bool at_top_level();

bool execCallbacks(double timeoutSecs, bool runAll, int loop_id);
bool idle(int loop);

void ensureInitialized();
// Declare platform-specific functions that are implemented in later_posix.cpp
// and later_win32.cpp.
void ensureAutorunnerInitialized();

uint64_t doExecLater(std::shared_ptr<CallbackRegistry> callbackRegistry,
                     SEXP callback, double delaySecs, bool resetTimer);
uint64_t doExecLater(std::shared_ptr<CallbackRegistry> callbackRegistry,
                     void (*callback)(void *), void *data, double delaySecs,
                     bool resetTimer);

#endif // _LATER2_H_
