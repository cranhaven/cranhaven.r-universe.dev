#include <cstring>
#include <memory>
#include <queue>

#include <R.h>              // RNG state functions
#include <R_ext/Rdynload.h> // for R_RegisterCCallable, DllInfo
#include <cpp4r.hpp>

using std::shared_ptr;
using namespace cpp4r;

#include "debug.h"
#include "later2.h"
#include "threadutils.h"
#include "utils.h"

#include "callback_registry.h"
#include "callback_registry_table.h"

#include "fd.h" // for struct pollfd
#include "interrupt.h"

static size_t exec_callbacks_reentrancy_count = 0;

// instance has global scope as declared in callback_registry_table.h
CallbackRegistryTable callbackRegistryTable;

class ProtectCallbacks {
public:
  ProtectCallbacks() { exec_callbacks_reentrancy_count++; }
  ~ProtectCallbacks() { exec_callbacks_reentrancy_count--; }
};

// Returns number of frames on the call stack. Basically just a wrapper for
// base::sys.nframe(). Note that this can report that an error occurred if the
// user sends an interrupt while the `sys.nframe()` function is running. I
// believe that the only reason that it should set errorOccurred is because of
// a user interrupt.
int sys_nframe() {
  ASSERT_MAIN_THREAD()
  SEXP e, result;
  int errorOccurred, value;

  BEGIN_SUSPEND_INTERRUPTS {
    PROTECT(e = Rf_lang1(Rf_install("sys.nframe")));
    PROTECT(result = R_tryEval(e, R_BaseEnv, &errorOccurred));

    if (errorOccurred) {
      value = -1;
    } else {
      value = INTEGER(result)[0];
    }

    UNPROTECT(2);
  }
  END_SUSPEND_INTERRUPTS;

  return value;
}

// Returns true if execCallbacks is executing, or sys.nframes() returns 0.
bool at_top_level() {
  ASSERT_MAIN_THREAD()
  if (exec_callbacks_reentrancy_count != 0)
    return false;

  int nframe = sys_nframe();
  if (nframe == -1) {
    cpp4r::stop("Error occurred while calling sys.nframe()");
  }
  return nframe == 0;
}

// ============================================================================
// Current registry/event loop
// ============================================================================
//
// In the R code, the term "loop" is used. In the C++ code, the terms "loop"
// and "registry" are both used. "Loop" is usually used when interfacing with
// R-facing event loop, and "registry" is usually used when interfacing with
// the implementation, which uses a callback registry.
//
// The current registry is kept track of entirely in C++, and not in R
// (although it can be queried from R). This is because when running a loop
// with children, it sets the current loop as it runs each of the children,
// and to do so in R would require calling back into R for each child, which
// would impose more overhead.

static int current_registry;

[[cpp4r::register]] void setCurrentRegistryId(int id) {
  ASSERT_MAIN_THREAD()
  current_registry = id;
}

[[cpp4r::register]] int getCurrentRegistryId() {
  ASSERT_MAIN_THREAD()
  return current_registry;
}

// Class for setting current registry and resetting when function exits, using
// RAII.
class CurrentRegistryGuard {
public:
  CurrentRegistryGuard(int id) {
    ASSERT_MAIN_THREAD()
    old_id = getCurrentRegistryId();
    setCurrentRegistryId(id);
  }
  ~CurrentRegistryGuard() { setCurrentRegistryId(old_id); }

private:
  int old_id;
};

// ============================================================================
// Callback registry functions
// ============================================================================

shared_ptr<CallbackRegistry> getGlobalRegistry() {
  shared_ptr<CallbackRegistry> registry =
      callbackRegistryTable.getRegistry(GLOBAL_LOOP);
  if (registry == nullptr) {
    (Rf_error)("Global registry does not exist.");
  }
  return registry;
}

// This deletes a CallbackRegistry and deregisters it as a child of its
// parent. Any children of this registry are orphaned -- they no longer have a
// parent. (Maybe this should be an option?)
[[cpp4r::register]] bool deleteCallbackRegistry(int loop_id) {
  ASSERT_MAIN_THREAD()
  if (loop_id == GLOBAL_LOOP) {
    cpp4r::stop("Can't destroy global loop.");
  }
  if (loop_id == getCurrentRegistryId()) {
    cpp4r::stop("Can't destroy current loop.");
  }

  return callbackRegistryTable.remove(loop_id);
}

// This is called when the R loop handle is GC'd.
[[cpp4r::register]] bool notifyRRefDeleted(int loop_id) {
  ASSERT_MAIN_THREAD()
  if (loop_id == GLOBAL_LOOP) {
    cpp4r::stop("Can't notify that reference to global loop is deleted.");
  }
  if (loop_id == getCurrentRegistryId()) {
    cpp4r::stop("Can't notify that reference to current loop is deleted.");
  }

  return callbackRegistryTable.notifyRRefDeleted(loop_id);
}

[[cpp4r::register]] void createCallbackRegistry(int id, int parent_id) {
  ASSERT_MAIN_THREAD()
  callbackRegistryTable.create(id, parent_id);
}

[[cpp4r::register]] bool existsCallbackRegistry(int id) {
  ASSERT_MAIN_THREAD()
  return callbackRegistryTable.exists(id);
}

[[cpp4r::register]] cpp4r::sexp list_queue_(int id) {
  ASSERT_MAIN_THREAD()
  shared_ptr<CallbackRegistry> registry = callbackRegistryTable.getRegistry(id);
  if (registry == nullptr) {
    cpp4r::stop("CallbackRegistry does not exist.");
  }
  return registry->list();
}

// Execute callbacks for a single event loop.
bool execCallbacksOne(bool runAll,
                      shared_ptr<CallbackRegistry> callback_registry,
                      Timestamp now) {
  ASSERT_MAIN_THREAD()
  // execCallbacks can be called directly from C code
  GetRNGstate();
  ProtectCallbacks pcscope;

  // Set current loop for the duration of this function.
  CurrentRegistryGuard current_registry_guard(callback_registry->getId());

  do {
    // We only take one at a time, because we don't want to lose callbacks if
    // one of the callbacks throws an error
    Callback_sp callback = callback_registry->pop(now);
    if (callback == nullptr) {
      break;
    }

    // This line may throw errors!
    callback->invoke();

  } while (runAll);

  // I think there's no need to lock this since it's only modified from the
  // main thread. But need to check.
  std::vector<std::shared_ptr<CallbackRegistry>> children =
      callback_registry->children;
  for (std::vector<std::shared_ptr<CallbackRegistry>>::iterator it =
           children.begin();
       it != children.end(); ++it) {
    execCallbacksOne(true, *it, now);
  }

  PutRNGstate();
  return true;
}

// Execute callbacks for an event loop and its children.
[[cpp4r::register]] bool execCallbacks(double timeoutSecs, bool runAll,
                                       int loop_id) {
  ASSERT_MAIN_THREAD()
  shared_ptr<CallbackRegistry> registry =
      callbackRegistryTable.getRegistry(loop_id);
  if (registry == nullptr) {
    cpp4r::stop("CallbackRegistry does not exist.");
  }

  if (!registry->wait(timeoutSecs, true)) {
    return false;
  }

  Timestamp now;
  execCallbacksOne(runAll, registry, now);

  // Call this now, in case any CallbackRegistries which have no R references
  // have emptied.
  callbackRegistryTable.pruneRegistries();
  return true;
}

// This function is called from the input handler on Unix, or the Windows
// equivalent. It may throw exceptions.
//
// Invoke execCallbacks up to 20 times. At the first iteration where no work is
// done, terminate. We call this from the top level instead of just calling
// execCallbacks because the top level only gets called occasionally (every 10's
// of ms), so tasks that generate other tasks will execute surprisingly slowly.
//
// Example:
// promise_map(1:1000, function(i) {
//   message(i)
//   promise_resolve(i)
// })
bool execCallbacksForTopLevel() {
  bool any = false;
  for (size_t i = 0; i < 20; i++) {
    if (!execCallbacks(0, true, GLOBAL_LOOP))
      return any;
    any = true;
  }
  return any;
}

[[cpp4r::register]] bool idle(int loop_id) {
  ASSERT_MAIN_THREAD()
  shared_ptr<CallbackRegistry> registry =
      callbackRegistryTable.getRegistry(loop_id);
  if (registry == nullptr) {
    cpp4r::stop("CallbackRegistry does not exist.");
  }
  return registry->empty();
}

static bool initialized = false;
[[cpp4r::register]] void ensureInitialized() {
  if (initialized) {
    return;
  }
  REGISTER_MAIN_THREAD()

  // Note that the global registry is not created here, but in R, from the
  // .onLoad function.
  setCurrentRegistryId(GLOBAL_LOOP);

  // Call the platform-specific initialization for the mechanism that runs the
  // event loop when the console is idle.
  ensureAutorunnerInitialized();
  initialized = true;
}

[[cpp4r::register]] std::string execLater(SEXP callback, double delaySecs,
                                          int loop_id) {
  ASSERT_MAIN_THREAD()
  ensureInitialized();
  shared_ptr<CallbackRegistry> registry =
      callbackRegistryTable.getRegistry(loop_id);
  if (registry == nullptr) {
    cpp4r::stop("CallbackRegistry does not exist.");
  }
  uint64_t callback_id = doExecLater(registry, callback, delaySecs, true);

  // We have to convert it to a string in order to maintain 64-bit precision,
  // since R doesn't support 64 bit integers.
  return toString(callback_id);
}

bool cancel(uint64_t callback_id, int loop_id) {
  ASSERT_MAIN_THREAD()
  shared_ptr<CallbackRegistry> registry =
      callbackRegistryTable.getRegistry(loop_id);
  if (registry == nullptr) {
    return false;
  }
  return registry->cancel(callback_id);
}

[[cpp4r::register]] bool cancel(std::string callback_id_s, int loop_id) {
  ASSERT_MAIN_THREAD()
  uint64_t callback_id;
  std::istringstream iss(callback_id_s);
  iss >> callback_id;

  // If the input is good (just a number with no other text) then eof will be
  // 1 and fail will be 0.
  if (!(iss.eof() && !iss.fail())) {
    return false;
  }

  return cancel(callback_id, loop_id);
}

[[cpp4r::register]] double nextOpSecs(int loop_id) {
  ASSERT_MAIN_THREAD()
  shared_ptr<CallbackRegistry> registry =
      callbackRegistryTable.getRegistry(loop_id);
  if (registry == nullptr) {
    cpp4r::stop("CallbackRegistry does not exist.");
  }

  Optional<Timestamp> nextTime = registry->nextTimestamp();
  if (!nextTime.has_value()) {
    return R_PosInf;
  } else {
    Timestamp now;
    return nextTime->diff_secs(now);
  }
}

// Schedules a C function to execute on a specific event loop. Returns
// callback ID on success, or 0 on error.
extern "C" uint64_t execLaterNative2(void (*func)(void *), void *data,
                                     double delaySecs, int loop_id) {
  ensureInitialized();
  return callbackRegistryTable.scheduleCallback(func, data, delaySecs, loop_id);
}

extern "C" int execLaterFdNative(void (*)(int *, void *), void *, int,
                                 struct pollfd *, double, int);
extern "C" int apiVersion();

namespace {
// R_RegisterCCallable requires a DL_FUNC (void (*)()), which necessarily
// differs from the real signature of each callable. A direct cast between
// incompatible function pointer types triggers -Wcast-function-type, and
// CRAN's checks disallow suppressing that with compiler pragmas. Reinterpret
// the pointer by copying its bytes instead: this never casts between
// function pointer types (memcpy's arguments are ordinary object pointers to
// the local function-pointer variables), so it is warning-free and portable
// across the platforms R supports.
template <typename Fn> DL_FUNC to_dl_func(Fn fn) {
  static_assert(sizeof(DL_FUNC) == sizeof(Fn),
                "function pointer size mismatch");
  DL_FUNC out;
  std::memcpy(&out, &fn, sizeof(out));
  return out;
}
} // namespace

[[cpp4r::init]] void later_init(DllInfo *dll) {
  (void)dll; // squelch compiler warning
  R_RegisterCCallable("later2", "execLaterNative2",
                      to_dl_func(&execLaterNative2));
  R_RegisterCCallable("later2", "execLaterFdNative",
                      to_dl_func(&execLaterFdNative));
  R_RegisterCCallable("later2", "apiVersion", to_dl_func(&apiVersion));
}

extern "C" int apiVersion() { return LATER2_DLL_API_VERSION; }
