#include <atomic>
#include <functional>
#include <memory>
#include <vector>

#include "callback_registry.h"
#include "debug.h"

static std::atomic<uint64_t> nextCallbackId(1);

// ============================================================================
// StdFunctionCallback
// ============================================================================

StdFunctionCallback::StdFunctionCallback(Timestamp when,
                                         std::function<void(void)> func)
    : Callback(when), func(func) {
  this->callbackId = nextCallbackId++;
}

cpp4r::sexp StdFunctionCallback::rRepresentation() const {
  ASSERT_MAIN_THREAD()
  return cpp4r::writable::list(
      {cpp4r::named_arg("id") = (double)callbackId,
       cpp4r::named_arg("when") = when.diff_secs(Timestamp()),
       cpp4r::named_arg("callback") = "C/C++ function"});
}

// ============================================================================
// cpp4rFunctionCallback
// ============================================================================

cpp4rFunctionCallback::cpp4rFunctionCallback(Timestamp when, SEXP func)
    : Callback(when), func(func) {
  ASSERT_MAIN_THREAD()
  this->callbackId = nextCallbackId++;
}

cpp4r::sexp cpp4rFunctionCallback::rRepresentation() const {
  ASSERT_MAIN_THREAD()
  return cpp4r::writable::list(
      {cpp4r::named_arg("id") = (double)callbackId,
       cpp4r::named_arg("when") = when.diff_secs(Timestamp()),
       cpp4r::named_arg("callback") = static_cast<SEXP>(func)});
}

// ============================================================================
// CallbackRegistry
// ============================================================================

[[cpp4r::register]] void testCallbackOrdering() {
  std::vector<StdFunctionCallback> callbacks;
  Timestamp ts;
  std::function<void(void)> func;
  for (size_t i = 0; i < 100; i++) {
    callbacks.push_back(StdFunctionCallback(ts, func));
  }
  for (size_t i = 1; i < 100; i++) {
    if (callbacks[i] < callbacks[i - 1]) {
      cpp4r::stop("Callback ordering is broken [1]");
    }
    if (!(callbacks[i] > callbacks[i - 1])) {
      cpp4r::stop("Callback ordering is broken [2]");
    }
    if (callbacks[i - 1] > callbacks[i]) {
      cpp4r::stop("Callback ordering is broken [3]");
    }
    if (!(callbacks[i - 1] < callbacks[i])) {
      cpp4r::stop("Callback ordering is broken [4]");
    }
  }
  for (size_t i = 100; i > 1; i--) {
    if (callbacks[i - 1] < callbacks[i - 2]) {
      cpp4r::stop("Callback ordering is broken [2]");
    }
  }
}

CallbackRegistry::CallbackRegistry(int id, Mutex *mutex,
                                   ConditionVariable *condvar)
    : id(id), mutex(mutex), condvar(condvar){ASSERT_MAIN_THREAD()}

      CallbackRegistry::~CallbackRegistry() {
  ASSERT_MAIN_THREAD()
}

int CallbackRegistry::getId() const { return id; }

uint64_t CallbackRegistry::add(SEXP func, double secs) {
  // R functions should only be accessed from the main thread.
  ASSERT_MAIN_THREAD()
  Timestamp when(secs);
  Callback_sp cb = std::make_shared<cpp4rFunctionCallback>(when, func);
  Guard guard(mutex);
  queue.insert(cb);
  condvar->signal();

  return cb->getCallbackId();
}

uint64_t CallbackRegistry::add(void (*func)(void *), void *data, double secs) {
  Timestamp when(secs);
  Callback_sp cb =
      std::make_shared<StdFunctionCallback>(when, std::bind(func, data));
  Guard guard(mutex);
  queue.insert(cb);
  condvar->signal();

  return cb->getCallbackId();
}

bool CallbackRegistry::cancel(uint64_t id) {
  Guard guard(mutex);

  cbSet::const_iterator it;
  for (it = queue.begin(); it != queue.end(); ++it) {
    if ((*it)->getCallbackId() == id) {
      queue.erase(it);
      return true;
    }
  }

  return false;
}

// The smallest timestamp present in the registry, if any.
// Use this to determine the next time we need to pump events.
Optional<Timestamp> CallbackRegistry::nextTimestamp(bool recursive) const {
  Guard guard(mutex);

  Optional<Timestamp> minTimestamp;

  if (!this->queue.empty()) {
    cbSet::const_iterator it = queue.begin();
    minTimestamp = Optional<Timestamp>((*it)->when);
  }

  // Now check children
  if (recursive) {
    for (std::vector<std::shared_ptr<CallbackRegistry>>::const_iterator it =
             children.begin();
         it != children.end(); ++it) {
      Optional<Timestamp> childNextTimestamp = (*it)->nextTimestamp(recursive);

      if (childNextTimestamp.has_value()) {
        if (minTimestamp.has_value()) {
          if (*childNextTimestamp < *minTimestamp) {
            minTimestamp = childNextTimestamp;
          }
        } else {
          minTimestamp = childNextTimestamp;
        }
      }
    }
  }

  return minTimestamp;
}

bool CallbackRegistry::empty() const {
  if (fd_waits.load() > 0) {
    return false;
  }
  Guard guard(mutex);
  return this->queue.empty();
}

// Returns true if the smallest timestamp exists and is not in the future.
bool CallbackRegistry::due(const Timestamp &time, bool recursive) const {
  ASSERT_MAIN_THREAD()
  Guard guard(mutex);
  cbSet::const_iterator cbSet_it = queue.begin();
  if (!this->queue.empty() && !((*cbSet_it)->when > time)) {
    return true;
  }

  // Now check children
  if (recursive) {
    for (std::vector<std::shared_ptr<CallbackRegistry>>::const_iterator it =
             children.begin();
         it != children.end(); ++it) {
      if ((*it)->due(time, true)) {
        return true;
      }
    }
  }

  return false;
}

Callback_sp CallbackRegistry::pop(const Timestamp &time) {
  ASSERT_MAIN_THREAD()
  Guard guard(mutex);
  Callback_sp result;
  if (this->due(time, false)) {
    cbSet::iterator it = queue.begin();
    result = *it;
    this->queue.erase(it);
  }
  return result;
}

bool CallbackRegistry::wait(double timeoutSecs, bool recursive) const {
  ASSERT_MAIN_THREAD()
  if (timeoutSecs == R_PosInf || timeoutSecs < 0) {
    // "1000 years ought to be enough for anybody" --Bill Gates
    timeoutSecs = 3e10;
  }

  Timestamp expireTime(timeoutSecs);

  Guard guard(mutex);
  while (true) {
    Timestamp end = expireTime;
    Optional<Timestamp> next = nextTimestamp(recursive);
    if (next.has_value() && *next < expireTime) {
      end = *next;
    }
    double waitFor = end.diff_secs(Timestamp());
    if (waitFor <= 0)
      break;
    // Don't wait for more than 2 seconds at a time, in order to keep us
    // at least somewhat responsive to user interrupts
    if (waitFor > 2) {
      waitFor = 2;
    }
    condvar->timedwait(waitFor);
    R_CheckUserInterrupt();
  }

  return due();
}

cpp4r::sexp CallbackRegistry::list() const {
  ASSERT_MAIN_THREAD()
  Guard guard(mutex);

  cpp4r::sexp results(cpp4r::safe[Rf_allocVector](VECSXP, queue.size()));
  size_t i = 0;
  for (cbSet::const_iterator it = queue.begin(); it != queue.end(); ++it, ++i) {
    cpp4r::sexp item = (*it)->rRepresentation();
    SET_VECTOR_ELT(static_cast<SEXP>(results), i, static_cast<SEXP>(item));
  }

  return results;
}

void CallbackRegistry::fd_waits_incr() { ++fd_waits; }

void CallbackRegistry::fd_waits_decr() { --fd_waits; }
