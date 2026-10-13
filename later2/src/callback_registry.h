#ifndef _CALLBACK_REGISTRY_H_
#define _CALLBACK_REGISTRY_H_

#include "optional.h"
#include "threadutils.h"
#include "timestamp.h"
#include <atomic>
#include <cpp4r.hpp>
#include <functional>
#include <memory>
#include <set>
#include <vector>

// Callback is an abstract class with two subclasses. The reason that there
// are two subclasses is because one of them is for C++ (std::function)
// callbacks, and the other is for R function callbacks. Because
// Callbacks can be created from either the main thread or a background
// thread, the top-level Callback class cannot hold R objects directly on a
// background thread, which would cause memory corruption.

class Callback {

public:
  virtual ~Callback() {};
  Callback(Timestamp when) : when(when) {};

  bool operator<(const Callback &other) const {
    return this->when < other.when ||
           (!(this->when > other.when) && this->callbackId < other.callbackId);
  }

  bool operator>(const Callback &other) const { return other < *this; }

  uint64_t getCallbackId() const { return callbackId; };

  virtual void invoke() const = 0;

  virtual cpp4r::sexp rRepresentation() const = 0;

  Timestamp when;

protected:
  // Used to break ties when comparing to a callback that has precisely the same
  // timestamp
  uint64_t callbackId;
};

class StdFunctionCallback : public Callback {
public:
  StdFunctionCallback(Timestamp when, std::function<void(void)> func);

  void invoke() const {
    // Do NOT use cpp4r::unwind_protect here. Nesting R_UnwindProtect calls that
    // share the same static token (as cpp4r's unwind_protect does per
    // compilation unit) causes the outer RCNTXT to be left dangling on R's
    // context chain when a C++ exception propagates through the outer
    // R_UnwindProtect C frame without endcontext() being called. This corrupts
    // R's context chain and causes crashes.
    //
    // Instead, let C++ exceptions propagate naturally: END_cpp4r (the generated
    // wrapper around execCallbacks) catches std::exception and unwind_exception
    // and handles them correctly. R errors (Rf_error longjmps) propagate
    // directly to R's nearest tryCatch handler.
    try {
      func();
    } catch (const cpp4r::unwind_exception &) {
      throw;
    } catch (const std::exception &) {
      throw;
    } catch (...) {
      throw std::runtime_error("C++ exception of unknown type");
    }
  }

  cpp4r::sexp rRepresentation() const;

private:
  std::function<void(void)> func;
};

class cpp4rFunctionCallback : public Callback {
public:
  cpp4rFunctionCallback(Timestamp when, SEXP func);

  void invoke() const { cpp4r::function{static_cast<SEXP>(func)}(); }

  cpp4r::sexp rRepresentation() const;

private:
  cpp4r::sexp func;
};

typedef std::shared_ptr<Callback> Callback_sp;

template <typename T> struct pointer_less_than {
  bool operator()(const T a, const T b) const { return *a < *b; }
};

// Stores R function callbacks, ordered by timestamp.
class CallbackRegistry {
private:
  int id;

  // Most of the behavior of the registry is like a priority queue. However, a
  // std::priority_queue only allows access to the top element, and when we
  // cancel a callback or get a list representation, we need random
  // access, so we'll use a std::set.
  typedef std::set<Callback_sp, pointer_less_than<Callback_sp>> cbSet;
  // This is a priority queue of shared pointers to Callback objects. The
  // reason it is not a priority_queue<Callback> is because that can cause
  // objects to be copied on the wrong thread, and even trigger an R GC event
  // on the wrong thread. https://github.com/r-lib/later/issues/39
  cbSet queue;
  std::atomic<int> fd_waits{};
  Mutex *mutex;
  ConditionVariable *condvar;

public:
  // The CallbackRegistry must be given a Mutex and ConditionVariable when
  // initialized, because they are shared among the CallbackRegistry objects
  // and the CallbackRegistryTable; they serve as a global lock. Note that the
  // lifetime of these objects must be longer than the CallbackRegistry.
  CallbackRegistry(int id, Mutex *mutex, ConditionVariable *condvar);
  ~CallbackRegistry();

  int getId() const;

  // Add a function to the registry, to be executed at `secs` seconds in
  // the future (i.e. relative to the current time).
  uint64_t add(SEXP func, double secs);

  // Add a C function to the registry, to be executed at `secs` seconds in
  // the future (i.e. relative to the current time).
  uint64_t add(void (*func)(void *), void *data, double secs);

  bool cancel(uint64_t id);

  // The smallest timestamp present in the registry, if any.
  // Use this to determine the next time we need to pump events.
  Optional<Timestamp> nextTimestamp(bool recursive = true) const;

  // Is the registry completely empty? (including later_fd waits)
  bool empty() const;

  // Is anything ready to execute?
  bool due(const Timestamp &time = Timestamp(), bool recursive = true) const;

  // Pop and return a function to execute now.
  Callback_sp pop(const Timestamp &time = Timestamp());

  // Wait until the next available callback is ready to execute.
  bool wait(double timeoutSecs, bool recursive) const;

  // Return a List of items in the queue.
  cpp4r::sexp list() const;

  // Increment and decrement the number of active later_fd waits
  void fd_waits_incr();
  void fd_waits_decr();

  // References to parent and children registries. These are used for
  // automatically running child loops. They should only be accessed and
  // modified from the main thread.
  std::shared_ptr<CallbackRegistry> parent;
  std::vector<std::shared_ptr<CallbackRegistry>> children;
};

#endif // _CALLBACK_REGISTRY_H_
