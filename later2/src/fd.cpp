#include "fd.h"
#include "callback_registry_table.h"
#include "later2.h"
#include "tinycthread.h"
#include <atomic>
#include <cpp4r.hpp>
#include <cstdlib>
#include <memory>
#include <unistd.h>
#include <vector>

class ThreadArgs {
public:
  ThreadArgs(int num_fds, struct pollfd *fds, double timeout, int loop,
             CallbackRegistryTable &table)
      : timeout(createTimestamp(timeout)),
        active(std::make_shared<std::atomic<bool>>(true)),
        fds(std::vector<struct pollfd>(fds, fds + num_fds)),
        results(std::vector<int>(num_fds)), loop(loop),
        registry(table.getRegistry(loop)) {

    if (registry == nullptr)
      throw std::runtime_error("CallbackRegistry does not exist.");

    registry->fd_waits_incr();
  }

  ThreadArgs(SEXP func, int num_fds, struct pollfd *fds, double timeout,
             int loop, CallbackRegistryTable &table)
      : ThreadArgs(num_fds, fds, timeout, loop, table) {
    callback = cpp4r::sexp(func);
  }

  ThreadArgs(void (*func)(int *, void *), void *data, int num_fds,
             struct pollfd *fds, double timeout, int loop,
             CallbackRegistryTable &table)
      : ThreadArgs(num_fds, fds, timeout, loop, table) {
    callback_native = std::bind(func, std::placeholders::_1, data);
  }

  ~ThreadArgs() { registry->fd_waits_decr(); }

  Timestamp timeout;
  std::shared_ptr<std::atomic<bool>> active;
  cpp4r::sexp callback;
  std::function<void(int *)> callback_native = nullptr;
  std::vector<struct pollfd> fds;
  std::vector<int> results;
  const int loop;

private:
  std::shared_ptr<CallbackRegistry> registry;

  static Timestamp createTimestamp(double timeout) {
    if (timeout > 3e10) {
      timeout =
          3e10; // "1000 years ought to be enough for anybody" --Bill Gates
    } else if (timeout < 0) {
      timeout =
          1; // curl_multi_timeout() uses -1 to denote a default we set at 1s
    }
    return Timestamp(timeout);
  }
};

static void later_callback(void *arg) {

  ASSERT_MAIN_THREAD()

  std::unique_ptr<ThreadArgs> args(static_cast<ThreadArgs *>(arg));
  bool still_active = true;
  // atomic compare_exchange_strong:
  // if args->active is true, it is changed to false (so future requests to
  // fd_cancel return false) if args->active is false (cancelled), still_active
  // is changed to false
  args->active->compare_exchange_strong(still_active, false);
  if (!still_active)
    return;
  if (static_cast<SEXP>(args->callback) != R_NilValue) {
    cpp4r::sexp results_sexp(
        cpp4r::safe[Rf_allocVector](LGLSXP, args->results.size()));
    for (size_t i = 0; i < args->results.size(); i++) {
      SET_LOGICAL_ELT(static_cast<SEXP>(results_sexp), i, args->results[i]);
    }
    cpp4r::function(static_cast<SEXP>(args->callback))(results_sexp);
  } else {
    args->callback_native(args->results.data());
  }
}

// CONSIDER: if necessary to add method for HANDLES on Windows. Would be
// different code to SOCKETs.
// TODO: implement re-usable background thread.
static int wait_thread(void *arg) {

  tct_thrd_detach(tct_thrd_current());

  std::unique_ptr<ThreadArgs> args(static_cast<ThreadArgs *>(arg));

  int ready;
  double waitFor = std::fmax(args->timeout.diff_secs(Timestamp()), 0);
  do {
    // Never wait for longer than ~1 second so we can check for cancellation
    waitFor = std::fmin(waitFor, 1.024);
    ready = LATER2_POLL_FUNC(args->fds.data(),
                             static_cast<LATER2_NFDS_T>(args->fds.size()),
                             static_cast<int>(waitFor * 1000));
    if (!args->active->load())
      return 1;
    if (ready)
      break;
  } while ((waitFor = args->timeout.diff_secs(Timestamp())) > 0);

  if (ready > 0) {
    for (std::size_t i = 0; i < args->fds.size(); i++) {
      (args->results)[i] = (args->fds)[i].revents == 0 ? 0
                           : (args->fds)[i].revents & (POLLIN | POLLOUT)
                               ? 1
                               : NA_INTEGER;
    }
  } else if (ready < 0) {
    std::fill(args->results.begin(), args->results.end(), NA_INTEGER);
  }

  int loop_id = args->loop;
  callbackRegistryTable.scheduleCallback(
      later_callback, static_cast<void *>(args.release()), 0, loop_id);

  return 0;
}

static SEXP execLater_fd_impl(SEXP callback, int num_fds, struct pollfd *fds,
                              double timeout, int loop_id) {

  std::unique_ptr<ThreadArgs> args(new ThreadArgs(
      callback, num_fds, fds, timeout, loop_id, callbackRegistryTable));
  std::shared_ptr<std::atomic<bool>> active = args->active;
  tct_thrd_t thr;

  if (tct_thrd_create(&thr, &wait_thread,
                      static_cast<void *>(args.release())) != tct_thrd_success)
    cpp4r::stop("Thread creation failed");

  cpp4r::external_pointer<std::shared_ptr<std::atomic<bool>>> xptr(
      new std::shared_ptr<std::atomic<bool>>(active));
  return static_cast<SEXP>(xptr);
}

// native version
static int execLater_fd_native(void (*func)(int *, void *), void *data,
                               int num_fds, struct pollfd *fds, double timeout,
                               int loop_id) {

  std::unique_ptr<ThreadArgs> args(new ThreadArgs(
      func, data, num_fds, fds, timeout, loop_id, callbackRegistryTable));
  tct_thrd_t thr;

  return tct_thrd_create(&thr, &wait_thread,
                         static_cast<void *>(args.release())) !=
         tct_thrd_success;
}

[[cpp4r::register]] SEXP execLater_fd(SEXP callback, cpp4r::integers readfds,
                                      cpp4r::integers writefds,
                                      cpp4r::integers exceptfds,
                                      SEXP timeoutSecs,
                                      cpp4r::integers loop_id) {

  const int rfds = static_cast<int>(readfds.size());
  const int wfds = static_cast<int>(writefds.size());
  const int efds = static_cast<int>(exceptfds.size());
  const int num_fds = rfds + wfds + efds;
  const double timeout = num_fds ? Rf_asReal(timeoutSecs) : 0;
  const int loop = loop_id[0];

  std::vector<struct pollfd> pollfds;
  pollfds.reserve(num_fds);
  struct pollfd pfd;

  for (int i = 0; i < rfds; i++) {
    pfd.fd = readfds[i];
    pfd.events = POLLIN;
    pfd.revents = 0;
    pollfds.push_back(pfd);
  }
  for (int i = 0; i < wfds; i++) {
    pfd.fd = writefds[i];
    pfd.events = POLLOUT;
    pfd.revents = 0;
    pollfds.push_back(pfd);
  }
  for (int i = 0; i < efds; i++) {
    pfd.fd = exceptfds[i];
    pfd.events = 0;
    pfd.revents = 0;
    pollfds.push_back(pfd);
  }

  return execLater_fd_impl(callback, num_fds, pollfds.data(), timeout, loop);
}

[[cpp4r::register]] bool fd_cancel(SEXP xptr) {

  cpp4r::external_pointer<std::shared_ptr<std::atomic<bool>>> active(xptr);

  bool cancelled = true;
  // atomic compare_exchange_strong:
  // if *active is true, *active is changed to false (successful cancel)
  // if *active is false (already run or cancelled), cancelled is changed to
  // false
  (*active)->compare_exchange_strong(cancelled, false);

  return cancelled;
}

// Schedules a C function that takes a pointer to an integer array (provided by
// this function when calling back) and a void * argument, to execute on file
// descriptor readiness. Returns 0 upon success and 1 if creating the wait
// thread failed. NOTE: this is different to execLaterNative2() which returns 0
// on failure.
extern "C" int execLaterFdNative(void (*func)(int *, void *), void *data,
                                 int num_fds, struct pollfd *fds,
                                 double timeoutSecs, int loop_id) {
  ensureInitialized();
  return execLater_fd_native(func, data, num_fds, fds, timeoutSecs, loop_id);
}
