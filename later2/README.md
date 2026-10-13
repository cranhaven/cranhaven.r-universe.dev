# later2

<!-- badges: start -->

<!-- badges: end -->

Schedule an R function or formula to run after a specified period of time. This is similar to JavaScript's `setTimeout` function.
Like JavaScript, R is single-threaded so there's no guarantee that the operation will run exactly at the requested time, only that
at least that much time will elapse.

To avoid bugs due to reentrancy, by default, scheduled operations only run when there is no other R code present on the execution
stack; i.e., when R is sitting at the top-level prompt. You can force past-due operations to run at a time of your choosing by
calling `later2::run_now()`.

This package is derived from [later](https://github.com/r-lib/later), which uses derived work from
[background](https://github.com/s-u/background) package and similar code in
[Rhttpd](https://search.r-project.org/CRAN/refmans/Rook/html/Rhttpd-class.html).

## Installation

You can install the development version of later with:

``` r
pak::pak("pachadotdev/later2")
```

The package is not currently on CRAN.

## Usage from R

Pass a function (in this case, delayed by 5 seconds):

``` r
later2::later(\() print("Got here!"), 5)
```

Or a formula (in this case, run as soon as control returns to the top-level):

``` r
later2::later(~print("Got here!"))
```

### File Descriptor Readiness

It is also possible to have a function run based on when file descriptors are ready for reading or
writing, at an unknown time in the future.

Below, a logical vector is printed indicating which of file descriptors 21 or 22 were ready,
subject to a timeout of 1s.

``` r
later2::later_fd(\(x) print(x), c(21L, 22L), timeout = 1)
```

This is useful in particular for asynchronous I/O, allowing reads to be made from TCP sockets as
soon as data becomes available. Functions such as `curl::multi_fdset()` return the relevant file
descriptors to be monitored.

## Usage from C++

You can also call `later2::later` from C++ code in your own packages, to cause your own C-style
functions to be called back. This is safe to call from either the main R thread or a different
thread; in both cases, your callback will be invoked from the main R thread.

`later2::later` is accessible from `later2_api.h` and its prototype looks like this:

``` cpp
void later(void (*func)(void*), void* data, double secs)
```

The first argument is a pointer to a function that takes one `void*` argument and returns void. The
second argument is a `void*` that will be passed to the function when it's called back. And the
third argument is the number of seconds to wait (at a minimum) before invoking.

`later2::later_fd` is also accessible from `later2_api.h` and its prototype looks like this:

``` cpp
void later_fd(void (*func)(int *, void *), void *data, int num_fds, struct pollfd *fds, double secs)
```

The first argument is a pointer to a function that takes two arguments:

1. An `int*` array provided by `later_fd()` when called back, and the second being
  a `void*`. The `int*` array will be the length of `num_fds` and contain the values `0`, `1` or
  `NA_INTEGER` to indicate the readiness of each file descriptor, or an error condition
  respectively.
2. A `data` object passed to the `void*` argument of the function when it's called back. The other
  required arguments are the total number of file descriptors, a pointer to an array of
  `stuct pollfd`, and the number of seconds to wait until timing out.

To use the C++ interface, you'll need to add `later` to your `DESCRIPTION` file under
`Imports` and `LinkingTo`, and also make sure that your `NAMESPACE` file has an `import(later)`
entry. See the [later2test](https://github.com/pachadotdev/later2/tree/main/later2test) side package
which I created to test later2 to check how to use it from another package.

### Background tasks

Finally, this package also offers a higher-level C++ helper class to make it easier to execute tasks
on a background thread. It is also available from `later2_api.h` and its public/protected interface
looks like this:

``` cpp
class BackgroundTask {

public:
  BackgroundTask();
  virtual ~BackgroundTask();

  // Start executing the task
  void begin();

protected:
  // The task to be executed on the background thread.
  // Neither the R runtime nor any R data structures may be
  // touched from the background thread; any values that need
  // to be passed into or out of the Execute method must be
  // included as fields on the Task subclass object.
  virtual void execute() = 0;

  // A short task that runs on the main R thread after the
  // background task has completed. It's safe to access the
  // R runtime and R data structures from here.
  virtual void complete() = 0;
}
```

Create your own subclass, implementing a custom constructor plus the `execute` and `complete`
methods.

It's critical that the code in your `execute` method not mutate any R data structures, call any R
code, or cause any R allocations, as it will execute in a background thread where such operations
are unsafe. You can, however, perform such operations in the constructor (assuming you perform
construction only from the main R thread) and `complete` method. Pass values between the constructor
and methods using fields.

``` cpp
#include "cpp11.hpp"
#include <later2_api.h>

using namespace cpp11;

class MyTask : public later2::BackgroundTask {
public:
  MyTask(doubles vec) :
    inputVals(as_cpp<std::vector<double>>(vec)) {
  }

protected:
  void execute() {
    double sum = 0;
    for (std::vector<double>::const_iterator it = inputVals.begin();
      it != inputVals.end();
      it++) {

      sum += *it;
    }
    result = sum / inputVals.size();
  }

  void complete() {
    Rprintf("Result is %f\n", result);
  }

private:
  std::vector<double> inputVals;
  double result;
};
```

To run the task, `new` up your subclass and call `begin()`, for example
`(new MyTask(vec))->begin()`. There's no need to keep track of the pointer; the task object will
delete itself when the task is complete.

``` r
[[cpp11::register]] void asyncMean(doubles data) {
  (new MyTask(data))->begin();
}
```

## Example with parallel jobs (Unix-like systems)

Using the base R 'parallel' package, you can fork child processes and obtain real, pollable file
descriptors. A child's `fd` becomes ready for reading once the child finishes running.

```r
# 1. timeout: prints FALSE, FALSE ----
job1 <- parallel::mcparallel({
  Sys.sleep(1)
  TRUE
})
job2 <- parallel::mcparallel({
  Sys.sleep(1)
  TRUE
})
fd1 <- job1$fd[1]
fd2 <- job2$fd[1]
later_fd(print, c(fd1, fd2), timeout = 0.1)
Sys.sleep(0.2)
run_now()

# 2. fd1 ready: prints TRUE, FALSE ----
job1 <- parallel::mcparallel(TRUE)
fd1 <- job1$fd[1]
Sys.sleep(0.1)
later_fd(print, c(fd1, fd2), timeout = 1)
Sys.sleep(0.1)
run_now()

# 3. both ready: prints TRUE, TRUE ----
job2 <- parallel::mcparallel(TRUE)
fd2 <- job2$fd[1]
Sys.sleep(0.1)
later_fd(print, c(fd1, fd2), timeout = 1)
Sys.sleep(0.1)
run_now()

# 4. fd2 ready: prints FALSE, TRUE ----
parallel::mccollect(job1)
job1 <- parallel::mcparallel({
  Sys.sleep(1)
  TRUE
})
fd1 <- job1$fd[1]
later_fd(print, c(fd1, fd2), timeout = 1)
Sys.sleep(0.1)
run_now()

# 5. fds invalid: prints NA, NA ----
parallel::mccollect(job1)
parallel::mccollect(job2)
later_fd(print, c(fd1, fd2), timeout = 0)
Sys.sleep(0.1)
run_now()
```

## Example with "promise" abstractions

I adapted some functions from the 'promises' package using base R. As 'later' points out, it is
useful to execute tasks on background threads if you cannoy get the results back in R.
See the [later2test](https://github.com/pachadotdev/later2/tree/main/later2test) for more "promise"
abstractions such as:

```r
# test that resolve works
result <- 0
later2::promise(function(resolve, reject) {
  later2test::asyncFib(resolve, reject, 3)
}) |>
  later2::then(\(x) {
    result <<- x
  })

expect_identical(result, 0)
later2::run_now(1)
while (!later2::loop_empty()) {
  later2::run_now(0.1)
}
expect_identical(result, 2)
```
