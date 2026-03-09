#include <RcppClock.h>

// a simple timing example
int fib(int n) {
  return ((n <= 1) ? n : fib(n - 1) + fib(n - 2));
}

//' Simple RcppClock example
//' 
//' Time the computation of fibonacci numbers
//' 
//' @details
//' The function being timed is the following:
//'
//' \code{int fib(int n) { return ((n <= 1) ? n : fib(n - 1) + fib(n - 2)); }}
//' 
//' Runtime for computations less than \code{n = 25} is nearly unmeasurable.
//' 
//' @param n vector giving integers for which to compute the fibonacci sum
//' @param reps number of replicates for timing 
//' @export
//' @examples
//' fibonacci(n = c(25:35), reps = 10)
//' # this function creates a global environment variable "clock"
//' #   that is an S3 RcppClock object
//' clock
//' plot(clock)
//' summary(clock, units = "ms")
//[[Rcpp::export]]
void fibonacci(std::vector<int> n, int reps = 10) {
  Rcpp::Clock clock;

  for(int i = 0; i < reps; ++i){
    for(auto number : n){
      clock.tick("fib" + std::to_string(number));
      fib(number);
      clock.tock("fib" + std::to_string(number));
    }
  }
  clock.stop("clock");
}
