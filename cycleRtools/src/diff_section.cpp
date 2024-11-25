#include <Rcpp.h>
using namespace Rcpp;
//' Section data according to breaks.
//'
//' Generates a vector of "section" values/levels according to differences in
//' the supplied vector. The function simply rolls over \code{x}, incrementing
//' the return vector every time there is a significant break (\code{stop}
//' argument) in the pattern of differences between adjacent elements of
//' \code{x}. In practical terms, if \code{x} is a series of timestamp values
//' (see example), every time there is a significant break in the timer (e.g.
//' >10 sec), the return vector is incremented by 1.
//'
//' @param x a numeric vector (e.g. a timer column) that increments uniformly.
//'   When there is a \strong{significant} break in this uniformity, a new
//'   section is created, and so forth.
//' @param br criterion for a significant break in terms of \code{x}.
//'
//' @return a vector of the same length as \code{x}.
//'
//' @examples
//' t_sec <- c(1:10, 40:60, 100:150)       # Discontinuous timer values.
//' pwr   <- runif(length(t_sec), 0, 400)  # Some power values.
//' x     <- data.frame(t_sec, pwr)
//'
//' ## Generate section levels.
//' x$section <- diff_section(x$t_sec, br = 10) # 10 second breaks.
//' print(x)
//' split(x, x$section)
//'
//' ## Using "intervaldata", which has a large stop.
//' data(intervaldata)
//' intervaldata$section <- diff_section(intervaldata$timer.s, br = 20)
//' sp <- split(intervaldata, intervaldata$section)
//'
//' ## Plot.
//' eplot <- function(x) cycleRtools:::elev_plot(x, "timer.min")
//' layout(matrix(c(1, 2, 1, 3), 2, 2))
//' eplot(cycleRtools:::expand_stops(intervaldata))
//' eplot(sp[[1]])
//' eplot(sp[[2]])
//'
//' @export
// [[Rcpp::export]]
std::vector<double> diff_section(NumericVector x, int br) {
  double n = x.size();
  std::vector<double> delta(n);
  for (double i = 1; i < n; ++i)
    delta[i] = x[i] - x[i - 1];
  std::vector<double> section(n, 1); // Fill with 1s.
  for (double i = 2; i < n; ++i)
  {
    section[i] = section[i - 1];
    if (section[i - 1] != section[i - 2]) // Was there just a break?
      continue;
    else if ((delta[i - 1] != delta[i]) && (delta[i] > br)) // Was this a sig. break?
      section[i] = section[i - 1] + 1;
  }
  return section;
}

// A more efficient version of base::diff.
// [[Rcpp::export]]
std::vector<double> Diff(NumericVector x) {
  double n = (x.size() - 1);
  std::vector<double> out(n);
  for (double i = 0; i < n; ++i) out[i] = x[i + 1] - x[i];
  return out;
}
