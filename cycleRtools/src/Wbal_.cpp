#include <Rcpp.h>
using namespace Rcpp;
//' @rdname Wbal
//' @export
// [[Rcpp::export]]
std::vector<double> Wbal_(NumericVector t, NumericVector P, double CP)
{                         /* NB: 0 = FALSE. */
  if (t.size() != P.size()) stop("Inputs are different lengths.");

  double n = P.size();
  // Vector of supra- and sub-CP sections.
  std::vector<double> section(n);
  for (double i = 0; i < n; ++i)  // NA = 2.
    section[i] = (NumericVector::is_na(P[i])) ? 2 : (P[i] <= CP) ? 0 : 1;

  // Generate vectors required for W' expenditure calculation.
  std::vector<double> dt(n), dWexp(n), tu(n), DCP(n), tau(n), rec(n);

  for (double i = 1; i < n; ++i) // NB: Starts at the **second** element.
  {
    if (NumericVector::is_na(t[i])) stop("NAs not allowed in time values.");

    dt[i] = t[i] - t[i - 1];

    if (NumericVector::is_na(P[i])) // NA power value.
    {
      dWexp[i] = dWexp[i - 1];
    }
    else if (section[i])            // Supra-CP section.
    {
      // In case there is a long pause (> 10 s), and data starts
      // recording with non-zero power, use next delta time value.
      if (dt[i] > 10) dWexp[i] = dt[i + 1] * (P[i] - CP);
      else dWexp[i] = dt[i] * (P[i] - CP);
      // Delta W' expended is cumulative; check previous value.
      if (section[i - 1]) dWexp[i] += dWexp[i - 1];
    }
    else // Sub-CP (recovery) section.
    {
      tu[i] = dt[i];
      if (tu[i - 1] != 0) tu[i] += tu[i - 1];  // Cumulative.
      DCP[i] = CP - P[i];
      tau[i] = 546 * std::exp(-0.01 * DCP[i]) + 316;
      rec[i] = std::exp(-tu[i] / tau[i]);
    }
  }

  // Generate W' expenditure from the above.
  std::vector<double> Wexp(n);
  double Wtmp = 0;
  for (double i = 0; i < n; ++i)
  {
    if (section[i])  // Supra-CP section.
    {
      Wexp[i] = dWexp[i] + Wtmp;
      if (dWexp[i + 1] == 0) Wtmp = Wexp[i];
    }
    else
    {
      if (Wtmp != 0)
      {
        Wexp[i] = Wtmp * rec[i];
        if (dWexp[i + 1] != 0) Wtmp = Wexp[i];
      }
    }
  }
  return Wexp;
  /* For debugging: create matrix from parameters.
   NumericMatrix testmx(n, 10);
   for (double i = 0; i < n; ++i) {
   //dWexp(n), tu(n), DCP(n), tau(n), rec(n);
   testmx(i,0) = t[i];testmx(i,1) = P[i];testmx(i,2) = section[i];
   testmx(i,3) = dt[i];testmx(i,4) = dWexp[i];testmx(i,5) = tu[i];
   testmx(i,6) = DCP[i];testmx(i,7) = tau[i];testmx(i,8) = rec[i];
   testmx(i,9) = Wexp[i];
   colnames(testmx) = CharacterVector::create(
   "t", "P", "section", "dt", "dWexp", "tu", "DCP", "tau", "rec", "Wexp");
   }
   return testmx;
   */
}
