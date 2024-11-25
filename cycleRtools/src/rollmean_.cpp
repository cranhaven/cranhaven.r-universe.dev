#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
std::vector<double> ema_weights(double len)
{
  double alpha = 2 / (len + 1), sum = 0;
  std::vector<double> s(len), out(len);
  for (int i = 1; i < (len + 1); ++i) s[i - 1] = i; // 1:len
  for (int i = 0; i < s.size(); ++i) sum += alpha * pow(1 - alpha, 1 - s[i]);
  for (int i = 0; i < len; ++i) out[i] = alpha * pow(1 - alpha, 1 - s[i]) / sum;
  return out;
}
// [[Rcpp::export]]
std::vector<double> mean_weights(double len)
{
  std::vector<double> out(len);
  for (int i = 0; i < len; ++i) out[i] = 1 / len;
  return out;
}

//' @rdname rollmean_smth
//' @export
// [[Rcpp::export]]
std::vector<double> rollmean_(NumericVector x, double window, bool ema, bool narm)
{

  double start = (window - 1), n = x.size();
  std::vector<double> out(n);
  /****************************************/
  if (narm && ema)
  {
    double len, weight_count;
    for (double i = start; i < n; ++i)
    {
      len = 0; // Reset.
      for (double c = (i - start); c <= i; ++c)
      { // Get length of this window.
        if (NumericVector::is_na(x[c])) continue;
        else ++len; // na.rm
      }
      // Create weights for this window.
      std::vector<double> weights = ema_weights(len);
      weight_count = 0; // Reset.
      for (double c = (i - start); c <= i; ++c)
      {
        if (NumericVector::is_na(x[c])) continue;
        else out[i] += (x[c] * weights[weight_count++]);
      }
    }
  }
  /****************************************/
  else if (narm && !ema)
  {
    double len, sum;
    for (double i = start; i < n; ++i)
    {
      len = 0; sum = 0; // Reset.
      for (double c = (i - start); c <= i; ++c)
      {
        if (NumericVector::is_na(x[c])) continue;
        else {sum += x[c]; ++len;}
      }
      out[i] = sum / len;
    }
  }
  /****************************************/
  else if (!narm && ema)
  {
    double weight_count;
    std::vector<double> weights = ema_weights(window);
    for (double i = start; i < n; ++i)
    {
      weight_count = 0; // Reset.
      for (double c = (i - start); c <= i; ++c)
        out[i] += (x[c] * weights[weight_count++]);
    }
  }
  /****************************************/
  else
  {
    double sum;
    for (double i = start; i < n; ++i)
    {
      sum = 0; // Reset.
      for (double c = (i - start); c <= i; ++c) sum += x[c];
      out[i] = sum / window;
    }
  }
  /****************************************/
  return out;
}

/*** R
x <- xna <- rnorm(1000, 500, 200)
xna[sample(seq_along(x), 1000)] <- NA
microbenchmark::microbenchmark(
  rollmean_(x, 50, ema = FALSE, narm = FALSE),
  rollmean_(x, 50, ema = TRUE,  narm = FALSE),
  rollmean_(x, 50, ema = FALSE, narm = TRUE),
  rollmean_(x, 50, ema = TRUE,  narm = TRUE)
)
*/
