# spaero 0.6.0

- Remove support for pomp version 1. This change is the simplest way to 
  avoid problems with CRAN checks.

# spaero 0.5.0

- Add support for pomp version 2.

# spaero 0.4.0

- Change name of "beta" in the parameters of the simulators to
  "beta_par". This change allows the simulation code to be simplified,
  which both resolves an error caused by pomp version 1.19 and may
  prevent future errors as pomp continues to develop.

# spaero 0.3.0

- Add vaccination reaction to simulator. A vaccination rate of zero
  remains the default parameter setting.

- Add backward-looking window option for get_stats.

- Avoid errors when input time series is constant and return a missing
  value instead.

# spaero 0.2.0

- Add transmission argument to create_simulator to allow for
  frequency-dependent transmission. Density-dependent transmission
  remains the default model.

- Add vector of first difference of the variance vector produced by
  get\_stats. This change makes it easier to use the convexity of the
  variance time series as an early warning signal. The name of the
  vector in the stats list is variance\_first\_diff. Note that this
  change makes the abbreviation stats\$var ambiguous. Code using that
  abbreviation to obtain the vector of variance estimates should
  substitute in stats\$variance.

- To the output of get_stats(), add list taus containing Kendall's
  correlation coefficient of the elements of each time series in the
  stats list in the output with time.

- Ensure variance and kurtosis estimates are non-negative. When using
  local linear for estimating statistics, it was possible in previous
  versions for negative values to occur.

# spaero 0.1.1

- Correct autocorrelation calculation. The previous version divided
  the autocovariance by the variance at the most recent time
  point. The current version divides by the geometric mean of the
  variance at each of the two time points, matching standard
  practice. The formula in the vignette for the autocorrelation has
  been changed accordingly.

- Clean up sloppy usage of the term statistic in the documentation.

# spaero 0.1.0

- Initial release
