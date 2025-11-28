## rtsplot 0.1.4

* Added 'rtsplot.polygon' function to plot polygon time series.

## rtsplot 0.1.3

* Fix donttest examples for CRAN

## rtsplot 0.1.2

* Added 'skip.breaks' parameter to the 'rtsplot' function to skip plotting of 
  missing date/times. i.e. weekends, holidays, and nights(non-trading hours).

## rtsplot 0.1.1

* Exposed internal functions to create candle, ohlc, hl plots.
* Exposed internal functions to access candle and volume colors.
* Fixed internal logic to access xts index. Use `.index` instead of `attr` to access xts index.

## rtsplot 0.1.0

Initial release.
