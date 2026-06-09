# lidaRtRee 4.0.8

## Change

* number of threads used by package `lidR` set to 2 in examples.
* projection information (epsg 2154) added to external data `las_chablais3.las`.

# lidaRtRee 4.0.7

## New

* `clouds_tree_metrics()` now has a `roi` argument to select trees to retain for subsequent computation based on spatial object.
* `ABA_predict()` now has a `...` argument to pass additional arguments to `stats::predict.lm()`.

## Change

* `boxcox_tr()` and `boxcox_itr()` do not automatically replace `0` with `NA` anymore.
* `RasterLayer` objects are not supported anymore as the `raster` package is now obsolete.
* in `aba_build_model()`, when specified the minimum `threshold` parameter now replaces non finite values in predictions. 
* added `...` argument in `aba_build_model()`: passed to `leaps::regsubsets()`

# lidaRtRee 4.0.6

## New

* `raster_metrics()` now has a `start` argument to align on predefined grid.

# lidaRtRee 4.0.5

## Change

* `max.width` parameter in function `maxima_detection()` is now expected to be in raster units if a `SpatRaster` is provided; default value is now `11`.
* Function `tree_segmentation()` now expects arguments passed to sub-functions as `...` rather than named arguments; default values for those sub-functions were modified to match with the default values which were used in `tree_segmentation()` ; a warning is issued in case `max.width` argument (passed to `maxima_detection()`) is not consistent with the `dmin` or `dprop` arguments (passed to `maxima_selection()`).

# lidaRtRee 4.0.3

## Fix

* fixes the number of decimals for WKT coordinates of crown in function `tree_extraction()`.

# lidaRtRee 4.0.2

## New

* adds a global function `tree_detection()` which includes both the segmentation and extraction, and can be applied to SpatRaster, LAS or LAS-catalog objects.
* `gap_detection()` now also accepts LAS and LAS-catalog objects as input.

# lidaRtRee 4.0.1

## Change

* now relies on [terra](https://cran.r-project.org/package=terra) and [sf](https://cran.r-project.org/package=sf) packages instead of [raster](https://cran.r-project.org/package=raster) and [sp](https://cran.r-project.org/package=sp).

# lidaRtRee 3.1.0

## Breaking change

* introduces names modifications for better constituency.