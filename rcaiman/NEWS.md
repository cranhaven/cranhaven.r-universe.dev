# rcaiman 2.0.0

Version 2.0 is a major overhaul of **rcaiman** that introduces a more consistent function naming and removes unuseful functions. Therefore, scripts written for version 1.2.x will need updating. The main goal of this release is to present a clear and cohesive interface accompanying our latest developments.

## Breaking changes

* **Function removals.**  A number of legacy functions have been removed because their functionality is now provided by other functions or is no longer needed: `write_sky_points()`, `write_sun_coord()`, `enhance_caim()`, `mask_sunlit_canopy()`, `local_fuzzy_thresholding()`, `membership_to_color()`, `obia()`, and `ootb_obia()`.  If you relied on these helpers, please check the documentation for updated workflows.

* **Renamed functions.**  To make the interface more idiomatic and self‑describing, many functions have new names. The most important renamings are:

  - `extract_sun_coord()` → **`estimate_sun_angles()`** (it now returns a numeric vector `sun_angles` instead of a list).
  - `extract_rl()` → **`extract_rr()`** (the `z_thr` argument has been removed).
  - `mask_hs()` → **`select_sky_region()`**.
  - `regional_thresholding()` → **`binarize_by_region()`**.
  - `rings_segmentation()` → **`ring_segmentation()`**.
  - `sectors_segmentation()` → **`sector_segmentation()`**.
  - `apply_thr()` → **`binarize_with_thr()`**.
  - `cie_sky_model_raster()` → **`cie_image()`**.
  - `interpolate_sky_points()` → **`interpolate_planar()`**.
  - `calc_co` → **`compute_canopy_openness()`**.
  - `normalize()` → **`normalize_minmax()`**.
  - `fit_cie_sky_model()` → **`fit_cie_model()`**; the optimisation routine has been redesigned, arguments `r`, `z`, `a`, `sky_points` and `zenith_dn` are replaced by `rr`, and the `rmse` argument has been dropped.  The output structure has also changed.
  - In `row_col_from_zenith_azimuth()` the argument `za` has been renamed to `zenith_azimuth`.
  - `ootb_sky_reconstruction()` has been split into **`ootb_sky_cie()`**, **`ootb_sky_above()`**, **`validate_cie_model()`**, and **`optim_sun_angles()`**.
  - `fit_trend_surface()` now has behaviour similar to `interpolate_planar()`.
  
  All former function names have been removed instead of deprecated. Scripts must be updated to call the new names directly. This provides a cleaner user experience with predictive keyboards.

* **Changed return values and argument orders.**  Functions such as `lens()`, `extract_feature()` and `ring_segmentation()` now return different classes or component names.  The order of arguments in `calc_co()` has been changed for consistency.  When a function takes a `sun_coord` argument, it is now named `sun_angles` and must be a numeric vector.

* **Removed thresholding methods.**  The `"Diaz2018"` method for regional thresholding has been removed.  

* **Behavioural changes.**  The default values and error handling of many functions have been adjusted. There was also changes in arguments and returns. 

## New features

* **Out-of-the-box pipeline.** `ootb_bin()`, `ootb_sky_cie()`, and `ootb_sky_above()` offer a robust and accurate processing workflow for gap fraction computation that does not require manual parameter tuning.

* **Directional processing.**  `apply_by_direction()` applies a built‑in method or user‑supplied function over subsets of pixels defined by a direction and field of view, enabling directional thresholding and sky brightness estimation.

* **Complementary gradients.**  `complementary_gradients()` computes complementary gradients from RGB imagery.

* **Morphological helpers.**  `grow_black()` fills black holes in binarised images, complementing dilation operations.

* **Sky points helpers.**  `sky_grid_centers()` returns the centre coordinates of sky grid cells, facilitating interpolation and modelling, and `rem_outliers()` and `rem_nearby_points()` implement flexible filters for selecting or discarding sky points.

* **Two‑corner thresholding.**  `thr_twocorner()` implements a geometric two‑corner thresholding strategy with robust fall‑backs for failure cases.

* **Display and I/O.**  `display_caim()` provides convenient display of canopy photographs.  `write_sky_cie()` and `read_sky_cie()` allow saving and loading the output of `ootb_sky_cie()` to file for later analysis.

* **Spherical utilities.**  `calc_spherical_distance()` computes great‑circle distances, and `interpolate_spherical()` performs interpolation on the sphere.

## Minor improvements and fixes
* Fix rotation argument of `azimuth_image()`.
* `sky_grid_segmentation()` gains `first_ring_different` to avoid small cells around the zenith.
* `extract_sky_points()` is now more robust.
* `extract_sky_points()` lose the argument `min_raster_dist` because of the new function `rem_nearby_points()`.
* `regional_thresholding()` lose the method `"Diaz2018"`. 
* `regional_thresholding()` gain method `"thr_twocorner"`. 

# rcaiman 1.2.0 

## Breaking changes
* `ootb_mblt()` was updated and results from this version will be different to
those from previous version. 

## New features
* New `calc_co()` provides canopy openness calculation for hemispherical images.
* New `correct_vignetting()` to correct the vignetting effect and doing so
standardize radiometry.
* New `crop_caim()` allows cropping after reading, complementing `read_caim()`.
* New `crosscalibrate_lens()` allows geometrical cross-calibration.
* New `extract_radiometry()` helps to built a function to correct the vignetting
effect with `correct_vignetting()`.
* New `extract_sky_points_simple()` to obtain sky points without the need of a
working binarized image.
* `fisheye_to_equidistant()` is now able to interpolate. It gains the `m`
argument. 

## Minor improvements and fixes
* `calc_zenith_colrow()` is former `calc_zenith_raster_coord()`.
* `calibrate_lens` gains a more extensive output.
* `extract_rl()` default values were changed.
* Fix `extrac_sky_points()`. Now, `dist_to_plan` works as intended.
* `find_sky_pixels_nonnull()` gains `intercept` and `w`.
* `fit_cie_sky_model()` output was simplified, it does not return a raster
anymore. It has to be calculated from the model with `cie_sky_model_raster()`.
* `interpolate_sky_points()` changes `g` for `r`. This acknowledge a change made
in the code but not reflected in the arguments or documentation. The change was
to approximate the function as much as possible to the Lang et al. (2010) method
in which this function was based.
* Fix an issue in `extract_sky_points()` due to a change on the behavior of
large data.frame naming.
* `ootb_obia()` gains `w_red`.
* `ootb_sky_reconstruction()` was updated.
* `row_col_from_zenith_azimuth()` and `zenith_azimuth_from_row_col()` now uses
different arguments because they were reprogrammed to overcome limitations in
azimuth angle computations and unintentional side effects.
* `thr_mblt()` is former `thr_image()`.

# rcaiman 1.1.1 

## New features
* `ootb_obia()` gains `gamma`

## Minor improvements and fixes
* `ootb_mblt()` now can detect when `find_sky_pixels()` fails to deliver a good
mask, and switch to find a mask by applying a global threshold calculated with
IsoData.This last method will works well since the failure of
`find_sky_pixels()` is associated with extremely open forest, in which
circumstances it will not produce comission errors. 

# rcaiman 1.0.7 

## Breaking changes
* Now *rcaiman* depends on *terra* package instead of *raster* since key
dependencies of the latter package are on the course of losing maintenance by
2023 (<https://r-spatial.org/book/sp-raster.html>). This implied changes in the
whole code. Although maximum efforts were made to maintain the behavior of
functions, it may happen that scripts running well with version 0.1.1 fails with
this new version.
* A major bug on `local_fuzzy_thresholding()` was fixed. This affected the main
function `enhance_caim()` since it internally uses `local_fuzzy_thresholding()`.
If possible, results from scripts using `local_fuzzy_thresholding()` or
`enhace_caim()` should be recalculated with this new version. 

## New features
* New HSP functions family enables a dynamic workflow between R and the HSP
software package (<doi:10.2478/fsmu-2013-0008>).
* `azimuth_image()` gains `rotation`, which allows processing images
with the top oriented to any known azimuth angle. Previously, by default it
assumed that the top was oriented to the north.
* New `chessboard()` provides chessboard segmentation.
* New `cie_sky_model_raster()` produces CIE sky images of any resolution from
custom parameterized CIE sky models. The CIE sky model implementation is based
on Pascal code by Mait Lang.
* New `colorfulness()` provides a method to quantify image colorfulness.
* New `deffuzify()` is an alternative to `apply_thr()` for turning fuzzy
classification into Boolean.
* New `extract_dn()` facilitates the extraction of digital numbers from canopy
photographs. The extraction is based on raster coordinates obtained
automatically with `extract_sky_points()` or manually with third-party software.
* New `extract_rl()` facilitates the extraction of relative luminance from
hemispherical photographs. This function uses objects from
`extract_sky_points()` and returns objects essential to `fit_cie_sky_model()`.
* New `extract_sky_points()` to automatically extract sky points from canopy
photographs.
* New `extract_sun_coord()` to automatically extract sun coordinates from canopy
photographs. Objects returned by this function are essential to
`fit_cie_sky_model()`.
* New `find_sky_pixels_nonnull_criteria()` offers a method for fine-tuning
working binarized images, which are the input of many functions, such as
`extract_sky_points()` and `extract_sun_coord()`. The method is based on the
assumption that the threshold can be tuned as long as no new cells with zero
gaps are obtained (the so-called null cells).
* New `fisheye_to_pano()` provides a method to reproject from hemispherical to
cylindrical.
* New `fit_cie_sky_model()` uses maximum likelihood to estimate the coefficients
of the CIE sky model that best fit to data sampled from a canopy photograph.
Then, those coefficients can be used to produce and image with
`cie_sky_model_raster()`.
* `fit_coneshaped_model()` now works with the point-like data objects returned
by `extract_rl()` and returns a function that can easily produce a raster when
*SpatRaster* objects are provided as arguments. As a result, the function is
faster and more versatile.
  ```R
r <- read_caim()
z <- zenith_image(ncol(r), lens("Nikon_FCE9"))
a <- azimuth_image(z)
g <- sky_grid_segmentation(z, a, 10)
bin <- find_sky_pixels(r, z, a)
sky_points <- extract_sky_points(r, bin, g)
sky_points <- extract_rl(r, z, a, sky_points, NULL)
model <- fit_coneshaped_model(sky_points$sky_points)
model$fun(60, 10)
model$fun(z, a)
  ```
* New `interpolate_sky_points()` provides a method to produce raster images from
point-like data, such as the objects returned by `extract_dn()` or
`extract_rl()`.
* New `Mask_sunlit_canopy()` is a wrapper function around
`membership_to_color()` that facilitates masking sunlit canopy.
* New `obia()` is a revised version of the object-based image analysis presented
in <doi:10.1109/lgrs.2015.2425931>.
* New `ootb_obia()` is a revised version of the full workflow presented in
<doi:10.1109/lgrs.2015.2425931>, which includes `enhance_caim()` and `obia()`.
* New `ootb_sky_reconstruction()` provides an easy to use function that will
build an above canopy image from a single below canopy image, by means of
`fit_cie_sky_model()` and `interpolate_sky_points()`.
* New `polar_qtree()` provides quad-tree segmentation in the polar space.
* New `qtree()` provides classical quad-tree segmentation
* New `thr_isodata()` is an alternative implementation of the IsoData method
from the autothresholdr package. 

## Minor improvements and fixes
* Examples of restricted view photography were added, broadening the scope of
the package since most new users search for code templates on the examples in
order to get started.
* Now `apply_thr()` turns NA values from `r` to 0. This allows to quickly
produce binarized images without NA values.
* `calc_zenith_raster_coord()` is former `calc_zenith_raster_coordinates()`.
* `enhance_caim()` gains `thr` and `fuzziness`, and default values for
all arguments except `caim`. This makes the functions easier for new users and
more flexible for advanced users.
* `expand_noncircular()` now produces the expected output when `zenith_colrow`
is far from the image center, not only when it is close to it.
* `extract_feature()` gains `ignore_label_0` since it cannot handle NA values as
expected after changing dependency from *raster* to *terra*.
* `find_sky_pixels()` now uses sample size percentage.
* `fisheye_to_equidistant()` is former `reproject_to_equidistant()`. Now the
resolution of the output is 1, and the extension is derived from `radius`.
Previously, and incorrectly, the resolution was different from 1 since while the
dimension was computed from `radius`, as it should, the extension was taken from
`z`.
* `fix_reconstructed_sky()` is former `fix_predicted_sky()`.
* `normalize()` gains `force_range` and defaults values for `mn` and `mx`.
* `ootb_mblt()` now uses `find_sky_pixels_nonnull_criteria()` and gains two
arguments, `bin` and `fix_sky_cs`, which allows quick customization.
* `read_caim()` now is allowed to read any raster image that `terra::raster()`
can read. Of course, the georeferencing is turned off by assigning a local
projection and manipulating extension and resolution, as usual.
  ```R
f <- system.file("ex/elev.tif", package="terra")
read_caim(f)
terra::rast(f)
  ```
* In `regional_thresholding()`, `method` gains the thr_isodata method.
* As a consequence of changing dependency from *raster* to *terra*,
`rings_segmentation()`, `sectors_segmentation()`, and `sky_grid_segmentation()`
return 0 outside the circular image instead of NA.
