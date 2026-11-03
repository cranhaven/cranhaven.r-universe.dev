# qrfactor 1.6

## New features

* `qrfactor()` gains a `rotation` argument (default `FALSE`). Set
  `rotation = TRUE` (or `"varimax"`) to varimax-rotate the retained
  factors to simple structure; the default keeps the classical
  unrotated solution, so existing results are unchanged. When rotation
  is requested the same orthogonal rotation is applied to the R-mode and
  Q-mode loadings, so both modes remain on one common set of axes and
  the scores, combined loadings, appended index columns and all
  biplots/maps rotate consistently. The eigenvectors, eigenvalues and
  PCA loadings are left unrotated. The fitted object now also carries
  `rotation`, `unrotated.r.loading` and `unrotated.q.loading`.

## CRAN policy compliance

* The `plot()` method no longer leaves the user's graphics state
  changed: `par()` is captured on entry and restored with `on.exit()`.

* The analysis views (`type = "cluster"`, `"anova"`, `"region"`,
  `"admin"`, `"diagnose"`) no longer write statistical tables to the
  console unconditionally. They are silent by default and print only
  when the new `plot(..., verbose = TRUE)` argument is set; the
  `print()`/`summary()` methods are unaffected.

* Quoted software and package names (`'qrfactor()'`, `'plot()'`,
  `'ESRI'`) in the `DESCRIPTION`, and rewrote the `?qrfactor` examples so
  the fast data-frame workflow runs unwrapped and only the shapefile /
  map examples (which need `sf`/`sp`) are under `\donttest{}`. The stale
  `readOGR()` example call was removed.

# qrfactor 1.5

Restoration release. `qrfactor` was archived from CRAN in 2018; this
version repairs it for current R and CRAN.

## Bug fixes

* Registered the S3 methods (`qrfactor.default`, `print.qrfactor`,
  `summary.qrfactor`, `plot.qrfactor`) in `NAMESPACE`. The archived
  version exported them with `exportPattern(".")` but never registered
  them, so `qrfactor()` failed with
  `no applicable method for 'qrfactor' applied to an object of class
  "data.frame"`. This was the cause of the CRAN archival.

* Fixed a crash under R >= 4.2 in the map plots, where a condition of
  length greater than one is now an error rather than a silent
  first-element take.

## Dependency changes

* Replaced retired spatial packages. Shapefiles are now read with
  `sf::st_read()` instead of `rgdal::readOGR()`, and the `maptools` and
  `mgraph` dependencies were removed. A small compatibility layer bridges
  the `sf`/`sp` object differences.

* `Depends:` on contributed packages moved to `Imports:`; base-R
  functions are now explicitly imported via `importFrom()`.

## Documentation

* Rewrote the `DESCRIPTION` (complete-sentence Description, modern
  `Authors@R`).
