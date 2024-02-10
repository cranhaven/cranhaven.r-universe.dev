# rgeoprofile (Development Version)

* Development version has no changes from 0.2.2.

# rgeoprofile 0.2.2

* Added `distances()` internal function to support the `cmd_pred()` function since the `aspace` package is scheduled for archival.

# rgeoprofile 0.2.1

* Fixed typo `cgt_profile()` function description.

* Adjustments made to the `circle_center()` function based on changes to the spatstat package architecture. The function is now solely dependent on the spatstat.geom sub-package.

* Updated maintainer contact information/affiliation.

# rgeoprofile 0.2.0

* Added `circle_center()` function to compute a center of the circle centrographic geographic profile prediction.

* Added /vignettes with examples of package functionality.

* Added NEWS.md for entire package history.

* Added hex sticker for package.

# rgeoprofile 0.1.1

* Change nearest neighbor calculation from orphaned RANN.L1 package to RANN package

# rgeoprofile 0.1.0

* Initial CRAN Release Version. 

FUNCTIONS IN INITAL VERSION

* `cgt_profile()`

* `cmd_pred()`

* `geom_mean_pred()`

* `harm_mean_pred()`

* `linear_profile()`

* `lognorm_profile()`

* `neg_exp_profile()`

* `norm_profile()`

* `trun_neg_exp_profile()`
