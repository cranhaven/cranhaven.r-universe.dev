# dbcsp 0.0.2.2

## Bug fixes

* Fixed issue with Rd cross-references; now properly using package anchors to ensure links work correctly.

---

# dbcsp 0.0.2.1

## Bug fixes

* Warning message changed. If the minimum eigenvalue is below the tolerance indicated by `eig.tol` when creating the `dbcsp` object, average covariance matrices are replaced by the most similar matrix that is positive definite and a warning message is printed to make the user aware of it.

---

# dbcsp 0.0.2.0

## New features

* Missing values handling is available. The missing values along a signal are imputed by a linear interpolation.

* User-defined custom distances are allowed. A function $d(x_1,x_2)$ which returns a scalar providing the distance value between $x_1, x_2$ can be used as distance function when creating the `dbcsp` object. The name of the function has to be passed to `type` parameter. 

* New parameter `getsignals` in `plot()`. If `getsignals=TRUE` the projected signals for a given instance within a given class are returned, that is, the plotted projected signals are returned.

## Bug fixes

* Optional arguments of `matplotlib` function can be passed to `plot()` function.

* In `plot()` the original signals are shown before standardizing.

* The `dbcsp::train` function was being masked by `caret::train` function. This has been fixed.

---

# dbcsp 0.0.1.0

First public release.
