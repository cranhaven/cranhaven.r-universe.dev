# wkutils 0.1.3

* Added a `#include <cstdint>` to fix CRAN check error on gcc13 (#6).

# wkutils 0.1.2

* Vendored wk headers that are no longer being shipped with wk in
  the development version.

# wkutils 0.1.1

* Removed support for `wk::wksxp()` for future compatibility with wk.
* Fixed error caught by CRAN ASAN/UBSAN checks.
* `wkb_plot()` and `wkt_plot()` can now accept unclassed input for
  improved consistency.

# wkutils 0.1.0

* Moved utility functions from the 'wk' package to create this package.
