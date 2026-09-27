# shinyreprex 0.3.0

New features:

* `reprex_lockfile` captures the package versions, sources and R version needed to reproduce 
  one or more reactives as an `renv` lockfile, with `reprex_packages` exposing the detected 
  packages so they can be offered for selection. Packages are detected from every branch
  of an `if` or `switch`, so the set may be a superset of the `library()` calls in the
  reproduced script
* `register_reactives` records reactives against the Shiny session, so `reprex_lockfile` 
  and `reprex_packages` can be called without arguments and still cover reactives owned by 
  separate modules

Bug fixes:

* Correctly reproduce unbraced `if`/`else` branches. Previously a branch that was a
  single expression rather than a `{` block had its function call stripped, producing
  a script containing only the call's arguments, and any package used solely in that
  branch went undetected
* Return an empty script for an `if` without an `else` whose condition is not met,
  rather than erroring with `subscript out of bounds`
* Select the correct branch when an `if` condition evaluates to a truthy value other
  than `TRUE`, such as a non-zero number. Previously the condition itself was
  reproduced in place of the `if` branch
* Functions with 0 arguments are now correctly treated as functions rather than 
  reactive objects

Other updates:

* Added `cli` as a dependency, used for error and warning messages
* Added `renv` as a dependency, used for producing the lockfile for packages used by reactives

# shinyreprex 0.2.0

New features:

* Handling special cases of additional base functions `switch` and `[[`

Other updates:

* Update to the logo
* Add shinylive example to GitHub Pages

# shinyreprex 0.1.0

* This is the first release of shinyreprex
