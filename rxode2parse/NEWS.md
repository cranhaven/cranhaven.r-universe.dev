# rxode2parse 2.0.19

* Added a evid suffix of 60 for cases where evid=2 adds an on event
  (fixes tad() calculation in certain edge cases)

* Initialize all variables to `NA`

# rxode2parse 2.0.18

* Removed linear compartment solutions with gradients from rxode2parse
  (and rxode2) when compiled with intel c++ compiler (since it crashes
  while compiling).

* Fixed `m1mac` string issues as requested by CRAN

# rxode2parse 2.0.17

* Added ability to query R user functions in a rxode2 model (will
  force single threaded solve)

* Moved core `rxFunParse` and `rxRmFunParse` here so that C and R user
  function clashes can be handled

* Model variables now tracks which compartments have a lag-time
  defined

* For compartment with steady state doses (NONMEM equivalent SS=1,
  SS=2), an additional tracking time-point is added at to track the
  time when the lagged dose is given.  As an upshot, the lagged dose
  will start at the steady state concentration shifted by + ii - lag
  in `rxode2` (currently for ode systems only)

* This release calculates non bio-availability adjusted duration for
  all rates instead of trying to figure the rate duration during
  solving.

* Make double assignment an error, ie  `a <- b <-`

* `NA` times are ignored (with warning)

* Steady state bolus doses with `addl` are treated as non steady state
  events (like what is observed in `NONMEM`)

* Timsort was upgraded; drop radix support in rxode2 structure

* `etTrans` now supports keeping logical vectors (with the appropriate
  version of `rxode2`).

* Security fixes were applied as requested by CRAN

# rxode2parse 2.0.16

* Import `data.table` explicitly in the R code (before was imported only in C/C++ code)

# rxode2parse 2.0.15

* Updates the make flags to support CXX17.

# rxode2parse 2.0.14

* 'linCmt()' translations of 'alpha', 'beta', 'gamma', 'k21', 'k31',
  'vc' now error instead of ignoring 'gamma' and 'k31' to give 2 cmt
  solution

* transit compartment internal code now changes dose to 0.0 when no
  dose has been administered to the depot compartment. This way dosing
  to the central compartment (without dosing to the transit
  compartment) will not give a `NA` for the depot compartment (and
  consequently for the central compartment)

* Moved `rxDerived` here and added tests for it here as well.

* Moved `etTransParse` here and added tests for it here as well (makes
  up most of `etTrans`). In addition the following changes were made
  to `etTransParse()`/`etTrans()`:

  * The internal translation (`etTrans()`) will not drop times when
    infusions stop. Before, if the infusion stopped after the last
    observation the time when the infusion stopped would be dropped.
    This interferes with `linCmt()` models.

  * Breaking change/bug fix `evid=2` are considered observations when
    translating data to internal `rxode2` event structure

  * Fix edge case to find infusion duration when it is the first item
    of the dosing record at time 0.

 * Fixed a bug for certain infusions where the `rate`, `ii` and/or
   `ss` data items were dropped from the output when `addDosing=TRUE`


* Also have internal functions to convert between classic NONMEM
  events and rxode2 events

* Have an internal function that gives information on the linear
  compartmental model translation type, which could be useful for
  babelmixr2

* 'time' in model is now case insensitive

* Use function declaration in `rxode2parseGetTranslation()` to
  determine thread safety of functions available to rxode2

* Add check for correct number of function arguments to parser.

* Like R, known functions can be assigned as a variable and the
  function can still be called (while not changing the variable
  value).  For example you can have a variable `gamma` as well as a
  function `gamma()`.

* Fix garbled error messages that occur with certain messages.

* Fixed errors that occurred when using capitalized AMT variables in
  the model.

# rxode2parse 2.0.13

* Version bump for dparser (so binaries will be built correctly)

# rxode2parse 2.0.12

* Bug fix for strict prototypes

* Removed `sprintf` as noted by CRAN

* Made `rxode2parse` dll binary independent of `rxode2()`

# rxode2parse 2.0.11

* Bug fix for strict aliasing as requested by CRAN

# rxode2parse 2.0.10

* Use strict aliasing as requested by CRAN

# rxode2parse 2.0.9

* Initial release to split of rxode2parse from rxode2 to reduce
  compilation time of 'rxode2'

* Added a `NEWS.md` file to track changes to the package.
