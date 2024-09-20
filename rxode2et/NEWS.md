# rxode2et 2.0.13

* Fix import of data where there are NA times

# rxode2et 2.0.12

* Fix formatting issues identified by m1mac, as requested by CRAN

# rxode2et 2.0.11

* Make the stacking more flexible to help rxode2 have more types of plots

* Add `toTrialDuration` by Omar Elashkar to convert event data to trial duration data

* Fix Issue #23 and prefer variable values over NSE values

# rxode2et 2.0.10

* Fix dollar sign accessing of objects (like data frames), as pointed
  out by @frbrz (issue #16)

* Use `rxode2parse` functions for internal event table creation (where
  they were moved to).

* Dropped C++14 and let the system decide.

# rxode2et 2.0.9

* Split off `et()`, `eventTable()` and related functions.

* Also split off `rxStack()` and `rxCbindStudyIndividual()` in this
  package.

* Added a `NEWS.md` file to track changes to the package.
