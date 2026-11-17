# PGRdup 0.3.0

## UPDATED FUNCTIONS:
* `ProbDup` - Updated to make the computation more memory efficient. 

# PGRdup 0.2.4.0

## OTHER NOTES: 
* Fixed the `checkRd` notes.

# PGRdup 0.2.3.9

## OTHER NOTES: 
* Fixed `CITATION`.

# PGRdup 0.2.3.8

## OTHER NOTES:
* Fixed non-ascii content and replaced `latin1` encoding with `utf-8`.

# PGRdup 0.2.3.7

## UPDATED FUNCTIONS:
* `ValidatePrimKey` - Fixed `xtfrm` on `data.frame`.

## VIGNETTE:
* Removed cairo dependency.

# PGRdup 0.2.3.6

## VIGNETTE:
* Reverted to using system certificates instead of RCurl ones for fetching and displaying version history as suggested by Prof. Brian Ripley (ripley@stats.ox.ac.uk).

# PGRdup 0.2.3.5

## OTHER NOTES:
* Removed reference to `RecordLinkage`package which was removed from CRAN.

# PGRdup 0.2.3.4

## UPDATED FUNCTIONS:
* `MergeKW` - Updated regular expressions to be PCRE2 compliant.
* `read.genesys` - Updated for reading of doi field.
* `DoubleMetaphone` - Fixed issue with underlying `C` code with ‘strncpy’. Changed all specified bound depending on the length of the source argument to that of destination argument.

***
# PGRdup 0.2.3.3

## OTHER NOTES:
* Use of packages in Suggests (eg. `microbenchmark`) made conditional to avoid problems when they are not available for an OS.

***

# PGRdup 0.2.3.2

## UPDATED FUNCTIONS:
* `DoubleMetaphone` - Fixed memory leak issues in underlying `C` code.
 
## OTHER NOTES:
* Minor corrections in vignette.
* Added welcome message.
* Added version history to vignette
* Replaced all `1:nrow()` and `1:length()` usage in function with `seq_len(nrow())` and `seq_len(length())` respectively.
* Added package to github.
* Added package documentation website (https://aravind-j.github.io/PGRdup/) as a github page with `pkgdown`.
* Added copyright to `Authors@R` along with original contributors for the underlying `C` code for `DoubleMetaphone`.

***

# PGRdup  0.2.3.1

## OTHER NOTES:
* Registered native routines in the `C` code for `DoubleMetaphone`.

***

# PGRdup  0.2.3

## UPDATED FUNCTIONS:
* `KWCounts` - Fixed error in case of large number of exceptions and fixed bug regarding non-exact removal of keyword exceptions.
* `ProbDup` - Changed code with a column vector specifying the columns having `with=FALSE` argument to the new preferred syntax in `data.table`.
* `ViewProbDup` - Fixed error 'formal argument "axis.ticks.y" matched by multiple actual arguments'.
* `ViewProbDup` - Fixed bug in case when all factor names in `select` argument are not present in `factor.db1` and/or `factor.db2`, the function stops. Now it gives an warning and stops only when none of the factor names in `select` are present in `factor.db*`.
 
## OTHER NOTES:
* Added `rmarkdown` to Suggests field in DESCRIPTION, as prompted by Jan Górecki.

***

# PGRdup  0.2.2.1

## OTHER NOTES:
* Fixed memory access error in src/fdouble_metaphone.c (Thanks to Prof. Brian Ripley)
 
***

# PGRdup  0.2.2

## NEW FUNCTIONS:
* `read.genesys` - Convert 'Darwin Core - Germplasm' zip archive to a flat file.
* `ViewProbDup` - Visualize the probable duplicate sets retrieved in a `ProbDup` object.

## UPDATED FUNCTIONS:
* `ReconstructProbDup` - Fixed bug regarding failure to retrieve db2 fields when method "c" is used.
* `ProbDup` - Updated code after bugfix in `stringdist` package (`stringdistmatrix`: output was transposed when `length(a)==1`).
 
## OTHER NOTES:
* Changed the contact email addresses of four authors (including maintainer) in DESCRIPTION.
* Updated the vignette and README.md with the details of new functions.

***

# PGRdup  0.2.1

## NEW FUNCTIONS:
* `SplitProbDup` - Split an object of class `ProbDup`.
* `MergeProbDup` - Merges two objects of class `ProbDup`.
* `KWCounts` - Generates keyword counts from database fields.
* `print.KWIC` - Prints summary of an object of class `KWIC` to console.
* `print.ProbDup` - Prints summary of an object of class `ProbDup` to console.

## UPDATED FUNCTIONS:
* `ProbDup` - Modified the phonetic matching for better handling of strings with digits.
* `ProbDup` - Fixed throwing of error when no duplicate sets are retrieved.
* `ProbDup` - Fixed issue regarding memory out error when large number of exceptions are there.
* `ProbDup` - Further converted code to use `data.table` package for greater efficiency and speed.
* `ProbDup` - Fixed bug regarding inconsistent matching when method "b" is used.
* `ProbDup` - Reduced the dimensions of the string matching matrices produced for greater efficiency and speed.
* `MergeKW` - Modified for better handling of regex special characters.
* `ReconstructProbDup` - Modified to ignore sets with counts less than 2 after reconstruction.

## OTHER NOTES:
* Edited README.md formatting.
* Added `diagram`, `microbenchmark` and `wordcloud` (required for vignette) to suggests field in DESCRIPTION.
* Added imports to functions from `methods`, `stats` and `utils` as `R CMD check --as-cran` now checks code usage (via `codetools`) with only the base package attached.
* Dropped the abbreviation PGR in the title in DESCRIPTION as it is mentioned in the description text.
 
## VIGNETTE:
* Added vignette "An Introduction to PGRdup package".

***

# PGRdup  0.2.0

* First release
