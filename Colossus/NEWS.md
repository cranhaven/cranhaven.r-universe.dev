# Colossus 0.9

* Added a `NEWS.md` file to track changes to the package.

# Colossus 1.0.0

* Initial submission
* C++ code modified to not apply OpenMP code if OpenMP isn't detected, to resolve MacOS installation failures

# Colossus 1.0.1

* Configuration improved to detect compiler information
* Linux configuration depends on the system wide c++ default and the compiler used to compile R
* OpenMP support is not used if the c++ default or the R compiler is clang

# Colossus 1.0.2

* utility checks updated to check that keep_constant is 0/1 values
* code will fail if keep_constant is not integer valued and 0/1, with explanation of why

# Colossus 1.0.3

* configuration script libraries moved from Suggested: to Imports:
* configuration script functions moved to non-exported functions in Colossus, circumvents note about imported libraries not being used and may be later used to provide the user with a function that informs them of why OpenMP is/isn't supported

# Colossus 1.0.4

* utility checks updated to check term numbers and subterm types in R side
* code will fail if term numbers are not integers, term numbers are missing, or subterm types are

# Colossus 1.0.5

* compilation flags changed to macros

# Colossus 1.1.0

* Default Makevars is now fully portable
* By default only windows uses OpenMP now
* The GitHub version will include instructions on activating configuration to use OpenMP on gcc Linux

# Colossus 1.1.1

* ggplot2 no longer required, now optional
* additional testing added for coverage
* default Makevars added via bash script

# Colossus 1.1.2

* Log-likelihood bound functionality added
* subject to usual convergence issues, manual search option

# Colossus 1.1.3

* Cox regression now removes rows that end before first event and start after last event
* Cox regression now sets constant rows to be constant, helps with aliasing
* Tests now use sink() to avoid printing as much excessive output to console. Tests now consolidated further.

# Colossus 1.1.4.1

* Cox plotting functions now return the tables used for plots (only last plot table returned)
* Plotting vignette updated to include more details and plots
* survival package listed as suggested for plotting vignette
