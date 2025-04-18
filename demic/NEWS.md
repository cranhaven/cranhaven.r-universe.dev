# demic 2.0.0

* Refactor package to improve organization. Now exports `est_ptr` which combines two other exposed functions: `est_ptr_on_all` and `est_ptr_on`. These functions allow for three different PTR estimation methods: 1) using all available samples and contigs, 2) subsetting by contigs and comparing estimates, and 3) subsetting by samples and comparing estimates. Also exports get_eptr_stats for calculating error statistics on PTR estimates.

# demic 1.0.3

* Remove debug `print` statements and no longer setting seed for runs.

# demic 1.0.2

* Remove additional data file that shouldn't have been included.

# demic 1.0.1

* Remove LICENSE file as requested by CRAN.

# demic 1.0.0

* Initial release of package, still pretty messy but thoroughly tested and working.
* Exports `est_ptr` function for PTR estimation.
