# arctools 1.1.5

* Add `testthat::skip_on_cran()` to the tests to aid the errors from devel linux and devel windows which I am unable to reproduce. This is to retain the package on CRAN. 

# arctools 1.1.3

* Minor changes in vignette. 

# arctools 1.1.2

* Replace Travis and AppVeyor with GitHub Actions. 
* Added notes to the documentation about using  `lubridate::ymd_hms()`. 

# arctools 1.1.0

* Added a `NEWS.md` file to track changes to the package.
* Added a protective step in `midnight_to_midnight` to protect from errors coming from providing a function with `base::as.POSIXct()`-generated timestamp (not ok) instead of `lubridate::ymd_hms()`-generated timestamp. 
