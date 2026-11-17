# mmequiv 1.0.0

* `calculate_mme.data.frame()` and `calculate_mme.tbl_df()` now allow users
    to specify `use_api` and includes informative warnings about the API 
    rate limit

* Deprecated `calculate_mme_df()` and refactored `calculate_mme()` to 
    use S3 methods for `list`, `data.frame`, and `tbl_df` objects (#4)

* Deprecated `calculate_mme_local()` and consolidated local calculation 
    functionality under `calculate_mme()` with new `use_api` argument (#3)

* Fixed `calculate_mme_df()` bug in error catching logic and removed hard 
    dependency on `tibble` package

* Updated `DESCRIPTION` in response to CRAN reviewer suggestion for next 
    submission

# mmequiv 0.1.1

* Updates in response to CRAN review.

# mmequiv 0.1.0

* Initial CRAN submission.
