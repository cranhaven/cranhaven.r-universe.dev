# moexer 0.3.0

* Implement fetching data by automatically incrementing `start` parameter
  (as `fetching_fully()` decorator).

* Refactor the old way of following the cursor to fetch the entire response data
  --- now a decorator (`following_cursor()`)
  
* Add "Low-level Interface to ISS" section to README.Rmd

* Allow specifying date-times for `from`, `till` arguments of `get_candles()`.


# moexer 0.2.0

* `moexer` is now able to download data for arbitrarily long time periods by
  following the `.cursor` sections in the response
  
* Minor code refactoring and bug fixes

# moexer 0.1.0

Initial release.
