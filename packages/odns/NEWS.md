# odns 1.0.2

## POTENTIALLY BREAKING CHANGES
* `resource_metadata()` now returns the actual metadata of a resource rather
than a data.frame detailing the names and types of all fields available for the
chosen resource. A new function provides the old behavior, 
`resource_data_items()`.

## Minor Changes
* Users running an R version < 4.0.0 will no longer be impacted by the  
`stringsAsFactors == TRUE` default. This change aligns behavior of the package 
across compatible versions of R.

# odns 1.0.1

## New
* Added function `nrow_resource()` returns the number of rows a resource 
contains.

## POTENTIALLY BREAKING CHANGES
* `all_packages()` now makes better use of the underlying CKAN API and returns
the package id in addition to the name. However, the function now returns a 
data.frame rather than a character vector. A default limit to the return of 1000
rows is also implemented, but can be set to a custom value by the user.

## Minor Changes
* `all_resources()` now makes better use of the underlying CKAN API and is
significantly faster. The function also now provides a warning and returns an
empty data.frame if the arguments specified result in no results being returned,
replacing the previous behavior which produced an error.

* `get_resource()` now allows accepts character vectors as arguments, allowing
for the download of resources from multiple packages. It also now allows the
user to specify resource names as well as ids. Multiple performance enhancements
too.

* `get_data()` now supports pagination. The `page_size` argument allows users to
specify the maximum number of records to be returned per query. The default 
behavior is to attempt to return all rows with a single query. SQL queries are
now used less often, improving performance.

Returns from `get_data()` are now by default ordered as per the database primary
key.

# odns 1.0.0

* Added a `NEWS.md` file to track changes to the package.
