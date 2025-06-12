# ramlegacy 0.1.0

* Initial Release

# ramlegacy 0.2.0

* Removed the loading behavior of `library(ramlegacy)` leaving the loading solely to `load_ramlegacy`
* Instead of assigning all the tables to global environment `load_ramlegacy` now just returns a list of tables that can be specified through `tables` argument.
* Modified `download_ramlegacy` and `load_ramlegacy` so that that while the default is still to download and read in the dataframes from the rappdirs directory the functions now also support downloading to a location chosen by the user and reading from that location. 

## For more fine-grained list of changes or to report a bug, consult 

* [The issues log](https://github.com/ropensci/ramlegacy/issues)
* [The commit log](https://github.com/ropensci/ramlegacy/commits/master)

## Versioning Guidelines

Releases will be numbered with the following semantic versioning format:

`<major>.<minor>.<patch>`

And constructed with the following guidelines:

* Breaking backward compatibility bumps the major (and resets the minor 
  and patch)
* New additions without breaking backward compatibility bumps the minor 
  (and resets the patch)
* Bug fixes and misc changes bumps the patch
* Following the RStudio convention, a .99 is appended after the patch
  number to indicate the development version on Github.  Any version
  Coming from Github will now use the .99 extension, which will never
  appear in a version number for the package on CRAN. 
