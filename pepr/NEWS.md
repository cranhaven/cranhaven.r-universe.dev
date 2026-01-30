# pepr 0.5.0 - 2023-11-16

## Fixed

* result class of appended attributes; it's no longer wrapped in a `list`
* various other fixes and improvements

## Changed 

* error message when nonexistent attribute is derived

## Added

* possibility to configure the sample and subsample table index attributes in the `Project` object constructor
* possibility to initialize `Project` with no configuration file
* ability to initialize a project from URL
* auto initialization file detection: CSV (sample table) or YAML (project config)
* duplicated sample auto-merging

# pepr 0.4.0 - 2020-10-14

## Changed

* added multiple subsample_table support

## Fixed

* issues with config file determination

# pepr 0.3.2 - 2020-06-03

## Changed

* package description

# pepr 0.3.1 - 2020-05-27

## Fixed

* issues with auto path expansion in objects of `Config` class on Windows
* issues with path absoluteness determination on Windows

# pepr 0.3.0 - 2020-05-26

**This version introduced backwards-incompatible changes.**

## Added

* auto path expansion in `Config` class
* attribute duplication functionality
* config importing functionality
* attribute removal functionality
* possibility to define multi-attribute rules in attribute implication


## Changed

* **project configuration file to follow [PEP2.0.0 specification](http://pep.databio.org/en/2.0.0/specification/).** Browse the specification for changes related to config format


# pepr 0.2.2 - 2020-01-09

## Changed

*  downgrade exception from error to warining when missing envieonment variable is found

## Fixed 

* issues related to subsample table functionality; [#33](https://github.com/pepkit/pepr/issues/33)

# pepr 0.2.1 - 2019-06-21

## Changed

* `checkSection` method can be used with a mixture of section names _and_ indices.
* improved interaction between pepr and BiocProject

# pepr 0.2 - 2019-04-17

## Changed

* keys in the config file: `sample_annotation` to `sample_table`, `sample_subannotation` to `subsample_table`. Backwards compatibility is preserved.

# pepr 0.1 - 2019-02-01

## Added

* add `activateSubproject` method
* add `fetchSamples` function
* add `checkSection` method on `Config` object

## Changed

* if the `subproject` argument of the `Project()` function is not present in the config, the original project is returned
* paths in the `bioconductor` section of the config are made absolute and environment variables are read
* no sample annotation is allowed if any subprojects are defined in the config
* fixed the problem with paths expansions in sample subannotations case


# pepr 0.0.4 - 2018-11-14

## Changed

* change the `Project` object construction, the subproject can be activated at construction time
* change `implied/derived_columns` to `implied/derived_attributes`. Backwards compatible
* change `constants` to `constantAttributes`. Backwards compatible
* fix `expandPath()` function, add error when environment variable not found


# pepr 0.0.3 - 2018-09-12

## Added

* add `derived_columns` functionality
* add `implied_columns` functionality
* add `subannotation` functionality
	
#  pepr 0.0.2 - 2018-09-06

## Added

* first release, includes basic [PEP](https://pepkit.github.io/) reading functions
