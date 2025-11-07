# Release Summaries <img src="man/figures/hex.png" width = "175" height = "200" align="right" />

# dyn.log 0.4.0

## What's Changed

* New Features
  + Ground-up rewrite of the dynamic log dispatch mechanics, to make them:
    + Simpler. There were areas of the old implementation that were incredibly hard to follow (log levels and log messages, in particular).
    + Consistent. Every format object that needs contextual information now gets it injected via the context list in the dispatch function.
    + Cleaner. Now leverages even more awesomeness from [rlang](https://github.com/r-lib/rlang), specifically 'rlang::new_function' to dynamically create the log dispatch routines & 'rlang::pairlist2' to handle completely dynamic & non-defaulted parameters (msg).
    + Testable. The new mechanics are much more unit testable, and once the code is cleaned up more (and simplified further), will focus on this area.
  
* Enhancements
  + updated the README.md to be more reflective of the overall goal of the package.
  + streamlined the configuration (in init_logger) to have clear separation of concerns.
  + added a 'default' logger method that is based on the attached level with the highest severity, so in areas where you always want the lowest possible information logged (even if configurations change) then you can just use 'Logger$default.' Useful for packages that might be attached by multiple downstream packages with different level definitions.

# dyn.log 0.3.3

## What's Changed

* New Features
  + N/A
  
* Enhancements
  + additional linters:
    + T_and_F_symbol_linter
    + equals_na_linter
    + nonportable_path_linter
  + resolved all warnings produced by including the new lintrs.
  + updated the README.md to be more reflective of the overall goal of the package.
  
* Other
  + vscode support
    + moved the development configuration that supports vscode into a separate branch and cleaned up all the additional dependencies.
  
# dyn.log 0.3.2

## What's Changed

* New Features
  + added a default logging function that uses the level with the highest severity.
  + changed how loggers are instantiated, you must now call init_logging() or
    specify the logging configuration with the dyn.log.config option.
  
* Enhancements
  + cleaned up the entire configuration module and streamlined the
    process of initialization.
  + updated configuration vignette to reflect these changes.
  + added withr suggestion package to isolate all logging unit
    tests to their initialized environment.
  + added a configuration setting to change the variable name
    of the global logging instance. "Logger" is still the default,
    but now you can specify any legal R variable name by default.

# dyn.log 0.3.1

## What's Changed

* New Features
  + None
  
* Enhancements
  + refactored the entire configuration module 
    to streamline the process of attaching a
    logger to the global environment.
    
* Bug-fixes
  + added an event handler to '.onAttach' that
    monitors for the logger's existence on every
    load; the annoying behavior of wiping local
    environment vars and the logger disappearing
    is now fully resolved.
    
# dyn.log 0.3.0

## What's Changed

* New Features
  + added private field support for class context logging.
  
* Enhancements
  + added lintr coverage to makefile.
  + added public & private property examples to the Configuration vignette.
  + bug fix in call stack context due to change in rlang::trace
  + cleaned up formatting for all unit tests.
  
* lintr code clean-up, cleaned up all lintr warnings in the following categories:
  + object_name_lintr (R6 Class names have explicit excludes)
  + commas_lintr
  + assignment_lintr
  + object_useage_lintr
  + spaces_left_parenthesis_linter
  
# dyn.log 0.2.5

## What's Changed
* New Pkg Logo
  + created a Hex logo for dyn.log package.

* Enhancements
  + added a pkg hex + added to readme
  + updated dependency pkg versions
  + cleaned up logging configurations
  + started cleaning up unit tests
  
# dyn.log 0.2.4

## What's Changed
* Enhancements
  + cleaned up code coverage to  ~95%.
  + added vignette on "Configuration" to detail the steps in customizing dyn.log in client applications.
  + cleaned up all logging configurations to streamline customization.
  + added clear examples on how to setup bespoke customizations to dyn.log via config templates
  
# dyn.log 0.2.3-1

## (Patch Release)
* Enhancements
  + patch to expose configurations
  
# dyn.log 0.2.3

## What's Changed
* New Features
  + added functionality to export logging configurations that are bundled with the package so they can effectively be used as templates in consuming clients.
  + Added a configuration vignette with examples on how to use the bundled configurations as templates.
  
* Enhancements
  + cleaned up renv dependencies and git actions build cache mechanics to reduce build times.
  + added dispatch & singleton helper objects to streamline unit testing of core functionality.
  + streamlined threshold and log dispatch evaluation routine.
  + cleaned up makefile to provide a clean & efficient interface for building and deploying the package.
  + cleaned up all logging configurations and added ability to specify layouts with strings or !expr's.
  
# dyn.log 0.2.2

## What's Changed
* New Features
  + added execution context & related log formatters (call stack, top call, parent fn, etc.)
  + added vignettes for: **levels**, **formats** and **layouts**

* Enhancements
  + clearly defined context objects as structured classes.
  + added call stack & execution scope based on [rlang](https://github.com/r-lib/rlang) trace.
  + updated all vignettes to use [fansi](https://github.com/brodieG/fansi) package to display clean logging output like you would see in the terminal.
  + added callstack evaluation parameters to logging configuration to account for things like [testthat](https://github.com/r-lib/testthat) and [knitr](https://github.com/yihui/knitr)
  + general cleaned up of documentation and unit tests.
  + added a lintr github action & started working through clearing all the warnings.

# dyn.log 0.1.3-alpha

## What's Changed
* New Features
  + Log layouts are now fully configuration driven, w/ some reasonable defaults.

* Enhancements
  + cleaned up log level/layout active bindings so they don't need separate accessor methods to get instantiated objects in the bindings (by name).
  + updated the README to use fansi package to display clean logging output like you would see in the terminal.
  + refactored log layouts to have a formats parameter that specify how to render the log layout.
  + moved default layouts from code to configuration under -layouts node in config.yaml.
  + cleaned up associated unit tests & documentation. cleaned up generics 'style', 'value' and 'format'.
  + updated renv pkgs cache in the github actions CI builds to reduce build time ~90%.

# dyn.log 0.1.2-alpha

## What's Changed
* New Features
  + added codecov, R CMD Check & pkgdown github actions
  + added a pkg down site and started fleshing out basic vignettes on usage/design.

* Enhancements
  + cleaned up documentation package-wide
  + converted log levels & layouts to active bindings
  + added README that gives a solid overview of what the package is trying to achieve & how
  
# dyn.log v0.1.1-alpha

## Initial Version
  * New Features
  + baseline logging components are fully functional: levels, formats, layouts and the dispatcher.
  + pkg instantiates a singleton instance of the log dispatcher, and the default configuration will give you a fully functional logging environment.
  + cls level customization options are working as expected; you can create a log layout associated with an R6 type, and the have the logger spit out variables from the enclosing class. Example added to README.
