
# dyn.log

# Overview <img src="man/figures/hex.png" width = "175" height = "200" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/bmoretz/dyn.log/workflows/R-CMD-check/badge.svg)](https://github.com/bmoretz/dyn.log/actions)
[![License:
MIT](https://img.shields.io/badge/license-MIT-green.svg)](https://cran.r-project.org/web/licenses/MIT)
[![Codecov test
coverage](https://codecov.io/gh/bmoretz/dyn.log/branch/master/graph/badge.svg)](https://app.codecov.io/gh/bmoretz/dyn.log?branch=master)
[![](https://img.shields.io/badge/devel%20version-0.4.0-blue.svg)](https://bmoretz.github.io/dyn.log/)
[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-darkgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

The goal of `dyn.log` is to be a comprehensive, dynamic,
configuration-based logging package for R. While there are several
excellent logging solutions already in the R ecosystem, I found none of
them were both robust functionality-wise, and completely generic i.e.,
they are designed exclusively for use within a specific environment
(shiny/plumber/etc).

This is the raison d’être for `dyn.log`: a flexible logging solution
that:

-   Integrates seamlessly
    -   set an R option or call `init_logging` in your .Rprofile and
        your logger is attached (and will re-attach when you
        `rm(list = ls())`).
-   Agnostics of project type, so it can be deployed across the entire
    application stack
    -   [shiny](https://github.com/rstudio/shiny)
    -   [plumber](https://github.com/rstudio/plumber)
    -   stand-alone prototype scripts
    -   etc.
-   Reusable
    -   log layout, levels, etc. can be highly-customized for both shiny
        apps and plumber services, then reused across packages running
        in different environments (docker containers).

### Demos

In order to keep the package as light as possible dependency-wise, there
are no demos that come integrated with the package. However, separate
repositories are **coming soon** to demonstrate the above scenarios and
will be linked here.

## Installation

#### GitHub

You can install the development version of dyn.log from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("bmoretz/dyn.log")
```

#### CRAN

You can install the latest stable version of dyn.log from CRAN:

(coming soon)

### Overview

``` r
library(dyn.log)
```

### Basic Usage

For basic/most common usage simply install the package from one of the
above sources, load the package, initialize the logger, and a logging
instance will show up in your global environment (by default, named
‘Logger’):

``` r
library(dyn.log)

init_logger()

var1 <- "abc"; var2 <- 123; var3 <- runif(1)

Logger$debug("my log message - var1: {var1}, var2: {var2}, var3: {var3}")
```

![basic log ouput](man/figures/README-basic-log-output.PNG)

You can also skip the call to *init_logger* by setting a global option
that specifies the configuration you wish to use, i.e., placing:

``` r
options("dyn.log.config" = "default")
```

In your .Rprofile will automatically configure the default logger and
the global logging instance will be attached when you call:

``` r
library(dyn.log)
```

The **“dyn.log.config”** variable can be either a predefined
configuration (name) in the package, or a path to a local file that you
have pre-customized. This is useful for sharing a single bespoke log
configuration across multiple packages or projects.

### Logging

There are three main components of a log message, each of them are
covered in detail in their respective vignettes. For more detail about
how logging works and how you can customize it, please see the [package
site](https://bmoretz.github.io/dyn.log/):

-   Levels
    -   The levels you want to have accessible in your logger.
-   Formats
    -   The types that define contextual information to be logged in a
        message.
-   Layouts
    -   Containers for format objects that define the rendering
        specifications for a log message.

The logging functionality is exposed by a R6 class, *LogDispatch*, is
accessible as a global variable called, by default, **Logger**. The
Logger will have methods that correspond to the *log levels* that are
defined in its yaml configuration, which makes logging intuitive. Log
messages are automatically assumed to be in standard
[glue](https://github.com/tidyverse/glue) format so local environment
variables are captured in messages.

#### Simple Example

The “out of the box” (OTB) configuration specifies a default vanilla log
format that displays the level that was logged, the current time-stamp
(with the default TS format), and the log message:

``` r
var1 <- "abc"; var2 <- 123; var3 <- runif(1)

Logger$debug("my log message - var1: {var1}, var2: {var2}, var3: {var3}")
```

![basic log ouput](man/figures/README-basic-log-output.PNG)

#### Configuration

Everything about dyn.log is configuration driven, the package comes with
a basic configuration **default.yaml**, show below it its entirety and
broken down in the sections that follow:

*For a detailed look at customizing these settings please see
[Configurations](https://bmoretz.github.io/dyn.log/articles/Configuration.html)
vignette online.*

``` yaml
variable: Logger
settings:
  threshold: TRACE
  callstack:
    max: 5
    start: -1
    stop: -1
levels:
- name: TRACE
  description: This level designates finer-grained informational events than the DEBUG.
  severity: 600
  log_style: !expr crayon::make_style("antiquewhite3")$bold
  msg_style: !expr crayon::make_style('gray60')
- name: DEBUG
  description: This level designates fine-grained informational events that are most useful to debug an application.
  severity: 500
  log_style: !expr crayon::make_style('deepskyblue2')$bold
  msg_style: !expr crayon::make_style('gray90')
- name: INFO
  description: This level designates informational messages that highlight the progress of the application at coarse-grained level.
  severity: 400
  log_style: !expr crayon::make_style('dodgerblue4')$bold
  msg_style: !expr crayon::make_style('gray100')
- name: SUCCESS
  description: This level designates that the operation was unencumbered.
  severity: 300
  log_style: !expr crayon::make_style('chartreuse')$bold
  msg_style: !expr crayon::bgGreen$bold$black
- name: WARN
  description: This level designates potentially harmful situations.
  severity: 350
  log_style: !expr crayon::make_style('darkorange')$bold
  msg_style: !expr crayon::bgYellow$bold$black
- name: ERROR
  description: This level designates error events that might still allow the application to continue running.
  severity: 200
  log_style: !expr crayon::make_style('firebrick1')$bold
  msg_style: !expr crayon::bgBlack$bold$white
- name: FATAL
  description: This level designates very severe error events that will presumably lead the application to abort.
  severity: 100
  log_style: !expr crayon::make_style('firebrick')$bold
  msg_style: !expr crayon::bgRed$bold$white
layouts:
- association: default
  seperator: ' '
  new_line: \n
  formats: new_fmt_log_level(),
           new_fmt_timestamp(crayon::silver$italic),
           new_fmt_log_msg()
- association: level_msg
  seperator: ' '
  new_line: \n
  formats: new_fmt_log_level(),
           new_fmt_log_msg()
```

### Logger Variable

The first setting, *variable*, defines the name of the global variable
you want to access the logger with. The default is **Logger**, but you
can easily change it to: *log*, *my_log*, *msg* or any other value (as
long as it’s a *valid* R variable name). The **LogDispatch** object is
also a [singleton](https://en.wikipedia.org/wiki/Singleton), so you
always access the logger directly:

``` r
nums <- paste0(round(rnorm(25, 0, 5), digits = 2), collapse = ", ")

LogDispatch$new()$warn("These numbers '{nums}' are out of the expected range.")
```

![custom var name](man/figures/README-cust-var-name.PNG)

#### Settings

The **settings** node contains the core settings of the log dispatcher,
by attribute. These are covered in detail in the
[Configuration](https://bmoretz.github.io/dyn.log/articles/Configuration.html)
section of the manual.

#### Levels

The **levels** node contains the log levels you want available in your
environment. When a log level is defined in the configuration, it
automatically becomes accessible via a first-class function on the
dispatcher, e.g.:

``` r
Logger$info("This will be logged with 'INFO' severity level")
```

You can view all configured log levels, and get a quick summary about
them by calling *display_log_levels():*

``` r
display_log_levels()
```

![log levels](man/figures/README-std-levels-out.PNG)

The default logging configuration closely resembles the fairly
ubiquitous
[log4j](https://logging.apache.org/log4j/1.2/apidocs/org/apache/log4j/Level.html)
scheme. For a detailed look at log levels refer to the
[Levels](https://bmoretz.github.io/dyn.log/articles/Levels.html)
vignette online.

#### Layouts

Every log message needs to have a format so the dispatcher knows what to
render on a log call. Formats are defined in the yaml config and comes
with some basic ones pre-configured.

The default log layout is a standard format: {LEVEL} - {TIMESTAMP} -
{MSG}, with space as a separator between format objects.

### Customizing a Log Message

Log message layouts are exposed as an S3 type in the package called
*log_layout*. Layouts are composed from a series of objects that inherit
from *fmt_layout*.

``` r
new_log_layout(
  format = list(
    new_fmt_metric(crayon::green$bold, "sysname"),
    new_fmt_metric(crayon::red$bold, "release"),
    new_fmt_line_break(),
    new_fmt_log_level(),
    new_fmt_timestamp(crayon::silver$italic),
    new_fmt_log_msg()
  ),
  seperator = '-',
  association = "custom"
)

Logger$info("my log message - var1: {var1}, var2: {var2}, var3: {var3}", layout = "custom")
```

![custom log ouput](man/figures/README-custom-log-output.PNG)

For a detailed look at layouts refer to the
[Layouts](https://bmoretz.github.io/dyn.log/articles/Layouts.html)
vignette online.

### Logging Associations

One thing you may have noticed about the previous log layout definition
was the *association* parameter. Associations are a useful way to build
a customized log layout for your custom R6 types. This can be especially
useful in larger applications, such as
[plumber](https://github.com/rstudio/plumber/) services or
[shiny](https://github.com/rstudio/shiny) dashboards.

A TestObject is defined as below, who’s primary responsibly is to assign
a randomly generated identifier to the instance via the constructor.
There is also a method on the object that will call the logger with some
local scope variables that will be logged as well.

``` r
TestObject <- R6::R6Class(
  classname = "TestObject",

  public = list(
    id = NULL,

    initialize = function() {
      self$id <- private$generate_id()
    },

    test_method = function() {
      a <- "test"; b <- 123; c <- runif(1)

      Logger$info("these are some variables: {a} - {b} - {c}")
    }
  ),

  private = list(
    generate_id = function(n = 15) {
      paste0(sample(LETTERS, n, TRUE), collapse =  '')
    }
  )
)

obj <- TestObject$new()
```

With the above class defined, we can create a custom log layout that
associated with this R6 type with a new log layout:

``` r
new_log_layout(
  format = list(
    new_fmt_literal(crayon::cyan$bold, "Object Id:"),
    new_fmt_cls_field(crayon::bgCyan$silver$bold, "id"),
    new_fmt_line_break(),
    new_fmt_log_level(),
    new_fmt_timestamp(crayon::silver$italic),
    new_fmt_log_msg(),
    new_fmt_line_break(),
    new_fmt_metric(crayon::green$bold, "sysname"),
    new_fmt_metric(crayon::red$bold, "nodename"),
    new_fmt_literal(crayon::blue$bold, "R Version:"),
    new_fmt_metric(crayon::blue$italic$bold, "r_ver"),
    new_fmt_line_break()
  ),
  association = "TestObject"
)

# notice above, "Logger$info" is called inside the context of the Test Object,
# and the variables are scoped to inside the function.
obj$test_method()
  
Logger$debug("this is a normal log msg")
```

![custom log ouput](man/figures/README-cls-association-output.PNG)

As you can see, only when the logger is invoked from inside the class
that has a custom layout associated with it does the custom layout get
used. The follow-up log call (outside the class scope) reverts back to
the standard layout settings.

*For a detailed look at customizing a layout for a specific type, please
see
[Configurations](https://bmoretz.github.io/dyn.log/articles/Configuration.html)
vignette online for an example.*

## Acknowledgments

-   R Core for developing and maintaining such an amazing language.
-   R Studio for building an incredible open-source ecosystem.
-   [Hadley Wickham](https://github.com/hadley) for being super-human.
-   [Jim Hester](https://github.com/jimhester) for all the fantastic
    r-lib/actions (covr, lintr & build, pkgdown, etc).
-   [Brody Gaslam](https://github.com/brodieG/fansi) for developing the
    [fansi](https://github.com/brodieG/fansi) package which is
    responsible for all the the pretty logger output in the
    documentation.
-   Everyone in the [#rstats](https://twitter.com/search?q=%23rstats)
    community for being inclusive, welcoming and incredibly
    knowledgeable.
