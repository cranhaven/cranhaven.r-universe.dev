
<!-- README.md is generated from README.Rmd. Please edit that file -->
shinyShortcut
=============

[![Travis Build Status](https://travis-ci.org/Ewan-Keith/shinyShortcut.svg?branch=master)](https://travis-ci.org/Ewan-Keith/shinyShortcut) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/Ewan-Keith/shinyShortcut?branch=master&svg=true)](https://ci.appveyor.com/project/Ewan-Keith/shinyShortcut) [![codecov](https://codecov.io/gh/Ewan-Keith/shinyShortcut/branch/master/graph/badge.svg)](https://codecov.io/gh/Ewan-Keith/shinyShortcut)

Overview
--------

This package allows users to create a shortcut for their shiny app that, when run, will launch the app directly in the user's default browser. No need to navigate to the correct working directory in R (or even to open R at all!). Simply run the shortcut and the app will fire up. Works for both windows and linux based systems.

Inspired by [this blog post](http://www.mango-solutions.com/wp/2017/03/shiny-based-tablet-or-desktop-app/) by Mark Sellors at Mango Solutions.

Installation
------------

``` r
# Package not yet on CRAN, needs installed from Github:
# install.packages("devtools")
devtools::install_github("ewan-keith/shinyShortcut")
```

Usage
-----

The package loads just a single function, also named `shinyShortcut()`. It takes three arguments:

-   `shinyDirectory`: The home directory of your shiny app (that contains your `server.r`, `ui.r`, or `app.r` files).
-   `OS`: The operating system for the app to be ran on, must be `"windows"` or `"unix"`.
-   `gitIgnore`: Whether to update the `.gitignore` file to prevent the shortcut files being tracked by git.

The function writes an exectuable script (.vbs on windows and .desktop on unix) in to the app's home directory. Shortcuts to this file can then be created elsewhere on the user's system. The function also creates a new directory `/.shiny_run` into which is written the raw batch or bash script (Windows and unix respectively) that runs the shiny app.

When the `shinyDirectory` is the current working directory then the default arguments are sufficient.

``` r
library(shinyShortcut)

shinyShortcut()
```
