[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active)
[![Is the package on CRAN?](http://www.r-pkg.org/badges/version/assertive.reflection)](http://www.r-pkg.org/pkg/assertive.reflection)
[![SemaphoreCI Build Status](https://semaphoreci.com/api/v1/projects/c2645947-b139-47ec-8cde-409f0912df32/635147/badge.svg)](https://semaphoreci.com/richierocks/assertive-reflection)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/jb6bmar2fxbe35pa?svg=true)](https://ci.appveyor.com/project/richierocks/assertive-reflection)
[![Research software impact](http://depsy.org/api/package/cran/assertive.reflection/badge.svg)](http://depsy.org/package/r/assertive.reflection)

# assertive.reflection

A set of predicates and assertions for checking the state and capabilities of R, the operating system it is running on, and the IDE being used.  Most of the documentation is on the *[assertive](https://bitbucket.org/richierocks/assertive)* page.  End-users will usually want to use *assertive* directly.


### Installation

To install the stable version, type:

```{r}
install.packages("assertive.reflection")
```

To install the development version, you first need the *devtools* package.

```{r}
install.packages("devtools")
```

Then you can install the *assertive.reflection* package using

```{r}
library(devtools)
install_bitbucket("richierocks/assertive.reflection")
```

### Predicates

There are checks for

Whether you are running R (or an old S-PLUS): `is_r`.

Your operating system: `is_windows`, `is_unix`, `is_linux`, `is_mac`, `is_solaris`.

Your IDE: `is_architect`, `is_rstudio`, `is_revo_r`.  Further, for RStudio, you have `is_rstudio_desktop`, `is_rstudio_server`.

R's capabilities: `r_has_png_capability`, `r_has_tcltk_capability`, etc., for each of the values returned by `base::capabilities()`.

32/64 bit R: `is_32_bit`, `is_64_bit`.

How you are running R: `is_batch_mode`, `is_interactive`, `is_r_slave`.

What type of R you are running: `is_r_release`, `is_r_patched`, `is_r_devel`, `is_r_alpha`, `is_r_beta`, `is_r_release_candidate`.

Whether your software is up to date: `is_r_current`, `is_package_current`, `is_rstudio_current`.

What you use for a (numeric or monetary) decimal point: `is_comma_for_decimal_point`, `is_period_for_decimal_point`.

Whether a file or directory is on the OS search path: `is_on_os_path`.

### Assertions

Predicates that return a vector have two corresponding assertions.  For example,
`is_on_os_path` has `assert_all_are_on_os_path` and `assert_any_are_on_os_path`.

Predicates returning a single logical value have one corresponding assertion.
For example, `is_r` has `assert_is_r`.


### Utilities

`sys_get_locale` and `sys_set_locale` are convenience wrappers to `Sys.getlocale` and `Sys.setlocale` respectively.