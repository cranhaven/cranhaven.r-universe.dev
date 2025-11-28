# depigner 0.9.1
- R 4.1+ is now required to allow for native pipe operator to be used in the source code.
- .data is not use anymore in tidyselect operations
- fix lintr test namespace
- simplified gdp (lintr)
- updated GHA to newer version and updated checks to linux 22.04, and
  win olderel
- updated `data(transplant)` to
 `data("transplant", package = "survival")`

# depigner 0.9.0

- updated the broken `README` links.
- added `view_in_excel()` function to view/open a data frame in excel in the middle of a pipe chain without disturbing the execution of the chain itself. (#49)
- now the documentation for `tidy_summary` explicitly stated that the output is supposed to be used as input to `pander` (#50).
- updated Roxygen Note for documentation settings to 7.2.0
- added `{progress}`, `{readxl}`, and `{writexl}` to `pkg_utils`; `{ggpubr}` to `pkg_docs`; `{ggeffects}`, and `{dlnm}` to `pkg_stat`; `{tarchetypes}`, and `{checkmate}`, removing `{fs}` (included in `{tarchetypes}`), from `pkg_devel`. (#47, #48)

# depigner 0.8.4

-   convert all implicit integers in explicit (e.g, 1L instead of 1).
-   convert all `$` extraction in `[[`.
-   add `digits = 3` option to `tidy_sumamry()`.
-   update README making all the code visible (\#39).
-   fix an error in the the example of `tidy_summary()` (\#38).
-   Improved error messages for `adjust_p()` when `tidy_summary()` is called without `prtest = TRUE` option.
-   Updated test tests for testthat 3rd edition (\#32).
-   Move `{rms}` to IMPORTS.

# depigner 0.8.3

-   Fixed a bug in `paired_test_categorical()` that happen when only one value is zero from one group to another (\#24).
-   Fixed a bug related to temporary project and packages in testing environments (\#28).
-   Fixed `summary_interact()` after stop of support from `{broom}` to `summary.rms` objects (\#27).
-   Fixed a bug that prevent to `paired_test_categorical()` to work properly when (possibly unnamed) matrices are provided to it instead of tables (\#22, \#23).

# depigner 0.8.2

-   Fixed a bug, caused to an updated version of `as_tibble`, that prevent to variables names used in `summary()` to be showed when passed to `tidy_summary()` (\#17).
-   To avoid collision with `{usethis}` namespace itself, `use_ui` now use its own `R/utils-depigner.R` template which incorporate the roxygen2 comments to update the `NAMESPACE` by depigner. Currently this is used to activate the `{usethis}`' User Interface functions only (fixes \#18).
-   Fixed a typo in `use_ui` that prevents it to work correctly (\#18).

# depigner 0.8.1

-   Changed `installed.packages()` using `find.package()` to speed up the evaluation considerably, especially when thousand of packages are installed into the system. In the `README`'s example is still used `installed.packages()` because that example have to know all the names of the installed packages.
-   Changed `\dontrun{}` with `\donttest{}` in the examples which can "run" but should not be tested. Telegram's related examples and the `use_ui()` one still report `\dontrun{}` because they require special environments (Telegram configuration and package-like project) to be executed without errors (\#16)

# depigner 0.8.0

-   Reduced cyclocomp up to 15.
-   Improved and execute `if (interactive())` only, the `gdp()` examples.
-   Removed `{usethis}` interface for message startup to make it able to be suppressed.
-   Added `imported_from()` function to get the list of imported packages by a package.
-   Changed interface to package installers and sets of packages. Introduced `pkg_*` sets, changed `check_pkg()` to `install_pkg_set()`, exported `please_install()`
-   Updated `README` including a table of contents, some badges, and reordering the sections
-   Moved `fs` to Imports
-   Added dependencies from packages `{lattice}`, `{survival}`, and `{Formula}`, which `Hmisc` depends on (require for checks).
-   Fix `{progress}` reference into `README`.
-   Isolate `use_ui()` from unexported functions of `{usethis}` package.
-   Reformat `DESCRIPTION` purging some unused imports dependencies, and rewriting pkg title and description accordingly to the new corresponding entries in the `README`
-   Fixed a typo in the call of the generic `tidy_summary()`, and refactor it `tidy_summary.summary.formula.reverse()` method.

# depigner 0.7.0

-   Change compatibility requiring R3.6+ (because of package `{latticeExtra}`, required by `{Hmisc}`, which depends on it).
-   Update gh-actions to tidy ones.
-   Update `README`.

# depigner 0.6.1

-   Switch to GH-action for CI.
-   Activate spellcheck.
-   Activate lint.
-   Activate gh-pages with pkgedown.

# depigner 0.6.0

-   Update `README` including examples for all the exported functions.
-   Added `Htypes()`, `Htype()` and complementary functions and relative. tests, to get/check when a variable is considered categorical or continuous by the `{Hmisc}` ecosystem.

# depigner 0.5.0

-   Introduce `rlang::.data` to remove notes on CMD check for tidyeval missing variables.
-   Update all the UIs to usethis' ones.
-   Exported `ci2p()` (issue \#6).
-   Add `use_ui()` to add what is necessary to use the usethis' user interfaces in a package.

# depigner 0.4.1

-   Add `tick()` as a wrapper to update a progress bar.
-   Add `pb_len()` as a wrapper to `progress::progress_bar()` for a quick and ready to use progress bar with default options.

# depigner 0.4.0

-   Minor (warnings) fixes to the tests.
-   Add `adjust_p()` function to adjust the P-values from a `tidy_summary` summary using methods provided by `p.adjust()`.
-   Add `errors_to_telegram()` function to automatically parrot all the error messages to a chat from a Telegram bot.

# depigner 0.3.1

-   Update `ubesp_pckg` (\#4).
-   Update `ubesp_pckg` (\#5).

# depigner 0.3.0

-   Introduce dependency from R \>= 3.5 because of the package **mvtnorm** which is in the tree of **rms** and **Hmisc** and depends on the version of R 3.5.
-   Moved **survival**, **rms** and **Hmisc** from Imports to Suggests.
-   Added `start_bot_for_chat()` function, `send_to_telegram()` generics with methods for string and ggplot plots, and not exported functionalities as a wrappers to the main functions of the **telegram.bot** package.
-   Added tests to paired tests functions' tests.
-   Remove dependencies from **vcd** package.

# depigner 0.2.0

-   Added `paired_test_categorical()` function to implement paired test for categorical variable accordingly to the number of groups, in a suitable way to be used in the **Hmisc** `summary()` with `method = "reverse"`.
-   Added `paired_test_continuous()` function to implement paired test for continuous variable accordingly to the number of groups, in a suitable way to be used in the **Hmisc** `summary()` with `method = "reverse"`.

# depigner 0.1.0

-   Some variable names updated.
-   Bug fixed in `summary_interact()`.
-   Add some tests.
-   Added support for `tibble`s.
-   Added `tidy_summary()` generics function and method for **Hmisc** `summary()` with `method = "reverse"`.

# depigner 0.0.2

-   Fixed `summary_interact()` function.
-   Added `gdp()` function.
-   Added basic infrastructure to the package.
