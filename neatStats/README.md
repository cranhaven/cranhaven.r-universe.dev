### neatStats

This R package aims to give a most down-to-earth way possible to get clear, comprehensive, reportable stats out of data from simple (psychological) experiments.

One main point is that all functions use data frames with single row per subjects. Not because "it's like that in SPSS", but because that's the clear and logical way, and because all the reshaping/melting/casting/dcasting is a tiresome fuss even for those not new to it, let alone novices, and leads to a lot of totally unnecessary confusion and problems.

### Installation in R

To install the stable version from [CRAN](https://cran.r-project.org/package=neatStats "The Comprehensive R Archive Network"), just run:

```R
install.packages("neatStats")
```

Then load with: `library("neatStats")`

Alternatively, if you want to install the latest (and potentially unstable) version from this repository:

```
install.packages("devtools") # if "devtools" package is not yet installed
library("devtools")
install_github("gasparl/neatstats")
```

### Usage

See the [neatStats vignette](https://gasparl.github.io/neatstats/vignettes/example_pipeline.html "Example vignette") for an example pipeline for every step from raw data to reportable statistics. For a much more detailed and extended version of the same example, see the [neatStats paper at TQMP](https://www.tqmp.org/RegularArticles/vol17-1/p007/p007.pdf "neatStats: An R package for a neat pipeline from raw data to reportable statistics in psychological science").

For details about each function, see [the manual](https://github.com/gasparl/neatstats/blob/master/neatStats.pdf "neatStats manual") (or enter `help(xy)` or `?xy` in R for any specific function).


### Useful Links

General introduction to R:

- Comprehensive and yet relatively concise and readable official R documentation (requires time and patience; prior programming knowledge advised): https://cran.r-project.org/doc/manuals/r-release/R-intro.html
- Succinct "to the point" intro (best for those with prior programming knowledge; the entire site is very useful): https://www.sthda.com/english/wiki/r-basics-quick-and-easy
- Another short and nice intro: https://cran.r-project.org/doc/contrib/Torfs+Brauer-Short-R-Intro.pdf
- Verbose, step by step intro: https://www.statmethods.net/r-tutorial/index.html
- Shortcuts in Rstudio: https://support.rstudio.com/hc/en-us/articles/200711853-Keyboard-Shortcuts
    - Run current line (or selection): Ctrl+Enter
    - Run from document beginning to current line: Ctrl+Alt+B
    - Run from current line to document end: Ctrl+Alt+E
    - Run code sections: Ctrl+Alt+T (Any comment line which includes at least four trailing dashes (-), equal signs (=), or pound signs (#) automatically creates a code section.)
    - Autoformat ("beautify") code: Ctrl+Shift+A
    - Comment/uncomment (any number of lines): Ctrl+Shift+C
    - Reflow Comment (format to lines with max 80 char): Ctrl+Shift+/

Data visualization:

- ggplot2: https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html
- ggpubr: https://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/
- Shiny R (online interactive sites): https://shiny.rstudio.com/tutorial/
- plotly for making ggplots interactive: https://plotly.com/ggplot2/
- Principles: https://biostat.mc.vanderbilt.edu/wiki/pub/Main/StatGraphCourse/graphscourse.pdf

Significance tests:

- General detailed tutorial on statistics (via R), see e.g. chapters 14 and 16 for ANOVA, 15 for linear modelling: https://learningstatisticswithr.com/book/index.html
- ANOVA assumptions (discussion and basic solution for normality): https://stats.stackexchange.com/questions/485022/
- Equal variances: https://uc-r.github.io/assumptions_homogeneity
- Equal variances discussion: https://stats.stackexchange.com/a/185368/237231
- More ANOVA diagnostics (residuals vs. fitted values): https://arc.lib.montana.edu/book/statistics-with-r-textbook/item/57
- ANOVA multiple-testing issue: https://daniellakens.blogspot.com/2016/01/error-control-in-exploratory-anovas-how.html
- Pairwise comparisons (and corrections) in R: https://www.sthda.com/english/wiki/two-way-anova-test-in-r#multiple-pairwise-comparison-between-the-means-of-groups
- Linear modelling R tutorial (lme4): https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf
- "The perfect t-test": https://github.com/Lakens/perfect-t-test
- Various robust statistics: https://cran.r-project.org/web/views/Robust.html

### Support

* If you run into an error despite carefully following the [documentation](https://github.com/gasparl/neatstats/blob/master/neatStats.pdf "neatStats.pdf"), [open a new issue](https://github.com/gasparl/neatstats/issues "Issues") or [email me](mailto:lkcsgaspar@gmail.com).
* If you have sound reason to believe that some of the presented statistics (or functions) are really not optimal and/or could be improved in some plausible way, [email me](mailto:lkcsgaspar@gmail.com).

### Citation

When you use neatStats in a publication, you can either cite the specific version you used (enter `citation("neatStats")` in R), or the following paper:

Lukács, G. (2021). neatStats: An R package for a neat pipeline from raw data to reportable statistics in psychological science. _The Quantitative Methods for Psychology, 17_(1), 7–23. https://doi.org/10.20982/tqmp.17.1.p007


[![DOI](https://zenodo.org/badge/187226036.svg)](https://zenodo.org/badge/latestdoi/187226036) ![](https://www.r-pkg.org/badges/version-last-release/neatStats "neatStats CRAN last version") ![](https://cranlogs.r-pkg.org/badges/grand-total/neatStats "neatStats CRAN total download count") ![](https://cranlogs.r-pkg.org/badges/neatStats?color=8585ad "neatStats CRAN monthly download count") [![R-CMD-check](https://github.com/gasparl/neatstats/workflows/R-CMD-check/badge.svg)](https://github.com/gasparl/neatstats/actions)
