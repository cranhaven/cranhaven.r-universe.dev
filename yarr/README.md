yarr
====

*Yet Another ARFF Reader*

[![R language](https://img.shields.io/badge/language-R-lightgrey.svg)](https://www.r-project.org/)
[![Downloads](https://cranlogs.r-pkg.org/badges/yarr)](https://cranlogs.r-pkg.org/downloads/total/last-month/yarr)
[![Travis](https://img.shields.io/travis/fdavidcl/yarr/master.svg)](https://travis-ci.org/fdavidcl/yarr)
[![GPL v3 license](https://img.shields.io/github/license/fdavidcl/yarr.svg)](https://www.gnu.org/licenses/gpl.html)

---

So you need to read an ARFF file. You can use:

- `foreign::read.arff`, but only on dense format.
- `farff::readARFF`, but only on dense format.
- `RWeka::read.arff`, but you need to configure Java.

`yarr::read.arff` can read dense **and sparse** ARFF files, and it's implemented in pure R.

The implementations in R are derivatives of those in [`mldr`](https://github.com/fcharte/mldr) and [`mldr.datasets`](https://github.com/fcharte/mldr.datasets), packages for management of multilabel learning datasets.


### Usage

```r
remotes::install_github("fdavidcl/yarr")
library(yarr)
download.file("https://www.openml.org/data/download/1681111/phpEUwA95", "dexter.arff")
dexter <- read.arff("dexter.arff")
dexter
# An ARFF dataset: dexter
# 20001 attributes and 600 instances
#   V0: numeric (min = 0, mean = 0.12, max = 72) 0, 0, 0, 0, 0...
#   V1: numeric (min = 0, mean = 0, max = 0) 0, 0, 0, 0, 0...
#   V2: numeric (min = 0, mean = 0, max = 0) 0, 0, 0, 0, 0...
#   V3: numeric (min = 0, mean = 1.165, max = 427) 0, 0, 0, 0, 0...
#   V4: numeric (min = 0, mean = 0.57, max = 342) 0, 0, 0, 0, 0...
#   V5: numeric (min = 0, mean = 0.386666666666667, max = 116) 0, 0, 0, 0, 0...
#   V6: numeric (min = 0, mean = 0, max = 0) 0, 0, 0, 0, 0...
#   V7: numeric (min = 0, mean = 0.448333333333333, max = 121) 0, 0, 0, 0, 0...
#   V8: numeric (min = 0, mean = 0.358333333333333, max = 84) 0, 0, 0, 0, 0...
#   ...
#   class: {-1,1} (-1: 300, 1: 300) 1, 1, 1, -1, 1...
relation(dexter)
# [1] "dexter"
write.arff(dexter, "dexter-new.arff")
```

### Supported features

- Dense format
- Sparse format (`{index value}`, e.g. `{0 1, 20 1}`)
- Missing values with `?`
- MEKA parameter `c` in `@relation`.

### Unsupported features

- Date attributes are read as mere strings
- Relational attributes are not supported
- Instance weights
