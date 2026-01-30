[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/pepr)](https://cranlogs.r-pkg.org/badges/grand-total/pepr)
[![CRAN](https://www.r-pkg.org/badges/version-last-release/pepr)](https://www.r-pkg.org/badges/version-last-release/pepr)
![R-CMD-check](https://github.com/pepkit/pepr/workflows/R-CMD-check/badge.svg)
[![codecov](https://codecov.io/gh/pepkit/pepr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pepkit/pepr)
[![PEP compatible](https://pepkit.github.io/img/PEP-compatible-green.svg)](https://pep.databio.org/en/2.0.0/)

# The `pepr` package: Portable Encapsulated Projects in R

`pepr` is an R package for reading [Portable Encapsulated Projects](http://pep.databio.org/en/2.0.0/), or **PEP**s, in R. If you describe your project (configuration and samples) according to this format, you can load all project metadata into R using the `pepr` package. To test `pepr`, you can try loading one of the [example PEPs](https://github.com/pepkit/example_peps).

Complete documentation and API for the `pepr` R package is at [code.databio.org/pepr](https://code.databio.org/pepr/).


## Quick start:

Install from [CRAN](https://CRAN.R-project.org/package=pepr):

```R
install.packages("pepr")
```

Load a project and explore metadata like this:

```R
library("pepr")
cfgPath = system.file(
    "extdata",
    "example_peps-master",
    "example_basic",
    "project_config.yaml",
    package = "pepr"
  )
p = Project(file = cfgPath)

sampleTable(p)
config(p)
```
