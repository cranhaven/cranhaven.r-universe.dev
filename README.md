[![CRAN synchronization](https://github.com/CRANhaven/cranhaven.r-universe.dev/actions/workflows/update.yml/badge.svg)](https://github.com/CRANhaven/cranhaven.r-universe.dev/actions/workflows/update.yml)

# CRANhaven - Repository for Recently Archived CRAN Packages

_WARNING: This is a proof of concept to explore the idea of giving
some more leeway to packages that get archived on CRAN. If it turns
out to be a not-so-good idea, it will be removed again._


The **CRANhaven** R package repository available on
[R-universe](https://cranhaven.r-universe.dev) hosts packages that
were recently archived on CRAN. Those packages can be installed using:

```r
install.packages("somepkg", repos = c("https://cranhaven.r-universe.dev", "https://cloud.r-project.org"))
```

## Motivation

Sometimes a package gets archived only because the maintainer did not
have time to address the issues before the deadline given by
CRAN. Sometimes the deadline is as short as two weeks.

The impact of a CRAN package being archived depends on the
package, and how widely used it is, and how many packages depend on
it. In some cases it can be rather disruptive for R users and
developers when one or more packages are archived on CRAN.

Importantly, any package that has a hard dependency (e.g. `Depends:`
and `Imports:`) on an archived package will automatically be archived
too. There is little a developer that is a "victim" of this can do,
other than doing a major rewrite, which is only reasonable to do if
the developer knows that the archived package will not be fixed
anytime soon.

The purpose of this R package repository is to give archived CRAN
packages a second chance. As soon as the package is archived on CRAN,
it is added to this repository. If the packages is fixed and
"unarchived" on CRAN, it is removed from this repository. If the
package is not unarchived on CRAN within four weeks, it is removed
from this repository. This gives R users, developers, and
reverse-dependency maintainers some extra leeway.


## Details

This repository is updated ones an hour. It queries
[CRANberries](https://dirk.eddelbuettel.com/cranberries/cran/removed/)
for recently archived packages, ignores the ones that have been
archived for more than four weeks, or that have since be unarchived on
CRAN.

