[![CRANhaven number of packages](https://cranhaven.r-universe.dev/badges/:total)](https://cranhaven.r-universe.dev/)
[![CRAN synchronization](https://github.com/CRANhaven/cranhaven.r-universe.dev/actions/workflows/update.yml/badge.svg)](https://github.com/CRANhaven/cranhaven.r-universe.dev/actions/workflows/update.yml)

# CRANhaven - Repository for Recently Archived CRAN Packages

_WARNING: This is work under development! Please do _not_ rely on it
for production and do _not_ add CRANhaven to `Additional_repositories`
of your package._


The **[CRANhaven]** R package repository, available on R-universe,
hosts packages that were recently archived on CRAN. Those packages can
be installed using:

```r
install.packages("somepkg", repos = c(getOption("repos", "https://cloud.r-project.org"), "https://cranhaven.r-universe.dev"))
```

Packages remain on CRANhaven for up to five weeks, or until they are
unarchived on CRAN.


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
package is not unarchived on CRAN within five weeks, it is removed
from this repository. This gives R users, developers, and
reverse-dependency maintainers some extra leeway.


## Known Limitations

Some of the CRANhaven packages fail to build temporarily only because
they depend on another archived CRAN package, which still has not been
built on CRANhaven. This will normally resolve itself after a few
hours; R-universe re-synchronize once an hour and it might require a
few rounds for all packages to be built.


## Details

The "Commit" datestamps seen on [CRANhaven] show the dates (±1 day)
when the packages were archived on CRAN.


[CRANhaven]: https://www.cranhaven.org/
