# miscset [![CRAN][cran_svg]][cran] [![license][license_svg]][license]

**A [GNU R][rproject] package for miscellaneous tools.**

A collection of miscellaneous methods to simplify various tasks,
including plotting, data.frame and matrix transformations, environment
functions, regular expression methods, and string and logical operations, as
well as numerical and statistical tools. Most of the methods are simple but
useful wrappers of common base R functions, which extend S3 generics or
provide default values for important parameters.
  
## Installation

Install the latest version from **CRAN** via:

```r
install.packages('miscset')
```

Install the development version from **github** via:

```r
install.packages('devtools')
devtools::install_github('setempler/miscset@develop', build_vignettes = TRUE)
```

## Documentation

For an introduction see the package vignette:

* online at [CRAN][cran_vignette]
* local R session via `vignette("miscset")` or `miscset::help.index(miscset)`

## Development [![travis CI][travis_svg2]][travis] [![releases][github_svg]][github]

Follow and contribute to development on github via:

* [source code](https://github.com/setempler/miscset)
* [issues](https://github.com/setempler/miscset/issues)

## License 

> Copyright (C) 2016 Sven E. Templer [sven.templer at gmail dot com]
> 
> This program is free software: you can redistribute it and/or modify
> it under the terms of the GNU General Public License as published by
> the Free Software Foundation, either version 3 of the License, or
> (at your option) any later version.
> 
> This program is distributed in the hope that it will be useful,
> but WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
> GNU General Public License for more details.
> 
> You should have received a copy of the GNU General Public License
> along with this program. If not, see <http://www.gnu.org/licenses/>.

[cran]: https://cran.r-project.org/package=miscset
[cran_svg]: http://www.r-pkg.org/badges/version/miscset
[github]: https://github.com/setempler/miscset/releases/
[github_svg]: https://img.shields.io/github/release/setempler/miscset.svg
[license]: https://github.com/setempler/miscset/blob/master/LICENSE
[license_svg]: https://img.shields.io/github/license/setempler/miscset.svg
[travis]: https://travis-ci.org/setempler/miscset
[travis_svg2]: https://travis-ci.org/setempler/miscset.svg?branch=develop

[rproject]: http://r-project.org
[cran_vignette]: https://CRAN.R-project.org/package=miscset/vignettes/miscset.html
[github_vignette]: http://htmlpreview.github.io/?https://github.com/setempler/miscset/blob/master/vignettes/miscset.html
