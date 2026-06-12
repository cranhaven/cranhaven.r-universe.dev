# `mathml`. Translate R expressions to MathML

_Version 1.6: with Prolog interface_

`mathml` allows rendering R terms as pretty mathematical equations, bridging the
gap between computational needs, presentation of results, and their
reproducibility. 

Researchers or teachers can already use RMarkdown to conduct analyses and show
results, `mathml` smoothes this process and allows for integrated calculations
and output. The package `mathml` can help in fact to improve data analyses and
statistical reports from an aesthetical perspective, as well as regarding 
reproducibility of research, by allowing also for a better detection of possible
mistakes in R programs. 

The package supports both MathML and Latex/MathJax for use in RMarkdown
documents, presentations and Shiny Apps.

## License

This R package is distributed under a BSD-2 simplified
license (see the file LICENSE).

## Installation

1. Download and install a recent R from https://www.r-project.org/

2. Download and install a recent RStudio from https://www.rstudio.com/

3. R> `install.packages("mathml")`

The package depends on 
R package `rolog` [https://cran.r-project.org/package=rolog], which
itself needs the SWI-Prolog runtime on the system. The latter can be installed
either from [https://swi-prolog.org] or by installing 
R package `rswipl` [https://cran.r-project.org/package=rswipl].

# Example

````
library(mathml)
term <- quote(a^b + c * (d + 3) - a^2L * (a + d))
mathout(term)
````

$a^b + c \cdot (d+3.00) - a^2 \cdot (a+d)$

````
term <- call("^", quote(x), 2L)
mathout(term)
````

$x^2$

# For R package developers

If you use `mathml` for your own package, please do not "Import" `mathml` in
your DESCRIPTION, but "Depend" on it.

```
Package: onmathml
Type: Package
Title: A package that uses mathml
...
Depends: 
    R (>= 4.3),
    mathml (>= 1.3)
```

It's not entirely clear why this is needed.

# From Prolog

Installation: `pack_install(mathml).`

Usage: `use_module(library(mathml)).`

Example: `pl_mathml(sin(x), X).`


