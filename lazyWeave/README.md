[![DOI](https://zenodo.org/badge/6246/nutterb/lazyWeave.png)](http://dx.doi.org/10.5281/zenodo.12618)  

lazyWeave
=====================

The motivation behind `lazyWeave` was to reproducible reports among those R users who hadn't yet learned LaTeX.  It was, in my opinion, a noble goal, but happened to coincide with the wiser efforts behind the development of the `knitr` and `rmarkdown` packages.  The development of these tools, which have become so common in the R community, have rendered most of the functionality of `lazyWeave` obsolete.

So at this point, we may ask, "why `lazyWeave` at all?"  There are a handful of functions that I find quite useful still, and they can still be used in the `rmarkdown` documents.  The functions you'll likely find most useful are:

* `lazy.matrix` 
* `lazy.table`
* `cattable`
* `conttable`
* `catconttable`
* `univ`

All of these functions are capable of producing output in LaTeX, HTML, and RMarkdown.

`lazyWeave` is somewhat similar to the `xtable` package.  What are the advantages of `lazyWeave`?  To be honest, there really aren't a lot. In fact, `xtable` has quite a few more bells and whistles than `lazy.matrix`.  For instance, with `xtable` you can turn column headings sideways, or use the `longtable` package in `LaTeX`.  Eventually, I may add support for these features.

The only advantage `lazy.matrix` has over `xtable` is the ability to apply colors to the background of table rows.

The other advantage over `xtable` is the ability to define multicolumn cells (in LaTeX and HTML only) when building custom tables with `lazy.table`.

Beyond those basics, `cattable`, `conttable`, `catconttable`, and `univ` provide ready-made functionality for basic summaries with univariable comparisons.  In fact, you may find that they are generally publication ready out of the box.
