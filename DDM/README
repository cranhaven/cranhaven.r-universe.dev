DDM R package
=============
This project has produced a small R package that implements three methods for two-census indirect estimation of death registration coverage (Generalized Growth Balance (GGB), Synthetic Extinct Generations (SEG), and a hybrid of the two (GGB-SEG)). 

A short tutorial 
------------------
To download the CRAN version of DDM:
```r
install.packages("DDM")
```

The CRAN version of the package does not get updated often, but the development version of the package is updated on github whenever we see the need. To download the most recent (development) version of DDM:

Download the [zip ball](https://github.com/timriffe/AdultCoverage/zipball/master) or [tar ball](https://github.com/timriffe/AdultCoverage/tarball/master), decompress and run `R CMD INSTALL` on the subfolder called `R/DDM` in the terminal command line, or (easier) use the **devtools** package to install the development version:

```r
# install.packages("devtools")

library(devtools)
install_github("timriffe/AdultCoverage/AdultCoverage/R/DDM")
```

Then you can load the package using:

```r
library(DDM)
```

Be aware that if you report a bug and we fix it, then you'll need to reinstall to get the changes. 

Your data need to be in this kind of shape:

```r
head(Moz)
  cod    pop1    pop2 deaths age sex year1 year2
1   1 1388350 1963660  88248   0   f  1997  2007
2   1 1113675 1615244  11424   5   f  1997  2007
3   1  878429 1183939   5677  10   f  1997  2007
4   1  854078  991323   6123  15   f  1997  2007
5   1  827614  986526   7280  20   f  1997  2007
6   1  654465  841416   7212  25   f  1997  2007
```

Here `cod` indicates the group, a single year, sex, region of data that is to be tested. `pop1` and `pop2` are the first and second censuses, respectively. `deaths` can contain the average number of deaths in each age group in the intercensal period or it can contain the sum of the deaths in each age in the intercensal period. If you give the sum, then specify `deaths.summed = TRUE` in the arguments to any of the estimation functions. Otherwise the default is to treat deaths as the average. This could be a straight arithmetic average, or simply the average of the deaths observed around census 1 or census 2. For this later case, you'll need to average yourself beforehand, as `deaths.summed = TRUE` will only do the right thing if deaths over the whole intercensal period are given. 

`age` should be the lower bound of five-year age groups (incl. age 0-4!). If you give standard abridged data (0,1,5), then the `pop1`, `pop2`, and `deaths` from ages 0 and 1 are automatically summed together into the infant category. Don't give single-age data at this time. We hope to add an abridgement function soon, though, to handle such data automatically. `sex` is character, either `"f"` or `"m"`. Census dates can be conveyed in a variety of ways. If only `year1` and  `year2` are given, we assume Jan 1. It is best to specify proper date classes and use `date1`, `date2` as column names instead:

```r
cod    pop1    pop2 deaths age sex      date1      date2
  1 1388350 1963660  88248   0   f 1997-08-01 2007-08-01
  1 1113675 1615244  11424   5   f 1997-08-01 2007-08-01
  1  878429 1183939   5677  10   f 1997-08-01 2007-08-01
  1  854078  991323   6123  15   f 1997-08-01 2007-08-01
  1  827614  986526   7280  20   f 1997-08-01 2007-08-01
  1  654465  841416   7212  25   f 1997-08-01 2007-08-01
```

Results are contingent on evaluating results for particular age ranges. In spreadsheets this is typically done visually, which a plot referenced to some cell range that the user could manipulate. Here, we have a function that works similarly, but you need to use it just for one data grouping at a time (`cod`):

```r
my_ages <- ggbChooseAges(x[x$cod==1,])
```

This will open a graphics device, where you can interactively select age ranges by clicking on ages. When you are done, click in the margin to close the device, and it returns the vector of ages. You can use these, or any other vector of ages, to manually specify the age range that each method should use:

```r
ggb(Moz, exact.ages = my_ages)
seg(Moz, exact.ages = my_ages)
ggbseg(Moz, exact.ages = my_ages)
```

By default these functions will pick a decent age-range on their own:

```r
ggb(Moz)
seg(Moz)
ggbseg(Moz)
```

The result depends on the age-range chosen. If left to automatically choose age-ranges, the evaluation methods will pick one independently for each data grouping (`cod`). Let's say your data has a large number of groupings (regions, countries, intercensal periods, whatever). You can get a messy overview of results by running:

```r
Results <- ddm(my.huge.data)
ddmplot(Results)
```

This overview plot also gives the harmonic mean of the coverage estimate given from the three methods provided.

License
=======

This package is by [Tim Riffe](https://sites.google.com/site/timriffepersonal/), [Everton Lima ](http://www.nepo.unicamp.br/nepo/perfils/everton_lima.html) and [Bernardo Queiroz](https://sites.google.com/site/blanza/). Related technical papers or manuscripts may have different author-ordering.

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">"Death Registration Coverage Estimation (DDM)/span> by Everton Lima, Bernardo Queiroz, and <a xmlns:cc="http://creativecommons.org/ns#" href="https://sites.google.com/site/timriffepersonal/" property="cc:attributionName" rel="cc:attributionURL">Timothy L. M. Riffe</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.

What's missing?
==============

More examples. A proper vignette. We aim to provide a user-guide as an overview of the methods provided here. Further features (calculations, usability features) may be requested by users. 


