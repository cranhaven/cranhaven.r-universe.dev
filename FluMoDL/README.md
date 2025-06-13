### FluMoDL - InFluenza-attributable Mortality with Distributed Lag models

This package implements a method to estimate influenza-attributable mortality, and 
also mortality attributable to ambient temperatures, using distributed lag nonlinear
models. These models allow addressing the lag dimension of mortality, and provide for
a more detailed adjustment for the confounding effect of temperature in the relationship
between influenza and mortality.

To fit a FluMoDL one needs to have:

* a series of daily mean temperatures for the region of interest
* a daily series of deaths
* a weekly series of Influenza-Like Illness (ILI) rates, estimated via sentinel surveillance
* three weekly series of laboratory swab samples Percentage Positives (%) to influenza 
A(H1N1)pdm09, A(H3N2) and B

These should all cover the same time period.

The package provides functions to facilitate fitting a FluMoDL, summarizing and plotting the
results of the analysis, and calculating attributable mortalities (including empirical 95% 
Confidence Intervals). It also includes the capability to pool analytical results together
and calculate attributable mortalities based on BLUP (Best Unbiased Linear Predictor) estimates.

**Installation**

Packages ['dlnm'](https://CRAN.R-project.org/package=dlnm) and 
['mvmeta'](https://CRAN.R-project.org/package=mvmeta), by Antonio Gasparrini are the only
dependencies for the FluMoDL package. You need to have those installed from CRAN with
`install.packages(c("dlnm","mvmeta"))`. Also install the devtools package if you don't 
already have it: `install.packages("devtools")`.

Then to install FluMoDL, open R and give:

      devtools::install_git("https://github.com/thlytras/FluMoDL.git")

Check back here often for new updates of the package!

**Usage**

See package documentation. One uses the function `fitFluMoDL()` to fit the model on the available
surveillance data, then gives the fitted model object to `attrMort()` to calculate attributable 
mortalities (to influenza, temperature and -optionally- RSV). Check the help pages of these two
functions for details. In order to try these out, the package also includes some example
surveillance data from Greece (see `?greece`).

**References**

* Lytras T, Pantavou K, Mouratidou E, Tsiodras S. Mortality attributable to seasonal influenza in Greece, 2013 to 2017: variation by type/subtype and age, and a possible harvesting effect. 
[Euro Surveill](https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2019.24.14.1800118) 2019;24(14):pii=1800118
* Gasparrini A, Armstrong B, Kenward MG. Distributed lag non-linear models.
[Stat Med](https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.3940) 2010;29(21):2224–34. 
* Gasparrini A, Leone M. Attributable risk from distributed lag models.
[BMC Med Res Methodol](https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-14-55) 2014;14:55.
* Gasparrini A, Armstrong B, Kenward MG. Multivariate meta-analysis for non-linear and other
multi-parameter associations. [Stat Med](https://onlinelibrary.wiley.com/doi/full/10.1002/sim.5471)
2012;31(29):3821–39. 


