# Changes of package aides



## aides 1.3.3. (release version)
### Date: April 08, 2024

**Modify document:** improve Vignettes file of package *aides* with much more information regarding sequential analysis.

** Note:** functions `DoSA()` and `DoOSA()` work as argument `IV` for parameter `poolProp` with package *lme4* 1.1-35.2. Users have to downgrade package *lme4* to version 1.1-35 if they would like to run functions `DoSA()` and `DoOSA()` with argument `GLMM` for parameter `poolProp`. 

**Release:** submit *aides* 1.3.3 to the CRAN.

<br>

### Date: March 31, 2024

**Modify function:** improve function `DoSA()` by introducing two new parameters, including `trnsfrm` and `poolProp`. The two parameters support users to obtain proportion of risk in each group using different method. Parameter `trnsfrm` is added for allowing users to calculate proportion of risk with transformation by log, logit, arcsine, or double arcsine approach. Parameter `poolProp` enables users to pool proportion of risk using inverse variance method and generalized linear mixed model.

**Modify function:** improve function `DoOSA()` by introducing two new parameters, including `trnsfrm` and `poolProp`. The two parameters support users to obtain proportion of risk in each group using different method. Parameter `trnsfrm` is added for allowing users to calculate proportion of risk with transformation by log, logit, arcsine, or double arcsine approach. Parameter `poolProp` enables users to pool proportion of risk using inverse variance method and generalized linear mixed model.

<br>


## aides 1.3.2. (release version)
### Date: March 07, 2024

**Modify document:** improve Vignettes file of package *aides* with much more information regarding sequential analysis.

**Release:** submit *aides* 1.3.2 to the CRAN.

<br>

### Date: February 27, 2024

**Modify function:** improve function `DoSA()` by introducing two new parameters, including `smooth` and `SAP`. Parameter `smooth` is added for allowing users to generate sequential plot with smooth boundaries, and parameter `SAP` enables users to calculate sequential-adjusted power. Pooling method was added on the plot during this modification. Besides, scale on the x axis scale was limited for reducing plot size.

**Modify function:** improve function `DoOSA()` by introducing a new parameter, `anchor`, enabling users to specify anchor-based minimal important difference in order to estimate required information size. Pooling method was added on the plot during this modification.

**Modify function:** improve function `PlotOSA()` by introducing a new parameter, `lgcZone`, enabling users to show six zones of finding. Pooling method was also added on the plot during this modification.

<br>


## aides 1.3.1. (release version)
### Date: February 3, 2024

**Modify function:** improve function `DOSA()`, `DoOSA()`, and `PlotOSA()` for avoiding the overlapping between OIS information and legend on the plot.

**Modify function:** improve function `DoSA()` by introducing a new parameter, `RRR`, enabling users to specify a pre-assumed relative risk reduction for estimation of required information size. This parameter is designed exclusively for dichotomous outcomes and replaces the argument of `PES` parameter.

**Modify document:** improve Vignettes file of package *aides* with much more information regarding sequential analysis.

**Modify document:** improve README file of package *aides* with appropriate plot size.

**Release:** submit *aides* 1.3.1 to the CRAN.

<br>


## aides 1.3.0. (release version)
### Date: December 21, 2023

**Modify document:** improve package *aides* vignette by illustrating an example for the function `PlotPower()`, and ensuring consistency by utilizing the same data set to illustrate examples for the functions `DoSA()`, `DoOSA()`, and `PlotOSA()`.

**Modify document:** improve package *aides* manual by introducing a detailed section for the `PlotPower()` function, and ensuring consistency by utilizing the same data set to illustrate examples for the functions `DoSA()`, `DoOSA()`, and `PlotOSA()`.

**Release:** submit *aides* 1.3.0 to the CRAN.

<br>


### Date: December 15, 2023

**Add function:** `PlotPower()` has been added in the package *aides* to visualize sequential-adjusted power.

<br>


## aides 1.2.3. (development version)
### Date: December 08, 2023

**Modify function:** improve function `DoOSA()` by setting argument `"D2"` as default for parameter `adjust` and calculating sequential-adjusted power. 

**Modify function:** improve function `DoSA()` by setting argument `"D2"` as default for parameter `adjust`.

**Modify function:** improve function `PlotOSA()` for better visualization.

<br>


## aides 1.2.2. (development version)
### Date: November 03, 2023

**Modify function:** improve function `PlotDistrSS()` with parameters `method` for indicating normality test method in terms of Shapiro test or Kolmogorov-Smirnov test.

<br>


## aides 1.2.1. (development version)
### Date: October 31, 2023

**Modify function:** improve function `TestDisparity()` with parameters for user-defined cutoff value of proportion of excessive cases in outlier-based disparity test, and for user-defined bootstrap replications for obtaining probability in variability-based disparity test.

> 1. parameter `ctf` with a numeric argument can be used for determining the threshold for proportion of excessive cases in outlier-based disparity test.
>
> 2. parameter `rplctns` with an integer argument can be used for assigning bootstrap replications for obtaining probability of variability-based disparity test.
>

<br>


## aides 1.2.0. (release version)
### Date: October 26, 2023

**Release:** submit *aides* 1.2.0 to the CRAN.

<br>

### Date: October 24, 2023

**Add function:** `PlotOSA()` has been added in the package *aides* to visualize observed sequential analysis.

**Add function:** `DoOSA()` has been added in the package *aides* to facilitate sequential method employing observed data, thereby distinguishing it from information derived from prospective planning.

**Add function:** `PlotDistrSS()` has been added in the package *aides* to assist decision on the method selection for outlier detection and variability in disparity test.

<br>


## aides 1.1.6. (development version)
### Date: October 05, 2023

**Modify document:** improve package *aides* manual by adding details section for function `DoSA()`.

<br>


## aides 1.1.5. (development version)
### Date: September 28, 2023

**Modify function:** improve function `DoSA()` for showing information of relative risk reduction.

<br>


## aides 1.1.4. (development version)
### Date: September 20, 2023

**Modify function:** improve function `PlotDisparity()` for fixing bug of labeling proportion of excessive cases.

**Modify document:** improve package *aides* vignette.

<br>


## aides 1.1.3. (development version)
### Date: September 08, 2023

**Modify function:** improve function `TestDisparity()` with robust Coefficient of Variations.

**Modify document:** improve package *aides* manual by detailing returned values from function `TestDisparity()`.

<br>


## aides 1.1.2. (development version)
### Date: August 30, 2023

**Modify function:** improve function `TestDisparity()` with default method for outlier detection according to distribution.

<br>


## aides 1.1.1. (development version)
### Date: August 17, 2023

**Modify function:** improve function `PlotDisparity()` with parameters for legend information, user-defined color of the association line between standard deviation and sample size on disparity plot (variability), and angle of study labels on disparity plot (outlier).

> 1. parameter `lgcDtls` with logic value `TRUE` or `FALSE` can be used for determining whether to show details of disparity test on the plot.
>
> 2. parameter `txtLbl` with argument `n`, `n.excessive`, or `prop.excessive` can be used for showing study information of each observed point on disparity plot (outlier).
>
> 3. parameter `szFntEC` with a numeric value between 0 and 5 can be used for setting font size of study label on axis X for those studies with excessive cases.
>
> 4. parameter `szFntLbl` with numeric value(s) between 0 and 5 can be used for setting font size of observed values.
>
> 5. parameter `szFntLblEC` with a numeric value between 0 and 5 can be used for setting font size of observed value(s) among those studies with excessive cases.
>
> 6. parameter `clrLnCV` with a color name can be used for changing line of the association between standard deviation and cases on disparity plot (variability).
>
> 7. parameter `clrLbl` with color name(s) can be used for coloring observed values on disparity plot (outlier).
>
> 8. parameter `clrLblEC` with a color name can be used for coloring observed value(s) among those studies with excessive cases on disparity plot (outlier).
>
> 9. parameter `anglAxsX` with a numeric value between 0 and 360 can be used for setting angle of study labels on disparity plot (outlier).
>
> 10. parameter `anglLbl` with a numeric value between 0 and 5 can be used for setting angle of observed values on disparity plot (outlier).
>

<br>


## aides 1.1.0. (release version)
### Date: August 11, 2023

**Add function:** `PlotDisparity()` to illustrate disparity plot.

**Release:** submit *aides* 1.1.0 to the CRAN.

<br>


## aides 1.0.5 (development version)
### Date: July 29, 2023

**Modify function:** improve function `TestDisparity()` with parameters for sorting data and building a data frame for user-defined disparity plot.

> 1. parameter `sort` with options of reference for data sorting (i.e. time, size, and excessive cases).
>
> 2. returning a data frame for user-defined disparity plot by argument `TRUE` for parameter `plot`.
>

<br>


## aides 1.0.4 (development version)
### Date: July 17, 2023

**Modify function:** improve function `TestDisparity()` with summary information.

> 1. displaying the results of outlier detection.
>
> 2. displaying information of outlier detection method.
>
> 3. removing results of normal distribution test from the summary infomation.
>


<br>

**Modify function:** improve function `TestDisparity()` with parameters for data input and outlier detection.

> 1. parameter `data` can be used for importing a data frame for disparity test.
>
> 2. parameter `time` is a vector of study year.
>
> 3. calculation of outlier detection (default with GESD and MAD for normal and non-normal distribution respectively).
>
> 4. parameter `outlier` consists of four outlier detection methods (i.e. IQR, Z, GESD, and MAD).
>

<br>


## aides 1.0.3 (development version)
### Date: July 01, 2023

**Modify function:** improve function `DoSA()` with parameters for sequential analysis.

> 1. parameter `PV` for presumed variance (default with post-hoc analysis mode).
>
> 2. parameter `ref` with options of reference group (i.e. 1 and 2; default with 2).
>
> 3. parameter `pooling` with options of pooling method (e.g. inverse variance, Mantel-Haensze, and Peto; default with IV).
>
> 4. parameter `adjust` with options of adjustment factor (including D-squared, I-squared, and conceptual heterogeneity; default none).
>

<br>


## aides 1.0.2 (development version)
### Date: June 30, 2023

**Modify function:** improve function `DoSA()` with information and displaying appearance on plot.

> 1. information on model of the meta-analysis and method for heterogeneity estimetor.
>
> 2. displaying spending boundaries with continuous curve rather lines between observed fraction points.
>
> 3. displaying cumulative z score with weighted points according to weights of each data source in the meta-analysis.
>
> 4. displaying required information size with red dash line.
>
> 5. displaying labels of each data source using parameter `id = TURE`.
>
> 6. displaying y axis title with inverted favorable direction using parameter `invert = TRUE`.
>
> 7. displaying beta-spending boundaries using parameter `BSB = TRUE`.
>

<br>


## aides 1.0.1 (development version)
### Date: June 25, 2023

**Modify function:** improve function `DoSA()` with structured  summary information as follows:

> *1. information on main outputs of sequential analysis*
>
> 1.1. acquired sample size
>
> 1.2. required sample size
>
> 1.3. cumulative z score
>
> 1.4. alpha-spending boundary
>
> 1.5. adjusted confidence interval (based on alpha-spending boundary)
>
>
> *2. presumed parameters for sequential analysis*
>
> 2.1. probability of type I error.
>
> 2.2. probability of type II error.
>
> 2.3. statistic power.
>
> 2.4. effect size (including risk in each group for outcome with binary data).
>
> 2.5. variance.
>
>
> *3. meta-analysis*
> 
> 3.1. settings for the meta-analysis.
>
> 3.2. result of the meta-analysis.
>
>
> *4. adjustment factor*
>
> 4.1. diversity (D-squared).
>
> 4.2. heterogeneity (I-squared). 
>
> 4.3. value of the adjustment.
>

<br>


## aides 1.0.0 (release version)
### Date: June 20, 2023

**Release:** submit *aides* 1.0.0 to the CRAN.
<br>

### Date: June 15, 2023

**Initiation:** Build package *aides* with first three functions including `TestDisparity()`, `TestDiscordance()`, and `DoSA()`.


> 1. `TestDisparity()` is to test assumption of disparity in sample size.
>
> 2. `TestDiscordance()` is to test assumption of discordance between theoretical and observed study scale.
>
> 3. `DoSA()` is to conduct sequential analysis.
>

<br>


### Writing style of *aides*

This package is mainly written according to Google's R style. For readers, details of naming rules are listed as follows:

> 1. **.R file** is named using lower case with underscore "_" between words (*e.g. test_disparity.R*). 
>
> 2. **function** is named using verb or verb with noun, and the first character of each word is written in capital letter (e.g. `TestDiscordance()`).
>
> 3. **object** is named using noun with the first word in lower case, but the first character of rest words is written using capital letter (e.g. `dataCases`).
>
> 4. **variable** is named using noun written in lower case. Words of variable name are separated by "." if a variable name consists of more than two words (e.g. `dataDiSS$w.normality`).
>

<br>


### Version numbering rule of *aides* (June 20, 2023)

version number consists of three integers with a period between them (eg. version 1.0.0).

> 1. Updating the first integer refers to a modification with new methodological impact.
>
> 2. Changing the second integer refers to an update with a new function without new methodological impact.
>
> 3. Updating the third integer refers to a modification in a function.
>
