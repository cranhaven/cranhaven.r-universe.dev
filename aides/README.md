<!-- readme: start -->
<!-- title: start -->
<br>


<table border = "0" width = "100%" align = "left">
  <tr>
    <td>  
    <img src = "https://github.com/EnochKang/RES/blob/main/aides/aides_logo.png?raw=true" align = "left" width = "120" />
    </td>
    <td> 
    <span style="color:darkblue">A</span>dditive <br>
    <span style="color:darkblue">I</span>nformation & <br>
    <span style="color:darkblue">D</span>etails of <br>
    <span style="color:darkblue">E</span>vidence <br>
    <span style="color:darkblue">S</span>ynthesis
    </td>
    <td>
    <H1> R-pckage aides </H1>
    </td>
    <td>
    [![CRAN](https://img.shields.io/cran/v/aides?color=blue&label=CRAN&logo=r&logoColor=skyblue)](https://cran.r-project.org/package=aides) <br>
    [![Date](https://img.shields.io/badge/Date-Apr.08.2024-blue.svg?logo=r&logoColor=skyblue)](https://github.com/EnochKang/RES/blob/main/aides/NEWS.md) <br>
    [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-blue.svg?color=blue&label=Lifecycle&logo=r&logoColor=skyblue)](https://lifecycle.r-lib.org/articles/stages.html#stable) <br>
    [![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg?color=blue&label=Licence&logo=gnu&logoColor=skyblue)](https://www.gnu.org/licenses/gpl-3.0.en.html) <br>
    [![Monthly Downloads](https://cranlogs.r-pkg.org:443/badges/aides?color=orange)](https://cranlogs.r-pkg.org:443/badges/aides) <br>
    [![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/aides?color=orange)](https://cran.r-project.org/package=aides)    
    </td>
  </tr>
</table>

<span style="color:white">.</span>

<!-- title: end -->
<!-- content: start -->

<br>

## Table of content:
 - [About aides](#about-aides)
 - [Features](#features) <br>
 - [Dependencies & installation](#dependencies-and-installation)    - [Flow and functions](#flow-and-functions)
 - [Examples](#examples)
 - [Coding conventions](#coding-conventions)
 - [License](#license)
 - [To do list](#to-do-list)

<!-- content: end -->
<!-- about: start -->

<br>

## About *aides*

*aides* is an **R** package which is planned to support users to do additional analysis or graphics of evidence synthesis. Essentially, package *aides* serves as an aiding toolkit for pooled analysis of aggregated data, crafted with a vision to support a more inclusive and informed approach to evidence-based decision-making; and it is developed with values of flexibility, ease of use, and comprehensibility. Package *aides* will be updated with advances of methodology of data synthesis and evidence evaluation. 

The initial goals of package *aides* are to simplify analysis process for both professionals and public users, and to support them in navigating the complexities of synthesized evidence. Long-term goals of package aides are to support knowledge translation and decision-making based on the obtained information with comprehensive understanding of the evidence.

<!-- content: end -->
<!-- features: start -->

<br>

## Features

Briefly, *aides* currently consists of three tasks as follows:

- **Disparity:** a newly proposed assumption regarding disparities in sample size analysis.

- **Discordance:** a newly proposed assumption regarding discordance in rank of study size analysis.

- **Sequential analysis:** a method to examine the sufficiency of information size.

<!-- features: end -->
<!-- dependencies and installation: start -->

<br>

## Dependencies and installation

[![Dependencis](https://tinyverse.netlify.com/badge/aides)](https://cran.r-project.org/package=aides) 

Package *aides* depends on various packages, and is developed using [**R (version 4.2.2)**](https://cran.r-project.org/bin/windows/base/old/4.2.2/). Therefore, those packages are concurrently installed with package *aides*. The dependencies are listed as follows:

-  [*boot*](https://cran.r-project.org/package=boot) (**note:** package *aides* is developed using [*boot* **version 1.3-28**](https://cran.r-project.org/src/contrib/Archive/boot/boot_1.3-28.tar.gz))

- [*metafor*](https://cran.r-project.org/package=metafor) (**note:** package *aides* is developed using [*metafor* **version 4.4-0**](https://cran.r-project.org/package=metafor))

- [*meta*](https://cran.r-project.org/package=meta) (**note:** package *aides* is developed using [*meta* **version 7.0-0**](https://cran.r-project.org/package=meta))

<br>

Formal released package *aides* can be installed from [CRAN](https://cran.r-project.org/package=aides) via R with following syntax:

```{r}
install.packages("aides")
```

<!-- dependencies and installation: end -->
<!-- flow and functions: start -->

<br>

## Flow and functions

[![Functions](https://img.shields.io/badge/Functions-8-green.svg?logo=r&logoColor=green)](https://drive.google.com/file/d/1gxw_mhdxThBs28MyEf8W5Oq6PcYfJYJW/view?usp=sharing)

Users can import their data and do relevant tests or graphics using functions in package *aides*. The present package consists of seven functions listed as follows. 

- **Disparity:**  `PlotDistrSS()`, `TestDisparity()`, and `PlotDisparity()`.

- **Discordance:** `TestDiscordance()`.  
  
- **Sequential analysis:**  `DoSA()`. `DoOSA()`, `PlotOSA()`, and `PlotPower()`.

<br>

#### Disparity:

- **Step 1.** Build or load data.

- **Step 2.** Do disparity test using function `TestDisparity()`.

- **Optional** Illustrate user-defined disparity plot using function `PlotDisparity()`.

<br>

#### Discordance:

- **Step 1.** Build or load data.

- **Step 2.** Do discordance test using function `TestDiscordance()`.

<br>

#### Sequential analysis:

- **Step 1.** Build or load data.

- **Step 2.** Do sequential analysis using function `DoSA()` or function `DoOSA()`.

- **Step 3.** Illustrate user-defined observed sequential analysis plot using function `PlotOSA()`.

<!-- flow and functions: end -->
<!-- examples: start -->

<br>

## Examples

#### 1. Disparity test

The following syntax demonstrates how user can carry out disparity test (example of the study by Olkin 1995 in package *meta*).

##### Test assumption of discordance in study size

```{r}
output <- output <- TestDisparity(n = n,
                                  data = dataOlkin1995,
                                  study = author,
                                  time = year)
```

Then, the returns are listed as follows:

```{r}
#> Summary of disparities in sample size test:
#> Number of outliers = 13 (Excessive cases = 36509; P-value < 0.001)
#> Variability = 3.658 (P-value < 0.001)
#> 
#> Outlier detection method: MAD
#> Variability detection method: CV
```

Users could simply visualize disparity using argument `TRUE` for parameter `plot` in the function `TestDisparity()`. If users would like to modify the figure more, they can further put output of the function `TestDisparity()` into function `PlotDisparity()`. The default of the function `PlotDisparity()` is to illustrate outlier-based disparity. Variability-based disparity could be generated with argument `CV` for parameter `which` in the function `PlotDisparity()`.

<br>

#### 2. Discordance test

The following syntax demonstrates how user can carry out discordance test (example of the study by Fleiss 1993 in package *meta*).

##### Test assumption of discordance in study size

```{r}
output <- TestDiscordance(n = n, 
                          se = se, 
                          study = study,
                          data = data)
```

Then, the returns are listed as follows:

```{r}
#> Summary of discordance in ranks test:
#>  Statistics (Bernoulli exact): 2
#>  P-value: 0.423
#>  Note: No significant finding in the test of discordance in study size ranks.
```

Users could simply visualize discordance using argument `TRUE` for parameter `plot` in the function `TestDiscordance()`.

<br>

#### 3. Sequential analysis

The following syntax demonstrates how user can carry out sequential analysis (example of the study by Fleiss 1993 in package *meta*)

```{r}
DoSA(Fleiss1993bin, 
     source = study, 
     time = year,
     r1 = d.asp, 
     n1 = n.asp, 
     r2 = d.plac, 
     n2 = n.plac, 
     measure = "RR",
     PES = 0.1,
     RRR = 0.2,
     group = c("Aspirin", "Placebo"))
```

Then, the returns are listed as follows:

```{r}
#> Summary of sequential analysis (main information)
#>  Acquired sample size: 28003
#>  Required sample size (heterogeneity adjusted): 21344
#>  Cumulative z score: -2.035
#>  Alpha-spending boundary: 1.711 and -1.711
#>  Adjusted confidence interval is not necessary to be performed.
#> 
#> Summary of sequential analysis (additional information)
#>  1. Assumed information
#>  1.1. Defined type I error: 0.05
#>  1.2. Defined type II error: 0.2
#>  1.3. Defined power: 0.8
#>  1.4. Presumed effect: 0.025
#>       (risks in group 1 and 2 were 9.87315825% (expected) and 12.34144781% respectively; RRR = 0.2)
#>  1.5. Presumed variance: 0.101
#> 
#>  2. Meta-analysis
#>  2.1. Setting of the meta-analysis
#>  Data were pooled using inverse variance approach in random-effects model with DL method.
#>  2.2. Result of the meta-analysis 
#>  Log RR: -0.113 (95% CI: -0.222 to -0.004)
#>  
#>  3. Adjustment factor 
#>  The required information size is calculated with adjustment factor based on diversity (D-squared). Relevant parameters are listed as follows.
#>  3.1. Heterogeneity (I-squared): 39.6%
#>  3.2. Diversity (D-squared): 76%
#>  3.3. Adjustement factor: 4.103
```

Users could simply visualize the result of sequential analysis using argument `TRUE` for parameter `plot` in the function `DoSA()`.

<!-- examples: end -->
<!-- coding conventions: start -->

<br>

## Coding conventions

There are some rules for version numbering in package *aides* (June 20, 2023). Basically, version number consists of three integers with a period between them (eg. version 1.0.0).

1. Updating the first integer refers to package update(s) with new methodological impact.

2. Changing the second integer refers to package update(s) with new function(s) without new methodological impact.

3. Updating the third integer refers to formal modification(s) of existed function(s).

<br>

This package is mainly written according to [Google's R style](https://web.stanford.edu/class/cs109l/unrestricted/resources/google-style.html). For readers, details of naming rules are listed as follows:

1. **.R file** is named using lower case with underscore "_" between words (*e.g. test_disparity.R*). 

2. **function** is named using verb or verb with noun, and the first character of each word is written in capital letter (e.g. `TestDiscordance()`).

3. **object** is named using noun with the first word in lower case, but the first character of rest words is written using capital letter (e.g. `dataCases`).

4. **variable** is named using noun written in lower case. Words of variable name are separated by "." if a variable name consists of more than two words (e.g. `dataDiSS$w.normality`).

<br>

Common-used prefix in package *aides* are listed as follows:

1. **`angl...`**   refers angle of text.

2. **`clr...`**    refers to color.

3. **`lgc...`**    refers to logic value.

4. **`szFnt...`**  refers to font size.

5. **`szLn...`**   refers to line width.

6. **`szPnt...`**  refers to point size.

7. **`txt...`**    refers text (string).

8. **`typLn...`**  refers type of line.

9. **`typPnt...`** refers type of point.

<!-- coding conventions: end -->
<!-- license: start -->

<br>

## License

This package is licensed under the [GPL-3 License](https://cran.r-project.org/web/licenses/GPL-3).

This package is developing by [Enoch Kang](https://orcid.org/0000-0002-4903-942X).

<!-- license: end -->
<!-- to do list: start -->

<br>

## To do list

Task force will keep update package *aides* for relevant issues.

<!-- to do list: end -->
