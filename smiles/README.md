<!-- readme: start -->
<!-- title: start -->
<br>

<table border = "0" width = "100%" align = "left">
  <tr>
    <td align = "right">  
    </td>
    <td align = "center">
    <H1> R-package SMILES </H1>
    </td>
    <td>
    </td>
  </tr>
</table>

<span style = "color:white">.</span>

<table border = "0" width = "100%" align = "left">
  <tr>
    <th>
      <span style = "color:blue">S</span>equential 
      <span style = "color:blue">M</span>ethods 
      <span style = "color:blue">I</span>n 
      <span style = "color:blue">L</span>eading 
      <span style = "color:blue">E</span>vidence 
      <span style = "color:blue">S</span>ynthesis <br>
    [![Version](https://img.shields.io/badge/Version-0.1.0-blue.svg?logo=r&logoColor=skyblue)]
    [![Date](https://img.shields.io/badge/Date-Aug.18.2024-blue.svg?logo=r&logoColor=skyblue)]
    [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-blue.svg?color=blue&label=Lifecycle&logo=r&logoColor=skyblue)](https://lifecycle.r-lib.org/articles/stages.html#stable)
    </th>
  </tr>
</table>

<span style = "color:white">.</span>


<!-- title: end -->
<!-- content: start -->

<br>

## Table of content:
 - [About SMILES](#about-SMILES)
 - [Dependencies & installation](#dependencies-and-installation)
 - [Flow and functions](#flow-and-functions)
 - [Examples](#examples)
 - [Coding conventions](#coding-conventions)
 - [License & disclamer](#license-and-disclamer)
 - [To do list](#to-do-list)

<!-- content: end -->
<!-- about: start -->

<br>

## About *SMILES*

*SMILES* is an **R** package which is to provide a useful collection of
functions designed to apply sequential method in data synthesis and evidence
evaluation.

<!-- content: end -->
<!-- features: start -->

<br>

## Dependencies and installation

Package *SMILES* is developed based on various packages using [**R (version 4.2.2)**](https://cran.r-project.org/bin/windows/base/old/4.2.2/). Therefore, those packages are concurrently installed with package *SMILES*. The main dependency of package *SMILES* is package [*meta*](https://cran.r-project.org/package=meta) (**note:** [*meta* **version 7.0-0**](https://cran.r-project.org/package=meta)).

<br>

Formal released package *SMILES* can be installed from CRAN via R with following syntax:

```{r}
install.packages("SMILES")
```

<!-- dependencies and installation: end -->
<!-- flow and functions: start -->

<br>

## Flow and functions

[![Functions](https://img.shields.io/badge/Functions-2-green.svg?logo=r&logoColor=green)]

Users can import their data and do relevant tests or graphics using functions in package *SMILES*. The present package consists of two functions listed as follows. 

- **Trial sequential analysis:**  `DoTSA()`

- **CoRNET plot:**  `PlotCoRNET()`

<br>

#### Steps for squential analysis and visualization:

- **Step 1.** Build or load data.

- **Step 2.** Do sequential analysis using function `DoTSA()`.

- **Step 3.** Illustrate user-defined SMILE plot in terms of the classic trial sequential analysis plot using function `PlotCoRNET()`.

<!-- flow and functions: end -->
<!-- examples: start -->

<br>

## Examples

#### 3. Sequential analysis

The following syntax demonstrates how user can carry out sequential analysis (example of the study by Fleiss 1993 in package *meta*)

```{r}
DoTSA(Fleiss1993bin, 
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

Users could simply visualize the result of sequential analysis using argument `TRUE` for parameter `plot` in the function `DoTSA()`.

<!-- examples: end -->
<!-- coding conventions: start -->

<br>

## Coding conventions

There are some rules for version numbering in package *SMILES* (June 20, 2023). Basically, version number consists of three integers with a period between them (eg. version 1.0.0).

1. Updating the first integer refers to package update(s) with new methodological impact.

2. Changing the second integer refers to package update(s) with new function(s) without new methodological impact.

3. Updating the third integer refers to formal modification(s) of existed function(s).

<br>

This package is mainly written according to [Google's R style](https://web.stanford.edu/class/cs109l/unrestricted/resources/google-style.html). For readers, details of naming rules are listed as follows:

1. **.R file** is named using lower case with underscore "_" between words (*e.g. do_sequential_analysis.R*). 

2. **function** is named using verb or verb with noun, and the first character of each word is written in capital letter (e.g. `DoTSA()`).

3. **object** is named using noun with the first word in lower case, but the first character of rest words is written using capital letter (e.g. `dataCases`).

4. **variable** is named using noun written in lower case. Words of variable name are separated by "." if a variable name consists of more than two words (e.g. `data$RIS`).

<br>

Common-used prefix in package *SMILES* are listed as follows:

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

## License and disclamer

This package is developing by [Enoch Kang](https://orcid.org/0000-0002-4903-942X) and licensed under the [GPL-3 License](https://cran.r-project.org/web/licenses/GPL-3).

<br>

**DISCLAIMER** <br>
Last updated July 26, 2024

- **WEBSITE DISCLAIMER** <br> The information generated by the R-package *SMILES* is for general informational purposes only. All information from the R-package *SMILES* is provided in good faith, however we make no representation or warranty of any kind, express or implied, regarding the adequacy, availability, or completeness of any information from the package. UNDER NO CIRCUMSTANCE SHALL WE HAVE ANY LIABILITY TO YOU FOR ANY LOSS OR DAMAGE OF ANY KIND INCURRED AS A RESULT OF THE USE OF THE PACKAGE OR RELIANCE ON ANY INFORMATION PROVIDED ON THE PACKAGE. YOUR USE OF THE PACKAGE AND YOUR RELIANCE ON ANY INFORMATION ON THE PACKAGE IS SOLELY AT YOUR OWN RISK.

- **PROFESSIONAL DISCLAIMER** <br> The package cannot and does not contain statistics advice. The statistics information is provided for general informational and educational purposes only and is not a substitute for professional advice. Accordingly, before taking any actions based upon such information, we encourage you to consult with the appropriate professionals. We do not provide any kind of statistics advice. THE USE OR RELIANCE OF ANY INFORMATION CONTAINED ON THE PACKAGE IS SOLELY AT YOUR OWN RISK. <br>

<!-- license: end -->
<!-- to do list: start -->

<br>

## To do list

Task force will keep update package *SMILES* for relevant issues.

<!-- to do list: end -->
