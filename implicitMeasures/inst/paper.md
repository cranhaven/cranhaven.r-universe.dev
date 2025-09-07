---
title: 'Implicit measures with reproducible results: The implicitMeasures package'
authors:
- affiliation: '1'
  name: Ottavia M. Epifania
  orcid: 0000-0001-8552-568X
- affiliation: '1'
  name: Pasquale Anselmi
  orcid:  0000-0003-2982-7178
- affiliation: '1'
  name: Egidio Robusto
date: ""
output: 
  html_document:
       keep_md: true
  pdf_document: default
bibliography: paper.bib
tags:
- R
- Social Pscyhology
- Implicit Social Cognition
- Implicit Association Test
- Single Category Implicit Association Test
- Replicability
affiliations:
- index: 1
  name: Department of Philosophy, Sociology, Education and Applied Psychology, University
    of Padova (IT)
---

# Statement of need

In the past few decades, there has been a growing interest in the possibility of assessing people's attitudes, preferences, self-esteem, opinions, and other social-psychological constructs, without directly asking them. This was made possible by the advent of what are called implicit measures. Implicit measures are generally based on the speed and accuracy with which respondents perform the same categorization task in contrasting conditions. 
The assumption underlying their functioning is that respondents' performance will be faster and more accurate in the condition that is consistent with their attitudes, opinions, or preferences.
The construct of interest is inferred from the difference in the response times between the associative conditions.

Among implicit measures, the Implicit Association Test [IAT; @Greenwald1998] and the Single Category IAT [SC-IAT; @karpinski2006] are the mostly common used ones [@review].
 Both tests result in a differential score (the so-called *D-score*) expressing respondents’ bias in performing the categorization task between conditions. While the scoring of the SC-IAT is based on one single algorithm [@karpinski2006], six different algorithms are available for computing the IAT *D-score* [@Greenwald2003]. The core procedure for the computation of the IAT *D-score* is the same for all the algorithms, which differentiate themselves according to their treatment for extreme fast responses and for the replacement of error responses. 

Although many R packages exist for computing IAT *D-score* algorithms, no packages exist for scoring the SC-IAT. Additionally, the majority of existing R packages created for the computation of IAT *D-score* algorithms do not provide all the available algorithms. The packages that allow for the computation of multiple *D-score* algorithms either do not offer the chance to compare their results, or do not disambiguate which specific algorithm they are computing, raising reproducibility issues [@ellithorpe2015]. 

Recently, a Web Application was developed  with `shiny` package [@shiny] for computing the IAT *D-score* [i.e., *DscoreApp*; @dscoreapp]. This app provides an intuitive and easy to use User Interface. By giving a detailed explanation of the *D-score* algorithms that can be computed, *DscoreApp* addresses the majority of the above mentioned replicability issues. Moreover, the graphical representation of the results can give an immediate glimpse of the general performance of the respondents. However, *DscoreApp* presents some shortcomings as well. Firstly, since it is a shiny app, it is associated with the most significant outstanding issue of shiny apps, namely, the replicability of the code, and hence of the results. Specifically, by putting the code into the shiny interface, it is impossible to call it from the command line, and this point is crucial for replication and automation. However, @dscoreapp used a [GitHub](https://github.com/OttaviaE/DscoreApp) repository to let the public access the code used for the computation. Despite the fact that the graphical representations of the results provided by *DscoreApp* are really useful for getting a first idea of the IAT results and that they are all downloadable in a .pdf format, they cannot be further customized by the users. Moreover, *DscoreApp* computes the *D-score* only for the IAT. 

`implicitMeasures` package is an `R` package aimed at overcoming both the shortcomings of the existing `R` packages for the computation of the IAT *D-score* and those of the shiny app *DscoreApp*.
`implicitMeasures` provides an easy and open source way to clean and score both the IAT and the SC-IAT, to easily compare different IAT *D-score* algorithms, and to provide clear and customizable plots. Plot functions are all based on `ggplot2` [@ggplot2].

# Package overview

The released version of `implicitMeasures` can be installed from [CRAN](https://cran.r-project.org/web/packages/implicitMeasures/index.html): 


```r
install.packages("implicitMeasures")
```

while the development version can be installed from [GitHub](https://github.com/OttaviaE/implicitMeasures):


```r
# install.packages("devtools") # un-comment to install devtools
devtools::install_github("OttaviaE/implicitMeasures")
```

The packages on which `implicitMeasures` depends are listed in the `DESCRIPTION` file. If they are not already installed, they will be installed when `implicitMeasures` package is installed. 

`implicitMeasures` contains the following functions: 

- `clean_iat()`: Prepare and clean IAT data
- `clean_sciat()`: Prepare and clean SC-IAT data
- `compute_iat()`: Compute IAT *D-score*
- `compute_sciat()`: Compute SC-IAT *D-score*
- `descript_d()`: Print descriptive table of *D-score*s (also in LaTeX)
- `d_density()`: Plot either IAT or SC-IAT scores (distribution)
- `d_point()`: Plot either IAT or SC-IAT scores (points)
- `IAT_rel()`: Compute IAT reliability 
- `multi_dsciat()`: Plot scores resulting from two SC-IATs
- `multi_dscore()`: Compute and plot multple IAT *D-score*s
- `raw_data()`: Example data set

Detailed explanations of the use of each function are provided in the [package manual](https://cran.r-project.org/web/packages/implicitMeasures/implicitMeasures.pdf).
The `raw_data` object is a data set included in the package. This data set has been used for all the examples provided in both the package documentation and all the vignettes. The data set contains data from one IAT for the assessment of the preference for Dark or Milk Chocolate (Chocolate IAT), a SC-IAT for the implicit assessment of the positive/negative evaluation of Dark Chocolate (Dark SC-IAT), and a SC-IAT for the implicit assessment of the positive/negative evaluation of Milk chocolate (Milk SC-IAT) [see: @epifania2020 for further details]. 



`implicitMeasures` contains three vignettes, namely "implicitMeasures", "IAT-example", and "SC-IAT-example". Vignette ["implicitMeasures"](https://cran.r-project.org/web/packages/implicitMeasures/vignettes/implicitMeasures.html) contains information regarding both the IAT and the SC-IAT, the computation of their respective scoring algorithms, as well as an explanation of the dataset (i.e., `raw_data`) included in the package. 
Vignettes ["IAT-example"](https://cran.r-project.org/web/packages/implicitMeasures/vignettes/IAT-example.html) and ["SC-IAT-example"](https://cran.r-project.org/web/packages/implicitMeasures/vignettes/SC-IAT-example.html) provide examples on how to use the package functions for computing the IAT and SC-IAT *D-score*, respectively, for plotting their results, and for obtaining descriptive tables of the results. In the IAT case, an illustration of how to use the function for computing multiple *D-score* algorithms concurrently, as well as for plotting their results, is provided. In the SC-IAT case, also an example of how to use the package for plotting multiple SC-IATs scores in one graph is provided. 

# References

