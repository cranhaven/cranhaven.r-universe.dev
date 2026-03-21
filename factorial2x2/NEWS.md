---
title: "NEWS.md"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# factorial2x2 0.2.0

# minor changes

The associate editor who reviewed our journal paper asked that we change the names
of the multiple testing procedures in our paper.  As a consequence,

1. The function power13.13.13 is now powerEA3.
2. The function power23.13 is now powerPA2.
3. The function power12.12 is now powerEA2.

# factorial2x2 0.1.0

The goals of the `factorial2x2` package are twofold: First, to provide
power calculations for a two-by-two factorial design in which the
effects of the two factors may be sub-additive. Power is provided for
the overall effect test for as well as the multiple testing procedures
described in Leifer, Troendle, Kolecki, and Follmann (2020). Second, to
analyze two-by-two factorial trial data which may include baseline
adjustment covariates. Further details are described in the factorial2x2
vignette.
