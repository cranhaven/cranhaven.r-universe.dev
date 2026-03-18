### Changes in PowerUpR v1.1.0
- MRSS functions for designs with fixed blocks find number of clusters per block rather than number of blocks
- shorter function names, e.g., `mdes.cra3()` <= `mdes.cra3r3()`, `mdes.bira3()` <= `mdes.bira3r1()` (still accommodates older function names)
- added `mod211` and `mod212` functions (Dong, Kelcey, \& Spybrook, 2021)
- added `power.med331()` and `power.med311()` functions (Kelcey, Xie, Spybrook, \& Dong, 2020)
- added `power.rep()` and `mdh.rep()` functions (Hedges \& Schauer, 2019)
- added partially nested designs (Kelcey, Bai, \& Xie, 2020; Lohr, Schochet, \& Sanders, 2014)
- added optional effect size variability argument to blocked designs 

### Changes in PowerUpR v1.0.4
- added 3-2-1 mediation
- fixed minor typos

### Changes in PowerUpR v1.0.3
- improved plots
- fixed minor typos

### Changes in PowerUpR v1.0.2
- fixed minor typos

### Changes in PowerUpR v1.0.1
 - changed plots to accommodate confidence intervals for MDES
 - removed experimental MDES functions for mediation effects
 - fixed minor typos, error handling
 
### Changes in PowerUpR v1.0.0
 - added moderation designs for two- and three-level cluster randomized trials
 - added mediation designs for two-level cluster randomized trials
 - fixed a bug in `.error.handler()`. We thank Dr. Thomas J. Leeper for bringing the bug to our attention and suggesting evaluating arguments before validation process
 - re-defined the argument `rho` to avoid confusion. We thank Dr. Benjamin Nagengast for bringing this issue to our attention
 
### Changes in PowerUpR v0.2.3
 - documented functions by designs
 - efficiency improvements, better error handling
 - removed constrained optimal sample allocation functions. See [**cosa**](https://CRAN.R-project.org/package=cosa) package
 - simplified MRSS functions
 - fixed a bug in `t1t2.error()` plotting function. We thank Dr. Eric. C. Hedberg for bringing the bug to our attention.
 
### Changes in PowerUpR v0.1.3
 - added citation info, vignettes, some object conversion functions 
 - changed title 
 - fixed minor typos
 
