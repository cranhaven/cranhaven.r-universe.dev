
<!-- README.md is generated from README.Rmd. Please edit that file -->

# factorial2x2

<!-- badges: start -->

<!-- badges: end -->

The goals of the `factorial2x2` package are twofold: First, to provide
power calculations for a two-by-two factorial design in which the
effects of the two factors may be sub-additive. Power is provided for
the overall effect test for as well as the multiple testing procedures
described in Leifer, Troendle, Kolecki, and Follmann (2020). Second, to
analyze two-by-two factorial trial data which may include baseline
adjustment covariates. Further details are described in the factorial2x2
vignette.

## Installation

You can install the released version of factorial2x2 from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("factorial2x2")
```

## Example of a power calculation

We reproduce the power calculations for scenario 4 from Table 2 in
Leifer, Troendle, et al. using the `fac2x2design` function.

``` r

  n <- 4600          # total sample size
  rateC <- 0.0445    # one year event rate in the control group
  hrA <- 0.80        # simple A effect hazard ratio
  hrB <- 0.80        # simple B effect hazard ratio
  hrAB <- 0.72       # simple AB effect hazard ratio
  mincens <- 4.0     # minimum censoring time in years
  maxcens <- 8.4     # maximum censoring time in years
  fac2x2design(n, rateC, hrA, hrB, hrAB, mincens, maxcens, dig = 2, alpha = 0.05)
  
$events
[1] 954.8738         # expected number of events
        
$evtprob             # event probabilities for the C, A, B, and AB groups, respectively
    probC     probA     probB    probAB 
0.2446365 0.2012540 0.2012540 0.1831806 

$powerEA3overallA    
[1] 0.5861992        # Equal Allocation 3's power to detect the overall A effect

$powerEA3simpleA    
[1] 0.5817954        # Equal Allocation 3's power to detect the simple A effect

$powerEA3simplAB    
[1] 0.9071236        # Equal Allocation 3's power to detect the simple AB effect

$powerEA3anyA
[1] 0.7060777        # Equal Allocation 3's power to detect either the overall A or simple A effects

$powerPA2overallA
[1] 0.6582819        # Proportional Allocation 2's power to detect the overall A effect

$powerPA2simpleAB
[1] 0.9197286        # Proportional Allocation 2's power to detect the simple AB effect

$powerEA2simpleA
[1] 0.6203837        # Equal Allocation 2's power to detect the simple A effect

$powerEA2simpleAB
[1] 0.9226679        # Equal Allocation 2's power to detect the simple AB effect

$powerA
[1] 0.7182932        # power to detect the overall A effect at the two-sided 0.05 level

```

## References

Leifer, E.S., Troendle, J.F., Kolecki, A., Follmann, D. Joint testing of
overall and simple effect for the two-by-two factorial design. 2020.
Submitted.

Lin, D-Y., Gong, J., Gallo, P., et al. Simultaneous inference on
treatment effects in survival studies with factorial designs.
*Biometrics*. 2016; 72: 1078-1085.

Slud, E.V. Analysis of factorial survival experiments. *Biometrics*.
1994; 50: 25-38.
