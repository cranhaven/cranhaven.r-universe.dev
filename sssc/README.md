<!-- README.md is generated from README.Rmd. Please edit that file -->
sssc
====

The goal of sssc is to detect whether a sample with variant information is contaminated by another sample from the same species.

Example
-------

This is a basic example which shows you how to detect whether vcf\_example is contaminated:

``` r
## basic example code
library('sssc')
data(vcf_example)
result <- sssc(file = vcf_example)
print(result$stat)
#>               Name       LOH       HomVar     HetVar  HomRate   HighRate
#> 1 sssc_test.vcf.gz 0.7248322 0.0001565125 0.02757586 0.536965 0.05350195
#>     HetRate    LowRate    AvgLL
#> 1 0.3608949 0.04669261 -2.01978
print(result$result)
#>               Name Class Regression
#> 1 sssc_test.vcf.gz     1  0.7131992
```

Given class = 1, vcf\_example is considered to be contaminated.
