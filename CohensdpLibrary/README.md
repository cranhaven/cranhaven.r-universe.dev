
# Cohen’s $d_p$ library: Getting the Cohen’s $d_p$ and its confidence interval in any design

<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/CohensdpLibrary)](https://cran.r-project.org/package=CohensdpLibrary)
<!-- badges: end -->

This library computes the Cohen’s $d_p$ and its confidence intervals in
any experimental design. In the past, researchers developed distinct
versions of standardized mean difference for between and within-subject
design. The consequence is that these various estimators could not be
compared between each others and more importantly, across experimental
design. Lakens (2013) noted the existence of two different measures in
within-subject design, and Westfall (2016) noted the existence of at
least 5 difference sorts of standardized mean difference. He concluded
by making this very important point: all these estimators ARE NOT
Cohen’s d measures.

The measure that J. Cohen (Cohen, 1969) created is obtained from the
mean difference standardized using the pooled standard deviation. Hence,
measures such as $d_a_v$, $d_z$, $d_a$, etc. are not Cohen’s d and
**more importantly**, they cannot be compared! They all return different
values because they measure different things. They are not just
different, they can be markedly different. As an example, a $d_z$, given
the means and standard deviations, can be *smaller* **or** *larger* than
the Cohen’s $d$ depending on the amount of correlation across the pairs
of data.

This whole mess implies lack of comparability and confusion as to what
statistics was actually reported. For that reason, I chose to call the
true Cohen’s $d$ with a distinct subscript $p$, as in $d_p$ so that (i)
we clearly see the difference (the reader is not left guessing what $d$
represents); (ii) is is clear that the pooled standard deviation and
only this statistic was used to standardized the mean difference.
Further, by advocating a unique statistic for standardized mean
difference, it allows for comparisons across studies, whether they used
within-subject or between-subject design.

## Why this package?

`MBESS` is an excellent package which already computes standardized mean
difference and returns confidence intervals (Kelley, 2022). However, it
does not compute confidence intervals in within-subject design directly.
The Algina and Keselman approximate method can be implemented within
MBESS with some programming (Cousineau & Goulet-Pelletier, 2021). This
package, on the other hand, can be used with any experimental design. It
only requires an argument `design` which specifies the type of
experimental design.

The confidence interval in within-subect design was unknown until
recently. In recent work (Cousineau, 2022, submitted), its exact
expression was found when the population correlation is know and an
approximation was proposed when the sample correlation is known, but not
the population correlation.

# Using `CohensdpLibrary`

You can install this library on you computer from CRAN (note the
uppercase C and uppercase L)

``` r
install.packages("CohensdpLibrary")
```

or if the library devtools is installed with:

``` r
devtools::install_github("dcousin3/CohensdpLibrary")
```

and before using it:

``` r
library(CohensdpLibrary)
```

The main function is `Cohensdp`, which returns the Cohen’s $d_p$ and its
confidence intervals under various designs. For example, this returns
the triplet (lower 95% confidence interval bound, $d_p$, upper 95%
confidence interval bound) given the sample means, the sample standard
deviations, and the correlation

``` r
Cohensdp( statistics = list(m1=76, m2=72, n=20, s1=14.8, s2=18.8, r=0.2),
          design = "within",
          method = "adjustedlambdaprime"
)
```

    ## [1] -0.3422415  0.2364258  0.8025925

You get a more readable output with `summarize`, e.g.,

``` r
summarize(Cohensdp( statistics = list(m1=76, m2=72, n=20, s1=14.8, s2=18.8, r=0.2),
                    design = "within",
                    method = "adjustedlambdaprime"
))
```

    ## Cohen's dp         = 0.236
    ##   95.0% Confidence interval = [-0.342, 0.803]

The design can be replaced with `between` for a between-subject design:

``` r
summarize(Cohensdp( statistics = list(m1=76, m2=72, n1=10, n2=10, s1=14.8, s2=18.8),
                    design = "between")
)
```

    ## Cohen's dp         = 0.236
    ##   95.0% Confidence interval = [-0.647, 1.113]

(the statistic `r` is removed as there is no correlation in
between-group design, and `n` is provided separately for each group,
`n1` and `n2`).

Finally, it is also possible to get a Cohen’s $d_p$ from a single group
as long as you have an hypothetical mean `m0` to compare the sample mean
to, e.g.,

``` r
summarize(Cohensdp( statistics = list(m=76, m0=72, n=20, s=14.8),
                    design = "single")
)
```

    ## Cohen's dp         = 0.270
    ##   95.0% Confidence interval = [-0.180, 0.713]

Replace `summarize` with `explain` for additional information on the
result.

Check the web site <https://github.com/dcousin3/CohensdpLibrary> for
more. also, `help(CohensdpLibrary)` will get you started.

# References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0" line-spacing="2">

<div id="ref-c69" class="csl-entry">

Cohen, J. (1969). *Statistical power analysis for the behavioral
sciences*. Academic Press.

</div>

<div id="ref-c22a" class="csl-entry">

Cousineau, D. (2022). *The exact distribution of the Cohen’s $d_p$ in
repeated-measure designs*. PsyArXiv.
<https://doi.org/10.31234/osf.io/akcnd>

</div>

<div id="ref-c22b" class="csl-entry">

Cousineau, D. (submitted). The exact confidence interval of the Cohen’s
$d_p$ in repeated-measure designs. *The Quantitative Methods for
Psychology*.

</div>

<div id="ref-CG057-1" class="csl-entry">

Cousineau, D., & Goulet-Pelletier, J.-C. (2021). A study of confidence
intervals for Cohen’s dp in within-subject designs with new proposals.
*The Quantitative Methods for Psychology*, *17*, 51–75.
<https://doi.org/10.20982/tqmp.17.1.p051>

</div>

<div id="ref-k22" class="csl-entry">

Kelley, K. (2022). *MBESS: The MBESS R package*. Retrieved from
<https://CRAN.R-project.org/package=MBESS>

</div>

<div id="ref-L0003-1" class="csl-entry">

Lakens, D. (2013). Calculating and reporting effect sizes to facilitate
cumulative science: A practical primer for t-tests and ANOVAs.
*Frontiers in Psychology*, *4*, 1–12.
<https://doi.org/10.3389/fpsyg.2013.00863>

</div>

<div id="ref-w16" class="csl-entry">

Westfall, J. (2016). Five different “Cohen’s $d$” statistics for
within-subject designs.

</div>

</div>
