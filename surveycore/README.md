<!-- README.md is generated from README.Rmd. Please edit that file -->

# surveycore <a href = "https://jdenn0514.github.io/surveycore/index.html"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/surveycore)](https://CRAN.R-project.org/package=surveycore)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/JDenn0514/surveycore/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JDenn0514/surveycore/actions/workflows/R-CMD-check.yaml)
[![pkgcheck](https://github.com/JDenn0514/surveycore/workflows/pkgcheck/badge.svg)](https://github.com/JDenn0514/surveycore/actions?query=workflow%3Apkgcheck)
[![Codecov test
coverage](https://codecov.io/gh/JDenn0514/surveycore/graph/badge.svg)](https://app.codecov.io/gh/JDenn0514/surveycore)
<!-- badges: end -->

**surveycore** is the foundation of the surveyverse ecosystem — a
modern, tidyverse-compatible replacement for the `survey` and `srvyr`
packages in R.

It provides S7-based survey design objects with:

- A tidy-select interface (`ids = c(psu, ssu)`, no formula syntax)
- Automatic preservation of haven-style variable labels and value labels
- Exact variance estimation (Taylor linearization, replicate weights,
  two-phase designs)
- Seamless conversion to and from `survey::svydesign` and
  `srvyr::tbl_svy`

For a side-by-side comparison with `survey` and `srvyr`, see
`vignette("surveycore-vs-survey")`.

## Installation

    # From CRAN:
    install.packages("surveycore")

    # Development version from GitHub:
    # install.packages("pak")
    pak::pak("JDenn0514/surveycore")

## What surveycore provides

- **S7 survey objects**: `survey_taylor`, `survey_replicate`,
  `survey_twophase`, `survey_nonprob`
- **Constructors**: `as_survey()`, `as_survey_replicate()`,
  `as_survey_twophase()`, `as_survey_nonprob()`
- **Metadata system**: `set_var_label()`, `set_val_labels()`,
  `extract_var_label()`, `extract_val_labels()` — with automatic haven
  attribute import
- **Analysis functions**: `get_freqs()`, `get_means()`, `get_totals()`,
  `get_corr()`, `get_quantiles()`, `get_ratios()`, `get_diffs()`
- **Regression**: `survey_glm()` for survey-weighted GLMs with `clean()`
  for tidy coefficient tables
- **Design utilities**: `update_design()`, `as_svydesign()`,
  `from_svydesign()`, `as_tbl_svy()`, `from_tbl_svy()`

## Who is this for?

surveycore is intended for:

- **Survey researchers and methodologists** who analyse complex
  probability samples and need design-consistent variance estimates
  (stratified, clustered, replicate-weight, and two-phase designs).
- **Social scientists, epidemiologists, and public health researchers**
  working with population surveys such as NHANES, ACS, GSS, or custom
  organizational surveys.
- **R users who want a tidyverse-compatible interface** for the survey
  analysis workflows currently served by `survey` and `srvyr`.

The software is designed to analyse **rectangular survey microdata**:
one row per respondent, numeric or categorical outcome variables, and
either explicit survey weights or a design specification (ids, strata,
FPC). It supports:

- Data frames, tibbles, and data.table objects as input.
- Variables with haven-style variable labels and value labels (e.g. from
  `.xpt` or `.sav` files read with `haven`).
- Grouped analyses (via `surveytidy::group_by()`).

Each analysis function accepts specific types of outcome variables:

<table>
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<thead>
<tr>
<th>Function</th>
<th>Accepts</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>get_freqs()</code></td>
<td>Categorical or coded integer variables</td>
</tr>
<tr>
<td><code>get_means()</code></td>
<td>Numeric variables</td>
</tr>
<tr>
<td><code>get_totals()</code></td>
<td>Numeric variables</td>
</tr>
<tr>
<td><code>get_corr()</code></td>
<td>Pairs of numeric variables</td>
</tr>
<tr>
<td><code>get_quantiles()</code></td>
<td>Numeric variables</td>
</tr>
<tr>
<td><code>get_ratios()</code></td>
<td>Two numeric variables (numerator / denominator)</td>
</tr>
<tr>
<td><code>get_diffs()</code></td>
<td>A categorical grouping variable + one or more numeric outcomes</td>
</tr>
<tr>
<td><code>survey_glm()</code></td>
<td>Numeric or binary response, numeric or categorical predictors</td>
</tr>
</tbody>
</table>

## Basic usage

    library(surveycore)

    # ── Simple SRS design ──────────────────────────────────────────────────────────
    set.seed(42)
    df <- data.frame(
      psu = rep(1:10, each = 10),
      strata = rep(c("A", "B"), each = 50),
      weight = runif(100, 0.5, 2),
      income = rnorm(100, 50000, 10000),
      age = sample(18:80, 100, replace = TRUE)
    )

    d <- as_survey(df, ids = psu, weights = weight, strata = strata, nest = TRUE)
    d
    #> 
    #> ── Survey Design ───────────────────────────────────────────────────────────────
    #> <survey_taylor> (Taylor series linearization)
    #> Sample size: 100
    #> 
    #> # A tibble: 100 × 5
    #>      psu strata weight income   age
    #>    <int> <chr>   <dbl>  <dbl> <int>
    #>  1     1 A       1.87  53219.    42
    #>  2     1 A       1.91  42162.    33
    #>  3     1 A       0.929 65757.    71
    #>  4     1 A       1.75  56429.    41
    #>  5     1 A       1.46  50898.    50
    #>  6     1 A       1.28  52766.    78
    #>  7     1 A       1.60  56793.    55
    #>  8     1 A       0.702 50898.    60
    #>  9     1 A       1.49  20069.    58
    #> 10     1 A       1.56  52849.    39
    #> # ℹ 90 more rows

    # ── Weighted mean and total ────────────────────────────────────────────────────
    get_means(d, income)
    #> # A tibble: 1 × 4
    #>     mean ci_low ci_high     n
    #>    <dbl>  <dbl>   <dbl> <int>
    #> 1 50206. 47921.  52490.   100
    get_totals(d, income)
    #> # A tibble: 1 × 4
    #>      total   ci_low  ci_high     n
    #>      <dbl>    <dbl>    <dbl> <int>
    #> 1 6460063. 5906356. 7013770.   100

## Complex survey designs

    # ── Replicate weights (BRR) ───────────────────────────────────────────────────
    df_rep <- data.frame(
      y = rnorm(20),
      wt = runif(20, 1, 3),
      rep1 = runif(20, 0.5, 2),
      rep2 = runif(20, 0.5, 2),
      rep3 = runif(20, 0.5, 2),
      rep4 = runif(20, 0.5, 2)
    )

    d_rep <- as_survey_replicate(
      df_rep,
      weights = wt,
      repweights = starts_with("rep"),
      type = "BRR"
    )
    d_rep
    #> 
    #> ── Survey Design ───────────────────────────────────────────────────────────────
    #> <survey_replicate> (BRR, 4 replicates)
    #> Sample size: 20
    #> 
    #> # A tibble: 20 × 6
    #>         y    wt  rep1  rep2  rep3  rep4
    #>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    #>  1 -2.00   2.30 1.09  0.849 0.705 1.71 
    #>  2  0.334  2.84 0.619 1.37  0.766 1.90 
    #>  3  1.17   1.73 1.74  1.76  1.28  1.75 
    #>  4  2.06   2.71 0.609 0.698 1.72  0.691
    #>  5 -1.38   1.60 0.672 1.84  0.673 1.47 
    #>  6 -1.15   1.93 1.46  1.18  1.84  1.54 
    #>  7 -0.706  1.29 0.981 1.84  1.36  0.548
    #>  8 -1.05   2.62 0.783 0.873 0.720 1.88 
    #>  9 -0.646  2.33 1.09  0.626 1.85  1.22 
    #> 10 -0.185  1.12 1.79  0.573 0.880 0.900
    #> # ℹ 10 more rows

## Variable labels

surveycore preserves haven-style labels automatically when reading
`.xpt` or `.sav` files. You can also set labels manually:

    d2 <- set_var_label(d, income = "Annual household income (USD)")
    d2 <- set_var_label(d2, age = "Respondent age in years")

    extract_var_label(d2, income)
    #>                          income 
    #> "Annual household income (USD)"
    extract_var_label(d2, age)
    #>                       age 
    #> "Respondent age in years"

## Conversion to/from survey and srvyr

    # To survey::svydesign
    svy <- as_svydesign(d)
    class(svy)
    #> [1] "survey.design2" "survey.design"

    # Back to surveycore
    d_rt <- from_svydesign(svy)
    d_rt

## The surveyverse ecosystem

surveycore is the foundation of the surveyverse — a family of packages
built around it:

- **[surveytidy](https://jdenn0514.github.io/surveytidy/)** — dplyr
  verbs (`filter()`, `select()`, `mutate()`, `group_by()`) that respect
  survey design structure, so grouped summaries and subsetting always
  propagate weights and strata correctly.
- **surveywts** — calibration and post-stratification for survey
  weights. Coming soon.

## Development status

The package API is stable. The core classes, constructors, and analysis
functions (`get_freqs()` through `get_diffs()`) are not expected to
change in breaking ways. New analysis functions may be added in future
releases. See `NEWS.md` for the full changelog.

## Code of Conduct

Please note that the surveycore project is released with a [Contributor
Code of
Conduct](https://jdenn0514.github.io/surveycore/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## License

GPL-3. Variance estimation code vendored from the
[`survey`](https://cran.r-project.org/package=survey) package (Thomas
Lumley, GPL-2/GPL-3) — see `VENDORED.md` for full attribution.

## References

Lumley T (2004). “Analysis of Complex Survey Samples.” *Journal of
Statistical Software*, **9**(1), 1–19.

Lumley T (2010). *Complex Surveys: A Guide to Analysis Using R*. John
Wiley and Sons.
