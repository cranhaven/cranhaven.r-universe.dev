
<!-- README.md is generated from README.Rmd. Please edit that file -->

# surveytidy <a href="https://jdenn0514.github.io/surveytidy/index.html"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/JDenn0514/surveytidy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JDenn0514/surveytidy/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://app.codecov.io/gh/JDenn0514/surveytidy/branch/main/graph/badge.svg)](https://app.codecov.io/gh/JDenn0514/surveytidy)
[![pkgdown](https://github.com/JDenn0514/surveytidy/actions/workflows/pkgdown.yaml/badge.svg)](https://jdenn0514.github.io/surveytidy/)
<!-- badges: end -->

surveytidy provides dplyr and tidyr verbs for survey design objects
created with the [surveycore](https://github.com/JDenn0514/surveycore)
package. It makes survey analysis feel natural to tidyverse users. Full
documentation is at <https://jdenn0514.github.io/surveytidy/>.

``` r
library(surveycore)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(surveytidy)
#> 
#> Attaching package: 'surveytidy'
#> The following object is masked from 'package:stats':
#> 
#>     filter

# Build a survey design from the 2025 Pew NPORS survey
d <- as_survey(pew_npors_2025, weights = weight, strata = stratum)

# Use familiar dplyr verbs — survey design is preserved automatically
young_adults <- d |>
  # use filter to perform domain estimation (18–29 year olds)
  filter(agecat == 1) |>
  # keep only the columns of interest
  select(agecat, gender, partysum, fin_sit) |>
  # group by gender
  group_by(gender) |>
  # flag respondents reporting financial difficulty
  mutate(struggling = as.integer(fin_sit >= 3))
```

## Key idea: `filter()` marks rows, never removes them

The most important statistical feature of surveytidy is that `filter()`
uses **domain estimation** rather than physical subsetting:

``` r
# This keeps all 5,022 rows — variance estimation stays correct
republicans <- d |> filter(partysum == 1)
nrow(republicans@data) #> 5022
#> [1] 5022

# This physically removes rows — variance estimates would be wrong
wrong <- subset(d, partysum == 1) # issues a warning
#> Warning: ! `subset()` physically removes rows from the survey data.
#> ℹ This is different from `filter()`, which preserves all rows for correct
#>   variance estimation.
#> ✔ Use `filter()` for subpopulation analyses instead.
```

When you call `filter()`, surveytidy writes a logical column
`..surveycore_domain..` to the data. Phase 1 estimation functions read
this column to restrict calculations to the domain while retaining all
rows for correct variance estimation. Simply put, this calculates the
variance for subpopulations correctly.

## Installation

``` r
install.packages("surveytidy")
```

Or install the development version from GitHub:

``` r
# install.packages("pak")
# install surveycore first, to create surveycore style survey objects
pak::pak("JDenn0514/surveycore")
# install surveytidy
pak::pak("JDenn0514/surveytidy")
```

## Usage

### Filtering (domain estimation)

``` r
# Single condition
republicans <- d |> filter(partysum == 1)

# Chained filters AND the conditions together
rep_women <- d |>
  filter(partysum == 1) |>
  filter(gender == 2)

# Equivalent — both produce identical domain columns
rep_women2 <- d |> filter(partysum == 1, gender == 2)
```

### Selecting columns

``` r
# Design variables (weights, strata) are always kept in @data
# even when not selected — they are required for variance estimation
d2 <- select(d, party, gender, agecat, cregion)

# Design variables are hidden from print() but still present in @data
print(d2)
#> 
#> ── Survey Design ───────────────────────────────────────────────────────────────
#> <survey_taylor> (Taylor series linearization)
#> Sample size: 5022
#> 
#> # A tibble: 5,022 × 4
#>    party gender agecat cregion
#>    <dbl>  <dbl>  <dbl>   <dbl>
#>  1     3      2      4       4
#>  2     2      1      4       4
#>  3     1      2      4       4
#>  4     1      1      2       2
#>  5     1      1      4       1
#>  6     1      1      4       2
#>  7     3      1      3       3
#>  8     3      2      4       4
#>  9     3      2      3       3
#> 10     2      2      2       4
#> # ℹ 5,012 more rows
#> 
#> ℹ Design variables preserved but hidden: weight and stratum.
#> ℹ Use `print(x, full = TRUE)` to show all variables.
```

### Mutating

``` r
# Add new columns
d |>
  mutate(
    economy_poor = as.integer(econ1mod >= 3),
    south = as.integer(cregion == 3)
  )
#> 
#> ── Survey Design ───────────────────────────────────────────────────────────────
#> <survey_taylor> (Taylor series linearization)
#> Sample size: 5022
#> 
#> # A tibble: 5,022 × 67
#>    respid  mode language languageinitial stratum interview_start interview_end
#>     <dbl> <dbl>    <dbl>           <dbl>   <dbl> <date>          <date>       
#>  1   1470     2        1              NA      10 2025-05-27      2025-05-27   
#>  2   2374     2        1              NA       7 2025-05-01      2025-05-01   
#>  3   1177     3        1              10       5 2025-03-04      2025-03-04   
#>  4  15459     2        1              NA      10 2025-05-05      2025-05-05   
#>  5   9849     1        1               9       9 2025-02-22      2025-02-22   
#>  6   8178     3        1               9      10 2025-03-10      2025-03-10   
#>  7   3682     1        1               9       4 2025-02-27      2025-02-27   
#>  8   6999     2        1              NA      10 2025-05-12      2025-05-12   
#>  9   9945     2        1              NA      10 2025-05-09      2025-05-09   
#> 10   1901     1        1               9      10 2025-03-01      2025-03-01   
#> # ℹ 5,012 more rows
#> # ℹ 60 more variables: econ1mod <dbl>, econ1bmod <dbl>, comtype2 <dbl>,
#> #   unity <dbl>, crimesafe <dbl>, govprotct <dbl>, moregunimpact <dbl>,
#> #   fin_sit <dbl>, vet1 <dbl>, vol12_cps <dbl>, eminuse <dbl>, intmob <dbl>,
#> #   intfreq <dbl>, intfreq_collapsed <dbl>, home4nw2 <dbl>, bbhome <dbl>,
#> #   smuse_fb <dbl>, smuse_yt <dbl>, smuse_x <dbl>, smuse_ig <dbl>,
#> #   smuse_sc <dbl>, smuse_wa <dbl>, smuse_tt <dbl>, smuse_rd <dbl>, …

# Modifying a design variable warns you
d |> mutate(weight = weight * 0.5)
#> Warning: ! mutate() modified design variable(s): weight.
#> ℹ The survey design has been updated to reflect the new values.
#> ✔ Use `update_design()` if you intend to modify design variables. Modifying
#>   them via `mutate()` may produce unexpected variance estimates.
#> 
#> ── Survey Design ───────────────────────────────────────────────────────────────
#> <survey_taylor> (Taylor series linearization)
#> Sample size: 5022
#> 
#> # A tibble: 5,022 × 65
#>    respid  mode language languageinitial stratum interview_start interview_end
#>     <dbl> <dbl>    <dbl>           <dbl>   <dbl> <date>          <date>       
#>  1   1470     2        1              NA      10 2025-05-27      2025-05-27   
#>  2   2374     2        1              NA       7 2025-05-01      2025-05-01   
#>  3   1177     3        1              10       5 2025-03-04      2025-03-04   
#>  4  15459     2        1              NA      10 2025-05-05      2025-05-05   
#>  5   9849     1        1               9       9 2025-02-22      2025-02-22   
#>  6   8178     3        1               9      10 2025-03-10      2025-03-10   
#>  7   3682     1        1               9       4 2025-02-27      2025-02-27   
#>  8   6999     2        1              NA      10 2025-05-12      2025-05-12   
#>  9   9945     2        1              NA      10 2025-05-09      2025-05-09   
#> 10   1901     1        1               9      10 2025-03-01      2025-03-01   
#> # ℹ 5,012 more rows
#> # ℹ 58 more variables: econ1mod <dbl>, econ1bmod <dbl>, comtype2 <dbl>,
#> #   unity <dbl>, crimesafe <dbl>, govprotct <dbl>, moregunimpact <dbl>,
#> #   fin_sit <dbl>, vet1 <dbl>, vol12_cps <dbl>, eminuse <dbl>, intmob <dbl>,
#> #   intfreq <dbl>, intfreq_collapsed <dbl>, home4nw2 <dbl>, bbhome <dbl>,
#> #   smuse_fb <dbl>, smuse_yt <dbl>, smuse_x <dbl>, smuse_ig <dbl>,
#> #   smuse_sc <dbl>, smuse_wa <dbl>, smuse_tt <dbl>, smuse_rd <dbl>, …
#> Warning: mutate() modified design variable(s): `weight`

# Grouped mutate via group_by()
d |>
  group_by(partysum) |>
  mutate(econ_rank = rank(econ1mod) / n())
#> 
#> ── Survey Design ───────────────────────────────────────────────────────────────
#> <survey_taylor> (Taylor series linearization)
#> Sample size: 5022
#> Groups: partysum
#> 
#> # A tibble: 5,022 × 66
#>    respid  mode language languageinitial stratum interview_start interview_end
#>     <dbl> <dbl>    <dbl>           <dbl>   <dbl> <date>          <date>       
#>  1   1470     2        1              NA      10 2025-05-27      2025-05-27   
#>  2   2374     2        1              NA       7 2025-05-01      2025-05-01   
#>  3   1177     3        1              10       5 2025-03-04      2025-03-04   
#>  4  15459     2        1              NA      10 2025-05-05      2025-05-05   
#>  5   9849     1        1               9       9 2025-02-22      2025-02-22   
#>  6   8178     3        1               9      10 2025-03-10      2025-03-10   
#>  7   3682     1        1               9       4 2025-02-27      2025-02-27   
#>  8   6999     2        1              NA      10 2025-05-12      2025-05-12   
#>  9   9945     2        1              NA      10 2025-05-09      2025-05-09   
#> 10   1901     1        1               9      10 2025-03-01      2025-03-01   
#> # ℹ 5,012 more rows
#> # ℹ 59 more variables: econ1mod <dbl>, econ1bmod <dbl>, comtype2 <dbl>,
#> #   unity <dbl>, crimesafe <dbl>, govprotct <dbl>, moregunimpact <dbl>,
#> #   fin_sit <dbl>, vet1 <dbl>, vol12_cps <dbl>, eminuse <dbl>, intmob <dbl>,
#> #   intfreq <dbl>, intfreq_collapsed <dbl>, home4nw2 <dbl>, bbhome <dbl>,
#> #   smuse_fb <dbl>, smuse_yt <dbl>, smuse_x <dbl>, smuse_ig <dbl>,
#> #   smuse_sc <dbl>, smuse_wa <dbl>, smuse_tt <dbl>, smuse_rd <dbl>, …
```

### Renaming

``` r
# @variables and @metadata stay in sync automatically
d |>
  rename(
    party_affiliation = partysum,
    age_group = agecat
  )
#> 
#> ── Survey Design ───────────────────────────────────────────────────────────────
#> <survey_taylor> (Taylor series linearization)
#> Sample size: 5022
#> 
#> # A tibble: 5,022 × 65
#>    respid  mode language languageinitial stratum interview_start interview_end
#>     <dbl> <dbl>    <dbl>           <dbl>   <dbl> <date>          <date>       
#>  1   1470     2        1              NA      10 2025-05-27      2025-05-27   
#>  2   2374     2        1              NA       7 2025-05-01      2025-05-01   
#>  3   1177     3        1              10       5 2025-03-04      2025-03-04   
#>  4  15459     2        1              NA      10 2025-05-05      2025-05-05   
#>  5   9849     1        1               9       9 2025-02-22      2025-02-22   
#>  6   8178     3        1               9      10 2025-03-10      2025-03-10   
#>  7   3682     1        1               9       4 2025-02-27      2025-02-27   
#>  8   6999     2        1              NA      10 2025-05-12      2025-05-12   
#>  9   9945     2        1              NA      10 2025-05-09      2025-05-09   
#> 10   1901     1        1               9      10 2025-03-01      2025-03-01   
#> # ℹ 5,012 more rows
#> # ℹ 58 more variables: econ1mod <dbl>, econ1bmod <dbl>, comtype2 <dbl>,
#> #   unity <dbl>, crimesafe <dbl>, govprotct <dbl>, moregunimpact <dbl>,
#> #   fin_sit <dbl>, vet1 <dbl>, vol12_cps <dbl>, eminuse <dbl>, intmob <dbl>,
#> #   intfreq <dbl>, intfreq_collapsed <dbl>, home4nw2 <dbl>, bbhome <dbl>,
#> #   smuse_fb <dbl>, smuse_yt <dbl>, smuse_x <dbl>, smuse_ig <dbl>,
#> #   smuse_sc <dbl>, smuse_wa <dbl>, smuse_tt <dbl>, smuse_rd <dbl>, …
```

### Sorting

``` r
# Rows sort; domain column moves with them
d |>
  filter(partysum == 1) |>
  arrange(agecat)
#> 
#> ── Survey Design ───────────────────────────────────────────────────────────────
#> <survey_taylor> (Taylor series linearization)
#> Sample size: 5022
#> 
#> # A tibble: 5,022 × 66
#>    respid  mode language languageinitial stratum interview_start interview_end
#>     <dbl> <dbl>    <dbl>           <dbl>   <dbl> <date>          <date>       
#>  1  13673     1        1               9      10 2025-03-02      2025-03-02   
#>  2   9247     1        1               9       7 2025-03-03      2025-03-03   
#>  3  14402     1        1               9      10 2025-03-09      2025-03-09   
#>  4   2820     1        1               9       9 2025-02-24      2025-02-24   
#>  5  12802     1        1               9       6 2025-02-24      2025-02-24   
#>  6   7003     1        1               9       7 2025-02-25      2025-02-25   
#>  7   8391     1        2              10       7 2025-03-14      2025-03-14   
#>  8   2499     1        1               9      10 2025-02-25      2025-02-25   
#>  9     80     1        1               9       1 2025-03-12      2025-03-12   
#> 10   5357     1        1               9      10 2025-03-15      2025-03-15   
#> # ℹ 5,012 more rows
#> # ℹ 59 more variables: econ1mod <dbl>, econ1bmod <dbl>, comtype2 <dbl>,
#> #   unity <dbl>, crimesafe <dbl>, govprotct <dbl>, moregunimpact <dbl>,
#> #   fin_sit <dbl>, vet1 <dbl>, vol12_cps <dbl>, eminuse <dbl>, intmob <dbl>,
#> #   intfreq <dbl>, intfreq_collapsed <dbl>, home4nw2 <dbl>, bbhome <dbl>,
#> #   smuse_fb <dbl>, smuse_yt <dbl>, smuse_x <dbl>, smuse_ig <dbl>,
#> #   smuse_sc <dbl>, smuse_wa <dbl>, smuse_tt <dbl>, smuse_rd <dbl>, …

# Sort by group first, then by value within group
d |>
  group_by(partysum) |>
  arrange(agecat, .by_group = TRUE)
#> 
#> ── Survey Design ───────────────────────────────────────────────────────────────
#> <survey_taylor> (Taylor series linearization)
#> Sample size: 5022
#> Groups: partysum
#> 
#> # A tibble: 5,022 × 65
#>    respid  mode language languageinitial stratum interview_start interview_end
#>     <dbl> <dbl>    <dbl>           <dbl>   <dbl> <date>          <date>       
#>  1  14402     1        1               9      10 2025-03-09      2025-03-09   
#>  2   8391     1        2              10       7 2025-03-14      2025-03-14   
#>  3   2499     1        1               9      10 2025-02-25      2025-02-25   
#>  4     80     1        1               9       1 2025-03-12      2025-03-12   
#>  5   5357     1        1               9      10 2025-03-15      2025-03-15   
#>  6  10841     1        1               9       5 2025-03-25      2025-03-25   
#>  7   8646     1        2               9       8 2025-02-28      2025-02-28   
#>  8   5576     1        1               9      10 2025-02-27      2025-02-27   
#>  9  11753     2        1              NA       5 2025-05-01      2025-05-01   
#> 10  17387     2        1              NA       7 2025-05-27      2025-05-27   
#> # ℹ 5,012 more rows
#> # ℹ 58 more variables: econ1mod <dbl>, econ1bmod <dbl>, comtype2 <dbl>,
#> #   unity <dbl>, crimesafe <dbl>, govprotct <dbl>, moregunimpact <dbl>,
#> #   fin_sit <dbl>, vet1 <dbl>, vol12_cps <dbl>, eminuse <dbl>, intmob <dbl>,
#> #   intfreq <dbl>, intfreq_collapsed <dbl>, home4nw2 <dbl>, bbhome <dbl>,
#> #   smuse_fb <dbl>, smuse_yt <dbl>, smuse_x <dbl>, smuse_ig <dbl>,
#> #   smuse_sc <dbl>, smuse_wa <dbl>, smuse_tt <dbl>, smuse_rd <dbl>, …
```

### Grouping

``` r
d2 <- group_by(d, partysum) # set groups
d3 <- group_by(d2, gender, .add = TRUE) # add to groups
d4 <- ungroup(d3) # remove all groups
```

### Physical row operations (use sparingly)

``` r
# These physically remove rows and always issue a warning.
# Use filter() instead for subpopulation analyses.
slice_head(d, n = 100) # first 100 rows
#> Warning: ! `slice_head()` physically removes rows from the survey data.
#> ℹ This is different from `filter()`, which preserves all rows for correct
#>   variance estimation.
#> ✔ Use `filter()` for subpopulation analyses instead.
#> 
#> ── Survey Design ───────────────────────────────────────────────────────────────
#> <survey_taylor> (Taylor series linearization)
#> Sample size: 100
#> 
#> # A tibble: 100 × 65
#>    respid  mode language languageinitial stratum interview_start interview_end
#>     <dbl> <dbl>    <dbl>           <dbl>   <dbl> <date>          <date>       
#>  1   1470     2        1              NA      10 2025-05-27      2025-05-27   
#>  2   2374     2        1              NA       7 2025-05-01      2025-05-01   
#>  3   1177     3        1              10       5 2025-03-04      2025-03-04   
#>  4  15459     2        1              NA      10 2025-05-05      2025-05-05   
#>  5   9849     1        1               9       9 2025-02-22      2025-02-22   
#>  6   8178     3        1               9      10 2025-03-10      2025-03-10   
#>  7   3682     1        1               9       4 2025-02-27      2025-02-27   
#>  8   6999     2        1              NA      10 2025-05-12      2025-05-12   
#>  9   9945     2        1              NA      10 2025-05-09      2025-05-09   
#> 10   1901     1        1               9      10 2025-03-01      2025-03-01   
#> # ℹ 90 more rows
#> # ℹ 58 more variables: econ1mod <dbl>, econ1bmod <dbl>, comtype2 <dbl>,
#> #   unity <dbl>, crimesafe <dbl>, govprotct <dbl>, moregunimpact <dbl>,
#> #   fin_sit <dbl>, vet1 <dbl>, vol12_cps <dbl>, eminuse <dbl>, intmob <dbl>,
#> #   intfreq <dbl>, intfreq_collapsed <dbl>, home4nw2 <dbl>, bbhome <dbl>,
#> #   smuse_fb <dbl>, smuse_yt <dbl>, smuse_x <dbl>, smuse_ig <dbl>,
#> #   smuse_sc <dbl>, smuse_wa <dbl>, smuse_tt <dbl>, smuse_rd <dbl>, …
subset(d, !is.na(fin_sit)) # remove rows with NA in fin_sit
#> Warning: ! `subset()` physically removes rows from the survey data.
#> ℹ This is different from `filter()`, which preserves all rows for correct
#>   variance estimation.
#> ✔ Use `filter()` for subpopulation analyses instead.
#> 
#> ── Survey Design ───────────────────────────────────────────────────────────────
#> <survey_taylor> (Taylor series linearization)
#> Sample size: 5022
#> 
#> # A tibble: 5,022 × 65
#>    respid  mode language languageinitial stratum interview_start interview_end
#>     <dbl> <dbl>    <dbl>           <dbl>   <dbl> <date>          <date>       
#>  1   1470     2        1              NA      10 2025-05-27      2025-05-27   
#>  2   2374     2        1              NA       7 2025-05-01      2025-05-01   
#>  3   1177     3        1              10       5 2025-03-04      2025-03-04   
#>  4  15459     2        1              NA      10 2025-05-05      2025-05-05   
#>  5   9849     1        1               9       9 2025-02-22      2025-02-22   
#>  6   8178     3        1               9      10 2025-03-10      2025-03-10   
#>  7   3682     1        1               9       4 2025-02-27      2025-02-27   
#>  8   6999     2        1              NA      10 2025-05-12      2025-05-12   
#>  9   9945     2        1              NA      10 2025-05-09      2025-05-09   
#> 10   1901     1        1               9      10 2025-03-01      2025-03-01   
#> # ℹ 5,012 more rows
#> # ℹ 58 more variables: econ1mod <dbl>, econ1bmod <dbl>, comtype2 <dbl>,
#> #   unity <dbl>, crimesafe <dbl>, govprotct <dbl>, moregunimpact <dbl>,
#> #   fin_sit <dbl>, vet1 <dbl>, vol12_cps <dbl>, eminuse <dbl>, intmob <dbl>,
#> #   intfreq <dbl>, intfreq_collapsed <dbl>, home4nw2 <dbl>, bbhome <dbl>,
#> #   smuse_fb <dbl>, smuse_yt <dbl>, smuse_x <dbl>, smuse_ig <dbl>,
#> #   smuse_sc <dbl>, smuse_wa <dbl>, smuse_tt <dbl>, smuse_rd <dbl>, …
```

## Relationship to surveycore

surveytidy is an add-on layer. surveycore provides:

- S7 survey design classes (`survey_taylor`, `survey_replicate`,
  `survey_twophase`)
- Constructors (`as_survey()`, `as_survey_rep()`,
  `as_survey_twophase()`)
- Metadata system (variable labels, value labels, question prefaces)

surveytidy provides the dplyr/tidyr interface on top of those classes.
Phase 1 (in development) will add estimation functions (`get_mean()`,
`get_total()`, etc.) that respect the domain column and `@groups` set by
surveytidy verbs.

## License

GPL-3
