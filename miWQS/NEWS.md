
# miWQS 0.4.4

  - Edited the Description field of DESCRIPTION file to meet CRAN
    standards.

# miWQS 0.4.3

  - Updated the email address of the maintainer in `DESCRIPTION`.
  - `make.quantile.matrix()`: Adjusted quantile function by adding
    optional argument `digits = 7` to reflect change in quantile version
    in CRAN version 4.0.5. The change does not affect CRAN version 4.0.4
    or below.

> The names of quantile()’s result no longer depend on the global
> getOption(“digits”), but quantile() gets a new optional argument
> digits = 7 instead.

# miWQS 0.4.2

  - Fixed discrepancy in accordance with CRAN standards: Removed global
    variables `estimate` and `chemical` from environment in
    `plot.wqs()`.

# miWQS 0.4.1

  - Fixed discrepancies with CRAN standards:
  - DESCRIPTION FILE: Rewrote dois to match CRAN standards (\<doi:…\>,
    not \<:…\>)
  - `impute.multivariate.bayesian()`: Removed empty details section

# miWQS 0.4.0 – TO CRAN

## Breaking Changes

  - Additional Resource: A methods paper (Hargarten & Wheeler (2020)
    \<:10.1016/j.envres.2020.109466\>) is cited in the README,
    DESCRIPTION, and some function files. This paper provides
    theoretical details in using the package.
  - `estimate.wqs()`: Can now compare training and validation datasets,
    which is commonly done in WQS analysis.
  - `plot.wqs()`: Removed defunct `filename` argument. Plots are no
    longer saved automatically; please save manually using ().

## New Functions

  - New `impute.multivariate.regress()`: imputes all chemicals jointly
    using a multivariate Bayesian regression.

## Minor Updates

  - Documentation clarity and edits
  - `do.many.wqs()`:
      - Fixed error in collecting WQS results. When training proportion
        not 1, WQS is smaller than matrix to hold values.
      - Added a message that included the sample size, number of
        chemicals, datasets to impute, and number of covariates
        modelled.
      - Passed the explicit `B` argument to the `...` argument. Should
        not impact any code, as it is called within `estimate.wqs()`.
  - `estimate.wqs()`:
      - Cleaned up code
      - Updated accessories `summarize.compare()` and
        `make.descriptive.tables()`.
  - `impute.sub()`: replaced for-loop with much faster
    `tidyr::replace_na()`.
  - `impute.boot()` and `impute.Lubin()`: Now uses the survival routines
    that have been updated in version 3.
  - `plot.wqs()`:
      - Now uses `tidyr::pivot_longer()` instead of `tidyr::gather()` in
        response to update in **tidyr** package from v. 0.8 to v. 1.0.0.
        See `vignette("pivot")` and `vignette("in-packages")`in tidyr
        documentation for more information.
      - Updated documentation to require tidyr to be v. 1.0.0.  
      - Added a `ggplot2` layer to make x-axis smaller so you can read
        labels on weights histogram
  - Internal Improvements
      - accessory `check_constants()`: Fixed bug that returns error if
        the number of bootstraps (B), imputed datasets (K), or length of
        chain (T) is 0 <thanks R Journal reviewer>
      - new accessory `imp_cond_MVN`: An accessory function used to
        impute conditional truncated multivariate normal used in
        `impute.multivariate.regress()`. Added **condMVNorm** &
        **tmvtnorm** packages to DESCRIPTION.
      - accessory `formatMedianIQR`: Changed format of median and IQR to
        “median\[Q1, Q3\]”.
      - accessory `is.even()`, `is.odd()` – now accepts tolerance
        argument for consistency with other `is...` functions.
      - accessory `specify.init()`: change argument to C.
      - accessory `replace_na()`: Changes made in `impute.sub()`

# miWQS 0.2.0

## New Features

  - New `estimate.wqs.formula()` now has a formula capability. Can type
    in a full formula and specify column names instead of dividing up
    the data into three columns.
  - `impute.Lubin()`:
      - Softly deprecated. Use `impute.boot` instead.
      - If chemcol is complete, the function NOW returns complete data
        with warning
      - The `bootstrap_index` element is now factors instead of numbers.
        Easier to see what subjects were selected.
  - `pool.mi()`:
      - Added argument `prt` to print to standard output so that the
        `pool.mi` object can be read in an understandable fashion.
      - Argument `methods` is NOW more robust by adding `tolower()`.
        Now, if someone writes in all caps, the function still works.
      - Accessory `mice.df()` is now clearer – only accepts either
        method to avoid mistake.

## Minor Improvements and Changes

  - Documentation & example clarity.
  - Moved the base **R** packages from `IMPORTS` to `DEPENDS` for
    clarity. As the base R packages are already attached, this should
    have no impact on package performance.
  - Replace all checks using `class(.)==matrix` with `is()`, [as
    directed from
    CRAN](https://developer.r-project.org/Blog/public/2019/11/09/when-you-think-class.-think-again/index.html).
  - Clarified internal performance
      - accessory `check_constants()`: Used `sprintf()` for more generic
        error returns.
      - accessory `check_imputation`: Removed checking Z here. Now Z is
        checked in each `impute.`.. function.
      - accessory `head.array()`: made package for `head()` clear by
        using `utils::head()`.
      - accessory `is.even()`, `is.odd()` – now accepts tolerance
        argument for consistency with other `is.`.. functions.
  - `combine.AIC()`: removed comma from output that was generated from
    format.mean.sd() (Added July 22, 2019)
  - `estimate.wqs()`:
      - Syntax clarification,
      - Added `if/else` on signal function options for speed.
      - Now uses `check_wqs_function()`; deleted duplicate
        check\_function() from before.
  - `impute.boot()`:
      - Added note to contact Jay Lubin for SAS macro
      - Cleaned up code/spaces. Didn’t change functionality.

# miWQS 0.1.0

## Breaking changes

  - The `plot.wqs()` no longer automatically saves the plots to reduce
    clutter and save the user’s workspace. However, you can still use
    `ggsave()` on the output if you wish to save WQS plots. If you
    depended on this behavior, you’ll need to condition on
    `packageVersion("miWQS") > "0.0.0"`.
  - The title of the package is changed from “analysis” to “regression”
    to make the name more specific.
  - Creates a README vignette.
  - Shortens description in DESCRIPTION file for clarity.

## New Features

  - New `analyze.individually()` does individual chemical analysis. The
    outcome is independently regressed on each chemical. Individual
    chemical analyses with the outcome can be used to determine whether
    the mixture of chemicals is positively or negatively related to the
    outcome (the `b1.pos` argument in `estimate.wqs()`).
  - New `do.many.wqs()` does many WQS analyses, which is useful in the
    second stage of multiple imputation.
  - New `combine.AIC()` combines AIC results from many WQS analyses,
    similar in spirit to `pool.mi()`.
  - New `impute.boot()` performs bootstrapping imputation for many
    chemicals, not just one.
  - New `impute.sub()` imputes the values below the detection limit with
    1/sqrt(2) of that’s chemical’s detection limit.

## Minor improvements and fixes

  - Documentation is clarified and keywords are added.
  - Most `cat()` used in the functions are changed to messages
    `message()` so that the user can suppress messages using
    `suppressMessage()`.
  - Consistent comments in all functions now use notation `#>`.
  - The `estimate.wqs()`
      - Arguments
          - `B` argument: Documentation is clarified when bootstrapping
            within WQS. You now can do WQS regression without
            bootstrapping, but it is not recommended.
          - `place.bdls.in.Q1` argument now does something. You can set
            it to FALSE and regular quantiles are made. Passed to
            `make.quantile.matrix()`.
          - `offset` argument fixes transfer to `glm2()`. User should
            enter the offset on the normal scale; the logarithm is not
            taken. It is passed to `glm2()` and `glm()`. FROM: The
            `offset` argument in `glm2()` has a default value of 0. The
            offset value in `estimate.wqs()` by default is a vector of
            1’s. NOW: When using `glm2()`, offset argument now takes
            the logarithm as expected in all instances, (especially in
            `wqs.fit()`). User does not change the default of offset
            value.
      - Inner Mechanics
          - Removes duplicate output in by adding the
            `suppressMessages()` function.
          - Uses `wqs.fit()` accessory function instead of code in
            `estimate.wqs()`.
          - Changes lower case “c” to upper case “C” to avoid conflict
            with `c()`.
  - The `impute.univariate.bayesian.mi()` function:
      - `T` argument: changes default length of chain to 1,000 in order
        to be consistent with other functions.
      - `indicator.miss` output now returns as a single number (a sum)
        rather than a vector.
      - Fixes bug in initial values for the standard deviation in MCMC
        chain. Calculates standard deviation using the logarithm of
        `cov()` function. FROM: The `complete.observations` argument was
        used for observed X. NOW: standard deviations are calculated
        based on the substituted imputed chemical matrix, X, as
        covariances may not exist if X has many missing values.
      - Fixes bug so that the imputed values now draw from the last
        state, instead of the second-to-last state.
      - Inner Mechanics
          - Reduced \# of objects to be used in finding `initial2`.
          - Changed object name `x.miss.initial` to
            `log.x.miss.initial`.
          - Examples still remain the same.
  - The `make.quantile.matrix()`:
      - Adds the `place.bdls.in.Q1` argument. The default automatically
        places any missing values in X placed into the first quantile.
        We suggest the user does not specify this argument unless the
        user wants to be specific in how missing values in components
        are handled. In version 0.0.9, this argument previously had no
        effect on how quantiles were made. This argument now has an
        effect.
      - Fixes an error if there are ties in the quantiles. An error
        occurred in `cut.default(...): 'breaks' are not unique`. We use
        the `.bincode()` function instead so that ties are handled if
        they arise.
  - The `print.wqs()` now concatenates, instead of prints, the
    convergence of the bootstrap samples.
  - The `simdata87` data:
      - element `Z` contains renamed covariate names for clarity.
      - elements `X.true, X.bdl, DL, delta, n0`: all chemical names are
        converted to plain text for clarity to be used in R. The “p-p\`”
        are removed, and the “alpha” and “gamma” are spelled out
        directly.

# miWQS 0.0.9

  - First Release of Package to the public.
  - For updates to CRAN team, see cran-comments.
  - Replaces examples using example dataset in package instead of using
    package wqs. Looks cleaner
  - Removes printed output from `estimate.wqs()`.
  - Makes documentation from `estimate.wqs()` clearer.
  - Cleans `print.wqs()` documentation

# miWQS 0.0.8

  - Reworked `plot.wqs()` function by using **ggplot2** instead of base
    plotting in R.

# miWQS 0.0.7

  - Fixes bug in doing Poisson Rate WQS regressions. Adds argument
    offset to the `check_function()` and `randomize.train()`.
  - For updates to CRAN team, see cran-comments.

# miWQS 0.0.0

  - Adds a `NEWS.md` file to track changes to the package.
  - First Release of the Package to CRAN team
  - Successfully passed windows check.
