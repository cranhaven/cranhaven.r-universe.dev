# rprev 1.0.6

CMD CHECK fixes

# rprev 1.0.5

Added `rmarkdown` as a dependency

# rprev 1.0.4

Made changes to account for the upcoming stringsAsFactors = FALSE default argument to data.frame() in R 4.0 so that the package is backwards compatible.

# rprev 1.0.3

Change to unit test to allow for a tolerance when comparing floating points and a fix for a warning thrown from r-devel.

# rprev 1.0.2

Hotfix to address new `sample` implementation forthcoming in R 3.6.0. Currently the warning is being suppressed, but the unit tests will be updated once these changes have been implemented in stable R.

# rprev 1.0.1

Minor documentation fixes, with the main one being correcting the name of the Diagnostics vignette.

# rprev 1.0.0

Major overhaul to the API with non-backwards compatible changes. The primary change is that both the incidence and survival models are now specifiable, in contrast to the previous version which forced a homogeneous Poisson process incidence model and a Weibull survival model that uses age and sex as covariates. These models are retained as defaults, but the user can provide custom objects for both these processes, as documented in the User Guide.

A number of small basic functions mostly relating to diagnostics have been removed to condense the API.

See the User Guide vignette for examples of the new parameterisation of `prevalence` and general documentation.

# rprev 0.2.3

## Renamed `raw_incidence` to `yearly_incidence` 
This function has been renamed to be more descriptive of what the function actually does, and reparameterised to allow the user to specify the ending date of the time interval of interested instead. `raw_incidence` is still included but it throws a deprecated warning and suggests the use of `yearly_incidence`

## Renamed `determine_registry_years` to `determine_yearly_limits`
The original function name isn't very descriptive for what it does (provides the yearly end points of a specific time interval) and so have renamed it to better reflect its purpose. `determine_yearly_limits` has a slighlty different argument list to `determine_registry_years` to allow for the specification of the closing date in the interval rather than the opening.

## Other

  - Plot methods now return ggplot objects, allowing for easier manual tweaking
  - `prevalence` no longer runs the simulation when there is more registry data available than needed to estimate N-year prevalence
  - `prevalence` no longer requires a population size as an argument. Absolute prevalence is always calculated, with relative rates provided if population size is specified
  - `user_manual`: Updated to include a link to the specific webpage where the ONS data set is obtained from and improved formatting
  - `summary.prevalence` correctly displays posterior age distributions of simulated cases and now displays the prevalence estimates themselves
  - unit tests updated to reflect the above changes
  - vignette updated to reflect the above changes


# rprev 0.2.2

Bug hotfix.

# rprev 0.2.1

The posterior age distribution, returned from `prevalence` as in the `simulated` object, is now stored in the format of a nested list rather than a matrix as before. The first dimension of the list corresponds to each sex (if applicable), the next indexing the number of years of simulated cases, and the final corresponds to the bootstrap samples. The final level comprises a vector holding the ages of the simulated cases which are still contributing to prevalence at the index date from the corresponding sex, year, and bootstrap sample number.

# rprev 0.2.0

Minor bug fixes and a slight change to the parameterisation of prevalence:

  - In prevalence, prevalence_counted, and prevalence_simulated, the user specifies the index date at which to estimate prevalence, rather than having it inferred from the data
  - max_yearly_incidence has been removed as a parameter from both prevalence and prevalence_simulated as it can be calculated from the supplied data
  - prevalence per 100K estimates now have the confidence intervals the correct way around
  - unit tests for prevalence functions don't rely on cached results any longer. This has helped to reduce the size of the source code from 25MB to 2MB.
  
# rprev 0.1.0

First release of the package, working with all features necessary to provide estimates of point prevalence. Issues which we'd like to address in future releases are:

  - Allow for other incidence processes than homogeneous Poisson
  - Enable more flexibility in survival modelling, rather than Weibull regression with linear covariate effects
  - Allow for the inclusion of more covariates in both the survival modelling, and the marking of the incidence process
