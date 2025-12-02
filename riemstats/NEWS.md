# riemstats 0.2.0

## Major Changes

### Permutation Test Implementation

- **BREAKING CHANGE**: Replaced parametric bootstrap with permutation test in `riem_anova()`
  - Function parameter renamed: `den` → `nperm` (default changed from 5 to 1000)
  - Old function `one_bootstrap()` removed and replaced with `one_permutation()`
  - Users must update existing code to use the new parameter name

**Benefits of permutation test approach:**
- Faster computation (no synthetic data generation required)
- More stable p-values (no parameter estimation variability)
- Exact test under null hypothesis (when group labels are exchangeable)
- Simpler implementation (direct shuffling of observations)

**Motivation:**
- Asymptotic tests showed inflation issues
- Bootstrap was computationally expensive and under-conservative
- Permutation test is more natural for comparing sub-populations

See GitHub issue #61 and PR #62 for detailed discussion and implementation.

## Documentation

- Updated `riem_anova()` documentation with permutation test description
- Added `one_permutation()` function documentation
- Updated vignette with working permutation test examples
- Reduced vignette permutation count to 100 for faster builds

## Testing

- Updated test suite for permutation approach
- All 126 tests passing
- Fixed CSample constructor issues in tests

## Note on Validation

This release is pending comprehensive validation through simulation studies (EXP-MP-062).
Initial testing shows correct behavior, but full Type I error and power analysis is ongoing.

# riemstats 0.1.0

## Dependencies
- Depends on R (>= 4.3.0)
- Imports the following packages:
  - Matrix
  - methods
  - expm
  - R6
  - purrr
  - MASS
  - furrr
- Suggests the following packages for testing and documentation:
  - testthat (>= 3.0.0)
  - knitr
  - rmarkdown

## Miscellaneous
- Added a `LICENSE` file with the MIT license.
- Maintainer: Nicolas Escobar <nescoba@iu.edu>
