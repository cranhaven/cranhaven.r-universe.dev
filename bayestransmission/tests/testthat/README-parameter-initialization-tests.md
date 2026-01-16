# Parameter Initialization Test Suite

## Overview

The `test-parameter-initialization.R` file contains comprehensive tests for verifying that model parameters are correctly initialized in the bayestransmission R package. These tests were created to prevent regression of the bugs identified and fixed during parameter initialization debugging.

## Test Coverage

The test suite includes **8 test cases** with **34 individual assertions** covering:

### 1. InsituParams Initialization (2-state model)
- **Purpose:** Verify that in-situ colonization probabilities are correctly mapped
- **Critical Feature:** Tests that colonized probability is at index 2 (not index 1) for 2-state models
- **Bug Prevented:** Index mapping error where colonized was incorrectly placed at index 1

### 2. RandomTestParams Initialization (2-state model)
- **Purpose:** Verify that clinical random test parameters are correctly set
- **Critical Feature:** Tests that uncolonized and colonized test probabilities/rates are properly assigned
- **Bug Prevented:** RandomTestParams::write() outputting wrong values (parent class instead of own values)

### 3. LinearAbxAcquisitionParams Initialization
- **Purpose:** Verify that antibiotic acquisition parameters are correctly initialized
- **Critical Feature:** Tests that mass and freq parameters use 0.9999 (not 1.0) to avoid logit infinity
- **Bug Prevented:** 
  - Double log-transformation bug (dolog=true + internal transform)
  - Logit boundary value causing NaN (logit(1.0) = infinity)

### 4. AbxRateParams Initialization (2-state model)
- **Purpose:** Verify that antibiotic rate parameters use correct index mapping
- **Critical Feature:** Tests that colonized rate is at index 2 (via colonized parameter)
- **Bug Prevented:** Index mapping error in setupAbxRateParams

### 5. Complete Parameter Initialization with Random Values
- **Purpose:** Integration test using randomly generated parameters
- **Coverage:** Tests ALL parameter types together:
  - InsituParams (in-situ colonization)
  - SurveillanceTestParams (surveillance testing)
  - RandomTestParams (clinical testing)
  - OutOfUnitInfectionParams (out-of-unit infection)
  - LinearAbxAcquisitionParams (antibiotic acquisition - 7 parameters)
  - ClearanceParams (clearance rates)
  - AbxParams (antibiotic configuration)
  - AbxRateParams (antibiotic rates)
- **Validation:** Verifies all 22 comparable parameters match expected values

### 6. Parameter Validation - Logit Boundary Values
- **Purpose:** Verify that logit-transformed parameters handle boundary values correctly
- **Test Cases:**
  - mass = 0.9999, freq = 0.9999 (safe upper bound)
  - mass = 0.0001, freq = 0.0001 (safe lower bound)
- **Bug Prevented:** NaN errors from logit(1.0) = log(1/(1-1)) = infinity

### 7. 3-State Model Parameter Initialization
- **Purpose:** Verify that 3-state models (with latent state) initialize correctly
- **Coverage:** Tests InsituParams, RandomTestParams, and AbxRateParams with latent state included
- **Validation:** Confirms that latent state parameters are properly used in 3-state models

### 8. Index Mapping Consistency Across Parameter Types
- **Purpose:** Verify consistent index mapping across all parameter classes
- **Critical Feature:** Tests that [0, 1, 2] = [uncolonized, latent, colonized] is consistent
- **Validation:** Confirms colonized values appear at correct positions in all parameter types

## Technical Details

### Index Mapping for 2-State Models
All parameter classes use the same index convention:
- **Index 0:** Uncolonized state
- **Index 1:** Latent state (unused in 2-state models, set to 0)
- **Index 2:** Colonized state

When constructing 2-state model parameters, values for colonized must be passed at index 2 (or via the `latent` parameter for RandomTestParams which maps to index 2 internally).

### Parameter Transformations
- **Log-transformed parameters:** Rates and base acquisition values
  - LinearAbxICP::set() applies `log()` transform internally
  - Do NOT use `dolog=true` in setParam() calls
- **Logit-transformed parameters:** Mass and frequency (probabilities)
  - LinearAbxICP::set() applies `logistic()` transform internally
  - Use values < 1.0 (e.g., 0.9999) to avoid `logit(1.0) = infinity`

### Random Parameter Generation
The comprehensive test uses `runif()` to generate random values within valid ranges:
- Probabilities: 0.05 to 0.9999
- Rates: 0.0001 to 2.0
- Mass/Freq: 0.9 to 0.9999 (avoid logit boundary)
- Seed: 123 (for reproducibility)

## Running the Tests

### Single Test File
```r
library(testthat)
test_file('tests/testthat/test-parameter-initialization.R')
```

### Full Test Suite
```r
library(testthat)
test_dir('tests/testthat')
```

### From Command Line
```bash
Rscript -e "library(testthat); test_file('tests/testthat/test-parameter-initialization.R')"
```

## Expected Results

All 34 assertions should pass:
```
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 34 ] Done!
```

## Bugs Prevented

This test suite prevents regression of the following bugs:

1. **InsituParams Index Mapping Bug**
   - File: `test_r_init_params.R`
   - Issue: Colonized probability at wrong index
   - Fix: Use `c(0.9, 0, 0.1)` not `c(0.9, 0.1, 0)`

2. **RandomTestParams::write() Bug**
   - File: `src/models_RandomTestParams.cpp` lines 197-223
   - Issue: Calling parent TestParams::write() instead of outputting own probs
   - Fix: Directly output `probs[0][1]`, `probs[2][1]`, `rates[0]`, `rates[2]`

3. **LinearAbxICP Double Transformation Bug**
   - File: `src/modelsetup.h` lines 177-190
   - Issue: `dolog=true` + internal `LinearAbxICP::set()` transform = double transformation
   - Fix: Removed `dolog=true` from all LinearAbx acquisition parameter setups

4. **AbxRateParams Index Mapping Bug**
   - File: `src/modelsetup.h` lines 227-235
   - Issue: Colonized mapped to index 1 instead of index 2
   - Fix: Corrected to `index 2 = colonized, index 1 = latent`

5. **Logit Boundary Value Bug**
   - File: Multiple
   - Issue: `logit(1.0) = infinity` causing NaN
   - Fix: Use 0.9999 instead of 1.0 for logit-transformed parameters

## Maintenance

When modifying parameter initialization code:

1. **Run these tests** to ensure no regression
2. **Update tests** if parameter structure changes
3. **Add new tests** for new parameter types
4. **Document** any new index mapping or transformation rules

## Related Documentation

- `final_parameter_comparison.md` - Comprehensive documentation of all bugs fixed
- `test_r_init_params.R` - Original debugging test script
- `src/modelsetup.h` - Parameter setup functions
- `src/models_RandomTestParams.cpp` - RandomTestParams implementation
- `R/constructors.R` - R parameter constructors
