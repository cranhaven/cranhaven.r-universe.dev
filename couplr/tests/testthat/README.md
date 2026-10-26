# Test Organization

This directory contains the test suite for the couplr package, organized into logical subdirectories for better maintainability.

## Directory Structure

```
tests/testthat/
├── algorithms/          # Core assignment algorithm tests
│   ├── test-assignment-auction.R
│   ├── test-assignment-auction-gs.R
│   ├── test-assignment-auto.R
│   ├── test-assignment-csflow.R
│   ├── test-assignment-hk01.R
│   ├── test-assignment-hungarian.R
│   ├── test-assignment-jv.R
│   ├── test-assignment-ssap-bucket.R
│   ├── test-assignment-ssp.R
│   └── test-cycle-cancel.R
├── gabow-tarjan/        # Gabow-Tarjan solver tests
│   ├── test-gabow_tarjan_solver.R
│   └── test_gabow_tarjan_module[A-H].R
├── kbest/              # K-best assignment algorithm tests
│   ├── test-kbest-lawler.R
│   └── test-kbest-murty.R
├── api/                # Modern API layer tests
│   ├── test-assign.R
│   └── test-assignment.R
├── matching/           # High-level matching functionality
│   └── test-matching.R
├── utilities/          # Utility function tests
│   ├── test-line-metric.R
│   ├── test-pixel-morph.R
│   └── test-prepare-cost-matrix.R
└── regression/         # Performance and regression tests
    └── test-auction-hang.R
```

## Test Categories

### Algorithms (`algorithms/`)
Tests for the core linear assignment problem solvers:
- **JV (Jonker-Volgenant)**: Efficient auction-based algorithm
- **Hungarian**: Classic Hungarian algorithm
- **Auction**: Standard auction algorithm with various epsilon values
- **Auction-GS**: Gauss-Seidel auction variant
- **SSP/SAP**: Shortest augmenting path algorithms
- **CSFlow**: Cost-scaling flow algorithm
- **HK01**: Hopcroft-Karp for 0/1 matrices
- **Auto**: Automatic algorithm selection based on problem characteristics
- **Cycle Cancel**: Cycle cancellation min-cost flow algorithm

### Gabow-Tarjan (`gabow-tarjan/`)
Tests for the Gabow-Tarjan 1-feasible complementary slackness algorithm:
- High-level solver tests with complementary slackness verification
- Modular component tests (modules A through H)

### K-Best (`kbest/`)
Tests for finding the k-best assignments:
- **Lawler**: Lawler's algorithm for k-best solutions
- **Murty**: Murty's partitioning algorithm

### API (`api/`)
Tests for the modern user-facing API:
- `lap_solve()`: Main assignment function with matrix/data frame inputs
- `lap_solve_batch()`: Batch processing for multiple problems
- `lap_solve_kbest()`: K-best solutions interface
- Grouped data operations with dplyr integration
- Print methods and utilities

### Matching (`matching/`)
High-level matching functionality for practical applications:
- `matchmaker()`: Blocking and stratification
- `match_couples()`: Optimal matching with constraints
- `greedy_couples()`: Fast greedy matching strategies
- Distance computation and caching
- Balance diagnostics and covariate assessment
- Join operations for matched data

### Utilities (`utilities/`)
Supporting functions and domain-specific utilities:
- Cost matrix preparation
- Pixel morphing metrics
- Line distance metrics

### Regression (`regression/`)
Performance and regression tests:
- Auction algorithm hang prevention
- Timing guards for algorithm performance

## Running Tests

### All tests
```r
devtools::test()
# or
testthat::test_dir("tests/testthat")
```

### Specific category
```r
testthat::test_dir("tests/testthat/algorithms")
testthat::test_dir("tests/testthat/matching")
```

### Single file
```r
testthat::test_file("tests/testthat/algorithms/test-assignment-jv.R")
```

## Test Conventions

- All test files follow the `test-*.R` naming convention
- Tests use `testthat` expectations (`expect_equal`, `expect_true`, etc.)
- Expensive tests use `skip_on_cran()` to avoid CRAN timeouts
- Numerical comparisons use appropriate tolerance (typically 1e-8 to 1e-10)
- Cross-validation: algorithms are compared against each other for correctness
- Each test group validates:
  - Correctness (optimal cost)
  - Edge cases (NA, Inf, empty, rectangular matrices)
  - Minimize/maximize duality
  - Proper error handling

## Notes

- testthat automatically discovers tests in subdirectories
- The directory structure does not affect test execution
- Tests can reference each other through the package namespace
