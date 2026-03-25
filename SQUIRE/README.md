# SQUIRE: Statistical Quality-Assured Integrated Response Estimation

[![CRAN Status](https://www.r-pkg.org/badges/version/SQUIRE)](https://CRAN.R-project.org/package=SQUIRE)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Author:** Richard A. Feiss  
**Version:** 1.0.1  
**License:** MIT  
**Institution:** Minnesota Center for Prion Research and Outreach (MNPRO), University of Minnesota  
**GitHub:** https://github.com/RFeissIV

---

## Overview

**SQUIRE** (Statistical Quality-Assured Integrated Response Estimation) provides a structured workflow for biological parameter estimation that combines statistical validation with systematic testing of constraint configurations to improve optimization reliability.

SQUIRE addresses common challenges in biological parameter estimation by organizing existing statistical and optimization methods into a coherent workflow. The package combines:

1. **Statistical validation** - ANOVA-based testing ensures optimization is only performed on data with significant treatment effects
2. **Systematic constraint configuration testing** - Evaluates combinations of T (log-scale), P (positive orthant), and E (Euclidean) parameter constraints
3. **Geometry-aware optimization** - Applies selected constraints during parameter estimation using GALAHAD
4. **Validation-gated workflow** - Prevents optimization on statistically insignificant data

SQUIRE integrates with the [**GALAHAD**](https://CRAN.R-project.org/package=GALAHAD) optimization package for geometry-adaptive trust-region methods.

---

## Key Features

### 📊 **Statistical Quality Assurance**
- **Pre-optimization validation**: ANOVA testing for treatment effects
- **Data quality requirements**: Minimum timepoints and replication checks
- **Effect size assessment**: Prevents optimization on statistically weak signals
- **Validation gates**: Clear criteria for when optimization is justified

### ⚙️ **Systematic Constraint Configuration Testing**
- **T/P/E systematic testing**: Compares log-scale, positive orthant, and Euclidean constraint combinations
- **Structured approach**: Organized testing of predefined constraint configurations
- **Automated selection**: Chooses optimal configuration from tested combinations
- **Geometry-aware optimization**: Applies selected constraints during parameter estimation

### 🧬 **Biological Applications**
- **Kinetic curve analysis**: RT-QuIC, enzyme kinetics, growth curves
- **Treatment comparisons**: Structured experimental design support  
- **Parameter validation**: Statistical significance testing of estimates
- **Uncertainty quantification**: Confidence assessment for biological interpretation

---

## Installation

```r
# From CRAN
install.packages("SQUIRE")

# Required dependency
install.packages("GALAHAD")

# Development version from GitHub
# install.packages("devtools")
# devtools::install_github("RFeissIV/SQUIRE")
```

---

## Quick Start

### Example with Synthetic Germination Data

```r
library(SQUIRE)

# Realistic synthetic germination data (based on seed germination study patterns)
# Replace with your actual experimental data for real applications
germination_data <- data.frame(
  time = rep(c(0, 1, 2, 3, 4, 5, 6, 7), times = 12),  # Days
  treatment = rep(c("Control", "Contaminant_A", "Contaminant_B"), each = 32),
  replicate = rep(rep(1:4, each = 8), times = 3),
  response = c(
    # Control: typical cumulative germination (%)
    0, 5, 15, 28, 45, 62, 75, 82,    # Rep 1
    0, 4, 12, 26, 43, 60, 73, 80,    # Rep 2  
    0, 6, 17, 30, 47, 64, 77, 84,    # Rep 3
    0, 5, 14, 27, 44, 61, 74, 81,    # Rep 4
    
    # Contaminant_A: reduced germination (growth inhibitor)
    0, 2, 8, 18, 32, 48, 60, 68,     # Rep 1
    0, 3, 7, 16, 30, 46, 58, 66,     # Rep 2
    0, 2, 9, 19, 34, 50, 62, 70,     # Rep 3
    0, 3, 8, 17, 31, 47, 59, 67,     # Rep 4
    
    # Contaminant_B: enhanced germination (growth promoter)
    0, 8, 22, 38, 55, 72, 85, 92,    # Rep 1
    0, 7, 20, 36, 53, 70, 83, 90,    # Rep 2
    0, 9, 24, 40, 57, 74, 87, 94,    # Rep 3
    0, 8, 21, 37, 54, 71, 84, 91     # Rep 4
  )
)

# Statistical validation and systematic optimization
results <- SQUIRE(
  data = germination_data,
  treatments = c("Control", "Contaminant_A", "Contaminant_B"),
  control_treatment = "Control",
  verbose = TRUE
)

# Check results
if (results$optimization_performed) {
  cat("Optimization was statistically justified\n")
  print(results$parameters$parameter_matrix)
} else {
  cat("No significant treatment effects detected\n")
  cat("Reason:", results$validation_results$reason, "\n")
}
```

### Example Output

```r
STEP 1: Statistical Validation
  Testing treatment effects...
  Treatment effect p-value: < 0.001
  Significant: YES

STEP 2: Systematic Constraint Configuration Testing (T/P/E)
  Run 1: Testing T (log-scale) configurations...
    Optimal T: []
  Run 2: Testing P (positive) configurations...
    Optimal P: [1,2,3]
  Run 3: Testing E (Euclidean) configurations...
    Optimal E: []
  
STEP 3: Geometry-Aware Optimization
  Using selected constraints: T=[], P=[1,2,3], E=[]
  Optimizing Control with geometry-aware constraints...
    Geometry-aware parameters: rate=0.125, offset=0.000, scale=82.0
  Optimizing Contaminant_A with geometry-aware constraints...
    Geometry-aware parameters: rate=0.118, offset=0.000, scale=68.0
  Optimizing Contaminant_B with geometry-aware constraints...
    Geometry-aware parameters: rate=0.135, offset=0.000, scale=92.0

SUCCESS! Systematic testing selected and applied optimal constraint configuration:
  All parameters constrained to be positive (P=[1,2,3])
  Scale parameter captures treatment effects: Control=82%, ContaminantA=68%, ContaminantB=92%
```

---

## Applications

### Designed For:
- **Germination studies** (seed germination curves, treatment effects)
- **Growth curve analysis** (bacterial, cellular, plant growth)
- **Enzyme kinetics** (Michaelis-Menten and related models)
- **RT-QuIC kinetic analysis** (prion amplification curves)
- **Dose-response studies** (pharmacological, toxicological)

### Experimental Design Requirements:
- **Time-series data** with treatment comparisons
- **Minimum 3 replicates** per treatment (recommended: 4+)
- **Minimum 5 timepoints** (recommended: 6+)
- **Control treatment** for statistical comparison

---

## Systematic Workflow

### Step 1: Statistical Validation
- Data quality assessment (timepoints, replication)
- ANOVA testing for significant treatment effects
- Effect size calculation
- Decision: proceed with optimization or not

### Step 2: Constraint Configuration Testing
- **T testing**: Log-scale parameter constraints
- **P testing**: Positive orthant constraints  
- **E testing**: Euclidean (unconstrained) parameters
- **Result**: Optimal configuration from tested combinations

### Step 3: Geometry-Aware Optimization
- GALAHAD optimization with selected constraints
- Projection-based feasible set methods
- Statistical validation of parameter estimates
- Biological interpretation with uncertainty quantification

---

## Important Notes

### About the Package
SQUIRE represents incremental methodological progress in biological parameter estimation, providing a practical tool that organizes existing statistical and optimization methods into a coherent workflow. The package's contribution lies in its systematic approach to configuration selection and workflow organization.

### About the Examples
- **Synthetic data**: Examples use realistic synthetic data based on germination study patterns
- **For real use**: Replace example data with your experimental measurements
- **Data requirements**: Structured time-series with treatment/replicate design

### Methodological Notes
- **Constraint testing**: Tests combinations of three predefined constraint types (T/P/E) rather than discovering novel constraints
- **Statistical validation**: Based on ANOVA; may not capture all biological complexities  
- **Parameter interpretation**: Requires domain knowledge for biological meaning
- **Computational cost**: Systematic testing requires multiple optimization runs

### Dependencies
- **GALAHAD package**: Required for geometry-adaptive optimization
- **stats package**: Used for ANOVA and statistical functions

---

## Advanced Usage

### Custom Validation Criteria
```r
# Stricter statistical requirements
results <- SQUIRE(
  data = my_data,
  treatments = c("Control", "Treatment_A", "Treatment_B"),
  validation_level = 0.01,  # Require p < 0.01
  min_timepoints = 8,       # Require >= 8 timepoints
  min_replicates = 5,       # Require >= 5 replicates
  verbose = TRUE
)
```

### Constraint Verification
```r
# Verify selected constraints were applied
if (results$optimization_performed) {
  config <- results$galahad_settings$optimal_config
  params <- results$parameters$parameter_matrix
  
  # Check constraint satisfaction
  if (length(config$P) > 0) {
    positive_check <- all(params[, config$P, drop = FALSE] > 0)
    cat("Positive constraints satisfied:", positive_check, "\n")
  }
}
```

---

## Citation

When using SQUIRE in publications, please cite:

```
Feiss, R. A. (2025). SQUIRE: Statistical Quality-Assured Integrated Response Estimation. 
R package version 1.0.1. https://CRAN.R-project.org/package=SQUIRE
```

**Please also cite GALAHAD** as SQUIRE depends on this optimization framework:

```
Feiss, R. A. (2025). GALAHAD: Geometry-Adaptive Lyapunov-Assured Hybrid Optimizer. 
R package version 1.0.0. https://CRAN.R-project.org/package=GALAHAD
```

---

## Development & Support

- **Institution**: Minnesota Center for Prion Research and Outreach (MNPRO), University of Minnesota
- **GitHub**: https://github.com/RFeissIV  
- **Email**: feiss026@umn.edu
- **Issues**: Please report bugs and feature requests on GitHub

---

## What's New in v1.0.1

### Major Features:
- **Initial CRAN release** with systematic constraint configuration testing
- **GALAHAD integration** for geometry-adaptive optimization
- **Statistical validation framework** with ANOVA-based gates
- **T/P/E systematic testing** for selecting optimal constraint configurations from predefined options

### Key Capabilities:
- Prevents optimization on statistically insignificant data
- Systematically tests and selects appropriate parameter constraints
- Applies selected geometry during optimization (not post-hoc)
- Provides uncertainty quantification for biological interpretation

---

## Value Proposition

SQUIRE's practical value includes:
1. **Workflow organization**: Combines existing methods in a structured pipeline
2. **Time savings**: Automates comparison of constraint configurations
3. **Statistical rigor**: Prevents optimization on non-significant data
4. **Reproducibility**: Consistent methodology across analyses
5. **Integration**: Clean interface between statistical testing and geometric optimization

---

## Human-AI Development Transparency

Development followed an **iterative human-machine collaboration**. All algorithmic design, statistical methodologies, and biological validation logic were conceptualized and developed by Richard A. Feiss.

AI systems (*Anthropic Claude* and *OpenAI GPT*) served as coding and documentation assistants under continuous human oversight, helping with:

- Code optimization and syntax validation
- Statistical method verification
- Documentation consistency and clarity
- Package compliance checking

AI systems did **not** originate algorithms, statistical approaches, or scientific methodologies.

---

## License

MIT License. See [LICENSE](LICENSE) file for details.
