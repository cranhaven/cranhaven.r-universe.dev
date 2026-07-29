# nhanesR

Download, harmonize, and analyze NHANES data with mortality linkage.

## Overview

`nhanesR` provides a structured workflow for working with
[NHANES](https://www.cdc.gov/nchs/nhanes/) (National Health and Nutrition
Examination Survey) public-use data and the NCHS Public-Use Linked Mortality
Files (LMF). It handles the main friction points in multi-cycle NHANES
analysis:

- File names change across cycles (e.g. total cholesterol: `LAB13` → `L13_B`
  → `L13_C` → `TCHOL_D` onward)
- Variable names change across cycles (e.g. HDL: `LBDHDL` → `LBXHDD` →
  `LBDHDD`)
- SI-unit duplicate columns appear alongside conventional-unit columns in the
  same file
- Mortality linkage requires fixed-width file parsing and SEQN joining across
  cycles

## Installation

```r
# install.packages("remotes")
remotes::install_github("dwinsemius/nhanesR", build_vignettes = TRUE, force = TRUE)
```

**Requirements:** R ≥ 4.1.0. The following packages are used optionally and
will be requested if needed:

- `rvest` — required for `nhanes_manifest()` and `nhanes_search_variables()`
- `foreign` — fallback parser for older XPT files (pre-2003 cycles)
- `survey`, `survival` — for the vignette examples

## Quick start

```r
library(nhanesR)

# 1. Find available cycles
cycles <- nhanes_cycles()[["cycle"]]   # character vector of all cycle labels

# 2. Browse files available in a cycle
nhanes_manifest("2015-2016", "Laboratory")

# 3. Search the variable catalog by keyword
nhanes_search_variables("total cholesterol", component = "Laboratory")
nhanes_variable_map("total cholesterol")   # per-cycle file/variable lookup

# 4. Download — use nhanes_download_analyte() when file names changed across cycles
demo_list  <- nhanes_download("DEMO", cycles[1:10])          # stable name
tchol_list <- nhanes_download_analyte("total cholesterol",   # resolves renames
                                       cycles[1:10])

# 5. Harmonize variable names and stack into one data frame
TC <- nhanes_harmonize(tchol_list,
                        unit          = "mg/dL",
                        name          = "TC_mgdl",
                        label_pattern = "total cholesterol")

# 6. Merge components by SEQN
demo   <- nhanes_stack(demo_list)
analytic <- nhanes_merge(demo, TC, by = c("SEQN", "cycle"))

# 7. Link mortality follow-up (through December 31, 2019)
analytic_mort <- nhanes_mortality_link(analytic)

# 8. Prepare survival dataset
surv_data <- nhanes_survival_prep(analytic_mort,
                                   origin     = "exam",
                                   time_unit  = "years",
                                   weight_var = "WTMEC2YR")
```

## Configuration

Downloaded files are cached locally. Three options control behavior — set
them in `~/.Rprofile` to make changes permanent:

```r
options(
  nhanesR.cache_dir = "/path/to/cache",   # default: tempdir()/nhanesR (session only)
  nhanesR.verbose   = FALSE,              # suppress progress messages
  nhanesR.timeout   = 300L               # HTTP timeout in seconds
)
```

View or change the cache location interactively:

```r
nhanes_cache_dir()                        # show current path
nhanes_cache_dir("~/my_nhanes_cache")     # change for this session
```

## Vignettes

Two vignettes are included:

```r
# Package overview and complete function map
vignette("nhanesR-overview", package = "nhanesR")

# Full worked example: TC/HDL and all-cause mortality across 10 cycles
# with survey-weighted Cox proportional hazards model
vignette("nhanes-mortality-workflow", package = "nhanesR")
```

## Function reference

| Stage | Functions |
|-------|-----------|
| Discovery | `nhanes_cycles()`, `nhanes_manifest()` |
| Variable search | `nhanes_search_variables()`, `nhanes_variable_map()` |
| Download | `nhanes_download()`, `nhanes_download_analyte()` |
| Harmonize / stack / merge | `nhanes_harmonize()`, `nhanes_stack()`, `nhanes_merge()` |
| Variable labelling | `NH_label()`, `NH_describe()` |
| Mortality linkage | `nhanes_mortality_link()`, `nhanes_mortality_download()`, `nhanes_mortality_parse()`, `nhanes_lmf_cycles()` |
| Survival prep | `nhanes_survival_prep()`, `nhanes_followup_summary()`, `nhanes_ucod_labels()` |
| Survey-weighted Cox | `svycph_fuse()`, `weighted_basehaz()`, `svycph_set_basehaz()` |
| Cache | `nhanes_cache_dir()` |

## Acknowledgements

Developed with assistance from [Claude Code](https://claude.ai/code) (Anthropic).
