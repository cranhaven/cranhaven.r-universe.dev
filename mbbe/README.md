# mbbe <img src='vignettes/images/mbbe_logo.png' align="right"  style = "float:right; height: 150px;" />

[![](https://img.shields.io/badge/devel%20version-0.1.0-green.svg)](https://github.com/certara/mbbe)

## Installation

### Development

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("certara/mbbe")
```

### CRAN

```r
install.packages("mbbe")
```

## Model Based Bioequivalence (MBBE)

Funded by FDA/NIH grant 1U01FD007355 - Model Based Bioequivalence

1.  Read the nmodels 
2.  Parse the data file name 
3.  Read the data file name, count the number of subjects.
4.  Generate samp.size bootstrap samples (currently 100), and write to data_rep??.csv where ?? is the sample number (currently 1-100).
5.  Edit the model file to read the data_rep??.csv file, copy new model file to b?.mod in each model?/? folder where ? is the model number (currently 1-5).
6.  Run bootstrap.
7.  Read the xml file from each bootstrap sample and construct a data frame with nmodels + 1 columns. The columns will contain the BIC for each model, each sample. Included in bootstrap sample evaluation will be identifability, as defined in "Model selection and averaging of nonlinear mixed-effect models for robust phase III dose selection". Delta.parms is the absolute fractional difference between parameters between pre and post SADDLE_REST criteria for identifability. The final column will contain an integer for which model is "best" (lowest BIC) for each sample.
8.  Simulate samp.size Monte Carlo samples using the "best" model and the parameter estimates for that sample/that model. Print out PK data from `$TABLE`, including treatment and sequence.
9.  The Monte Carlo simulation can be done using the original NONMEM estimation model, or (more likely), a user provided data set that reflects the study design of the virtual study. For example, the data set may include multiple studies, sparse data, other populations, other doses etc. Regardless of the actual study designs, the virtual study designs can reflect a standard BE study, e.g, a replicate cross over design.
10. Run NCA on the resulting data (using PKNCA).
11. Do TOST on NCA for each Monte Carlo simulation (not done yet).
12. Calculate power.

Command to run is:

```r
library(mbbe)
run_mbbe_json("Args.json")
```

Where Args.json is the path to the json file with the arguments. An example of the contents of an Args.json file is below:

``` json
{
"run_dir": "c:/fda/mbbe/",
"model_source": "u:/fda/mbbe/mbbe/",
"nmodels": 2,
"ngroups": 4,
"samp_size": 8,
"reference_groups": [1,2],
"test_groups": [3,4],
"numParallel": 16,
"crash_value": 999999,
"nmfe_path": "c:/nm744/util/nmfe74.bat",
"delta_parms": 0.1,
"use_check_identifiable": true,
"NCA_end_time": 72,
"rndseed": 1,
"use_simulation_data": true,
"simulation_data_path": "U:/fda/mbbe/mbbe/data_sim.csv" 
}
```
