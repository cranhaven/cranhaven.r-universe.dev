HealthMarkers
================

- [HealthMarkers](#healthmarkers)
  - [Installation](#installation)
  - [Quick start](#quick-start)
  - [Package overview](#package-overview)
  - [Which function do I need?](#which-function-do-i-need)
  - [All markers dispatcher
    `all_health_markers()`:](#all-markers-dispatcher-all_health_markers)
  - [Column mapping](#column-mapping)
  - [Multi-biobank automatic column
    recognition](#multi-biobank-automatic-column-recognition)
  - [Selected function families](#selected-function-families)
  - [Common problems and solutions](#common-problems-and-solutions)
  - [Missing data](#missing-data)
  - [Normalisation](#normalisation)
  - [Output utilities](#output-utilities)
  - [Citation policy](#citation-policy)
  - [Articles and further reading](#articles-and-further-reading)
  - [Development status and validated
    publications](#development-status-and-validated-publications)
  - [Contributing](#contributing)
  - [Citation](#citation)
  - [License](#license)
  - [AI use disclaimer](#ai-use-disclaimer)

<!-- badges: start -->

[![R-CMD-check](https://github.com/sufyansuleman/HealthMarkers/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sufyansuleman/HealthMarkers/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

# HealthMarkers

You have a large data frame of clinical and lab measures. You want
HOMA-IR, AIP, eGFR, NLR, and FIB-4, without writing a single formula.
That is what HealthMarkers does.

**HealthMarkers** is an R toolkit for computing, standardising, and
summarising clinical and research biomarkers directly from routine
laboratory and phenotypic data. Over 50 specialist functions cover more
than 290 biomarkers: from insulin sensitivity indices and cardiovascular
risk scores to inflammatory aging clocks, frailty indices, psychiatric
rating scales, and alternate-biofluids, all accessible through a single
unified dispatcher, `all_health_markers()`.

> **Full documentation, function reference, and articles** are available
> at:\
> <https://sufyansuleman.github.io/HealthMarkers/>

**Key features:**

- **Broad coverage.** One `all_health_markers()` call returns glycaemic,
  lipid, liver, renal, pulmonary, inflammatory, hormonal, bone,
  psychiatric, nutritional, and frailty markers as a single wide tibble.
- **Works with your column names as-is.** The built-in synonym
  dictionary covers 15+ cohorts and biobanks (UK Biobank, NHANES, HUNT,
  Tromsø, FinnGen, Estonian Biobank, LifeLines, Generation Scotland,
  Danish NPU codes, OMOP/LOINC codes, and more). In most cases you do
  not need to rename anything.
- **Safe defaults.** NA handling, input validation, and column-name
  inference are built in. A failed marker group is skipped with a
  warning; your pipeline never crashes.
- **Fully traceable.** Every function cites its primary source paper.
  Full bibliography in `inst/REFERENCES.bib`.

**Typical workflow (four steps):**

``` r
library(HealthMarkers)

# 1. See which columns in your data are recognised automatically
hm_col_report(my_data)

# 2. (If needed) fill in any unmatched keys
col_map <- list(eGFR = "GFR_CKD_EPI", G0 = "fasting_glucose_mmol")

# 3. Compute the markers you need
results <- all_health_markers(my_data,
                               which   = c("glycemic", "lipid", "renal"),
                               col_map = col_map)

# 4. Inspect, summarise, or export the new columns
new_cols <- setdiff(names(results), names(my_data))
health_summary(results[, new_cols])
```

------------------------------------------------------------------------

## Installation

``` r
# From CRAN
install.packages("HealthMarkers")

# Development version from GitHub
remotes::install_github("sufyansuleman/HealthMarkers")
```

Several marker groups require optional packages. Install only what you
need:

``` r
install.packages(c(
  "CVrisk",        # Framingham / basic CVD risk
  "PooledCohort",  # ASCVD Pooled Cohort Equations + stroke risk
  "QRISK3",        # QRISK3
  "RiskScorescvd", # SCORE2 / SCORE2-OP
  "rspiro",        # Spirometry GLI 2012 z-scores
  "di",            # DXA-based insulin sensitivity
  "mice",          # Multiple imputation
  "missForest"     # Random-forest imputation
))
```

When an optional package is absent, its marker group is skipped safely;
`verbose = TRUE` reports which groups ran and which were skipped.

------------------------------------------------------------------------

## Quick start

The package ships a simulated dataset (`n = 200`, 100+ columns) so you
can run every example without your own data.

``` r
library(HealthMarkers)

sim_path  <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim       <- readRDS(sim_path)
sim_small <- sim[1:50, ]   # small subset for speed
```

**Step 1: find out what is already recognised in your data:**

``` r
hm_col_report(sim_small)
```

**Step 2: compute several marker groups at once:**

``` r
out <- all_health_markers(
  data    = sim_small,
  which   = c("glycemic", "lipid", "renal", "inflammatory"),
  verbose = FALSE
)

# How many new columns were added?
new_cols <- setdiff(names(out), names(sim_small))
length(new_cols)

# Preview the new markers
head(out[, new_cols])
```

> Note: this example is marked `eval=FALSE` in the README so rendering
> stays fast. Run it interactively if you want to compute the full demo
> output.

**Step 3: call a single-purpose function when you need one biomerker or
one gorup only:**

``` r
# Atherogenic index of plasma (log TG/HDL): two columns in, one index out
aip <- cvd_marker_aip(sim_small, col_map = list(TG = "TG", HDL_c = "HDL_c"),
                      na_action = "keep")
head(aip[, setdiff(names(aip), names(sim_small))])
```

------------------------------------------------------------------------

## Package overview

| Domain | Functions | Key outputs |
|----|----|----|
| **Insulin sensitivity** | `fasting_is()`, `ogtt_is()`, `adipo_is()`, `tracer_dxa_is()`, `all_insulin_indices()` | HOMA-IR, QUICKI, Matsuda, Stumvoll, Gutt, SPISE, LIRI, 40+ indices |
| **Glycaemic** | `glycemic_markers()` | TyG index, METS-IR, LAR, ASI, HOMA-CP, diabetes risk flags |
| **Lipid & atherogenic** | `lipid_markers()`, `atherogenic_indices()`, `cvd_marker_aip()`, `cvd_marker_ldl_particle_number()` | TC/HDL, AIP, CRI-I/II, Castelli, LDL particle number |
| **Liver** | `liver_markers()`, `liver_fat_markers()` | FLI, NFS, FIB-4, APRI, BARD, ALBI, MELD-XI, HSI, LAP |
| **Metabolic syndrome** | `metss()`, `metabolic_risk_features()`, `allostatic_load()` | MetS severity score, component flags, allostatic load index |
| **Cardiovascular risk** | `cvd_risk()`, `cvd_risk_ascvd()`, `cvd_risk_qrisk3()`, `cvd_risk_scorescvd()`, `cvd_risk_stroke()` | ASCVD (PCE), QRISK3, SCORE2/SCORE2-OP, 10-yr stroke risk |
| **Renal / CKD** | `kidney_failure_risk()`, `renal_markers()`, `ckd_stage()`, `urine_markers()` | KFRE 2-yr/5-yr, eGFR (CKD-EPI), CKD stage, UACR, FE-Urea |
| **Pulmonary** | `pulmo_markers()`, `spirometry_markers()`, `bode_index()` | FEV1/FVC z-scores, GLI 2012 % predicted, BODE index |
| **Inflammatory & aging** | `inflammatory_markers()`, `iAge()`, `oxidative_markers()`, `kyn_trp_ratio()` | NLR, PLR, SII, LMR, iAge clock, 8-OHdG, KTR |
| **Hormonal** | `hormone_markers()` | T/E2 ratio, TSH/fT4, cortisol/DHEA, LH/FSH, HOMA-B, FAI |
| **Body composition** | `obesity_indices()`, `adiposity_sds()`, `adiposity_sds_strat()`, `alm_bmi_index()`, `calc_sds()` | BMI, WHR, ABSI, BRI, BAI, sex/age-stratified SDS, ALM/BMI |
| **Bone & fracture** | `bone_markers()`, `frax_score()` | P1NP, osteocalcin, CTX, NTX, FRAX 10-yr fracture probability |
| **Frailty & comorbidity** | `frailty_index()`, `charlson_index()`, `sarc_f_score()` | Rockwood deficit index, Charlson CCI, SARC-F |
| **Vitamins & nutrients** | `vitamin_markers()`, `vitamin_d_status()`, `nutrient_markers()` | Vitamin D status category, B12/folate ratio, ferritin saturation |
| **Alternate biofluids** | `saliva_markers()`, `sweat_markers()`, `urine_markers()` | Cortisol awakening response, sweat chloride, urinary ratios |
| **Neurological** | `nfl_marker()`, `kyn_trp_ratio()`, `corrected_calcium()` | Age-adjusted NfL, kynurenine/tryptophan ratio, corrected calcium |
| **Psychiatric** | `psych_markers()`, `phq9_score()`, `gad7_score()`, `k6_score()`, `k10_score()`, and more | PHQ-9, GAD-7, ISI, GHQ-12, K10, K6, WHO-5, ASRS, BIS-11, SPQ |

------------------------------------------------------------------------

## Which function do I need?

| I have… | I want… | Use |
|----|----|----|
| Fasting glucose + insulin | HOMA-IR, QUICKI, FIRI, 15+ fasting indices | `fasting_is()` |
| OGTT glucose + insulin (multiple time points) | Matsuda, Stumvoll, Gutt, Avignon, 25+ OGTT indices | `ogtt_is()` |
| Lipid panel (TC, HDL, LDL, TG) | AIP, CRI, Castelli, TC/HDL ratio, LDL particle number | `lipid_markers()` / `cvd_marker_aip()` |
| Age, sex, cholesterol, BP, smoking | 10-year ASCVD / QRISK3 / SCORE2 cardiovascular risk | `cvd_risk_ascvd()` / `cvd_risk_qrisk3()` / `cvd_risk_scorescvd()` |
| Creatinine, age, sex (± UACR) | eGFR, CKD stage, KFRE kidney failure probability | `renal_markers()` / `ckd_stage()` / `kidney_failure_risk()` |
| FEV1, FVC, age, height | Spirometry z-scores (GLI 2012) | `spirometry_markers()` |
| CBC (neutrophils, lymphocytes, platelets) | NLR, PLR, SII, LMR inflammatory ratios | `inflammatory_markers()` |
| BMI, waist, hip | ABSI, BRI, BAI, WHR, obesity indices | `obesity_indices()` |
| Height, weight ± DXA measures | Body composition SDS z-scores | `adiposity_sds()` |
| Questionnaire item columns | PHQ-9, GAD-7, K10, ISI, GHQ-12, MDQ scores | `psych_markers()` / `phq9_score()` / `gad7_score()` |
| Many lab variables at once | All of the above in one call | `all_health_markers(which = "all")` |

------------------------------------------------------------------------

## All markers dispatcher `all_health_markers()`:

Use this when you have a large data set with many variables and want to
compute many marker groups in one call, returned as a single wide
tibble:

``` r
results <- all_health_markers(
  data    = my_data,
  which   = c("glycemic", "lipid", "liver", "renal", "mets", "inflammatory"),
  col_map = list(G0 = "fasting_glucose", I0 = "insulin0"),
  verbose = TRUE
)
```

All available `which` group keys:

    insulin_fasting     insulin_ogtt        insulin_adipose     insulin_tracer_dxa
    glycemic            lipid               atherogenic         cvd_aip
    cvd_risk            cvd_ldl_particles   cvd_ascvd           cvd_qrisk3
    cvd_scorescvd       cvd_stroke          liver               liver_fat
    mets                metabolic_risk      allostatic_load     pulmo
    spirometry          bode                saliva              sweat
    urine               renal               kidney_kfre         ckd_stage
    nutrient            vitamin             vitamin_d_status    hormone
    inflammatory        iAge                bone                frax
    oxidative           allostatic_load     frailty_index       charlson
    sarc_f              psych               nfl                 inflammatory_age
    calcium_corrected   kyn_trp             adiposity_sds       adiposity_sds_strat
    obesity_metrics     alm_bmi

Pass `which = "all"` to run everything. Groups requiring unavailable
optional packages are silently skipped.

### Returning only the computed markers (not the full input)

By default `all_health_markers()` appends the new marker columns to your
original data and returns everything together. With large cohort data
(e.g. 40,000 rows × 300 columns) this doubles the width of the object.
Use `return_input = FALSE` to get back **only the newly computed
columns**, keeping your pipeline lean:

``` r
# Default: original columns + new markers (wide output)
out <- all_health_markers(my_data, which = c("glycemic", "lipid"))

# Markers only: much smaller result
markers_only <- all_health_markers(
  my_data,
  which        = c("glycemic", "lipid", "renal"),
  return_input = FALSE,
  id_col       = "participant_id"   # carry the ID so you can join back later
)

# Join back when you need the full picture
final <- dplyr::left_join(my_data, markers_only, by = "participant_id")
```

`id_col` is optional; omit it if you don’t need to join back, or if row
order is sufficient.

------------------------------------------------------------------------

## Column mapping

### Step 1: `hm_col_report()` - see what is detected

Call this **before** any computation to get a full report of which
columns are auto-matched and which need manual mapping:

``` r
hm_col_report(my_data)
```

Output:

    ── HealthMarkers column report ────────────────────────────────────────────
     Data: 40314 rows × 299 columns   |   Keys in dictionary: 258

     key                  data_column        how matched
     -------------------- ------------------ ------------------
     fasting_glucose      pglu0              exact  ✔
     TG                   trig               exact  ✔
     ALT                  alat               exact  ✔
     eGFR                 ─                  NOT FOUND ✘

     ✔ 187 keys matched   ✘ 71 keys not found

    ── col_map template for missing keys ──────────────────────────────────────
     col_map <- list(
       eGFR  = "from_your_data",
     )

### Step 2: fill in unmatched keys

`hm_col_report()` returns a named list with every auto-detected key
already mapped. Add only the missing keys to that list, then pass the
completed `col_map` to your marker function.

``` r
cm        <- hm_col_report(my_data, verbose = FALSE)  # returns named list
cm$eGFR   <- "GFR_ckdepi"   # add your column name for any unmatched key
cm$G0     <- "fasting_glucose_mmol"  # if not already matched
```

### Step 3: pass `col_map` to any function

``` r
all_health_markers(my_data, which = c("renal", "glycemic"), col_map = cm)
fasting_is(my_data, col_map = list(G0 = "fasting_glucose_mmol", I0 = "insulin_uU_mL"))
```

`hm_col_report()` options:

``` r
hm_col_report(my_data, show_unmatched = TRUE)  # also list every unmatched key
hm_col_report(my_data, fuzzy = TRUE)           # add fuzzy matching as last resort
```

### Common internal keys

| Key | Meaning | Example column names |
|----|----|----|
| `G0` | Fasting glucose (mmol/L) | `pglu0`, `fasting_glucose`, `LBXGLU`, `paastoglukoosi` |
| `I0` | Fasting insulin (mU/L) | `insu0`, `insulin0`, `ins_fast` |
| `G30`, `G120` | 30-/120-min OGTT glucose | `pglu30`, `pglu120` |
| `I30`, `I120` | 30-/120-min OGTT insulin | `insu30`, `insu120` |
| `TG` | Triglycerides (mmol/L) | `trig`, `TAG`, `triglyserider`, `LOINC_2571_8` |
| `HDL_c` | HDL cholesterol | `hdlc`, `HDL`, `hdl_kolesteroli`, `LOINC_2085_9` |
| `LDL_c` | LDL cholesterol | `ldl`, `LDL`, `ldl_kolesteroli`, `LOINC_13457_7` |
| `TC` | Total cholesterol | `chol`, `kokonaiskolesteroli`, `LOINC_2093_3` |
| `ALT` | Alanine aminotransferase | `alat`, `SGPT`, `LBXSATSI`, `NPU03429`, `LOINC_1742_6` |
| `creatinine` | Serum creatinine | `crea`, `kreatinin`, `NPU01994`, `LOINC_2160_0` |
| `UACR` | Urine albumin/creatinine ratio | `ualbcrea`, `ACR` |
| `SBP` / `DBP` | Systolic/diastolic BP | `sysbp`, `systolisk_blodtrykk`, `LOINC_8480_6` |
| `BMI` | Body mass index | `bmi`, `painoindeksi`, `LOINC_39156_5` |
| `waist` | Waist circumference (cm) | `waist_cm`, `WC`, `midjeomkrets` |
| `vitaminD` | 25-OH vitamin D (nmol/L) | `vitd25`, `d_vitamin`, `NPU10501`, `LOINC_62292_8` |
| `HbA1c` | Glycated haemoglobin (%) | `hba1c`, `hemoglobiini_a1c`, `NPU27300`, `LOINC_4548_4` |

------------------------------------------------------------------------

## Multi-biobank automatic column recognition

The synonym dictionary recognises column names from 15+ major cohorts
and biobanks. The same analyte across systems:

| Internal key | UK Biobank | NHANES | HUNT/Tromsø | FinnGen | Estonian BB | LifeLines (NL) | LOINC |
|----|----|----|----|----|----|----|----|
| `fasting_glucose` | `glucose_0_0` | `LBXGLU` | `fastende_blodsukker` | `paastoglukoosi` | `p_glukoos` | `nuchtere_glucose` | `LOINC_2345_7` |
| `total_cholesterol` | `cholesterol_0_0` | `LBXSCH` | `total_kolesterol` | `kokonaiskolesteroli` | `kogukolesterool` | `totaal_cholesterol` | `LOINC_2093_3` |
| `creatinine` | `creatinine_0_0` | `LBXSCR` | `kreatinin` | `kreatiniini` | `kreatiniin` | `creatinine` | `LOINC_2160_0` |
| `HbA1c` | `glycated_haemoglobin_hba1c_0_0` | `LBXGH` | `HbA1c` | `hemoglobiini_a1c` | `HbA1c` | `geglycosyleerd_hemoglobine` | `LOINC_4548_4` |
| `SBP` | `systolic_blood_pressure_0_0` | `BPXSY1` | `systolisk_blodtrykk` | `SBP` | `sbp` | `systolische_bloeddruk` | `LOINC_8480_6` |
| `vitaminD` | `vitamin_d_0_0` | `LBXVD2` | `d_vitamin` | `D_vitamiini` | `D_vitamiin` | `vitamine_D` | `LOINC_62292_8` |
| `ALT` | `alanine_aminotransferase_0_0` | `LBXSATSI` | `ALAT` | `alaniiniaminotransferaasi` | `ALAT` | `alanineaminotransferase` | `LOINC_1742_6` |

Also supported: **Danish NPU codes** (`NPU01994`, `NPU01567`, etc.),
**Generation Scotland** (`SBP_mean`, `DBP_mean`, `genetic_sex`),
**SCAPIS/TwinGene** Swedish terms, and **OMOP CDM / All of Us** LOINC
concept codes for all major analytes.

------------------------------------------------------------------------

## Selected function families

Use an individual function when you need one specific domain, want
fine-grained control over `col_map`, or are working with specialist data
formats (OGTT time-series, DXA output, spirometry). For reference pages
and formulas, use `?function_name`.

### Insulin sensitivity

``` r
# Fasting indices (HOMA-IR, QUICKI, Bennett, FIRI, ...)
fasting_is(data, col_map = list(G0 = "glucose", I0 = "insulin"))

# OGTT indices (Matsuda, Stumvoll, Gutt, Avignon, ...)
ogtt_is(data, col_map = list(G0="G0", G30="G30", G60="G60", G120="G120",
                              I0="I0", I30="I30", I60="I60", I120="I120"))

# Adipose-tissue indices (LIRI, SPISE, VAI, LAP, ...)
adipo_is(data, col_map = list(BMI="BMI", WC="WC", TG="TG", HDL_c="HDL_c"))

# DXA / tracer-based indices
tracer_dxa_is(data, col_map = list(fat_mass="FM_kg", lean_mass="LM_kg"))

# All insulin indices at once (fasting + OGTT + adipose + DXA)
all_insulin_indices(data, col_map = list(...), normalize = "none",
                    mode = "both",   # "IS" sensitivity only, "IR" resistance only
                    na_action = "keep")
```

### Cardiovascular risk

``` r
# ASCVD Pooled Cohort Equations (requires PooledCohort)
cvd_risk_ascvd(data, year = 10)

# QRISK3 (UK population, requires QRISK3)
cvd_risk_qrisk3(data)

# SCORE2 / SCORE2-OP (European, requires RiskScorescvd)
cvd_risk_scorescvd(data)

# Atherogenic index of plasma
cvd_marker_aip(data, col_map = list(TG = "TG", HDL_c = "HDL_c"))

# LDL particle number from ApoB
cvd_marker_ldl_particle_number(data, col_map = list(ApoB = "ApoB"))

# Run all CVD algorithms at once
cvd_risk(data, model = "ALL")
```

### Renal function

``` r
# Kidney Failure Risk Equation (KFRE) 2-year and 5-year
kidney_failure_risk(data, col_map = list(age="age", sex="sex",
                                          eGFR="eGFR", UACR="UACR"))

# eGFR, BUN/creatinine, FE-Urea, renal ratios
renal_markers(data, col_map = list(creatinine="Creat", age="age", sex="sex"))

# KDIGO CKD staging (G1–G5 × A1–A3)
ckd_stage(data, col_map = list(eGFR="eGFR", UACR="UACR"))

# Urine panel: protein/creatinine ratio, microalbumin, osmolality
urine_markers(data, col_map = list(urine_creat="UCr", urine_protein="UPr"))
```

### Pulmonary function

``` r
# Spirometry z-scores and % predicted (GLI 2012, requires rspiro)
spirometry_markers(data, col_map = list(fev1="FEV1", fvc="FVC",
                                         age="age", height="ht_cm", sex="sex"))

# Simple pulmonary ratios (no extra packages)
pulmo_markers(data)

# BODE index for COPD prognosis
bode_index(data, col_map = list(fev1_pct="FEV1pct", sixmwd="Walk6m",
                                  mmrc="mMRC", bmi="BMI"))
```

### Body composition and anthropometric SDS

``` r
# Common obesity and adiposity indices (BMI, WHR, ABSI, BRI, BAI, ...)
obesity_indices(data)

# SDS z-score from any reference mean and SD
calc_sds(x = data$BMI, mean_ref = 22.5, sd_ref = 3.8)

# Sex-stratified SDS for multiple adiposity variables
adiposity_sds_strat(data, col_map = list(sex = "sex"),
                    var_cols = c("BMI", "WC", "WHR"),
                    ref_male   = list(BMI = c(mean = 25, sd = 4)),
                    ref_female = list(BMI = c(mean = 24, sd = 3.8)))

# Appendicular lean mass / BMI index (sarcopenia screening)
alm_bmi_index(data, col_map = list(alm = "ALM_kg", bmi = "BMI", sex = "Sex"))
```

### Psychiatric scores

``` r
# Score any combination of standardised scales from item columns.
# Supported: PHQ-9, GAD-7, K6, K10, GHQ-12, WHO-5, ISI, MDQ,
#            ASRS, BIS-11, SPQ, cognitive composite
psych_markers(
  data,
  col_map = list(
    phq9 = list(items = list(phq9_01 = "Q1", phq9_02 = "Q2")),
    gad7 = list(items = list(gad7_01 = "G1", gad7_02 = "G2"))
  ),
  which = c("phq9", "gad7", "k10")
)

# If columns are already named phq9_01 ... phq9_09 etc., no col_map needed:
phq9_score(data)
gad7_score(data)
k10_score(data)
```

### Inflammatory and aging markers

``` r
# Blood count-derived ratios (NLR, PLR, SII, LMR, ...)
inflammatory_markers(data, col_map = list(neut="NEUT", lymph="LYMPH",
                                           mono="MONO", plt="PLT"))

# iAge inflammatory aging clock
iAge(data, col_map = list(IL6="IL6", CXCL9="CXCL9"))
```

### Alternate biofluids

``` r
saliva_markers(data, col_map = list(cortisol_wake="C_wake", cortisol_30="C_30min"))
sweat_markers(data,  col_map = list(sweat_chloride="Cl_mmol"))
urine_markers(data,  col_map = list(urine_creat="UCr", urine_na="UNa"))
```

------------------------------------------------------------------------

## Common problems and solutions

| Symptom | Likely cause | Fix |
|----|----|----|
| A marker column is all `NA` | Required input column not found or all-missing | Run `hm_col_report(data)` to see which keys are unmatched; add them to `col_map` |
| An entire group is silently skipped | Optional package not installed, or no matched columns for that group | Set `verbose = TRUE` to see which groups ran and why others were skipped |
| `hm_col_report()` shows 0 matches | Column names not in the synonym dictionary | Use `col_map` to manually map your names to internal keys |
| ASCVD / QRISK3 / SCORE2 result is all `NA` | Optional package (`PooledCohort`, `QRISK3`, `RiskScorescvd`) not installed | Install the relevant package; see Installation section |
| Spirometry z-scores are `NA` | `rspiro` not installed, or `ethnicity` column missing/wrong codes | Install `rspiro`; see `?spirometry_markers` for ethnicity code table |
| Output is very wide (300+ columns) | `which = "all"` on a rich dataset | Use `which = c("glycemic", "lipid", ...)` to select only the groups you need, or `return_input = FALSE` to get markers only |
| `Error: object 'x' not found` in examples | Using `my_data` placeholder from README | Replace with your actual data object |

------------------------------------------------------------------------

## Missing data

All marker functions accept a `na_action` argument:

``` r
# "keep"  : compute what is possible, return NA where inputs are missing (default)
# "omit"  : drop rows with any missing required input before computing
# "error" : abort if any required input is missing
renal_markers(data, na_action = "keep")
```

> **`na_action` aliases:** `"ignore"` is a backward-compatible alias for
> `"keep"` (same behaviour; retained so older code continues to work).
> `"warn"` is also an alias for `"keep"` that additionally emits a
> missingness warning. If you are reading a function’s help page and see
> `"ignore"` listed first in the choices, it behaves identically to
> `"keep"`.

For pre-computation imputation, use the built-in helpers:

``` r
# Multiple imputation via mice
imputed <- impute_missing(data, method = "mice", m = 1, maxit = 5)

# Random-forest imputation (good for mixed data types)
imputed <- impute_missing(data, method = "missForest")

# Simple mean/median fill
imputed <- impute_missing(data, method = "mean")

# Then pass imputed data to any marker function
all_health_markers(imputed, which = c("glycemic", "lipid"))
```

------------------------------------------------------------------------

## Normalisation

The `normalize` argument is implemented in the insulin sensitivity
functions (`fasting_is()`, `ogtt_is()`, `adipo_is()`,
`all_insulin_indices()`). For all other domain functions, use
`hm_normalize()` to normalise outputs after computation.

### `hm_normalize()`: post-computation normalisation

``` r
out <- all_health_markers(data, which = c("glycemic", "lipid", "renal"))

# Identify new marker columns
new_cols <- setdiff(names(out), names(data))

# Normalise only the new marker columns (z-score)
out_z <- hm_normalize(out, cols = new_cols, method = "z")

# Rank-based inverse-normal transform on all numeric columns,
# keeping age and BMI on their original scale
out_int <- hm_normalize(out, method = "inverse", skip_cols = c("age", "BMI"))

# Min-max scaling to [0, 1]
out_range <- hm_normalize(out, cols = new_cols, method = "range")

# Robust median/MAD scaling
out_rob <- hm_normalize(out, cols = new_cols, method = "robust")
```

Available methods:

| Method      | Description                                     |
|-------------|-------------------------------------------------|
| `"z"`       | z-score (mean 0, sd 1)                          |
| `"inverse"` | Rank-based inverse-normal transform (Rankit)    |
| `"range"`   | Min-max to \[0, 1\] (or custom `feature_range`) |
| `"robust"`  | Median/MAD scaling                              |

### `normalize_vec()`: single-vector normalisation

``` r
normalize_vec(x, method = "inverse")
normalize_vec(x, method = "inverse", invnorm_denominator = "blom")
normalize_vec(x, method = "z")
normalize_vec(x, method = "robust")
normalize_vec(x, method = "range", feature_range = c(-1, 1))
```

See `?hm_normalize` and the package website articles page
<https://sufyansuleman.github.io/HealthMarkers/articles/> for full
details.

------------------------------------------------------------------------

## Output utilities

``` r
# Quick numeric summary (n, n_na, mean, sd, median, p25, p75) for any data frame
health_summary(out)

# Summary of marker outputs (variable, mean, sd, IQR)
marker_summary(out)

# Plot frailty-index deficit accumulation against age
plot_frailty_age(data, cols = c("deficit1", "deficit2", "deficit3"), age = "age")
```

------------------------------------------------------------------------

## Citation policy

Every function cites at least one reference in its help page
(`?function_name`). We have made every effort to trace citations back to
the **original primary source**: the paper in which the index, formula,
or scoring system was first described.

In practice this is not always straightforward:

- The commonly cited paper may be a **validation study** or a **derived
  index paper** rather than the mathematical original. In these cases we
  cite the paper most widely used in the clinical or epidemiological
  literature for that formula.
- For **statistical and mathematical methods** (e.g., rank-based
  inverse-normal transformation, median absolute deviation, standardised
  difference scores), the original work may be decades old and the
  specific parameterisation in common use was formalised in a more
  recent methodological paper. We cite whichever source most precisely
  describes the implementation used.
- For **multi-component composite indices** (e.g., allostatic load,
  frailty index, ASCVD Pooled Cohort Equations), multiple papers
  collectively define the algorithm; we aim to cite the most complete or
  most cited specification.

The full bibliography is in `inst/REFERENCES.bib` and is rendered in
each function’s help page via Rdpack.

If you identify a citation that could be improved (a closer original
source, a more authoritative validation paper, or a correction), please
open an issue at
<https://github.com/sufyansuleman/HealthMarkers/issues>.

------------------------------------------------------------------------

## Articles and further reading

Full articles are available on the **package website**:

- **Website:** <https://sufyansuleman.github.io/HealthMarkers/>
- **Function reference** (all arguments, details, and citations):
  <https://sufyansuleman.github.io/HealthMarkers/reference/>
- **All articles / tutorials:**
  <https://sufyansuleman.github.io/HealthMarkers/articles/>

Below are some key articles. For the full list, visit: [All
articles](https://sufyansuleman.github.io/HealthMarkers/articles/)

| Topic | Article |
|----|----|
| Fasting insulin sensitivity (HOMA-IR, QUICKI, 15+ indices) | [fasting_is](https://sufyansuleman.github.io/HealthMarkers/articles/fasting_is.html) |
| OGTT insulin sensitivity (Matsuda, Stumvoll, Gutt, 25+ indices) | [adipo_is](https://sufyansuleman.github.io/HealthMarkers/articles/adipo_is.html) |
| Glycaemic markers (TyG, METS-IR, LAR, diabetes flags) | [glycemic_markers](https://sufyansuleman.github.io/HealthMarkers/articles/glycemic_markers.html) |
| Lipid and atherogenic indices | [lipid_markers](https://sufyansuleman.github.io/HealthMarkers/articles/lipid_markers.html) |
| Cardiovascular risk scores (ASCVD, QRISK3, SCORE2) | [cvd_risk](https://sufyansuleman.github.io/HealthMarkers/articles/cvd_risk.html) |
| Inflammatory ratios and iAge clock | [inflammatory_markers](https://sufyansuleman.github.io/HealthMarkers/articles/inflammatory_markers.html) |
| Hormone markers | [hormone_markers](https://sufyansuleman.github.io/HealthMarkers/articles/hormone_markers.html) |
| Missing data and imputation | [impute_missing](https://sufyansuleman.github.io/HealthMarkers/articles/impute_missing.html) |
| Normalising marker outputs (`hm_normalize`, `normalize_vec`) | [health markers articles](https://sufyansuleman.github.io/HealthMarkers/articles/) |
| All-in-one dispatcher (`all_health_markers`) | [health_markers](https://sufyansuleman.github.io/HealthMarkers/articles/health_markers.html) |

For details on any individual function, use `?function_name` (e.g.,
`?fasting_is`, `?cvd_risk_ascvd`, `?frailty_index`): every help page
includes the formula, argument details, and primary citations.

------------------------------------------------------------------------

## Development status and validated publications

HealthMarkers is under active development. All indices are implemented
from their original, revised or verified published manuscripts. If you
notice an error in any index, please [open an
issue](https://github.com/sufyansuleman/HealthMarkers/issues) so it can
be corrected.

The insulin sensitivity and resistance indices have been independently
verified and are used in the following peer-reviewed publications:

- Suleman S, Madsen AL, Ängquist LH, Schubert M, Linneberg A, Loos RJF,
  Hansen T, Grarup N. Genetic Underpinnings of Fasting and Oral
  Glucose-stimulated Based Insulin Sensitivity Indices. *J Clin
  Endocrinol Metab.* 2024;109(11):2754–2763. [PMID
  38635292](https://pubmed.ncbi.nlm.nih.gov/38635292/)

- Suleman S, Ängquist L, Linneberg A, Hansen T, Grarup N. Exploring the
  genetic intersection between obesity-associated genetic variants and
  insulin sensitivity indices. *Sci Rep.* 2025;15:15761. [PMID
  40328835](https://pubmed.ncbi.nlm.nih.gov/40328835/)

------------------------------------------------------------------------

## Contributing

Issues and pull requests are welcome at
<https://github.com/sufyansuleman/HealthMarkers/issues>.

When contributing a new marker function please:

1.  Add a unit test in `tests/testthat/` with at least one numeric
    check.
2.  Add a `@references` entry in the roxygen block and cite the primary
    paper in `inst/REFERENCES.bib`.
3.  Register the function in the `all_health_markers()` dispatcher if it
    fits an existing domain.
4.  Add or update the relevant article in `vignettes/articles/`
    (articles are published to the pkgdown site at
    <https://sufyansuleman.github.io/HealthMarkers/articles/> but are
    not bundled with the CRAN package).

------------------------------------------------------------------------

## Citation

``` r
citation("HealthMarkers")
```

------------------------------------------------------------------------

## License

MIT Sufyan Suleman ([ORCID
0000-0001-6612-6915](https://orcid.org/0000-0001-6612-6915))

------------------------------------------------------------------------

## AI use disclaimer

OpenAI (ChatGPT) and Anthropic Claude were used during the development
of this package to assist with code refinement, debugging, and editing
of documentation content. All outputs were reviewed, verified, and
approved by the author.
