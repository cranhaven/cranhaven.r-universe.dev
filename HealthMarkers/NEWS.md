# HealthMarkers 0.1.2

## New features

* **`hm_normalize()` — post-computation normalisation helper (new function).**
  Apply z-score, rank-based inverse-normal transform (Rankit), min-max, or
  robust median/MAD scaling to any marker output data frame.  Use `cols` to
  target specific columns and `skip_cols` to protect covariates (e.g. age, BMI)
  from being rescaled.  This covers domain functions whose internal `normalize`
  argument currently has no effect (`glycemic_markers()`, `lipid_markers()`,
  `renal_markers()`, etc.).  See `?hm_normalize`.

* **Multi-biobank column inference (major).** The internal synonym dictionary
  (`.hm_default_col_patterns_exact()`) now recognises column-naming conventions
  from 15+ major cohort studies and biobanks for all primary analytes:
  - **UK Biobank** — `_0_0` / `_1_0` field naming (e.g. `glucose_0_0`,
    `creatinine_0_0`, `vitamin_d_0_0`); `standing_height`, `25OHD`.
  - **NHANES** — LBXS, LBDS, BPX, URX prefix variables (e.g. `LBXGLU`,
    `LBXSCH`, `BPXSY1`, `URXUMA`).
  - **Danish national registers / EHR (LABKA/OPEN)** — NPU codes (`NPU01994`
    for creatinine, `NPU01567` for cholesterol, `NPU03609` for haemoglobin,
    etc.) and Danish clinical labels (`kreatinin`, `kolesterol`, `blodsukker`,
    `leukocytter`, `trombocytter`, `d_vitamin`).
  - **HUNT Study and Tromsø Study (Norway)** — Norwegian-language terms
    (`blodsukkerfasting`, `systolisk_blodtrykk`, `triglyserider`, `karbamid`,
    `kjonn`, `midjeomkrets`).
  - **SCAPIS / TwinGene (Sweden)** — Swedish-language terms (`glukos`,
    `urinsyra`, `leukocyter`, `trombocyter`, `kön`, `längd`, `vikt`).
  - **FinnGen / THL Biobank (Finland)** — Finnish-language terms
    (`glukoosi`, `kolesteroli`, `kreatiniini`, `hemoglobiini`, `sukupuoli`,
    `virtsahappo`, `leukosyytit`, `ferritiini`, `D_vitamiini`).
  - **Estonian Biobank (EstBB)** — Estonian terms (`glukoos`, `kolesterool`,
    `kreatiniin`, `hemoglobiin`, `naatrium`, `kaalium`, `vanus`, `sugu`).
  - **LifeLines Cohort / Rotterdam Study (Netherlands)** — Dutch terms
    (`nuchtere_glucose`, `totaal_cholesterol`, `urinezuur`, `ureum`,
    `leukocyten`, `hemoglobine`, `vitamine_D`, `tailleomtrek`).
  - **Generation Scotland (GS:SFHS)** — `SBP_mean`, `DBP_mean`,
    `genetic_sex`, `ethnic_group`.
  - **All of Us / OMOP CDM** — LOINC codes in `LOINC_XXXX_X` format for
    all major analytes (e.g. `LOINC_2345_7` for fasting glucose,
    `LOINC_2160_0` for creatinine, `LOINC_718_7` for haemoglobin).
  - **NAKO / KORA (Germany)** — German-language terms (`Cholesterin`,
    `Triglyzeride`, `Harnsäure`, `Harnstoff`, `Leukozyten`, `Thrombozyten`).

* Added `hm_col_report()` — an interactive column-mapping diagnostic. Call
  `hm_col_report(your_data)` **before** running any computation to see a
  formatted table of which internal keys were matched to columns in your data
  and how (exact synonym, case-insensitive, substring, or fuzzy), plus a
  ready-to-paste `col_map` template for any unmatched keys.  
  The function uses a five-layer matching pipeline and returns the matched
  mappings invisibly so the result can be passed directly as `col_map` to any
  HealthMarkers function.

* **Auto-derivation of computed inputs.** `.hm_global_precompute()` now
  automatically derives 18+ secondary columns before marker computation begins,
  so functions that require (e.g.) `eGFR`, `UACR`, `WHR`, `LDL_c`, or `waist`
  no longer fail silently when only the raw inputs are present. Affected keys
  include `eGFR` (from creatinine/age/sex via CKD-EPI), `WHR` (from `waist`
  and `hip`), `UACR` (from `u_albumin` / `u_creatinine`), `LDL_c` (Friedewald
  from TC/HDL_c/TG), `MAP`, `PP`, `BMI` (from height/weight), and more.

* Expanded the internal synonym dictionary (`R/utils_infer-cols.R`) for all
  variable groups, additionally incorporating:
  - Inter99 / ADDITION real phenotype dataset labels
  - Common literature spellings (`TryG`, `TAG`, `TRIG`, `triacylglycerol`;
    `SGPT`/`GPT`/`ALAT`; `hsCRP`/`hs_CRP`)
  - Longitudinal follow-up suffixes (`_0`, `_1`, `_3`, `_5`)
  - NMR metabolomics (urine) panel, ECG markers, lifestyle covariates, and
    cytokine multiplex proteins.

## Bug fixes

* `adiposity_sds()`: fixed `fn_name` lookup bug that caused incorrect
  error messages when validation failed on non-standard column names.

## Internal

* Renamed the internal verbose-message helper from `hm_col_report()` to
  `hm_fmt_col_map()` to avoid a name collision with the new exported function.

# HealthMarkers 0.1.1

## New features

* Added `liver_fat_markers()` for hepatic steatosis and fibrosis index calculation.
* Added `nfl_marker()` for plasma neurofilament light chain (NfL) z-score computation.
* Added `impute_missing()` for within-row imputation of missing biomarker values.

## Bug fixes and improvements

* `pulmo_markers()`: fixed column-inference logic for spirometry z-score inputs.
* All marker functions: standardised verbose progress messages to emit at the
  `"debug"` level when `verbose = FALSE`, so that `getOption("healthmarkers.verbose")`
  controls visibility consistently across every function.
* Computing-phase messages are now unconditionally emitted at `"debug"` level
  (independent of the per-call `verbose` argument) in `bone_markers()`,
  `ckd_stage()`, and `corrected_calcium()`.
* `all_health_markers()`: column inference is now keyed to the requested
  groups, falls back to regex-based `infer_cols()` when exact matches fail, and
  reports per-group status (including missing optional packages) in verbose
  summaries.
* Cardiovascular risk wrappers: `cvd_risk_qrisk3()` now prefers the correctly
  named `ethnicity` column (typo tolerated for backward compatibility), and
  optional dependency errors now report the missing package name consistently.
* Shared validation: lipid marker helpers now route through `hm_validate_inputs()`
  so duplicate/empty mappings are caught uniformly.

## Internal

* Package gains a `_pkgdown.yml` that lists all 46 vignettes in the articles
  index for the pkgdown site.
