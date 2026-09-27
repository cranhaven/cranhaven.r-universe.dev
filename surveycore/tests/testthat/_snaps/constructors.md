# as_survey() errors when data is not a data frame [row 1]

    Code
      as_survey(list(x = 1:5), weights = x)
    Condition
      Error in `as_survey()`:
      x `data` must be a data frame, not <list>

# as_survey() errors when data has 0 rows [row 2]

    Code
      as_survey(empty_df, weights = w)
    Condition
      Error in `as_survey()`:
      x `data` must have at least one row

# as_survey() errors when data has duplicate column names [row 3]

    Code
      df2 <- data.frame(x = 1:3, y = 4:6)
      names(df2) <- c("x", "x")
      as_survey(df2, weights = x)
    Condition
      Error in `as_survey()`:
      x Column names in `data` must be unique. Duplicates: x

# as_survey() errors when data has 1 row [row 4]

    Code
      as_survey(single_row, weights = w)
    Condition
      Error in `as_survey()`:
      x `data` has only 1 row. A survey design requires at least 2 observations.

# as_survey() errors when probs and weights are inconsistent [row 5]

    Code
      as_survey(df, probs = prob, weights = wt, strata = strata)
    Condition
      Error in `as_survey()`:
      x Cannot specify both `probs` and `weights` with inconsistent values. `weights` should equal 1 / `probs`

# as_survey() errors when weights helper matches no columns [row 8]

    Code
      as_survey(df, weights = starts_with("zzz"))
    Condition
      Error in `as_survey()`:
      x `weights` matched no columns in `data`

# as_survey() errors when weights expression selects multiple columns [row 9]

    Code
      as_survey(df, weights = starts_with("wt"))
    Condition
      Error in `as_survey()`:
      x `weights` must select exactly one column, not 2

# as_survey() errors when strata expression selects multiple columns [row 11]

    Code
      as_survey(df, weights = wt, strata = starts_with("st"))
    Condition
      Error in `as_survey()`:
      x `strata` must select exactly one column, not 2

# as_survey() errors when fpc expression selects multiple columns [row 13]

    Code
      as_survey(df, weights = wt, fpc = starts_with("fpc"))
    Condition
      Error in `as_survey()`:
      x `fpc` selected 2 column(s) but `ids` has only 1 stage(s).
      i FPC columns must correspond 1-to-1 with ID stages. Supply at most 1 FPC column(s).

# as_survey() errors when fpc column has NA values [row 14]

    Code
      as_survey(df, weights = wt, fpc = fpc)
    Condition
      Error in `.validate_fpc()`:
      x `fpc` column fpc contains 1 NA value(s). FPC must be fully observed.
      v Remove rows with missing FPC or set `fpc = NULL` to omit the correction.

# as_survey() errors when nest = TRUE and strata is NULL [row 15]

    Code
      as_survey(df, ids = psu, weights = wt, nest = TRUE)
    Condition
      Error in `as_survey()`:
      x `nest = TRUE` requires `strata` to be specified

# as_survey_replicate() errors when data is not a data frame [row 1]

    Code
      as_survey_replicate(list(x = 1:5), weights = x, repweights = starts_with("r"),
      type = "JK1")
    Condition
      Error in `as_survey_replicate()`:
      x `data` must be a data frame, not <list>

# as_survey_replicate() errors when data has 0 rows [row 2]

    Code
      as_survey_replicate(empty_df, weights = w, repweights = r1, type = "JK1")
    Condition
      Error in `as_survey_replicate()`:
      x `data` must have at least one row

# as_survey_replicate() errors when repweights matches no columns [row 16]

    Code
      as_survey_replicate(df, weights = wt, repweights = starts_with("zzz"), type = "JK1")
    Condition
      Error in `as_survey_replicate()`:
      x `repweights` must select at least one column

# as_survey_replicate() errors when rscales length doesn't match n_rep [row 17]

    Code
      as_survey_replicate(df, weights = wt, repweights = starts_with("repwt_"), type = "BRR",
      rscales = c(1, 2))
    Condition
      Error in `.validate_rscales()`:
      x Length of `rscales` (2) must equal number of replicate weights (10).

# as_survey_twophase() snapshot: method = 'simple' + clustered Phase 1 warning

    Code
      as_survey_twophase(phase1, subset = subset, method = "simple")
    Condition
      Warning:
      ! `method = "simple"` ignores the Phase 1 cluster design (PSUs: psu). This understates variance. Use `method = "full"` or `method = "approx"`.
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_twophase> (method: simple)
      Phase 1 sample size: 200
      Phase 2 sample size: 75
      
    Output
      # A tibble: 200 x 11
         psu   strata      fpc    wt    y1      y2    y3 group subset phase1_prob
         <chr> <chr>     <dbl> <dbl> <dbl>   <dbl> <int> <chr> <lgl>        <dbl>
       1 psu_1 stratum_1   416 12.2   48.4  0.0895     1 C     TRUE        0.0865
       2 psu_1 stratum_1   416 10.8   42.3  0.231      0 A     FALSE       0.0865
       3 psu_2 stratum_1   416 11.1   49.0 -0.0118     0 A     FALSE       0.0865
       4 psu_2 stratum_1   416 14.7   50.4  0.885      0 A     TRUE        0.0865
       5 psu_2 stratum_1   416 16.9   43.6  0.469      1 A     FALSE       0.0865
       6 psu_2 stratum_1   416 11.7   44.3 -0.978      0 C     TRUE        0.0865
       7 psu_2 stratum_1   416 16.4   40.0  0.631      0 A     FALSE       0.0865
       8 psu_3 stratum_1   416 11.0   63.5 -0.509      0 B     FALSE       0.0865
       9 psu_3 stratum_1   416 12.9   51.3  0.232      0 C     FALSE       0.0865
      10 psu_4 stratum_1   416  9.48  45.8 -1.64       0 C     FALSE       0.0865
      # i 190 more rows
      # i 1 more variable: phase2_prob <dbl>

# as_survey_twophase() errors when phase1 is a data.frame [row 19]

    Code
      as_survey_twophase(df, subset = subset)
    Condition
      Error in `as_survey_twophase()`:
      x `phase1` must be a survey design object (<survey_base>), not <data.frame>.
      i Create it first with `as_survey()` or `as_survey_replicate()`.

# as_survey_twophase() errors when subset is not provided [row 20]

    Code
      as_survey_twophase(phase1)
    Condition
      Error in `as_survey_twophase()`:
      x `subset` is required: a logical column indicating Phase 2 membership

# as_survey_twophase() errors when subset selects multiple columns [row 21]

    Code
      as_survey_twophase(phase1, subset = starts_with("subset"))
    Condition
      Error in `as_survey_twophase()`:
      x `subset` must select exactly one column, not 2

# as_survey_twophase() errors when subset column is not logical [row 22]

    Code
      as_survey_twophase(phase1, subset = phase2_int)
    Condition
      Error in `as_survey_twophase()`:
      x `subset` column phase2_int must be logical, not <integer>

# as_survey_twophase() errors when subset is all TRUE [row 23]

    Code
      as_survey_twophase(phase1, subset = all_true)
    Condition
      Error in `as_survey_twophase()`:
      x `subset` column all_true must contain both TRUE and FALSE values (non-NA). Found 200 TRUE and 0 FALSE (non-NA) value(s).

# as_survey_twophase() errors for NA in subset column [row 23b]

    Code
      as_survey_twophase(phase1, subset = phase2_na, method = "approx")
    Condition
      Error in `as_survey_twophase()`:
      x `subset` column phase2_na contains 1 NA value(s).
      i The phase 2 membership indicator must be fully observed for all phase 1 units.
      v Remove rows with missing `subset` values before calling `as_survey_twophase()`.

# as_survey_nonprob() rejects non-data-frame input

    Code
      as_survey_nonprob(list(y = 1:10, w = rep(1, 10)), weights = w)
    Condition
      Error in `as_survey_nonprob()`:
      x `data` must be a data frame, not <list>

# as_survey_nonprob() rejects empty data

    Code
      as_survey_nonprob(empty, weights = w)
    Condition
      Error in `as_survey_nonprob()`:
      x `data` must have at least one row

# as_survey_nonprob() rejects duplicate column names

    Code
      as_survey_nonprob(df, weights = w)
    Condition
      Error in `as_survey_nonprob()`:
      x Column names in `data` must be unique. Duplicates: w

# as_survey_nonprob() rejects missing weights argument

    Code
      as_survey_nonprob(df)
    Condition
      Error in `as_survey_nonprob()`:
      x `weights` is required for `as_survey_nonprob()`.
      i Supply the column name of your calibration weight variable (e.g., `weights = cal_wt`).

# as_survey_nonprob() rejects weights selecting multiple columns

    Code
      as_survey_nonprob(df, weights = c(w1, w2))
    Condition
      Error in `as_survey_nonprob()`:
      x `weights` must select exactly one column, not 2

# as_survey_nonprob() rejects non-positive weights

    Code
      as_survey_nonprob(df, weights = w)
    Condition
      Error in `.validate_weights()`:
      x Weight column w has 1 non-positive value(s).
      i All non-NA weights must be strictly greater than 0.
      v Remove or replace rows where w is 0 or negative.

# print() produces correct output for survey_nonprob (default)

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_nonprob> (non-probability) [experimental]
      * Variance: SRS approximation (no bootstrap replicate weights)
      Sample size: 10
      
    Output
      # A tibble: 10 x 2
             y     w
         <int> <dbl>
       1     1     1
       2     2     1
       3     3     1
       4     4     1
       5     5     1
       6     6     1
       7     7     1
       8     8     1
       9     9     1
      10    10     1

# print(d, full = TRUE) produces correct output for survey_nonprob

    Code
      print(d, full = TRUE)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_nonprob> (non-probability) [experimental]
      * Variance: SRS approximation (no bootstrap replicate weights)
      Sample size: 10
      Weighted N: 10
      
      
      -- Design specification --
      
      * Weights: w
      * Calibration provenance: none stored
      
      Design variables: w
      
      
      -- Weight distribution --
      
      * Range: 1 – 1
      * Mean: 1
      * CV: 0
      
      
      -- Metadata --
      
      0 variable(s) labeled
      
    Output
      # A tibble: 10 x 2
             y     w
         <int> <dbl>
       1     1     1
       2     2     1
       3     3     1
       4     4     1
       5     5     1
       6     6     1
       7     7     1
       8     8     1
       9     9     1
      10    10     1

# print(d, design_info = TRUE) produces correct output for survey_nonprob

    Code
      print(d, design_info = TRUE)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_nonprob> (non-probability) [experimental]
      * Variance: SRS approximation (no bootstrap replicate weights)
      Sample size: 10
      
      
      -- Design specification --
      
      * Weights: w
      * Calibration provenance: none stored
      
      Design variables: w
      
    Output
      # A tibble: 10 x 2
             y     w
         <int> <dbl>
       1     1     1
       2     2     1
       3     3     1
       4     4     1
       5     5     1
       6     6     1
       7     7     1
       8     8     1
       9     9     1
      10    10     1

# print(d, weights_info = TRUE) produces correct output for survey_nonprob

    Code
      print(d, weights_info = TRUE)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_nonprob> (non-probability) [experimental]
      * Variance: SRS approximation (no bootstrap replicate weights)
      Sample size: 10
      Weighted N: 10
      
      
      -- Weight distribution --
      
      * Range: 1 – 1
      * Mean: 1
      * CV: 0
      
    Output
      # A tibble: 10 x 2
             y     w
         <int> <dbl>
       1     1     1
       2     2     1
       3     3     1
       4     4     1
       5     5     1
       6     6     1
       7     7     1
       8     8     1
       9     9     1
      10    10     1

# print(d, metadata_info = TRUE) produces correct output for survey_nonprob

    Code
      print(d, metadata_info = TRUE)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_nonprob> (non-probability) [experimental]
      * Variance: SRS approximation (no bootstrap replicate weights)
      Sample size: 10
      
      
      -- Metadata --
      
      1 variable(s) labeled
      
    Output
      # A tibble: 10 x 2
             y     w
         <int> <dbl>
       1     1     1
       2     2     1
       3     3     1
       4     4     1
       5     5     1
       6     6     1
       7     7     1
       8     8     1
       9     9     1
      10    10     1

# summary() produces correct output for survey_nonprob

    Code
      summary(d)
    Message
      
      -- Survey Design Summary -------------------------------------------------------
      Type: non-probability [experimental]
      Sample size: 10
      Weighted N: 10
      
      
      -- Design --
      
      Weights: w
      * Range: 1 – 1
      * Mean: 1
      * CV: 0
      Calibration provenance: none stored
      
      Metadata: 0 of 2 variable(s) labeled

# as_survey_nonprob() rejects non-numeric weight column

    Code
      as_survey_nonprob(df, weights = w)
    Condition
      Error in `.validate_weights()`:
      x Weight column w must be numeric.
      i Got <character>.
      v Convert with `as.numeric(w)`.

# as_survey_nonprob() rejects repweights selecting 0 columns

    Code
      as_survey_nonprob(df, weights = w, repweights = starts_with("rw"))
    Condition
      Error in `as_survey_nonprob()`:
      x `repweights` must select at least one column

# as_survey_nonprob() rejects repweights selecting exactly 1 column

    Code
      as_survey_nonprob(df, weights = w, repweights = rw1)
    Condition
      Error in `as_survey_nonprob()`:
      x `repweights` must name at least 2 replicate weight columns.
      i Replicate variance requires >= 2 replicates. Got "1".

# as_survey_nonprob() rejects type != 'bootstrap' (legacy — superseded by NB-1 blocks)

    Code
      as_survey_nonprob(df, weights = w, repweights = c(rw1, rw2), type = "BRR")
    Condition
      Error in `as_survey_nonprob()`:
      x `type` must be one of "bootstrap", "JK1", "JK2", "JKn", or "jackknife" for <survey_nonprob> objects.
      i Got "BRR".

# as_survey_nonprob() rejects rscales length mismatch

    Code
      as_survey_nonprob(df, weights = w, repweights = c(rw1, rw2), rscales = c(1, 1,
        1))
    Condition
      Error in `.validate_rscales()`:
      x Length of `rscales` (3) must equal number of replicate weights (2).

# as_survey_nonprob() rejects rscales with NA values

    Code
      as_survey_nonprob(df, weights = w, repweights = c(rw1, rw2), rscales = c(1,
        NA_real_))
    Condition
      Error in `.validate_rscales()`:
      x `rscales` must be a non-negative numeric vector with no NA values.
      i Got 1 NA value(s) and/or 0 negative value(s).
      v Supply a numeric vector of length 2 with all values >= 0.

# as_survey_nonprob() rejects rscales with negative values

    Code
      as_survey_nonprob(df, weights = w, repweights = c(rw1, rw2), rscales = c(1,
        -0.5))
    Condition
      Error in `.validate_rscales()`:
      x `rscales` must be a non-negative numeric vector with no NA values.
      i Got 0 NA value(s) and/or 1 negative value(s).
      v Supply a numeric vector of length 2 with all values >= 0.

# as_survey_nonprob() rejects reference_sample that is not survey_taylor

    Code
      as_survey_nonprob(df, weights = w, reference_sample = data.frame(x = 1:5))
    Condition
      Error in `as_survey_nonprob()`:
      x `reference_sample` must be a <survey_taylor> object.
      i Got <data.frame>.
      v Pass the <survey_taylor> object used to estimate propensity scores.

# as_survey_nonprob() rejects calibration$bootstrap = FALSE with repweights

    Code
      as_survey_nonprob(df, weights = w, repweights = c(rw1, rw2), calibration = cal)
    Condition
      Error in `as_survey_nonprob()`:
      x `calibration` indicates the replicate weights were not produced by re-running the adjustment procedure.
      i `calibration$bootstrap` must be "TRUE" for bootstrap variance to be valid.
      v Use `surveywts::create_bootstrap_weights()` to produce repweights with re-calibration.

# as_survey_nonprob() rejects calibration$R mismatch with repweights count

    Code
      as_survey_nonprob(df, weights = w, repweights = c(rw1, rw2), calibration = cal)
    Condition
      Error in `as_survey_nonprob()`:
      x Provenance records 5 replicates but 2 replicate weight columns were found.
      i The `calibration` object and `repweights` columns must come from the same `surveywts::create_bootstrap_weights()` call.

# surveycore_error_provenance_not_bootstrap still fires for bootstrap + calibration$bootstrap = FALSE

    Code
      as_survey_nonprob(df, weights = wt, repweights = c(r1, r2, r3, r4), type = "bootstrap",
      calibration = list(bootstrap = FALSE))
    Condition
      Error in `as_survey_nonprob()`:
      x `calibration` indicates the replicate weights were not produced by re-running the adjustment procedure.
      i `calibration$bootstrap` must be "TRUE" for bootstrap variance to be valid.
      v Use `surveywts::create_bootstrap_weights()` to produce repweights with re-calibration.

# surveycore_error_provenance_R_mismatch fires for JK1 with mismatched calibration$R

    Code
      as_survey_nonprob(df, weights = wt, repweights = c(r1, r2, r3), type = "JK1",
      calibration = list(R = 5L))
    Condition
      Error in `as_survey_nonprob()`:
      x Provenance records 5 replicates but 3 replicate weight columns were found.
      i The `calibration` object and `repweights` columns must come from the same `surveywts::create_bootstrap_weights()` call.

# calibration = list() with type = 'bootstrap' raises surveycore_error_provenance_not_bootstrap

    Code
      as_survey_nonprob(df, weights = wt, repweights = c(r1, r2, r3, r4), type = "bootstrap",
      calibration = list())
    Condition
      Error in `as_survey_nonprob()`:
      x `calibration` indicates the replicate weights were not produced by re-running the adjustment procedure.
      i `calibration$bootstrap` must be "TRUE" for bootstrap variance to be valid.
      v Use `surveywts::create_bootstrap_weights()` to produce repweights with re-calibration.

# surveycore_error_rscales_length fires for wrong-length rscales with JK1

    Code
      as_survey_nonprob(df, weights = wt, repweights = c(r1, r2, r3, r4), type = "JK1",
      rscales = c(1, 1))
    Condition
      Error in `.validate_rscales()`:
      x Length of `rscales` (2) must equal number of replicate weights (4).

# surveycore_error_rscales_na fires for NA in rscales with JK1

    Code
      as_survey_nonprob(df, weights = wt, repweights = c(r1, r2, r3, r4), type = "JK1",
      rscales = c(1, NA_real_, 1, 1))
    Condition
      Error in `.validate_rscales()`:
      x `rscales` must be a non-negative numeric vector with no NA values.
      i Got 1 NA value(s) and/or 0 negative value(s).
      v Supply a numeric vector of length 4 with all values >= 0.

# surveycore_error_reference_sample_nonprob fires for non-taylor reference_sample with JK1

    Code
      as_survey_nonprob(df, weights = wt, repweights = c(r1, r2, r3, r4), type = "JK1",
      reference_sample = data.frame(x = 1:5))
    Condition
      Error in `as_survey_nonprob()`:
      x `reference_sample` must be a <survey_taylor> object.
      i Got <data.frame>.
      v Pass the <survey_taylor> object used to estimate propensity scores.

# as_survey() errors when fpc has more columns than ID stages

    Code
      as_survey(df, ids = c(psu, ssu), weights = wt, fpc = c(fpc, fpc2, fpc3))
    Condition
      Error in `as_survey()`:
      x `fpc` selected 3 column(s) but `ids` has only 2 stage(s).
      i FPC columns must correspond 1-to-1 with ID stages. Supply at most 2 FPC column(s).

# as_survey() rejects NA in stage-2 FPC column [dual-pattern]

    Code
      as_survey(df, ids = c(psu, ssu), weights = wt, fpc = c(fpc, fpc2_bad))
    Condition
      Error in `.validate_fpc()`:
      x `fpc` column fpc2_bad contains 1 NA value(s). FPC must be fully observed.
      v Remove rows with missing FPC or set `fpc = NULL` to omit the correction.

# as_survey() rejects nonpositive stage-2 FPC value [dual-pattern]

    Code
      as_survey(df, ids = c(psu, ssu), weights = wt, fpc = c(fpc, fpc2_bad))
    Condition
      Error in `as_survey()`:
      x `fpc` column fpc2_bad has 1 non-positive value(s). FPC values must be > 0.
      i FPC must be either population sizes (> 1) or sampling fractions (0 < f ≤ 1).

# as_survey() rejects stage-2 FPC smaller than stage-2 cluster count [dual-pattern]

    Code
      as_survey(df, ids = c(psu, ssu), weights = wt, fpc = c(fpc, fpc2_bad))
    Condition
      Error in `as_survey()`:
      x Stage-2 FPC column fpc2_bad has population sizes smaller than the observed cluster count in 20 parent group(s).
      i Population size must be >= the number of sampled units within each parent cluster.

# as_survey() rejects non-constant stage-2 FPC fraction within PSU [dual-pattern]

    Code
      as_survey(df, ids = c(psu, ssu), weights = wt, fpc = c(fpc, fpc2_bad))
    Condition
      Error in `as_survey()`:
      x Stage-2 FPC column fpc2_bad is not constant within parent clusters.
      i FPC fractions must be the same for all units within each parent cluster.

# as_survey_replicate() still rejects multi-column fpc

    Code
      as_survey_replicate(df, weights = wt, repweights = tidyselect::all_of(
        repwt_cols), type = "BRR", fpc = c(fpc, fpc2))
    Condition
      Error in `as_survey_replicate()`:
      x `fpc` must select exactly one column, not 2

# as_survey_nonprob() rejects type = 'BRR' with surveycore_error_type_unsupported_for_nonprob

    Code
      as_survey_nonprob(df, weights = wt, repweights = c(r1, r2, r3), type = "BRR")
    Condition
      Error in `as_survey_nonprob()`:
      x `type` must be one of "bootstrap", "JK1", "JK2", "JKn", or "jackknife" for <survey_nonprob> objects.
      i Got "BRR".

# as_survey_nonprob() rejects type = 'Fay' with surveycore_error_type_unsupported_for_nonprob

    Code
      as_survey_nonprob(df, weights = wt, repweights = c(r1, r2, r3), type = "Fay")
    Condition
      Error in `as_survey_nonprob()`:
      x `type` must be one of "bootstrap", "JK1", "JK2", "JKn", or "jackknife" for <survey_nonprob> objects.
      i Got "Fay".

# as_survey_nonprob() rejects type = 'bootstrap2' with surveycore_error_type_unsupported_for_nonprob

    Code
      as_survey_nonprob(df, weights = wt, repweights = c(r1, r2, r3), type = "bootstrap2")
    Condition
      Error in `as_survey_nonprob()`:
      x `type` must be one of "bootstrap", "JK1", "JK2", "JKn", or "jackknife" for <survey_nonprob> objects.
      i Got "bootstrap2".

# as_survey_nonprob() rejects type = c('JK1', 'JK2') (vector) with surveycore_error_type_unsupported_for_nonprob

    Code
      as_survey_nonprob(df, weights = wt, repweights = c(r1, r2, r3), type = c("JK1",
        "JK2"))
    Condition
      Error in `as_survey_nonprob()`:
      x `type` must be one of "bootstrap", "JK1", "JK2", "JKn", or "jackknife" for <survey_nonprob> objects.
      i Got "JK1" and "JK2".

# as_survey_nonprob() rejects type = NA_character_ with surveycore_error_type_unsupported_for_nonprob

    Code
      as_survey_nonprob(df, weights = wt, repweights = c(r1, r2, r3), type = NA_character_)
    Condition
      Error in `as_survey_nonprob()`:
      x `type` must be one of "bootstrap", "JK1", "JK2", "JKn", or "jackknife" for <survey_nonprob> objects.
      i Got NA.

# as_survey_nonprob() rejects type = 'jk1' (lowercase) with surveycore_error_type_unsupported_for_nonprob

    Code
      as_survey_nonprob(df, weights = wt, repweights = c(r1, r2, r3), type = "jk1")
    Condition
      Error in `as_survey_nonprob()`:
      x `type` must be one of "bootstrap", "JK1", "JK2", "JKn", or "jackknife" for <survey_nonprob> objects.
      i Got "jk1".

# surveycore_error_repweights_single message says 'Replicate variance' for JK1

    Code
      as_survey_nonprob(df, weights = wt, repweights = r1, type = "JK1")
    Condition
      Error in `as_survey_nonprob()`:
      x `repweights` must name at least 2 replicate weight columns.
      i Replicate variance requires >= 2 replicates. Got "1".

# surveycore_error_repweights_single message says 'Replicate variance' for bootstrap

    Code
      as_survey_nonprob(df, weights = wt, repweights = r1, type = "bootstrap")
    Condition
      Error in `as_survey_nonprob()`:
      x `repweights` must name at least 2 replicate weight columns.
      i Replicate variance requires >= 2 replicates. Got "1".

# surveycore_error_stratified_jk_rscales_unset fires for JK2 with rscales = NULL

    Code
      as_survey_nonprob(df, weights = wt, repweights = c(r1, r2), type = "JK2")
    Condition
      Error in `as_survey_nonprob()`:
      x `type` = "JK2" requires explicit `rscales`.
      i Stratified jackknife rscales are stratum-specific: `(n_h - 1) / n_h`. Supplying `NULL` would silently use `rep(1, R)`, which is statistically incorrect for JK2/JKn.
      v Compute `rscales` as `(n_h - 1) / n_h` where `n_h` is the number of units in stratum `h`, indexed to replicate order.

# surveycore_error_stratified_jk_rscales_unset fires for JKn with rscales = NULL

    Code
      as_survey_nonprob(df, weights = wt, repweights = c(r1, r2), type = "JKn")
    Condition
      Error in `as_survey_nonprob()`:
      x `type` = "JKn" requires explicit `rscales`.
      i Stratified jackknife rscales are stratum-specific: `(n_h - 1) / n_h`. Supplying `NULL` would silently use `rep(1, R)`, which is statistically incorrect for JK2/JKn.
      v Compute `rscales` as `(n_h - 1) / n_h` where `n_h` is the number of units in stratum `h`, indexed to replicate order.

# surveycore_error_scale_negative fires for negative scale with JK1

    Code
      as_survey_nonprob(df, weights = wt, repweights = c(r1, r2, r3, r4), type = "JK1",
      scale = -0.5)
    Condition
      Error in `as_survey_nonprob()`:
      x `scale` must be >= 0. Got -0.5.
      i A negative scale factor produces negative variance, which is nonsensical.
      v Use `scale = 0` to exclude a replicate's contribution, or omit `scale` to use the type-specific default.

