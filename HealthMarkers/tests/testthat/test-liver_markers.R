library(testthat)
library(tibble)
library(HealthMarkers)

test_that("computes markers for a minimal valid example", {
  skip_on_cran()
  df <- tibble(
    BMI = 24, waist = 80, TG = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  out <- liver_markers(df, col_map = cm)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("FLI","NFS","APRI","FIB4","BARD","ALBI","MELD_XI"))

  L <- 0.953 * log(150) + 0.139 * 24 + 0.718 * log(30) + 0.053 * 80 - 15.745
  expect_equal(out$FLI, exp(L) / (1 + exp(L)) * 100, tolerance = 1e-8)
  expect_equal(
    out$NFS,
    -1.675 + 0.037 * 30 + 0.094 * 24 + 1.13 * 0 + 0.99 * (25/20) - 0.013 * 250 - 0.066 * 45,
    tolerance = 1e-8
  )
  expect_equal(out$APRI, (25/40)/250*100, tolerance = 1e-8)
  expect_equal(out$FIB4, (30*25)/(250*sqrt(20)), tolerance = 1e-8)
  expect_equal(out$BARD, as.integer((24 >= 28) + 2L*(25/20 >= 0.8) + (0 == 1)))
  expect_equal(out$ALBI, log10(1.0*17.1)*0.66 + 45*(-0.0852), tolerance = 1e-8)
  expect_equal(out$MELD_XI, 5.11*log(1.0) + 11.76*log(0.9) + 9.44, tolerance = 1e-8)
})

test_that("verbose emits preparing, column map, and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(
    BMI = 24, waist = 80, TG = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_message(liver_markers(df, col_map = cm, verbose = TRUE), "liver_markers")
  expect_message(liver_markers(df, col_map = cm, verbose = TRUE), "col_map")
  expect_message(liver_markers(df, col_map = cm, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(
    BMI = 24, waist = 80, TG = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  msgs <- testthat::capture_messages(
    liver_markers(df, col_map = cm, verbose = TRUE)
  )
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("na_action='omit' emits omit message and preserves expected row count", {
  skip_on_cran()
  df <- tibble(
    BMI = c(24, 24), waist = c(80, 80), TG = c(150, 150), GGT = c(30, 30),
    age = c(30, 30), AST = c(25, 25), ALT = c(20, 20),
    platelets = c(250, NA_real_), albumin = c(45, 45),
    diabetes = c(FALSE, FALSE), bilirubin = c(1.0, 1.0), creatinine = c(0.9, 0.9)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(
    out <- liver_markers(df, col_map = cm, na_action = "omit", verbose = TRUE),
    "omitting 1 rows with NA in required inputs"
  )
  expect_equal(nrow(out), 1L)
})

test_that("custom column mapping works with renamed inputs", {
  skip_on_cran()
  df <- tibble(
    BMIx = 24, waistx = 80, TGx = 150, GGTx = 30, agex = 30,
    ASTx = 25, ALTx = 20, ptx = 250, albuminx = 45, diabx = FALSE,
    bilix = 1.0, creatx = 0.8
  )
  cm <- list(
    BMI = "BMIx", waist = "waistx", TG = "TGx", GGT = "GGTx",
    age = "agex", AST = "ASTx", ALT = "ALTx", platelets = "ptx",
    albumin = "albuminx", diabetes = "diabx", bilirubin = "bilix", creatinine = "creatx"
  )
  out <- liver_markers(df, col_map = cm)

  expect_named(out, c("FLI","NFS","APRI","FIB4","BARD","ALBI","MELD_XI"))
  L <- 0.953 * log(150) + 0.139 * 24 + 0.718 * log(30) + 0.053 * 80 - 15.745
  expect_equal(out$FLI, exp(L) / (1 + exp(L)) * 100, tolerance = 1e-8)
})

test_that("verbose emits range note for out-of-range values", {
  skip_on_cran()
  df <- tibble(
    BMI = 24, waist = 80,
    TG = 5000,  # above plausible range -> range note in verbose
    GGT = 30, age = 30, AST = 25, ALT = 20,
    platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(
    liver_markers(df, col_map = cm, verbose = TRUE),
    "range note"
  )
})

test_that("extreme values are not altered; non-positive inputs propagate NaN to outputs", {
  skip_on_cran()
  df <- tibble(
    BMI = 24, waist = 80,
    TG = -5,   # negative -> log(-5) = NaN -> FLI = NaN
    GGT = 30, age = 30, AST = 25, ALT = 20,
    platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  expect_warning(
    withCallingHandlers(
      out <- liver_markers(df, col_map = cm, verbose = FALSE),
      warning = function(w) {
        if (conditionMessage(w) == "NaNs produced") invokeRestart("muffleWarning")
      }
    ),
    "log\\(\\) undefined"
  )
  expect_true(!is.finite(out$FLI))
})

test_that("numeric coercion warns when NAs introduced", {
  skip_on_cran()
  df <- tibble(
    BMI = c(24, 24), waist = c(80, 80), TG = c("150","oops"), GGT = c(30, 30),
    age = c(30, 30), AST = c(25, 25), ALT = c(20, 20),
    platelets = c(250, 250), albumin = c("45","46"),
    diabetes = c(FALSE, FALSE), bilirubin = c(1.0, 1.0), creatinine = c(0.9, 0.9)
  )
  cm <- list(
    BMI="BMI", waist="waist", TG="TG", GGT="GGT", age="age",
    AST="AST", ALT="ALT", platelets="platelets", albumin="albumin",
    diabetes="diabetes", bilirubin="bilirubin", creatinine="creatinine"
  )

  expect_warning(
    liver_markers(df, col_map = cm),
    "coerced to numeric; NAs introduced"
  )
})

test_that("diabetes logical values compute BARD correctly without warnings", {
  skip_on_cran()
  df <- tibble(
    BMI = 30, waist = 90, TG = 150, GGT = 30, age = 50,
    AST = 40, ALT = 30, platelets = 250, albumin = 45, diabetes = TRUE,
    bilirubin = 1.0, creatinine = 1.0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  expect_warning(
    out <- liver_markers(df, col_map = cm),
    NA
  )
  expect_equal(out$BARD, 4L) # BMI>=28 +1, AST/ALT>=0.8 +2 (Harrison 2008), diabetes TRUE +1
})

test_that("vectorized diabetes logical values contribute per-row", {
  skip_on_cran()
  df <- tibble(
    BMI = c(24, 24), waist = c(80, 80), TG = c(150, 150),
    GGT = c(30, 30), age = c(30, 30),
    AST = c(20, 20), ALT = c(40, 40), # AST/ALT=0.5 -> 0 points for ratio, BMI<28 -> 0
    platelets = c(250, 250), albumin = c(45, 45),
    diabetes = c(TRUE, FALSE),
    bilirubin = c(1.0, 1.0), creatinine = c(1.0, 1.0)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  out <- liver_markers(df, col_map = cm)
  expect_equal(out$BARD, c(1L, 0L))
})

test_that("liver_markers returns all seven markers and correct values", {
  skip_on_cran()
  df <- tibble(
    BMI           = 24,
    waist         = 80,
    TG = 150, # mg/dL
    GGT           = 30,
    age           = 30,
    AST           = 25,
    ALT           = 20,
    platelets     = 250,
    albumin       = 45,
    diabetes      = FALSE,
    bilirubin     = 1.0,
    creatinine    = 0.8
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- liver_markers(df, col_map = cm)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("FLI", "NFS", "APRI", "FIB4", "BARD", "ALBI", "MELD_XI"))

  # FLI
  L <- 0.953 * log(150) + 0.139 * 24 + 0.718 * log(30) + 0.053 * 80 - 15.745
  expect_equal(out$FLI, exp(L) / (1 + exp(L)) * 100, tolerance = 1e-8)

  # NFS
  expect_equal(
    out$NFS,
    -1.675 + 0.037 * 30 + 0.094 * 24 + 1.13 * 0 + 0.99 * (25 / 20) - 0.013 * 250 - 0.066 * 45,
    tolerance = 1e-8
  )

  # APRI
  expect_equal(out$APRI, (25 / 40) / 250 * 100, tolerance = 1e-8)

  # FIB-4
  expect_equal(out$FIB4, (30 * 25) / (250 * sqrt(20)), tolerance = 1e-8)

  # BARD
  expected_bard <- as.integer((24 >= 28) + 2L*(25 / 20 >= 0.8) + (0 == 1))
  expect_equal(out$BARD, expected_bard)

  # ALBI
  expect_equal(out$ALBI, log10(1.0 * 17.1) * 0.66 + 45 * (-0.0852), tolerance = 1e-8)

  # MELD_XI
  expect_equal(out$MELD_XI, 5.11 * log(1.0) + 11.76 * log(0.8) + 9.44, tolerance = 1e-8)
})

test_that("errors if mapped columns are missing in data", {
  skip_on_cran()
  df <- tibble(
    BMI = 24, waist = 80, TG = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE
    # bilirubin and creatinine missing
  )
  expect_error(
    liver_markers(df, col_map = list(
      BMI = "BMI", waist = "waist", TG = "TG", GGT = "GGT",
      age = "age", AST = "AST", ALT = "ALT", platelets = "platelets",
      albumin = "albumin", diabetes = "diabetes",
      bilirubin = "bilirubin", creatinine = "creatinine"
    )),
    regexp = "mapped columns not found in data|missing required columns"
  )
})

test_that("partial col_map is supplemented by dictionary inference for missing keys", {
  skip_on_cran()
  df <- tibble(
    BMI = 24, waist = 80, TG = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.8
  )
  cm <- list(
    BMI = "BMI", waist = "waist", TG = "TG", GGT = "GGT",
    age = "age", AST = "AST", ALT = "ALT", platelets = "platelets",
    albumin = "albumin", diabetes = "diabetes"
    # bilirubin/creatinine keys missing -- now auto-inferred from dictionary
  )
  # Should NOT error: missing keys are filled by dictionary inference
  out <- suppressWarnings(liver_markers(df, col_map = cm))
  expect_s3_class(out, "tbl_df")
  expect_true(!is.na(out$ALBI))
  expect_true(!is.na(out$MELD_XI))
})

test_that("na_action='error' aborts when required inputs contain NA", {
  skip_on_cran()
  df <- tibble(
    BMI = 24, waist = 80, TG = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = NA_real_, creatinine = 0.9
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_error(
    suppressWarnings(liver_markers(df, col_map = cm, na_action = "error")),
    "required inputs contain missing values"
  )
})

test_that("na_action='omit' drops rows with NA", {
  skip_on_cran()
  df <- tibble(
    BMI = c(24, 24), waist = c(80, 80), TG = c(150, 150), GGT = c(30, 30),
    age = c(30, 30), AST = c(25, 25), ALT = c(20, 20), platelets = c(250, NA_real_),
    albumin = c(45, 45), diabetes = c(FALSE, FALSE), bilirubin = c(1.0, 1.0), creatinine = c(0.9, 0.9)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- suppressWarnings(liver_markers(df, col_map = cm, na_action = "omit"))
  expect_equal(nrow(out), 1L)
})

test_that("na_action='keep' propagates NA to outputs", {
  skip_on_cran()
  df <- tibble(
    BMI = 24, waist = 80, TG = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = NA_real_, creatinine = 0.9
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- suppressWarnings(liver_markers(df, col_map = cm, na_action = "keep"))
  expect_true(is.na(out$ALBI))
  expect_true(is.na(out$MELD_XI))
})

test_that("extreme values compute without error when outside plausible range", {
  skip_on_cran()
  df <- tibble(
    BMI = 80,              # above range
    waist = 300,           # above range
    TG = 5000,  # above range
    GGT = 5000,            # above range
    age = 150,             # above range
    AST = 6000,            # above range
    ALT = 6000,            # above range
    platelets = 5,         # below range
    albumin = 5,           # below range
    diabetes = FALSE,
    bilirubin = 0.05,      # below range
    creatinine = 50        # above range
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- liver_markers(df, col_map = cm, verbose = FALSE)
  expect_s3_class(out, "tbl_df")
  expect_equal(ncol(out), 7L)
})

test_that("extreme values produce finite or NaN/Inf outputs without error", {
  skip_on_cran()
  df <- tibble(
    BMI = 80, waist = 300, TG = 5000, GGT = 5000, age = 150,
    AST = 6000, ALT = 6000, platelets = 5, albumin = 5, diabetes = FALSE,
    bilirubin = 0.05, creatinine = 50
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- liver_markers(df, col_map = cm, verbose = FALSE)
  expect_s3_class(out, "tbl_df")
  expect_equal(ncol(out), 7L)
})

test_that("verbose range note is informational and does not warn or cap", {
  skip_on_cran()
  # All values positive so no log/sqrt warnings; only range note emitted in verbose
  df <- tibble(
    BMI = 5, waist = 30, TG = 2000, GGT = 3000,
    age = 10, AST = 1, ALT = 1,
    platelets = 5, albumin = 5, diabetes = FALSE,
    bilirubin = 0.09, creatinine = 0.1
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  # No warning emitted; range note visible only in verbose messages
  expect_no_warning(liver_markers(df, col_map = cm, verbose = FALSE))

  # Verbose mode emits range note message
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(
    liver_markers(df, col_map = cm, verbose = TRUE),
    "range note"
  )
})

test_that("no warning emitted for extreme values in non-verbose mode", {
  skip_on_cran()
  df <- tibble(
    BMI = 80, waist = 300, TG = 5000, GGT = 5000, age = 150,
    AST = 6000, ALT = 6000, platelets = 5, albumin = 5, diabetes = FALSE,
    bilirubin = 0.05, creatinine = 50
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  expect_no_warning(liver_markers(df, col_map = cm, verbose = FALSE))
})

test_that("BMI pre-computed from weight and height when BMI absent", {
  skip_on_cran()
  df <- tibble(
    weight = 70, height = 170,  # BMI = 70 / (1.70)^2 = 24.22
    waist = 80, TG = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  df_bmi <- tibble(
    BMI = 70 / (170 / 100)^2,
    waist = 80, TG = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  out_precomp  <- liver_markers(df,     verbose = FALSE)
  out_explicit <- liver_markers(df_bmi, verbose = FALSE)
  expect_equal(out_precomp$FLI, out_explicit$FLI, tolerance = 1e-6)
  expect_equal(out_precomp$NFS, out_explicit$NFS, tolerance = 1e-6)
})

test_that("ID column is detected and prepended to output", {
  skip_on_cran()
  df <- tibble(
    id  = c("p1", "p2"),
    BMI = c(24, 28), waist = c(80, 90), TG = c(150, 200), GGT = c(30, 40),
    age = c(30, 45), AST = c(25, 35), ALT = c(20, 30), platelets = c(250, 200),
    albumin = c(45, 42), diabetes = c(FALSE, TRUE),
    bilirubin = c(1.0, 1.5), creatinine = c(0.9, 1.1)
  )
  out <- liver_markers(df, verbose = FALSE)
  expect_equal(names(out)[1], "id")
  expect_equal(out$id, c("p1", "p2"))
  expect_equal(ncol(out), 8L)  # id + 7 markers
})

test_that("denominator and transform warnings fire (isolated tests)", {
  skip_on_cran()
  # Zero platelets -> denominator warning for APRI/FIB4
  df1 <- tibble(
    BMI = 24, waist = 80, TG = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 0, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  cm1 <- as.list(names(df1)); names(cm1) <- names(df1)
  expect_warning(liver_markers(df1, col_map = cm1), "zero denominators detected")

  # Non-positive for log: TG
  df2 <- tibble(
    BMI = 24, waist = 80, TG = 0, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  cm2 <- as.list(names(df2)); names(cm2) <- names(df2)
  expect_warning(liver_markers(df2, col_map = cm2), "TG.*log\\(\\) undefined")

  # ALT negative -> target only the sqrt() warning; muffle other warnings
  df3 <- tibble(
    BMI = 24, waist = 80, TG = 150, GGT = 30, age = 30,
    AST = 25, ALT = -1, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  cm3 <- as.list(names(df3)); names(cm3) <- names(df3)
  expect_warning(
    withCallingHandlers(
      liver_markers(df3, col_map = cm3),
      # Muffle base numeric warning from sqrt()
      warning = function(w) {
        if (identical(conditionMessage(w), "NaNs produced")) invokeRestart("muffleWarning")
      },
      # Allow only our targeted rlang warning through
      rlang_warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl("sqrt\\(\\) undefined", msg)) return(invisible(NULL))
        invokeRestart("muffleWarning")
      }
    ),
    "sqrt\\(\\) undefined"
  )
})

test_that("diabetes='1' as character does not warn and BARD is computed as 4", {
  skip_on_cran()
  df <- tibble(
    BMI = 30, waist = 90, TG = 150, GGT = 30, age = 50,
    AST = 40, ALT = 30, platelets = 250, albumin = 45, diabetes = "1",
    bilirubin = 1.0, creatinine = 1.0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  expect_warning(
    out <- liver_markers(df, col_map = cm),
    NA
  )
  # BMI=30(+1) + AST/ALT=1.33>=0.8(+2) + diabetes="1"->1L(+1) = 4
  expect_equal(out$BARD, 4L)
})

test_that("non-binary diabetes values warn about coercion and propagate NA into BARD", {
  skip_on_cran()
  df <- tibble(
    BMI = 30, waist = 90, TG = 150, GGT = 30, age = 50,
    AST = 40, ALT = 30, platelets = 250, albumin = 45, diabetes = "yes",
    bilirubin = 1.0, creatinine = 1.0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  expect_warning(
    out <- withCallingHandlers(
      liver_markers(df, col_map = cm),
      warning = function(w) {
        if (grepl("^NAs introduced by coercion$", conditionMessage(w))) invokeRestart("muffleWarning")
      }
    ),
    "diabetes.*coercing"
  )
  expect_true(is.na(out$BARD))
})

test_that("validating inputs message appears when verbose is TRUE", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(
    BMI = 24, waist = 80, TG = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  expect_message(
    liver_markers(df, col_map = cm, verbose = TRUE),
    "liver_markers"
  )
})

test_that("verbose summary reports Inf counts for zero denominators", {
  skip_on_cran()
  df <- tibble(
    BMI = 24, waist = 80, TG = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 0, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(
    suppressWarnings(liver_markers(df, col_map = cm, verbose = TRUE)),
    "APRI 1/1"
  )
})
