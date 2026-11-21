test_that("journal style reaches the bivariate renderer via style() on a result", {
  a <- suppressMessages(tb(epitabl, renal_impairment, mace_event, design = "cohort", ref = "No"))
  styled <- style(a, "lancet")

  expect_identical(a$data, styled$data)
  ratio_col <- grep("\\(95% CI\\)$", names(as.data.frame(styled)), value = TRUE)
  plain <- as.data.frame(a)[[ratio_col]]
  lancet <- as.data.frame(styled)[[ratio_col]]

  expect_true(any(grepl("^\\d+\\.\\d{2} \\(\\d+\\.\\d{2} - \\d+\\.\\d{2}\\)", plain)))
  expect_true(any(grepl("^\\d+\\.\\d{2} \\(\\d+\\.\\d{2}, \\d+\\.\\d{2}\\)", lancet)))
  expect_false(any(grepl(" - ", lancet)))
  expect_true(any(grepl("p (=|<) \\.\\d{3}", lancet)))
  expect_output(print(styled), "\\d\\.\\d{2}, \\d")
})

test_that("tb journal formatting covers custom styles, cells, and explicit templates", {
  a <- suppressMessages(tb(epitabl, renal_impairment, mace_event, flags = "row", design = "cohort", ref = "No"))
  ratio_col <- grep("\\(95% CI\\)$", names(as.data.frame(a)), value = TRUE)

  custom <- as.data.frame(style(a, journal_style(ci_sep = " to ", digits_est = 3)))
  expect_true(any(grepl("^\\d+\\.\\d{3} \\(\\d+\\.\\d{3} to \\d+\\.\\d{3}\\)", custom[[ratio_col]])))

  # A preset name is a journal, never a literal {n}/{p} cell template.
  lancet <- as.data.frame(style(a, "lancet"))
  cells <- unlist(lancet[-1][setdiff(names(lancet)[-1], ratio_col)])
  expect_false(any(cells == "lancet"))
  expect_true(any(grepl("^\\d+ \\(\\d+\\.\\d%\\)$", cells)))
  nejm <- as.data.frame(style(a, "nejm"))
  expect_true(any(grepl("^\\d+ \\(\\d+\\.\\d\\)$", unlist(nejm[-1]))))

  # An explicitly customised effect template still wins over the journal.
  own <- suppressMessages(tb(
    epitabl, renal_impairment, mace_event, design = "cohort", ref = "No",
    style.rp = "{rp} [{lower}; {upper}]"
  ))
  expect_true(any(grepl("\\[\\d+\\.\\d{2}; \\d+\\.\\d{2}\\]", as.data.frame(style(own, "lancet"))[[ratio_col]])))
})

test_that("tb journal style applies through the direct argument and the spec grammar", {
  direct <- suppressMessages(tb(epitabl, renal_impairment, mace_event, design = "cohort", ref = "No", style = "lancet"))
  ratio_col <- grep("\\(95% CI\\)$", names(as.data.frame(direct)), value = TRUE)
  expect_true(any(grepl(", ", as.data.frame(direct)[[ratio_col]])))

  base <- suppressMessages(tb(epitabl, renal_impairment, mace_event, design = "cohort", ref = "No"))
  grammar <- suppressMessages(evaluate(style(base$spec, "lancet")))
  expect_identical(as.data.frame(grammar), as.data.frame(direct))
})

test_that("tb gt and flextable renderers use the journal style", {
  a <- suppressMessages(tb(epitabl, renal_impairment, mace_event, flags = "p", design = "cohort", ref = "No"))
  styled <- style(a, "jama")
  ratio_col <- grep("\\(95% CI\\)$", names(as.data.frame(styled)), value = TRUE)

  skip_if_not_installed("gt")
  gt_tab <- as_gt(styled)
  expect_true(any(grepl(", ", gt_tab[["_data"]][[ratio_col]])))
  expect_false(any(grepl("\\d - \\d", gt_tab[["_data"]][[ratio_col]])))

  skip_if_not_installed("flextable")
  ft <- flextable::as_flextable(styled)
  body <- ft$body$dataset[[ratio_col]]
  expect_true(any(grepl(", ", body)))
  expect_identical(unname(ft$body$styles$text$font.family$data[1, 1]), "Times New Roman")
})

test_that("journal style reaches the GLM renderer across print, data frame, gt and flextable", {
  r <- suppressMessages(regtab(epitabl, "rehospitalized", ~ renal_impairment + age, family = binomial("log")))
  lancet <- style(r, "lancet")
  custom <- style(r, journal_style(ci_sep = " to "))

  expect_identical(r$data, lancet$data)
  expect_identical(r$data, custom$data)

  est_col <- setdiff(names(as.data.frame(r)), c("Variable", grep("p-value$", names(as.data.frame(r)), value = TRUE)))[1]
  p_col <- grep("p-value$", names(as.data.frame(r)), value = TRUE)
  plain <- as.data.frame(r)[[est_col]]
  expect_true(any(grepl("\\d - \\d", plain)))
  expect_true(any(grepl("\\d to \\d", as.data.frame(custom)[[est_col]])))
  expect_true(any(grepl("\\d, \\d", as.data.frame(lancet)[[est_col]])))
  if (length(p_col)) {
    expect_true(all(grepl("^(|[<>]?\\.\\d+)$", as.data.frame(lancet)[[p_col]])))
  }
  expect_output(print(custom), "\\d to \\d")

  # Unstyled output and the "default" preset keep the established regtab text.
  expect_identical(as.data.frame(style(r, "default")), as.data.frame(r))

  skip_if_not_installed("gt")
  expect_true(any(grepl("\\d, \\d", as_gt(lancet)[["_data"]][[est_col]])))

  skip_if_not_installed("flextable")
  ft <- flextable::as_flextable(lancet)
  expect_true(any(grepl("\\d, \\d", ft$body$dataset[[est_col]])))
  expect_identical(unname(ft$body$styles$text$font.family$data[1, 1]), "Times New Roman")
})

test_that("GLM journal style applies through the spec grammar", {
  r <- suppressMessages(regtab(epitabl, "rehospitalized", ~ renal_impairment + age, family = binomial("log")))
  grammar <- suppressMessages(evaluate(style(r$spec, "lancet")))
  expect_identical(grammar$data, r$data)
  expect_identical(as.data.frame(grammar), as.data.frame(style(r, "lancet")))
})

test_that("style() on a report restyles every item without changing evidence", {
  rep <- suppressMessages(simtablr(epitabl, rehospitalized, renal_impairment, vars = c(age, sex), design = "cohort"))
  styled <- style(rep, "lancet")
  expect_s3_class(styled, "simtab_report")
  for (nm in names(rep$items)) {
    expect_identical(styled$items[[nm]]$data, rep$items[[nm]]$data)
    expect_identical(styled$items[[nm]]$meta$style, "lancet")
  }
  expect_error(style(rep), class = "simtab_error")
})

test_that("journal style reaches the Cox renderer while the default stays unchanged", {
  skip_if_not_installed("survival")
  s <- survtab(epitabl, mace_time_days, mace_event, ~ renal_impairment + age)
  hr <- as.data.frame(s)[["HR (95% CI)"]]
  expect_true(all(grepl("^\\d+\\.\\d{2} \\(\\d+\\.\\d{2}, \\d+\\.\\d{2}\\)$", hr)))

  custom <- style(s, journal_style(ci_sep = " to ", digits_est = 3))
  expect_identical(custom$data, s$data)
  expect_true(all(grepl("^\\d+\\.\\d{3} \\(\\d+\\.\\d{3} to \\d+\\.\\d{3}\\)$", as.data.frame(custom)[["HR (95% CI)"]])))
  expect_output(print(custom), " to ")
})

test_that("style() restyles every table in an rbind() of tb results", {
  stacked <- rbind(
    suppressMessages(tb(epitabl, sex, mace_event, rr, ref = "Female")),
    suppressMessages(tb(epitabl, smoking, mace_event, rr, ref = "Never"))
  )
  styled <- style(stacked, "lancet")

  expect_s3_class(styled, "simtab_rbind_tb")
  expect_identical(styled$data, stacked$data)
  ratio_col <- grep("\\(95% CI\\)$", names(as.data.frame(styled)), value = TRUE)
  cells <- as.data.frame(styled)[[ratio_col]]
  expect_true(any(grepl("^\\d+\\.\\d{2} \\(\\d+\\.\\d{2}, \\d+\\.\\d{2}\\)", cells)))
  expect_false(any(grepl("\\d - \\d", cells)))
  expect_error(style(stacked, "no-such-journal"), class = "simtab_error")
})

test_that("interval separators switch only where they would be ambiguous", {
  comma <- suppressMessages(tb(epitabl, renal_impairment, mace_event, rr, ref = "No", decimal_mark = ","))
  lancet <- as.data.frame(style(comma, "lancet"))
  ratio_col <- grep("95% CI", names(lancet), fixed = TRUE, value = TRUE)
  expect_true(any(grepl("1,94 (1,51; 2,49)", lancet[[ratio_col]], fixed = TRUE)))
  expect_identical(style(comma, "lancet")$data, comma$data)

  fit <- suppressMessages(regtab(epitabl, "systolic_bp", ~ sex + age, family = gaussian()))
  cells <- as.data.frame(fit)[[2]]
  expect_true(any(grepl("(-0.58 to 3.34)", cells, fixed = TRUE)))
  expect_true(any(grepl("(0.26 - 0.40)", cells, fixed = TRUE)))
  nejm <- as.data.frame(style(fit, "nejm"))[[2]]
  expect_true(any(grepl("(-0.58 to 3.34)", nejm, fixed = TRUE)))

  # Unambiguous output is unchanged.
  plain <- as.data.frame(suppressMessages(tb(epitabl, renal_impairment, mace_event, rr, ref = "No")))
  expect_true(any(grepl("1.94 (1.51 - 2.49)", plain[[ratio_col]], fixed = TRUE)))
  expect_identical(SimtablR:::.ci_sep_safe(", ", c("0.26", "0.40")), ", ")
})
