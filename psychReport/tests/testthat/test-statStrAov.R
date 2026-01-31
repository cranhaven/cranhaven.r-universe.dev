context("statStrAov")

test_that("statStrAov", {

  set.seed(1)

  # create dataframe and add data with 2(Comp: comp vs. incomp) and 2(Side: left vs. right)
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp"),
                                "Side" = c("left", "right")))

  dat <- addDataDF(dat, RT = list("Comp:Side comp:left"    = c(500, 150, 100),
                                  "Comp:Side comp:right"   = c(500, 150, 100),
                                  "Comp:Side incomp:left"  = c(550, 150, 100),
                                  "Comp:Side incomp:right" = c(550, 150, 100)))

  # base R aov
  aovRT <- aov(RT ~ Comp*Side + Error(VP/(Comp*Side)), dat)

  aovStringComp     <- statStrAov(aovRT, "Comp")
  aovStringSide     <- statStrAov(aovRT, "Side")
  aovStringCompSide <- statStrAov(aovRT, "Comp:Side")

  testthat::expect_equal(aovStringComp,     "\\emph{F}(1, 49) = 4.39, \\emph{p} = .041, $\\eta_{p}^2$ = 0.08")
  testthat::expect_equal(aovStringSide,     "\\emph{F}(1, 49) = 0.32, \\emph{p} = .574, $\\eta_{p}^2$ = 0.01")
  testthat::expect_equal(aovStringCompSide, "\\emph{F}(1, 49) = 0.49, \\emph{p} = .489, $\\eta_{p}^2$ = 0.01")

  # repeated measures ANOVA using ezANOVA
  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp, Side), return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT)

  aovStringComp     <- statStrAov(aovRT, "Comp")
  aovStringSide     <- statStrAov(aovRT, "Side")
  aovStringCompSide <- statStrAov(aovRT, "Comp:Side")

  testthat::expect_equal(aovStringComp,     "\\emph{F}(1, 49) = 4.39, \\emph{p} = .041, $\\eta_{p}^2$ = 0.08")
  testthat::expect_equal(aovStringSide,     "\\emph{F}(1, 49) = 0.32, \\emph{p} = .574, $\\eta_{p}^2$ = 0.01")
  testthat::expect_equal(aovStringCompSide, "\\emph{F}(1, 49) = 0.49, \\emph{p} = .489, $\\eta_{p}^2$ = 0.01")

  # create dataframe and add data with 2(Comp: comp vs. incomp) and 2(Side: left vs. right)
  dat <- createDF(nVP = 50, nTrl = 1, design = list("Comp" = c("comp", "neutral", "incomp")))
  dat <- addDataDF(dat)

  # ezANOVA
  dat$VP <- as.factor(dat$VP)
  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp), return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT, sphericityCorrectionAdjDF = FALSE)

  aovStringComp <- statStrAov(aovRT, "Comp")
  testthat::expect_equal(aovStringComp, "\\emph{F}(2, 98) = 0.02, \\emph{p} = .980, $\\eta_{p}^2$ = 0.00, $\\epsilon$ = 0.97")

})
