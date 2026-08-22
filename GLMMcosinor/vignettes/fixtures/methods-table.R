dat_list <- list(
  list(
    name = "GLMMcosinor",
    language = "R",
    multicomponent = TRUE,
    disp = TRUE,
    zi = TRUE,
    rhythmdiff = FALSE,
    diff_est = TRUE,
    family = "n > 15 \nAny family avilable in {glmmTMB}:
    Gaussian, gamma, binomial, Poisson, ...",
    estimates = "Amplitude, acrophase, MESOR",
    ref = "Parsons, 2023"
  ),
  list(
    name = "CosinorPy",
    language = "Python",
    multicomponent = TRUE,
    disp = FALSE,
    zi = FALSE,
    rhythmdiff = TRUE,
    diff_est = TRUE,
    family = "n = 3 \nGaussian, Poisson, negative-binomial",
    estimates = "Amplitude, acrophase, MESOR",
    ref = "Moskon, 2020"
  ),
  list(
    name = "RhythmCount",
    language = "Python",
    multicomponent = TRUE,
    disp = FALSE,
    zi = TRUE,
    rhythmdiff = FALSE,
    diff_est = TRUE,
    family = "n = 6 \nPoisson, generalised-Poisson, zero-inflated
    Poisson, negative binomial, zero-inflated negative-binomial",
    estimates = "Amplitude, acrophase, MESOR, Zenith",
    ref = "Velikajne, 2022"
  ),
  list(
    name = "FMM",
    language = "R",
    multicomponent = TRUE,
    disp = FALSE,
    zi = FALSE,
    rhythmdiff = FALSE,
    diff_est = TRUE,
    family = "n = 1 \nGaussian",
    estimates = "Amplitude, FMM phase angle parameters: alpha, beta, gamma",
    ref = "Fernández, 2022"
  ),
  list(
    name = "CircaCompare",
    language = "R",
    multicomponent = FALSE,
    disp = FALSE,
    zi = FALSE,
    rhythmdiff = FALSE,
    diff_est = TRUE,
    family = "n = 1 \nGaussian",
    estimates = "Amplitude, acrophase, MESOR, and exponential decay of
    any of these characteristics or the differences in them between groups",
    ref = "Parsons, 2020"
  ),
  list(
    name = "Cosinor",
    language = "R",
    multicomponent = FALSE,
    disp = FALSE,
    zi = FALSE,
    rhythmdiff = TRUE,
    diff_est = TRUE,
    family = "n = 1 \nGaussian",
    estimates = "Amplitude, acrophase, MESOR",
    ref = "Sachs, 2014"
  ),
  list(
    name = "Cosinor2",
    language = "R",
    multicomponent = FALSE,
    disp = FALSE,
    zi = FALSE,
    rhythmdiff = TRUE,
    diff_est = TRUE,
    family = "n = 1 \nGaussian",
    estimates = "Amplitude, acrophase, MESOR",
    ref = "Mutak, 2018"
  ),
  list(
    name = "DiscoRhythm",
    language = "R",
    multicomponent = FALSE,
    disp = FALSE,
    zi = FALSE,
    rhythmdiff = FALSE,
    diff_est = FALSE,
    family = "n = 1 \nGaussian",
    estimates = "Amplitude, acrophase",
    ref = "Carlucci, 2019"
  ),
  list(
    name = "LimoRhyde",
    language = "R",
    multicomponent = FALSE,
    disp = FALSE,
    zi = FALSE,
    rhythmdiff = TRUE,
    diff_est = TRUE,
    family = "n = 1 \nGaussian",
    estimates = "Amplitude, acrophase, MESOR, period",
    ref = "Singer, 2019"
  ),
  list(
    name = "Kronos",
    language = "R",
    multicomponent = TRUE,
    disp = FALSE,
    zi = FALSE,
    rhythmdiff = TRUE,
    diff_est = TRUE,
    family = "n = 1 \nGaussian",
    estimates = "Amplitude, acrophase, MESOR",
    ref = "Bastiaanssen, 2023"
  )
)
dat <- do.call(rbind, dat_list)
dat <- as.data.frame(dat)
dat <- dplyr:::mutate(
  dat,
  dplyr::across(dplyr::everything(), unlist),
  dplyr::across(
    multicomponent:diff_est,
    function(x) ifelse(x, "&#x2713;", "&#x2717;")
  )
)

dat <- rbind(
  dplyr::filter(dat, name == "GLMMcosinor"),
  dplyr::arrange(dplyr::filter(dat, name != "GLMMcosinor"), name)
)
dat <- flextable::flextable(dat)
dat <- flextable::color(
  dat,
  j = c("multicomponent", "disp", "zi", "rhythmdiff", "diff_est"),
  col = function(x) ifelse(x == "&#x2713;", "green", "red")
)
dat <- ftExtra::colformat_md(dat)
dat <- flextable::theme_zebra(
  dat,
  even_header = "#FFFFFF",
  even_body = "#FFFFFF"
)
dat <- flextable::set_header_labels(
  dat,
  values = list(
    name = "Software",
    language = "Language",
    multicomponent = "Multicomponent",
    disp = "Dispersion model",
    zi = "Zero-inflated model",
    rhythmdiff = "Differential rhythmicity",
    diff_est = "Parameter estimates for differences",
    estimates = "Estimated parameters",
    family = "Family (available link functions)",
    ref = "Reference"
  )
)
