params <-
list(EVAL = FALSE)

## ----SETTINGS-knitr, include=FALSE------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::knit_hooks$set(purl = knitr::hook_purl)
options(width = 90)
# fig.width = 6, fig.height=4, fig.asp = 1.0,
knitr::opts_chunk$set(
  echo = FALSE,
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE,
  # purl = NOT_CRAN,
  # eval = NOT_CRAN,
  dev = "jpeg",
  dpi = 40,
  fig.cap = "",
  fig.asp = 0.8,
  fig.height=4,
  fig.width = 5,
  out.width = "60%",
  fig.align = "center"
)

## ----lib, eval=FALSE, echo=TRUE,include=TRUE, warning= FALSE, message=FALSE-------------
#  if (!require(bsitar)) install.packages('bsitar')
#  if (!require(sitar)) install.packages('sitar')
#  if (!require(ggplot2)) install.packages('ggplot2')
#  if (!require(knitr)) install.packages('knitr')
#  if (!require(kableExtra)) install.packages('kableExtra')

## ----loadlib----------------------------------------------------------------------------
#  stopifnot(require(knitr))
#  stopifnot(require(kableExtra))
#  stopifnot(require(ggplot2))
#  stopifnot(require(sitar))
#  library(bsitar)
#  library(ggplot2)
#  existsdata <- existsfit  <- FALSE

## ----modelfit, results='hide', echo=TRUE,include=TRUE, warning= FALSE, message=FALSE----
#  if(existsdata) {
#    if(exists('berkeley_mdata')) data <- berkeley_mdata else stop("data does not exist")
#  } else {
#    data <- na.omit(berkeley[berkeley$sex == "Female" &
#                           berkeley$age >= 8 & berkeley$age <= 18,
#                         c('id', 'age', 'height')])
#  }
#  
#  
#  # Fit SITAR model (fit via 'sitar' package)
#  if(existsfit) {
#    model_frequ <- sitar::sitar(x = age, y = height, id = id, data = data, df = 4)
#  } else {
#    model_frequ <- sitar::sitar(x = age, y = height, id = id, data = data, df = 5)
#  }
#  
#  
#  # Fit Bayesian SITAR model (via 'bsitar' package) - note prefix 'b' to 'model_bayes'.
#  # To save time, the model is fit by running two (default four) parallel chain
#  # with 1000 iterations (default 2000) per chain. Note also we have requested to
#  # expose Stan functions by setting expose_function as TRUE (default FALSE) and
#  # save them for later use.
#  
#  # model_bayes <- bsitar(x = age, y = height, id = id, data = data, df = 5)
#  
#  if(existsfit) {
#    if(exists('berkeley_mfit')) model_bayes <- berkeley_mfit else stop("model does not exist")
#  } else {
#    model_bayes <-  bsitar(x = age, y = height, id = id, data = data, df = 5,
#                  chains = 2, cores = 2, iter = 2000, refresh = 100,
#                  sample_prior = "no",
#                  expose_function = TRUE)
#  }

## ----summary_sitar, results='show', echo=FALSE,include=TRUE-----------------------------
#  print(model_frequ)

## ----summary_bsitar, results='show', echo=FALSE,include=TRUE----------------------------
#  print(model_bayes)

## ----prepare_advNALL, eval=TRUE,echo=TRUE,include=FALSE, warning= FALSE, message=FALSE----
# When NOT_CRAN, set eval=TRUE to avoid error on sitar_ranef_id_min and sitar_ranef_id_max
sitar_ranef_id_min   <- data.frame(id=1:1, a = NA_real_, b = NA_real_, c = NA_real_)
sitar_ranef_id_max   <- data.frame(id=1:1, a = NA_real_, b = NA_real_, c = NA_real_)
bsitar_ranef_id_min  <- data.frame(id=1:1, a = NA_real_, b = NA_real_, c = NA_real_)
bsitar_ranef_id_max  <- data.frame(id=1:1, a = NA_real_, b = NA_real_, c = NA_real_)

## ----prepare_adv, echo=TRUE,include=TRUE, warning= FALSE, message=FALSE-----------------
#  idvar <- 'id'
#  xvar  <- 'age'
#  yvar  <- 'height'
#  
#  # For observed and adjusted plot
#  data_sitar_a  <- plot(model_frequ, opt = 'a', xlim = sitar::xaxsd(), ylim = sitar::yaxsd(), returndata = T)
#  data_bsitar_a <- plot_curves(model_bayes, opt = 'a', returndata = T)
#  data_all_a <- data %>% dplyr::mutate(Curve = 'observed') %>%
#    dplyr::bind_rows(., data_sitar_a %>% dplyr::relocate(all_of(colnames(data))) %>%
#                       dplyr::mutate(Curve = 'adjusted_sitar')) %>%
#    dplyr::bind_rows(., data_bsitar_a %>% dplyr::relocate(all_of(colnames(data))) %>%
#                       dplyr::mutate(Curve = 'adjusted_bsitar')) %>%
#    dplyr::mutate(Curve = as.factor(Curve))
#  
#  
#  # For distance and velocity (dv) plots
#  data_sitar_d  <- plot(model_frequ, opt = 'd', xlim = sitar::xaxsd(), ylim = sitar::yaxsd(), returndata = T)
#  data_sitar_v  <- plot(model_frequ, opt = 'v', xlim = sitar::xaxsd(), ylim = sitar::yaxsd(), returndata = T)
#  data_sitar_df <- data_sitar_d %>% data.frame()
#  data_bsitar_d <- plot_curves(model_bayes, opt = 'd', returndata = T, newdata = data_sitar_df, ipts = NULL, usesavedfuns = T, envir = globalenv())
#  data_bsitar_v <- plot_curves(model_bayes, opt = 'v', returndata = T, newdata = data_sitar_df, ipts = NULL, usesavedfuns = T, envir = globalenv())
#  
#  data_sitar_d  <- data_sitar_d %>% dplyr::rename(distance = height)
#  data_sitar_v  <- data_sitar_v %>% dplyr::rename(velocity = height)
#  data_bsitar_d <- data_bsitar_d %>% dplyr::rename(distance = Estimate) %>% dplyr::select(all_of(colnames(data_sitar_d)))
#  data_bsitar_v <- data_bsitar_v %>% dplyr::rename(velocity = Estimate) %>% dplyr::select(all_of(colnames(data_sitar_v)))
#  
#  idx <- c('id', 'age')
#  data_sitar_d  <- data_sitar_d %>% dplyr::relocate(all_of(idx))
#  data_sitar_v  <- data_sitar_v %>% dplyr::relocate(all_of(idx))
#  data_bsitar_d <- data_bsitar_d %>% dplyr::relocate(all_of(idx))
#  data_bsitar_v <- data_bsitar_v %>% dplyr::relocate(all_of(idx))
#  
#  data_sitar_dv <- data_sitar_d %>% dplyr::left_join(., data_sitar_v, by = idx)
#  data_bsitar_dv <- data_bsitar_d %>% dplyr::left_join(., data_bsitar_v, by = idx)
#  
#  data_all_dv <- data_sitar_dv %>% dplyr::mutate(Model = 'sitar') %>%
#    dplyr::bind_rows(., data_bsitar_dv %>%
#                       dplyr::mutate(Model = 'bsitar'))
#  
#  
#  
#  # For Distance and Velocity (DV) plots
#  data_sitar_D  <- plot(model_frequ, opt = 'D', xlim = sitar::xaxsd(), ylim = sitar::yaxsd(), returndata = T)
#  data_sitar_V  <- plot(model_frequ, opt = 'V', xlim = sitar::xaxsd(), ylim = sitar::yaxsd(), returndata = T)
#  data_sitar_DF <- data_sitar_D %>% data.frame()
#  data_bsitar_D <- plot_curves(model_bayes, opt = 'D', returndata = T, newdata = data_sitar_DF, ipts = NULL, usesavedfuns = T, envir = globalenv())
#  data_bsitar_V <- plot_curves(model_bayes, opt = 'V', returndata = T, newdata = data_sitar_DF, ipts = NULL, usesavedfuns = T, envir = globalenv())
#  
#  data_sitar_D  <- data_sitar_D %>% dplyr::rename(distance = height)
#  data_sitar_V  <- data_sitar_V %>% dplyr::rename(velocity = height)
#  data_bsitar_D <- data_bsitar_D %>% dplyr::rename(distance = Estimate) %>% dplyr::select(all_of(colnames(data_sitar_D)))
#  data_bsitar_V <- data_bsitar_V %>% dplyr::rename(velocity = Estimate) %>% dplyr::select(all_of(colnames(data_sitar_V)))
#  
#  idx <- c('id', 'age')
#  data_sitar_D  <- data_sitar_D %>% dplyr::relocate(all_of(idx))
#  data_sitar_V  <- data_sitar_V %>% dplyr::relocate(all_of(idx))
#  data_bsitar_D <- data_bsitar_D %>% dplyr::relocate(all_of(idx))
#  data_bsitar_V <- data_bsitar_V %>% dplyr::relocate(all_of(idx))
#  
#  data_sitar_DV <- data_sitar_D %>% dplyr::left_join(., data_sitar_V, by = idx)
#  data_bsitar_DV <- data_bsitar_D %>% dplyr::left_join(., data_bsitar_V, by = idx)
#  
#  data_all_DV <- data_sitar_DV %>% dplyr::mutate(Model = 'sitar') %>%
#    dplyr::bind_rows(., data_bsitar_DV %>%
#                       dplyr::mutate(Model = 'bsitar'))
#  
#  
#  
#  
#  # Set x range for x axis
#  xrange <- c(min(data$age), max(data$age))
#  
#  # Get random effects for sitar model
#  fh1_ranef <- nlme::ranef(model_frequ)
#  
#  # Get random effects for bsitar model
#  setid <- model_bayes$model_info$ids
#  bfh1_ranef_frame    <- brms::ranef(model_bayes)
#  bfh1_ranef_frame_id <- bfh1_ranef_frame[[setid]]
#  dimxi_l <- list()
#  for (dimxi in 1:dim(bfh1_ranef_frame_id)[3]) {
#    dimxi_l[[dimxi]] <- bfh1_ranef_frame_id[,,dimxi][,1]
#  }
#  
#  bfh1_ranef <- dimxi_l %>% do.call(cbind, .) %>%
#    data.frame() %>%
#    setNames(letters[1:3])
#  
#  # Get correlation of random effects for sitar model
#  fh1_ranef_corr  <- cor(fh1_ranef)
#  
#  # Get correlation of random effects for bsitar model
#  bfh1_ranef_corr <- cor(bfh1_ranef)
#  colnames(bfh1_ranef_corr) <- letters[1:3]
#  rownames(bfh1_ranef_corr) <- letters[1:3]
#  
#  # Get random effects along with id cilumn for sitar model
#  fh1_ranef_id  <- fh1_ranef %>% data.frame() %>% tibble::rownames_to_column(., "id")
#  
#  # Get random effects along with id cilumn for sitar model
#  bfh1_ranef_id <- bfh1_ranef %>% data.frame() %>% tibble::rownames_to_column(., "id")
#  
#  # Get ids with random effects size (a) for the shortest and tallest individual - sitar model
#  # ranef_id_max_min <- fh1_ranef_id %>%
#  #   dplyr::slice(c(which.min(a), which.max(a))) %>%
#  #   dplyr::select(id) %>% unlist() %>% as.vector()
#  
#  # Get ids for shortest and tallest individual - predicted by the sitar model
#  fh1_ranef_id_pred     <- predict(model_frequ, level = 1)
#  fh1_ranef_id_pred_max <- fh1_ranef_id_pred[ which.max(fh1_ranef_id_pred)]
#  fh1_ranef_id_pred_min <- fh1_ranef_id_pred[ which.min(fh1_ranef_id_pred)]
#  ranef_id_max_min      <- c(attr(fh1_ranef_id_pred_max, "names"),
#                             attr(fh1_ranef_id_pred_min, "names"))
#  
#  # Get individuals with max min random effects for size - sitar
#  fh1_ranef_id_max_min <- fh1_ranef_id %>%
#    dplyr::slice(c(which.min(a), which.max(a))) %>%
#    dplyr::select(id) %>% unlist() %>% as.vector()
#  
#  # Get individuals with max min random effects for size - bsitar
#  bfh1_ranef_id_max_min <- bfh1_ranef_id %>%
#    dplyr::slice(c(which.min(a), which.max(a))) %>%
#    dplyr::select(id) %>% unlist() %>% as.vector()
#  
#  sitar_ranef_id_min <- fh1_ranef_id %>% dplyr::filter(.data[[idvar]] %in% ranef_id_max_min[1])
#  sitar_ranef_id_max <- fh1_ranef_id %>% dplyr::filter(.data[[idvar]] %in% ranef_id_max_min[2])
#  
#  bsitar_ranef_id_min <- bfh1_ranef_id %>% dplyr::filter(.data[[idvar]] %in% ranef_id_max_min[2])
#  bsitar_ranef_id_max <- bfh1_ranef_id %>% dplyr::filter(.data[[idvar]] %in% ranef_id_max_min[2])
#  
#  # Set legend theme for ggplot
#  theme_legends1 <- theme(legend.position = "bottom",
#                         legend.title = element_text(size=12),
#                         legend.key.height = unit(1.5, 'cm'),
#                         legend.key.width = unit(1.5, 'cm'),
#                         legend.key.size = unit(1.5, 'cm'),
#                         legend.text = element_text(size=12))
#  
#  theme_legends2 <- theme(legend.position = "right",
#                         legend.title = element_text(size=12),
#                         legend.key.height = unit(1.5, 'cm'),
#                         legend.key.width = unit(1.5, 'cm'),
#                         legend.key.size = unit(1.5, 'cm'),
#                         legend.text = element_text(size=12))
#  
#  # Set grid theme for ggplot
#  theme_grids1 <- theme(panel.grid.major = element_blank(),
#                        panel.grid.minor = element_blank()) +
#    theme_bw()
#  
#  linewidths <- c(0.5, 0.75, 1.0, 1.5, 2.0)
#  alphas     <- c(0.25, 0.5, 0.75, 1.0, 1.0)
#  colours    <- c('black', 'red', 'green', 'orange', 'magenta')
#  linetypes  <- c('solid', 'dashed', 'dotted', 'dotdash', 'longdash', 'twodash')

## ----prepare_apv, echo=FALSE,include=FALSE, warning= FALSE, message=FALSE---------------
#  parms_apv_sitar <- plot(model_frequ, apv = T, returndata = F)
#  parms_apv_sitar <- parms_apv_sitar$apv %>% data.frame() %>% setNames('sitar')
#  parms_apv_sitar <- round(parms_apv_sitar, 2)
#  row.names(parms_apv_sitar) <- NULL
#  parms_apv_bsitar <- plot_curves(model_bayes, apv = T, newdata = data_sitar_df, ipts = NULL, usesavedfuns = T, envir = globalenv())
#  parms_apv_bsitar <- parms_apv_bsitar$growthparameters
#  colnames(parms_apv_bsitar) <- c('Parameter', 'bsitar')
#  parms_apv_all <- cbind(parms_apv_bsitar, parms_apv_sitar)

## ----prepare_Vapv, echo=FALSE,include=FALSE, warning= FALSE, message=FALSE--------------
#  # Individual specific APGV and PGV
#  data_sitar_Vapv  <- plot(model_frequ, opt = 'V', apv = T)$apv %>% data.frame()
#  data_bsitar_Vapv <- plot_curves(model_bayes, opt = 'V', returndata = T, newdata = data_sitar_DF, ipts = NULL, usesavedfuns = T, envir = globalenv(),
#                                  apv = T)
#  data_bsitar_Vapv <- attr(data_bsitar_Vapv, "growthparameters")
#  data_bsitar_Vapv <- data_bsitar_Vapv %>%
#    tidyr::pivot_wider(names_from = Parameter, values_from = c(Estimate))
#  colnames(data_sitar_Vapv) <- colnames(data_bsitar_Vapv)

## ----plota, echo=TRUE,include=TRUE, fig.width = 6, fig.height=4, fig.asp = 1.0, fig.cap="\\label{fig:plota}Observed curves along with the superimposed adjusted curves (via random effects) for the **sitar** and **bsitar** models. The observed curves are shown as solid light grey lines whereas the adjusted curves for the **sitar** model are displayed as solid black lines. The adjusted curves for the **bsitar** model are shown as dotted black lines."----
#  data_all_a <- data_all_a %>%
#    dplyr::mutate(across(Curve, ~factor(.,
#                                        levels=c("observed",
#                                                 "adjusted_sitar",
#                                                 "adjusted_bsitar"))))
#  data_all_alevs <- levels(data_all_a$Curve)
#  
#  p <-
#    data_all_a %>%
#    dplyr::mutate(groupby = interaction(id, Curve) ) %>%
#    ggplot(., aes(x = age))
#  
#  p +
#    geom_line(data = data_all_a %>%
#                dplyr::filter(Curve==data_all_alevs[1]) %>%
#                dplyr::mutate(groupby = interaction(id, Curve)),
#              aes(x = age, y = height, group = groupby),
#              linetype = linetypes[1],
#               color = colours[1], linewidth = linewidths[1], alpha = alphas[1],
#              show.legend = T) +
#    geom_line(data = data_all_a %>%
#                dplyr::filter(Curve==data_all_alevs[2]) %>%
#                dplyr::mutate(groupby = interaction(id, Curve)),
#              aes(x = age, y = height, group = groupby),
#              linetype = linetypes[1],
#               color = colours[1], linewidth = linewidths[1], alpha = alphas[3],
#              show.legend = T) +
#    geom_line(data = data_all_a %>%
#                dplyr::filter(Curve==data_all_alevs[3]) %>%
#                dplyr::mutate(groupby = interaction(id, Curve)),
#              aes(x = age, y = height, group = groupby),
#              linetype = linetypes[2],
#               color = colours[1], linewidth = linewidths[1], alpha = alphas[4],
#              show.legend = T) +
#    scale_x_continuous(breaks = seq(xrange[1], xrange[2], 1), limits = xrange) +
#    theme_grids1 + theme_legends1 +
#    theme(legend.title=element_blank())
#  
#  # data_all_a %>%
#  #   dplyr::mutate(groupby = interaction(id, Curve) ) %>%
#  #   ggplot(., aes(x = age)) +
#  #   geom_line(aes(x = age, y = height, group = groupby,
#  #                 linetype = Curve),
#  #             color = colours[1], alpha = alphas[2], linewidth = linewidths[2],
#  #             show.legend = T) +
#  #   scale_x_continuous(breaks = seq(xrange[1], xrange[2], 1), limits = xrange) +
#  #   theme_grids1 + theme_legends1

## ----plotuadids, echo=TRUE,include=TRUE, fig.width = 6, fig.height=4, fig.asp = 1.0, fig.cap="\\label{fig:plotuadids}Observed and adjusted curve (via random effects) curves along with the population averag curves for **sitar** and **bsitar** models. The observed curves for all individuals are shown as solid light grey lines whereas curves for the tallest and shortest individuals are displayed using the solid black lines. The adjusted curves for the tallest and shortest individuals are shown as dotted black lines, and the population average curve is shown as solid black line"----
#  
#  data_sitar_u_f <- plot(model_frequ, opt = 'u', returndata = T) %>%
#    dplyr::relocate( all_of(idvar),  all_of(xvar), all_of(yvar)) %>%
#    dplyr::mutate(Curve = 'u') %>% dplyr::mutate(Model = 'sitar')
#  data_sitar_a_f <- plot(model_frequ, opt = 'a', returndata = T) %>%
#    dplyr::relocate( all_of(idvar),  all_of(xvar), all_of(yvar)) %>%
#    dplyr::mutate(Curve = 'a') %>% dplyr::mutate(Model = 'sitar')
#  data_sitar_d_f <- plot(model_frequ, opt = 'd', returndata = T) %>%
#    dplyr::relocate( all_of(idvar),  all_of(xvar), all_of(yvar)) %>%
#    dplyr::mutate(Curve = 'd') %>% dplyr::mutate(Model = 'sitar')
#  
#  data_sitar_uad_df12 <- data_sitar_u_f[, 1:2]
#  data_sitar_uad_df123 <- data_sitar_u_f[, 1:3]
#  data_bsitar_u_f <- plot_curves(model_bayes, opt = 'u', returndata = T, newdata = data_sitar_uad_df123, ipts = NULL, usesavedfuns = T, envir = globalenv())
#  data_bsitar_a_f <- plot_curves(model_bayes, opt = 'a', returndata = T, newdata = data_sitar_uad_df123, ipts = NULL, usesavedfuns = T, envir = globalenv())
#  data_bsitar_d_f <- plot_curves(model_bayes, opt = 'd', returndata = T, newdata = data_sitar_uad_df12, ipts = NULL, usesavedfuns = T, envir = globalenv())
#  
#  data_bsitar_u_f <- data_bsitar_u_f %>% dplyr::rename(!!yvar := yvar) %>% dplyr::select(all_of(colnames(data_sitar_uad_df123)))
#  data_bsitar_a_f <- data_bsitar_a_f %>% dplyr::rename(!!yvar := yvar) %>% dplyr::select(all_of(colnames(data_sitar_uad_df123)))
#  data_bsitar_d_f <- data_bsitar_d_f %>% dplyr::rename(!!yvar := 'Estimate') %>% dplyr::select(all_of(colnames(data_sitar_uad_df123)))
#  
#  data_bsitar_u_f <- data_bsitar_u_f %>% dplyr::mutate(Curve = 'u') %>% dplyr::mutate(Model = 'bsitar')
#  data_bsitar_a_f <- data_bsitar_a_f %>% dplyr::mutate(Curve = 'a') %>% dplyr::mutate(Model = 'bsitar')
#  data_bsitar_d_f <- data_bsitar_d_f %>% dplyr::mutate(Curve = 'd') %>% dplyr::mutate(Model = 'bsitar')
#  
#  data_sitar_uad_f <- data_sitar_u_f %>%
#    dplyr::bind_rows(., data_sitar_a_f) %>%
#    dplyr::bind_rows(., data_sitar_d_f)
#  
#  data_bsitar_uad_f <- data_bsitar_u_f %>%
#    dplyr::bind_rows(., data_bsitar_a_f) %>%
#    dplyr::bind_rows(., data_bsitar_d_f)
#  
#  
#  data_sitar_uad_f <- data_sitar_uad_f %>%
#    dplyr::mutate(across(Curve, ~factor(.,
#                                        levels=c("u",
#                                                 "a",
#                                                 "d"))))
#  
#  data_sitar_uad_f <- data_sitar_uad_f %>%
#    dplyr::mutate(Curve = dplyr::recode(Curve, u = "unadj",
#                            a = "adj",
#                            d = "pop.avg"))
#  
#  
#  
#  data_bsitar_uad_f <- data_bsitar_uad_f %>%
#    dplyr::mutate(across(Curve, ~factor(.,
#                                        levels=c("u",
#                                                 "a",
#                                                 "d"))))
#  
#  data_bsitar_uad_f <- data_bsitar_uad_f %>%
#    dplyr::mutate(Curve = dplyr::recode(Curve, u = "unadj",
#                                        a = "adj",
#                                        d = "pop.avg"))
#  
#  
#  data_sitar_bsitar_uad_f <- data_sitar_uad_f %>%
#    dplyr::bind_rows(., data_bsitar_uad_f)
#  
#  data_all_alevs <- levels(data_sitar_uad_f$Curve)
#  
#  
#  which_uad_f <- data_sitar_bsitar_uad_f # data_sitar_uad_f
#  
#  
#  p <-
#    which_uad_f %>%
#    dplyr::mutate(groupby = interaction(.data[[idvar]], Curve) ) %>%
#    ggplot(., aes(x = .data[[xvar]]))
#  
#  p +
#    geom_line(data = which_uad_f %>%
#                dplyr::filter(Curve==data_all_alevs[1]) %>%
#                dplyr::mutate(groupby = interaction(.data[[idvar]], Curve)),
#              aes(x = .data[[xvar]], y = .data[[yvar]], group = groupby),
#              linetype = linetypes[1],
#             color = colours[1], linewidth = linewidths[1], alpha = alphas[1],
#              show.legend = T) +
#    geom_line(data = which_uad_f %>%
#                dplyr::filter(Curve==data_all_alevs[1]) %>%
#                dplyr::filter(.data[[idvar]] %in% ranef_id_max_min[1]) %>%
#                dplyr::mutate(Curve =
#                                paste0(Curve, ".", idvar, ".", ranef_id_max_min[1])) %>%
#                dplyr::mutate(groupby = interaction(.data[[idvar]], Curve)),
#              aes(x = .data[[xvar]], y = .data[[yvar]], group = groupby),
#              linetype = linetypes[1],
#              color = colours[1], linewidth = linewidths[2], alpha = alphas[4],
#                show.legend = T) +
#    geom_line(data = which_uad_f %>%
#                dplyr::filter(Curve==data_all_alevs[2]) %>%
#                dplyr::filter(.data[[idvar]] %in% ranef_id_max_min[1]) %>%
#                dplyr::mutate(Curve =
#                                paste0(Curve, ".", idvar, ".", ranef_id_max_min[1])) %>%
#                dplyr::mutate(groupby = interaction(.data[[idvar]], Curve)),
#              aes(x = .data[[xvar]], y = .data[[yvar]], group = groupby),
#              linetype = linetypes[2],
#              color = colours[1], linewidth = linewidths[2], alpha = alphas[4],
#              show.legend = T) +
#    geom_line(data = which_uad_f %>%
#                dplyr::filter(Curve==data_all_alevs[1]) %>%
#                dplyr::filter(.data[[idvar]] %in% ranef_id_max_min[2]) %>%
#                dplyr::mutate(Curve =
#                                paste0(Curve, ".", idvar, ".", ranef_id_max_min[2]) ) %>%
#                dplyr::mutate(groupby = interaction(.data[[idvar]], Curve)),
#              aes(x = .data[[xvar]], y = .data[[yvar]], group = groupby),
#              linetype = linetypes[1],
#              color = colours[1], linewidth = linewidths[2], alpha = alphas[4],
#              show.legend = T) +
#    geom_line(data = which_uad_f %>%
#                dplyr::filter(Curve==data_all_alevs[2]) %>%
#                dplyr::filter(.data[[idvar]] %in% ranef_id_max_min[2]) %>%
#                dplyr::mutate(Curve =
#                                paste0(Curve, ".", idvar, ".", ranef_id_max_min[2]) ) %>%
#                dplyr::mutate(groupby = interaction(.data[[idvar]], Curve)),
#              aes(x = .data[[xvar]], y = .data[[yvar]], group = groupby),
#              linetype = linetypes[2],
#              color = colours[1], linewidth = linewidths[2], alpha = alphas[4],
#              show.legend = T) +
#    geom_line(data = which_uad_f %>%
#                dplyr::filter(Curve==data_all_alevs[3]) %>%
#                dplyr::mutate(groupby = interaction(Curve)),
#              aes(x = .data[[xvar]], y = .data[[yvar]], group = groupby),
#              linetype = linetypes[1],
#               color = colours[1], linewidth = linewidths[2], alpha = alphas[4],
#              show.legend = T) +
#    facet_wrap(~Model) +
#    scale_x_continuous(breaks = seq(xrange[1], xrange[2], 1), limits = xrange) +
#    theme_grids1 + theme_legends1 +
#    theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0)) +
#    theme(legend.title=element_blank())

## ----tsidssitar-------------------------------------------------------------------------
#  knitr::kable(fh1_ranef_id %>% dplyr::filter(.data[[idvar]] %in% ranef_id_max_min)  ,
#               digits = 2,
#               caption = "Random effects for the tallest and shortest individual for the **sitar** model",
#               row.names=FALSE,
#               align='c',
#               format="html",
#               booktabs=TRUE) %>%
#    kableExtra::kable_styling(latex_options="scale_down",
#                              row_label_position = "c",
#                              position = "float_left",
#                              font_size = 14,
#                              htmltable_class = 'lightable-classic',
#                              html_font = "Cambria") %>%
#    # kableExtra::row_spec(1:4, align = "c", monospace = T) %>%
#    kableExtra::column_spec(1, width = "0.4in") %>%
#    kableExtra::column_spec(2, width = "0.4in") %>%
#    kableExtra::column_spec(3, width = "0.4in") %>%
#    kableExtra::column_spec(4, width = "0.4in") %>%
#    kableExtra::footnote(
#      general = "a: Size (height); b: Timing (age at peak growth velocity, APGV); c: Intensity (peak growth velocity, AGV)",
#      footnote_as_chunk = TRUE,
#      general_title = "",
#      threeparttable = FALSE)

## ----tsidsbsitar------------------------------------------------------------------------
#  knitr::kable(bfh1_ranef_id %>% dplyr::filter(.data[[idvar]] %in% ranef_id_max_min)  ,
#               digits = 2,
#               caption = "Random effects for the tallest and shortest individual for the **bsitar** model",
#               row.names=FALSE,
#               align='c',
#               format="html",
#               booktabs=TRUE) %>%
#    kableExtra::kable_styling(latex_options="scale_down",
#                              row_label_position = "c",
#                              position = "float_left",
#                              font_size = 14,
#                              htmltable_class = 'lightable-classic',
#                              html_font = "Cambria") %>%
#    # kableExtra::row_spec(1:4, align = "c", monospace = T) %>%
#    kableExtra::column_spec(1, width = "0.4in") %>%
#    kableExtra::column_spec(2, width = "0.4in") %>%
#    kableExtra::column_spec(3, width = "0.4in") %>%
#    kableExtra::column_spec(4, width = "0.4in") %>%
#    kableExtra::footnote(
#      general = "a: Size (height); b: Timing (age at peak growth velocity, APGV); c: Intensity (peak growth velocity, AGV)",
#      footnote_as_chunk = TRUE,
#      general_title = "",
#      threeparttable = FALSE)

## ----sitarraneffecid--------------------------------------------------------------------
#  # https://community.rstudio.com/t/sizing-tables-in-pdf-documents-using-knitr-and-kableextra/19285/2
#  knitr::kable(fh1_ranef_id[1:10, ],
#               digits = 2,
#               caption = "Random effects estimates for the first ten individuals        (**sitar** model)",
#               # label = '\\label{tab:tab_ranef_bsitar}Table',
#               row.names=F,
#               # align=c("l",rep("r",3)),
#               align='c',
#               format="html",
#               booktabs=TRUE) %>%
#     kableExtra::kable_styling(latex_options="scale_down",
#                              row_label_position = "c",
#                              position = "float_left",
#                              font_size = 14,
#                              htmltable_class = 'lightable-classic',
#                              html_font = "Cambria") %>%
#    # kableExtra::kable_classic(full_width = F, html_font = "Cambria") %>%
#    # kableExtra::kable_styling() %>%
#    # kableExtra::row_spec(0, align = "c", bold = T ) %>%
#    # kableExtra::row_spec(1:4, align = "c", monospace = T) %>%
#    kableExtra::column_spec(1, width = "0.4in") %>%
#    kableExtra::column_spec(2, width = "0.4in") %>%
#    kableExtra::column_spec(3, width = "0.4in") %>%
#    kableExtra::column_spec(4, width = "0.4in") %>%
#    # kableExtra::add_indent(c(3:9)) %>%
#    # kableExtra::add_header_above(header="Header") %>%
#    kableExtra::footnote(
#      general = "a: Size (height); b: Timing (age at peak growth velocity, APGV); c: Intensity (peak growth velocity, AGV)",
#      footnote_as_chunk = TRUE,
#      general_title = "",
#      threeparttable = FALSE)

## ----bsitarraneffecid-------------------------------------------------------------------
#  knitr::kable(bfh1_ranef_id[1:10, ],
#               digits = 2,
#               caption = "Random effects estimates for the first ten individuals        (**bsitar** model)",
#               row.names=FALSE,
#               align='c',
#               format="html",
#               booktabs=TRUE) %>%
#    kableExtra::kable_styling(latex_options="scale_down",
#                              row_label_position = "c",
#                              position = "float_left",
#                              font_size = 14,
#                              htmltable_class = 'lightable-classic',
#                              html_font = "Cambria") %>%
#    # kableExtra::row_spec(1:4, align = "c", monospace = T) %>%
#    kableExtra::column_spec(1, width = "0.4in") %>%
#    kableExtra::column_spec(2, width = "0.4in") %>%
#    kableExtra::column_spec(3, width = "0.4in") %>%
#    kableExtra::column_spec(4, width = "0.4in") %>%
#    kableExtra::footnote(
#      general = "a: Size (height); b: Timing (age at peak growth velocity, APGV); c: Intensity (peak growth velocity, AGV)",
#      footnote_as_chunk = TRUE,
#      general_title = "",
#      threeparttable = FALSE)

## ----sitarranefcorrplot, echo=TRUE,include=TRUE, fig.width = 6, fig.height=4, fig.asp = 1.0, fig.cap="\\label{fig:plotuadids}Correlation scatterplot of random effects for the **sitar** model"----
#  pairs(nlme::ranef(model_frequ), labels = c('size', 'timing', 'intensity'), pch=20)

## ----bsitarranefcorrplot, echo=TRUE,include=TRUE, fig.width = 6, fig.height=4, fig.asp = 1.0, fig.cap="\\label{fig:plotuadids}Correlation scatterplot of random effects for the **bsitar** model"----
#  pairs(bfh1_ranef, labels = c('size', 'timing', 'intensity'), pch=20)

## ----sitarranefcorr---------------------------------------------------------------------
#  # fh1_ranef_corr[upper.tri(fh1_ranef_corr)] = NA_real_
#  # options(knitr.kable.NA="")
#  knitr::kable(fh1_ranef_corr,
#               digits = 2,
#               caption = "Correlation estimates for the random effects (**sitar** model)",
#               row.names=F,
#               # col.names = TRUE,
#               align='c',
#               format="html",
#               booktabs=TRUE) %>%
#    kableExtra::kable_styling(latex_options="scale_down",
#                              row_label_position = "c",
#                              position = "float_left",
#                              font_size = 14,
#                              htmltable_class = 'lightable-classic',
#                              html_font = "Cambria") %>%
#    # kableExtra::row_spec(1:3, align = "c", monospace = T) %>%
#    kableExtra::column_spec(1, width = "1.2in", monospace = F) %>%
#    kableExtra::column_spec(2, width = "1.2in", monospace = F) %>%
#    kableExtra::column_spec(3, width = "1.2in", monospace = F) %>%
#    kableExtra::footnote(
#      general = "a: Size (height); b: Timing (age at peak growth velocity, APGV); c: Intensity (peak growth velocity, AGV)",
#      footnote_as_chunk = TRUE,
#      general_title = "",
#      threeparttable = FALSE)

## ----bsitarranefcorr--------------------------------------------------------------------
#  # fh1_ranef_corr[upper.tri(fh1_ranef_corr)] = NA_real_
#  # options(knitr.kable.NA="")
#  knitr::kable(bfh1_ranef_corr,
#               digits = 2,
#               caption = "Correlation estimates for the random effects (**bsitar** model)",
#               row.names=F,
#               # col.names = TRUE,
#               align='c',
#               format="html",
#               booktabs=TRUE) %>%
#    kableExtra::kable_styling(latex_options="scale_down",
#                              row_label_position = "c",
#                              position = "float_left",
#                              font_size = 14,
#                              htmltable_class = 'lightable-classic',
#                              html_font = "Cambria") %>%
#    # kableExtra::row_spec(1:3, align = "c", monospace = T) %>%
#    kableExtra::column_spec(1, width = "1.2in", monospace = F) %>%
#    kableExtra::column_spec(2, width = "1.2in", monospace = F) %>%
#    kableExtra::column_spec(3, width = "1.2in", monospace = F) %>%
#    kableExtra::footnote(
#      general = "a: Size (height); b: Timing (age at peak growth velocity, APGV); c: Intensity (peak growth velocity, AGV)",
#      footnote_as_chunk = TRUE,
#      general_title = "",
#      threeparttable = FALSE)

## ----plotd, echo=TRUE,include=TRUE, fig.cap="\\label{fig:plotd}Population average distance curves (increase in size) for the **sitar** and **bsitar** models"----
#  data_sitar_dx2  <- plot(model_frequ, opt = 'd', xlim = sitar::xaxsd(), ylim = sitar::yaxsd(), returndata = T)
#  data_sitar_dfx2 <- data_sitar_dx2 %>% data.frame()
#  data_bsitar_dx2 <- plot_curves(model_bayes, opt = 'd', returndata = T, newdata = data_sitar_dfx2, ipts = NULL, usesavedfuns = T, envir = globalenv()) %>%
#    dplyr::rename(distance = Estimate) %>%
#    dplyr::select(-dplyr::all_of(c('distance', 'Est.Error',  'Q2.5', 'Q97.5')))
#  data_all_ad <- data_all_a %>%
#    dplyr::bind_rows(., data_sitar_dx2 %>% dplyr::relocate(all_of(colnames(data))) %>%
#                       dplyr::mutate(Curve = 'pop.avg_sitar')) %>%
#    dplyr::bind_rows(., data_bsitar_dx2 %>% dplyr::relocate(all_of(colnames(data))) %>%
#                       dplyr::mutate(Curve = 'pop.avg_bsitar')) %>%
#    dplyr::mutate(Curve = as.factor(Curve))
#  
#  
#  data_all_ad <- data_all_ad %>%
#    dplyr::mutate(across(Curve, ~factor(.,
#                                        levels=c("observed",
#                                                 "adjusted_sitar",
#                                                 "adjusted_bsitar",
#                                                 "pop.avg_sitar",
#                                                 "pop.avg_bsitar" ))))
#  
#  data_all_alevs <- levels(data_all_ad$Curve)
#  
#  p <-
#    data_all_ad %>%
#    dplyr::mutate(groupby = interaction(id, Curve)) %>%
#    ggplot(., aes(x = age))
#  # p +
#  #   geom_line(data = data_all_ad %>% dplyr::filter(Curve==data_all_alevs[1]) %>%
#  #               dplyr::mutate(groupby = interaction(id, Curve)),
#  #               aes(x = age, y = height, group = groupby),
#  #             linetype = 1, color = colours[1], , linewidth = linewidths[2],
#  #             alpha = alphas[2], show.legend = F) +
#  #   geom_line(data = data_all_ad %>% dplyr::filter(Curve==data_all_alevs[2]) %>%
#  #               dplyr::mutate(groupby = interaction(id, Curve)),
#  #             aes(x = age, y = height, group = groupby),
#  #             linetype = 1, color = colours[1], , linewidth = linewidths[2],
#  #             alpha = alphas[2], show.legend = F) +
#  #   geom_line(data = data_all_ad %>% dplyr::filter(Curve==data_all_alevs[3]) %>%
#  #               dplyr::mutate(groupby = interaction(id, Curve)),
#  #             aes(x = age, y = height, group = groupby),
#  #             linetype = 1, color = colours[1], , linewidth = linewidths[2],
#  #             alpha = alphas[2], show.legend = F) +
#  #   geom_line(data = data_all_ad %>% dplyr::filter(Curve==data_all_alevs[4]) %>%
#  #               dplyr::mutate(groupby = interaction(id, Curve)),
#  #             aes(x = age, y = height, group = groupby),
#  #             linetype = 1, color = colours[1], , linewidth = linewidths[2],
#  #             alpha = alphas[2], show.legend = F) +
#  #   geom_line(data = data_all_ad %>% dplyr::filter(Curve==data_all_alevs[5]) %>%
#  #               dplyr::mutate(groupby = interaction(id, Curve)),
#  #             aes(x = age, y = height, group = groupby),
#  #             linetype = 1, color = colours[1], , linewidth = linewidths[2],
#  #             alpha = alphas[2], show.legend = F) +
#  #   facet_wrap(~Curve) +
#  #   scale_x_continuous(breaks = seq(xrange[1], xrange[2], 1), limits = xrange) +
#  #   theme_grids1 + theme_legends1
#  
#  p +
#    geom_line(data = data_all_ad %>% dplyr::filter(Curve==data_all_alevs[4]) %>%
#                dplyr::mutate(groupby = interaction(id, Curve)),
#              aes(x = age, y = height, group = groupby, linetype = Curve),
#              color = colours[1], linewidth = linewidths[2],
#              alpha = alphas[2], show.legend = T) +
#    geom_line(data = data_all_ad %>% dplyr::filter(Curve==data_all_alevs[5]) %>%
#                dplyr::mutate(groupby = interaction(id, Curve)),
#              aes(x = age, y = height, group = groupby, linetype = Curve),
#              color = colours[1], linewidth = linewidths[2],
#              alpha = alphas[2], show.legend = T) +
#    scale_x_continuous(breaks = seq(xrange[1], xrange[2], 1), limits = xrange) +
#    theme_grids1 + theme_legends1

## ----plotv, echo=TRUE,include=TRUE, fig.cap="\\label{fig:plotv}Population average velocity curves (change in growth rate) for the **sitar** and **bsitar** models"----
#  data_all_dv %>%
#    dplyr::mutate(groupby = interaction(id, Model) ) %>%
#    ggplot(., aes(x = age)) +
#    geom_line(aes(x = age, y = velocity, group = groupby),
#              color = colours[1], alpha = alphas[2], linewidth = linewidths[2],
#              show.legend = TRUE) +
#    scale_x_continuous(breaks = seq(xrange[1], xrange[2], 1), limits = xrange) +
#    theme_grids1 + theme_legends1

## ----plotD2, echo=TRUE,include=TRUE, fig.cap="\\label{fig:plotD2}Individual specific distance curves (increase in size) for the **sitar** and **bsitar** models"----
#  data_all_DV %>%
#    dplyr::mutate(groupby = interaction(id, Model) ) %>%
#    ggplot(., aes(x = age)) +
#    geom_line(aes(x = age, y = distance, group = groupby,
#                  linetype = Model),
#              color = colours[1], alpha = alphas[2], linewidth = linewidths[2],
#              show.legend = T) +
#    scale_x_continuous(breaks = seq(xrange[1], xrange[2], 1), limits = xrange) +
#    theme_grids1 + theme_legends1

## ----plotV2, echo=TRUE,include=TRUE, fig.cap="\\label{fig:plotV2}Individual specific velocity curves (change in growth rate) for the **sitar** and **bsitar** models"----
#  data_all_DV %>%
#    dplyr::mutate(groupby = interaction(id, Model) ) %>%
#    ggplot(., aes(x = age)) +
#    geom_line(aes(x = age, y = velocity, group = groupby,
#                  linetype = Model),
#              color = colours[1], alpha = alphas[2], linewidth = linewidths[2],
#              show.legend = T) +
#    scale_x_continuous(breaks = seq(xrange[1], xrange[2], 1), limits = xrange) +
#    theme_grids1 + theme_legends1

## ----tabapgv, echo=FALSE, results='asis'------------------------------------------------
#  knitr::kable(parms_apv_all,
#               digits = 2,
#               caption = "Population average timing and intensity estimates for the **sitar** and **bsitar** models",
#               row.names=F,
#               # col.names = TRUE,
#               align='c',
#               format="html",
#               booktabs=TRUE) %>%
#    kableExtra::kable_styling(latex_options="scale_down",
#                              row_label_position = "c",
#                              position = "float_left",
#                              font_size = 14,
#                              htmltable_class = 'lightable-classic',
#                              html_font = "Cambria") %>%
#    # kableExtra::row_spec(1:3, align = "c", monospace = T) %>%
#    kableExtra::column_spec(1, width = "1.2in", monospace = F) %>%
#    kableExtra::column_spec(2, width = "1.2in", monospace = F) %>%
#    kableExtra::column_spec(3, width = "1.2in", monospace = F) %>%
#    kableExtra::footnote(
#      general = "APGV: age at peak growth velocity; PGV: peak growth velocity",
#      footnote_as_chunk = TRUE,
#      general_title = "",
#      threeparttable = FALSE)

## ----sitarraneffecidapv-----------------------------------------------------------------
#  # https://community.rstudio.com/t/sizing-tables-in-pdf-documents-using-knitr-and-kableextra/19285/2
#  knitr::kable(data_sitar_Vapv[1:10, ],
#               digits = 2,
#              caption = "Individual specific timing and intensity estimates for the first ten individuals (**sitar** model)",
#               row.names=F,
#               # align=c("l",rep("r",3)),
#               align='c',
#               format="html",
#               booktabs=TRUE) %>%
#     kableExtra::kable_styling(latex_options="scale_down",
#                              row_label_position = "c",
#                              position = "float_left",
#                              font_size = 14,
#                              htmltable_class = 'lightable-classic',
#                              html_font = "Cambria") %>%
#    kableExtra::column_spec(1, width = "0.4in") %>%
#    kableExtra::column_spec(2, width = "0.4in") %>%
#    kableExtra::column_spec(3, width = "0.4in") %>%
#    kableExtra::footnote(
#      general = "APGV: age at peak growth velocity; PGV: peak growth velocity",
#      footnote_as_chunk = TRUE,
#      general_title = "",
#      threeparttable = FALSE)

## ----bsitarraneffecidapv----------------------------------------------------------------
#  knitr::kable(data_bsitar_Vapv[1:10, ],
#               digits = 2,
#                caption = "Individual specific timing and intensity estimates for the first ten individuals (**bsitar** model)",
#               row.names=FALSE,
#               align='c',
#               format="html",
#               booktabs=TRUE) %>%
#    kableExtra::kable_styling(latex_options="scale_down",
#                              row_label_position = "c",
#                              position = "float_left",
#                              font_size = 14,
#                              htmltable_class = 'lightable-classic',
#                              html_font = "Cambria") %>%
#    kableExtra::column_spec(1, width = "0.4in") %>%
#    kableExtra::column_spec(2, width = "0.4in") %>%
#    kableExtra::column_spec(3, width = "0.4in") %>%
#    kableExtra::footnote(
#      general = "APGV: age at peak growth velocity; PGV: peak growth velocity",
#      footnote_as_chunk = TRUE,
#      general_title = "",
#      threeparttable = FALSE)

