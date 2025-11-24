
#' GAM confidence and prediction intervals
#'
#' Computes CI from posterior, and PI for Tweedie and scat gams.
#'
#' @param object_fit **\[gam OR bam\]** The fit to use for predictions.
#' @param new_data **\[data.frame\]** The data to predict onto.
#' @param n **\[numeric\]** The number of simulations to run for parameters.
#' @param CI **\[logical\]** Whether to compute the CI.
#' @param PI **\[logical\]** Whether to compute the PI.
#' @param ... further arguments passed to the quantile function.
#'
#' @return
#' A `data.frame` with intervals.
#'
#' @examples
#' gam1 <- gam(cyl ~ mpg, data=mtcars, family = tw)
#' predict_intervals(gam1)
#'
#' @rdname intervals
#' @export
predict_intervals <- function(object_fit, new_data, n = 1000,
                              CI = TRUE, PI = TRUE, ...){

  stopifnot(n > 1)

  # Compute simulations
  sims <- produce_sims(object_fit, new_data, n)
  CI_df <- NULL

  if (!CI & !PI) stop("at least one of CI or PI must be TRUE")

  if (CI) {
    # Confidence interval
    CI_prod <- confidence_interval(sims, ...)
    CI_df <- dplyr::bind_cols(CI_df, CI_prod)
  }

  if (PI) {
    # Prediction interval
    PI_prod <- prediction_interval(object_fit, sims, ...)
    CI_df <- dplyr::bind_cols(CI_df, PI_prod)
  }

  return(CI_df)

}

NULL

predict_biomass_intervals <- function(object_fit, patches, smoothed_data, time_col,
                                      new_data, biomass, patch_area_col, next_ts,
                                      bounds_col){

  if (next_ts){

    CI_df_prod <- predict_intervals(object_fit, new_data)

    # Get ts data
    all_ts <- as.numeric(as.character(unique(smoothed_data[[time_col]])))
    max_ts <- max(all_ts)
    density_last_year <- smoothed_data %>%
      dplyr::filter(.data[[time_col]] %in% max_ts) %>%
      dplyr::pull(.data[[biomass]])

    # Bind all
    CI_df <- patches %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(

        # CI
        biomass_density_lower =
          density_last_year * CI_df_prod$CI_lower,
        biomass_density_upper =
          density_last_year * CI_df_prod$CI_upper,

        # PI
        biomass_density_lower_P =
          density_last_year * CI_df_prod$PI_lower,
        biomass_density_upper_P =
          density_last_year * CI_df_prod$PI_upper) %>%

      dplyr::mutate(

        # CI
        biomass_CI_lower = .data$biomass_density_lower *
          .data[[patch_area_col]],
        biomass_CI_upper = .data$biomass_density_upper *
          .data[[patch_area_col]],

        # PI
        biomass_PI_lower = .data$biomass_density_lower_P *
          .data[[patch_area_col]],
        biomass_PI_upper = .data$biomass_density_upper_P *
          .data[[patch_area_col]]) %>%

      dplyr::select("biomass_CI_lower", "biomass_CI_upper",
                    "biomass_PI_lower", "biomass_PI_upper")

  } else {

    CI_df_prod <- predict_intervals(object_fit, new_data)

    # Get catch density
    catch_density <- smoothed_data$catch_density

    # Bind all
    CI_df <- smoothed_data %>%
      sf::st_drop_geometry() %>%
      dplyr::select(dplyr::all_of(biomass), "patch_id", dplyr::all_of(time_col),
                    dplyr::all_of(bounds_col), dplyr::all_of(patch_area_col)) %>%

      dplyr::bind_cols(CI_df_prod) %>%

      dplyr::arrange(.data$patch_id, .data[[time_col]]) %>%
      dplyr::group_by(.data$patch_id, .data[[bounds_col]]) %>%
      dplyr::mutate(

        # CI
        biomass_density_with_catch_lower =
          dplyr::lag(.data[[biomass]]) * .data$CI_lower,
        biomass_density_with_catch_upper =
          dplyr::lag(.data[[biomass]]) * .data$CI_upper,

        # PI
        biomass_density_with_catch_lower_P =
          dplyr::lag(.data[[biomass]]) * .data$PI_lower,
        biomass_density_with_catch_upper_P =
          dplyr::lag(.data[[biomass]]) * .data$PI_upper) %>%

      dplyr::ungroup() %>%

      # CI
      dplyr::mutate(biomass_density_lower = .data$biomass_density_with_catch_lower -
          catch_density,
        biomass_density_upper = .data$biomass_density_with_catch_upper -
          catch_density,

        # PI
        biomass_density_lower_P = .data$biomass_density_with_catch_lower_P -
          catch_density,
        biomass_density_upper_P = .data$biomass_density_with_catch_upper_P -
          catch_density) %>%

      dplyr::mutate(
        # biomass_with_catch_lower = .data$biomass_density_with_catch_lower *
        #   .data[[patch_area_col]],
        # biomass_with_catch_upper = .data$biomass_density_with_catch_upper *
        #   .data[[patch_area_col]],

        # CI
        biomass_CI_lower = .data$biomass_density_lower *
          .data[[patch_area_col]],
        biomass_CI_upper = .data$biomass_density_upper *
          .data[[patch_area_col]],

        # PI
        biomass_PI_lower = .data$biomass_density_lower_P *
          .data[[patch_area_col]],
        biomass_PI_upper = .data$biomass_density_upper_P *
          .data[[patch_area_col]]) %>%

      dplyr::ungroup() %>%

      dplyr::select("biomass_CI_lower", "biomass_CI_upper",
                    "biomass_PI_lower", "biomass_PI_upper")

  }

  return(CI_df)

}

# Functions for making intervals from bam objects -------------------------

# Expect a gam fit object, returns a data.frame

produce_sims <- function(fit, new_data, n){

  checkmate::assert_class(fit, "gam")

  coefs <- stats::coef(fit)
  lp <- predict(fit, newdata = new_data, type = "lpmatrix")
  vcv <- stats::vcov(fit)
  coefs_sim <- t(rmvn(n = n, coefs, vcv))

  sims <- lp %*% coefs_sim

  return(sims)

}

find_quantiles <- function(mat, prob = c(0.025, 0.975), name, MARGIN = 1,
                           na.rm = TRUE, compute_exp = TRUE) {

  quants <- t(apply(mat, MARGIN = MARGIN, FUN = stats::quantile,
                    probs = prob, na.rm = na.rm)) %>%
    as.data.frame()

  colnames(quants) <-  paste0(name, c("_log_lower", "_log_upper"))

  if (compute_exp) {

    quants_exp <- exp(quants)
    colnames(quants_exp) <-  paste0(name, c("_lower", "_upper"))
    quants <- dplyr::bind_cols(quants, quants_exp)

  }

  return(quants)
}

confidence_interval <- function(sims, ...){

  CI <- find_quantiles(sims, name = "CI", ...)

  return(CI)
}

prediction_interval <- function(fit, sims, ...){

  checkmate::assert_class(fit, "gam")

  model_family <- fit$family
  invlink <- model_family$linkinv
  inv_sims <- invlink(sims)

  fam_char <- as.character(model_family)[1]

  if (grepl("Scaled", fam_char)){

    min_df <- environment(model_family[["rd"]])[[".min.df"]]
    theta <- environment(model_family[["rd"]])[[".Theta"]]

    nu <- min_df + exp(theta[1])
    sig <- exp(theta[2])

    pred_sims <- sig*(matrix(stats::rt(prod(dim(inv_sims)), df = nu),
                             nrow = nrow(inv_sims),
                             ncol = ncol(inv_sims))) + inv_sims

  } else if (grepl("Tweedie", fam_char)){

    p_char <- gsub(gsub(fam_char, pattern = "Tweedie(p=",
                        replacement = "", fixed = T), pattern = ")",
                   replacement = "", fixed = TRUE)
    p <- as.numeric(p_char)

    pred_sims <- matrix(rTweedie(mu = c(inv_sims), p = p),
                        nrow = nrow(inv_sims),
                        ncol = ncol(inv_sims))

  } else {

    stop("PI is only supported for sspm_fit if the family is scat or Tweedie")

  }

  PI <- find_quantiles(pred_sims, name = "PI", ...)

  return(PI)
}
