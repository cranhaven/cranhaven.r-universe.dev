#' Build model and make predictions
#'
#' @param df_control data frame with control data (including historical control,
#' if provided)
#' @param df_newstudy data frame, data from new study
#' @param end_day period of time used for the statistical modelling of
#' the control data
#' @param method "Two-stage non-linear model" or "Linear model"
#'
#' @return list: two data frames with prediction results
#' (for new study and for control data)
#'
#' @importFrom lme4 lmer
#' @importFrom dplyr filter select ungroup
#' @importFrom stats as.formula
#' @importFrom purrr map_df
#' @importFrom brms bf
#' @importFrom rlang .data
model_control <- function(df_control,
                          df_newstudy,
                          method,
                          end_day) {
  # cut the data for modelling by using end_day_of_modelling
  dat_mod <- filter(df_control, .data$day <= end_day)

  # create data frames for predictions
  # newdata for the control data
  newdata_control <- unique(select(ungroup(dat_mod), .data$study, .data$day))

  # newdata for the new studies
  newdata_newstudy <-
    unique(select(ungroup(df_newstudy), .data$study, .data$day))

  single <- length(unique(dat_mod$study)) == 1

  if (method == "Linear model") {
    formula <- if (single) {
      as.formula("log_tv ~ day + (1|animal_id)")
    } else {
      as.formula("log_tv ~ day + (1|study/animal_id)")
    }

    mod_control <- lmer(formula, data = dat_mod)
    predict_control <-
      predict_lm(mod_control, newdata_control, single = single)
    predict_newstudy <-
      predict_lm(mod_control, newdata_newstudy, single = single)

  } else { #non linear model

    start <- map_df(unique(dat_mod$study), function(.x) {
      data.frame(f_start(
        df = filter(dat_mod, .data$study == .x),
        r_change = 0.05,
        x = "day",
        y = "log_tv"
      ),
      row.names = .x)
    })

    if (single) {
      formula <- bf(
        log_tv ~ a + b0 * (day - x0) +
          (b1 - b0) * delta * log1p_exp((day - x0) / delta),
        b0 + b1 + delta + x0 ~ 1,
        a ~ 1 + (1 | animal_id),
        nl = TRUE
      )
    } else {
      formula <- bf(
        log_tv ~ a + b0 * (day - x0) +
          (b1 - b0) * delta * log1p_exp((day - x0) / delta),
        b0 + b1 + delta ~ 1,
        x0 ~ 0 + study,
        a ~ 1 + (1 | study / animal_id),
        nl = TRUE
      )
    }

    mod_control <- run_nl_model(start, dat_mod, formula, n_cores = 4)

    if (single) {
      change_time <- change_time_single(mod_control)
      predict_control <-
        predict_nlm_single(mod_control, newdata_control, change_time)
      predict_newstudy <-
        predict_nlm_single(mod_control, newdata_newstudy, change_time)
    } else {
      change_time <- change_time_multi(mod_control)
      predict_control <-
        predict_nlm_multi(mod_control, newdata_control, change_time)
      predict_newstudy <-
        predict_nlm_multi(mod_control, newdata_newstudy, change_time)
    }
  }

  out <- list(predict_control = predict_control,
              predict_newstudy = predict_newstudy)

  return(out)
}

#' Calculate coefficients for a nonlinear model
#' @param df data frame with x as a predictor and y is an outcome
#' @param x predictor string
#' @param y outcome string
#' @param r_change numeric
#'
#' @return list of coefficients
#'
#' @importFrom stats lm coef
f_start <- function(df, x, y, r_change) {

  formula <- paste(y, "~", x)

  y_vec <- df[[y]]
  x_vec <- df[[x]]

  a  <- min(y_vec)
  x0 <- min(x_vec) + r_change * (max(x_vec) - min(x_vec))
  b0 <- 0.01
  b1 <- as.numeric(coef(lm(formula, data = df[df[[x]] > x0, ]))[x])

  return(list(
    a = a,
    x0 = x0,
    b0 = b0,
    b1 = b1
  ))

}

#' Fit nonlinear model - continuous hinge function
#' @param start df with coefficients
#' @param df_mod data of all variables used in the model
#' @param formula an object of class brmsformula
#' @param n_cores number of cores to use
#'
#' @return object of class brmsfit
#'
#' @importFrom brms stanvar prior_string brm bf
run_nl_model <- function(start, df_mod, formula, n_cores) {
  stanvars <- stanvar(mean(start$a), name = "a_prior") +
    stanvar(mean(start$b0), name = "b0_prior") +
    stanvar(mean(start$b1), name = "b1_prior") +
    stanvar(start$x0, name = "x0_prior")

  priors <- prior_string("normal(a_prior, 1)", nlpar = "a") +
    prior_string("normal(b0_prior, 1)", nlpar = "b0") +
    prior_string("normal(b1_prior, 1)", nlpar = "b1") +
    prior_string("normal(x0_prior, 1)", nlpar = "x0", lb = 0) +
    prior_string("normal(1, 1)", nlpar = "delta", lb = 0)

  mod_control <- brm(
    formula,
    data = df_mod,
    prior = priors,
    cores = n_cores,
    stanvars = stanvars
  )

  return(mod_control)
}


#' Get a change time from the population-level effects, single study
#' @param model an object of class brmsfit
#'
#' @return a numeric vector of length one
#'
#' @importFrom brms fixef
change_time_single <- function(model) {
  change_time <- fixef(model, pars = "x0_Intercept")[1]
  return(change_time)
}


#' Get an array with change_time for studies from the population-level effects,
#' multiple studies
#' @param model an object of class brmsfit
#'
#' @return data frame
#'
#' @importFrom brms fixef
#' @importFrom dplyr mutate select %>%
#' @importFrom rlang .data
change_time_multi <- function(model) {

  pop_lvl_effects <- fixef(model)

  pop_lvl_effects_filtered <-
    pop_lvl_effects[grep("x0", rownames(pop_lvl_effects)), ] %>%
    data.frame()
  pop_lvl_effects_filtered$study <- row.names(pop_lvl_effects_filtered)

  change_time_df <-
    pop_lvl_effects_filtered %>%
    mutate(study = gsub("x0_study", "", .data$study),
           change_time = .data$Estimate) %>%
    select(.data$study, .data$change_time)

  return(change_time_df)
}


#' Make predictions based on non-linear model, single study
#' @param model an object of class brmsfit
#' @param newdata data frame in which to look for variables
#' with which to predict
#' @param change_time numeric
#'
#' @return data frame with predictions
#'
#' @importFrom tidybayes add_linpred_draws
#' @importFrom dplyr group_by summarise mutate filter %>% tibble
#' @importFrom stats median quantile
#' @importFrom rlang .data
predict_nlm_single <- function(model, newdata, change_time) {
  predictions <- tibble(day = unique(newdata$day)) %>%
    add_linpred_draws(
      model,
      ndraws = 1e3,
      re_formula = ~ 1 | animal_id,
      allow_new_levels = TRUE
    ) %>%
    group_by(.data$day) %>%
    summarise(
      estimate = median(.data$.linpred),
      upper = quantile(.data$.linpred, 0.975),
      lower = quantile(.data$.linpred, 0.025)
    ) %>%
    mutate(study = unique(newdata$study)) %>%
    filter(.data$day >= change_time)
  return(predictions)
}

#' Make predictions based on non-linear model, multiple studies
#' @param model an object of class brmsfit
#' @param newdata data frame in which to look for variables
#' with which to predict
#' @param change_time data frame
#'
#' @return data frame with predictions
#'
#' @importFrom tidybayes add_linpred_draws
#' @importFrom dplyr group_by summarise mutate left_join filter %>%
#' @importFrom stats median quantile
#' @importFrom rlang .data
predict_nlm_multi <- function(model, newdata, change_time) {
  predictions <- newdata %>%
    add_linpred_draws(
      model,
      ndraws = 1e3,
      re_formula = ~ 1 | study / animal_id,
      allow_new_levels = TRUE
    ) %>%
    group_by(.data$day, .data$study) %>%
    summarise(
      estimate = median(.data$.linpred),
      upper = quantile(.data$.linpred, 0.975),
      lower = quantile(.data$.linpred, 0.025)
    ) %>%
    mutate(study_new = gsub(" ", "", .data$study)) %>%
    left_join(change_time, by = c("study_new" = "study")) %>%
    filter(.data$day >= change_time) %>%
    select(-c("change_time", "study_new"))
  return(predictions)
}

#' Create a character vector with the names of terms from model, for which
#' predictions should be displayed
#' Specific values are specified in square brackets
#' @param days vector with days with which to predict
#' @param studies vector with studies with which to predict
#'
#' @return vector with values for predictions
make_terms <- function(days, studies = NULL) {
  days <- paste0("day [", paste(unique(days), collapse = ", "), "]")
  terms <- if (!is.null(studies)) {
    c(days,
      paste0("study [", paste(unique(studies), collapse = ", "), "]"))
  } else {
    days
  }
  return(terms)
}

#' Make predictions, linear model
#' @param model a model object
#' @param newdata  data frame in which to look for variables with which to
#' predict
#' @param single logical: TRUE if single study experiment
#'
#' @return data frame with predictions
#'
#' @importFrom ggeffects ggpredict
#' @importFrom dplyr mutate %>% rename
#' @importFrom rlang .data
predict_lm <- function(model, newdata, single) {

  terms <- if (single) {
    make_terms(newdata$day)
  } else {
    make_terms(newdata$day, newdata$study)
  }

  preds <- ggpredict(model,
                     terms = terms,
                     type = "re",
                     interval = "prediction") %>%
    as.data.frame()

  if (single) {
    preds <- preds %>% mutate(study = unique(newdata$study))
  } else {
    preds <- preds %>% rename(study = .data$group)
  }

  preds <- preds %>%
    rename(
      day = .data$x,
      estimate = .data$predicted,
      lower = .data$conf.low,
      upper = .data$conf.high
    )

  return(preds)
}

#' Classify individual data points as Responders or Non-responders
#' @param df_newstudy data from new study
#' @param pred_newstudy data frame with predictions
#'
#' @return data frame with "Responder"/"Non-responder"
#' for individual data points
#'
#' @importFrom dplyr mutate left_join %>%
#' @importFrom tidyr drop_na
#' @importFrom rlang .data
classify_data_point <- function(df_newstudy, pred_newstudy) {
  classify_points_df <- df_newstudy %>%
    left_join(pred_newstudy, by = c("study", "day")) %>%
    drop_na(.data$lower) %>%
    mutate(classify_point = ifelse(.data$log_tv < .data$lower,
                                   "Responder", "Non-responder"))
  return(classify_points_df)
}

#' Classify tumour based on response status of individuals
#' @param x character vector with response statuses of one animal
#' @param n consecutive measurements for classification
#'
#' @return "Responder" or "Non-responder"
get_responder <- function(x, n) {
  if (length(x) < n) {
    if (all(x == "Responder")) {
      classify_tumour <- "Responder"
    } else {
      classify_tumour <- "Non-responder"
    }
  } else {
    classify_tumour <- NULL

    for (i in length(x):n) {
      if (all(x[(i - n + 1):i] == "Responder")) {
        classify_tumour <- "Responder"
        break
      }
    }

    if (is.null(classify_tumour)) {
      classify_tumour <- "Non-responder"
    }
  }

  return(classify_tumour)
}


#' Function to return rate of growth (e.g. the slope after a log transformation
#' of the tumour data against time)
#' @param df subset, one animal_id
#' @param log_tv name of the column, tumour volume
#' @param day name of the column, days
#'
#' @return tibble with GR and GR_SE
#'
#' @importFrom dplyr tibble
#' @importFrom stats as.formula glm
calc_gr <- function(df, log_tv = "log_tv", day = "day") {
  gr_formula <- as.formula(paste(log_tv, day, sep = "~"))
  gr_model <- glm(gr_formula, data = df, na.action = "na.exclude")
  model_results <- summary(gr_model)

  return(
    tibble(
      animal_id = unique(df$animal_id),
      treatment = unique(df$treatment),
      study = unique(df$study),
      gr = round(model_results$coefficients[2], 10),
      gr_se = round(model_results$coefficients[4], 10)
    )
  )

}

#' Classify tumour based on the growth rate and the p_value for a two-sided
#' T test
#' Tumour will be considered as "Non-responder", "Modest responder",
#' "Stable responder" or "Regressing responder"
#' @param df data frame
#'
#' @return data frame with a new column `classify_tumour`
#'
#' @importFrom dplyr mutate select %>%
#' @importFrom rlang .data
classify_type_responder <- function(df) {
  df %>%
    mutate(
      classification = ifelse(
        .data$classify_tumour == "Non-responder",
        "Non-responder",
        ifelse(
          is.na(.data$p_value),
          "Not reliable",
          ifelse(
            .data$p_value > 0.05,
            "Stable responder",
            ifelse(.data$gr < 0, "Regressing responder", "Modest responder")
          )
        )
      ),
      classification = factor(
        .data$classification,
        levels = c(
          "Not reliable",
          "Non-responder",
          "Modest responder",
          "Stable responder",
          "Regressing responder"
        )
      )
    ) %>%
    select(
      .data$study,
      .data$animal_id,
      .data$treatment,
      .data$gr,
      .data$gr_se,
      .data$classification
    )
}


#' Function to plot a control growth profile
#' @param df data frame
#' @param model_type string
#' @param col_palette character palette
#'
#' @return ggplot object
#'
#' @importFrom ggplot2 ggplot facet_wrap geom_point geom_line geom_ribbon labs
#' theme_minimal aes scale_color_manual element_text theme
#' @importFrom rlang .data
control_growth_plot <- function(df, model_type, col_palette) {

  title <- if (model_type == "Linear model") {
    "Linear model - control growth profile (fitted line with 95% prediction interval)"
  } else {
    "Non linear model - control growth profile (fitted line with 95% prediction interval)"
  }

  plot_palette <- expand_palette(col_palette, length(unique(df$study)))

  p <- ggplot(df, aes(animal_id = .data$animal_id)) +
    facet_wrap(~ study, scales = "free") +
    geom_point(aes(x = .data$day, y = .data$log_tv, color = .data$study)) +
    geom_line(aes(
      x = .data$day,
      y = .data$log_tv,
      group = .data$animal_id,
      color = .data$study
    )) +
    geom_line(data = drop_na(df), aes(x = .data$day, y = .data$estimate),
              size = 1) +
    geom_ribbon(data = drop_na(df), aes(
      x = .data$day,
      y = .data$estimate,
      ymin = .data$lower,
      ymax = .data$upper
    ),
    alpha = 0.2) +
    scale_color_manual(values =  plot_palette) +
    labs(x = "Day", y = "Log10(tumour volume)",
         title = title) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14))
  return(p)

}
