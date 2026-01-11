#' Generate table representing number of animals in classification groups
#'
#' @param data final classification data
#'
#' @return data frame
#'
#' @importFrom dplyr group_by mutate ungroup summarise mutate_if %>%
#' @importFrom rlang .data
animal_info_classification <- function(data) {
  df <- data %>%
    group_by(.data$study, .data$treatment) %>%
    mutate(total_animal = length(.data$animal_id)) %>%
    ungroup() %>%
    group_by(.data$study,
             .data$treatment,
             .data$classification,
             .data$total_animal) %>%
    summarise(animal_number = length(.data$animal_id)) %>%
    mutate(percentage = .data$animal_number / .data$total_animal * 100) %>%
    mutate_if(is.numeric, ~ round(., 4))
  return(df)
}

#' Plot representing number of animals in classification groups
#' @param data final classification data
#' @param col_palette character palette
#'
#' @return ggplot object
#'
#' @importFrom ggplot2 ggplot aes facet_wrap geom_bar labs scale_fill_manual
#' theme_minimal theme element_text
#' @importFrom rlang .data
plot_animal_info <- function(data, col_palette) {
  p <- ggplot(data, aes(.data$treatment)) +
    facet_wrap(~ study, ncol = 2, scales = "free") +
    geom_bar(aes(fill = .data$classification),
             position = "dodge2",
             width = 0.3) +
    labs(x = "Group", y = "Animal number",
         title = "Animal numbers for tumour classification") +
    scale_fill_manual(values = col_palette, limits = force) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 75, vjust = 0.7),
          plot.title = element_text(size = 20))
  return(p)
}


#' Fit model (Bayesian ordered logistic regression)
#' @param df data frame with classification results. Tumour classification is
#' converted into ordinal data
#' @param formula string
#' @param n_cores number of cores to use
#'
#' @return object of class brmsfit
#'
#' @importFrom brms brm cumulative
ordered_regression <- function(df, formula, n_cores) {
  model_classification <- brm(
    formula = formula,
    data = df,
    family = cumulative("logit"),
    chains = 4,
    cores = n_cores,
    iter = 5000
  )
  return(model_classification)
}

#' Make predictions
#'
#' @param model object of class brmsfit
#' @param df data frame with classification results
#'
#' @return data frame
#'
#' @importFrom stats predict
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% mutate group_by summarise
#' @importFrom rlang .data
predict_regr_model <- function(model, df) {
  as.data.frame(t(predict(model,
                          summary = FALSE) - 1)) %>%
    mutate(treatment = df$treatment,
           study = df$study) %>%
    pivot_longer(cols = - c("treatment", "study")) %>%
    group_by(.data$treatment, .data$study, .data$name) %>%
    summarise(mean = mean(.data$value))
}

#' Credible interval (or say “Bayesian confidence interval”) of the mean
#' difference between two groups (treatment and reference) is used to assess the
#' efficacy. If 0 falls outside the interval, the drug was considered
#' significantly effective
#' @param data prediction results
#' @param reference name of the reference treatment
#'
#' @return dataframe with information about drug efficacy
#' @importFrom dplyr %>% group_by mutate filter summarise
#' @importFrom rlang .data
assess_efficacy <- function(data, reference = "Control") {
  df <- data %>%
    group_by(.data$study, .data$name) %>%
    mutate(control_mean = mean[.data$treatment == reference]) %>%
    filter(.data$treatment != reference) %>%
    mutate(
      diff = .data$mean - .data$control_mean,
      # Derive the predicted difference between
      # treated group and the control group
      contrast = paste(.data$treatment, "-", reference)
    ) %>%
    group_by(.data$study, .data$contrast) %>%
    summarise(
      mean_difference = median(.data$diff),
      # Calculate the median, lower and
      # upper 95% quantile of the differences,
      # credible interval = [Lower, Upper]
      lower = quantile(.data$diff, 0.025),
      upper = quantile(.data$diff, 0.975)
    ) %>%
    mutate(efficacy = ifelse(.data$lower <= 0 & .data$upper >= 0,
                             " ", "**"))
  return(df)
}

#' Make predictions for subcategories
#' @param data data frame with classification results
#' @param model object of class brmsfit
#'
#' @importFrom tidybayes add_epred_draws
#' @importFrom modelr data_grid
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
#'
#' @return data frame
classify_subcategories <- function(data, model) {
  data %>%
    data_grid(.data$treatment, .data$study) %>%
    add_epred_draws(model, dpar = TRUE) %>%
    mutate(
      classification = levels(data$order)[.data$.category],
      classification = factor(.data$classification, levels = levels(data$order))
    )
}

#' Calculate probability of categories
#' @param data data frame with predictions
#'
#' @return data frame
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
calc_probability <- function(data) {
  df <- data %>%
    group_by(.data$treatment, .data$classification, .data$.draw) %>%
    summarise(mean = mean(.data$.epred)) %>%
    group_by(.data$treatment, .data$classification) %>%
    summarise(prob = median(.data$mean)) %>%
    pivot_wider(names_from = .data$classification,
                values_from = .data$prob) %>%
    mutate_if(is.numeric, ~ round(., 4))
  return(df)
}

#' Plot estimated proportions
#' @param data table of the category prediction
#' @param col_palette character palette
#'
#' @importFrom ggplot2 ggplot aes position_dodge scale_size_continuous labs
#' theme_bw theme scale_color_manual element_text
#' @importFrom tidybayes stat_pointinterval
#' @importFrom rlang .data
plot_proportions <- function(data, col_palette) {
  p <-
    ggplot(data,
           aes(
             x = .data$treatment,
             y = .data$.epred,
             color = .data$classification
           )) +
    stat_pointinterval(
      position = position_dodge(width = .5),
      .width = 0.95,
      point_size = 2,
      interval_size = 1,
      slab_linetype = 2
    ) +
    scale_size_continuous(guide = "none") +
    labs(
      x = "Group",
      y = "Proportion",
      color = "Classification",
      title = "Estimated proportion of category with 95% credible interval"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    scale_color_manual(values = col_palette, limits = force)
  return(p)
}
