#' @title pretty_coefficients
#'
#' @description Creates a pretty kable of model coefficients including coefficient base levels, type III P.values, and variable importance.
#'
#' @param model_object Model object to create coefficient table for. Must be of type: \code{\link[stats]{glm}}, \code{\link[stats]{lm}}.
#' @param relativity_transform String of the function to be applied to the model estimate to calculate the relativity, for example: 'exp(estimate)-1'. Default is for relativity to be excluded from output.
#' @param relativity_label String of label to give to relativity column if you want to change the title to your use case.
#' @param type_iii Type III statistical test to perform. Default is none. Options are 'Wald' or 'LR'. Warning 'LR' can be computationally expensive. Test performed via \code{\link[car]{Anova}}
#' @param conf.int Set to TRUE to include confidence intervals in summary table. Warning, can be computationally expensive.
#' @param vimethod Variable importance method to pass to method of \code{\link[vip]{vi}}. Defaults to "model". Currently supports "permute" and "firm", pass any additional arguments to \code{\link[vip]{vi}} in ...
#' @param spline_seperator Separator to look for to identity a spline. If this input is not null, it is assumed any features with this separator are spline columns. For example an age spline from 0 to 25 you could use: AGE_0_25 and "_".
#' @param significance_level Significance level to P-values by in kable. Defaults to 0.05.
#' @param return_data Set to TRUE to return \code{\link[base]{data.frame}} instead of creating \code{\link[knitr]{kable}}.
#' @param ... Any additional parameters to be past to  \code{\link[vip]{vi}}
#'
#' @return \code{\link[knitr]{kable}} if return_data = FALSE. \code{\link[base]{data.frame}} if return_data = TRUE.
#'
#' @examples
#'
#' library(dplyr)
#' library(prettyglm)
#' data('titanic')
#' columns_to_factor <- c('Pclass',
#'                        'Sex',
#'                        'Cabin',
#'                        'Embarked',
#'                        'Cabintype',
#'                        'Survived')
#' meanage <- base::mean(titanic$Age, na.rm=TRUE)
#'
#' titanic  <- titanic  %>%
#'  dplyr::mutate_at(columns_to_factor, list(~factor(.))) %>%
#'  dplyr::mutate(Age =base::ifelse(is.na(Age)==TRUE,meanage,Age)) %>%
#'  dplyr::mutate(Age_0_25 = prettyglm::splineit(Age,0,25),
#'                Age_25_50 = prettyglm::splineit(Age,25,50),
#'                Age_50_120 = prettyglm::splineit(Age,50,120)) %>%
#'  dplyr::mutate(Fare_0_250 = prettyglm::splineit(Fare,0,250),
#'                Fare_250_600 = prettyglm::splineit(Fare,250,600))
#'
#' # A simple example
#' survival_model <- stats::glm(Survived ~
#'                               Pclass +
#'                               Sex +
#'                               Age +
#'                               Fare +
#'                               Embarked +
#'                               SibSp +
#'                               Parch +
#'                               Cabintype,
#'                              data = titanic,
#'                              family = binomial(link = 'logit'))
#' pretty_coefficients(survival_model)
#'
#' # A more complicated example with a spline and different importance method
#' survival_model3 <- stats::glm(Survived ~
#'                                         Pclass +
#'                                         Age_0_25 +
#'                                         Age_25_50 +
#'                                         Age_50_120 +
#'                                         Sex:Fare_0_250 +
#'                                         Sex:Fare_250_600 +
#'                                         Embarked +
#'                                         SibSp +
#'                                         Parch +
#'                                         Cabintype,
#'                               data = titanic,
#'                               family = binomial(link = 'logit'))
#' pretty_coefficients(survival_model3,
#'                     relativity_transform = 'exp(estimate)-1',
#'                     spline_seperator = '_',
#'                     vimethod = 'permute',
#'                     target = 'Survived',
#'                     metric = "roc_auc",
#'                     event_level = 'second',
#'                     pred_wrapper = predict.glm,
#'                     smaller_is_better = FALSE,
#'                     train = survival_model3$data, # need to supply training data for vip importance
#'                     reference_class = 0)
#'
#'
#' @export
#' @importFrom tibble "tibble"
#' @importFrom broom "tidy"
#' @importFrom tidyselect "all_of"
#' @importFrom stringr "str_replace"
#' @importFrom knitr "kable"
#' @importFrom kableExtra "kable_styling"
#' @importFrom kableExtra "row_spec"
#' @importFrom kableExtra "cell_spec"
#' @importFrom kableExtra "collapse_rows"
#' @importFrom kableExtra "footnote"
#' @importFrom kableExtra "spec_plot"
#' @importFrom car "Anova"
#' @import dplyr
#'

pretty_coefficients <- function(model_object, relativity_transform = NULL, relativity_label = 'relativity', type_iii = NULL, conf.int = FALSE, vimethod = 'model', spline_seperator = NULL, significance_level = 0.05, return_data = FALSE, ...){
  # add other options in the if statemnt for splines

  if (any(class(model_object) == 'workflow') == TRUE){
    # if model object is a workflow, pull the model fit
    model_object <- model_object$fit$fit$fit
  } else if (any(class(model_object) == 'model_fit') == TRUE){
    # if model object is a parsnip model, pull the model fit
    model_object <- model_object$fit
  } else{
    # stats lm or glm fit
    model_object$call$formula <- model_object$formula
  }

  # tidy coefficients
  model_tidy_df <- broom::tidy(model_object, conf.int=conf.int)
  tidy_coef <- prettyglm::clean_coefficients(d=model_tidy_df, m=model_object, vimethod = vimethod, spline_seperator = spline_seperator, ...)

  # replace NAs with 0
  tidy_coef <- tidy_coef %>%  dplyr::mutate(estimate = base::ifelse(is.na(estimate), 0, estimate),
                                            std.error = base::ifelse(is.na(std.error), 0, std.error))
  # add relativity
  if (base::is.null(relativity_transform) != TRUE){
    base::eval(base::parse(text = base::paste('relativity <- function(estimate) { return(' , relativity_transform , ')}', sep='')))
    tidy_coef <- tidy_coef %>%
      dplyr::mutate(relativity = (relativity(estimate))) #relativity_label
  }

  # confidence interval and relativity formatting
  if (conf.int == TRUE){
    tidy_coef <- tidy_coef %>%
      dplyr::mutate(conf.low = base::ifelse(is.na(conf.low), 0, conf.low),
                    conf.high = base::ifelse(is.na(conf.high), 0, conf.high))
    if (base::is.null(relativity_transform) != TRUE){
      tidy_coef <- tidy_coef %>%
        dplyr::select(c(variable, level, Importance, estimate, std.error, conf.low, conf.high, relativity, p.value, term, effect)) %>%
        dplyr::rename(Variable = variable,
                      Level = level,
                      Estimate = estimate,
                      Std.Error = std.error,
                      Conf.low = conf.low,
                      Conf.high = conf.high,
                      Relativity = relativity,
                      P.Value= p.value,
                      Term = term,
                      Effect = effect)
    } else{
      tidy_coef <- tidy_coef %>%
        dplyr::select(c(variable, level, Importance, estimate, std.error, conf.low, conf.high, p.value, term, effect)) %>%
        dplyr::rename(Variable = variable,
                      Level = level,
                      Estimate = estimate,
                      Std.Error = std.error,
                      Conf.low = conf.low,
                      Conf.high = conf.high,
                      P.Value= p.value,
                      Term = term,
                      Effect = effect)
      }
  } else {
    if (base::is.null(relativity_transform) != TRUE){
      tidy_coef <- tidy_coef %>% dplyr::select(c(variable, level, Importance, estimate, std.error, relativity, p.value, term, effect)) %>%
        dplyr::rename(Variable = variable,
                      Level = level,
                      Estimate = estimate,
                      Std.error = std.error,
                      Relativity = relativity,
                      P.Value= p.value,
                      Term = term,
                      Effect = effect)
    } else {
      tidy_coef <- tidy_coef %>% dplyr::select(c(variable, level, Importance, estimate, std.error, p.value, term, effect)) %>%
        dplyr::rename(Variable = variable,
                      Level = level,
                      Estimate = estimate,
                      Std.error = std.error,
                      P.Value= p.value,
                      Term = term,
                      Effect = effect)
    }
  }

  # add type III test
  if (is.null(type_iii) == FALSE){
    if(!(type_iii %in% c('Wald', 'LR'))) {stop('type_iii must be either: "Wald" or "LR"')}
      term_p_values <- broom::tidy(car::Anova(model_object, type = 'III', test.statistic=type_iii)) %>%
        dplyr::select(c('term', 'p.value')) %>%
        dplyr::rename('Type.III.P.Value' = 'p.value')
      tidy_coef <- dplyr::inner_join(tidy_coef, term_p_values, by = c('Variable' = 'term'))
  }

  # rename relativity column if new label given
  if (relativity_label != 'relativity'){
    names(tidy_coef)[names(tidy_coef) == "Relativity"] <- relativity_label
  }

  # some cleaning for spline columns
  tidy_coef <- tidy_coef %>%
    dplyr::mutate(Variable = base::ifelse(Effect %in% c('ctsspline'),
                                          yes = stringr::str_extract(Variable, "[^_]+"),
                                          no = as.character(Variable)))

  tidy_coef <- tidy_coef %>%
    dplyr::mutate(Variable = base::ifelse(Effect %in% c('factorandctsinteractionspline'),
                                          yes = base::paste0(stringr::word(Variable,1,sep = ":"),':',stringr::str_extract(stringr::word(Variable,2,sep = ":"), "[^_]+")),
                                          no = as.character(Variable))) %>%
    dplyr::mutate(Level = base::ifelse(Effect %in% c('factorandctsinteractionspline'),
                                          yes = base::paste0(Level, ':',stringr::word(Term,2,sep = ":")),
                                          no = as.character(Level)))


  # tidy_coef <- tidy_coef %>%
  #   dplyr::mutate(Variable = base::ifelse(Effect %in% c('factorandctsinteractionspline'),
  #                                         yes = base::paste0(stringr::word(tidy_coef$Term,1,sep = ":"),':',stringr::str_extract(stringr::word(tidy_coef$Variable,2,sep = ":"), "[^_]+")),
  #                                         no = Variable))
  # tidy_coef_fc_spline <- tidy_coef %>%
  #   dplyr::filter(Effect == 'factorandctsinteractionspline') %>%
  #   dplyr::mutate(Level = as.factor(base::ifelse(Effect %in% c('factorandctsinteractionspline'),
  #                                                yes = stringr::word(Term,2,sep = ":"),
  #                                                no = as.character(Level)))) %>%
  #   dplyr::arrange(Variable, Level)
  #
  # tidy_coef <- tidy_coef %>%
  #   dplyr::filter(Effect != 'factorandctsinteractionspline') %>%
  #   dplyr::bind_rows(tidy_coef_fc_spline)

  # return desired output
  if (return_data == FALSE){

    # Extract goodness of fit metrics
    if (any(class(model_object) == 'model_fit') == TRUE){
      aic_print <- base::ifelse(base::is.null(model_object$fit$aic), NA, model_object$fit$aic)
      deviance_print <- base::ifelse(base::is.null(model_object$fit$deviance), NA, model_object$fit$deviance)
      null_deviance_print <- base::ifelse(base::is.null(model_object$fit$null.deviance ), NA, model_object$fit$null.deviance)
    } else{
      aic_print <- base::ifelse(base::is.null(model_object$aic), NA, model_object$aic)
      deviance_print <- base::ifelse(base::is.null(model_object$deviance ), NA, model_object$deviance)
      null_deviance_print <- base::ifelse(base::is.null(model_object$null.deviance), NA, model_object$null.deviance)
    }

    # Create a nice kable output of coefficients
    kable_df <- tidy_coef %>%
      dplyr::select(-c(Term, Effect))
    kable_df$P.Value = kableExtra::cell_spec(base::round(kable_df$P.Value,5), background  = ifelse(is.na(kable_df$P.Value) |  kable_df$P.Value < significance_level, "#black", "#F08080"))
    use_it <- base::lapply(base::as.list(tidy_coef$Importance), function(x) base::as.vector(base::cbind(0,x))) #importance in list for bar plot
    if(is.null(type_iii)){
      kable_table <- kable_df %>%
        mutate(Importance = "") %>%
        knitr::kable(. ,
                     escape = FALSE,
                     booktabs = TRUE,
                     align = c("l","l","l","r", "r", "r", "r", "r"))%>%
        kableExtra::kable_styling() %>%
        kableExtra::column_spec(3, image = kableExtra::spec_plot(x = use_it,
                                                                 y = rep(list(c(1,1)),base::nrow(tidy_coef)),
                                                                 same_lim = TRUE,
                                                                 lwd = 10,
                                                                 type='l',
                                                                 pch=0,
                                                                 cex = 0,
                                                                 ann = T,
                                                                 xlim = c(0,base::max(tidy_coef$Importance)),
                                                                 ylim = c(1,1))) %>%
        kableExtra::collapse_rows(columns = 1) %>%
        kableExtra::footnote(general = base::paste(' AIC:',
                                                   base::round(aic_print,1),
                                                   ', Devience :',
                                                   base::round(deviance_print,1),
                                                   ', Null Devience: ',
                                                   base::round(null_deviance_print,1)),
                             general_title = 'Goodness-of-Fit:',
                             footnote_as_chunk = TRUE,
                             title_format = c("italic", "underline"))
    } else{
      kable_df$Type.III.P.Value = kableExtra::cell_spec(base::round(kable_df$Type.III.P.Value,5), background  = ifelse(is.na(kable_df$Type.III.P.Value) |  kable_df$Type.III.P.Value < significance_level, "#black", "#F08080"))
      kable_table <- kable_df %>%
        mutate(Importance = "") %>%
        knitr::kable(. ,
                     escape = FALSE,
                     booktabs = TRUE,
                     align = c("l","l","l","r", "r", "r", "r", "r"))%>%
        kableExtra::kable_styling() %>%
        kableExtra::column_spec(3, image = kableExtra::spec_plot(x = use_it,
                                                                 y = rep(list(c(1,1)),base::nrow(tidy_coef)),
                                                                 same_lim = TRUE,
                                                                 lwd = 10,
                                                                 type='l',
                                                                 pch=0,
                                                                 cex = 0,
                                                                 ann = TRUE,
                                                                 xlim = c(0,base::max(tidy_coef$Importance)),
                                                                 ylim = c(1,1))) %>%
        kableExtra::collapse_rows(columns = c(1, base::ncol(kable_df)), target = 1) %>%
        kableExtra::footnote(general = base::paste(' AIC:',
                                                   base::round(aic_print,1),
                                                   ', Devience :',
                                                   base::round(deviance_print,1),
                                                   ', Null Devience: ',
                                                   base::round(null_deviance_print,1)),
                             general_title = 'Goodness-of-Fit:',
                             footnote_as_chunk = TRUE,
                             title_format = c("italic", "underline"))
    }
    return(kable_table)
  } else{
    # If kable false, return data frame
    return(tidy_coef)
  }
}
