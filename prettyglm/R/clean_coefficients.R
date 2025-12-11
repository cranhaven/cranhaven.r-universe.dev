#' @title clean_coefficients
#'
#' @description Processing to split out base levels and add variable importance to each term. Inspired by `tidycat::tidy_categorical()`, modified for use in prettyglm..
#'
#' @param d Data frame \code{\link[tibble]{tibble}} output from \code{\link[broom]{tidy.lm}}; with one row for each term in the regression, including column `term`
#' @param m Model object \code{\link[stats]{glm}}
#' @param vimethod Variable importance method. Still in development
#' @param spline_seperator Sting of the spline separator. For example AGE_0_25 would be "_"
#' @param ...  Any additional parameters to be past to  \code{\link[vip]{vi}}
#'
#' @return Expanded \code{\link[tibble]{tibble}} from the version passed to `d` including additional columns:
#' \item{variable}{The name of the variable that the regression term belongs to.}
#' \item{level}{The level of the categorical variable that the regression term belongs to. Will be an the term name for numeric variables.}
#' @seealso \code{\link[broom]{tidy.lm}}
#'
#' @author Jared Fowler, Guy J. Abel
#'
#' @export
#' @importFrom tibble "enframe"
#' @importFrom vip "vi"
#' @importFrom stringr "str_remove"
#' @importFrom forcats "fct_inorder"
#' @importFrom tidycat "factor_regex"
#' @importFrom tidyselect "all_of"
#' @importFrom methods "is"
#' @import dplyr

clean_coefficients <- function(d = NULL, m  = NULL, vimethod = 'model', spline_seperator = NULL, ...){

  # Extract model object if parsnip object
  if (any(class(m) == 'model_fit') == TRUE) m <- m$fit else m <- m

  # Extract model object if workflow object
  if (any(class(m) == 'workflow') == TRUE) m <- m$fit$fit$fit else m <- m

  #Split terms and get base levels
  x <- m %>%
    stats::dummy.coef() %>%
    base::unlist() %>%
    tibble::enframe(value = "est") %>%
    dplyr::mutate(
      variable = stringr::str_extract(string = name, pattern = tidycat::factor_regex(m)),
      level = stringr::str_remove(string = name, pattern = tidycat::factor_regex(m)),
      level = stringr::str_remove(string = level, pattern = "^[.]"),
      level = forcats::fct_inorder(level),
      effect = base::ifelse(test = stringr::str_detect(string = variable, pattern = ":"),
                      yes = base::ifelse(stringr::str_detect(string = level, pattern = ":", negate = TRUE),
                                         yes = "factorandctsinteraction",
                                         no = "otherinteraction"),
                      no = "main"))

  # count rows to to help categorise effects
  number_of_rows <- x %>%  dplyr::group_by(variable) %>% dplyr::summarise(number_of_rows=dplyr::n(), .groups = "drop")
  x <- x %>%
    dplyr::left_join(number_of_rows, by = "variable") %>%
    dplyr::mutate(effect = base::ifelse(effect == 'otherinteraction',
                                        yes = base::ifelse(number_of_rows == 1,
                                                           yes = 'ctsctsinteraction',
                                                           no = 'factorfactorinteraction'),
                                        no = effect)) %>%
    dplyr::mutate(effect = base::ifelse(effect == 'main',
                                        yes = base::ifelse(variable == level,
                                                           yes = 'ctsmain',
                                                           no = 'factormain'),
                                        no = effect))

  # Re-create term field for join
  term_record <- vector(mode = "list", length = length(x$variable))
  for(i in 1:length(x$variable)){
    if (x$variable[i] == '(Intercept)'){
      term_record[[i]] <- '(Intercept)'
    } else if (base::grepl(x = x$variable[i], pattern=':', fixed = TRUE)){
      # Re-create term name for any number of interacted variables
      termname <- ''
      if (x$effect[i] == 'factorandctsinteraction'){
        # factors and cts need to be handled different
        # not built for more than one interaction
        lev <- base::unlist(base::strsplit(as.character(x$level[i]), ':'))
        var1 <- base::unlist(base::strsplit(x$variable[i], ':'))[1]
        var2 <- base::unlist(base::strsplit(x$variable[i], ':'))[2]
        # At the moment we need to check the class of the training data, hopefully we can change this at some point
        if (methods::is(dplyr::pull(dplyr::select(m$data, tidyselect::all_of(var1))), 'factor') == TRUE){
          termname <- base::paste0(var1,lev, ':', var2)
        }
        if (methods::is(dplyr::pull(dplyr::select(m$data, tidyselect::all_of(var2))), 'factor') == TRUE){
          termname <- base::paste0(var2,lev, ':', var1)
        }
      } else{
        for (k in 1:base::length(base::unlist(base::strsplit(as.character(x$level[i]), ':')))){
          lev <- base::unlist(base::strsplit(as.character(x$level[i]), ':'))[k]
          var <- base::unlist(base::strsplit(x$variable[i], ':'))[k]
          if (base::nchar(termname)==0){
            termname <- base::paste0(termname, var, lev)
          } else{
            termname <- base::paste0(termname, ':', var, lev)
          }
        }
      }
      # Assign term name to correct value in a list
      term_record[[i]] <- termname
    } else if (x$variable[i] %in% names(which(attr(m$terms,"dataClasses") == 'numeric'))){
      # Numeric variables have the same term name as their variable
      term_record[[i]] <- x$variable[i]
    } else{
      #If not an interaction of an numeric it must be a factor with multiple levels
      term_record[[i]] <- base::paste0(x$variable[i], x$level[i])
    }
  }
  x$term <- base::unlist(term_record)
  x <- x %>%
    dplyr::mutate(term = base::ifelse(effect == 'ctsctsinteraction',
                                      yes = variable,
                                      no = term))

  # Tag splines if seperator is not null
  if (is.null(spline_seperator) == F){
    x <- x %>%
      dplyr::mutate(effect = base::ifelse((effect == 'ctsmain') & (stringr::str_detect(string = variable, pattern = spline_seperator) == T),
                                                      yes = 'ctsspline',
                                                      no = effect)) %>%
      dplyr::mutate(effect = base::ifelse((effect == 'factorandctsinteraction') & (stringr::str_detect(string = variable, pattern = spline_seperator) == T),
                                          yes = 'factorandctsinteractionspline',
                                          no = effect))
  }

  # Calculate variable importance and add to summary
  if (vimethod == 'model'){
    v <- vip::vi(m, method = vimethod)
    x <- dplyr::left_join(x, v, by = c('term' = 'Variable')) %>%
      dplyr::mutate(Importance = base::ifelse(is.na(Importance), 0 , Importance),
                    Sign = base::ifelse(is.na(Sign), 'NEU' , Sign))
  } else if (vimethod %in% c('permute', 'firm')){
    vp <- vip::vi(object = m, method = vimethod, ...)
    vp <-  vp %>%
      dplyr::filter(Importance != 0) %>%
      dplyr::mutate(Importance = base::abs(Importance))
    # Make a copy for interaction importance, in these case for interactions the importance will be addative
    vp <- vp %>%
      dplyr::mutate(dummyjoin  = 'a')
    ivp <-vp %>%
      dplyr::left_join(vp, by = 'dummyjoin') %>%
      dplyr::mutate(Interacted_Variable = base::paste0(Variable.x,':',Variable.y)) %>%
      dplyr::mutate(Interacted_Importance = Importance.x + Importance.y) %>%
      dplyr::select(c('Interacted_Variable', Interacted_Importance))

    x <- dplyr::left_join(x, vp, by = c('variable' = 'Variable')) %>%
      dplyr::left_join(ivp, by = c('variable' = 'Interacted_Variable')) %>%
      dplyr::mutate(Importance = base::ifelse(is.na(Importance) == T, Interacted_Importance,Importance)) %>%
      dplyr::select(-c('dummyjoin','Interacted_Importance')) %>%
      dplyr::mutate(Sign = NA) %>%
      dplyr::mutate(Importance = base::ifelse(is.na(Importance), 0 , Importance),
                    Sign = base::ifelse(is.na(Sign), 'NEU' , Sign)
      )
  }

  # Select columns we want in the output
  x <- x %>%
    dplyr::select(-name, -est) %>%
    dplyr::left_join(d, by = "term") %>%
    dplyr::select(c(term, variable, level, effect, Importance, Sign, estimate, std.error, statistic, p.value))

  return(x)
}

