#' Bin Cols
#'
#' Make bins in a tidy fashion. Adds a column to your data frame containing the integer codes of the specified bins of a certain column.
#' Specifying multiple columns is only intended for supervised binning, so mutliple columns can be simultaneously binned
#' optimally with respect to a target variable.
#'
#'
#' Description of the arguments for bin_type
#'
#' \describe{
#'   \item{\emph{frequency (fr)}}{ creates bins of equal content via quantiles. Wraps \code{\link[OneR]{bin}} with method "content". Similar to  \code{\link[dplyr]{ntile}}}
#'   \item{\emph{width (wi)}}{ create bins of equal numeric width. Wraps \code{\link[OneR]{bin}} with method "length"}
#'   \item{\emph{kmeans (km)}}{ create bins using 1-dimensional kmeans. Wraps \code{\link[OneR]{bin}} with method "clusters"}
#'   \item{\emph{value (va)}}{ each bin has equal sum of values}
#'   \item{\emph{xgboost (xg)}}{ column is binned by best predictor of a target column using  \code{\link[embed]{step_discretize_xgb}} }
#'   \item{\emph{cart (ca)}}{ if the col does not have enough distinct values, xgboost will fail and automatically revert to \code{\link[embed]{step_discretize_cart}} }
#'   \item{\emph{woe (wo)}}{ column is binned by weight of evidence. Requires binary target}
#'   \item{\emph{logreg (lr)}}{ column is binned by logistic regression. Requires binary target.}
#'   \item{\emph{mdlp}}{ uses the \code{\link[arulesCBA]{discretizeDF.supervised}} algorithm with a variety of methods.}
#' }
#'
#' @param .data a data frame
#' @param col a column, vector of columns, or tidyselect
#' @param n_bins number of bins
#' @param bin_type method to make bins
#' @param ... params to be passed to selected binning method
#' @param target unquoted column for supervised binning
#' @param pretty_labels logical. If T returns interval label rather than integer rank
#' @param seed seed for stochastic binning (xgboost)
#' @param method method for bin mdlp
#'
#' @return a data frame
#' @export
#'
#' @examples
#'
#' iris %>%
#' bin_cols(Sepal.Width, n_bins = 5, pretty_labels = TRUE) %>%
#' bin_cols(Petal.Width, n_bins = 3, bin_type = c("width", "kmeans")) %>%
#' bin_cols(Sepal.Width, bin_type = "xgboost", target = Species, seed = 1) -> iris1
#'
#' #binned columns are named by original name + method abbreviation + number bins created.
#' #Sometimes the actual number of bins is less than n_bins if the col lacks enough variance.
#' iris1 %>%
#' print(width = Inf)
#'
#' iris1 %>%
#' bin_summary() %>%
#' print(width = Inf)
bin_cols <- function(.data,
                           col,
                           n_bins = 10,
                           bin_type = "frequency",
                           ...,
                           target = NULL,
                           pretty_labels = FALSE,
                           seed = 1,
                           method = "mdlp"
){

bin_type = strex::match_arg(arg = bin_type, choices = c("frequency", "width", "value",
                                                        "kmeans", "xgboost",
                                                        "woe", "logreg", "mdlp"), several_ok = T, ignore_case = T)

  col <- rlang::enexpr(col)
  cols <- rlang::enexprs(col)

  .data %>%
    select_otherwise(!!!cols,
                     otherwise = rlang::is_bare_double,
                     return_type = "df") -> bin_cols1

  bin_cols1 %>% names() -> bin_cols_string

if(any(bin_type %in% c("xgboost", "woe", "logreg"))){
  rlang::enexpr(target) -> target1
  rlang::as_name(target1) -> outcome1
  }


  if("value" %in% bin_type){


    col_nm <- rlang::sym(stringr::str_glue("{bin_cols_string}_va{n_bins}"))

    bin_cols1 %>%
      tibble::rownames_to_column() %>%
      bin_equal_value(col = !!col, n_bins = n_bins) -> value_cols


  suppressMessages({
    .data %>%
      tibble::rownames_to_column() %>%
      dplyr::left_join(value_cols) %>%
      dplyr::select(-tidyselect::all_of("rowname")) -> .data
  })


    if(pretty_labels){
      .data %>% make_labels(original_col = !!col, bucket_col = !!col_nm) -> .data
    }

    }




  if("mdlp" %in% bin_type){

    abbv <- switch(method,
                   caim         = "ci",
                   cacc         = "cc",
                   ameva        = "am",
                   chi2         = "ch",
                   chimerge     = "cm",
                   extendedchi2 = "ec",
                   modchi2      = "mh",
                   mdlp         = "md"
    )

    my_form <- .data %>% autostats::tidy_formula(!!target1, tidyselect::any_of(bin_cols_string))


    .data %>%
      arulesCBA::discretizeDF.supervised(formula = my_form, data = ., method = method) %>%
      dplyr::select( tidyselect::any_of(bin_cols_string)) -> bin_df


    bin_df %>%
      rename_bin_lens(abbv, tidyselect::everything()) %>%
      dplyr::bind_cols(.data) -> .data

    .data %>%
      make_pretty(abbv, pretty_labels) -> .data

  }

  if("woe" %in% bin_type){


    binning <- woeBinning::woe.binning(.data, outcome1, pred.var = bin_cols_string)
    woeBinning::woe.binning.deploy(.data, binning) -> .data

    .data %>%
      dplyr::summarize(dplyr::across(tidyselect::matches("\\.binned$"), dplyr::n_distinct)) %>%
      purrr::map_chr(1) %>%
      stringr::str_c("_wo", .) -> bin_lens


    .data %>%
      dplyr::rename_with(.cols = tidyselect::matches("\\.binned$"), .fn = ~stringr::str_replace(.,"\\.binned$",  bin_lens)) %>%
      dplyr::relocate(tidyselect::any_of(bin_cols_string), .before = tidyselect::matches("_wo[0-9]*$")) -> .data

      .data %>%
        make_pretty(abbv = "wo", pretty_labels)
  }



  if("xgboost" %in% bin_type){

    .data %>%
      dplyr::summarise(dplyr::across(tidyselect::any_of(bin_cols_string), dplyr::n_distinct)) %>%
      unlist() -> sizes
      any(sizes < 20) -> use_cart



    set.seed(seed)
    rlang::new_formula(target1, rlang::sym(".")) -> myform

    rec1 <- recipes::recipe(myform, data = .data)

    if(!use_cart){
      rec2 <- embed::step_discretize_xgb(rec1, tidyselect::any_of(bin_cols_string), outcome = outcome1, num_breaks = n_bins, ...)
      abbv <- "xg"
    } else{
      rec2 <- embed::step_discretize_cart(rec1, tidyselect::any_of(bin_cols_string), outcome = outcome1, ...)
      abbv <- "ca"
    }

    rec3 <- recipes::prep(rec2, training = .data)

    recipes::bake(rec3, new_data = NULL) -> new_data

    new_data %>%
      rename_bin_lens(abbv = abbv, cols = tidyselect::any_of(bin_cols_string)) -> new_data

    new_data %>%
      dplyr::bind_cols(.data %>% dplyr::select(tidyselect::any_of(bin_cols_string))) %>%
      dplyr::relocate(tidyselect::matches(stringr::str_c(bin_cols_string, collapse = "|"))) -> .data

    .data %>%  make_pretty(abbv = abbv, pretty_labels = pretty_labels) -> .data
  }

if("logreg" %in% bin_type){

  .data %>% dplyr::pull(!!target1) %>% dplyr::n_distinct() -> n_levels
  my_form <- .data %>% autostats::tidy_formula(!!target1, tidyselect::any_of(bin_cols_string))
  OneR::optbin(my_form, .data) -> optbins
  optbins %>% dplyr::select(-!!target1) %>% dplyr::rename_with(.fn = ~stringr::str_c(., "_", "lr", n_levels)) -> opt_bins
  .data %>% dplyr::bind_cols(opt_bins) -> .data

  .data %>%
    make_pretty("lr", pretty_labels) -> .data
  }

  if("frequency" %in% bin_type){

    oner_wrapper(bin_cols1, .data,  "fr", "content", n_bins = n_bins, pretty_labels = pretty_labels) -> .data
  }

  if("width" %in% bin_type){

    oner_wrapper(bin_cols1, .data,  "wi", "length", n_bins = n_bins, pretty_labels = pretty_labels) -> .data
  }

  if("kmeans" %in% bin_type){

    oner_wrapper(bin_cols1, .data,  abbv = "km", bin_method = "cluster", n_bins = n_bins, pretty_labels = pretty_labels) -> .data

  }

  .data %>% tibble::as_tibble()

}


