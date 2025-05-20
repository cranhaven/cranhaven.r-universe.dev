#' Return the recaptures from amongst the samples
#'
#' Given the tibble of sampled individuals
#' return a tibble of all the recaptures.
#' This is a somewhat complicated operation
#' that expands recaptures to all possible pairs of recaptures. (i.e. if the
#' individual was caught on 3 different occasions there will be three separate pairs).
#' The column pair_type shows RC for recapture.
#'
#' This is broken now that sampling in different life episodes is
#' demarcated as such. But I have left it in here unexported and internal
#' in case I want to pick it up at some point.
#' @param S the samples tibble with columns ID, sex, born_year, sampling_year
#' @keywords internal
recapture_pairs <- function(S) {
  S %>%
    rename_all(paste0, "_1") %>%
    group_by(ID_1) %>%
    filter(n() > 1) %>%
    arrange(sampling_year_1) %>% # always put the first sampling episode first
    dplyr::do(
      bind_cols(
        .[rep(1:nrow(.), each = nrow(.)), ],
        .[rep(1:nrow(.), nrow(.)), ] %>%
          rename_all(.funs = function(x) str_replace(x, "_1$", "_2"))
      )
    ) %>%
    ungroup() %>%
    filter(sampling_year_1 < sampling_year_2) %>%
    mutate(pair_type = "RC") %>%
    select(pair_type, everything())

}
