
#' Estimate Multi-Valued Krippendorff's Alpha
#'
#' `mvalpha()` calculates Krippendorff's alpha statistic when multi-valued observers are
#' allowed to apply multiple values to an observation.
#'
#' @param data a data frame containing a list column for each observer. Each row represents
#' an observation unit, and each cell contains a vector of 0 to `w` unique values, where `w` is the
#' number of unique values found in the data set. `NA` values are used to represent
#' missing observations and `NULL` values represent the empty set, `{}`, of responses.
#' @param type a string describing the data type of the label set. This can be "nominal",
#' "ordinal", "interval", or "ratio" and is used to select the appropriate distance metric.
#' @param verbose a logical value which toggles whether status updates are printed to
#' the console while alpha is being calculated.
#' @param n_boot an integer representing the number of bootstrap estimates to calculate
#' for mvDo. The default, `NULL`, will not generate additional estimates.
#' @param n_threads an integer describing the number of cores to allocate to parallelization.
#' @returns An object of class `mvalpha`
#' @export
#' @example man/examples/mvalpha_example.R
#' @references
#' \insertRef{Krippendorff-Craggs-2016}{mvalpha}
#' @useDynLib mvalpha, .registration = TRUE
#' @importFrom Rdpack reprompt
#' @importFrom stats na.omit setNames
#' @importFrom utils head tail
#' @importFrom arrangements icombinations ncombinations
#' @importFrom Rcpp sourceCpp


mvalpha <-
  function(data, type = "nominal", verbose = TRUE, n_boot = NULL, n_threads = 1){

    if(type != "nominal") warning("Data types other than nominal have not been thoroughly tested. Use with caution.")

    bootstrap_mvDo <-
      function(){
        vapply(units[which(m != 0)], function(u){
          (sample(dist_CK, size = (m[u] - 1) * m[u] / 2, replace = TRUE, prob = p_CK) /
             (m[u] - 1)) |>
            sum()
        }, 1) |> sum() * 2 / n..
      }

    metric_delta_CK <-
      function(C, K, KC_set_ops){

        if(type == "nominal"){
          if(!length(C) & !length(K)){return(0)}
          else{
            result <-
              1 - (2 * length(KC_set_ops$A_intersect_B) /
                     (length(C) + length(K)))
            return(result)
          }
        }

        if(rlang::is_empty(C) | rlang::is_empty(KC_set_ops$A_diff_B)){lhs_numerator <- 0}else{
          lhs_numerator <-
            outer(C, KC_set_ops$A_diff_B, metric_delsq_ck) |>
            unlist() |>
            sum(na.rm = TRUE) |>
            (\(x) x / length(C))()
        }
        if(rlang::is_empty(K) | rlang::is_empty(KC_set_ops$B_diff_A)){rhs_numerator <- 0}else{
          rhs_numerator <-
            outer(K, KC_set_ops$B_diff_A, metric_delsq_ck) |>
            unlist() |>
            sum(na.rm = TRUE) |>
            (\(x) x / length(K))()
        }
        denominator <- length(C) + length(K)
        result <-
          ifelse(lhs_numerator == 0 & rhs_numerator == 0 & denominator == 0,
                 0,
                 ifelse(rlang::is_empty(C) | rlang::is_empty(K),
                        1,
                        (lhs_numerator + rhs_numerator) / denominator))
        return(result)
      }

    nominal_delsq_ck <-
      function(c, k){as.numeric(!(c == k))}

    ordinal_delsq_ck <-
      Vectorize(
        function(c, k){
          if(is.character(c)) c <- match(values[c], labels)
          if(is.character(k)) k <- match(values[k], labels)
          ((sum(n_c_w[c:k]) - ((n_c_w[c] + n_c_w[k]) / 2)) /
              (n.. - (n_c_last + n_c_first) / 2)) ^ 2
        },
        vectorize.args = c("c", "k")
      )

    interval_delsq_ck <-
      function(c, k){
        ((c - k) / (c_max - c_min)) ^ 2
      }

    ratio_delsq_ck <-
      function(c, k){
        ((c - k) / (c + k)) ^ 2
      }

    # Select appropriate metric

    metric_delsq_ck <-
      switch(type,
             nominal = nominal_delsq_ck,
             ordinal = ordinal_delsq_ck,
             interval = interval_delsq_ck,
             ratio = ratio_delsq_ck)

    # Summary vectors and tables used to organize data

    labels <- data |> unlist() |> unique() |> sort()
    observers <- colnames(data)
    units <- rownames(data)
    n_labels <- length(labels)
    n_observers <- length(observers)
    n_units <- length(units)
    continuous_data <- type %in% c("interval", "ratio") # logical

    label_array <- # indicator array for labels by observer and unit
      apply(data, MARGIN = c(1, 2),
            function(x){
              if(!is.na(x)){as.numeric(labels %in% unlist(x))
              }else{
                rep(NA, length(labels))
              }
            }) |>
      aperm(c(2, 3, 1))

    dimnames(label_array) <- list(unit = units, observer = observers, label = labels)

    values_def <- apply(label_array, 3, rbind) |> unique(MARGIN = 1) |> stats::na.omit()
    n_values <- nrow(values_def)

    values <- apply(values_def, MARGIN = 1, function(x){
      if(type == "nominal"){x[which(x == 1)] |> names()}
      else{x[which(x == 1)] |> names() |> as.numeric()}}) |>
      lapply(function(v){factor(v, levels = labels)})

    value_names <- values |> lapply(function(x){paste0("{", paste0(x, collapse = ", "), "}")}) |> unlist() |> unname()
    names(values) <- value_names

    rownames(values_def) <- value_names

    values_by_unit <- # create value-by-unit matrix
      apply(values_def, MARGIN = 1, function(v){
        apply(label_array, MARGIN = 1, function(x){
          apply(x, MARGIN = 1, function(r){
            all(v==r)
          }) |> sum(na.rm = TRUE)
        })
      }) |> t() |> unname()

    dimnames(values_by_unit) <- list(value = value_names, unit = units)

    m <- colSums(values_by_unit)
    m[which(m<2)] <- 0 # remove non-pairable observations
    values_by_unit[, which(m==0)] <- 0

    n <- rowSums(values_by_unit)
    n.. <- sum(n)

    value_cardinalities <- rowSums(values_def)
    unique_cardinalities <- unique(value_cardinalities) |> sort()
    null_set_observed <- 0 %in% unique_cardinalities # special behavior flag for null set

    w <- data |> unlist() |> unique() |> setdiff(NA) |> length()
    if(null_set_observed) w <- w + 1
    n_c_w <- colSums(values_def * n)
    if(null_set_observed) n_c_w <- c(null_set = 1, n_c_w)

    if(type == "ordinal"){ # define additional variables needed for ordinal metric
      n_c_first <- utils::head(n_c_w, n = 1)
      n_c_last <- utils::tail(n_c_w, n = 1)
    }

    if(type == "interval"){ # define additional variables needed for interval metric
      c_min <- min(labels)
      c_max <- max(labels)
    }

    P <- # probability of observing a label set of cardinality |C|
      vapply(unique_cardinalities, function(x){
        sum(n[which(value_cardinalities == x)]) / n..
      }, 1) |> stats::setNames(unique_cardinalities)

    all_label_combinations <- # generate all ways to choose |C| from n_labels
      lapply(unique_cardinalities,
             function(f){if(f == 0){arrangements::icombinations(k = 1, v = 0)}else{arrangements::icombinations(n_labels, f)}}) |>
      stats::setNames(unique_cardinalities)

    n_label_combinations <- lapply(all_label_combinations, function(`|H|`){arrangements::ncombinations(n = `|H|`$n, k = `|H|`$k)}) |> unlist()

    # if(continuous_data){lapply(all_label_combinations, function(`|H|`){ # redefine indicator combinations using actual values if data are continuous
    #   matrix(labels[`|H|`], nrow = nrow(`|H|`))
    # })}

    p_CK <- # probabilities of the observed coincidence of C-K pairs
      lapply(value_names, function(C){
        lapply(value_names, function(K){
          if(C == K){(values_by_unit[C, ] * (values_by_unit[K, ] - 1) / (m - 1)) |> sum()}
          else{(values_by_unit[C, ] * values_by_unit[K, ] / (m - 1)) |> sum()}
        }) |> unlist()
      }) |> do.call(cbind, args = _) |>
      (\(i) i / n..)()

    dimnames(p_CK) <- list(value_names, value_names)

    dist_CK <- # distances between C-K pairs
      lapply(values, function(C){
        C_num <- as.numeric(C)
        lapply(values, function(K){
          K_num <- as.numeric(K)
          metric_delta_CK(C_num, K_num, set_ops_r(K_num, C_num, type = type, n_labels = n_labels))
        }) |> unlist()
      }) |> do.call(cbind, args = _)

    # Print status update

    if(verbose){cat(
      paste0("n Units: ", n_units, "\n",
             "n Observers: ", n_observers, "\n",
             "n Labels: ", n_labels, "\n",
             "n Values: ", n_values, "\n",
             "Max Cardinality: ", max(unique_cardinalities), "\n",
             "Possible C-K pairs: ",
             (n_label_combinations %*% t(n_label_combinations)) |>
               (\(m) m[upper.tri(m, diag = TRUE)])() |>
               sum() |> prettyNum(big.mark = ","),
             "\n"
      ))}

    # Notation:
    # C is a set (vector) of elements
    # |C| is the cardinality (integer) of set C
    # C_|C| is the collection (list) of sets with cardinality |C|

    mvDo <- sum(p_CK * dist_CK)

    cardinality_pairs <-
      lapply(unique_cardinalities, function(`|C|`){
        lapply(unique_cardinalities[which(unique_cardinalities >= `|C|`)], function(`|K|`){
          list(`|C|` = `|C|`, `|K|` = `|K|`)
        })
      }) |> unlist(recursive = FALSE)

    pair_cardC <- sapply(cardinality_pairs, `[[`, "|C|")
    pair_cardK <- sapply(cardinality_pairs, `[[`, "|K|")
    pair_P_C   <- sapply(cardinality_pairs, function(p) P[as.character(p$`|C|`)])
    pair_P_K   <- sapply(cardinality_pairs, function(p) P[as.character(p$`|K|`)])

    mvDe <- calc_mvDe_cpp(
      n_labels           = n_labels,
      cardC_vec          = as.integer(pair_cardC),
      cardK_vec          = as.integer(pair_cardK),
      n_c_w              = n_c_w,
      null_set_observed  = as.integer(null_set_observed),
      P_C_vec            = pair_P_C,
      P_K_vec            = pair_P_K,
      type               = type,
      n_dotdot           = if(type == "ordinal")  n..    else 0,
      n_c_first          = if(type == "ordinal")  n_c_first else 0,
      n_c_last           = if(type == "ordinal")  n_c_last  else 0,
      c_min              = if(type == "interval") c_min  else 0,
      c_max              = if(type == "interval") c_max  else 0,
      labels             = if(type %in% c("interval", "ratio")) as.numeric(labels) else numeric(0),
      n_threads          = n_threads
    )

    mvDo_boot <- # Bootstrap method described by Krippendorff and Craggs (2016)
      if(!is.null(n_boot)){
        lapply(1:n_boot, function(iter){bootstrap_mvDo()}) |> unlist()
      }else{NULL}

    return(
      new_mvalpha(
        mvalpha = 1 - mvDo / mvDe,
        type = type,
        mvDo = mvDo,
        mvDe = mvDe,
        bootstrap_mvalpha = 1 - (mvDo_boot / mvDe),
        unique_cardinalities = unique_cardinalities,
        units = units,
        observers = observers,
        labels = labels,
        values = values,
        values_by_unit = values_by_unit,
        dist_CK = dist_CK,
        p_CK = p_CK
      )
    )

  }

#' Generate Multi-Valued Data Sets
#'
#' @param type Data type. One of "nominal", "ordinal", "interval", or "ratio".
#' @param n_units Number of units (rows) in data.
#' @param n_observers Number of observers (cols) in data.
#' @param n_labels Number of possible labels which could be applied to the data.
#' @param tpr True Positive Rate. To generate the data, first a latent set of labels. `tpr` describes the probability that a latent label will be identified by each observer.
#' @param fpr False Positive Rate. To generate the data, first a latent set of labels. `fpr` describes the probability that each observer includes each label not in the latent set.
#' @param card_pmf Probability mass function describing the cardinality of observed label sets. Length should be less than or equal to `n_labels`.
#' @param p_missing Proportion of observations that are randomly missing, indicated by `NA`. These are distinct from observations which are made, but the observer applied 0 labels, indicated by `NULL`.
#'
#' @returns A matrix with `n_units` rows and `n_observers` columns with list elements.
#' @export
#' @importFrom stats rbinom rnorm runif
#' @importFrom stringr str_pad
#'
#' @examples generate_mv_data()

generate_mv_data <-
  function(type = "nominal", n_units = 10, n_observers = 3, n_labels = 5, tpr = 0.80, fpr = 0.01, card_pmf = c(0.3, 0.4, 0.2, 0.1), p_missing = 0){

    labels <-
      switch(type,
             nominal = letters[1:n_labels],
             ordinal = factor(letters[1:n_labels], ordered = TRUE),
             interval = runif(n_labels, -50, 50),
             ratio = runif(n_labels, 0, 100))

    max_card <- length(card_pmf)

    latent_label <- lapply(1:n_units, function(i){sample(labels, size = sample(1:max_card, size = 1, prob = card_pmf))})

    data <-
      lapply(1:n_observers, function(r){
        lapply(latent_label, function(u){
          probs <- c(rep(tpr, length(u)), rep(fpr, n_labels - length(u)))
          response <- c(labels[which(labels %in% u)], labels[which(!labels %in% u)])[rbinom(n_labels, 1, prob = probs) == 1]
          if(length(response) == 0){
            NULL
            }else{
              if(type %in% c("interval")){response <- rnorm(length(response), mean = response, sd = 10)}
              if(type %in% c("ratio")){response <- rnorm(length(response), mean = response, sd = abs(0.1 * response)) |> pmax(response, 0)}
              response
              }
        })
      }) |> do.call(cbind, args = _)

    dimnames(data) <- list(paste0("unit_", stringr::str_pad(1:n_units, width = nchar(n_units), side = "left", pad = "0")),
                           paste0("obs_", stringr::str_pad(1:n_observers, width = nchar(n_observers), side = "left", pad = "0")))

    data[which(rbinom(length(data), 1, prob = p_missing) == 1)] <- NA

    return(data)
}


