check_tibble_or_df <- function(data){
  if (!(inherits(data, "tbl_df") || inherits(data, "data.frame")))
    cli::cli_abort("Currently only support a tibble or a data frame as
                    the input of tidyindex workflow.")
}

check_idx_tbl <- function(data){
  if (!inherits(data, "idx_tbl")){
    cli::cli_abort("A index table object is required as input.
                   Created it using {.fn init}. ")
  }
}

check_rescale_obj <- function(obj){
  if (!inherits(obj, "rescale")){
    cli::cli_abort("A rescale object is required as input.
                   Created it using {.fn rescale_*}.")
    }
}


check_dim_red_obj <- function(obj){
  if (!inherits(obj, "dim_red")){
    cli::cli_abort("A dimension reduction object is required as input.
                   Create it using {.fn aggregate_*}")
  }
}

check_dist_fit_obj <- function(obj){
  if (!inherits(obj, "dist_fit")){
    cli::cli_abort("A distribution fit object is required as input.
                   Create it using {.fn dist_*}")
  }
}


check_var_trans_obj <- function(obj){
  if (!inherits(obj, "var_trans")){
    cli::cli_abort("A variable transformation object is required as input.
                   Create is using {.fn trans_*}")
  }
}

check_temp_agg_obj <- function(obj){
  if (!inherits(obj, "temporal_agg")){
    cli::cli_abort("A temporal aggregation object is required as input.
                   Create it using {.fn temporal_*}")
  }
}

check_normalise_obj <- function(obj){
  if (!inherits(obj, "normalise")){
    cli::cli_abort("A normalisation object is required as input.
                   Create it using {.fn norm_*}")
  }
}

check_temporal_index <- function(obj){
  index <- get_temporal_index(obj)
  if (length(index) == 0){
    cli::cli_abort("No temporal index is found in the input data.
                   Please supply through {.fn init}")
  }
  id <- get_id(obj)
  if (length(id) == 1){
    dt <- obj$data |> dplyr::group_by(!!sym(id))
  } else{
    dt <- obj$data
  }
  res <-  dt |> arrange(!!sym(index), .by_group = TRUE) |> pull(!!sym(index))
  obj_idx <- obj$data |> pull(!!sym(index))
  if (any(res != obj_idx)){
    cli::cli_abort("The temporal index is not ordered. Please check the
                   input data.")
  }
}


check_lmomco_installed <- function(){
  if (!requireNamespace("lmomco", quietly = TRUE)){
    cli::cli_abort("lmomco package is required for computing L-moments")
  }
}

check_slider_installed <- function(){
  if (!requireNamespace("slider", quietly = TRUE)){
    cli::cli_abort("slider package is required for computing rolling
                   window statistics")
  }
}
