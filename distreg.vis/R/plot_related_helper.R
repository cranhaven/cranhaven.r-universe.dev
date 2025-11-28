#' Internal: Plot limit getter
#'
#' A function that heavily relies on the \code{distreg.vis::dists} data.frame to
#' obtain optimal plotting limits. Specifically, this function relies on the
#' columns \code{type_limits}, \code{l_limit}, \code{u_limit}.
#'
#' Three cases: categorical limits (\code{cat_limits}), no_limits, has_limits, both_limits
#' @keywords internal
limits <- function(fam_name, predictions) {

  # Get limit type
  lim_type <- type_getter(fam_name)

  # First case - no limits. Example: Normal distribution
  if (lim_type == "no_limits") {

    # Get limits with quants()
    quant_lims <- quants(fam_name, predictions)

    # Max and min for limits
    lims <- c(lower = min(quant_lims$lower), upper = max(quant_lims$upper))
  }

  # Second case: One Limit / Two Limits. Example: Beta distribution
  if (lim_type %in% c("one_limit", "both_limits")) {

    # Get theoretical lims by dists data.frame - this means the support of the distribution
    theo_lims <- lims_getter(fam_name)

    # Get limits with quants()
    quant_lims <- quants(fam_name, predictions)

    # Min/max of empirical 0.1% and 99.1% quantiles
    min_lim <- min(quant_lims$lower)
    max_lim <- max(quant_lims$upper)

    # Check here whether to use max/min or the theoretical limits
    if (isTRUE(min_lim < theo_lims$l_limit)) # this works even if there is an NA because then it won't be true as well... magic...
      lower <- theo_lims$l_limit
    else
      lower <- min_lim
    if (isTRUE(max_lim > theo_lims$u_limit))
      upper <- theo_lims$u_limit
    else
      upper <- max_lim

    # Limits
    lims <- c(lower = lower, upper = upper)
  }

  return(lims)
}

#' Internal: Limit type getter
#'
#' Get the limit type depending on \code{distreg.vis::dists}.
#' @keywords internal
type_getter <- function(fam_name) {
  type <- distreg.vis::dists[distreg.vis::dists$dist_name == fam_name, "type_limits", drop = TRUE]
  return(as.character(type))
}

#' Internal: Upper- and lower limit of distribution getter
#'
#' Obtain the theoretical upper and lower limits of the distribution. Only
#' necessary if the distribution has limits
#' @keywords internal
lims_getter <- function(fam_name) {
  return(distreg.vis::dists[distreg.vis::dists$dist_name == fam_name, c("l_limit", "u_limit")])
}


#' Internal: Transform discrete predictions into a usable df
#'
#' @importFrom stats reshape
#' @keywords internal
disc_trans <- function(pred_params, fam_name, type, model, lims) {

  if (fam_name != "multinomial") {
    # Get discrete sequence (x axis for distribution plots)
    xvals <- seq.int(from = lims["lower"], to = lims["upper"])

    # Get right function
    if (type == "pdf")
      fun <- fam_fun_getter(fam_name, "d")
    if (type == "cdf")
      fun <- fam_fun_getter(fam_name, "p")

    # Get y values and construct df
    compl_df <- apply(pred_params, 1, FUN = function(x) {
      fun(xvals, par = as.list(x))
    })
    compl_df <- as.data.frame(compl_df)
    colnames(compl_df) <- paste0("pn.", row.names(pred_params))
    compl_df$xvals <- xvals

    # If cdf then add a new row b.c. of visual reasons
    if (type == "cdf")
      compl_df <- rbind(c(rep(0, nrow(pred_params)), -1e-100), compl_df)

    # Wide to long
    compl_reshaped <- reshape(
      compl_df,
      direction = "long",
      idvar = "xvals",
      varying = seq_len(nrow(pred_params))
    )
    row.names(compl_reshaped) <- seq_len(nrow(compl_reshaped))
    colnames(compl_reshaped) <- c("xvals", "rownames", "value")
    compl_reshaped$rownames <- as.character(compl_reshaped$rownames)

    # Return it
    return(compl_reshaped)
  }

  if (fam_name == "multinomial") {
    if (type == "pdf") {
      ## Transform the predictions such that probabilities for all classes are given
      levels <- levels(model$model.frame[, 1])
      psums <- rowSums(pred_params) + 1
      p0 <- 1 / psums
      trans_preds <- cbind(p0, matrix(apply(pred_params, 2, FUN = function(x)
        return(x * p0)), ncol = length(levels) - 1)) # matrix because else it will not work with just one row
      trans_preds <- as.data.frame(trans_preds)
      colnames(trans_preds) <- paste0("lv.", levels)
      trans_preds$rownames <- row.names(trans_preds)
      tf_df <- reshape(trans_preds,
                       varying = seq_len(length(levels)),
                       idvar = "rownames",
                       direction = "long")
      colnames(tf_df) <- c("rownames", "xvals", "value")
      rownames(tf_df) <- seq_len(nrow(tf_df))
      tf_df$xvals <- factor(tf_df$xvals, labels = levels)
      return(tf_df)
    }
    if (type == "cdf") {
      stop("CDF of Multinomial Family not feasible")
    }
  }
}

#' Internal: Family obtainer
#'
#' Gets the right family (in characters) from a given model
#' @keywords internal
#' @importFrom methods is
#' @examples
#' # Generating data
#' data_fam <- model_fam_data(fam_name = "BE")
#' # Fit model
#' library("gamlss")
#' beta_model <- gamlss(BE ~ norm2 + binomial1,
#'   data = data_fam, family = BE())
#' distreg.vis:::fam_obtainer(model = beta_model)
fam_obtainer <- function(model) {

  # Check whether model is gamlss or bamlss
  if (!distreg_checker(model))
    stop("Unsupported model class provided. \n
         Check ?distreg_checker for supported model classes")

  # gamlss families
  if (is(model, "gamlss"))
    fam <- model$family[1]

  # bamlss families
  if (is(model, "bamlss"))
    fam <- model$family$family

  # betareg
  if (is(model, "betareg") | is(model, "betatree"))
    fam <- "betareg"

  # Return it
  return(fam)
}
