
#' Split reported case data
#' 
#' Split reported case integer time series into train and validate time series through thinning.
#'
#' @inheritParams fit_incidence
#' @return A list(reported_train, reported_val) where the elements reported_train
#'         and reported_val are both length, Tobs, and 'frac_train' of
#'         the counts fall in reported_train, the rest in reported_val.
#' @export
train_val_split <- function(reported, frac_train=.75) {
    # string out into cases
    Xn = rep(1:length(reported), reported)
    # generate random train/val sets
    num_train = round(frac_train*length(Xn))
    rand_perm = sample(1:length(Xn))
    train_idx = rand_perm[1:num_train]
    val_idx   = rand_perm[(num_train+1):length(rand_perm)]
    # subset, stack into time series of counts, and return
    Tmax = length(reported)
    reported_train = c(unname(table(factor(Xn[train_idx], 1:Tmax))))
    reported_val   = c(unname(table(factor(Xn[val_idx], 1:Tmax))))
    return(list(reported_train=reported_train, reported_val=reported_val))
}

