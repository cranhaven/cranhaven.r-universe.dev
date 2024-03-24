
# Decides whether a vector is so large that it should be sampled instead of used straight
#
# Running statistics on very long vectors can take quite a while, and that time piles
# up when you have to do it repeatedly; a 100 ms operation over 800 columns is a
# 1.5 minute wait for just that one operation. This function decides whether a variable
# is small enough to simply use, or whether it's so big that randomly sampling from it
# would be more efficient.
#
# @param x (Vector) The vector to sample.
#
# @return A named list with 3 entries: `indices` contains chosen indices of `x`; `marker`
#      is a text marker that indicates whether `x` was sampled. `is_approx` is `TRUE` if
#      `x` was sampled, and `FALSE` if all indices of `x` were returned.
#
# @md
should_approx <- function(x) {
    if (length(x) > options_sift("sift_guessmax")) {
        x         <- sort(sample.int(n       = length(x),
                                     replace = FALSE,
                                     prob    = NULL,
                                     size    = options_sift("sift_guessmax")))
        mark      <- cli::symbol["warning"]
        is_approx <- TRUE
    } else {
        x         <- seq_along(x)
        mark      <- character(0)
        is_approx <- FALSE
    }

    return(list(indices = x, marker = mark, is_approx = is_approx))
}
