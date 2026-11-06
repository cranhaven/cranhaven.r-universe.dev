#' Compute efficiencies for (standard) DEA model
#'
#' For the given orientation, input, output and weights compute DMU's efficiencies
#'
#' Unlike the dea function, this function expects input and output to be arrays or data.frames, not vectors.
#'
#' Note: As this function is mainly for internal use, to avoid unnecessary overload, it does not do an extensive check of the input parameters. Use the higher level dea function instead.
#' 
#' @name eff_dea
#' @aliases eff_dea
#' @keywords internal
#' @inheritParams adea
#' @return eff
eff_dea <- function(input, output, orientation, ux, vy)
{
    orientation <- match.arg(orientation, choices = c('input', 'output'))

    ## Compute scores
    eff <- switch(orientation,
                  input = rowSums(vy * output),
                  output = rowSums(ux * input)
                  )
    names(eff) <- rownames(input)

    eff
}
