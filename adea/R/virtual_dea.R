#' Compute virtual input and output for (standard) DEA model
#'
#' For the given orientation, input, output and weights compute virtual input and output as described in @references A new approach to the bi-dimensional representation of the DEA efficient frontier with multiple inputs and outputscompute DMU's efficiencies
#'
#' @name virtual_dea
#' @aliases virtual_dea
#' @keywords internal
#' @inheritParams adea
#' @return {vinput, voutput}
virtual_dea <- function(input, output, orientation, ux, vy)
{
    orientation <- match.arg(orientation, choices = c('input', 'output'))

    ## Compute vinput and voutput
    s <- switch(orientation,
                output = rowSums(vy),
                input = rowSums(ux)
                )
    ux <- ux/s
    vy <- vy/s
    vinput <- rowSums(ux * input)
    names(vinput) <- rownames(input)
    voutput <- rowSums(vy * output)
    names(voutput) <- rownames(input)

    list(vinput = vinput, voutput = voutput)
}
