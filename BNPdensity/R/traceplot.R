#' Draw a traceplot for multiple chains
#'
#' This is a convenience function which works when coda is not yet loaded by the user. If coda is
#' loaded, it gets masked. See also file multMixNRMI.R
#'
#' @param fitlist Output of multMixNRMI.
#' @return A traceplot for multiple chains.
#' @export
traceplot <- function(fitlist) {
  param <- value <- chain_id <- iteration <- NULL
  mcmc_object <- convert_to_mcmc(fitlist)
  to_plot <- tidyr::gather(
    dplyr::bind_rows(
      lapply(
        X = seq_along(mcmc_object),
        FUN = function(chain_id) {
          dplyr::mutate(dplyr::mutate(data.frame(mcmc_object[[chain_id]]), chain_id = chain_id),
            iteration = seq_along(chain_id)
          )
        }
      )
    ),
    param, value, -chain_id, -iteration
  )

  ggplot(to_plot, aes(x = iteration, y = value, colour = factor(chain_id), group = chain_id)) +
    geom_line() +
    facet_wrap(~param, scales = "free") +
    theme_classic() +
    ylab("") +
    theme(legend.position = "none") +
    xlab("Iteration")
}
