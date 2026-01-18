#' Simulating many sequential testing with evidence ratios and plotting their distribution
#'
#' Simulating many sequential evidence ratios using \code{simER}, keeps the last
#' of each simulation, and plotting their distribution.
#'
#' @inheritParams simER
#' @param nsims Number of experiments to simulate.
#'
#' @examples
#' \dontrun{distER(cohensd = 0.6, nmin = 20, nmax = 100, nsims = 100, ic = bic)}
#'
#' @author Ladislas Nalborczyk <\email{ladislas.nalborczyk@@gmail.com}>
#'
#' @seealso \code{\link{simER}}
#'
#' @export

distER <- function(cohensd, nmin, nmax, nsims, ic = bic) {

    .Deprecated("seqER")

    ER <- vector(mode = "numeric", length = nsims)

    ER <-
        simER(
            cohensd = cohensd, nmin = nmin, nmax = nmax, ic = ic,
            boundary = Inf, nsims = nsims, cores = 1, verbose = FALSE) %>%
        group_by(id) %>%
        filter(n == nmax) %>%
        ungroup() %>%
        select(ER) %>%
        data.frame

    print(
        qplot(x = ER, geom = "histogram", bins = sqrt(nsims),
        alpha = 0.75, log = "x", show.legend = FALSE,
        xlab = expression(Evidence~ ~Ratio~ ~ (ER[10]) ) ) +
        theme_bw(base_size = 12)
        )

    return(ER)

}
