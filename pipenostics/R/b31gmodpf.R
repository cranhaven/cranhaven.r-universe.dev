#' @title
#'  ASME B31G. Failure pressure of the corroded pipe (modified)
#'
#' @family ASME B31G functions
#'
#' @description
#'  Calculate failure pressure of the corroded pipe
#'  according to \emph{Modified B31G},
#'  \emph{Level-1} algorithm listed
#'  in \href{https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines}{ASME B31G-2012}.
#'
#'  The next assumption of corrosion shape is adopted by \emph{Modified B31G}:
#'
#'  \figure{b31gmodpf.png}
#'
#'  There d\emph{cor} represents argument \code{depth}.
#'
#' @details
#'   Since the definition of flow stress, \emph{Sflow}, in
#'   \href{https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines}{ASME B31G-2012}
#'   is recommended with \emph{Level 1} as follows:
#'
#'   \deqn{Sflow =  1.1SMYS}
#'
#'   no other possibilities of its evaluation are incorporated.
#'
#'   For this code we avoid possible semantic optimization to
#'   preserve readability and correlation with original text description
#'   in \href{https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines}{ASME B31G-2012}.
#'   At the same time source code for estimated failure pressure preserves
#'   maximum affinity with its semantic description in \href{https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines}{ASME B31G-2012}.
#'
#'   Numeric \code{NA}s may appear in case prescribed conditions of
#'   use are offended.
#'
#' @param d
#'  nominal outside diameter of pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.
#'
#' @param wth
#'  nominal wall thickness of pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.
#'
#' @param smys
#'  specified minimum yield of stress (\emph{SMYS}) as a
#'  characteristics of steel strength, [\emph{PSI}]. Type: \code{\link{assert_double}}.
#'
#' @param depth
#'   measured maximum depth of the corroded area, [\emph{inch}]. Type: \code{\link{assert_double}}.
#'
#' @param l
#'  measured maximum longitudinal length of corroded area, [\emph{inch}]. Type: \code{\link{assert_double}}.
#'
#' @return
#'  Estimated failure pressure of the corroded pipe, [\emph{PSI}]. Type: \code{\link{assert_double}}.
#'
#' @references
#'  \enumerate{
#'  \item \href{https://store.accuristech.com:443/standards/asme-b31g-2012-r2017?product_id=1842873}{ASME B31G-2012}.
#'    Manual for determining the remaining strength of corroded pipelines:
#'    supplement to \emph{B31 Code} for pressure piping.
#'  \item  S. Timashev and A. Bushinskaya, \emph{Diagnostics and Reliability
#'    of Pipeline Systems}, Topics in Safety, Risk, Reliability and Quality 30,
#'    \strong{DOI 10.1007/978-3-319-25307-7}
#'  }
#'
#' @seealso
#'   Other fail pressure functions: \code{\link{b31gpf}}, \code{\link{dnvpf}},
#'   \code{\link{shell92pf}}, \code{\link{pcorrcpf}}
#'
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#'  ## Example: maximum percentage disparity of original B31G
#'  ## algorithm and modified B31G showed on CRVL.BAS data
#'  with(b31gdata, {
#'    original  <- b31gpf(d, wth, smys, depth, l)
#'    modified  <- b31gmodpf(d, wth, smys, depth, l)
#'    round(max(100*abs(1 - original/modified), na.rm = TRUE), 4)
#'  })
#'  ## Output:
#'  #[1] 32.6666
#'
#'  ## Example: plot disparity of original B31G algorithm and
#'  ## modified B31G showed on CRVL data
#'  with(b31gdata[-(6:7),], {
#'    b31g  <- b31gpf(depth, wth, smys, depth, l)
#'    b31gmod  <- b31gmodpf(depth, wth, smys, depth, l)
#'    axe_range <- range(c(b31g, b31gmod))
#'    plot(b31g, b31g, type = 'b', pch = 16,
#'         xlab = 'Pressure, [PSI]',
#'         ylab = 'Pressure, [PSI]',
#'         main = 'Failure pressure method comparison',
#'         xlim = axe_range, ylim = axe_range)
#'    inc <- order(b31g)
#'    lines(b31g[inc], b31gmod[inc], type = 'b', col = 'red')
#'    legend('topleft',
#'           legend = c('B31G Original',
#'                      'B31G Modified'),
#'           col = c('black', 'red'),
#'           lty = 'solid')
#'  })
#'
b31gmodpf <- function(d, wth, smys, depth, l) {
  checkmate::assert_double(
    d, lower = .03937008, upper = 196.8504, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    wth, lower = 0, upper = 19.68504, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    smys, lower = 725.1887, upper = 290075.4760, finite = TRUE,
    any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    depth, lower = 0, upper = 39.37008, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    l, lower = 0, upper = 196.8504, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_true(commensurable(c(
    length(d), length(wth), length(smys), length(depth), length(l)
  )))

  z  <- l^2/d/wth
  s_flow <- 1.1*smys

  # Operate with M^2 to avoid sqrt warning when negative argument
  M2  <- ifelse(z > 50, (3.2e-2*z + 3.3)^2, 1.0 + .6275*z - 3.375e-3*z^2)
  M2[ M2 < .Machine[["double.eps"]] ] <- NA_real_

  dw <- depth/wth
  sf <- s_flow*(1.0 - .85*dw)/(1.0 - .85*dw/sqrt(M2))
  Pf <- 2.0*sf*wth/d
  Pf[smys > 55984.57 | depth < .1*wth | depth > .85*wth] <- NA_real_
  Pf  # Failure pressure, Pf, [PSI]
}
