#' @title
#'  Minenergo-278. Thermal conductivity of pipe insulation materials
#'
#' @family Minenergo
#'
#' @description
#'  Get normative values of thermal conductivity of pipe insulation
#'  materials affirmed by
#'  \href{https://docs.cntd.ru/document/1200035568}{Minenergo Method 278} as
#'  a function of temperature of heat carrier (water).
#'
#' @param temperature
#'  temperature of heat carrier (water) inside the pipe, [\emph{°C}].
#'  Type: \code{\link{assert_double}}.
#'
#' @param material
#'  designation of insulation material as it stated in \code{\link{m278insdata}},
#'  Type: \code{\link{assert_subset}}.
#'
#' @return
#'  Thermal conductivity of insulation materials at given
#'  set of temperatures, [\emph{W/m/°C}], [\emph{W/m/K}].
#'  Type: \code{\link{assert_double}}.
#'
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#' # Averaged thermal conductivity of pipe insulation at 110 °C
#' print(m278insdata)
#' mean(m278inshcm(110, m278insdata[["material"]]))
#' # [1] 0.09033974  # [\emph{W/m/°C}]
#'
m278inshcm <- function(temperature = 110, material = "aerocrete"){
    checkmate::assert_double(
      temperature, lower = 0, upper = 450, finite = TRUE,
      any.missing = FALSE, min.len = 1L
    )
    norms <- pipenostics::m278insdata
    checkmate::assert_subset(material, choices = norms[["material"]])
    checkmate::assert_true(commensurable(c(
      length(temperature), length(material)
    )))

    cf <- merge(
      data.frame(
        idm = seq.int(length(material)),
        material = material
      ),
      norms,
      all.x = TRUE, by = "material", sort = FALSE
    )
    rm(norms)
    checkmate::assert_true(!is.null(cf[["material"]]))
    checkmate::assert_true(!is.null(cf[["idm"]]))
    rank <- order(cf[["idm"]])
    checkmate::assert_true(all(material == cf[rank, "material"]))

    1e-3*cf[rank, "lambda"] + 1e-6*cf[rank, "k"] * .5 * (temperature + 40.0)
}
