#' Set of 15 CIE Standard Skies
#'
#' @description
#' The Commission Internationale de l’Éclairage (CIE; International Commission
#' on Illumination) standard \insertCite{CIE2004}{rcaiman} defines 15
#' pre-calibrated sky luminance distributions, each described by a pair of
#' analytical functions, the **gradation function**
#' \eqn{\Phi(\theta) = 1 + a \cdot \exp\left(\frac{b}{\cos\theta}\right)},
#' and the **indicatrix function**
#' \eqn{f(\chi) = 1 + c \cdot \left[ \exp(d \cdot \chi) - \exp\left(d \cdot \frac{\pi}{2}\right) \right] + e \cdot \cos^2\chi}.
#' Combined, they can predict the relative radiance \eqn{\rho_R} in any sky
#' direction \eqn{(\theta, \phi)} as:
#' \eqn{
#' \hat{\rho_R}^{\circ}(\theta, \phi) =
#' \frac{\Phi(\theta) \cdot f(\chi(\theta, \phi; \theta_\odot, \phi_\odot))}
#' {\Phi(0) \cdot f(\chi(\theta, \phi; 0, 0))}
#' }, where \eqn{\theta} is the zenith angle, \eqn{\phi} is the azimuth angle,
#' and \eqn{\theta_\odot, \phi_\odot} are the zenith and azimuth of the sun disk.
#'
#' @format `data.frame` with 15 rows and 8 columns:
#' \describe{
#'   \item{`a`}{gradation function parameter.}
#'   \item{`b`}{gradation function parameter.}
#'   \item{`c`}{indicatrix function parameter.}
#'   \item{`d`}{indicatrix function parameter.}
#'   \item{`e`}{indicatrix function parameter.}
#'   \item{`indicatrix_group`}{factor with six categories and numerical tags.}
#'   \item{`general_sky_type`}{factor with three categories: `"Overcast"`,
#'     `"Clear"`, and `"Partly cloudy"`.}
#'   \item{`description`}{user-friendly description of the sky type.}
#' }
#'
#' @source \insertCite{Li2016;textual}{rcaiman}
#'
#' @references \insertAllCited{}
#'
"cie_table"
