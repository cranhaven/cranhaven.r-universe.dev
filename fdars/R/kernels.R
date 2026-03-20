#' Kernel Functions
#'
#' Symmetric, asymmetric, and integrated kernel functions for
#' nonparametric smoothing and density estimation.

# =============================================================================
# Symmetric Kernels (Ker.*)
# Support: [-1, 1] except Ker.norm which has unbounded support
# =============================================================================

#' Normal (Gaussian) Kernel
#'
#' @param u Numeric vector of evaluation points.
#' @return Kernel values at u.
#' @export
#' @examples
#' u <- seq(-3, 3, length.out = 100)
#' plot(u, Ker.norm(u), type = "l", main = "Normal Kernel")
Ker.norm <- function(u) {
  dnorm(u)
}

#' Epanechnikov Kernel
#'
#' @param u Numeric vector of evaluation points.
#' @return Kernel values at u (0 outside \[-1, 1\]).
#' @export
#' @examples
#' u <- seq(-1.5, 1.5, length.out = 100)
#' plot(u, Ker.epa(u), type = "l", main = "Epanechnikov Kernel")
Ker.epa <- function(u) {
  ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0)
}

#' Triweight Kernel
#'
#' @param u Numeric vector of evaluation points.
#' @return Kernel values at u (0 outside \[-1, 1\]).
#' @export
#' @examples
#' u <- seq(-1.5, 1.5, length.out = 100)
#' plot(u, Ker.tri(u), type = "l", main = "Triweight Kernel")
Ker.tri <- function(u) {
  ifelse(abs(u) <= 1, (35/32) * (1 - u^2)^3, 0)
}

#' Quartic (Biweight) Kernel
#'
#' @param u Numeric vector of evaluation points.
#' @return Kernel values at u (0 outside \[-1, 1\]).
#' @export
#' @examples
#' u <- seq(-1.5, 1.5, length.out = 100)
#' plot(u, Ker.quar(u), type = "l", main = "Quartic Kernel")
Ker.quar <- function(u) {
  ifelse(abs(u) <= 1, (15/16) * (1 - u^2)^2, 0)
}

#' Cosine Kernel
#'
#' @param u Numeric vector of evaluation points.
#' @return Kernel values at u (0 outside \[-1, 1\]).
#' @export
#' @examples
#' u <- seq(-1.5, 1.5, length.out = 100)
#' plot(u, Ker.cos(u), type = "l", main = "Cosine Kernel")
Ker.cos <- function(u) {
  ifelse(abs(u) <= 1, (pi/4) * cos(pi * u / 2), 0)
}

#' Uniform (Rectangular) Kernel
#'
#' @param u Numeric vector of evaluation points.
#' @return Kernel values at u (0 outside \[-1, 1\]).
#' @export
#' @examples
#' u <- seq(-1.5, 1.5, length.out = 100)
#' plot(u, Ker.unif(u), type = "l", main = "Uniform Kernel")
Ker.unif <- function(u) {
  ifelse(abs(u) <= 1, 0.5, 0)
}

# =============================================================================
# Asymmetric Kernels (AKer.*)
# Support: [0, 1] or [0, Inf) - one-sided kernels
# =============================================================================

#' Asymmetric Normal Kernel
#'
#' @param u Numeric vector of evaluation points.
#' @return Kernel values at u (0 for u < 0).
#' @export
#' @examples
#' u <- seq(-0.5, 3, length.out = 100)
#' plot(u, AKer.norm(u), type = "l", main = "Asymmetric Normal Kernel")
AKer.norm <- function(u) {
  ifelse(u >= 0, 2 * dnorm(u), 0)
}

#' Asymmetric Epanechnikov Kernel
#'
#' @param u Numeric vector of evaluation points.
#' @return Kernel values at u (0 outside \[0, 1\]).
#' @export
#' @examples
#' u <- seq(-0.5, 1.5, length.out = 100)
#' plot(u, AKer.epa(u), type = "l", main = "Asymmetric Epanechnikov Kernel")
AKer.epa <- function(u) {
  ifelse(u >= 0 & u <= 1, 1.5 * (1 - u^2), 0)
}

#' Asymmetric Triweight Kernel
#'
#' @param u Numeric vector of evaluation points.
#' @return Kernel values at u (0 outside \[0, 1\]).
#' @export
#' @examples
#' u <- seq(-0.5, 1.5, length.out = 100)
#' plot(u, AKer.tri(u), type = "l", main = "Asymmetric Triweight Kernel")
AKer.tri <- function(u) {
  ifelse(u >= 0 & u <= 1, (35/16) * (1 - u^2)^3, 0)
}

#' Asymmetric Quartic Kernel
#'
#' @param u Numeric vector of evaluation points.
#' @return Kernel values at u (0 outside \[0, 1\]).
#' @export
#' @examples
#' u <- seq(-0.5, 1.5, length.out = 100)
#' plot(u, AKer.quar(u), type = "l", main = "Asymmetric Quartic Kernel")
AKer.quar <- function(u) {
  ifelse(u >= 0 & u <= 1, (15/8) * (1 - u^2)^2, 0)
}

#' Asymmetric Cosine Kernel
#'
#' @param u Numeric vector of evaluation points.
#' @return Kernel values at u (0 for u < 0).
#' @export
#' @examples
#' u <- seq(-0.5, 1.5, length.out = 100)
#' plot(u, AKer.cos(u), type = "l", main = "Asymmetric Cosine Kernel")
AKer.cos <- function(u) {
  ifelse(u >= 0 & u <= 1, (pi/2) * cos(pi * u / 2), 0)
}

#' Asymmetric Uniform Kernel
#'
#' @param u Numeric vector of evaluation points.
#' @return Kernel values at u (0 outside \[0, 1\]).
#' @export
#' @examples
#' u <- seq(-0.5, 1.5, length.out = 100)
#' plot(u, AKer.unif(u), type = "l", main = "Asymmetric Uniform Kernel")
AKer.unif <- function(u) {
  ifelse(u >= 0 & u <= 1, 1, 0)
}

# =============================================================================
# Integrated Kernels (IKer.*)
# Cumulative integrals from -1 (or 0 for asymmetric) to u
# =============================================================================

#' Integrated Normal Kernel
#'
#' @param u Numeric vector of evaluation points.
#' @return Cumulative integral of normal kernel from -Inf to u.
#' @export
#' @examples
#' u <- seq(-3, 3, length.out = 100)
#' plot(u, IKer.norm(u), type = "l", main = "Integrated Normal Kernel")
IKer.norm <- function(u) {
  pnorm(u)
}

#' Integrated Epanechnikov Kernel
#'
#' Integral of Ker.epa from -1 to u.
#'
#' @param u Numeric vector of evaluation points.
#' @return Cumulative integral values.
#' @export
#' @examples
#' u <- seq(-1.5, 1.5, length.out = 100)
#' plot(u, IKer.epa(u), type = "l", main = "Integrated Epanechnikov Kernel")
IKer.epa <- function(u) {
  # Analytical integral: 0.75 * (u - u^3/3) evaluated from -1 to u
  # At -1: 0.75 * (-1 - (-1/3)) = 0.75 * (-2/3) = -0.5
  # At u: 0.75 * (u - u^3/3)
  # Integral = 0.75 * (u - u^3/3) - (-0.5) = 0.75 * (u - u^3/3) + 0.5
  ifelse(u < -1, 0,
         ifelse(u > 1, 1,
                0.5 + 0.75 * (u - u^3/3)))
}

#' Integrated Triweight Kernel
#'
#' Integral of Ker.tri from -1 to u.
#'
#' @param u Numeric vector of evaluation points.
#' @return Cumulative integral values.
#' @export
#' @examples
#' u <- seq(-1.5, 1.5, length.out = 100)
#' plot(u, IKer.tri(u), type = "l", main = "Integrated Triweight Kernel")
IKer.tri <- function(u) {
  # Integral of (35/32)(1-u^2)^3 from -1 to u
  # (35/32) * [u - u^3 + (3/5)u^5 - (1/7)u^7]
  # At -1: (35/32) * [-1 + 1 - 3/5 + 1/7] = (35/32) * (-16/35) = -0.5
  ifelse(u < -1, 0,
         ifelse(u > 1, 1,
                0.5 + (35/32) * (u - u^3 + 0.6*u^5 - u^7/7)))
}

#' Integrated Quartic Kernel
#'
#' Integral of Ker.quar from -1 to u.
#'
#' @param u Numeric vector of evaluation points.
#' @return Cumulative integral values.
#' @export
#' @examples
#' u <- seq(-1.5, 1.5, length.out = 100)
#' plot(u, IKer.quar(u), type = "l", main = "Integrated Quartic Kernel")
IKer.quar <- function(u) {
  # Integral of (15/16)(1-u^2)^2 from -1 to u
  # (15/16) * [u - (2/3)u^3 + (1/5)u^5]
  # At -1: (15/16) * [-1 + 2/3 - 1/5] = (15/16) * (-8/15) = -0.5
  ifelse(u < -1, 0,
         ifelse(u > 1, 1,
                0.5 + (15/16) * (u - (2/3)*u^3 + 0.2*u^5)))
}

#' Integrated Cosine Kernel
#'
#' Integral of Ker.cos from -1 to u.
#'
#' @param u Numeric vector of evaluation points.
#' @return Cumulative integral values.
#' @export
#' @examples
#' u <- seq(-1.5, 1.5, length.out = 100)
#' plot(u, IKer.cos(u), type = "l", main = "Integrated Cosine Kernel")
IKer.cos <- function(u) {
  # Integral of (pi/4)*cos(pi*u/2) from -1 to u
  # = (pi/4) * (2/pi) * sin(pi*u/2) = 0.5 * sin(pi*u/2)
  # At -1: 0.5 * sin(-pi/2) = -0.5
  ifelse(u < -1, 0,
         ifelse(u > 1, 1,
                0.5 + 0.5 * sin(pi * u / 2)))
}

#' Integrated Uniform Kernel
#'
#' Integral of Ker.unif from -1 to u.
#'
#' @param u Numeric vector of evaluation points.
#' @return Cumulative integral values.
#' @export
#' @examples
#' u <- seq(-1.5, 1.5, length.out = 100)
#' plot(u, IKer.unif(u), type = "l", main = "Integrated Uniform Kernel")
IKer.unif <- function(u) {
  # Integral of 0.5 from -1 to u = 0.5 * (u - (-1)) = 0.5 * (u + 1)
  ifelse(u < -1, 0,
         ifelse(u > 1, 1,
                0.5 * (u + 1)))
}

# =============================================================================
# Unified Kernel Interfaces
# =============================================================================

#' Unified Symmetric Kernel Interface
#'
#' Evaluates a symmetric kernel function by name.
#'
#' @param u Numeric vector of evaluation points.
#' @param type.Ker Kernel type: "norm", "epa", "tri", "quar", "cos", or "unif".
#' @return Kernel values at u.
#' @export
#' @examples
#' u <- seq(-1.5, 1.5, length.out = 100)
#' plot(u, Kernel(u, "epa"), type = "l")
#' lines(u, Kernel(u, "norm"), col = "red")
Kernel <- function(u, type.Ker = "norm") {
  type.Ker <- match.arg(type.Ker, c("norm", "epa", "tri", "quar", "cos", "unif"))

  switch(type.Ker,
         "norm" = Ker.norm(u),
         "epa" = Ker.epa(u),
         "tri" = Ker.tri(u),
         "quar" = Ker.quar(u),
         "cos" = Ker.cos(u),
         "unif" = Ker.unif(u))
}

#' Unified Asymmetric Kernel Interface
#'
#' Evaluates an asymmetric kernel function by name.
#'
#' @param u Numeric vector of evaluation points.
#' @param type.Ker Kernel type: "norm", "epa", "tri", "quar", "cos", or "unif".
#' @return Kernel values at u.
#' @export
#' @examples
#' u <- seq(-0.5, 1.5, length.out = 100)
#' plot(u, Kernel.asymmetric(u, "epa"), type = "l")
Kernel.asymmetric <- function(u, type.Ker = "norm") {
  type.Ker <- match.arg(type.Ker, c("norm", "epa", "tri", "quar", "cos", "unif"))

  switch(type.Ker,
         "norm" = AKer.norm(u),
         "epa" = AKer.epa(u),
         "tri" = AKer.tri(u),
         "quar" = AKer.quar(u),
         "cos" = AKer.cos(u),
         "unif" = AKer.unif(u))
}

#' Unified Integrated Kernel Interface
#'
#' Evaluates an integrated kernel function by name.
#'
#' @param u Numeric vector of evaluation points.
#' @param Ker Kernel type: "norm", "epa", "tri", "quar", "cos", or "unif".
#' @param a Lower integration bound (default -1 for symmetric kernels).
#'   Not currently used (always integrates from -1 or -Inf).
#' @return Cumulative integral values.
#' @export
#' @examples
#' u <- seq(-1.5, 1.5, length.out = 100)
#' plot(u, Kernel.integrate(u, "epa"), type = "l")
Kernel.integrate <- function(u, Ker = "norm", a = -1) {
  Ker <- match.arg(Ker, c("norm", "epa", "tri", "quar", "cos", "unif"))

  switch(Ker,
         "norm" = IKer.norm(u),
         "epa" = IKer.epa(u),
         "tri" = IKer.tri(u),
         "quar" = IKer.quar(u),
         "cos" = IKer.cos(u),
         "unif" = IKer.unif(u))
}
