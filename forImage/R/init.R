# Geometric models functions
sphere <- function(d_one) { vol <- (pi * (d_one^3))/6 }

half_sphere <- function(d_one) { vol <- (pi * (d_one^3))/12 }

spheroid <- function(h, d_one) { vol <- (pi * h * (d_one^2))/6 }

cone <- function(h, d_one) { vol <- (pi * h * (d_one^2))/12 }

paraboloid <- function(h, d_one) {

# Call:
# lm(formula = z$h1 ~ z$h2)
#
# Residuals:
# Min       1Q   Median       3Q      Max
# -18.3820  -1.3572  -0.8581   2.1366  15.1083
#
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.83515    0.35039   2.383   0.0173 *
# z$h2         0.50088    0.00933  53.685   <2e-16 ***
#
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 3.722 on 1020 degrees of freedom
# Multiple R-squared:  0.7386,	Adjusted R-squared:  0.7383
# F-statistic:  2882 on 1 and 1020 DF,  p-value: < 2.2e-16

  hx <- (0.50088*h) + 0.83515
  vol1 <- (pi * h * (d_one^2))/8
  vol2  <- (pi * hx * (d_one^2))/8
  vol  <- vol1 - vol2
  }

dome <- function(h, d_one) { vol <- (pi * h * (4 * (h^2) + 3 * (d_one^2)))/24 }

cylinder <- function(h, d_one) { vol <- (pi * h * (d_one^2))/4 }

ellipsoid <- function(h, d_one, d_two) { vol <- (pi * h * d_one * d_two)/6 }

elliptic_cone <- function(h, d_one, d_two) { vol <- (pi * h * d_one * d_two)/12 }

c_half_ellipsoid <- function(h, d_one, d_two) { vol <- (pi * h * d_one * d_two)/12 }

gomphonemoid <- function(h, d_one, d_two) {vol <- ((d_one * d_two)/4) * (d_one + ((pi/4) - 1) * d_two) * asin(h/(2*d_one))}

elliptic_prism <- function(h, d_one, d_two) { vol <- (pi * h * d_one * d_two)/4 }

half_elliptic_prism <- function(h, d_one, d_two) { vol <- (pi * h * d_one * d_two)/4 }

dypyramid <- function(h, length, width) { vol <- (1 * ((length * width)/2) * h)/3 }

axh <- function(area, h) { vol <- area * h}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c(".", "d_one", "d_two", "h", "width", "area", "vol", "utilities", "measure_dim"))
}
