# @title
#  Modified ground temperature double harmonic model (GTDH-model)
#
# @family utils
#
# @description
#  Calculate ground temperature with \emph{Modified Ground Temperature Double Harmonic Model}
# (MGTDH-model). This is the internal function that is used inside interface-specific package
# functions. 
#
# @param x
#   Time after year beginning, [hour].
#
# @param depth
#   Depth where temperature value is calculated, [m].
#
# @param avg
#   Average year temperature of ground, [°C]. 
#
# @param ampl1
#   First harmonic amplitude coefficient, [°C].
#
# @param ampl2
#   Second harmonic amplitude coefficient, [°C].
#
# @param pl1
#   Period correction coefficient 1, [].
#
# @param pl2
#   Period correction coefficient 2, [].
#
# @param diffusivity
#   Diffusivity of soil, [mm^2/s].
#
# @return
#   Ground temperature for the given time and depth.
#
# @noRd
mgtdh <- function(
     x           =  1
    ,depth       =  1
    ,avg         =  4.1
    ,ampl1       = 12.1
    ,ampl2       =  1  
    ,pl1         = 31.0
    ,pl2         = 19.0
    ,diffusivity =   .490
    ){

  PI    = base::pi
  FREQ  = 1/8760  # [year/h]
  DAY   = 86400   # [s/day]
  SQRT2 = sqrt(2)
  BETA  = -1      # Model design factor - temperature shift
  pow   = .Primitive("^")

  # Calculation operation minimization
  root1 <- -1e3 * depth * sqrt(PI * FREQ/diffusivity/DAY)
  root2 <- -1 * root1 * SQRT2

  angle1 <- 2 * PI * FREQ * x
  angle2 <- 2 * angle1

  avg +
    BETA * exp(root1) * ampl1 * cos(angle1 + root1 - pl1) +
    BETA * pow(root2, ampl2 * depth) * cos(angle2 - root2 - pl2)
}
