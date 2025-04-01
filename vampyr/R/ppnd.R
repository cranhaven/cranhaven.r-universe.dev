ppnd <- function(P){

  ## PPND produces the normal deviate value corresponding to lower tail area = P.
  #
  #  Licensing:
  #
  #    This code is distributed under the GNU LGPL license.
  #
  #  Modified:
  #
  #  7 November 2017
  #
  #  Author:
  #
  #    Original FORTRAN77 version by J Beasley, S Springer.
  #    R version by David Navarro-Gonzalez.
  #
  #  Reference:
  #
  #    J Beasley, S Springer,
  #    Algorithm AS 111:
  #    The Percentage Points of the Normal Distribution,
  #    Applied Statistics,
  #    Volume 26, Number 1, 1977, pages 118-121.
  #
  #  Parameters:
  #
  #    Input, real P, the value of the cumulative probability
  #    densitity function.  0 < P < 1.
  #
  #    Output, real VALUE, the normal deviate value with the property that
  #    the probability of a standard normal deviate being less than or
  #    equal to PPND is P.

  SPLIT=0.42
  A0 =  2.50662823884
  A1 =-18.61500062529
  A2 = 41.39119773534
  A3 =-25.44106049637
  B1 = -8.47351093090
  B2 = 23.08336743743
  B3 =-21.06224101826
  B4 =  3.13082909833
  C0 = -2.78718931138
  C1 = -2.29796479134
  C2 =  4.85014127135
  C3 =  2.32121276858
  D1 = 3.54388924762
  D2 = 1.63706781897
  ZERO = 0.0
  ONE  = 1.0
  HALF = 0.5

  IER = 0
  Q = P-HALF

  if (abs(Q) <= SPLIT){
    R <- Q*Q
    PPND <- Q*(((A3*R + A2)*R +A1)*R + A0)/((((B4*R + B3)*R + B2)*R +B1)*R+ ONE)
    return(PPND)
  }

  R <- P
  if (Q > ZERO){
    R <- ONE-P
  }

  if (R > ZERO){
    R <- sqrt(-log(R))
    PPND <- (((C3*R + C2)*R + C1)*R + C0)/((D2*R + D1)*R + ONE)
    if (Q < ZERO){
      PPND <- -PPND
    }
    return(PPND)
  }

  PPND = ZERO
  return(PPND)

}
