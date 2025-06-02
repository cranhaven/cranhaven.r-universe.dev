
#' A function rather aimed at developers
#' @noRd

HilbertWithFT = function(absorptionIn){

  ### get time domain
  at=stats::fft(absorptionIn)
  mm=sqrt(Re(at)**2+Im(at)**2)
  at[1]=complex(real=max(mm),imaginary=0)
  ### now, change all signs of imaginary
  at=complex(real=Re(at), imaginary=-Im(at))
  #### now, get freq data
  af=stats::fft(at)
  ### a complex vector containing both absorption and dispersion,
  ### to get the same scale as input
  af = af/length(absorptionIn)

  return(af)
}
