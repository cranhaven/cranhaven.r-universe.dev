#' Adjust the wavelength solution of a spectrum
#'
#' This function takes the wavelength and flux vectors of a normalized spectrum
#' and uses cubic-spline interpolation to adjust the flux vector to match a new
#' wavelength solution.
#'
#' @param wvl1 vector of wavelengths for the spectrum to be interpolated
#' @param flx1 vector of normalized flux for the spectrum to be interpolated
#' @param targetwvl vector of wavelengths to interpolate to.
#' @return A vector of normalized flux for the spectrum at the targetwvl wavelengths. Only flux for targetwvl wavelengths that are contained by the wvl1 wavelengths are returned.
#' @examples x = seq(0,10)
#' y = 5*sin(x + 2)
#' newx = seq(0.5, 9.5)
#' newy = wave_match(x, y, newx)
#' plot(x, y)
#' points(newx, newy, col=2, pch=19)
#' @export
wave_match = function(wvl1, flx1, targetwvl){
  wvlrng = which((targetwvl >= min(wvl1)) & (targetwvl <= max(wvl1)))
  f = splinefun(wvl1, flx1, method = 'natural')
  return(f(targetwvl[wvlrng]))
}
