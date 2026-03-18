triangular_parameters_U=function(data){


  # trovo la moda

  Mode <- function(x, na.rm = FALSE) {
    if (na.rm) {
      x = x[!is.na(x)]
    }
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
  }



  # fit della vc triangolare

  fCvM <-
    fitdistrplus::fitdist(
      data,
      "triang",
      method = "mge",
      start = list(
        min = min(data),
        mode = Mode(data),
        max = max(data)
      ),
      gof = "CvM"
    )


  # parametri della triangolare di U
  param=c(fCvM$estimate[1], fCvM$estimate[3], fCvM$estimate[2])

  a=1+param[1]/100
  b=1+param[3]/100
  c=1+param[2]/100

  return(c(a,b,c))

}








