triangular_parameters=function(data){


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

 print(summary(fCvM))
  plot(fCvM)

  # parametri della triangolare

param=c(fCvM$estimate[1], fCvM$estimate[3], fCvM$estimate[2])
return(param)

}








