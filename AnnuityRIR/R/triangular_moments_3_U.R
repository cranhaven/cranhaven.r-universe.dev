triangular_moments_3_U=function(data,order){


  
  r=order

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

  a=as.numeric(a)
  b=as.numeric(b)
  c=as.numeric(c)


  mom=2*((b-c)/(a^(r-2))-(b-a)/(c^(r-2))+(c-a)/(b^(r-2)))/((r-1)*(r-2)*(b-a)*(c-a)*(b-c))

  return(mom)

}
