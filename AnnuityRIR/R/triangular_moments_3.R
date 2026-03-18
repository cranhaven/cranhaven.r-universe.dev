triangular_moments_3=function(data,order){



  x=data
  r=order


  # moda

  Mode <- function(x, na.rm = FALSE) {
    if (na.rm) {
      x = x[!is.na(x)]
    }

    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
  }


# vc triangolare

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


  a=fCvM$estimate[1]
  b=fCvM$estimate[3]
  c=fCvM$estimate[2]

  a=as.numeric(a)
  b=as.numeric(b)
  c=as.numeric(c)

  mom=2*((b-c)/(a^(r-2))-(b-a)/(c^(r-2))+(c-a)/(b^(r-2)))/((r-1)*(r-2)*(b-a)*(c-a)*(b-c))

  return(mom)

}
