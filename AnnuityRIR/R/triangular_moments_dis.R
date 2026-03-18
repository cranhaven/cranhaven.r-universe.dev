triangular_moments_dis=function(data,order){



  r=order


# trovo la moda

    Mode <- function(x, na.rm = FALSE) {
      if (na.rm) {
        x = x[!is.na(x)]
      }

      ux <- unique(x)
      return(ux[which.max(tabulate(match(x, ux)))])
    }




# fit vc triangolare

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


# parametri della triangolare

    a=fCvM$estimate[1]
    b=fCvM$estimate[3]
    c=fCvM$estimate[2]

    a=as.numeric(a)
    b=as.numeric(b)
    c=as.numeric(c)


#split the triangular distribution into 2 pieces

    f1=function(u){u^(-r)*2*(u-a)/((b-a)*(c-a))}
    f2=function(u){u^(-r)*2*(b-u)/((b-a)*(b-c))}


#compute 2 approssimate integrals for each part

# from lower point to the mode c

    i1=integrate(f1,a,c)

    # from the mode c to the highest value

    i2=integrate(f2,c,b)

    # the moment is the sum of the integrals

    mom=i1$value+i2$value

    return(mom)
  }


