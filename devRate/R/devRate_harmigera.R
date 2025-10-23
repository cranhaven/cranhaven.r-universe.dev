#' Bartekova and Praslicka linear thermal performance curve for the
#'   developmentof Helicoverpa armigera
#'
#' @description Linear development performance curve for eggs, larvae and
#'   pupae from three experimental temperatures (20, 25, and 30 degrees
#'   Celsius).
#' @seealso Bartekova, A., and Praslicka, J. (2006). The effect of ambient
#'   temperature on the development of cotton bollworm (Helicoverpa armigera
#'   Hubner, 1808). Plant Protection Science, 42(4), 135.
#'   https://doi.org/10.17221/2768-PPS
#' @details This work is part of the ACOMPLI project. The ACOMPLI project
#'   is part of the Strategic Action Plan for the anticipation of the potential
#'   European withdrawal of active substances and the development of
#'   alternative crop protection techniques (PARSADA). It is financed by
#'   ecological planning funds. The French Ministry of Agriculture cannot be
#'   held responsible for the content of this package.
#' @param plotfig A Boolean used to return the experimental points and the
#'   equation fitted in the article.
#' @return A list with the equation used, and a list of model parameters for
#'   the different life stages considered in the article.
#' @examples
#'   mymodel <- ha_bartekova2006(plotfig = FALSE)
#' @export
ha_bartekova2006 <- function(plotfig = TRUE){
  LDT <- c(14.8, 11.3, 8.2, 11.5)
  SET <- c(64.1, 344.8, 222.2, 625.0)
  bb <- 1/SET
  aa <- -LDT*bb
  if(plotfig){
    temp <- c(5, 20, 25, 30)
    DR_egg <- aa[1] + bb[1]*temp
    DR_larva <- aa[2] + bb[2]*temp
    DR_pupa <- aa[3] + bb[3]*temp
    graphics::plot(
      x = temp, y = DR_egg, ylim = c(0, 0.25), type = "o", pch = 15,
      main = "Figure 1 ; DOI: 10.17221/2768-PPS ; Bartekova et al. 2006",
      xlab = "Ambient temperature (Celsius)",
      ylab = "Development rate (1/DT)",
      xlim = c(0, 40)
    )
    graphics::points(x = temp, y = DR_larva, type = "o", pch = 16)
    graphics::points(x = temp, y = DR_pupa, type = "o", pch = 17)
    graphics::legend(
      "topleft", pch = c(15, 16, 17), legend = c("egg", "larva", "pupa"),
      bty = "n"
    )
  }
  dr_egg <- list(aa = aa[1], bb = bb[1])
  dr_larva <- list(aa = aa[2], bb = bb[2])
  dr_pupa <- list(aa = aa[3], bb = bb[3])
  return(list(
    equation = list(egg = "campbell_74", larva = "campbell_74", pupa = "campbell_74"),
    model = list(egg = dr_egg, larva = dr_larva, pupa = dr_pupa)
  ))
}

#' Jallow and Matsumura linear thermal performance curve for the
#'   development of Helicoverpa armigera
#'
#' @description Linear development performance curve for eggs, larvae and
#'   pupae from nine experimental temperatures (10, 13.3, 16.4, 20, 22.5, 25,
#'   27.9, 30.5, and 32.5 degrees Celsius).
#' @seealso Jallow, M. F., and Matsumura, M. (2001). Influence of temperature
#'   on the rate of development of Helicoverpa armigera (Hubner)(Lepidoptera:
#'   Noctuidae). Applied Entomology and Zoology, 36(4), 427-430.
#'   https://doi.org/10.1303/aez.2001.427
#' @details This work is part of the ACOMPLI project. The ACOMPLI project
#'   is part of the Strategic Action Plan for the anticipation of the potential
#'   European withdrawal of active substances and the development of
#'   alternative crop protection techniques (PARSADA). It is financed by
#'   ecological planning funds. The French Ministry of Agriculture cannot be
#'   held responsible for the content of this package.
#' @param plotfig A Boolean used to return the experimental points and the
#'   equation fitted in the article.
#' @return A list with the equation used, and a list of model parameters for
#'   the different life stages considered in the article.
#' @examples
#'   mymodel <- ha_jallow2001(plotfig = FALSE)
#' @export
ha_jallow2001 <- function(plotfig = TRUE){
  if(plotfig){
    temp <- c(10, 13.3, 16.4, 20, 22.5, 25, 27.9, 30.5, 32.5)
    DR_egg <- -0.21+0.02*temp
    DR_larva <- -0.045+0.004*temp
    DR_pupa <- -0.083+0.006*temp
    graphics::plot(
      x = temp, y = DR_egg, ylim = c(0, 0.4), type = "o", pch = 15,
      main = "Figure 1 ; DOI: 10.1303/aez.2001.427 ; Jallow et al. 2001",
      xlab = "Ambient temperature (Celsius)",
      ylab = "Development rate (1/DT)",
      xlim = c(0, 40)
    )
    graphics::points(x = temp, y = DR_larva, type = "o", pch = 16)
    graphics::points(x = temp, y = DR_pupa, type = "o", pch = 17)
    graphics::legend(
      "topleft", pch = c(15:17), legend = c("egg", "larva", "pupa"),
      bty = "n"
    )
  }
  dr_egg <- list(aa = -0.21, bb = 0.02)
  dr_larva <- list(aa = -0.045, bb = 0.004)
  dr_pupa <- list(aa = -0.083, bb = 0.006)
  return(list(
    equation = list(egg = "campbell_74", larva = "campbell_74", pupa = "campbell_74"),
    model = list(egg = dr_egg, larva = dr_larva, pupa = dr_pupa)
  ))
}

#' Mironidis and Savopoulou-Soultani linear thermal performance curve for the
#'   development of Helicoverpa armigera
#'
#' @description Linear development performance curve for eggs, larvae and
#'   pupae from twelve experimental temperatures (10, 12.5, 15, 17.5, 20,
#'   25, 27.5, 30, 32.5, 35, 37.5, and 40 degrees Celsius).
#' @seealso Mironidis, G. K., and Savopoulou-Soultani, M. (2008). Development,
#'   survivorship, and reproduction of Helicoverpa armigera (Lepidoptera:
#'   Noctuidae) under constant and alternating temperatures. Environmental
#'   Entomology, 37(1), 16-28. https://doi.org/10.1093/ee/37.1.16
#' @details This work is part of the ACOMPLI project. The ACOMPLI project
#'   is part of the Strategic Action Plan for the anticipation of the potential
#'   European withdrawal of active substances and the development of
#'   alternative crop protection techniques (PARSADA). It is financed by
#'   ecological planning funds. The French Ministry of Agriculture cannot be
#'   held responsible for the content of this package.
#' @param plotfig A Boolean used to return the experimental points and the
#'   equation fitted in the article.
#' @return A list with the equation used, and a list of model parameters for
#'   the different life stages considered in the article.
#' @examples
#'   mymodel <- ha_mironidis2008_ls(plotfig = FALSE)
#' @export
ha_mironidis2008_ls <- function(plotfig = TRUE){
  if(plotfig){
    temp <- c(10, 12.5, 15, 17.5, 20, 25, 27.5, 30, 32.5, 35, 37.5, 40)
    DR_egg <- -0.3013+0.0252*temp
    DR_larva <- -0.0442+0.0042*temp
    DR_pupa <- -0.0529+0.0052*temp
    graphics::plot(
      x = temp, y = DR_egg, ylim = c(0, 0.4), type = "o", pch = 15,
      main = "Figure X ; DOI: 10.1093/ee/37.1.16 ; Mironidis et al. 2008",
      xlab = "Ambient temperature (Celsius)",
      ylab = "Development rate (1/DT)",
      xlim = c(0, 40)
    )
    graphics::points(x = temp, y = DR_larva, type = "o", pch = 16)
    graphics::points(x = temp, y = DR_pupa, type = "o", pch = 17)
    graphics::legend(
      "topleft", pch = c(15:17), legend = c("egg", "larva", "pupa"),
      bty = "n"
    )
  }
  dr_egg <- list(aa = -0.3013, bb = 0.0252)
  dr_larva <- list(aa = -0.0442, bb = 0.0042)
  dr_pupa <- list(aa = -0.0529, bb = 0.0052)
  return(list(
    equation = list(egg = "campbell_74", larva = "campbell_74", pupa = "campbell_74"),
    model = list(egg = dr_egg, larva = dr_larva, pupa = dr_pupa)
  ))
}

#' Mironidis and Savopoulou-Soultani non-linear Lactin-2 thermal performance
#'   curve for the development of Helicoverpa armigera
#'
#' @description Non-linear development performance curve for eggs, larvae and
#'   pupae from twelve experimental temperatures (10, 12.5, 15, 17.5, 20,
#'   25, 27.5, 30, 32.5, 35, 37.5, and 40 degrees Celsius), using Lactin2
#'   model (Lactin, 1995).
#' @seealso Mironidis, G. K., and Savopoulou-Soultani, M. (2008). Development,
#'   survivorship, and reproduction of Helicoverpa armigera (Lepidoptera:
#'   Noctuidae) under constant and alternating temperatures. Environmental
#'   Entomology, 37(1), 16-28. https://doi.org/10.1093/ee/37.1.16
#' @details This work is part of the ACOMPLI project. The ACOMPLI project
#'   is part of the Strategic Action Plan for the anticipation of the potential
#'   European withdrawal of active substances and the development of
#'   alternative crop protection techniques (PARSADA). It is financed by
#'   ecological planning funds. The French Ministry of Agriculture cannot be
#'   held responsible for the content of this package.
#' @param plotfig A Boolean used to return the experimental points and the
#'   equation fitted in the article.
#' @return A list with the equation used, and a list of model parameters for
#'   the different life stages considered in the article.
#' @examples
#'   mymodel <- ha_mironidis2008_nls(plotfig = FALSE)
#' @export
ha_mironidis2008_nls <- function(plotfig = TRUE){
  if(plotfig){
    temp <- c(10, 12.5, 15, 17.5, 20, 25, 27.5, 30, 32.5, 35, 37.5, 40)
    temp2 <- 5:40
    aa <- c(0.0165, 0.0042, 0.0053)
    Tmax <- c(42.0240, 43.1521, 43.3886)
    deltaT <- c(2.0503, 1.8197, 1.6854)
    bb <- c(-1.1906, -1.0480, -1.0674)
    DR_egg2 <- exp(aa[1] * temp2) - exp(aa[1] * Tmax[1] - (Tmax[1] - temp2)/deltaT[1]) + bb[1]
    DR_larva2 <- exp(aa[2] * temp2) - exp(aa[2] * Tmax[2] - (Tmax[2] - temp2)/deltaT[2]) + bb[2]
    DR_pupa2 <- exp(aa[3] * temp2) - exp(aa[3] * Tmax[3] - (Tmax[3] - temp2)/deltaT[3]) + bb[3]
    DR_egg <- exp(aa[1] * temp) - exp(aa[1] * Tmax[1] - (Tmax[1] - temp)/deltaT[1]) + bb[1]
    DR_larva <- exp(aa[2] * temp) - exp(aa[2] * Tmax[2] - (Tmax[2] - temp)/deltaT[2]) + bb[2]
    DR_pupa <- exp(aa[3] * temp) - exp(aa[3] * Tmax[3] - (Tmax[3] - temp)/deltaT[3]) + bb[3]
    graphics::plot(
      x = temp2, y = DR_egg2, ylim = c(0, 0.6), type = "l", pch = 15,
      main = "Figure 2 ; DOI: 10.1093/ee/37.1.16 ; Mironidis et al. 2008",
      xlab = "Temperature (Celsius)",
      ylab = "Development rate (Day^(-1))",
      xlim = c(0, 40)
    )
    graphics::points(x = temp2, y = DR_larva2, type = "l", pch = 16)
    graphics::points(x = temp2, y = DR_pupa2, type = "l", pch = 17)
    graphics::points(x = temp, y = DR_egg, type = "p", pch = 15)
    graphics::points(x = temp, y = DR_larva, type = "p", pch = 16)
    graphics::points(x = temp, y = DR_pupa, type = "p", pch = 17)
    graphics::legend(
      "topleft", pch = c(15:17), legend = c("egg", "larva", "pupa"),
      bty = "n"
    )
  }
  dr_egg <- list(aa = 0.0165, Tmax = 42.0240, deltaT = 2.0503, bb = -1.1906)
  dr_larva <- list(aa = 0.0042, Tmax = 43.1521, deltaT = 1.8197, bb = -1.0480)
  dr_pupa <- list(aa = 0.0053, Tmax = 43.3886, deltaT = 1.6854, bb = -1.0674)
  return(list(
    equation = list(egg = "lactin2_95", larva = "lactin2_95", pupa = "lactin2_95"),
    model = list(egg = dr_egg, larva = dr_larva, pupa = dr_pupa)
  ))
}

#' Foley linear thermal performance curve for the post-diapausing and
#'   non-diapausing pupae development of Helicoverpa armigera
#'
#' @description Linear development performance curve for post-diapausing and
#'   non-diapausing pupae from 3 to 4 experimental temperatures (20, 24,
#'   28, and 32 degrees Celsius).
#' @seealso Foley, D. H. (1981). Pupal development rate of Heliothis armiger
#'   (Hubner)(Lepidoptera: Noctuidae) under constant and alternating
#'   temperatures. Australian Journal of Entomology, 20(1), 13-20.
#'   https://doi.org/10.1111/j.1440-6055.1981.tb00993.x
#' @details This work is part of the ACOMPLI project. The ACOMPLI project
#'   is part of the Strategic Action Plan for the anticipation of the potential
#'   European withdrawal of active substances and the development of
#'   alternative crop protection techniques (PARSADA). It is financed by
#'   ecological planning funds. The French Ministry of Agriculture cannot be
#'   held responsible for the content of this package.
#' @param plotfig A Boolean used to return the experimental points and the
#'   equation fitted in the article.
#' @return A list with the equation used, and a list of model parameters for
#'   the different life stages considered in the article.
#' @examples
#'   mymodel <- ha_foley1981(plotfig = FALSE)
#' @export
ha_foley1981 <- function(plotfig = TRUE){
  # Non-diapausing pupae ; %dev per day = -9.382 + 0.634X
  # Post-diapause pupae ; %dev per day = -14.022 + 0.857X
  if(plotfig){
    temp <- c(10, 20, 24, 28, 32)
    DR_pupa_nondiapause <- -0.09382+0.00634*temp
    DR_pupa_diapause <- -0.14022+0.00857*temp
    graphics::plot(
      x = temp, y = DR_pupa_diapause, ylim = c(0, 0.3), type = "o", pch = 15,
      main = "Figure 1&2 ; DOI: 0.1111/j.1440-6055.1981.tb00993.x ; Foley et al. 1981",
      xlab = "Temperature (Celsius)",
      ylab = "Development rate (day^-1)",
      xlim = c(0, 40)
    )
    graphics::points(x = temp, y = DR_pupa_nondiapause, type = "o", pch = 16)
    graphics::legend(
      "topleft", pch = c(15:16),
      legend = c("diapausing pupae", "non-diapausing pupae"),
      bty = "n"
    )
  }
  dr_pupa_diapause <- list(aa = -0.14022, bb = 0.00857)
  dr_pupa_nondiapause <- list(aa = -0.09382, bb = 0.00634)
  return(list(
    equation = list(diapausingpupae = "campbell_74", nondiapausingpupae = "campbell_74"),
    model = list(diapausingpupae = dr_pupa_diapause, nondiapausingpupae = dr_pupa_nondiapause)
  ))
}

#' Kay linear thermal performance curve for the egg development of
#'   Helicoverpa armigera
#'
#' @description Linear development performance curve for eggs from ten
#'   experimental temperatures (8, 10, 13.3, 17.8, 20.8, 24.4, 27.2, 31.4,
#'   35, 39.4 degrees Celsius).
#' @seealso Kay, I. R. (1981). The effect of constant temperatures on the
#'   development time of eggs of Heliothis armiger (Hubner) (Lepidoptera:
#'   Noctuidae). Australian Journal of Entomology, 20(2), 155-156.
#'   https://doi.org/10.1111/j.1440-6055.1981.tb01020.x
#' @details This work is part of the ACOMPLI project. The ACOMPLI project
#'   is part of the Strategic Action Plan for the anticipation of the potential
#'   European withdrawal of active substances and the development of
#'   alternative crop protection techniques (PARSADA). It is financed by
#'   ecological planning funds. The French Ministry of Agriculture cannot be
#'   held responsible for the content of this package.
#' @param plotfig A Boolean used to return the experimental points and the
#'   equation fitted in the article.
#' @return A list with the equation used, and a list of model parameters for
#'   the different life stages considered in the article.
#' @examples
#'   mymodel <- ha_kay1981_ls(plotfig = FALSE)
#' @export
ha_kay1981_ls	<- function(plotfig = TRUE){
  if(plotfig){
    temp <- c(8, 10, 13.3, 17.8, 20.8, 24.4, 27.2, 31.4, 35, 39.4)
    dev <- 1/(c(NA, NA, 443.5, 205.7, 121.3, 81.9, 59.9, 49.2, 48.8, NA)/24)
    graphics::plot(
      x = temp, y = dev, type = "p", pch = 19,
      main = "DOI: 10.1111/j.1440-6055.1981.tb01020.x ; Kay et al. 1981",
      xlab = "Temperature (Celsius)",
      ylab = "Development rate (day^-1)", ylim = c(0, 0.7),
      xlim = c(0, 40)
    )
    graphics::points(
      x = temp, y = 0.09621/100*24*temp - 1.128/100*24, type = "l"
    )
  }
  dr_eggs <- list(aa = -1.128/100*24, bb = 0.09621/100*24)
  return(list(
    equation = list(egg = "campbell_74"),
    model = list(egg = dr_eggs)
  ))
}

#' Kay non-linear Davidson thermal performance curve for the egg development of
#'   Helicoverpa armigera
#'
#' @description Non-linear development performance curve for eggs from ten
#'   experimental temperatures (8, 10, 13.3, 17.8, 20.8, 24.4, 27.2, 31.4,
#'   35, 39.4 degrees Celsius), using Davidson equation.
#' @seealso Kay, I. R. (1981). The effect of constant temperatures on the
#'   development time of eggs of Heliothis armiger (Hubner) (Lepidoptera:
#'   Noctuidae). Australian Journal of Entomology, 20(2), 155-156.
#'   https://doi.org/10.1111/j.1440-6055.1981.tb01020.x
#' @details This work is part of the ACOMPLI project. The ACOMPLI project
#'   is part of the Strategic Action Plan for the anticipation of the potential
#'   European withdrawal of active substances and the development of
#'   alternative crop protection techniques (PARSADA). It is financed by
#'   ecological planning funds. The French Ministry of Agriculture cannot be
#'   held responsible for the content of this package.
#' @param plotfig A Boolean used to return the experimental points and the
#'   equation fitted in the article.
#' @return A list with the equation used, and a list of model parameters for
#'   the different life stages considered in the article.
#' @examples
#'   mymodel <- ha_kay1981_nls(plotfig = FALSE)
#' @export
ha_kay1981_nls <- function(plotfig = TRUE){
  if(plotfig){
    temp <- c(8, 10, 13.3, 17.8, 20.8, 24.4, 27.2, 31.4, 35, 39.4)
    dev <- 1/(c(NA, NA, 443.5, 205.7, 121.3, 81.9, 59.9, 49.2, 48.8, NA)/24)
    graphics::plot(
      x = temp, y = dev, type = "p", pch = 19,
      main = "DOI: 10.1111/j.1440-6055.1981.tb01020.x ; Kay et al. 1981",
      xlab = "Temperature (Celsius)",
      ylab = "Development rate (day^-1)", ylim = c(0, 0.7),
      xlim = c(0, 40)
    )
    graphics::points(
      x = temp, y = (2.259/100*24) / (1 + exp(5.468-0.2350*temp)), type = "l"
    )
  }
  dr_eggs <- list(aa = 5.468, bb = -0.2350, K = 2.259/100*24)
  return(list(
    equation = list(egg = "davidson_44"),
    model = list(dr_eggs)
  ))
}

#' Noor-ul-Ane et al. linear thermal performance curve for the development of
#'   Helicoverpa armigera
#'
#' @description Linear development performance curve from ten
#'   experimental temperatures (10, 15, 17.5, 20, 25, 27.5, 30, 35, 37.5
#'   and 40 degrees Celsius). Experimental development data were retrieve from
#'   Figure 1 using plotdigitizer.com.
#' @seealso Noor-ul-Ane M., Mirhosseini M. A., Crickmore N., Saeed S.,
#'   Noor I., Zalucki M. P. (2018). Temperature-dependent development of
#'   Helicoverpa armigera (Hubner) (Lepidoptera: Noctuidae) and its larval
#'   parasitoid, Habrobracon hebetor (Say) (Hymenoptera: Braconidae):
#'   implications for species interactions. Bulletin of Entomological Research
#'   108, 295–304. https://doi.org/10.1017/S0007485317000724
#' @details This work is part of the ACOMPLI project. The ACOMPLI project
#'   is part of the Strategic Action Plan for the anticipation of the potential
#'   European withdrawal of active substances and the development of
#'   alternative crop protection techniques (PARSADA). It is financed by
#'   ecological planning funds. The French Ministry of Agriculture cannot be
#'   held responsible for the content of this package.
#' @param plotfig A Boolean used to return the experimental points and the
#'   equation fitted in the article.
#' @return A list with the equation used, and a list of model parameters for
#'   the different life stages considered in the article.
#' @examples
#'   mymodel <- ha_noorulane2018_ls(plotfig = FALSE)
#' @export
ha_noorulane2018_ls	<- function(plotfig = TRUE){
  if(plotfig){
    temp <-      c(10,     15, 17.5,   20,   25, 27.5,   30,   35, 37.5, 40)
    dev_egg <- 1/c(23.6, 15.6, 10.7,  8.4,  4.2,  3.6,  2.9,  2.3,  2.7, NA)
    dev_lar <- 1/c(NA,   70.2, 49.7, 25.3, 15.9, 15.1, 13.2, 10.8, 11.2, NA)
    dev_pup <- 1/c(NA,     NA, 31.8, 22.2, 12.5, 11.4, 10.7,  8.5, 11.6, NA)
    graphics::plot(
      x = temp, y = dev_egg, type = "n", pch = 19,
      main = "DOI: 10.1017/S0007485317000724 ; Noor-ul-Ane et al. 2018",
      xlab = "Temperature (Celsius)",
      ylab = "Development rate (day^-1)", ylim = c(0, 0.5),
      xlim = c(0, 40)
    )
    graphics::points(x = temp, y = -10.6*(1/57.8)+(1/57.8)*temp, type = "l", lty = 1)
    graphics::points(x = temp, y = -11.1*(1/247.7)+(1/247.7)*temp, type = "l", lty = 2)
    graphics::points(x = temp, y = -9.9*(1/213)+(1/213)*temp, type = "l", lty = 3)
    graphics::points(x = temp, y = dev_egg, pch = 15)
    graphics::points(x = temp, y = dev_lar, pch = 16)
    graphics::points(x = temp, y = dev_pup, pch = 17)
    graphics::legend(
      "topleft", pch = c(15:17), legend = c("egg", "larva", "pupa"),
      bty = "n", lty = 1:3
    )
  }
  dr_egg <- list(aa = -10.6*(1/57.8), bb = (1/57.8))
  dr_larva <- list(aa = -11.1*(1/247.7), bb = (1/247.7))
  dr_pupa <- list(aa = -9.9*(1/213), bb = (1/213))
  return(list(
    equation = list(egg = "campbell_74", larva = "campbell_74", pupa = "campbell_74"),
    model = list(egg = dr_egg, larva = dr_larva, pupa = dr_pupa)
  ))
}

#' Noor-ul-Ane et al. non-linear thermal performance curve for the development
#'   of Helicoverpa armigera
#'
#' @description Non-linear Briere2 development performance curve from ten
#'   experimental temperatures (10, 15, 17.5, 20, 25, 27.5, 30, 35, 37.5
#'   and 40 degrees Celsius). Experimental development data were retrieve from
#'   Figure 1 using plotdigitizer.com.
#' @seealso Noor-ul-Ane M., Mirhosseini M. A., Crickmore N., Saeed S.,
#'   Noor I., Zalucki M. P. (2018). Temperature-dependent development of
#'   Helicoverpa armigera (Hubner) (Lepidoptera: Noctuidae) and its larval
#'   parasitoid, Habrobracon hebetor (Say) (Hymenoptera: Braconidae):
#'   implications for species interactions. Bulletin of Entomological Research
#'   108, 295–304. https://doi.org/10.1017/S0007485317000724
#' @details This work is part of the ACOMPLI project. The ACOMPLI project
#'   is part of the Strategic Action Plan for the anticipation of the potential
#'   European withdrawal of active substances and the development of
#'   alternative crop protection techniques (PARSADA). It is financed by
#'   ecological planning funds. The French Ministry of Agriculture cannot be
#'   held responsible for the content of this package.
#' @param plotfig A Boolean used to return the experimental points and the
#'   equation fitted in the article.
#' @return A list with the equation used, and a list of model parameters for
#'   the different life stages considered in the article.
#' @examples
#'   mymodel <- ha_noorulane2018_nls(plotfig = FALSE)
#' @export
ha_noorulane2018_nls	<- function(plotfig = TRUE){
  if(plotfig){
    aa = 0.00003442
    Tmin = 8.205
    Tmax = 38.657
    bb = 3.910
    xx = seq(from = 5, to = 40, by = 0.1)
    graphics::plot(
      x = xx, y = aa * xx * (xx - Tmin) * (Tmax - xx)^(1 / bb), type = "l",
      main = "DOI: 10.1017/S0007485317000724 ; Noor-ul-Ane et al. 2018",
      xlab = "Temperature (Celsius)",
      ylab = "Development rate (day^-1)"
    )
    t_egg <- c(23.6, 15.6, 10.7,  8.4,  4.2,  3.6,  2.9,  2.3,  2.7, NA)
    t_lar <- c(NA,   70.2, 49.7, 25.3, 15.9, 15.1, 13.2, 10.8, 11.2, NA)
    t_pup <- c(NA,     NA, 31.8, 22.2, 12.5, 11.4, 10.7,  8.5, 11.6, NA)
    temp <-      c(10,     15, 17.5,   20,   25, 27.5,   30,   35, 37.5, 40)
    dev = 1/(t_egg+t_lar+t_pup)
    graphics::points(x = temp, y = dev, pch = 15)
  }
  dr_all <- list(aa = 0.00003442,
                 Tmin = 8.205,
                 Tmax = 38.657,
                 bb = 3.910)
  return(list(
    equation = list(all = "briere2_99"),
    model = list(all = dr_all)
  ))
}

#' Qureshi et al. linear thermal performance curve for the development of
#'   Helicoverpa armigera
#'
#' @description Linear development performance curve from four
#'   experimental temperatures (15, 20, 25,
#'   and 30 degrees Celsius).
#' @seealso Qureshi, M. H., T. Murai, H. Yoshida, T. Shiraga and H. Tsumuki
#'   (1999) Effects of photoperiod and temperature on development and diapause
#'   induction in the Okayama population of Helicoverpa armigera (Hb)
#'   (Lepidoptera: Noctuidae). Appl. Entomol. Zool. 34: 327–331.
#'   https://doi.org/10.1303/aez.34.327
#' @details This work is part of the ACOMPLI project. The ACOMPLI project
#'   is part of the Strategic Action Plan for the anticipation of the potential
#'   European withdrawal of active substances and the development of
#'   alternative crop protection techniques (PARSADA). It is financed by
#'   ecological planning funds. The French Ministry of Agriculture cannot be
#'   held responsible for the content of this package.
#' @param plotfig A Boolean used to return the experimental points and the
#'   equation fitted in the article.
#' @return A list with the equation used, and a list of model parameters for
#'   the different life stages considered in the article.
#' @examples
#'   mymodel <- ha_qureshi1999_ls(plotfig = FALSE)
#' @export
ha_qureshi1999_ls	<- function(plotfig = TRUE){
  if(plotfig){
    temp <-      c(15, 20, 25, 30)
    dev_egg <- -0.239 + 0.022*temp
    dev_lar <- -0.068 + 0.005*temp
    dev_pup <- -0.102 + 0.007*temp
    graphics::plot(
      x = temp, y = dev_egg, type = "n", pch = 19,
      main = "DOI: 10.1303/aez.34.327 ; Qureshi et al. 1999",
      xlab = "Temperature (Celsius)",
      ylab = "Development rate (day^-1)", ylim = c(0, 0.5),
      xlim = c(0, 40)
    )
    graphics::points(x = temp, y = dev_egg, type = "l", lty = 1)
    graphics::points(x = temp, y = dev_lar, type = "l", lty = 2)
    graphics::points(x = temp, y = dev_pup, type = "l", lty = 3)
    graphics::legend(
      "topleft", pch = c(15:17), legend = c("egg", "larva", "pupa"),
      bty = "n", lty = 1:3
    )
  }
  dr_egg <- list(aa = -0.239, bb = 0.022)
  dr_larva <- list(aa = -0.068, bb = 0.005)
  dr_pupa <- list(aa = -0.102, bb = 0.007)
  return(list(
    equation = list(egg = "campbell_74", larva = "campbell_74", pupa = "campbell_74"),
    model = list(egg = dr_egg, larva = dr_larva, pupa = dr_pupa)
  ))
}

#' Ahmad linear thermal performance curve for the development of
#'   Helicoverpa armigera
#'
#' @description Linear development performance curve from four
#'   experimental temperatures (14, 16, 18, 20, 22, 25, 27, 30, 32, 35 and 36
#'   degrees Celsius).
#' @seealso Ahmad S. (2024) Temperature dependent survivorship and development
#'   of Helicoverpa armigera (Hubner) (Lepidoptera-Noctuidae) on chickpea.
#'   Preprint. https://doi.org/10.21203/rs.3.rs-4845823/v1
#' @details This work is part of the ACOMPLI project. The ACOMPLI project
#'   is part of the Strategic Action Plan for the anticipation of the potential
#'   European withdrawal of active substances and the development of
#'   alternative crop protection techniques (PARSADA). It is financed by
#'   ecological planning funds. The French Ministry of Agriculture cannot be
#'   held responsible for the content of this package.
#' @param plotfig A Boolean used to return the experimental points and the
#'   equation fitted in the article.
#' @return A list with the equation used, and a list of model parameters for
#'   the different life stages considered in the article.
#' @examples
#'   mymodel <- ha_ahmad2024_ls(plotfig = FALSE)
#' @export
ha_ahmad2024_ls	<- function(plotfig = TRUE){
  if(plotfig){
    temp <- c(0, 40)
    dev_egg <- -0.1041 + 0.0135*temp
    dev_l1 <- -0.1033 + 0.0208*temp
    dev_l2 <- -0.3041 + 0.0289*temp
    dev_l3 <- -0.0798 + 0.0171*temp
    dev_l4 <- -0.0654 + 0.0147*temp
    dev_l5 <- -0.1492 + 0.026*temp
    dev_l6 <- -0.0713 + 0.017*temp
    dev_prepup <- -0.2086 + 0.0248*temp
    dev_pup <- -0.0547 + 0.0062*temp
    graphics::plot(
      x = temp, y = dev_egg, type = "n", pch = 19,
      main = "DOI: 10.21203/rs.3.rs-4845823/v1 ; Ahmad 2024",
      xlab = "Temperature (Celsius)",
      ylab = "Development rate (day^-1)", ylim = c(0, 0.8),
      xlim = c(0, 40)
    )
    graphics::points(x = temp, y = dev_egg, type = "l", lty = 1)
    graphics::points(x = temp, y = dev_l1, type = "l", lty = 2)
    graphics::points(x = temp, y = dev_l2, type = "l", lty = 3)
    graphics::points(x = temp, y = dev_l3, type = "l", lty = 4)
    graphics::points(x = temp, y = dev_l4, type = "l", lty = 5)
    graphics::points(x = temp, y = dev_l5, type = "l", lty = 6)
    graphics::points(x = temp, y = dev_l6, type = "l", lty = 7)
    graphics::points(x = temp, y = dev_prepup, type = "l", lty = 8)
    graphics::points(x = temp, y = dev_pup, type = "l", lty = 9)
    graphics::legend(
      "topleft", lty = 1:9, legend = c("egg", paste0("l", 1:6), "prepupa", "pupa"),
      bty = "n"
    )
  }
  dr_egg <- list(aa = -0.1041, bb = 0.0135)
  dr_l1 <- list(aa = -0.1033, bb = 0.0208)
  dr_l2 <- list(aa = -0.3041, bb = 0.0289)
  dr_l3 <- list(aa = -0.0798, bb = 0.0171)
  dr_l4 <- list(aa = -0.0654, bb = 0.0147)
  dr_l5 <- list(aa = -0.1492, bb = 0.026)
  dr_l6 <- list(aa = -0.0713, bb = 0.017)
  dr_prepup <- list(aa = -0.2086, bb = 0.0248)
  dr_pup <- list(aa = -0.0547, bb = 0.0062)
  return(list(
    equation = list(
      egg = "campbell_74",
      l1 = "campbell_74",
      l2 = "campbell_74",
      l3 = "campbell_74",
      l4 = "campbell_74",
      l5 = "campbell_74",
      l6 = "campbell_74",
      prepupa = "campbell_74",
      pupa = "campbell_74"
    ),
    model = list(
      egg = dr_egg,
      l1 = dr_l1,
      l2 = dr_l2,
      l3 = dr_l3,
      l4 = dr_l4,
      l5 = dr_l5,
      l6 = dr_l6,
      prepupa = dr_prepup,
      pupa = dr_pup
    )
  ))
}
