
vigor_corr <- function(lengths, Ger = 100, wg = 0.7, wu = 0.3, Unif = 1){

 if (Unif == 1) {
   ivigor <- (growth(lengths)*wg + unif_1(lengths)*wu)*Ger/100
  }

  if (Unif == 2) {

    ivigor <- (growth(lengths)*wg + unif_2(lengths)*wu)*Ger/100
  }

  result <- ivigor
  return(result)
}

