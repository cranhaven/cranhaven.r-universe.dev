
vigor <- function(lengths, wg = 0.7, wu = 0.3, Unif = 1){

 if (Unif == 1) {
   ivigor <- (growth(lengths)*wg + unif_1(lengths)*wu)
  }

  if (Unif == 2) {

    ivigor <- (growth(lengths)*wg + unif_2(lengths)*wu)
  }

  result <- ivigor

  return(result)
}

