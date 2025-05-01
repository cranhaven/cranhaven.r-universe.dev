#' @title Write Chinese Tang Poems
#'
#' @description This package write Chinese Tang Poems automatically
#' @param n, should be the length of the poem
#' @return NULL
#' @examples write_tangpoem(5)
#' @export
#'
write_tangpoem <- function(n)
{
  #write a five-character quatrain)
  if(n==5){
    empty5 <- ""
    for (i in 1:length(sp5_2)){
      temp5 <- example_2[attributes(example_2)$name == cixing5[i]]
      temp5 <- temp5[nchar(temp5) == nchar(sp5_2[i])]
      if (length(temp5)==0) break
      empty5 <- paste0(empty5,sample(temp5, 1, replace=FALSE))
    }
    #add punctuations
     result5 <- paste0(substr(empty5, 1, 5),"\n", substr(empty5, 6, 10),"\n",
                  substr(empty5, 11, 15), "\n", substr(empty5, 16, 20))
     cat(result5)
  }
  #write a seven-character quatrain
  else if(n==7){
    empty7 <- ""
    for (i in 1:length(sp7_2)){
      temp7 <- example_2[attributes(example_2)$name == cixing7[i]]
      temp7 <- temp7[nchar(temp7) == nchar(sp7_2[i])]
      if (length(temp7)==0) break
      empty7 <- paste0(empty7,sample(temp7, 1, replace=FALSE))
    }
    #add punctuations
    result7 <- paste0(substr(empty7, 1, 7),"\n", substr(empty7, 8, 14),"\n",
                      substr(empty7, 15, 21), "\n", substr(empty7, 22, 28))
    cat(result7)
  }
  else cat("Your input does not meet the requirement of Tang Poem. Thank you.")
}

