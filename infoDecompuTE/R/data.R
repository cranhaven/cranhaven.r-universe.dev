##' Randomised Block design consisted of 6 blocks and 3 plots.
##' 
##' @name chrisEx1
##' @docType data
##' @format A data frame with 18 rows and 5 variables: \describe{
##' \item{Blocks}{Block factor containing 6 levels} \item{Plots}{Plot factor
##' containing 3 levels}
##' \item{A}{Treatment factor A containing 3 levels}
##' \item{B}{Treatment factor B containing 3 levels} 
##' \item{C}{Treatment factor C containing 9 levels} }
##' @keywords datasets
##' 
##' @examples 
##'  data(chrisEx1)
##' 
##' summaryAovOnePhase(chrisEx1, "Blocks/Plots", "A*B*C")
"chrisEx1"





##' Randomised Block design consisted of 8 blocks and 2 plots.
##' 
##' @name chrisEx2
##' @docType data
##' @format A data frame with 18 rows and 5 variables: \describe{
##' \item{Blocks}{Block factor containing 6 levels} \item{Plots}{Plot factor
##' containing 3 levels} \item{A}{Treatment factor A containing 2 levels}
##' \item{B}{Treatment factor B containing 2 levels} \item{C}{Treatment factor
##' C containing 3 levels} }
##' @keywords datasets
##' 
##' @examples 
##'  data(chrisEx2)
##' 
##' summaryAovOnePhase(chrisEx2, "Blocks/Plots", "A*B*C")
"chrisEx2"





##' Randomised Block design consisted of 4 blocks and 2 plots.
##' 
##' 
##' @name chrisEx3
##' @docType data
##' @format A data frame with 8 rows and 5 variables: \describe{
##' \item{Blocks}{Block factor containing 4 levels} \item{Plots}{Plot factor
##' containing 2 levels} \item{A}{Treatment factor A containing 2 levels}
##' \item{B}{Treatment factor B containing 2 levels} \item{C}{Treatment factor
##' C containing 4 levels} }
##' @keywords datasets
##' 
##' @examples 
##'  data(chrisEx2)
##' 
##' summaryAovOnePhase(chrisEx2, "Blocks/Plots", "A*B*C") 
"chrisEx3"
