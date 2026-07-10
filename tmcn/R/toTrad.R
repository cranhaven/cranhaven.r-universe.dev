
##' Convert a chinese text from simplified to traditional characters and vice versa.
##' 
##' @title Convert a Chinese text from simplified to traditional characters and vice versa.
##' @param string A Chinese string vector.
##' @param rev Reverse. TRUE means traditional to simplified. Default is FALSE.
##' @return Converted vectors.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @examples
##' toTrad("hello")

toTrad <- function(string, rev = FALSE)
{
	string <- .verifyChar(string)
	string <- toUTF8(string)
	.tmcnEnv <- .verifyEnv()
	utils::data(SIMTRA, envir = .tmcnEnv)
	SIMTRA <- get("SIMTRA", envir = .tmcnEnv)
	if (rev) {
		OUT <- chartr(SIMTRA$Tra, SIMTRA$Sim, string)
	} else {
		OUT <- chartr(SIMTRA$Sim, SIMTRA$Tra, string)
	}
		
	return(OUT)
}

