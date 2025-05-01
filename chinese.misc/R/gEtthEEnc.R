#' @import stringi
gEtthEEnc <- function(x1, x2) {
	if (x2 == "auto"){
		x1=file(x1, "rt")
		on.exit(close(x1))
		y=stringi::stri_enc_detect(paste0(readLines(x1, warn=FALSE, n=50), collapse=""))[[1]]$Encoding[1]
		if (is.na(y)) y="native.enc"
	} else{
		y=x2
	}
	y
}
# gEtthEEnc <- function(x1, x2) {
#   if (x2 == "auto") {
#     y <- tryCatch(expr = {
#       inner_enc <- Ruchardet::detectFileEncoding(x1, 40)
#     }, warning = function(w) {
#       return("native.enc")
#     })
#   }
#   else {
#     y <- x2
#   }
#   return(y)
# }
