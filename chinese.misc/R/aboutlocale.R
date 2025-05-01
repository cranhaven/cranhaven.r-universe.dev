localestart2 <- function(){
	y <- tryCatch(
		expr = {
			s_right_locale = getOption("tmp_chi_locale")
			if (is.null(s_right_locale) || is.na(s_right_locale)){
				stop("not change")
			}
			if (identical(s_right_locale, "auto")){
				s_right_locale <- ifelse(.Platform$OS.type == "windows", "Chinese (Simplified)_China.936", "zh_CN.UTF-8")
			}
			s <- Sys.getlocale("LC_CTYPE")
			if (s == s_right_locale){
				yy <- "n"
			} else {
				Sys.setlocale(category = "LC_COLLATE", s_right_locale)
				Sys.setlocale(category = "LC_CTYPE", s_right_locale)
				yy <- c("y", s)
			}
			yy
		},
		error = function(e){
			return("n")
		}
	)
	return(y)
}

localeend2 <- function(x){
	tryCatch(
		expr = {
			if (x[1] == "y"){
				Sys.setlocale(category = "LC_COLLATE", x[2])
				Sys.setlocale(category = "LC_CTYPE", x[2])
			}
		},
		error = function(e){
			message(" ")
		}
	)
}

whetherencode <- function(x){
	y <- tryCatch(
		expr = {
			xx <- x
			de <- suppressWarnings(stringi::stri_enc_detect(x))
			de <- sapply(de, FUN=function(sapp) sapp$Encoding[1])
			not_utf8=which(! de %in% c("UTF-8", "utf-8"))
			if (length(not_utf8) > 0){
				for (i in not_utf8) xx[i] <- stringi::stri_encode(xx[i], to = "UTF-8")
			}
			xx
		},
		error = function(e){
			return(-999999)
		},
		warning = function(w){
			return(-999999)
		}
	)
	if (identical(y, -999999))
		y <- x
	return(y)
}


# whetherencode <- function(x){
# 	y <- tryCatch(
# 		expr = {
# 			xx <- x
# 			de <- suppressWarnings(Ruchardet::detectEncoding(x))
# 			for (i in 1: length(de)){
# 				if (! de[i] %in% c("UTF-8", "utf-8")){
# 					xx[i] <- stringi::stri_encode(xx[i], to = "UTF-8")
# 				}
# 			}
# 			xx
# 		},
# 		error = function(e){
# 			return(-999999)
# 		},
# 		warning = function(w){
# 			return(-999999)
# 		}
# 	)
# 	if (identical(y, -999999))
# 		y <- x
# 	return(y)
# }
