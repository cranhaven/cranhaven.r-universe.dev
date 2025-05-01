chEck_cOntrOl <- function(x) {
  inner_token <- NLP::as.Token_Tokenizer(NLP::Regexp_Tokenizer("\\s", invert = TRUE))
  if (is.null(x)) {
    x <- list(wordLengths = c(1, 25), tokenizer = inner_token)
  }
  else if (identical(x, "auto") | identical(x, "auto1")){
    x <- list(wordLengths = c(1, 25), tokenizer = inner_token)
  }
  else if (identical(x, "auto2")){
    x <- list(wordLengths = c(2, 25), tokenizer = inner_token)
  }  
  else if (class(x) != "list") {
    stop("control must be a list or NULL.")
  }
  else {
    namescontrol <- names(x)
    if (is.null(namescontrol)) 
      stop("Elements of control must have names.")
    if (!all(namescontrol %in% c("bounds", "have", "wordLengths", "dictionary", "tokenizer", "weighting"))) 
      stop("List names must be among bounds, have, wordLengths, dictionary, tokenizer, weighting.")
    if ("dictionary" %in% namescontrol) {
	  dicvec <- x$dictionary
	  stopifnot(is_character_vector(dicvec, allow_all_na = FALSE))
	  dicvec <- dicvec[!is.na(dicvec)]
	  dicvec <- whetherencode(dicvec)
      max_nchar <- max(nchar(dicvec))
	  x$dictionary <- dicvec
      x$wordLengths <- c(1, max_nchar)
    }
    else {
	  if ("wordLengths" %in% namescontrol){
	    if(!is_positive_integer(x$wordLengths, len = 2))
	      stop("wordLengths must be a positive numeric vector of length 2.")
	    x$wordLengths <- sort(x$wordLengths)
      }
	  if (!"wordLengths" %in% namescontrol){
        x$wordLengths <- c(1, 25)
	  }
	}
    if ("bounds" %in% namescontrol){
	   if(!is_positive_integer(x$bounds, len = 2))
	     stop("bounds must be a positive numeric vector of length 2.")
	   x$bounds <- sort(x$bounds)	  
    }
    if ("have" %in% namescontrol){
	   if(!is_positive_integer(x$have, len = 2))
	     stop("have must be a positive numeric vector of length 2.")
	   x$have <- sort(x$have)	  
    }
    if ("weighting" %in% namescontrol){
      if (!is.function(x$weighting))
        stop("The weighting in control list must be a function.")
    }
  }
  x$tokenizer <- inner_token 
  return(x)
}
