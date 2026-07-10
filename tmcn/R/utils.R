
.verifyChar <- function(vec) {
	if (!is.atomic(vec)) stop("Must be an atomic vector!", call. = FALSE)
	OUT <- as.character(vec)
	return(OUT)
}

.verifyNum <- function(vec) {
	if (!is.atomic(vec)) stop("Must be an atomic vector!")
	OUT <- suppressWarnings(as.numeric(vec))
	if (length(OUT[!is.na(OUT)]) == 0) stop("Does not contain any numeric elements!", call. = FALSE)
	return(OUT)
}

.verifyEnv <- function() {
	if (!exists(".tmcnEnv", envir = .GlobalEnv)) {
		envir0 = as.environment(1)
		assign(".tmcnEnv", new.env(), envir = envir0)
	} 
	OUT <- get(".tmcnEnv", envir = .GlobalEnv)
	return(OUT)
}

.getStopWords <- function() {
	.tmcnEnv <- .verifyEnv()
	utils::data(STOPWORDS, envir = .tmcnEnv)
	STOPWORDS <- get("STOPWORDS", envir = .tmcnEnv)
	return(STOPWORDS)
}

.strsplit_space_tokenizer <- function(x) {
	unlist(strsplit(as.character(x), "[[:space:]]+"))
}

.createHashmapEnv <- function(key, value)
{
	key <- .verifyChar(key)
	value <- .verifyChar(value)
	if (length(key) != length(value)) stop("Length of 'key' and 'value' are not same!")
	e <- new.env(hash = TRUE, size = length(key))
	for (ikey in 1:length(key)) {
		assign(x = key[ikey], value = value[ikey], envir = e)
	}
	return(e)
}

