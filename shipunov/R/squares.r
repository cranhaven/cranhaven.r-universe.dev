Squares <- function(ppts, relative=FALSE) {
sq <- sapply(ppts, Polyarea) # absolute areas
if (relative) sq <- sq/sum(sq) # relative areas
sq
}
