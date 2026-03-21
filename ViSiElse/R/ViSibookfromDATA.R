#' \code{ViSibookfromDATA} build an object class ViSibook from observational data. The process is the ordered list of punctual actions given by the columns names of X.
#' @name ViSibookfromDATA
#' @title Function \code{ViSibookfromDATA}
#' @rdname ViSibookfromDATA
#' @aliases ViSibookfromDATA
#' @export ViSibookfromDATA
#' @param X data.frame.
#' @param idsubject numeric indicates the number of the column of X which stores id.
#' @return a ViSibook corresponding to the dataset X.
ViSibookfromDATA <- function( X , idsubject = 1 ){
          if( length( idsubject ) >0 ){
          vars <- colnames(X)[-1]
         label <- colnames(X)[-1]
         typeA <- rep( "p", dim(X)[2]-1)
          showorder <- seq( 1, dim(X)[2]-1)
          deb   <- rep( NA, dim(X)[2]-1)
          fin <- rep( NA, dim(X)[2]-1)
          GZDeb  <- rep( NA, dim(X)[2]-1)
          GZFin  <- rep( NA, dim(X)[2]-1)
          Repetition  <- rep( NA, dim(X)[2]-1)
          BZBeforeDeb  <- rep( NA, dim(X)[2]-1)
          BZBeforeFin  <- rep( NA, dim(X)[2]-1)
          BZAfterDeb  <- rep( NA, dim(X)[2]-1)
          BZAfterFin  <- rep( NA, dim(X)[2]-1)
          BZLong <- rep( NA, dim(X)[2]-1)
          BZLtype  <- rep( NA, dim(X)[2]-1)
          book <- data.frame( cbind( vars, label, typeA, showorder, deb, fin, GZDeb, GZFin, BZBeforeDeb, BZBeforeFin, BZAfterDeb, BZAfterFin, BZLong ,BZLtype),stringsAsFactors = FALSE )
          book <- ConvertoViSibook(book)
          }else{
                    vars <- colnames(X)
                    label <- colnames(X)
                    typeA <- rep( "p", dim(X)[2])
                    showorder <- seq( 1, dim(X)[2])
                    deb   <- rep( NA, dim(X)[2])
                    fin <- rep( NA, dim(X)[2])
                    GZDeb  <- rep( NA, dim(X)[2])
                    GZFin  <- rep( NA, dim(X)[2])
                    Repetition  <- rep( NA, dim(X)[2])
                    BZBeforeDeb  <- rep( NA, dim(X)[2])
                    BZBeforeFin  <- rep( NA, dim(X)[2])
                    BZAfterDeb  <- rep( NA, dim(X)[2])
                    BZAfterFin  <- rep( NA, dim(X)[2])
                    BZLong <- rep( NA, dim(X)[2])
                    BZLtype  <- rep( NA, dim(X)[2])
                    book <- data.frame( cbind( vars, label, typeA, showorder, deb, fin, GZDeb, GZFin, BZBeforeDeb, BZBeforeFin, BZAfterDeb, BZAfterFin, BZLong ,BZLtype),stringsAsFactors = FALSE )
                    book <- ConvertoViSibook(book)
          }
          return(book)
}
