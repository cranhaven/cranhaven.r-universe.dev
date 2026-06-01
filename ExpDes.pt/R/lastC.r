#' Setting the last character of a chain
#'
#' \code{lastC} A special function for the group of treatments
#' in the multiple comparison tests. Use order.group.
#' @param x letters
#' @return x  character.
#' @author Eric B Ferreira,
#'  \email{eric.ferreira@@unifal-mg.edu.br}
#'  @author Denismar Alves Nogueira
#'  @author Portya Piscitelli Cavalcanti
#'  (Adapted from Felipe de Mendiburu - GPL)
#' @seealso \code{\link{order.group}}.
#' @examples
#' x<-c("a","ab","b","c","cd")
#' lastC(x)
#' # "a" "b" "b" "c" "d"
#' @export

lastC<-function(x)
{
    y <- sub(" +$", "", x)
    p1 <- nchar(y)
    cc <- substr(y, p1, p1)
    return(cc)
}
