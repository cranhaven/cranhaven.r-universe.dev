#' @title Return x Between a and b
#'
#' @param x numeric vector
#' @param a one number
#' @param b one number
#' @param include The direction of a and b. Any left letter of lower or upper
#'
#' @return values of x between a and b
#' @export
#'
#' @examples
#' x_ab(mtcars$disp,150,190)
x_ab <- function(x,a,b,include='l'){
    min.x=min(a,b)
    max.x=max(a,b)
    if (do::left('lower',nchar(include))==include){
        res=x[x >=min.x & x < max.x]
    }else if (do::left('upper',nchar(include))==include){
        res=x[x > min.x & x <=max.x]
    }
    return(res)
}
