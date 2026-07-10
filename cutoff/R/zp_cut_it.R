#' Cut Continuous Vector to Classification
#'
#' @param x numeric vector
#' @param cut_points cuting points value
#' @param include The direction of cutoff point. Any left letter of lower or upper
#' @param labels logical. False is defaulted. TRUE means set range as factor.
#'
#' @return numeric vector or factor
#' @export
#'
#' @examples
#' cutit(mtcars$disp,c(150,190))
#' cutit(mtcars$disp,c(150,190),labels = TRUE)
cutit <- function(x,cut_points,include='low',labels=FALSE){
    if (!judge_123(cut_points)) stop('cut_points should be from small to big')
    seg = length(cut_points)+1
    cut2 = c(min(x,na.rm = T),cut_points,max(x,na.rm = T))
    res = x
    for (i in 1:seg) {
        pt1=cut2[i]
        pt2=cut2[i+1]
        if (do::left('lower',nchar(include))==include){
            if (i==1){
                lab=NULL
                res[x <= pt2] = (1:seg)[i]
                lab=c(lab,paste0('(' ,pt1,',',pt2, ']'))
            }else{
                res[x >pt1 & x <= pt2] = (1:seg)[i]
                lab=c(lab,paste0('(' ,pt1,',',pt2, ']'))
            }
        }else if (do::left('upper',nchar(include))==include){
            if (i==seg){
                lab=NULL
                res[x >= pt1] = (1:seg)[i]
                lab=c(lab,paste0('(' ,pt1,',',pt2, ']'))
            }else{
                res[x >= pt1 & x < pt2] = (1:seg)[i]
                lab=c(lab,paste0('(' ,pt1,',',pt2, ']'))
            }
        }
    }
    if (labels){
        res2=factor(res,levels = 1:seg,labels = lab)
        return(res2)
    }else{
        return(res)
    }
}
