#' Sets the control parameters for \code{jlctree}. 
#'
#' @param test.stat test statistic to use, ``lrt'' for likelihood ratio test,
#'      and ``wald'' for Wald test. Default is ``lrt''.
#' @param stop.thre stops splitting if current node has test statistic less than \code{stop.thre}. 
#'      Default is 3.84.
#' @param stable if TRUE, check the variance of the estimated coefficients in survival models fit at tree nodes.
#'      If a node has variance larger than \code{cov.max}, the splitting function
#'      will not consider splits leading to that node. Default is TRUE.
#' @param maxng maximum number of terminal nodes. Default is 6.
#' @param min.nevents minimum number of events in any terminal node.
#'      By default, this parameter is set to the number of covariates 
#'      used in the survival model.
#' @param split.add when computing the difference between parent node's test statistic
#'      and sum of child nodes' test statistics, add \code{split.add} to the difference. 
#'      When \code{split.add} > 0, tree may still split even if current split 
#'      leads to negative improvement. 
#'      Set \code{split.add} to a large positive value for the purpose of greedy splitting. 
#'      Default is 20.
#' @param cov.max upper bound on the variance of the estimated coefficients 
#'      in survival models at tree nodes. The covariates will be scaled to have unit variance.
#'      Default is 1e5. 
#' @param fity if TRUE, once a tree is constructed, fit a linear mixed-effects model using 
#'      tree nodes as group indicators. Default is TRUE.
#' @param fits if TRUE, once a tree is constructed, fit survival models using tree nodes
#'      as group indicators. Default is TRUE.
#' @param ... further arguments to pass to or from other methods.
#'
#' @return A list of all these parameters.
#' @seealso \code{\link{jlctree},\link{jlctree-package}}
#' @export

jlctree.control <- function(test.stat='lrt', stop.thre=3.84, stable=TRUE, maxng=6,
                            min.nevents=5, split.add=20, cov.max=1e5, 
                            fity=TRUE, fits=TRUE, ... )
{
    if (!is.character(test.stat) || ! test.stat %in% c('lrt','wald'))
        stop("value of 'test.stat' must be 'lrt' or 'wald'")
    if (!is.numeric(stop.thre) || stop.thre < 0)
        stop("value of 'stop.thre' must be >= 0")
    if (!is.logical(stable))
        stop("value of 'stable' must be TRUE or FALSE")
    if (!is.numeric(maxng) || maxng < 1)
        stop("value of 'maxng' must be >= 1")
    if (!is.numeric(min.nevents) || min.nevents < 0)
        stop("value of 'min.nevents' must be >= 0")
    if (!is.numeric(split.add) || split.add < 0)
        stop("value of 'split.add' must be >= 0")
    if (!is.numeric(cov.max) || cov.max< 0)
        stop("value of 'cov.max' must be >= 0")
    if (!is.logical(fity))
        stop("value of 'fity' must be TRUE or FALSE")
    if (!is.logical(fits))
        stop("value of 'fits' must be TRUE or FALSE")

    list(test.stat=test.stat, stop.thre=stop.thre, 
         stable=stable, maxng=maxng, 
         min.nevents=min.nevents, split.add=split.add,
         cov.max=cov.max, fity=fity, fits=fits)
}
