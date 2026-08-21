#' Pruning of a Exact Tree
#'
#' Determines the optimally pruned size of the tree by applying the one
#' standard error rule to the results from the bias-corrected bootstrap procedure.
#'
#' @param tree fitted tree of the class \code{ETree}.
#' @param pp pruning parameter, the constant (\eqn{c}) to be used in the \eqn{c*}standard
#'   error rule. The default value is 1.
#' @param \dots optional additional arguments.
#'
#' @details The one standard error rule for \code{ETrees} uses the estimates of the bias-corrected
#'   criterion value (\eqn{C}) and its standard error for each value of \eqn{L}
#'   (= maximum number of leaves). The optimally pruned tree corresponds to the
#'   smallest tree with a bias-corrected \eqn{C} higher or equal to the maximum
#'   bias-corrected \eqn{C} minus its standard error.
#'
#' @return Returns an object of class \code{ETree}. The number of leaves of this object is
#'   equal to the optimally pruned size of the tree.
#'
#'
#'
#'
#'
#' @keywords tree
#'
#' @importFrom rpart prune
#' @export
prune.ETree<-function(tree,pp=1,...){

    #pp
    mindex <- which.min(tree$CVOutput$Rel_error)  # find the row of the minimum x-error
    crit.minse <- tree$CVOutput$Rel_error[mindex] + pp*tree$CVOutput$SE[mindex]  # the minimum x-error + c*SE
    crit.row <- min(which(tree$CVOutput$Rel_error<= crit.minse))  # find the smallest tree within the minimum x-error + c*SE
    size <- tree$CVOutput$Size[crit.row]  # get the cp value for the smallest tree


    CVOutput <- tree$CVOutput
    CVOutput$Size <- CVOutput$Size[size]
    CVOutput$Error <- CVOutput$Error[size]
    CVOutput$Rel_error <- CVOutput$Rel_error[size]
    CVOutput$SE <- CVOutput$SE[size]
    CVOutput$SE2 <- CVOutput$SE2[size]
    CVOutput$CHECK <- CVOutput$CHECK[size]

    pruned_tree<-list(Xvarnames = tree$Xvarnames, Yvarnames = tree$Yvarnames,
                      Desc = tree$Desc, Evaluation = tree$Evaluation[size],
                      ProbLeaves = tree$ProbLeaves, Prob = tree$Prob,
                      Sizes = tree$Sizes, Index = tree$Index[[size]],
                      CVOutput = CVOutput, Transf_Trees = list(tree$Transf_Trees[[size]]),
                      TAll = tree$TAll[[size]], hAll = tree$hAll[[size]],
                      Tree = tree$Tree, h = tree$hAll[size], Data = tree$Data)

    class(pruned_tree) <- "ETree"

    return(pruned_tree)
}
