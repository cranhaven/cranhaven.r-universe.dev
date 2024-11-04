#' Calculate balance statistics from an assignment object
#' 
#' Calculate several balance statistics for experimental units assigned to treatment conditions.  Naturally accepts output from the \code{assignment} function, and passes it to \code{xBalance} from \code{library(RItools)}. Provides balance summaries for the entire experiment and by group.
#' 
#' @details As of \code{RItools} version 0.1-11, \code{to.report} must be a subset of \code{c("std.diffs","z.scores","adj.means", "adj.mean.diffs","adj.mean.diffs.null.sd", "chisquare.test","p.values", "all")}. The default, \code{all}, returns all measures.
#' 
#' @author Ryan T. Moore
#' 
#' @param assg.obj an output object from \code{assignment}.
#' @param data the data frame that was input into \code{block} for blocking.
#' @param id.var a string specifying the column of \code{data} containing identifying information.
#' @param bal.vars a string or vector of strings specifying which column(s) of \code{data} contain the variables on which balance is to be checked.
#' @param to.report a string or vector of strings passed to \code{xBalance} listing the measures to report for each group. See Details for more information.
#' 
#' @return A list of output objects from \code{xBalance}. For each group defined in the \code{assignment} object, one list element is assigned the name of that group and summarizes the balance in that group according to \code{to.report}. The last element of the list is named \code{"Overall"} and summarizes balance across all groups. The elements of this list are themselves objects of class \code{c("xbal", "list")}.
#' 
#' If \code{assg.obj} has only one group, the first element of the output list is named \code{"Group1"}, and the second is named \code{"Output"}. In this case, these two elements will be identical.
#' 
#' @references 
#' 
#' Hansen, Ben B. and Jake Bowers. 2008. "Covariate balance in simple, 
#' stratified and clustered comparative studies". \emph{Statistical Science} 
#' 23(2):219--236.
#' 
#' Bowers, Jake and Mark Fredrickson and Ben Hansen. 2010. 
#' "RItools:Randomization Inference Tools". R package version 0.1-11.
#' 
#' Moore, Ryan T. 2012. "Multivariate Continuous Blocking to Improve Political
#' Science Experiments". \emph{Political Analysis}, 20(4):460--479, Autumn.
#' 
#' @keywords design multivariate
#' 
#' @examples
#' data(x100)
#' b <- block(x100, groups = "g", id.vars = "id", block.vars = c("b1", "b2"))
#' a <- assignment(b)
#' axb <- assg2xBalance(a, x100, id.var = "id", bal.vars = c("b1", "b2"))
#' axb
#' # axb is a list with 4 elements (one for each of 3 groups, plus one for 'Overall')
#' 
#' @seealso \code{\link{assignment}}
#' 
#' @export

assg2xBalance <- function(assg.obj, data, id.var, bal.vars, to.report = "all"){
  
  data[["Tr"]] <- extract_conditions(assg.obj, data, id.var)
    
  fff <- formula(paste("Tr ~ ", paste(bal.vars, collapse = "+")))
  xbal.list <- list()
  n.groups <- length(assg.obj$assg)
  
  for(i in 1:n.groups){
    
    # This group's assignments:
    assg.gp <- assg.obj$assg[[i]]
    
    # This group's ID's:
    ids.gp <- unlist(assg.gp[, 1:(ncol(assg.gp) - 1)])
    
    # The data from this group:
    data.tr.gp <- data |> 
      dplyr::filter(get(id.var) %in% ids.gp)
    
    xbal.out <- RItools::xBalance(fff, data = data.tr.gp, report = c(to.report))

    xbal.list[[paste("Group", i, sep = "")]] <- xbal.out
  }
  
  data <- data[!(is.na(data$Tr)), ]
  
  xbal.list[["Overall"]] <- RItools::xBalance(fff, data = data, report = c(to.report))

  return(xbal.list)	
}