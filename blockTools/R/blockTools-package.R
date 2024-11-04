#' @title Block, Randomly Assign, and Diagnose Potential Interference in Randomized Experiments
#' 
#' @description
#' Block units into experimental blocks, with one unit per treatment condition, by creating a measure of multivariate distance between all possible pairs of units. Maximum, minimum, or an allowable range of differences between units on one variable can be set. Randomly assign units to treatment conditions. Diagnose potential interference problems between units assigned to different treatment conditions. Write outputs to .tex and .csv files.
#' 
#' @details
#' Given raw data, \code{block} creates experimental blocks, \code{assignment} assigns units to treatment conditions, \code{diagnose} detects possible interference problems, and \code{outTeX} and \code{outCSV} write block or assignment output objects to a set of .tex and .csv files, respectively. In sequential experiments, \code{seqblock} assigns units to treatment conditions.
#' 
#' @examples
#' data(x100)
#' 
#' # block
#' out <- block(x100, groups = "g", n.tr = 2, id.vars = c("id"), 
#'              block.vars = c("b1", "b2"), algorithm = "optGreedy", 
#'              distance = "mahalanobis", level.two = FALSE, valid.var = "b1",
#'              valid.range = c(0,500), verbose = TRUE)
#'              
#' # assign
#' assg <- assignment(out, seed = 123)
#' 
#' # diagnose
#' diag <- diagnose(object = assg, data = x100, id.vars = "id",
#'                  suspect.var = "b2", suspect.range = c(0,50))
#'                  
#' # create .tex files of assigned blocks
#' # outTeX(assg)
#' 
#' # create .csv files of unassigned blocks
#' # outCSV(out)
#' 
#' # create block IDs
#' createBlockIDs(out, x100, id.var = "id")
#' 
#' # block ID integers are unique, even with several groups
#' axb <- assg2xBalance(assg, x100, id.var = "id", bal.vars = c("b1", "b2"))
#' 
#' @author
#' Ryan T. Moore [aut, cre] (\email{rtm@american.edu}),
#' Keith Schnakenberg [aut] (\email{keith.schnakenberg@gmail.com})
#' 
#' @references 
#' \url{https://www.ryantmoore.org/html/software.blockTools.html}
#' 
#' Moore, Ryan T. Multivariate Continuous Blocking to Improve Political Science Experiments. Political Analysis, 20(4):460-479, 2012.
#' 
#' Moore, Ryan T. and Sally A. Moore. Blocking for Sequential Political Experiments. Political Analysis, 21(4):507-523, 2013.
#' 
#' @keywords package design
"_PACKAGE"