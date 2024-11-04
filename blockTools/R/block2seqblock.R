#' @title Prepare prior nonsequential assignments for subsequent sequential assignments
#' 
#' @description
#' Converts output objects from the \code{block} and \code{assignment} functions into an object in the format of one output by the \code{seqblock} function. This allows the user to block and assign multiple units at the beginning of an experiment (using \code{block} and \code{assignment}) and then sequentially block and assign more units to the experiment over time (using \code{seqblock}).
#' 
#' @details
#' The function converts data from a blocked experiment into a form allowing subsequent sequential blocking. Minimally, the user sets only the arguments \code{block.obj}, \code{assg.obj} and \code{data}. Then, \code{block2seqblock} uses the call to \code{block}, the assignment object, and the original data to create an object that is ready to be input into \code{seqblock}.
#' 
#' If the user explicitly specifies \code{groups}, \code{id.vars} and \code{block.vars} in the initial \code{block} function that is used to create the \code{block.obj}, \code{block2seqblock} will order the variables in the output it produces according to the order specified in the initial \code{block} function call. If the user does not explicitly specify the blocking variables in the \code{block} function call, \code{block2seqblock} will order the variables according to the order in the initial matrix or dataframe that was used to run the original \code{block} function.
#' 
#' As part of the function, variables that are of class \code{factor} in the original matrix or dataframe specified in \code{data}, will be converted into class \code{character}. 
#' 
#' The \code{trn} argument uses the \code{n.tr} argument from \code{block} to extract the names of the treatment variables. Most other arguments are set to default values that mirror those in the \code{seqblock} function. One exception is the \code{datetime} argument, which defaults to a vector of \code{NA}'s instead of the current datetime.
#' 
#' @param block.obj an output object from \code{block}, or a user-specified block object
#' @param assg.obj an output object from \code{assignment}, or a user-specified assignment object
#' @param data a matrix or dataframe containing the original data used to block the units in the study
#' @param exact.restr a list object containing the restricted values that the exact blocking variables can take on. Thus the first element of \code{exact.restr} is a vector containing all of the possible values that the first exact blocking variable can take on; the second element is a vector containing all of the possible values for the second exact blocking variable; and so on
#' @param covar.restr a list object containing the restricted values that the non-exact blocking variables can take on. Thus the first element of \code{covar.restr} is a vector containing all of the possible values that the first non-exact blocking variable can take on; the second element is a vector containing all of the possible values for the second non-exact blocking variable; and so on
#' @param covar.order a string or vector of strings containing the name of the non-exact blocking variables ordered so that the highest priority covariate comes first, followed by the second highest priority covariate, then the third, etc.
#' @param trn a string or vector of strings containing the names of the different treatment groups
#' @param apstat a string specifying the assignment probability summary statistic that was used
#' @param mtrim a numeric value specifying the proportion of observations to be dropped when the assignment probability statistic takes on the value \code{"trimmean"}.
#' @param apmeth a string specifying the assignment probability algorithm that was used.
#' @param kfac the assignment probability \emph{kfactor}; see \emph{assg.prob.kfac} in the Arguments section above
#' @param assgpr a vector of assignment probabilities to each treatment group
#' @param distance a string specifying how the multivarite distance used for blocking is calculated
#' @param datetime the date and time that the units were assigned to the treatment group; by default this is set to be a vector of NA; however the user could also specify a specific datetime and all of the units from the block object will be given the same datetime stamp
#' @param orig a dataframe containing the names and values for the different id and blocking variables, as well as each unit's initial treatment assignment
#' @param seed an optional integer value for the random seed set which is used when assigning units to treatment groups
#' @param file.name a string containing the name of the file that one would like the output to be written to. Ideally this file name should have the extension .RData
#' @param verbose a logical stating whether the function should print the name of the output file, the current working directory, and the dataframe \code{x} returned by the function as part of the \code{bdata} list
#' 
#' @return 
#' A list (called \code{bdata}) with elements
#' \itemize{
#'   \item \strong{x}: a dataframe containing the names and values for the different ID and blocking variables, as well as each unit's initial treatment assignment.
#'   \item \strong{nid}: a string or vector of strings containing the name(s) of the ID variable(s).
#'   \item \strong{nex}: a string or vector of strings containing the name(s) of the exact blocking variable(s).
#'   \item \strong{ncv}: a string or vector of strings containing the name(s) of the non-exact blocking variable(s).
#'   \item \strong{rex}: a list of the restricted values of the exact blocking variables.
#'   \item \strong{rcv}: a list of the restricted values of the non-exact blocking variables.
#'   \item \strong{ocv}: a vector of the order of the non-exact blocking variables.
#'   \item \strong{trn}: a string or vector of strings containing the name(s) of the different treatment groups.
#'   \item \strong{apstat}: a string specifying the assignment probability summary statistic that was used.
#'   \item \strong{mtrim}: a numeric value specifying the proportion of observations to be dropped when the assignment probability statistic takes on the value \code{"trimmean"}.
#'   \item \strong{apmeth}: a string specifying the assignment probability algorithm that was used.
#'   \item \strong{kfac}: the assignment probability \emph{kfactor}; see \emph{assg.prob.kfac} in the Arguments section above.
#'   \item \strong{assgpr}: a vector of assignment probabilities to each treatment group.
#'   \item \strong{distance}: a string specifying how the multivarite distance used for blocking is calculated
#'   \item \strong{trd}: a list with the length equal to the number of previously assigned treatment conditions; each object in the list contains a vector of the distance between each unit in one treatment group and the new unit. Set to \code{NULL} when there are no non-exact blocking variables.
#'   \item \strong{tr.sort}: a string vector of treatment conditions, sorted from the largest to the smallest
#'   \item \strong{p}: a vector of assignment probabilities to each treatment group used in assigning a treatment condition to the new unit.
#'   \item \strong{trcount}: a table containing the counts for each experimental/treatment conditions.
#'   \item \strong{datetime}: the date and time that the user was assigned a treatment group.
#'   \item \strong{orig}: a dataframe containing the names and values for the different id and blocking variables, as well as each unit's initial treatment assignment.
#' }
#' 
#' @examples
#' # data(x100)
#' # out <- block(x100, n.tr = 2, id.vars = c("id"), block.vars = c("b1", "b2"), 
#' #              algorithm = "optGreedy", distance = "mahalanobis", 
#' #              valid.var = "b1", valid.range = c(0,500))
#' # assg.out <- assignment(out, seed = 123)
#' # b2sb <- block2seqblock(block.obj = out, assg.obj = assg.out, data = x100)
#' # sb <- seqblock("sbout.RData", id.vals = 1101, covar.vals = c(100, 200), file.name = "sb101.RData")
#' 
#' @seealso \code{\link{block}}, \code{\link{assignment}}, \code{\link{seqblock}}
#' 
#' @author Tommy Carroll \email{tcarroll22@wustl.edu}, Jonathan Homola \email{homola@wustl.edu}, and Ryan T. Moore \email{rtm@american.edu}
#' 
#' @keywords design
#' 
#' @export

block2seqblock <- function(block.obj, assg.obj, data, exact.restr = NULL, 
                           covar.restr = NULL, covar.order = NULL, trn = NULL, 
                           apstat = "mean", mtrim = 0.1, apmeth = "ktimes", 
                           kfac = 2, assgpr = c(0.5, 0.5), distance = NULL, 
                           datetime = NULL, orig, seed = NULL, 
                           file.name = "sbout.RData", verbose = FALSE){
  
  # Store call used to run the block function originally as character string:
  block.call <- as.character(deparse(block.obj$call)) 
  
  # Remove white space:
  block.call <- gsub("    ", "", block.call, fixed=T)   
  block.call <- paste(block.call, collapse="")
  block.call <- substr(block.call, 7, nchar(block.call)-1)
  block.call <- unlist(strsplit(block.call, " "))
  
  # Read in id.vars from the provided block object
    first.id <- grep("id.vars", block.call) + 2
    if(length(grep("=", block.call[(first.id + 1):length(block.call)]))!=0){
      last.id <- grep("=", block.call[(first.id + 1):length(block.call)])[1] - 2 + first.id
    } else {last.id <- length(block.call)}
    id.vars <- block.call[first.id:last.id]
    # Remove c() if it exists:
    if(substr(id.vars[1], 1, 2)=="c("){ 
      id.vars[1] <- substr(id.vars[1], 3, nchar(id.vars[1]))
      # Drop 2, one for right parenthesis and one for comma --- ),
      id.vars[length(id.vars)] <- substr(id.vars[length(id.vars)], 1, nchar(id.vars[length(id.vars)])-2) 
    }
    for(i in 1:length(id.vars)){
      # Remove trailing commas 
      if(substr(id.vars[i], nchar(id.vars[i]), nchar(id.vars[i]))==","){ 
        id.vars[i] <- substr(id.vars[i], 1, nchar(id.vars[i])-1)
      }
      # Remove extra quotes around the word
      if(substr(id.vars[i], 1, 1)=="\""){ 
        id.vars[i] <- substr(id.vars[i], 2, nchar(id.vars[i])-1)
      }
    }
  
  # Read in exact.vars from the provided block object or set them to NULL
    if(length(grep("groups", block.call)) != 0){
      first.exact <- grep("groups", block.call) + 2
      if(length(grep("=", block.call[(first.exact + 1):length(block.call)]))!=0){
        last.exact <- grep("=", block.call[(first.exact + 1):length(block.call)])[1] - 2 + first.exact
      } else {last.exact <- length(block.call)}
      exact.vars <- block.call[first.exact:last.exact]
      # Remove c() if it exists
      if(substr(exact.vars[1], 1, 2) == "c("){ 
        exact.vars[1] <- substr(exact.vars[1], 3, nchar(exact.vars[1]))
        exact.vars[length(exact.vars)] <- substr(exact.vars[length(exact.vars)], 1, nchar(exact.vars[length(exact.vars)])-2)
      }
      for(i in 1:length(exact.vars)){
        if(substr(exact.vars[i], nchar(exact.vars[i]), nchar(exact.vars[i])) == ","){
          exact.vars[i] <- substr(exact.vars[i], 1, nchar(exact.vars[i])-1)
        }
        if(substr(exact.vars[i], 1, 1) == "\""){
          exact.vars[i] <- substr(exact.vars[i], 2, nchar(exact.vars[i])-1)
        }
      }
    } else{exact.vars <- NULL}
  
  # Read in covar.vars from the provided block object or define all non-id-variables as covar.vars   
    if(length(grep("block.vars", block.call)) != 0){
      first.covar <- grep("block.vars", block.call) + 2
      if(length(grep("=", block.call[(first.covar + 1):length(block.call)]))!=0){
        last.covar <- grep("=", block.call[(first.covar + 1):length(block.call)])[1] - 2 + first.covar
      } else {
        last.covar <- length(block.call)
      }
      covar.vars <- block.call[first.covar:last.covar]
      # Remove c() if it exists
      if(substr(covar.vars[1], 1, 2) == "c("){ 
        covar.vars[1] <- substr(covar.vars[1], 3, nchar(covar.vars[1]))
        covar.vars[length(covar.vars)] <- substr(covar.vars[length(covar.vars)], 1, nchar(covar.vars[length(covar.vars)])-2)
      }
      for(i in 1:length(covar.vars)){
        if(substr(covar.vars[i], nchar(covar.vars[i]), nchar(covar.vars[i])) == ","){
          covar.vars[i] <- substr(covar.vars[i], 1, nchar(covar.vars[i])-1)
        }
        if(last.covar < length(block.call)){
          if(substr(covar.vars[i], 1, 1) == "\""){
            covar.vars[i] <- substr(covar.vars[i], 2, nchar(covar.vars[i])-1)
          }
        } else if(last.covar == length(block.call)){
          if(i < length(covar.vars)){
            if(substr(covar.vars[i], 1, 1) == "\""){
              covar.vars[i] <- substr(covar.vars[i], 2, nchar(covar.vars[i])-1)
            }
          }
          if(i == length(covar.vars)){
            if(substr(covar.vars[i], 1, 1) == "\""){
              covar.vars[i] <- substr(covar.vars[i], 2, nchar(covar.vars[i]))
            }
          }
        }
      }
    } else{
      covar.vars <- colnames(data)[which(colnames(data) != id.vars)]
      covar.vars <- covar.vars[which(covar.vars != exact.vars)]    
    }

  # Create trn object
  if(is.null(trn)){
    if(length(grep("n.tr", block.call)) != 0){
      temp <- grep("n.tr", block.call) + 2
      numb.tr <- block.call[temp]
      if(substr(numb.tr[1], 1, 2)=="c("){ #get rid of c() if it exists
        numb.tr[1] <- substr(numb.tr[1], 3, nchar(numb.tr[1]))
        # Drop 2, one for right parenthesis and one for comma --- ),
        numb.tr[length(numb.tr)] <- substr(numb.tr[length(numb.tr)], 1, nchar(numb.tr[length(numb.tr)])-2) 
      }
      if(substr(numb.tr, nchar(numb.tr), nchar(numb.tr))==","){
        numb.tr <- substr(numb.tr, 1, nchar(numb.tr)-1)
      }
      if(substr(numb.tr, 1, 1)=="\""){
        numb.tr <- substr(numb.tr, 2, nchar(numb.tr)-1)
      }
      numb.tr <- as.numeric(numb.tr)
      trn <- paste("Treatment", 1:numb.tr)
    } else{
      numb.tr <- 2
      trn <- paste("Treatment", 1:numb.tr)
    }
  }
  
  # Create distance object
  if(is.null(distance)){
    if(length(grep("distance", block.call)) != 0){
      temporary <- grep("distance", block.call) + 2
      distance <- block.call[temporary]
      # Remove c() if it exists
      if(substr(distance[1], 1, 2) == "c("){ 
        distance[1] <- substr(distance[1], 3, nchar(distance[1]))
        # Drop 2, one for right parenthesis and one for comma --- ),
        distance[length(distance)] <- substr(distance[length(distance)], 1, nchar(distance[length(distance)])-2) 
      }
      if(substr(distance, nchar(distance), nchar(distance)) == ","){
        distance <- substr(distance, 1, nchar(distance)-1)
      }
      if(substr(distance, 1, 1) == "\""){
        distance <- substr(distance, 2, nchar(distance)-1)
      }
    }
    else{distance <- "mahalanobis"}
  }
  
  # Set seed and create x and orig objects 
  if(!is.null(seed)){
    set.seed(seed)
  }
  x1 <- data[, which(colnames(data) %in% id.vars)]
  x2 <- data[, which(colnames(data) %in% exact.vars)]
  x3 <- data[, which(colnames(data) %in% covar.vars)]
  x <- as.data.frame(cbind(x1, x2, x3), stringsAsFactors = FALSE) 
  colnames(x) <- c(id.vars, exact.vars, covar.vars)
  # Draw treatment assignments from original assignment object:
  assg <- assg.obj[[1]]
  treat.assign <- vector(mode = "list", length = length(trn))
  for(i in 1:length(assg)){
    for(j in 1:length(trn)){
      treat.assign[[j]] <- append(treat.assign[[j]], as.vector(assg[[i]][,j]))
    } 
  }
  # treat.assign is a list:
  # first element contains id values for all units that were assigned to TR 1,
  # second element contains id values for all units assigned to TR 2, etc. 
  treatment <- vector(length = nrow(data))
  for(i in 1:length(trn)){
    treatment[which(x[,colnames(x) == id.vars[1]] %in% treat.assign[[i]])] <- trn[i]
  }
  x <- cbind(x, treatment)
  colnames(x)[ncol(x)] <- "Tr"
  
  for(i in 1:ncol(x)){
    if(is.factor(x[,i])){
      x[,i] <- as.character(x[,i])
    }
  }
  
  bdata <- list()
  bdata$x <- x
  bdata$nid <- id.vars
  bdata$nex <- exact.vars
  bdata$ncv <- covar.vars
  bdata$rex <- exact.restr # EXACT restricted values list
  if(!is.null(bdata$rex)){
    names(bdata$rex) <- exact.vars
  }
  bdata$rcv <- covar.restr  # BLOCK restricted values list
  if(!is.null(bdata$rcv)){
    names(bdata$rcv) <- covar.vars
  }
  bdata$ocv <- covar.order   # block covariates order
  bdata$trn <- trn
  bdata$apstat <- apstat 
  bdata$mtrim <- mtrim
  bdata$apmeth <- apmeth
  bdata$kfac <- kfac # multiple for method 'ktimes'
  bdata$assgpr <- assgpr
  bdata$distance <- distance
#   bdata$trd <- tr.dist
#   bdata$tr.sort <- tr.sort
#   bdata$p <- p
#   bdata$trcount <- tr.counts
  if(!is.null(datetime)){
    bdata$datetime <- rep(datetime, length = nrow(x))  
  } else(bdata$datetime <- rep(NA, length = nrow(x)))
  bdata$orig <- x

  # Write bdata to file
  save(bdata, file = file.name)
  if(verbose == TRUE){
    cat("Units' data stored as file ", file.name, 
        ".\nThe current working directory is ", getwd(), "\n", sep = "")
    cat("The new data as entered:\n")
    print(x)
  }  
  return(bdata)	
}