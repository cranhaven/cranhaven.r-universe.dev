#' @title Block units into homogeneous experimental blocks
#' 
#' @description
#' Block units into experimental blocks, with one unit per treatment condition. Blocking begins by creating a measure of multivariate distance between all possible pairs of units. Maximum, minimum, or an allowable range of differences between units on one variable can be set.
#' 
#' @details
#' If \code{vcov.data = NULL}, then \code{block} calculates the variance-covariance matrix using the \code{block.vars} from \code{data}. 
#' 
#' If \code{groups} is not user-specified, \code{block} temporarily creates a variable in \code{data} called \code{groups}, which takes the value 1 for every unit.
#' 
#' Where possible, one unit is assigned to each condition in each block. If there are fewer available units than treatment conditions, available units are used.
#' 
#' If \code{n.tr} $> 2$, then the \code{optGreedy} algorithm finds the best possible pair match, then the best match to either member of the pair, then the best match to any member of the triple, etc. After finding the best pair match to a given unit, the other greedy algorithms proceed by finding the third, fourth, etc. best match to that given unit.
#' 
#' An example of \code{id.vars} is \code{id.vars = c("id", "id2")}.  If two-level blocking is selected, \code{id.vars} should be ordered (unit id, subunit id).  See details for \code{level.two} below for more information.
#' 
#' If \code{block.vars = NULL}, then all variables in \code{data} except the \code{id.vars} are taken as blocking variables. E.g., \code{block.vars = c("b1", "b2")}.  
#' 
#' The algorithm \code{optGreedy} calls an optimal-greedy algorithm, repeatedly finding the best remaining match in the entire dataset; \code{optimal} finds the set of blocks that minimizes the sum of the distances in all blocks; \code{naiveGreedy} finds the best match proceeding down the dataset from the first unit to the last; \code{randGreedy} randomly selects a unit, finds its best match, and repeats; \code{sortGreedy} resorts the dataset according to \code{row.sort}, then implements the \code{naiveGreedy} algorithm.
#' 
#' The \code{optGreedy} algorithm breaks ties by randomly selecting one of the minimum-distance pairs.  The \code{naiveGreedy}, \code{sortGreedy}, and \code{randGreedy} algorithms break ties by randomly selecting one of the minimum-distance matches to the particular unit in question.
#' 
#' As of version 0.5-1, blocking is done in C for all algorithms except \code{optimal} (see following paragraphs for more details on the \code{optimal} algorithm implementation).
#' 
#' The \code{optimal} algorithm uses two functions from the \pkg{nbpMatching} package: \code{distancematrix} prepares a distance matrix for optimal blocking, and \code{nonbimatch} performs the optimal blocking by minimizing the sum of distances in blocks. \code{nonbimatch}, and thus the \code{block} algorithm \code{optimal}, requires that \code{n.tr = 2}.
#' 
#' Because \code{distancematrix} takes the integer \code{floor} of the distances, and one may want much finer precision, the multivariate distances calculated within \code{block} are multiplied by \code{optfactor} prior to optimal blocking. Then \code{distancematrix} prepares the resulting distance matrix, and \code{nonbimatch} is called on the output. The distances are then untransformed by dividing by \code{optfactor} before being returned by \code{block}.
#' 
#' The choice of \code{optfactor} can determine whether the Fortran code can allocate enough memory to solve the optimization problem. For example, blocking the first 14 units of \code{\link{x100}} by executing \code{block(x100[1:14, ], id.vars = "id", block.vars = c("b1", "b2"), algorithm = "optimal", optfactor = 10^8)} fails for Fortran memory reasons, while the same code with \code{optfactor = 10^5} runs successfully. Smaller values of \code{optfactor} imply easier computation, but less precision.
#' 
#' Most of the algorithms in \code{block} make prohibited blockings by using a distance of \code{Inf}. However, the optimal algorithm calls \code{Fortran} code from \pkg{nbpMatching} and requires integers. Thus, a distance of \code{99999 * max(dist.mat)} is used to effectively prohibit blockings. This follows the procedure demonstrated in the example of \code{help(nonbimatch)}.
#' 
#' In order to enable comparisons of block-quality across groups, when \code{distance} is a string, $Sigma$ is calculated using units from all groups.
#' 
#' The \code{distance = "mcd"} and \code{distance = "mve"} options call \code{cov.rob} to calculate measures of multivariate spread robust to outliers.  The \code{distance = "mcd"} option calculates the Minimum Covariance Determinant estimate (Rousseeuw 1985); the \code{distance = "mve"} option calculates the Minimum Volume Ellipsoid estimate (Rousseeuw and van Zomeren 1990). When \code{distance = "mcd"}, the interquartile range on blocking variables should not be zero.
#' 
#' A user-specified distance matrix must have diagonals equal to 0, indicating zero distance between a unit and itself. Only the lower triangle of the matrix is used.
#' 
#' If \code{weight} is a vector, then it is used as the diagonal of a square weighting matrix with non-diagonal elements equal to zero.  The weighting is done by using as the Mahalanobis distance scaling matrix $((((chol(Sigma))')^\{-1\})'W((chol(Sigma))')^\{-1\})^\{-1\}$, where $chol(Sigma)$ is the Cholesky decomposition of the usual variance-covariance matrix and $W$ is the weighting matrix.  Differences should be smaller on covariates given higher weights.
#' 
#' If \code{level.two = TRUE}, then the best subunit block-matches in different units are found. E.g., provinces could be matched based on the most similar cities within them. All subunits in the data should have unique names. Thus, if subunits are numbered 1 to (number of subunits in unit) within each unit, then they should be renumbered, e.g., 1 to (total number of subunits in all units). \code{level.two} blocking is not currently implemented for \code{algorithm = "optimal"}. Units with no blocked subunit are put into their own blocks. However, unblocked subunits within a unit that does have a blocked subunit are not put into their own blocks.
#' 
#' An example of a variable restriction is \code{valid.var = "b2"}, \code{valid.range = c(10,50)}, which requires that units in the same block be at least 10 units apart, but no more than 50 units apart, on variable \code{"b2"}. As of version 0.5-3, variable restrictions are implemented in all algorithms except \code{optimal}. Note that employing a variable restriction may result in fewer than the maximum possible number of blocks. See \url{https://www.ryantmoore.org/html/software.blockTools.html} for details.
#' 
#' If \code{namesCol = NULL}, then \dQuote{Unit 1}, \dQuote{Unit 2}, \ldots are used.  If \code{level.two = FALSE}, then \code{namesCol} should be of length \code{n.tr}; if \code{level.two = TRUE}, then \code{namesCol} should be of length 2*\code{n.tr}, and in the order shown in the example below.
#' 
#' @param data a dataframe or matrix, with units in rows and variables in columns.
#' @param vcov.data an optional matrix of data used to estimate the variance-covariance matrix for calculating multivariate distance.
#' @param groups an optional column name from \code{data}, specifying subgroups within which blocking occurs.
#' @param n.tr the number of treatment conditions per block.
#' @param id.vars a required string or vector of two strings specifying which column(s) of \code{data} contain identifying information.
#' @param block.vars an optional string or vector of strings specifying which column(s) of \code{data} contain the numeric blocking variables.
#' @param algorithm a string specifying the blocking algorithm. \code{"optGreedy"}, \code{"optimal"}, \code{"naiveGreedy"}, \code{"randGreedy"}, and \code{"sortGreedy"} algorithms are currently available. See Details for more information.
#' @param distance either a) a string defining how the multivariate distance used for blocking is calculated (options include \code{"mahalanobis"}, \code{"mcd"}, \code{"mve"}, and \code{"euclidean"}), or b) a user-defined $k$-by-$k$ matrix of distances, where $k$ is the number of rows in \code{data}.
#' @param weight either a vector of length equal to the number of blocking  variables or a square matrix with dimensions equal to the number of blocking variables used to explicitly weight blocking variables.
#' @param optfactor a number by which distances are multiplied then divided when \code{algorithm = "optimal"}.
#' @param row.sort an optional vector of integers from 1 to \code{nrow(data)} used to sort the rows of data when \code{algorithm = "sortGreedy"}.
#' @param level.two a logical defining the level of blocking.
#' @param valid.var an optional string defining a variable on which units in the same block must fall within the range defined by \code{valid.range}. 
#' @param valid.range an optional vector defining the range of \code{valid.var} within which units in the same block must fall.
#' @param seed.dist an optional integer value for the random seed set in \code{cov.rob}, used to calculate measures of the variance-covariance matrix robust to outliers.
#' @param namesCol an optional vector of column names for the output table.
#' @param verbose a logical specifying whether \code{groups} names and block numbers are printed as blocks are created.
#' @param \dots additional arguments passed to \code{cov.rob}.
#' 
#' @return 
#' A list with elements
#' \itemize{
#'   \item \strong{blocks}: a list of dataframes, each containing a group's blocked units. If there are two treatment conditions, then the last column of each dataframe displays the multivariate distance between the two units. If there are more than two treatment conditions, then the last column of each dataframe displays the largest of the multivariate distances between all possible pairs in the block.
#'   \item \strong{level.two}: a logical indicating whether \code{level.two = TRUE}.
#'   \item \strong{call}: the orginal call to \code{block}.
#' }
#' 
#' @examples
#' data(x100)
#' out <- block(x100, groups = "g", n.tr = 2, id.vars = c("id"), 
#'              block.vars = c("b1", "b2"), algorithm = "optGreedy", 
#'              distance = "mahalanobis", level.two = FALSE, valid.var = "b1", 
#'              valid.range = c(0, 500), verbose = TRUE)
#'              
#' # out$blocks contains 3 data frames
#' 
#' # To illustrate two-level blocking, with multiple level two units per level one unit:
#' 
#' for(i in (1:nrow(x100))){if((i %% 2) == 0){x100$id[i] <- x100$id[i-1]}}
#' 
#' out2 <- block(x100, groups = "g", n.tr = 2, id.vars = c("id", "id2"),
#'               block.vars = c("b1", "b2"), algorithm="optGreedy",
#'               distance = "mahalanobis", level.two = TRUE, valid.var = "b1",
#'               valid.range = c(0,500), namesCol = c("State 1", "City 1", 
#'               "State 2", "City 2"), verbose = TRUE)
#'
#' @references 
#' King, Gary, Emmanuela Gakidou, Nirmala Ravishankar, Ryan T. Moore, Jason Lakin, Manett Vargas, Martha Mar\'ia T\'ellez-Rojo and Juan Eugenio Hern\'andez \'Avila and Mauricio Hern\'andez \'Avila and H\'ector Hern\'andez Llamas.  2007.  "A 'Politically Robust' Experimental Design for Public Policy Evaluation, with Application to the Mexican Universal Health Insurance Program". \emph{Journal of Policy Analysis and Management} 26(3):  479-509.
#' 
#' Moore, Ryan T. 2012. "Multivariate Continuous Blocking to Improve Political Science Experiments." \emph{Political Analysis} 20(4):460-479.
#' 
#' Rousseeuw, Peter J. 1985. "Multivariate Estimation with High Breakdown Point". \emph{Mathematical Statistics and Applications} 8:283-297.
#' 
#' Rousseeuw, Peter J. and Bert C. van Zomeren. 1990. "Unmasking Multivariate Outliers and Leverage Points". \emph{Journal of the American Statistical Association} 85(411):633-639.
#' 
#' @seealso \code{\link{assignment}}, \code{\link{diagnose}}
#' 
#' @author Ryan T. Moore \email{rtm@american.edu} and Keith Schnakenberg \email{keith.schnakenberg@gmail.com}
#' 
#' @keywords design multivariate
#' 
#' @export

block <- function(data, vcov.data = NULL, groups = NULL, n.tr = 2, id.vars,
                  block.vars = NULL, algorithm = "optGreedy", distance =
                  "mahalanobis", weight = NULL, optfactor = 10^7, row.sort = NULL, 
                  level.two = FALSE, valid.var = NULL, valid.range = NULL, seed.dist, 
                  namesCol = NULL, verbose = FALSE, ...){ 
  
  if(is.null(algorithm)){
    stop("Blocking algorithm is unspecified. See documentation for
options.")
  }
  if(!(algorithm %in%  c("optGreedy", "optimal", "naiveGreedy", "randGreedy", "sortGreedy"))){
    stop("Blocking algorithm must be one of the following: optGreedy, optimal, naiveGreedy, randGreedy, sortGreedy")
  }
  if(algorithm == "randGreedy"){    
    row.sort <- sample(seq(1:nrow(data)))
    data <- data[row.sort, ]
  }
  
  if(algorithm == "sortGreedy"){
    if(is.null(row.sort)){
      stop("Blocking algorithm is 'sortGreedy', but vector of new row
positions unspecified.  See documentation for details.")      
    }
    if(length(row.sort)!= nrow(data)){
      stop("Length of vector 'row.sort' does not equal number of rows in
the data.  Respecify 'row.sort'.")
  }
    data <- data[row.sort, ]
  }
  
  if(is.matrix(data)){
    data <- as.data.frame(data)
  }
  
  if(is_tibble(data)){
    data <- as.data.frame(data)
  }
 
  if(is.null(block.vars)){
    block.vars <- names(data)[!(names(data) %in% c(id.vars, groups))]
  }
  if(!is.null(namesCol)){
    if(level.two == FALSE && length(namesCol) != n.tr){
      stop("The length of namesCol should equal n.tr") 
    }else if(level.two == TRUE && length(namesCol) != 2 * n.tr){
      stop("When level.two == TRUE, the length of namesCol should equal 2 * n.tr")
    }
  }

  ## subset appropriate columns for vcov calculation
  if(is.null(vcov.data)){  
    vcov.data <- data[, block.vars]
  }else{
    vcov.data <- vcov.data[, block.vars]
  }
  vcov.data <- as.data.frame(vcov.data)
  ## If ncol(vcov.data) == 1, make data frame and fix name:
  if(ncol(vcov.data) == 1){
    names(vcov.data) <- block.vars
  }
  
  # calculate variance using all groups' units
  if(is.character(distance)){
    # Since mve and mcd require IQR != 0, check, warn, use nonresistant:
    if(distance %in% c("mve", "mcd")){
      iqr.idx <- 0
      while(iqr.idx < length(block.vars)){
        iqr.idx <- iqr.idx + 1
        iqr.tmp <- unname(quantile(vcov.data[, iqr.idx], c(.25, .75)))
        if(isTRUE(all.equal(iqr.tmp[1], iqr.tmp[2]))){
          
          warning(paste("Variable ", colnames(vcov.data)[iqr.idx], 
                        " has IQR 0; blocking will proceed using nonresistant Mahalanobis distance scaling matrix.", sep = ""))
          distance <- "mahalanobis"
          iqr.idx <- length(block.vars)
        }
      }
    }
    
    if(distance == "mahalanobis"){
      # Cut variables with no variation at all:
      blvar.cut <- NULL
      for(blvar.idx in 1:length(block.vars)){
        if(isTRUE(all.equal(var(vcov.data[, block.vars[blvar.idx]]), 0))){
          blvar.cut <- append(blvar.cut, blvar.idx)
        }
      }
      if(length(blvar.cut) != 0){
        
        warning(paste("The following blocking variables have zero variance and are dropped: ", 
                      paste(block.vars[blvar.cut], collapse = ", "), sep = ""))
        
        block.vars <- block.vars[-(blvar.cut)]
      }
      
      # After cutting variables w/o variation, redefine vcov.data:
      vcov.data <- vcov.data[, block.vars]
      
      # Calculate vcov matrix for Mahalanobis scaling:
      vc.all <- var(vcov.data)
    }
    
    if(distance == "mcd"){
      vc.all <- cov.rob(vcov.data, method = "mcd", seed = seed.dist, ...)$cov
    }
    if(distance == "mve"){
      vc.all <- cov.rob(vcov.data, method = "mve", seed = seed.dist, ...)$cov
    }
    if(distance == "euclidean"){
      vc.all <- diag(ncol(vcov.data))
    }
  }
  
  if(!is.null(weight)){
    if(is.vector(weight)){
      if(length(weight) != ncol(vc.all)){
        stop("Weight vector length must equal number of blocking variables.  Respecify 'weight'.")
  		 } 		  		
      weight <- diag(weight)
    }
    if(is.matrix(weight)){
           if(sum(dim(weight) == dim(vc.all)) != 2){
      	stop("Weight matrix dimensions must equal number of blocking variables.  Respecify 'weight'.")
      }
    }
    
    vc.all <- solve(t(solve(t(chol(vc.all)))) %*% weight %*% solve(t(chol(vc.all))))
  }
  
  # counter for groups
  gp <- 0
  
  # list of output for every group
  out <- list()  
  
  if(is.null(groups)){
    data[, "groups"] <- 1 
    groups <- "groups"
  }
  
  if(is.factor(data[, groups])){
    data[, groups] <- as.character(data[, groups])
  }
  gp.names <- unique(data[, groups])
  
  # perform blocking w/in groups
  for(i in unique(data[, groups])){ 
    
    gp <- gp + 1  
    
    if(verbose == TRUE){
      cat("Blocking group ", i, "\n")
    }
    
    data.gp <- data[data[, groups] == i, c(id.vars, block.vars)]

    level.one.names <- data.gp[, id.vars[1]]
    
    # Change factor names to character strings:
    if(is.factor(level.one.names)){
      level.one.names <- as.character(level.one.names)
    }
    
    if(level.two == TRUE){
      if(length(id.vars) < 2){
        stop("Blocking requested at second level, but second level not
identified.  Specify a second ID variable and re-block.")
      }
      row.names(data.gp) <- data.gp[, id.vars[2]]
    }else{
      if(length(unique(data.gp[, id.vars[1]])) !=
         length(data.gp[, id.vars[1]])){
        stop("Blocking requested at top level, but some units have
identical values of the identification variable. Respecify first
identification variable and re-block.")
      }      
      row.names(data.gp) <- data.gp[, id.vars[1]]  
    }
    data.block <- data.frame(data.gp[, !(names(data.gp) %in% id.vars)])
    
    if(is.null(valid.var)){
      valid <- 0
      validvar <- numeric(1)
      validlb <- numeric(1)
      validub <- numeric(1)
    }
    else{
      if(is.null(valid.range)){
        stop("A valid.var has been specified, but the valid.range has not.  
             Specify both or neither and re-block.")
      }
      valid <- 1
      validvar <- data.gp[, valid.var]
      validlb <- valid.range[1]
      validub <- valid.range[2]
    }
  
    if(!is.character(distance)){
            dist.mat <- distance[data[, groups] == i, 
                                 data[, groups] == i]
    }else{
      nnn <- sum(as.integer(data[, groups] == i))
      dist.mat <- matrix(0, nrow = nnn, ncol = nnn)
    }
    if(algorithm != "optimal"){
      if(algorithm == "optGreedy"){
        out1 <- optgreed(x = data.gp,
                         block.vars = block.vars,
                         id.vars = id.vars,
                         dist = dist.mat,
                         vcov = vc.all,
                         n.tr = n.tr,
                         l2 = level.two,
                         l1names = level.one.names,
                         valid = as.integer(valid),
                         validvar = as.double(validvar),
                         validlb = as.double(validlb),
                         validub = as.double(validub),
                         verbose = as.integer(verbose),
                         ismahal=is.character(distance)
                         )
      }
      else if(algorithm  %in%   c("naiveGreedy", "randGreedy", "sortGreedy")){
        out1 <- naive(x = data.gp,
                      block.vars = block.vars,
                      id.vars = id.vars,
                      vcov = vc.all,
                      n.tr = n.tr,
                      l2 = level.two,
                      l1names = level.one.names,
                      valid = as.integer(valid),
                      validvar = as.double(validvar),
                      validlb = as.double(validlb),
                      validub = as.double(validub),
                      verbose=as.integer(verbose),
                      dist = dist.mat,
                      ismahal = is.character(distance)
                      )
      }
    }
    
    if(algorithm == "optimal"){
#      if(require("nbpMatching") == FALSE){
#        stop("The package 'nbpMatching' must be installed to block using the 'optimal' algorithm.")
#      }
#      require("nbpMatching")
    	 if(n.tr > 2){
    	  warning("You specified algorithm = optimal and n.tr > 2.  However, 
    	          optimal blocking only implemented for exactly two treatment 
    	          conditions.  If no other error is encountered, optimal blocks 
    	          for n.tr = 2 are returned here.")
    	 }
      if(is.character(distance)){
        dist.mat <- mahal(data.block, vc.all)
      }else{      
        dist.mat <- distance[data[, groups] == i, data[, groups] == i]
      }

      if(!is.null(valid.var)){
        d.mat <- expand.grid(data.block[, valid.var], data.block[, valid.var])
        diffs <- abs(d.mat[, 1] - d.mat[, 2])
        valid.vec <- (valid.range[1] <= diffs) & (diffs <= valid.range[2])
      }

      dist.mat <- matrix(as.integer(optfactor * dist.mat),
                         nrow = nrow(dist.mat),
                         ncol = ncol(dist.mat))
      
      if(!is.null(valid.var)){
      	warning("You specified algorithm = optimal and valid.var.  
      	        However, valid.var and valid.range are only implemented for other algorithms. 
      	        If no other error is encountered, optimal blocks ignoring the restriction are returned here.")
        # dist.mat[!valid.vec] <- 2147483647 # maximum 32 bit integer
      }

      optimalDistanceMatrix <- nbpMatching::distancematrix(dist.mat)
      optimalOutput <- nbpMatching::nonbimatch(optimalDistanceMatrix, precision = 9)
      optimalOutput$halves$Distance <- as.double(optimalOutput$halves$Distance)/optfactor
                                        #      optimalOutput$halves[, 1] <- level.one.names[optimalOutput$halves[, 1]] 
                                        #      optimalOutput$halves[, 2] <- level.one.names[optimalOutput$halves[, 2]]
      out1 <- optimalOutput$halves
      out1 <- data.frame("Unit 1" = out1$Group1.Row, "Unit 2" = out1$Group2.Row, "Distance" = out1$Distance)
    }

    storage1 <- out1
    storage1[storage1 == 0 & col(storage1) < ncol(storage1)] <- NA
#    storage1[storage1[,1:(ncol(storage1)-1)]==0, 1:(ncol(storage1)-1)] <- NA
    count <- 1

    if(algorithm != "optimal"){
      for(col.idx in 1:(ncol(out1)-1)){
        storage1$temp <- as.character(data.gp[storage1[, col.idx], id.vars[1]])
        storage1$temp2 <- as.character(data.gp[storage1[, col.idx], id.vars[length(id.vars)]])
        names(storage1)[ncol(out1) + count] <- paste("Unit", col.idx)
        count <- count + 1
        names(storage1)[ncol(out1) + count] <- paste("Subunit", col.idx)
        count <- count + 1
      }
    }

    else if(algorithm == "optimal"){
      for(col.idx in 1:(ncol(out1)-1)){
        if(is.null(namesCol)){
          names(storage1)[col.idx] <- paste("Unit", col.idx)
        }else{
          names(storage1)[col.idx] <- namesCol[col.idx]
        }
        storage1[, col.idx] <-  as.character(data.gp[storage1[, col.idx], id.vars[1]])
      }
    }
    
    if(algorithm != "optimal"){
      storage1$Distance <- storage1[, ncol(out1)]
      storage <- storage1[, (ncol(out1)+1):ncol(storage1)]
    }
    else if(algorithm == "optimal"){
      storage <- storage1
    }
    if(n.tr > 2){
      names(storage)[ncol(storage)] <- "Max Distance"
    }
    odd.col <- seq(1:ncol(storage))[seq(1:ncol(storage))%%2 == 1]
    even.col <- (odd.col+1)[1:(length(odd.col)-1)]
    if(level.two == FALSE && algorithm != "optimal"){
      storage <- storage[, odd.col]
    }
    
    # Sort storage by max distance:
    if(nrow(storage) > 1){
      o <- order(as.numeric(as.character(storage[, ncol(storage)])),
                 storage[, 1])
      storage <- data.frame(storage[o,], check.names = FALSE)
    }
    
    # Add rows containing unblocked units in group = gp:
    tmp.names <- unique(unlist(storage[, 1:(ncol(storage)-1)]))
    left.out.names <- level.one.names[!(level.one.names %in% tmp.names)]  
    len.lon <- length(left.out.names)
    if(len.lon > 0){
      left.out.df <- as.data.frame(matrix(c(left.out.names, 
                                            rep(NA, len.lon * (ncol(storage) - 1))), 
                                          len.lon, 
                                          ncol(storage))) 
      names(left.out.df) <- names(storage)
      storage.tmp <- data.frame(rbind(storage, left.out.df))
      names(storage.tmp) <- names(storage)
      storage <- storage.tmp
      rm(storage.tmp)
    }
    if(!is.null(namesCol)){
        names(storage)[1:(ncol(storage) - 1)] <- namesCol
    }
   
    # function to count NA, to remove empty rows (for valid.var)
    sum.na <- function(sum.na.vector){return(sum(is.na(sum.na.vector)))}
    
    # remove empty rows
    storage <- storage[apply(storage[, 1:(ncol(storage) - 1)], 1, sum.na) != (ncol(storage) - 1), ]
    rownames(storage) <- 1:(nrow(storage))
    out[[gp]] <- storage
  } 
  
  names(out) <- gp.names
  
  # sort out by group names
  o <- order(names(out))
  out <- out[o]
  
  output <- list(blocks = out, level.two = level.two)
  output$call <- match.call()  
  class(output) <- "block"
  return(output)
}