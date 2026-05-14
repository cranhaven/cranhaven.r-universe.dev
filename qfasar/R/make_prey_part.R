#' Make prey partition
#'
#' The function \code{make_prey_part} partitions a prey library into clusters
#' based on user specifications informed by the results of a call to the
#' function \code{dimac}.
#'
#' @param sig A matrix of scaled signatures ready for analysis, intended to be
#'   the object \code{sig_scale} returned by the function \code{prep_sig}.
#' @param clust A data frame containing cluster definitions, intended to be the
#'   object \code{clust} returned by the function \code{dimac}.
#' @param n_clust An integer vector constructed by the user to specify the
#'   number of clusters into which each prey type should be partitioned.
#'
#' @return A list containing the following elements: \describe{
#'   \item{type}{A character vector of the partitioned type of each signature.}
#'   \item{id}{A character vector of the unique sample ID of each signature.}
#'   \item{n_types}{The number of unique types in the partitioned library.}
#'   \item{uniq_types}{A character vector of the unique types, sorted
#'     alphanumerically.}
#'   \item{type_ss}{The number of signatures for each unique \code{type}.}
#'   \item{loc}{A vector or matrix giving the first and last locations of the
#'     signatures of each \code{type}, after being sorted by \code{type} and
#'     \code{id}.}
#'   \item{sig_part}{A matrix of partitioned signatures ready for analysis,
#'     sorted by \code{type} and \code{id}, in column-major format.}
#'   \item{pool_pre}{A matrix to pre-multiply diet estimates associated with a
#'     partitioned library to pool estimates back to the original prey types.}
#'   \item{pool_post}{A matrix to post-multiply diet estimates associated with a
#'     partitioned library to pool estimates back to the original prey types.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string contains a brief summary of the execution.}
#'   }
#'
#' @section Details:
#' The function \code{make_prey_part} partitions a matrix of prey signatures
#' and into a larger number of prey types based on user input (in the vector
#' \code{n_clust}) informed by the results of a preceding call to the clustering
#' function \code{dimac}.  The signatures in \code{sig} are presumed to be
#' ready for analysis, which is best accomplished by a call to the function
#' \code{prep_sig}.
#'
#' For each prey type, the column in \code{clust} designated by the
#' corresponding integer in \code{n_clust} is accessed and used to partition the
#' prey type.  For example, if the element of \code{n_clust} is 1, the first
#' column of \code{clust} is accessed and the prey type is not partitioned.  If
#' the element of \code{n_clust} is 3, the third column of \code{clust} is
#' accessed and the prey type is partitioned into three clusters.
#'
#' The length of the integer vector \code{n_clust} must equal the number of
#' unique types in \code{type}.  The integers themselves should be between 1 and
#' the number of signatures of each type.
#'
#' After all prey types have been partitioned, the prey signatures are sorted by
#' \code{type} and \code{id}.  The matrix \code{rep_grp} is created to map the
#' new prey types to the original prey types.  Multiplying diet estimates
#' corresponding to a partitioned prey library \code{sig_part} by \code{rep_grp}
#' pools the diet estimates into the original prey types.
#'
#' Please refer to the vignette and documentation for the functions
#' \code{\link{dimac}} and \code{\link{prep_sig}} for additional
#' information.
#'
#' @examples
#' make_prey_part(sig = matrix(c(0.01, 0.20, 0.30, 0.49,
#'                               0.05, 0.14, 0.39, 0.42,
#'                               0.07, 0.21, 0.28, 0.44,
#'                               0.04, 0.19, 0.34, 0.43,
#'                               0.12, 0.29, 0.39, 0.20,
#'                               0.15, 0.28, 0.34, 0.23,
#'                               0.17, 0.21, 0.31, 0.31,
#'                               0.18, 0.22, 0.28, 0.32), ncol = 8),
#'                data.frame(type = c("prey_1", "prey_1", "prey_1", "prey_2",
#'                                    "prey_2", "prey_2", "prey_2", "prey_2"),
#'                           id = c("1-1", "1-2", "1-3", "2-1",
#'                                  "2-2", "2-3", "2-4", "2-5"),
#'                           clust_1 = c(1, 1, 1, 1, 1, 1, 1, 1),
#'                           clust_2 = c(1, 2, 1, 2, 1, 1, 2, 2),
#'                           clust_3 = c(1, 2, 3, 3, 1, 2, 3, 3),
#'                           clust_4 = c(0, 0, 0, 4, 1, 2, 3, 4)),
#'                n_clust = c(1, 2))
#'
#' @export
#'
################################################################################


make_prey_part <- function(sig, clust, n_clust){


  # Error check inputs ---------------------------------------------------------

  # Initialize objects returned.
  type <- NA
  id <- NA
  n_types <- NA
  uniq_types <- NA
  type_ss <- NA
  loc <- NA
  sig_part <- NA
  pool_pre <- NA
  pool_post <- NA


  # Check that sig is a numeric matrix.
  if(!(is.numeric(sig) & is.matrix(sig))){
    err_code <- 1
    err_message <- "The argument sig is not a numeric matrix!"

    return(list(type = type,
                id = id,
                n_types = n_types,
                uniq_types = uniq_types,
                type_ss = type_ss,
                loc = loc,
                sig_part = sig_part,
                pool_pre = pool_pre,
                pool_post = pool_post,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that the signatures are non-negative and their sums do not exceed 1.
  # If sig_prep() was called with scale = 2, the signatures will not sum to 1.
  if((min(sig) < 0) | is.na(min(sig)) | (max(apply(sig, 2, sum)) > 1)){
    err_code <- 2
    err_message <- "Signatures have not been properly prepared by sig_prep()!"

    return(list(type = type,
                id = id,
                n_types = n_types,
                uniq_types = uniq_types,
                type_ss = type_ss,
                loc = loc,
                sig_part = sig_part,
                pool_pre = pool_pre,
                pool_post = pool_post,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that clust is a valid data frame.
  if(!(is.data.frame(clust) | is.list(clust))){
    err_code <- 3
    err_message <- "The cluster definitions are not in a valid data frame!"

    return(list(type = type,
                id = id,
                n_types = n_types,
                uniq_types = uniq_types,
                type_ss = type_ss,
                loc = loc,
                sig_part = sig_part,
                pool_pre = pool_pre,
                pool_post = pool_post,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that the first two columns are factors and the remaining columns are
  # numeric.
  are_numeric <- TRUE
  for(li1 in 3:ncol(clust)){
    if(mode(clust[,li1]) != "numeric"){
      are_numeric <- FALSE
      break
    }
  }

  if(!(is.factor(clust[,1]) & is.factor(clust[,2]) & are_numeric)){

    # There is an error in the format of the cluster definitions.
    err_code <- 4
    err_message <- "The cluster definitions are not properly formatted."

    return(list(type = type,
                id = id,
                n_types = n_types,
                uniq_types = uniq_types,
                type_ss = type_ss,
                loc = loc,
                sig_part = sig_part,
                pool_pre = pool_pre,
                pool_post = pool_post,
                err_code = err_code,
                err_message = err_message))
  }
  rm(are_numeric)


  # Check that the length of n_clust matches the number of original types.
  if(length(unique(clust[,1])) != length(n_clust)){
    err_code <- 5
    err_message <- paste("The length of n_clust does not equal",
                         "the number of prey types in clust!", sep = " ")

    return(list(type = type,
                id = id,
                n_types = n_types,
                uniq_types = uniq_types,
                type_ss = type_ss,
                loc = loc,
                sig_part = sig_part,
                pool_pre = pool_pre,
                pool_post = pool_post,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that the number of clusters does not exceed the sample size.
  if(!as.logical(min(table(clust[,1]) >= n_clust))){
    err_code <- 6
    err_message <- "Number of clusters cannot exceed prey-type sample size!"

    return(list(type = type,
                id = id,
                n_types = n_types,
                uniq_types = uniq_types,
                type_ss = type_ss,
                loc = loc,
                sig_part = sig_part,
                pool_pre = pool_pre,
                pool_post = pool_post,
                err_code = err_code,
                err_message = err_message))
  }



  # Construct new prey types ---------------------------------------------------

  # Find signature locations for original prey types.
  n_old_types <- length(n_clust)
  old_loc <- matrix(0, nrow=n_old_types, ncol=2)
  old_ss <- table(clust[,1])
  old_loc[1,1] <- 1
  old_loc[1,2] <- old_ss[1]
  if(n_old_types > 1){
    for(li1 in 2:n_old_types){
      old_loc[li1,1] <- old_loc[li1-1,2] + 1
      old_loc[li1,2] <- old_loc[li1-1,2] + old_ss[li1]
    }
  }


  # Construct cluster definitions for each prey type.
  clust_def <- vector(mode = "integer", length = ncol(sig))
  for(li1 in 1:n_old_types){
    clust_def[old_loc[li1,1]:old_loc[li1,2]] <-
                           clust[old_loc[li1,1]:old_loc[li1,2],(n_clust[li1]+2)]
  }


  # Create new prey types.
  type <- paste(clust[,1], clust_def, sep = " ")
  id <- clust[,2]
  uniq_types <- unique(type)
  uniq_types <- uniq_types[order(uniq_types)]
  n_types <- length(uniq_types)
  type_ss <- table(type)


  # Order by type and id.
  new_order <- order(type, id)
  sig_part <- sig[,new_order]
  rownames(sig_part) <- rownames(sig)
  colnames(sig_part) <- paste(type[new_order], trimws(id[new_order]),
                              sep = " - ")


  # Construct new location matrix.
  loc <- matrix(0, nrow=n_types, ncol=2)
  loc[1,1] <- 1
  loc[1,2] <- type_ss[1]
  if(n_types > 1){
    for(li1 in 2:n_types){
      loc[li1,1] <- loc[li1-1,2] + 1
      loc[li1,2] <- loc[li1-1,2] + type_ss[li1]
    }
  }
  rownames(loc) <- uniq_types



  # Create pooling matrices ----------------------------------------------------
  pool_pre <- matrix(data = 0, nrow = n_old_types, ncol = n_types)
  pool_pre[1,1:n_clust[1]] <- type_ss[1:n_clust[1]]/old_ss[1]
  dum <- n_clust[1]
  for(li1 in 2:n_old_types){
      first <- dum + 1
      last <- dum + n_clust[li1]
      pool_pre[li1,first:last] <- type_ss[first:last]/old_ss[li1]
      dum <- dum + n_clust[li1]
    }
  rownames(pool_pre) <- unique(clust[,1])
  colnames(pool_pre) <- uniq_types

  pool_post <- t(pool_pre)
  pool_post[pool_post > 0] <- 1



  # Return ---------------------------------------------------------------------
  err_code <- 0
  err_message <- "Success!"

  return(list(type = type,
              id = id,
              n_types = n_types,
              uniq_types = uniq_types,
              type_ss = type_ss,
              loc = loc,
              sig_part = sig_part,
              pool_pre = pool_pre,
              pool_post = pool_post,
              err_code = err_code,
              err_message = err_message))
}
