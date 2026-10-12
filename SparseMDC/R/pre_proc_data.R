#' Pre-process data
#'
#' This function centers on a gene-by-gene basis, normalizes and/or log
#' transforms the data prior to the application of SparseMDC.For the
#' sequencing depth normalization we recommend that users use one of the many
#' methods developed for normalizing scRNA-Seq data prior to using SparseMDC and
#' so can set \code{norm = FALSE}. However, here we normalize the data by
#' dividing by the total number of reads. This function log transforms the data
#' by applying \code{log(x + 1)} to each of the data sets. By far the most
#' important pre-processing step for SparseMDC is the centralization of the
#' data. Having centralized data is a core component of the SparseMDC algorithm
#' and is necessary for both accurate clustering of the cells and identifying
#' marker genes. We therefore recommend that all users centralize their data
#' using this function and that only experienced users set \code{center = FALSE}.
#'
#'
#' @param dat_l list with D entries, each entry contains data d, p * n matrix.
#' The entries should be ordered according the condition of each dataset. The
#' rows of the data matrix should contain samples while the columns contain
#' features or genes.
#' @param dim Total number of conditions, D.
#' @param norm True/False on if the data should be normalized. This parameter
#' controls whether the data is normalized for sequencing depth by dividing
#' each column by the total number of reads for that sample. We recommend that
#' user use one of the many methods for normalizing scRNA-Seq data and so set
#' this as \code{FALSE}. The default value is \code{FALSE}
#' @param log True/False of if the data should be transformed as log(x + 1).
#' The default value is \code{TRUE}.
#' @param center This parameter controls whether the data is centered on a gene
#' by gene basis. We recommend all users center their data prior to applying
#' SparseMDC and only experienced users should set this as \code{FALSE}. The
#' default value is \code{TRUE}.
#'
#' @return A list with D entries containing the pre-processed data.
#' @export
#' @examples
#' set.seed(10)
#' # Select small dataset for example
#' data_test <- data_biase[1:100,]
#' # Split data into condition A and B
#' data_A <- data_test[ , which(condition_biase == "A")]
#' data_B <- data_test[ , which(condition_biase == "B")]
#' data_C <- data_test[ , which(condition_biase == "C")]
#' # Store data as list
#' dat_l <- list(data_A, data_B, data_C)
#' # Pre-process the data
#' pdat <- pre_proc_data(dat_l, dim=3, norm = FALSE, log = TRUE,
#' center = TRUE)
pre_proc_data <- function(dat_l, dim, norm=FALSE, log=TRUE, center=TRUE){
  dat_l_temp <- dat_l
  if (norm == TRUE){
    for(d in 1:dim){
      dat_sf <- colSums(dat_l_temp[[d]])
      dat_l_temp[[d]] <- as.matrix(dat_l_temp[[d]]) %*% diag(1/dat_sf)
    }
  }
  if (log == TRUE) {
    for(d in 1:dim){
      dat_l_temp[[d]] <- log(dat_l_temp[[d]] + 1)
    }
  }
  if (center == TRUE){
    n_store <- rep(NA, dim)
    c_dat <- dat_l_temp[[1]]
    n_store[1] <- ncol(dat_l_temp[[1]])
    for(d in 2:dim){  # Create pooled data
      n_store[d] <- ncol(dat_l_temp[[d]]) + n_store[d - 1]
      c_dat <- cbind(c_dat, dat_l_temp[[d]])
    }
    c.dat <- t(scale(t(c_dat), scale=F))
    dat_l_temp[[1]] <- c.dat[,1:n_store[1]]
    for(d in 2:dim){
      dat_l_temp[[d]] <- c.dat[,(n_store[d-1] + 1):n_store[d]]
    }
  }
  return(dat_l_temp)
}
