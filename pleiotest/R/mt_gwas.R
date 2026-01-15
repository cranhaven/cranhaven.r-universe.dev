#' @title Multi-trait Genome wide association model.
#' @description Performs a multi-trait model with correlated errors (seemingly unrelated regressions), and generates results by trait in a list.
#' @param pleio_results object of class pleio_class (returned by pleioR() function).
#' @param save_at character with directory and/or file name (.rdata) to save the results. This is useful when handling multiple results such as in parallel jobs.
#' @return list with by trait dataframes that contain results of the multi-trait model.
#' @export
mt_gwas <- function(pleio_results, save_at = NULL){
  if (!'pleio_class' %in% class(pleio_results))
    stop('pleio_results should be a pleio_class object')

  n_traits <- nrow(pleio_results[[1]]$betas)
  n_snp <- length(pleio_results)

  n <- vector()
  allele_freq <- vector()
  estimate <- matrix(nrow = n_snp, ncol = n_traits)
  s_error <- matrix(nrow = n_snp, ncol = n_traits)

  for (i in 1:n_snp){
    result <- pleio_results[[i]]
    n[i] <- result$n
    allele_freq[i] <- result$allele_freq
    estimate[i,] <- result$betas
    s_error[i,] <- sqrt(diag(result$lhss))
  }

  t_stat <- estimate / s_error

  result_list <- list()
  for (j in 1:n_traits){
    result_list[[j]] <- cbind(allele_freq, n, estimate[,j], s_error[,j], t_stat[,j], stats::pt(q = abs(t_stat[,j]), df = n - 2, lower.tail = F) * 2)
    colnames(result_list[[j]]) <- c('allele_freq', 'n', "estimate", "se", "t value", "p value")
    rownames(result_list[[j]]) <- names(pleio_results)
  }
  names(result_list) <- rownames(pleio_results[[1]]$betas)

  if (!is.null(save_at)){
    if(!is.character(save_at))
      stop('Save at must be a character string with a directory or file name')
    if(length(grep('.rdata', save_at, ignore.case = T)) > 0){
      file_name <- strsplit(casefold(basename(save_at)), '.rdata')[[1]]
      dir_name <- dirname(save_at)
    }else{
      if(length(grep('\\.', save_at)) > 0)
        stop('The file must be .rdata')
      dir_name <- save_at
      file_name <- 'mt_gwas_result'
    }

    if(!substring(dir_name, nchar(dir_name)) == '/')
      dir_name <- paste0(dir_name, '/')

    if(!dir.exists(dir_name))
      dir.create(dir_name)

    i <- 1
    while (file.exists(paste0(dir_name, file_name, '_', i, '.rdata')))
      i <- i + 1
    save(result_list, file = paste0(dir_name, file_name, '_', i, '.rdata'))
  }

  return(result_list)
}

