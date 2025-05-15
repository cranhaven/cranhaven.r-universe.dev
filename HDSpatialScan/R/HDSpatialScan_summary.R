########################################################################################################################################################
##' @title Summary of the clusters obtained with a univariate scan function (UG or UNP).
##'
##' @description This function gives a summary of the clusters in a table
##'
##' @param object ResScanOutputUni. Output of a univariate scan function (UG or UNP).
##' @param type_summ character. "param" or "nparam". "param" gives the mean and the sd for each variable in the clusters, outside, and globally and "nparam" gives the Q25, Q50 and Q75 quantiles for each variables in the clusters, outside, and globally.
##' @param digits integer. Number of decimals in the output.
##' @param quantile.type An integer between 1 and 9 (see function quantile). Ignored if type_summ is "param"
##' @param only.MLC logical. Should we summarize only the MLC or all the significant clusters?
##' @param ... Further arguments to be passed to or from methods.
##'
##' @examples
##' \donttest{
##' library(sp)
##' data("map_sites")
##' data("multi_data")
##' uni_data <- multi_data[,1]
##' coords <- coordinates(map_sites)
##' res_unp <- SpatialScan(method = "UNP", data=uni_data, sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2)$UNP
##'
##' summary(object = res_unp, type_summ = "nparam")}
##' \dontshow{
##' library(sp)
##' data("map_sites")
##' data("multi_data")
##' uni_data <- multi_data[,1]
##' coords <- coordinates(map_sites)
##' res_unp <- SpatialScan(method = "UNP", data=uni_data, sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2, MC = 9)$UNP
##' if(length(res_unp$sites_clusters)>0){
##' summary(object = res_unp, type_summ = "nparam")}
##' }
##'
##' @method summary ResScanOutputUni
##'
##' @return No value returned, displays the results in the console
##'
##' @export
##'
summary.ResScanOutputUni <- function(object, type_summ = "param", digits = 3, quantile.type = 7, only.MLC = FALSE, ...){
  html <- FALSE

  if(is.logical(only.MLC) == FALSE){
    stop("only.MLC must be logical")
  }

  if(length(object$sites_clusters) == 0){
    stop("No significant cluster has been detected by the scan procedure")
  }

  if(is.logical(html)==FALSE){
    stop("The argument html must be logical")
  }

  if(!(type_summ%in% c("param", "nparam"))){
    stop("type_summ must be param or nparam")
  }
  if(type_summ == "param"){
    if(only.MLC == FALSE){
      data_summary <- data.frame(matrix(nrow = 2*length(object$sites_clusters)+1, ncol = 1+2))
    }else{
      data_summary <- data.frame(matrix(nrow = 2*1+1, ncol = 1+2))
    }
    columns <- c("Number of sites", "Mean", "Sd")
    colnames(data_summary) <- columns

    temp_mean <- round(mean(object$data), digits = digits)
    temp_sd <- round(sd(object$data), digits = digits)

    complete_mean_sd <- c(temp_mean, temp_sd)

    data_summary[1,] <- c(length(object$data), complete_mean_sd)
    row_names <- c("Overall")
    if(only.MLC == FALSE){
      for(i in 1:length(object$sites_clusters)){

        temp_mean <- round(mean(object$data[object$sites_clusters[[i]]]), digits = digits)
        temp_sd <- round(sd(object$data[object$sites_clusters[[i]]]), digits = digits)
        complete_mean_sd <- c(temp_mean, temp_sd)

        data_summary[2*i,] <- c(length(object$sites_clusters[[i]]), complete_mean_sd)

        temp_mean <- round(mean(object$data[-object$sites_clusters[[i]]]), digits = digits)
        temp_sd <- round(sd(object$data[-object$sites_clusters[[i]]]), digits = digits)
        complete_mean_sd <- c(temp_mean, temp_sd)

        data_summary[2*i+1,] <- c(length(object$data) - length(object$sites_clusters[[i]]), complete_mean_sd)

        row_names <- c(row_names, paste("Inside cluster ", i, sep = ""), paste("Outside cluster ", i, sep = ""))

      }
    }else{
      i <- 1

      temp_mean <- round(mean(object$data[object$sites_clusters[[i]]]), digits = digits)
      temp_sd <- round(sd(object$data[object$sites_clusters[[i]]]), digits = digits)
      complete_mean_sd <- c(temp_mean, temp_sd)

      data_summary[2*i,] <- c(length(object$sites_clusters[[i]]), complete_mean_sd)

      temp_mean <- round(mean(object$data[-object$sites_clusters[[i]]]), digits = digits)
      temp_sd <- round(sd(object$data[-object$sites_clusters[[i]]]), digits = digits)
      complete_mean_sd <- c(temp_mean, temp_sd)

      data_summary[2*i+1,] <- c(length(object$data) - length(object$sites_clusters[[i]]), complete_mean_sd)

      row_names <- c(row_names, paste("Inside cluster ", i, sep = ""), paste("Outside cluster ", i, sep = ""))

    }

  }else{

    if(is.numeric(quantile.type) == "FALSE"){
      stop("quantile.type must be integer")
    }

    if(as.integer(quantile.type) != quantile.type){
      stop("quantile.type must be integer")
    }

    if(quantile.type > 9 | quantile.type < 1){
      stop("quantile.type must be between 1 and 9")
    }

    if(only.MLC == FALSE){
      data_summary <- data.frame(matrix(nrow = 2*length(object$sites_clusters)+1, ncol = 1+3))
    }else{
      data_summary <- data.frame(matrix(nrow = 2*1+1, ncol = 1+3))
    }
    columns <- c("Number of sites", "Q25", "Median", "Q75")

    colnames(data_summary) <- columns

    complete_quantiles <- round((quantile(object$data, probs = c(0.25,0.5,0.75), type = quantile.type)), digits = digits)

    data_summary[1,] <- c(length(object$data), complete_quantiles)

    row_names <- c("Overall")

    if(only.MLC == FALSE){
      for(i in 1:length(object$sites_clusters)){

        complete_quantiles <- round(quantile(object$data[object$sites_clusters[[i]]], probs = c(0.25,0.5,0.75), type = quantile.type), digits = digits)

        data_summary[2*i,] <- c(length(object$sites_clusters[[i]]), complete_quantiles)

        complete_quantiles <- round(quantile(object$data[-object$sites_clusters[[i]]], probs = c(0.25,0.5,0.75), type = quantile.type), digits = digits)

        data_summary[2*i+1,] <- c(length(object$data) - length(object$sites_clusters[[i]]), complete_quantiles)

        row_names <- c(row_names, paste("Inside cluster ", i, sep = ""), paste("Outside cluster ", i, sep = ""))

      }
    }else{
      i <- 1

      complete_quantiles <- round(quantile(object$data[object$sites_clusters[[i]]], probs = c(0.25,0.5,0.75), type = quantile.type), digits = digits)

      data_summary[2*i,] <- c(length(object$sites_clusters[[i]]), complete_quantiles)

      complete_quantiles <- round(quantile(object$data[-object$sites_clusters[[i]]], probs = c(0.25,0.5,0.75), type = quantile.type), digits = digits)

      data_summary[2*i+1,] <- c(length(object$data) - length(object$sites_clusters[[i]]), complete_quantiles)

      row_names <- c(row_names, paste("Inside cluster ", i, sep = ""), paste("Outside cluster ", i, sep = ""))

    }


  }

  rownames(data_summary) <- row_names

  # first data : for each cluster : its pvalue, radius and area if available
  ncol <- 1
  columns <- c("p-value")

  if(sum(is.na(object$radius_clusters)) == 0){
    ncol <- ncol + 1
    columns <- c(columns, "Radius")
  }
  if(sum(is.na(object$areas_clusters)) == 0){
    ncol <- ncol + 1
    columns <- c(columns, "Area")
  }

  if(only.MLC == FALSE){
    first_data <- data.frame(matrix(nrow = length(object$sites_clusters), ncol = ncol))
    colnames(first_data) <- columns
    rownames(first_data) <- paste("Cluster", c(1:length(object$sites_clusters)), sep = " ")

    first_data$`p-value` <- round(object$pval_clusters, digits = digits)
    if(sum(is.na(object$radius_clusters)) == 0){
      first_data$Radius <- round(object$radius_clusters, digits = digits)
    }
    if(sum(is.na(object$areas_clusters)) == 0){
      first_data$Area <- round(object$areas_clusters, digits = digits)
    }
  }else{
    first_data <- data.frame(matrix(nrow = 1, ncol = ncol))
    colnames(first_data) <- columns
    rownames(first_data) <- paste("Cluster", 1, sep = " ")

    first_data$`p-value` <- round(object$pval_clusters[1], digits = digits)
    if(sum(is.na(object$radius_clusters)) == 0){
      first_data$Radius <- round(object$radius_clusters[1], digits = digits)
    }
    if(sum(is.na(object$areas_clusters)) == 0){
      first_data$Area <- round(object$areas_clusters[1], digits = digits)
    }
  }




  if(html == TRUE){
    DT::datatable(t(first_data))
    DT::datatable(t(data_summary))
  }else{
    return(list(basic_summary = data.frame(t(first_data), check.names = FALSE), complete_summary = data.frame(t(data_summary), check.names = FALSE)))
  }

}

########################################################################################################################################################
##' @title Summary of the clusters obtained with a multivariate scan function (MG or MNP).
##'
##' @description This function gives a summary of the clusters in a table
##'
##' @param object ResScanOutputMulti. Output of a multivariate scan function (MG or MNP).
##' @param type_summ character. "param" or "nparam". "param" gives the mean and the sd for each variable in the clusters, outside, and globally and "nparam" gives the Q25, Q50 and Q75 quantiles for each variables in the clusters, outside, and globally.
##' @param digits integer. Number of decimals in output.
##' @param quantile.type An integer between 1 and 9 (see function quantile). Ignored if type_summ is "param"
##' @param only.MLC logical. Should we summarize only the MLC or all the significant clusters?
##' @param ... Further arguments to be passed to or from methods.
##'
##' @examples
##' \donttest{
##' library(sp)
##' data("map_sites")
##' data("multi_data")
##' coords <- coordinates(map_sites)
##' res_mg <- SpatialScan(method = "MG", data=multi_data, sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2)$MG
##' summary(object = res_mg)}
##' \dontshow{
##' library(sp)
##' data("map_sites")
##' data("multi_data")
##' indices <- c(51:75)
##' coords <- coordinates(map_sites[indices,])
##' res_mg <- SpatialScan(method = "MG", data=multi_data[indices,],
##' sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2, MC = 9)$MG
##' if(length(res_mg$sites_clusters)>0){
##' summary(object = res_mg)
##' }
##'
##' }
##'
##'
##' @method summary ResScanOutputMulti
##'
##' @return No value returned, displays the results in the console
##'
##' @export
##'
summary.ResScanOutputMulti <- function(object, type_summ = "param", digits = 3, quantile.type = 7, only.MLC = FALSE, ...){

  html <- FALSE

  if(is.logical(only.MLC) == FALSE){
    stop("only.MLC must be logical")
  }

  if(length(object$sites_clusters) == 0){
    stop("No significant cluster has been detected by the scan procedure")
  }

  if(is.logical(html)==FALSE){
    stop("The argument html must be logical")
  }

  if(is.null(object$variable_names)){
    variable_names <- paste("var", c(1:ncol(object$data)), sep = "")
  }else{
    if(length(object$variable_names) != ncol(object$data)){
      stop("There must be the same number of elements in variable_names than the number of columns in data")
    }
    variable_names <- object$variable_names
  }

  if(!(type_summ%in% c("param", "nparam"))){
    stop("type_summ must be param or nparam")
  }
  if(type_summ == "param"){

    if(only.MLC == FALSE){
      data_summary <- data.frame(matrix(nrow = 2*length(object$sites_clusters)+1, ncol = 1+2*ncol(object$data)))
    }else{
      data_summary <- data.frame(matrix(nrow = 2*1+1, ncol = 1+2*ncol(object$data)))
    }
    columns <- c("Number of sites")
    for(v in 1:ncol(object$data)){
      columns <- c(columns, paste("Mean ", variable_names[v], sep = ""))
      columns <- c(columns, paste("Sd ", variable_names[v], sep = ""))
    }

    colnames(data_summary) <- columns

    temp_mean <- round(colMeans(object$data), digits = digits)
    temp_sd <- round(colSds(object$data), digits = digits)

    complete_mean_sd <- numeric(2*length(temp_mean))
    for(v in 1:length(temp_mean)){
      complete_mean_sd[c(2*(v-1)+1,2*(v-1)+2)] <- c(temp_mean[v], temp_sd[v])
    }


    data_summary[1,] <- c(nrow(object$data), complete_mean_sd)
    row_names <- c("Overall")

    if(only.MLC == FALSE){
      for(i in 1:length(object$sites_clusters)){

        temp_mean <- round(colMeans(object$data[object$sites_clusters[[i]],,drop = FALSE]), digits = digits)
        temp_sd <- round(colSds(object$data[object$sites_clusters[[i]],, drop = FALSE]), digits = digits)
        complete_mean_sd <- numeric(2*length(temp_mean))
        for(v in 1:length(temp_mean)){
          complete_mean_sd[c(2*(v-1)+1,2*(v-1)+2)] <- c(temp_mean[v], temp_sd[v])
        }
        data_summary[2*i,] <- c(length(object$sites_clusters[[i]]), complete_mean_sd)

        temp_mean <- round(colMeans(object$data[-object$sites_clusters[[i]],, drop = FALSE]), digits = digits)
        temp_sd <- round(colSds(object$data[-object$sites_clusters[[i]],, drop = FALSE]), digits = digits)
        complete_mean_sd <- numeric(2*length(temp_mean))
        for(v in 1:length(temp_mean)){
          complete_mean_sd[c(2*(v-1)+1,2*(v-1)+2)] <- c(temp_mean[v], temp_sd[v])
        }

        data_summary[2*i+1,] <- c(nrow(object$data) - length(object$sites_clusters[[i]]), complete_mean_sd)

        row_names <- c(row_names, paste("Inside cluster ", i, sep = ""), paste("Outside cluster ", i, sep = ""))

      }
    }else{
      i <- 1

      temp_mean <- round(colMeans(object$data[object$sites_clusters[[i]],,drop = FALSE]), digits = digits)
      temp_sd <- round(colSds(object$data[object$sites_clusters[[i]],, drop = FALSE]), digits = digits)
      complete_mean_sd <- numeric(2*length(temp_mean))
      for(v in 1:length(temp_mean)){
        complete_mean_sd[c(2*(v-1)+1,2*(v-1)+2)] <- c(temp_mean[v], temp_sd[v])
      }
      data_summary[2*i,] <- c(length(object$sites_clusters[[i]]), complete_mean_sd)

      temp_mean <- round(colMeans(object$data[-object$sites_clusters[[i]],, drop = FALSE]), digits = digits)
      temp_sd <- round(colSds(object$data[-object$sites_clusters[[i]],, drop = FALSE]), digits = digits)
      complete_mean_sd <- numeric(2*length(temp_mean))
      for(v in 1:length(temp_mean)){
        complete_mean_sd[c(2*(v-1)+1,2*(v-1)+2)] <- c(temp_mean[v], temp_sd[v])
      }

      data_summary[2*i+1,] <- c(nrow(object$data) - length(object$sites_clusters[[i]]), complete_mean_sd)

      row_names <- c(row_names, paste("Inside cluster ", i, sep = ""), paste("Outside cluster ", i, sep = ""))


    }

  }else{

    if(is.numeric(quantile.type) == "FALSE"){
      stop("quantile.type must be integer")
    }

    if(as.integer(quantile.type) != quantile.type){
      stop("quantile.type must be integer")
    }

    if(quantile.type > 9 | quantile.type < 1){
      stop("quantile.type must be between 1 and 9")
    }


    if(only.MLC == FALSE){
      data_summary <- data.frame(matrix(nrow = 2*length(object$sites_clusters)+1, ncol = 1+3*ncol(object$data)))
    }else{
      data_summary <- data.frame(matrix(nrow = 2*1+1, ncol = 1+3*ncol(object$data)))
    }
    columns <- c("Number of sites")
    for(v in 1:ncol(object$data)){
      columns <- c(columns, paste("Q25 ", variable_names[v], sep = ""))
      columns <- c(columns, paste("Median ", variable_names[v], sep = ""))
      columns <- c(columns, paste("Q75 ", variable_names[v], sep = ""))
    }

    colnames(data_summary) <- columns

    temp_quantiles <- round((colQuantiles(object$data, probs = c(0.25,0.5,0.75), type = quantile.type)), digits = digits)

    complete_quantiles <- c()
    for(v in 1:nrow(temp_quantiles)){
      complete_quantiles <- c(complete_quantiles, temp_quantiles[v,])
    }

    data_summary[1,] <- c(nrow(object$data), complete_quantiles)

    row_names <- c("Overall")

    if(only.MLC == FALSE){
      for(i in 1:length(object$sites_clusters)){

        temp_quantiles <- round((colQuantiles(object$data[object$sites_clusters[[i]],, drop = FALSE], probs = c(0.25,0.5,0.75), type = quantile.type)), digits = digits)

        complete_quantiles <- c()
        for(v in 1:nrow(temp_quantiles)){
          complete_quantiles <- c(complete_quantiles, temp_quantiles[v,])
        }
        data_summary[2*i,] <- c(length(object$sites_clusters[[i]]), complete_quantiles)

        temp_quantiles <- round((colQuantiles(object$data[-object$sites_clusters[[i]],,drop = FALSE], probs = c(0.25,0.5,0.75), type = quantile.type)), digits = digits)

        complete_quantiles <- c()
        for(v in 1:nrow(temp_quantiles)){
          complete_quantiles <- c(complete_quantiles, temp_quantiles[v,])
        }

        data_summary[2*i+1,] <- c(nrow(object$data) - length(object$sites_clusters[[i]]), complete_quantiles)

        row_names <- c(row_names, paste("Inside cluster ", i, sep = ""), paste("Outside cluster ", i, sep = ""))

      }
    }else{
      i <- 1

      temp_quantiles <- round((colQuantiles(object$data[object$sites_clusters[[i]],, drop = FALSE], probs = c(0.25,0.5,0.75), type = quantile.type)), digits = digits)

      complete_quantiles <- c()
      for(v in 1:nrow(temp_quantiles)){
        complete_quantiles <- c(complete_quantiles, temp_quantiles[v,])
      }
      data_summary[2*i,] <- c(length(object$sites_clusters[[i]]), complete_quantiles)

      temp_quantiles <- round((colQuantiles(object$data[-object$sites_clusters[[i]],,drop = FALSE], probs = c(0.25,0.5,0.75), type = quantile.type)), digits = digits)

      complete_quantiles <- c()
      for(v in 1:nrow(temp_quantiles)){
        complete_quantiles <- c(complete_quantiles, temp_quantiles[v,])
      }

      data_summary[2*i+1,] <- c(nrow(object$data) - length(object$sites_clusters[[i]]), complete_quantiles)

      row_names <- c(row_names, paste("Inside cluster ", i, sep = ""), paste("Outside cluster ", i, sep = ""))


    }

  }

  rownames(data_summary) <- row_names

  # first data : for each cluster : its pvalue, radius and area if available
  ncol <- 1
  columns <- c("p-value")

  if(sum(is.na(object$radius_clusters)) == 0){
    ncol <- ncol + 1
    columns <- c(columns, "Radius")
  }
  if(sum(is.na(object$areas_clusters)) == 0){
    ncol <- ncol + 1
    columns <- c(columns, "Area")
  }

  if(only.MLC == FALSE){
    first_data <- data.frame(matrix(nrow = length(object$sites_clusters), ncol = ncol))
    colnames(first_data) <- columns
    rownames(first_data) <- paste("Cluster", c(1:length(object$sites_clusters)), sep = " ")

    first_data$`p-value` <- round(object$pval_clusters, digits = digits)
    if(sum(is.na(object$radius_clusters)) == 0){
      first_data$Radius <- round(object$radius_clusters, digits = digits)
    }
    if(sum(is.na(object$areas_clusters)) == 0){
      first_data$Area <- round(object$areas_clusters, digits = digits)
    }
  }else{
    first_data <- data.frame(matrix(nrow = 1, ncol = ncol))
    colnames(first_data) <- columns
    rownames(first_data) <- paste("Cluster", 1, sep = " ")

    first_data$`p-value` <- round(object$pval_clusters[1], digits = digits)
    if(sum(is.na(object$radius_clusters)) == 0){
      first_data$Radius <- round(object$radius_clusters[1], digits = digits)
    }
    if(sum(is.na(object$areas_clusters)) == 0){
      first_data$Area <- round(object$areas_clusters[1], digits = digits)
    }
  }


  if(html == TRUE){
    DT::datatable(t(first_data))
    DT::datatable(t(data_summary))
  }else{
    return(list(basic_summary = data.frame(t(first_data), check.names = FALSE), complete_summary = data.frame(t(data_summary), check.names = FALSE)))
  }

}

########################################################################################################################################################
##' @title Summary of the clusters obtained with a univariate functional scan function (PFSS, NPFSS, DFFSS or URBFSS).
##'
##' @description This function gives a summary of the clusters in a table
##'
##' @param object ResScanOutputUniFunct. Output of a univariate functional scan function (PFSS, NPFSS, DFFSS or URBFSS).
##' @param type_summ character. "param" or "nparam". "param" gives the mean and the sd for each variable in the clusters, outside, and globally and "nparam" gives the Q25, Q50 and Q75 quantiles for each variables in the clusters, outside, and globally.
##' @param digits integer. Number of decimals in the output.
##' @param quantile.type An integer between 1 and 9 (see function quantile). Ignored if type_summ is "param"
##' @param only.MLC logical. Should we summarize only the MLC or all the significant clusters?
##' @param ... Further arguments to be passed to or from methods.
##'
##' @examples
##' \donttest{
##' library(sp)
##' data("map_sites")
##' data("funi_data")
##' coords <- coordinates(map_sites)
##'
##' res_npfss <- SpatialScan(method = "NPFSS", data = funi_data, sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2)$NPFSS
##'
##' summary(object = res_npfss, type_summ = "nparam")}
##' \dontshow{
##' library(sp)
##' data("map_sites")
##' data("funi_data")
##' indices <- c(51:75)
##' coords <- coordinates(map_sites[indices,])
##' res_npfss <- SpatialScan(method = "NPFSS", data = funi_data[indices,],
##' sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2, MC = 99)$NPFSS
##' if(length(res_npfss$sites_clusters)>0){
##' summary(object = res_npfss, type_summ = "nparam")
##' }
##'
##' }
##'
##'
##' @method summary ResScanOutputUniFunct
##'
##' @return No value returned, displays the results in the console
##'
##' @export
##'
summary.ResScanOutputUniFunct <- function(object, type_summ = "param", digits = 3, quantile.type = 7, only.MLC = FALSE, ...){
  html <- FALSE

  if(is.logical(only.MLC) == FALSE){
    stop("only.MLC must be logical")
  }

  if(length(object$sites_clusters) == 0){
    stop("No significant cluster has been detected by the scan procedure")
  }

  if(is.logical(html)==FALSE){
    stop("The argument html must be logical")
  }

  if(!(type_summ%in% c("param", "nparam"))){
    stop("type_summ must be param or nparam")
  }
  if(type_summ == "param"){
    if(is.integer(digits) == FALSE & is.numeric(digits) == FALSE){
      stop("digits must be an integer")
    }else{
      if(as.integer(digits) != digits){
        stop("digits must be an integer")
      }
    }

    if(only.MLC == FALSE){
      data_summary <- data.frame(matrix(nrow = 2*length(object$sites_clusters)+1, ncol = 3))
    }else{
      data_summary <- data.frame(matrix(nrow = 2*1+1, ncol = 3))
    }
    colnames(data_summary) <- c("Number of sites", "Mean", "Sd")
    data_summary[1,] <- c(nrow(object$data), round(mean(object$data), digits = digits), round(sd(object$data), digits = digits))

    row_names <- c("Overall")

    if(only.MLC == FALSE){
      for(i in 1:length(object$sites_clusters)){
        data_summary[2*i,] <- c(length(object$sites_clusters[[i]]), round(mean(object$data[object$sites_clusters[[i]],]), digits = digits), round(sd(object$data[object$sites_clusters[[i]],]), digits = digits))
        data_summary[2*i+1,] <- c(nrow(object$data) - length(object$sites_clusters[[i]]), round(mean(object$data[-object$sites_clusters[[i]],]), digits = digits), round(sd(object$data[-object$sites_clusters[[i]],]), digits = digits))
        row_names <- c(row_names, paste("Inside cluster ", i, sep = ""), paste("Outside cluster ", i, sep = ""))
      }
    }else{
      i <- 1
      data_summary[2*i,] <- c(length(object$sites_clusters[[i]]), round(mean(object$data[object$sites_clusters[[i]],]), digits = digits), round(sd(object$data[object$sites_clusters[[i]],]), digits = digits))
      data_summary[2*i+1,] <- c(nrow(object$data) - length(object$sites_clusters[[i]]), round(mean(object$data[-object$sites_clusters[[i]],]), digits = digits), round(sd(object$data[-object$sites_clusters[[i]],]), digits = digits))
      row_names <- c(row_names, paste("Inside cluster ", i, sep = ""), paste("Outside cluster ", i, sep = ""))

    }

  }else{

    if(is.numeric(quantile.type) == "FALSE"){
      stop("quantile.type must be integer")
    }

    if(as.integer(quantile.type) != quantile.type){
      stop("quantile.type must be integer")
    }

    if(quantile.type > 9 | quantile.type < 1){
      stop("quantile.type must be between 1 and 9")
    }


    if(only.MLC == FALSE){
      data_summary <- data.frame(matrix(nrow = 2*length(object$sites_clusters)+1, ncol = 4))
    }else{
      data_summary <- data.frame(matrix(nrow = 2*1+1, ncol = 4))
    }

    colnames(data_summary) <- c("Number of sites", "Q25", "Median", "Q75")

    data_summary[1,] <- c(nrow(object$data), round(as.numeric(quantile(object$data, probs = c(0.25,0.5,0.75)), type = quantile.type), digits = digits))

    row_names <- c("Overall")

    if(only.MLC == FALSE){
      for(i in 1:length(object$sites_clusters)){
        data_summary[2*i,] <- c(length(object$sites_clusters[[i]]), round(as.numeric(quantile(object$data[object$sites_clusters[[i]],], probs = c(0.25,0.5,0.75), type = quantile.type)), digits = digits))
        data_summary[2*i+1,] <- c(nrow(object$data) - length(object$sites_clusters[[i]]), round(as.numeric(quantile(object$data[-object$sites_clusters[[i]],], probs = c(0.25,0.5,0.75), type = quantile.type)), digits = digits))
        row_names <- c(row_names, paste("Inside cluster ", i, sep = ""), paste("Outside cluster ", i, sep = ""))
      }
    }else{
      i <- 1
      data_summary[2*i,] <- c(length(object$sites_clusters[[i]]), round(as.numeric(quantile(object$data[object$sites_clusters[[i]],], probs = c(0.25,0.5,0.75), type = quantile.type)), digits = digits))
      data_summary[2*i+1,] <- c(nrow(object$data) - length(object$sites_clusters[[i]]), round(as.numeric(quantile(object$data[-object$sites_clusters[[i]],], probs = c(0.25,0.5,0.75), type = quantile.type)), digits = digits))
      row_names <- c(row_names, paste("Inside cluster ", i, sep = ""), paste("Outside cluster ", i, sep = ""))

    }

  }

  rownames(data_summary) <- row_names

  # first data : for each cluster : its pvalue, radius and area if available
  ncol <- 1
  columns <- c("p-value")

  if(sum(is.na(object$radius_clusters)) == 0){
    ncol <- ncol + 1
    columns <- c(columns, "Radius")
  }
  if(sum(is.na(object$areas_clusters)) == 0){
    ncol <- ncol + 1
    columns <- c(columns, "Area")
  }

  if(only.MLC == FALSE){
    first_data <- data.frame(matrix(nrow = length(object$sites_clusters), ncol = ncol))
    colnames(first_data) <- columns
    rownames(first_data) <- paste("Cluster", c(1:length(object$sites_clusters)), sep = " ")

    first_data$`p-value` <- round(object$pval_clusters, digits = digits)
    if(sum(is.na(object$radius_clusters)) == 0){
      first_data$Radius <- round(object$radius_clusters, digits = digits)
    }
    if(sum(is.na(object$areas_clusters)) == 0){
      first_data$Area <- round(object$areas_clusters, digits = digits)
    }
  }else{
    first_data <- data.frame(matrix(nrow = 1, ncol = ncol))
    colnames(first_data) <- columns
    rownames(first_data) <- paste("Cluster", 1, sep = " ")

    first_data$`p-value` <- round(object$pval_clusters[1], digits = digits)
    if(sum(is.na(object$radius_clusters)) == 0){
      first_data$Radius <- round(object$radius_clusters[1], digits = digits)
    }
    if(sum(is.na(object$areas_clusters)) == 0){
      first_data$Area <- round(object$areas_clusters[1], digits = digits)
    }
  }


  if(html == TRUE){
    DT::datatable(t(first_data))
    DT::datatable(t(data_summary))
  }else{
    return(list(basic_summary = data.frame(t(first_data), check.names = FALSE), complete_summary = data.frame(t(data_summary), check.names = FALSE)))
  }

}

########################################################################################################################################################
##' @title Summary of the clusters obtained with a multivariate functional scan function (MPFSS, NPFSS, MDFFSS or MRBFSS).
##'
##' @description This function gives a summary of the clusters in a table
##'
##' @param object ResScanOutputMultiFunct. Output of an multivariate functional scan function (MPFSS, NPFSS, MDFFSS or MRBFSS).
##' @param type_summ character. "param" or "nparam". "param" gives the mean and the sd for each variable in the clusters, outside, and globally and "nparam" gives the Q25, Q50 and Q75 quantiles for each variables in the clusters, outside, and globally.
##' @param digits integer. Number of decimals in the output.
##' @param quantile.type An integer between 1 and 9 (see function quantile). Ignored if type_summ is "param"
##' @param only.MLC logical. Should we summarize only the MLC or all the significant clusters?
##' @param ... Further arguments to be passed to or from methods.
##'
##' @examples
##' \donttest{
##' library(sp)
##' data("map_sites")
##' data("fmulti_data")
##' coords <- coordinates(map_sites)
##'
##' res_npfss <- SpatialScan(method = "NPFSS", data = fmulti_data, sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2)$NPFSS
##'
##' summary(object = res_npfss, type_summ = "nparam")}
##' \dontshow{
##' library(sp)
##' data("map_sites")
##' data("fmulti_data")
##' indices <- c(51:75)
##' coords <- coordinates(map_sites[indices,])
##' res_npfss <- SpatialScan(method = "NPFSS", data = fmulti_data[indices],
##' sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2, MC = 99)$NPFSS
##' if(length(res_npfss$sites_clusters)>0){
##' summary(object = res_npfss, type_summ = "nparam")
##' }
##' }
##'
##' @method summary ResScanOutputMultiFunct
##'
##' @return No value returned, displays the results in the console
##'
##' @export
##'
summary.ResScanOutputMultiFunct <- function(object, type_summ = "param", digits = 3, quantile.type = 7, only.MLC = FALSE, ...){
  html <- FALSE

  if(is.logical(only.MLC) == FALSE){
    stop("only.MLC must be logical")
  }


  if(length(object$sites_clusters) == 0){
    stop("No significant cluster has been detected by the scan procedure")
  }

  if(is.logical(html)==FALSE){
    stop("The argument html must be logical")
  }

  if(is.null(object$variable_names)){
    variable_names <- paste("var", c(1:nrow(object$data[[1]])), sep = "")
  }else{
    if(length(object$variable_names) != nrow(object$data[[1]])){
      stop("There must be the same number of elements in variable_names than the number of variables in data")
    }
    variable_names <- object$variable_names
  }

  if(!(type_summ%in% c("param", "nparam"))){
    stop("type_summ must be param or nparam")
  }
  if(type_summ == "param"){

    if(only.MLC == FALSE){
      data_summary <- data.frame(matrix(nrow = 2*length(object$sites_clusters)+1, ncol = 1 + 2*nrow(object$data[[1]])))
    }else{
      data_summary <- data.frame(matrix(nrow = 2*1+1, ncol = 1 + 2*nrow(object$data[[1]])))
    }
    columns <- c("Number of sites")
    for(v in 1:nrow(object$data[[1]])){
      columns <- c(columns, paste("Mean ", variable_names[v], sep = ""))
      columns <- c(columns, paste("Sd ", variable_names[v], sep = ""))
    }

    colnames(data_summary) <- columns
    data_summary[1,1] <- c(length(object$data))
    row_names <- c("Overall")

    if(only.MLC == FALSE){
      for(i in 1:length(object$sites_clusters)){
        data_summary[2*i,1] <- c(length(object$sites_clusters[[i]]))
        data_summary[2*i+1,1] <- c(length(object$data) - length(object$sites_clusters[[i]]))
        row_names <- c(row_names, paste("Inside cluster ", i, sep = ""), paste("Outside cluster ", i, sep = ""))

      }
    }else{
      i <- 1
      data_summary[2*i,1] <- c(length(object$sites_clusters[[i]]))
      data_summary[2*i+1,1] <- c(length(object$data) - length(object$sites_clusters[[i]]))
      row_names <- c(row_names, paste("Inside cluster ", i, sep = ""), paste("Outside cluster ", i, sep = ""))

    }

    for(v in 1:nrow(object$data[[1]])){
      temp <- matrix(ncol = ncol(object$data[[1]]), nrow = length(object$data))
      for(l in 1:length(object$data)){
        temp[l,] <- object$data[[l]][v,]
      }
      data_summary[1,c(2*v, 1+2*v)] <- c(round(mean(temp), digits = digits), round(sd(temp), digits = digits))

      if(only.MLC == FALSE){
        for(i in 1:length(object$sites_clusters)){
          data_summary[2*i,c(2*v, 1+2*v)] <- c(round(mean(temp[object$sites_clusters[[i]],, drop = FALSE]), digits = digits), round(sd(temp[object$sites_clusters[[i]],, drop = FALSE]), digits = digits))
          data_summary[2*i+1,c(2*v, 1+2*v)] <- c(round(mean(temp[-object$sites_clusters[[i]],, drop = FALSE]), digits = digits), round(sd(temp[-object$sites_clusters[[i]],, drop = FALSE]), digits = digits))
        }
      }else{
        i <- 1
        data_summary[2*i,c(2*v, 1+2*v)] <- c(round(mean(temp[object$sites_clusters[[i]],, drop = FALSE]), digits = digits), round(sd(temp[object$sites_clusters[[i]],, drop = FALSE]), digits = digits))
        data_summary[2*i+1,c(2*v, 1+2*v)] <- c(round(mean(temp[-object$sites_clusters[[i]],, drop = FALSE]), digits = digits), round(sd(temp[-object$sites_clusters[[i]],, drop = FALSE]), digits = digits))

      }

    }

  }else{

    if(is.numeric(quantile.type) == "FALSE"){
      stop("quantile.type must be integer")
    }

    if(as.integer(quantile.type) != quantile.type){
      stop("quantile.type must be integer")
    }

    if(quantile.type > 9 | quantile.type < 1){
      stop("quantile.type must be between 1 and 9")
    }

    if(only.MLC == FALSE){
      data_summary <- data.frame(matrix(nrow = 2*length(object$sites_clusters)+1, ncol = 1 + 3*nrow(object$data[[1]])))
    }else{
      data_summary <- data.frame(matrix(nrow = 2*1+1, ncol = 1 + 3*nrow(object$data[[1]])))
    }
    columns <- c("Number of sites")
    for(v in 1:nrow(object$data[[1]])){
      columns <- c(columns, paste("Q25 ", variable_names[v], sep = ""))
      columns <- c(columns, paste("Median ", variable_names[v], sep = ""))
      columns <- c(columns, paste("Q75 ", variable_names[v], sep = ""))

    }

    colnames(data_summary) <- columns
    data_summary[1,1] <- c(length(object$data))
    row_names <- c("Overall")

    if(only.MLC == FALSE){
      for(i in 1:length(object$sites_clusters)){
        data_summary[2*i,1] <- c(length(object$sites_clusters[[i]]))
        data_summary[2*i+1,1] <- c(length(object$data) - length(object$sites_clusters[[i]]))
        row_names <- c(row_names, paste("Inside cluster ", i, sep = ""), paste("Outside cluster ", i, sep = ""))

      }
    }else{
      i <- 1
      data_summary[2*i,1] <- c(length(object$sites_clusters[[i]]))
      data_summary[2*i+1,1] <- c(length(object$data) - length(object$sites_clusters[[i]]))
      row_names <- c(row_names, paste("Inside cluster ", i, sep = ""), paste("Outside cluster ", i, sep = ""))

    }

    for(v in 1:nrow(object$data[[1]])){
      temp <- matrix(ncol = ncol(object$data[[1]]), nrow = length(object$data))
      for(l in 1:length(object$data)){
        temp[l,] <- object$data[[l]][v,]
      }
      data_summary[1,c(3*v-1, 3*v, 3*v+1)] <- round(as.numeric(c(quantile(temp, probs = c(0.25, 0.5, 0.75), type = quantile.type))), digits = digits)
      if(only.MLC == FALSE){
        for(i in 1:length(object$sites_clusters)){
          data_summary[2*i,c(3*v-1, 3*v, 3*v+1)] <- round(as.numeric(c(quantile(temp[object$sites_clusters[[i]],, drop = FALSE], probs = c(0.25, 0.5, 0.75), type = quantile.type))), digits = digits)
          data_summary[2*i+1,c(3*v-1, 3*v, 3*v+1)] <- round(as.numeric(c(quantile(temp[-object$sites_clusters[[i]],, drop = FALSE], probs = c(0.25, 0.5, 0.75), type = quantile.type))), digits = digits)
        }
      }else{
        i <- 1
        data_summary[2*i,c(3*v-1, 3*v, 3*v+1)] <- round(as.numeric(c(quantile(temp[object$sites_clusters[[i]],, drop = FALSE], probs = c(0.25, 0.5, 0.75), type = quantile.type))), digits = digits)
        data_summary[2*i+1,c(3*v-1, 3*v, 3*v+1)] <- round(as.numeric(c(quantile(temp[-object$sites_clusters[[i]],, drop = FALSE], probs = c(0.25, 0.5, 0.75), type = quantile.type))), digits = digits)

      }

    }
  }

  rownames(data_summary) <- row_names

  # first data : for each cluster : its pvalue, radius and area if available
  ncol <- 1
  columns <- c("p-value")

  if(sum(is.na(object$radius_clusters)) == 0){
    ncol <- ncol + 1
    columns <- c(columns, "Radius")
  }
  if(sum(is.na(object$areas_clusters)) == 0){
    ncol <- ncol + 1
    columns <- c(columns, "Area")
  }

  if(only.MLC == FALSE){
    first_data <- data.frame(matrix(nrow = length(object$sites_clusters), ncol = ncol))
    colnames(first_data) <- columns
    rownames(first_data) <- paste("Cluster", c(1:length(object$sites_clusters)), sep = " ")

    first_data$`p-value` <- round(object$pval_clusters, digits = digits)
    if(sum(is.na(object$radius_clusters)) == 0){
      first_data$Radius <- round(object$radius_clusters, digits = digits)
    }
    if(sum(is.na(object$areas_clusters)) == 0){
      first_data$Area <- round(object$areas_clusters, digits = digits)
    }
  }else{
    first_data <- data.frame(matrix(nrow = 1, ncol = ncol))
    colnames(first_data) <- columns
    rownames(first_data) <- paste("Cluster", 1, sep = " ")

    first_data$`p-value` <- round(object$pval_clusters[1], digits = digits)
    if(sum(is.na(object$radius_clusters)) == 0){
      first_data$Radius <- round(object$radius_clusters[1], digits = digits)
    }
    if(sum(is.na(object$areas_clusters)) == 0){
      first_data$Area <- round(object$areas_clusters[1], digits = digits)
    }
  }


  if(html == TRUE){
    DT::datatable(t(first_data))
    DT::datatable(t(data_summary))
  }else{
    return(list(basic_summary = data.frame(t(first_data), check.names = FALSE), complete_summary = data.frame(t(data_summary), check.names = FALSE)))
  }

}
