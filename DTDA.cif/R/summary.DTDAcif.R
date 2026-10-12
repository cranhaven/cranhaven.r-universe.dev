#' S3 method to summarize a DTDAcif object by using the generic summary function.
#'
#' @title summary.DTDAcif
#'
#' @aliases summary.DTDAcif
#'
#' @param object DTDAcif object.
#' @param ... Additonal parameters.
#'
#'
#' @author
#' \itemize{
#' \item{de Uña-Álvarez, Jacobo.}
#' \item{Soage González, José Carlos.}
#' \item{Maintainer: José Carlos Soage González. \email{jsoage@@uvigo.es}}
#' }
#'
#'
#' @export
summary.DTDAcif <- function(object, ...){

  res <- NULL
  res <- object
  nz <- length(unique(res$data$z))

  if (is.null(res$method)) {

    if(is.null(res$sd.boot)){
      df <- data.frame(unique(data.frame(res$data$x[order(res$data$x)])),
                       res$biasf[order(as.numeric(rownames(unique(data.frame(res$data$x)))))],
                       cumsum(res$cif.mas[order(res$data$x)])[as.numeric(rownames(unique(data.frame(res$data$x))))])
      colnames(df) <- c("time", "biasf", "cif")
      return(df)
    } else {

      df <- data.frame(unique(data.frame(res$data$x[order(res$data$x)])),
                       res$biasf[order(as.numeric(rownames(unique(data.frame(res$data$x)))))],
                       cumsum(res$cif[order(res$data$x)])[as.numeric(rownames(unique(data.frame(res$data$x))))],
                       res$sd.boot)
      colnames(df) <- c("time", "biasf", "cif", "sd.boot")
      return(df)
    }

  } else {

    if (res$method == "dep") {

      x   <- vector("list", length(unique(res$data$z)))

      for (i in  length(unique(res$data$z)):1) {

        x[i]   <- list(res$data$x[res$data$z == i])

      }
      names(x) <- paste0("time_", 1:length(unique(res$data$z)))

      df <- list()
      if(is.null(res$sd.boot)) {

        for(i in 1:length(res$cif.mas)) {

          df[[i]] <-  cbind(unique(data.frame(x[i][order(x[i])])),
                            data.frame(res$biasf[i])[as.numeric(rownames(unique(data.frame(x[i])))), ],
                            cumsum(unlist(res$cif.mas[i][order(res$data$x)]))[as.numeric(rownames(unique(data.frame(x[i]))))])
          names(df[[i]]) <- c("x", "biasf", "cif")
          }

        names(df) <- paste0("Comp_event_", 1:length(res$cif.mas))
        return(df)

        for(i in 1:length(res$cif.mas)) {
          print(df[[i]], sep = "\n")
        }

      } else {

        for(i in 1:length(res$cif)) {
          df[[i]] <-  cbind(unique(data.frame(x[i][order(x[i])])),
                            data.frame(res$biasf[i])[as.numeric(rownames(unique(data.frame(x[i])))), ],
                            cumsum(unlist(res$cif.mas[i][order(res$data$x)]))[as.numeric(rownames(unique(data.frame(x[i]))))],
                            res$sd.boot[[i]])
          names(df[[i]]) <- c(paste0("time_", i),  paste0("biasf_", i), paste0("cif", i) , paste0("sd.boot_", i))

          }

        names(df) <- paste0("Comp_event_", 1:length(res$cif.mas))
        return(df)
        for(i in 1:length(res$cif.mas)) {
          print(df[i], sep = "\n")
        }
      }
    }

    if (res$method == "indep") {

      df <- list()

      if(is.null(res$sd.boot)){

        for(i in 1:length(res$cif.mas)){

          df[[i]] <- cbind(x = data.frame(unique(res$data$x[res$data$z == i])),
                           biasf = res$biasf[which(res$data$z == i)][as.numeric(rownames(unique(data.frame(res$data$x[res$data$z == i]))))],
                           as.numeric(cumsum(unlist(res$cif.mas[i][order(res$data$x)]))[as.numeric(rownames(unique(data.frame(res$data$x[res$data$z == i]))))]))
          names(df[[i]]) <- c(paste0("time_", i),  paste0("biasf_", i), paste0("cif_", i))
          }

        names(df) <- paste0("Comp_event_", 1:length(res$cif.mas))
        return(df)
        for(i in 1:length(res$cif.mas)){
          print(df[i], sep = "\n")
        }

      }else{

        for(i in 1:length(res$cif.mas)){

          df[[i]] <-  cbind(x = data.frame(unique(res$data$x[res$data$z == i])),
                            biasf = res$biasf[which(res$data$z == i)][as.numeric(rownames(unique(data.frame(res$data$x[res$data$z == i]))))],
                            as.numeric(cumsum(unlist(res$cif.mas[i][order(res$data$x)]))[as.numeric(rownames(unique(data.frame(res$data$x[res$data$z == i]))))]),
                            res$sd.boot[[i]])
          names(df[[i]]) <- c(paste0("time_", i),  paste0("biasf_", i), paste0("cif_", i) , paste0("sd.boot_", i))
        }


        names(df) <- paste0("Comp_event_", 1:length(res$cif.mas))
        return(df)
        for(i in 1:length(res$cif.mas)){
          print(df[i], sep = "\n")
        }
      }
    }
  }
}

