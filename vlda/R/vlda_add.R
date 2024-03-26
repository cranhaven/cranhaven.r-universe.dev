#' Supplementary Objects and Variables
#'
#'
#' Add objects or variables with new information to the two-dimensional VLDA plot proposed for multidimensional longitudinal data.
#'
#'
#'
#' @param fit An object returned by vlda()
#'
#' @param add.col A data matirx, The type of indicator matrix. Additional data sets in column format. \eqn{p \ge 2}
#'
#' @param add.row A data matirx, The type of indicator matrix. Additional data sets in row format.
#'  Supplemental data should have the same variable name as \code{fit$ind.mat} returned by \code{\link{vlda}},
#'  and if it is not an indicator matrix, you can use it after generate an indicator matrix using \code{\link{indicator}} function built into vlda.
#'
#' @param time.name If supplemental data to add contains a time variable, it requires argument a character string that specifies the name of the time variable.
#'
#' @usage
#' vlda_add(fit, add.col = NULL, add.row = NULL, time.name = NULL)
#'
#' @return
#' \item{...}{Same as the result of vlda}
#' \item{sup.coordiante}{A tibble data class. The coordinates of the new object created when adding supplemental data to the already provided vlda plot.}
#'
#' @details
#' The longitudinal data inevitably has the characteristic that supplementary data is added such as: \cr
#' 
#' * Outcome variables measured at additional time points, such as \eqn{T+1, T+2, ...} after the last time point \eqn{T}. \cr
#' * New objects that are not previously measured.\cr
#' * Other covariates that indicate the characteristics of objects.\cr
#' 
#' Find coordinates representing objects and variables that are added in the VLDA plot already
#' provided, through a method obtain that find coordinates on low-dimensional space for supplementary elements.
#'
#'
#' @seealso \code{vlda}
#' @keywords Supplement
#' @examples
#' #### Supplementary row and column indicator matrix added ####
#' ### long form ###
#' data(PTSD)
#' PTSD <- as.data.frame(PTSD)
#' PTSD[,2:4] <- apply(PTSD[,2:4], 2, function(x) ifelse(x >= 3, 1, 0))
#' PTSD[,5] <-  ifelse(PTSD[,5] >= 6 , 1, 0)
#' PTSD <- data.frame(lapply(PTSD, function(x) as.factor(x)))
#' fit <- vlda(x = PTSD, object = "subject", time = "time", type = "long")
#'
#' data(PTSD_column) # The degree of drinking that may affect PTSD
#' PTSD_column <- as.matrix(PTSD_column)
#'
#' data(PTSD_row) # Added to the row, and is intended for 316 patients after 18 months.
#' PTSD_row <- as.matrix(PTSD_row)
#'
#'
#' vlda_add(
#' fit,
#' add.row = PTSD_row,
#' add.col = PTSD_column
#' )
#'
#'
#' ### Wide form ###
#' data(Depression)
#' fit2 <- vlda(x = Depression, object = "Case", time = c("1week", "2weeks", "4weeks"), type = "wide")
#'
#' # Response after 6 weeks and gender were added the columns for 800 existing patients.
#' data(Depression_column) 
#' Depression_column <- as.matrix(Depression_column)
#' 
#' # 100 patients who took placebo in each group of mild and severe were added to the rows.
#' data(Depression_row) 
#' Depression_row <- as.matrix(Depression_row)
#'
#'
#' vlda_add(
#' fit2,
#' time.name = "6weeks",
#' add.row = Depression_row,
#' add.col = Depression_column
#' )
#'
#'
#' @import dplyr
#' @importFrom utils combn globalVariables
#' @export vlda_add
#'
vlda_add <- function(fit, add.col = NULL, add.row = NULL, time.name = NULL){

  obs.raw <- fit$obs.raw
  obs.coordinate <- fit$obs.coordinate
  var.coordinate <- fit$var.coordinate
  svd.G <- fit$svd.G
  x <- fit$x
  object <-fit$object
  time <- c(fit$time, time.name)
  type <- fit$type
  name <- fit$name
  Eigen <- fit$Eigen
  GOF <- fit$GOF
  Ags <- NULL
  ind.mat <- fit$ind.mat
  Du <- diag(svd.G$d[-1])
  xtime <- x[fit$time]
  xtime <- as.data.frame(xtime)
  ind.time <- colnames(indicator(xtime))
  LAB <- fit$LAB

  if (!is.null(add.row)){

    add.row <- data.matrix(add.row)

    if (sum(colSums(add.row) == 0) > 0) {

      Iq <- matrix(rep(1, ncol(add.row)), ncol=1)

      Rgs <- solve(diag(c(add.row%*%Iq))) %*% add.row

      add.colnames <- names(which(colSums(Rgs) == 0))

      Ags <- Rgs %*% var.coordinate

      row.new <- cbind(mean(Ags[,1]),mean(Ags[,2]))

      colnames(Rgs) <- colnames(ind.mat)

      if ( type == "long" ){

        wh.time <- sapply(strsplit(add.colnames, "\\."),

                          function(x) x[2]) %>% length() + 1

        rownames(row.new) <- paste0(time, ".", wh.time)

      } else if( type == "wide" ){

        supplement <- sapply(strsplit(add.colnames, "\\."),

                             function(x) x[1]) %>% unique()

        wh.sup <- sapply(strsplit(add.colnames, "\\."),

                         function(x) x[2]) %>% length() + 1

        rownames(row.new) <- paste0(supplement, ".", wh.sup)

      }

      var.coordinate <- rbind(var.coordinate,row.new)

    }

  }

  if (!is.null(add.col)){

    add.col <- data.matrix(add.col)

    if(!is.na(table(add.col)[1] == table(add.col)[2])) {

      In <- matrix(rep(1, nrow(add.col)), ncol=1)

      Cgs <- solve(diag(c(t(add.col)%*%In))) %*% t(add.col)

      invDu <- solve(Du[1:2, 1:2])

      Bgs <- Cgs %*% obs.raw %*% invDu %*% invDu

      rename.col <- colnames(add.col)

      str.rename.col <- strsplit(rename.col, "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])|([.])", perl=TRUE)

      split.pattern <- lapply(str.rename.col, function(x) paste0(x[-(length(x))], collapse = ""))

      split.num <- lapply(str.rename.col, function(x) x[length(x)])

      c.name <- paste0(split.pattern, ".", split.num)

      which.error <- lapply(strsplit(c.name, "\\."),function(x) x[1]) == ""

      which.sep <- lapply(strsplit(c.name, "([.])", perl = TRUE), function(x) length(x)) == 1

      find.error <- rename.col[which(which.error)]

      fv.error <- paste0("'",find.error,"'", collapse = " and ")

      find.sep <- rename.col[which(which.sep)]

      fv.sep <- paste0("'",find.sep,"'",collapse = ",")

      if (any(which.sep)) warning(fv.sep," is not an indicator matrix or no separator ","'.'")

      if (any(which.error)) stop("if ", fv.error,

                                 " belong to the same group, use the names of ", fv.error,

                                 " as ", paste0("'Group.",find.error,"'", collapse = " and ") )

      tmp <- sapply(strsplit(c.name,"\\."), function(x) x[1])

      index.i <-lapply( unique(tmp), function(x) which(tmp %in% x) )

      names(index.i) <- unique(tmp)

      comparison <- NULL
      for ( i in index.i){

        comparison <- cbind(comparison, table(add.col[,i])[1] == table(add.col[,i])[2])

      }
      f.list <- index.i[which(comparison == FALSE)]

      f.col <- colnames(add.col[,f.list %>% unlist])


      if (any(!comparison)) stop("if ",paste0("'",f.col, sep = "'", collapse = " and "),

                                 " belong to the same group, use formats such as 'Group.A' and 'Group.B'" )

      rownames(Bgs) <- c.name

      var.coordinate <- rbind(var.coordinate, Bgs)

    }

  }

  var.coordinate <- round(var.coordinate[ order(rownames(var.coordinate), decreasing=FALSE), ], 3)


  ev.add.wide <- NULL
  if (!is.null(Ags)) {

    Ags <- as.data.frame(Ags)

    Ags <- round(Ags, 3)

    suppressWarnings({

      if (type == "long"){

              Ags$name <- paste0(unique(x[,object]), "_", sapply(strsplit(rownames(row.new), "\\."), function(x) x[2]))

              Ags$name <- as.character(Ags$name)

              Ags <- summarise(group_by(Ags, x,y), obs_list = list(name))

              Ags$obs_time <- paste( time, "=", sapply(strsplit(rownames(row.new), "\\."), function(x) x[2]), collapse = " " )

        } else if (type == "wide"){

              Ags$name <- seq(length(name) + 1, length(name) + nrow(add.row))

              ev.add.wide <- as.data.frame(Depression_row[,-which(colnames(Depression_row) %in% c(ind.time, add.colnames))])

              rownames(ev.add.wide) <- Ags$name

              Ags$name <- as.character(Ags$name)

              Ags <- summarise(group_by(Ags, x,y), obs_list = list(name))

               }


      i <-  1
      for ( i in 1 : nrow(Ags)){

        names(Ags$obs_list)[i] <- paste0( "x = ",Ags[i, 1]," ", "y = ", Ags[i, 2] )

        Ags$list[i] <-  paste0( Ags$obs_list[[i]], collapse = " " )

        Ags$list2[i] <- ifelse(length(Ags$obs_list[[i]]) >= 5,

                               paste0( paste( Ags$obs_list[[i]][1:5], collapse = " " ), "..." ),

                               paste0( Ags$obs_list[[i]], collapse = " " ))
      }

      if( type == "long"){

        Ags$new_tooltip <- paste0("Add Obs Group [", rownames(Ags), "]<br>",

                                  object, "(",time," = ",sapply(strsplit(rownames(row.new), "\\."), function(x) x[2]),

                                  ")<br>",Ags$list2, "<br>", "x = ", Ags$x, ", y = ", Ags$y)

      } else if ( type == "wide" ){

        Ags$new_tooltip <- paste0("Add Obs Group [", rownames(Ags), "]<br>",

                                  object, "<br>", Ags$list2, "<br>","x = ", Ags$x, ", y = ", Ags$y)

      }


    })

  }


  output <- list(obs.coordinate = obs.coordinate, sup.coordiante = Ags[, 1:3], var.coordinate = var.coordinate, Eigen = Eigen, GOF = GOF,
                 x = x, object = object, time = time, type = type, name = name, svd.G = svd.G,  Ags = Ags,
                 obs.raw = obs.raw, ind.mat = ind.mat, ind.time = ind.time, ev.add.wide = ev.add.wide, LAB = LAB)

  class(output) <- "vlda"

  attr(output, "hidden") <- c("svd.G", "x", "object", "time", "type", "name", "ind.mat", "obs.raw", "Ags", "ind.time", "ev.add.wide", "LAB")

  return(output)

}




