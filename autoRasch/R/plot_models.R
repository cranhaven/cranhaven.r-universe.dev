
#' Plot The Expected Value Curves
#'
#' This function plots the curve(s) of the estimated ability parameters against its expected responses.
#'
#' @param obj The object of class \code{'pcm'}.
#' @param itemno A number of the item that is wanted to be plot.
#' @param xlab a title for the x axis.
#' @param ylab a title for the y axis.
#' @param xlim the x limits (x1, x2) of the plot. Note that x1 > x2 is allowed and leads to a ‘reversed axis’.
#' The default value, \code{NULL}, indicates that the range of the finite values to be plotted should be used;
#' see \code{\link[graphics:plot.default]{plot.default()}}
#' @param col a vector of plotting colors
#' @param lty a vector of line types.
#' @param ... Further arguments to be passed.
#'
#' @return
#' There are no values to return. Instead, it plots expected values from the model.
#'
#' @examples
#' res <- pcm(short_poly_data)
#' plot_EVC(res, itemno = 4)
#'
#' @export
plot_EVC <- function(obj = c(), itemno = 5, xlab = NULL, ylab = NULL, xlim = c(-10,10),
                     col = c("green4","darkorange2","red2"), lty = c(1,1,1), ...){

  if(itemno > length(obj$mt_vek)){
    stop("autoRasch ERROR: itemno exceed the number of items.")
  }

  # dotdotdot <- list(...)
  # if(!is.null(dotdotdot$main)){
  #   par(mar = c(6.5, 7.5, 2.5, 1), oma = c(0, 0,0, 0))
  # } else {
  #   par(mar = c(6.5, 7.5, 0.5, 1), oma = c(0, 0,0, 0))
  # }

  if(!is.null(xlab)){
    x.lab <- xlab
  } else {
    x.lab <- "Ability scores / Theta"
  }

  if(!is.null(ylab)){
    y.lab <- ylab
  } else {
    y.lab <- "Expected Values"
  }

  if(is.null(xlim) | length(xlim) != 2){
    xlim <- c(-10,10)
  }

  emat_list <- emat_compute(obj, theta.lim = c((xlim[1]-1),(xlim[2]+1)))

  if(is.null(col)){
    col <- rep(1,(length(emat_list$emat) - 1))
    lty <- rep(1,(length(emat_list$emat) - 1))
  } else if(length(col) < (length(emat_list$emat) - 1)){
    col <- rep(col,length(emat_list$emat))
    lty <- rep(lty,length(emat_list$emat))
    length(col) <- (length(emat_list$emat) - 1)
    length(lty) <- (length(emat_list$emat) - 1)
  }

  # print(length(emat_list[[1]]))
  # print(length(emat_list$emat[[1]][,7]))

  plot(emat_list[[1]], emat_list$emat[[1]][,itemno], col = col[1], type = "l", lty = lty[1],
       mgp = c(5,2,0), xlab = x.lab, ylab = y.lab, xlim = xlim, ...)
  # print(length(emat_list$emat))
  if(length(emat_list$emat) > 1){
    for(i in 2:length(emat_list$emat)){
      lines(emat_list[[1]], emat_list$emat[[i]][,itemno], col = col[i], lty = lty[i], ...)
    }
  }
}

#' Plot The Item Characteristic Curves
#'
#' This function plots the curve(s) of the estimated ability parameters against the probabilities of responses.
#'
#' @param obj The object of class \code{'pcm'}.
#' @param itemno A number of the item that is wanted to be plot.
#' @param xlab a title for the x axis.
#' @param ylab a title for the y axis.
#' @param xlim the x limits (x1, x2) of the plot. Note that x1 > x2 is allowed and leads to a ‘reversed axis’.
#' The default value, \code{NULL}, indicates that the range of the finite values to be plotted should be used;
#' see \code{\link[graphics:plot.default]{plot.default()}}
#' @param col a vector of plotting colors
#' @param lty a vector of line types.
#' @param ... Further arguments to be passed.
#' @param main String. Plot title.
#'
#' @return
#' There are no values to return. Instead, it plots the curve of item characteristics from the model.
#'
#' @examples
#' res <- pcm(short_poly_data)
#' plot_ICC(res, itemno = 4)
#'
#' @export
plot_ICC <- function(obj, itemno = 5, xlab = NULL, ylab = NULL, xlim = c(-10,10),
                     col = c("green4","darkorange2","red2"), lty = c(1,1,1),
                     main = NULL, ...){

  if(itemno > length(obj$mt_vek)){
    stop("autoRasch ERROR: itemno exceed the number of items.")
  }

  dotdotdot <- list(...)
  # if(!is.null(dotdotdot$main)){
  #   par(mar = c(6.5, 7.5, 2.5, 1), oma = c(0, 0,0, 0))
  # } else {
  #   par(mar = c(6.5, 7.5, 0.5, 1), oma = c(0, 0,0, 0))
  # }

  if(is.null(xlim) | length(xlim) != 2){
    xlim <- c(-10,10)
  }

  if(!is.null(xlab)){
    x.lab <- xlab
  } else {
    x.lab <- expression("ability trait/ "*theta)
  }

  if(!is.null(ylab)){
    y.lab <- ylab
  } else {
    y.lab <- "P(ability)"
  }

  if(is.null(main)){
    main <- paste("ICC of Item - ",colnames(obj$X)[itemno])
  }

  pmat_list <- emat_compute(obj, theta.lim = c((xlim[1]-1),(xlim[2]+1)))
  n.cat <- max(obj$mt_vek)+1

  if(is.null(col)){
    col <- rep(1,(length(pmat_list$pmat) - 1))
    lty <- rep(1,(length(pmat_list$pmat) - 1))
  } else if(length(col) < (length(pmat_list$pmat) - 1)){
    col <- rep(col,length(pmat_list$pmat))
    lty <- rep(lty,length(pmat_list$pmat))
    length(col) <- (length(pmat_list$pmat) - 1)
    length(lty) <- (length(pmat_list$pmat) - 1)
  }

  matplot(as.matrix(pmat_list[[1]]), pmat_list$pmat[[1]][,c(((itemno-1)*n.cat+1):(itemno*n.cat))], type = "l", lty = lty[1], xlab = x.lab, ylab = y.lab, main = main, ...)
  if(length(pmat_list$pmat) > 1){
    for(i in 2:length(pmat_list$pmat)){
      matlines(as.matrix(pmat_list[[1]]), pmat_list$pmat[[i]][,c(((itemno-1)*n.cat+1):(itemno*n.cat))], lty = lty[i], ...)
    }
  }
  legend("topright", legend = c(0:((obj$mt_vek[itemno]+1)-1)),col=seq_len(obj$mt_vek[itemno]+1),cex=0.8,fill=seq_len(obj$mt_vek[itemno]+1), horiz = TRUE)

}

emat_compute <- function(obj, theta.lim = c(-10,10)){

  X <- as.matrix(obj$X)
  RM.res <- obj
  if(!is.null(obj$groups_map)){
    n_groups <- ncol(obj$groups_map)
  } else {
    n_groups <- 1
  }

  if(!is.null(obj$delta)){
    #if(obj$mode == "DIF"){
      delta_mat <- matrix(obj$delta, ncol = (n_groups))
      delta_mat <- cbind(rep(0,nrow(delta_mat)),delta_mat)
    #} else {
      #delta_cube <- array(obj$delta, c(,,n_groups))
    #}
  } else {
    delta_mat <- matrix(rep(0,length(obj$mt_vek)),ncol = 1)
  }

  if(!is.null(obj$gamma)){
    gamma <- RM.res$gamma
  } else {
    gamma <- rep(0,length(obj$mt_vek))
  }

  beta.init <- unlist(tapply(obj$beta,rep(seq_along(obj$mt_vek),obj$mt_vek),function(x){
    if(length(x) < max(obj$mt_vek)){
      x <- c(x,rep(NA,(max(obj$mt_vek)-length(x))))
      x
    } else {
      x
    }
  }))

  if("pcm" %in% class(obj) | "gpcm" %in% class(obj)){
    n.iter <- n_groups
  } else if("pcmdif" %in% class(obj) | "gpcmdif" %in% class(obj)){
    n.iter <- n_groups + 1
  }


  res <- list("theta" = c(seq(theta.lim[1],theta.lim[2],0.01)))
  for(z in 1:(n.iter)){
    beta <- beta.init
    theta <- seq(theta.lim[1],theta.lim[2],0.01)
    if(z > 1){
      # beta <- beta + rep(rowSums(delta_mat[,1:z]) ,each = max(obj$mt_vek,na.rm = TRUE))
      if(obj$mode == "DIF"){
        beta <- beta + rep((delta_mat[,z]) ,each = max(obj$mt_vek,na.rm = TRUE))
      } else {
        beta <- beta + delta_mat[,z]
      }
    } else {
      if(obj$mode == "DIF"){
        beta <- beta + rep((delta_mat[,1]) ,each = max(obj$mt_vek,na.rm = TRUE))
      } else {
        beta <- beta + delta_mat[,1]
      }
    }
    exp.gamma <- exp(gamma)

    mt_vek <- RM.res$mt_vek

    exp.gamma <- rep(exp.gamma, each = max(obj$mt_vek,na.rm = TRUE))

    n.th <- max(mt_vek)

    xna.mat <- matrix(1,nrow = nrow(X), ncol = ncol(X))

    idx <- which(is.na(X))
    xna.mat[idx] <- NA
    XNA <- xna.mat

    t.diff <- outer((-beta), theta, "+")
    disc.diff <- t.diff * exp.gamma

    per.cat.list <- matrix(disc.diff, nrow = n.th)

    temp.prob <- as.matrix(per.cat.list[1,])
    temp.l2 <- exp(temp.prob)
    temp.l1 <- exp(temp.l2*0)
    temp.l1 <- cbind(temp.l1,temp.l2)
    if(n.th > 1){
      for(i in 2:n.th){
        temp.prob <- cbind(temp.prob,(temp.prob[,i-1]+per.cat.list[i,]))
        temp.l1 <- cbind(temp.l1,(exp(temp.prob[,i])))
        temp.l2 <- rowSums(cbind(temp.l2,exp(temp.prob[,i])), na.rm = TRUE)

        # if(is.na(temp.prob[,i])){
        #   temp.l2 <- temp.l2
        # } else {
        #   temp.l2 <- temp.l2 + (exp(temp.prob[,i]))
        # }
      }
    }
    l2 <- (temp.l2+1)

    l1 <- as.vector(t(temp.l1))
    l2 <- rep(l2, each = (n.th+1))

    pmat <- l1/l2

    mt_vek0 <- rep(n.th,length(mt_vek)) + 1
    mt_seq <- sequence(mt_vek0)-1

    Emat <- pmat * mt_seq
    Emat <- matrix(Emat, nrow = (n.th+1))
    Emat <- colSums(Emat,na.rm = TRUE)

    Emat <- t(matrix(Emat, nrow = ncol(X)))
    Pmat <- t(matrix(pmat, nrow = ((n.th+1)*length(mt_vek))))

    res[["emat"]][[z]] <- Emat
    res[["pmat"]][[z]] <- Pmat
  }

  return(res)
}

#' Plot The Person-Item Map
#'
#' This function maps the distribution of the persons' abilities and the items difficulties along the latent continuum.
#'
#' @param obj The object of class \code{'pcm'}.
#' @param main main title of the plot; see \code{\link[graphics:plot.default]{plot.default()}}.
#' @param xlab Label for the x-axis; see \code{\link[graphics:plot.default]{plot.default()}}.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default;
#' see \code{\link[graphics:par]{par()}}.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of cex;
#' see \code{\link[graphics:par]{par()}}.
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of cex;
#' see \code{\link[graphics:par]{par()}}.
#' @param cex.main The magnification to be used for main titles relative to the current setting of cex;
#' see \code{\link[graphics:par]{par()}}.
#' @param lwd The line width, a positive number, defaulting to 1;
#' see \code{\link[graphics:par]{par()}}.
#' @param v Variable names used
#' @param th_dif The threshold at which a DIF effect is still considered a DIF.
#'
#' @return
#' There are no values to return. Instead, it shows a graphical map of the estimated ability and the estimated difficulty on the same scale.
#'
#' @examples
#' \dontrun{
#' groupsMap <- matrix(c(rep(1,50),rep(0,50)),ncol = 1, dimnames = list(c(1:100),c("V1")))
#' pcmdif_res <- pcm_dif(shortDIF, groups_map = groupsMap)
#' plot_PImap(pcmdif_res)
#' }
#'
#' @export
plot_PImap <- function(obj, main = NULL, xlab = NULL, cex = NULL, cex.lab = NULL,
                       cex.axis = NULL, cex.main = NULL, lwd = NULL, v = NULL, th_dif = 1e-5){

  if(("pcm" %in% class(obj)) | ("pcmdif" %in% class(obj))){
  } else {
    stop("autoRasch ERROR: person-item map only for \"pcm\" or \"pcmdif\" object.")
  }

  if(is.null(th_dif)){
    th_dif <- 1e-5
  }

  oldpar <- par(no.readonly = TRUE)

  par(mar = c(0, 1, 3, 0), oma = c(0, 0,0, 0))
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), widths=c(8,2), heights=c(1,3))

  # reported_beta <- obj$beta * obj$real_vek
  reported_beta <- unlist(tapply(obj$beta,rep(seq_along(obj$mt_vek),obj$mt_vek),function(x){
    if(length(x) < max(obj$mt_vek)){
      x <- c(x,rep(NA,(max(obj$mt_vek)-length(x))))
      x
    } else {
      x
    }
  }))
  beta_mat <- matrix(reported_beta, nrow = length(obj$mt_vek), byrow = TRUE)
  if(!is.null(v)){
    beta_mat <- as.data.frame(round(beta_mat,4), row.names = as.character(v))
  } else {
    beta_mat <- as.data.frame(round(beta_mat,4), row.names = obj$itemName)
  }
  colnames(beta_mat) <- paste("Th_",c(1:max(obj$mt_vek)),sep = "")

  if("pcmdif" %in% class(obj)){
    if(obj$mode == "DIF"){
      delta_mat <- matrix(obj$delta, ncol = ncol(obj$groups_map))
      for(g in seq_len(ncol(obj$groups_map))) {
        idx <- which(abs(delta_mat[,g]) > th_dif)
        for(j in idx){
          # tempName <- paste(rownames(beta_mat)[j],"_",letters[g],sep = "")
          tempName <- paste(rownames(beta_mat)[j],"_",colnames(obj$groups_map)[g],sep = "")
          tempItem <- beta_mat[j,] + delta_mat[j,g]
          beta_mat[nrow(beta_mat)+1,] <- tempItem
          rownames(beta_mat)[nrow(beta_mat)] <- tempName
        }
      }
    } else {
      delta_cube <- array(obj$delta, c(max(obj$mt_vek,na.rm = TRUE),length(obj$mt_vek),ncol(obj$groups_map)))
      for(g in seq_len(ncol(obj$groups_map))) {
        # idx <- which(abs(delta_mat[,g]) > th_dif)
        idx <- which(apply(delta_cube[,,g],2,function(x){TRUE %in% (abs(x) > th_dif)}))
        for(i in idx){
          # tempName <- paste(rownames(beta_mat)[j],"_",letters[g],sep = "")
          tempName <- paste(rownames(beta_mat)[i],"_",colnames(obj$groups_map)[g],sep = "")
          tempItem <- beta_mat[i,] + delta_cube[,i,g]
          beta_mat[nrow(beta_mat)+1,] <- tempItem
          rownames(beta_mat)[nrow(beta_mat)] <- tempName
        }
      }
    }
  }

  beta_mat[["Item Loc."]] <- round(apply(beta_mat,1,mean,na.rm=TRUE),4)
  if(max(obj$mt_vek) > 1){
    beta_mat$` ` <- apply(beta_mat[,1:max(obj$mt_vek)],1,function(x){if(is.unsorted(na.omit(x))){return("*")}else{return("")}})
  }
  beta_mat <- beta_mat[order(as.numeric(beta_mat[,(max(obj$mt_vek,na.rm = TRUE)+1)]),decreasing = TRUE),]

  x_minbound <- ceiling(min(c(as.vector(obj$theta),as.vector(reported_beta)),na.rm = TRUE) - 1)
  x_maxbound <- floor(max(c(as.vector(obj$theta),as.vector(reported_beta)),na.rm = TRUE) + 1)

  if(is.null(main)){
    main <- "Person-Item Map"
  }
  if(is.null(cex.main)){
    cex.main <- 1
  }
  breaks <- length(obj$theta)/1
  hist(obj$theta, breaks = breaks, mgp = c(5,1,0), yaxt = "n", xlim = c(x_minbound,x_maxbound), mgp = c(100,100,0), main = main, cex.main = cex.main)
  plot(c(1:1),c(1:1),type = "n", yaxt='n', xaxt = 'n', main = "", xlab = "", ylab = "", bty = "n")

  if(is.null(xlab)){
    xlab <- "Latent trait"
  }

  if(is.null(cex.lab)){
    cex.lab <- 1
  }

  if(is.null(cex)){
    cex <- 1
  }

  if(is.null(cex.axis)){
    cex.axis <- 1
  }


  par(mar = c(5, 1, 0, 0))
  plot(c(1:1),c(1:1),xlim = c(x_minbound,x_maxbound) ,ylim = c(0,nrow(beta_mat)+1), type = "n", yaxt='n', main = "", xlab = xlab, ylab = "", cex.lab = cex.lab, cex.axis = cex.axis)
  for(i in seq_len(nrow(beta_mat))) {
    if(max(obj$mt_vek) > 1){
      if(beta_mat[i,(max(obj$mt_vek,na.rm = TRUE)+2)] == "*"){
        col <- "red"
      } else {
        col <- "black"
      }
    } else {
      col <- "black"
    }
    points(c(beta_mat[i,1:max(obj$mt_vek,na.rm = TRUE)]), rep(i,max(obj$mt_vek,na.rm = TRUE)),type = "b", bg = col, col = col, fg = col, pch = 16, lwd = lwd)
    points(beta_mat[i,(max(obj$mt_vek,na.rm = TRUE)+1)], c(i), type = "p", pch=22, bg = col, col = col, fg = col)
    text(c(beta_mat[i,1:max(obj$mt_vek,na.rm = TRUE)]), rep(i-0.5,4), labels = c(1:max(obj$mt_vek,na.rm = TRUE)), adj = c(0.5,NA), col = col, cex = (0.7*cex))
  }
  par(mar = c(5, 0, 0, 1))
  plot(c(1:1),c(1:1),xlim = c(0,10),ylim = c(0,nrow(beta_mat)+1), type = "n", yaxt='n', xaxt = 'n', main = "", xlab = "", ylab = "", bty = "n")
  for(i in seq_len(nrow(beta_mat))) {
    if(max(obj$mt_vek) > 1){
      if(beta_mat[i,(max(obj$mt_vek,na.rm = TRUE)+2)] == "*"){
        col <- "red"
      } else {
        col <- "black"
      }
    } else {
      col <- "black"
    }
    text(1, i, labels = rownames(beta_mat)[i], adj = c(0,NA), col = col, cex = cex)
  }

  par(mfrow = c(1,1))

  on.exit(par(oldpar))
}

# plot_PImap <- function(obj, main = NULL, xlab = NULL, cex = NULL, cex.lab = NULL,
#                        cex.axis = NULL, cex.main = NULL, lwd = NULL, v = NULL){
#
#   if(("pcm" %in% class(obj)) | ("pcmdif" %in% class(obj))){
#   } else {
#     stop("autoRasch ERROR: person-item map only for \"pcm\" or \"pcmdif\" object.")
#   }
#
#   th_dif <- 1e-2
#
#   par(mar = c(0, 1, 3, 0), oma = c(0, 0,0, 0))
#   layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), widths=c(8,2), heights=c(1,3))
#
#   # reported_beta <- obj$beta * obj$real_vek
#   reported_beta <- unlist(tapply(obj$beta,rep(seq_along(obj$mt_vek),obj$mt_vek),function(x){
#     if(length(x) < max(obj$mt_vek)){
#       x <- c(x,rep(NA,(max(obj$mt_vek)-length(x))))
#       x
#     } else {
#       x
#     }
#   }))
#   beta_mat <- matrix(reported_beta, nrow = length(obj$mt_vek), byrow = TRUE)
#   if(!is.null(v)){
#     beta_mat <- as.data.frame(round(beta_mat,4), row.names = as.character(v))
#   } else {
#     beta_mat <- as.data.frame(round(beta_mat,4), row.names = obj$itemName)
#   }
#   colnames(beta_mat) <- paste("Th_",c(1:max(obj$mt_vek)),sep = "")
#
#   if("pcmdif" %in% class(obj)){
#     delta_mat <- matrix(obj$delta, ncol = ncol(obj$groups_map))
#     for(i in 1:ncol(obj$groups_map)){
#       idx <- which(abs(delta_mat[,i]) > th_dif)
#       for(j in idx){
#         # tempName <- paste(rownames(beta_mat)[j],"_",letters[i],sep = "")
#         tempName <- paste(rownames(beta_mat)[j],"_",colnames(obj$groups_map)[i],sep = "")
#         tempItem <- beta_mat[j,] + delta_mat[j,i]
#         beta_mat[nrow(beta_mat)+1,] <- tempItem
#         rownames(beta_mat)[nrow(beta_mat)] <- tempName
#       }
#     }
#   }
#
#   beta_mat[["Item Loc."]] <- round(apply(beta_mat,1,mean,na.rm=TRUE),4)
#   if(max(obj$mt_vek) > 1){
#     beta_mat$` ` <- apply(beta_mat[,1:max(obj$mt_vek)],1,function(x){if(is.unsorted(na.omit(x))){return("*")}else{return("")}})
#   }
#   beta_mat <- beta_mat[order(as.numeric(beta_mat[,(max(obj$mt_vek,na.rm = TRUE)+1)]),decreasing = TRUE),]
#
#   x_minbound <- ceiling(min(c(as.vector(obj$theta),as.vector(reported_beta)),na.rm = TRUE) - 1)
#   x_maxbound <- floor(max(c(as.vector(obj$theta),as.vector(reported_beta)),na.rm = TRUE) + 1)
#
#   if(is.null(main)){
#     main <- "Person-Item Map"
#   }
#   if(is.null(cex.main)){
#     cex.main <- 1
#   }
#   breaks <- length(obj$theta)/1
#   hist(obj$theta, breaks = breaks, mgp = c(5,1,0), yaxt = "n", xlim = c(x_minbound,x_maxbound), mgp = c(100,100,0), main = main, cex.main = cex.main)
#   plot(c(1:1),c(1:1),type = "n", yaxt='n', xaxt = 'n', main = "", xlab = "", ylab = "", bty = "n")
#
#   if(is.null(xlab)){
#     xlab <- "Latent trait"
#   }
#
#   if(is.null(cex.lab)){
#     cex.lab <- 1
#   }
#
#   if(is.null(cex)){
#     cex <- 1
#   }
#
#   if(is.null(cex.axis)){
#     cex.axis <- 1
#   }
#
#
#   par(mar = c(5, 1, 0, 0))
#   plot(c(1:1),c(1:1),xlim = c(x_minbound,x_maxbound) ,ylim = c(0,nrow(beta_mat)+1), type = "n", yaxt='n', main = "", xlab = xlab, ylab = "", cex.lab = cex.lab, cex.axis = cex.axis)
#   for(i in 1:nrow(beta_mat)){
#     if(max(obj$mt_vek) > 1){
#       if(beta_mat[i,(max(obj$mt_vek,na.rm = TRUE)+2)] == "*"){
#         col <- "red"
#       } else {
#         col <- "black"
#       }
#     } else {
#       col <- "black"
#     }
#     points(c(beta_mat[i,1:max(obj$mt_vek,na.rm = TRUE)]), rep(i,max(obj$mt_vek,na.rm = TRUE)),type = "b", bg = col, col = col, fg = col, pch = 16, lwd = lwd)
#     points(beta_mat[i,(max(obj$mt_vek,na.rm = TRUE)+1)], c(i), type = "p", pch=22, bg = col, col = col, fg = col)
#     text(c(beta_mat[i,1:max(obj$mt_vek,na.rm = TRUE)]), rep(i-0.5,4), labels = c(1:max(obj$mt_vek,na.rm = TRUE)), adj = c(0.5,NA), col = col, cex = (0.7*cex))
#   }
#   par(mar = c(5, 0, 0, 1))
#   plot(c(1:1),c(1:1),xlim = c(0,10),ylim = c(0,nrow(beta_mat)+1), type = "n", yaxt='n', xaxt = 'n', main = "", xlab = "", ylab = "", bty = "n")
#   for(i in 1:nrow(beta_mat)){
#     if(max(obj$mt_vek) > 1){
#       if(beta_mat[i,(max(obj$mt_vek,na.rm = TRUE)+2)] == "*"){
#         col <- "red"
#       } else {
#         col <- "black"
#       }
#     } else {
#       col <- "black"
#     }
#     text(1, i, labels = rownames(beta_mat)[i], adj = c(0,NA), col = col, cex = cex)
#   }
#
#   par(mfrow = c(1,1))
# }
