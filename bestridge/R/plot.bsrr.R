#' Produces a coefficient profile plot of the coefficient or loss function
#' paths
#'
#' Produces a coefficient profile plot of the coefficient or loss function
#' paths
#'
#'
#' @param x A \code{"bsrr"} object.
#' @param type One of \code{"tune"}, \code{"coefficients"}, \code{"both"}.
#' For \code{"bsrr"} with \eqn{L_2} shrinkage:
#' If (\code{type = "tune"}), the path of corresponding information criterion
#' or cross-validation loss is provided;
#' If \code{type = "coefficients"}, a lambda should be provided and and this funciton provides a coefficient profile plot of the coefficient;
#' For \code{"bsrr"} object without \eqn{L_2} shrinkage:
#' If \code{type = "tune"}, a path of lcorresponding information criterion or cross-validation loss is provided.
#' If \code{type = "coefficients"}, it provides a coefficient profile plot of the coefficient.
#' @param lambda For \code{"bsrr"} with \eqn{L_2} shrinkage: To plot the change of coefficients with lambda equals this value for \code{type = "coefficients"} or \code{type = "both"}.
#' @param sign.lambda For \code{"bsrr"} with \eqn{L_2} shrinkage: A logical value indicating whether to show lambda on log scale. Default is 0.
#' @param breaks For \code{"bsrr"} object without \eqn{L_2} shrinkage: If \code{TRUE}, a vertical line is drawn at a specified break point in
#' the coefficient paths.
#' @param K For \code{"bsrr"} object without \eqn{L_2} shrinkage: Which break point should the vertical line be drawn at. Default is the optimal model size.
#' @param \dots Other graphical parameters to plot
#' @seealso \code{\link{bsrr}}.
#' @return
#' No return value, called for plots generation
#' @examples
#'
#' # Generate simulated data
#' n <- 200
#' p <- 20
#' k <- 5
#' rho <- 0.4
#' seed <- 10
#' Tbeta <- rep(0, p)
#' Tbeta[1:k*floor(p/k):floor(p/k)] <- rep(1, k)
#' Data <- gen.data(n, p, k, rho, family = "gaussian", beta = Tbeta, seed = seed)
#' lambda.list <- exp(seq(log(5), log(0.1), length.out = 10))
#' lm.bsrr <- bsrr(Data$x, Data$y, method = "pgsection")
#'
#' # generate plots
#' plot(lm.bsrr)
#'
#'@inherit bsrr return author
#'
#'@method plot bsrr
#'@export
#'@export plot.bsrr
plot.bsrr<-function(x, type=c( "tune", "coefficients"),lambda = NULL, sign.lambda = 0, breaks=T, K=NULL, ...)
{
  originalpar <- par(no.readonly = TRUE)
  on.exit(par(originalpar))
  type <- match.arg(type)
  if(!is.null(x$bsrr.one)) stop("Plots for object from bsrr.one are not available.")
  if(x$algorithm_type == "GPDAS" | x$algorithm_type == "GL0L2") stop("Plots for group selection are not available now.")
  if(x$algorithm_type == "PDAS" || (length(x$lambda.all)==1) && length(x$lambda.list)==1){
    type <- match.arg(type)
    # s.list=x$s.list

    if(is.null(K))  K <- length(which(x$beta!=0))#K<-df_list[length(df_list)]
    # if(x$family=="gaussian") dev=x$mse else dev=x$deviance

    if(type == "tune"){
      if(x$ic.type == "cv"){
        dev <- unlist(x$cvm.all)
      } else{
        dev <- unlist(x$ic.all)
      }
    }
    if(type == "loss"){
      dev = x$loss.all
    }
    if(type == "both"){
      if(x$ic.type == "cv"){
        dev <- unlist(x$cvm.all)
      } else{

        dev <- unlist(x$ic.all)
      }
    }
    if(x$method == "sequential"){
      beta.all <- x$beta.all[[1]]
    } else{
      beta.all <- x$beta.all
    }
    #beta.all <- matrix(unlist(x$beta.all), nrow = nrow(x$beta.all), byrow = F)
    df_list <- apply(beta.all, 2, function(x){sum(ifelse(abs(x) < 1e-6, 0, 1))})
    df_order <- order(df_list)
    df_list <- df_list[df_order]
    beta.all <- beta.all[,df_order]
    beta.all <- cbind(rep(0,nrow(beta.all)), beta.all)

    if(type=="loss" | type == "tune")
    {
      dev <- dev[df_order]
      plot_loss(dev,df_list,K,breaks, mar = c(3,4,3,4), ic.type=x$ic.type)
    }
    if(type=="coefficients")
    {
      plot_solution(beta.all, c(0, df_list), K, breaks, mar = c(3,4,3,4))
    }
    if(type=="both")
    {
      dev <- dev[df_order]
      layout(matrix(c(1,2),2,1,byrow=TRUE),heights=c(0.45,0.55), widths=1)
      oldpar <- par(las=1, mar=c(2,4,2,4), oma=c(2.5,0.5,1.5,0.5))
      plot_loss(dev,df_list,K,breaks,show_x = FALSE, ic.type = x$ic.type)
      plot_solution(beta.all, c(0, df_list), K,breaks)
      par(oldpar)
      par(mfrow=c(1,1))
    }
  } else{
    # plot_l0l2(x, sign.lambda, threeD)
    if(type=="coefficients"){
      if(is.null(lambda)) lambda = x$lambda
      plot_coef_l0l2(x, lambda)
    }else if(type == "tune"){
      plot_heatmap(x,sign.lambda)
    }else{
      if(is.null(lambda)) lambda = x$lambda
      layout(matrix(c(1,2),2,1,byrow=TRUE),heights=c(0.45,0.55), widths=1)
      oldpar <- par(las=1, mar=c(2,4,2,4), oma=c(2.5,0.5,1.5,0.5))
      plot_heatmap(x,sign.lambda)
      plot_coef_l0l2(x, lambda)
      par(oldpar)
      par(mfrow=c(1,1))
    }

  }
}


plot_loss <- function(loss,df,K,breaks=TRUE,show_x=TRUE, mar = c(0,4,2,4), ic.type){

  plot.new()                            # empty plot
  plot.window(range(df), range(loss), xaxs="i")
  oldpar <- par(mar = mar,              # no bottom spacing
                lend="square")          # square line ends
  par(new=TRUE)                         # add to the plot
  if(show_x)
  {
    plot(df, loss, type = "b", ylab=ifelse(ic.type=="cv", "cross validation error", ic.type),
         xlim=c(0,max(df)))
  }else
  {
    plot(df, loss, type = "b", ylab=ifelse(ic.type=="cv", "cross validation error", ic.type),
         xlim=c(0,max(df)), xaxt='n')
  }
  title(xlab='Model size', line = 2)
  if(breaks)abline(v=K, col="orange", lwd=1.5, lty=2) ## add a vertical line
  grid()
  axis(2)
  #axis(4, pos=par("usr")[1], line=0.5)  # this would plot them 'inside'
  # box()                                 # outer box

  par(oldpar)
}

plot_solution <- function(beta, df, K, breaks = TRUE, mar = c(3,4,0,4)){
  p <- nrow(beta)
  plot.new()                            # empty plot

  plot.window(range(df), range(beta), xaxs="i")

  oldpar <- par(mar=mar,         # no top spacing
                lend="square")          # square line ends
  par(new=TRUE)                         # add to the plot

  plot(df, beta[1,], type="l",col=1, xlim=c(0,max(df)),xlab="",
       ylim=range(beta),ylab=expression(beta))
  title(xlab='Model size', line = 2)
  for(i in 2:p){
    lines(df, beta[i,], col=i,xlim=c(0,p+1))
  }
  if(breaks) abline(v=K, col="orange", lwd=1.5, lty=2) ## add a vertical line
  #matplot(df, t(beta), lty = 1, ylab="",  xaxs="i",type = "l",xlim=c(0,p+1))

  nnz <- p
  xpos <- max(df)-0.8
  pos <- 4
  xpos <- rep(xpos, nnz)
  ypos <- beta[, ncol(beta)]
  text(xpos, ypos, 1:p, cex = 0.8, pos = pos)

  grid()
  axis(2)
  box()                                 # outer box

  par(oldpar)
}

plot_coef_l0l2 <- function(x, lambda){
  if(x$method != "sequential"){
    beta_mat = cbind(0, x$beta.all[, which(x$lambda.all == lambda)])
  }
  else{
    ind = which(x$lambda.list == lambda)
    beta_mat = cbind(rep(0, nrow(x$beta.all[[ind]])), x$beta.all[[ind]])
  }
  df_list = apply(beta_mat, 2, function(x){sum(ifelse(abs(x) < 1e-6, 0, 1))})
  breaks=F
  K=NULL
  plot_solution(beta_mat, c(df_list), K,breaks)
}

plot_heatmap <- function(x, sign.lambda){

  # plot.new()                            # empty plot
  # plot.window(range(df), range(x$lambda.list), xaxs="i")
  # oldpar <- par(mar = mar,              # no bottom spacing
  #               lend="square")          # square line ends
  # par(new=TRUE, oldpar)                         # add to the plot
  # sequential path
  if(x$method == "sequential"){
    #val = ifelse(is.null(x$ic.all), x$cvm.all, x$ic.all)
    if(x$ic.type == "cv"){
      val = x$cvm.all
    } else{
      val = x$ic.all
    }
    if(length(x$lambda.list>15)){
      lambda_col =x$lambda.list

      if(sign.lambda)
        colnames(val) = exp(lambda_col)
      else
        colnames(val) = lambda_col
      lambda_col[5*(1:ceiling(length(lambda_col)/5))] = round(lambda_col[5*(1:ceiling(length(lambda_col)/5))], 3)
      lambda_col[-5*(1:ceiling(length(lambda_col)/5))] = ""
    } else{
      colnames(val) = round(x$lambda.list, 3)
      if(sign.lambda) colnames(val) = exp(as.numeric(colnames(val)))
    }
    s_row = x$s.list
    if(length(s_row)>15){
      s_row[-5*(1:ceiling(length(s_row)/5))] = ""
    }
    rownames(val) = s_row
    colnames(val) = lambda_col
    # if(is.null(col)) col = heat.colors(nrow(x$ic.all) * ncol(x$ic.all))
    if(x$ic.type == "cv"){
      pheatmap(val, cluster_cols = FALSE, cluster_rows = FALSE, scale="none",  xlab = "lambda", ylab = "model size", main = "Cross-validation error")
    } else{
      main = x$ic.type
      pheatmap(val, cluster_cols = FALSE, cluster_rows = FALSE, scale="none",
               xlab = "lambda", ylab = "model size", main = main)
    }
    # powell path
  } else{
    df_list <- apply(x$beta.all, 2, function(x){sum(ifelse(abs(x) < 1e-8, 0, 1))})
    ## line search = sequential
    if(x$line.search == "sequential"){
      lambda.list = exp(seq(log(x$lambda.min), log(x$lambda.max),length.out = x$nlambda))
      s.list = x$s.min : x$s.max
      val = x$ic_mat
      # for(i in 1:length(x$lambda.all)){
      #   print(i)
      #   row_ind = which(s.list == df_list[i])
      #   col_ind = which(Isequal(x$lambda.all[i], lambda.list))
      #   # print(paste("col",col_ind))
      #   if(is.na(val[row_ind, col_ind])){
      #     val[row_ind, col_ind] = x$ic.all[i]
      #   } else{
      #     val[row_ind, col_ind] = pmin(val[row_ind, col_ind], x$ic.all[i])
      #   }
      # }

      if(length(lambda.list>15)){
        lambda_col = lambda.list
        lambda_col[5*(1:ceiling(length(lambda_col)/5))] = round(lambda.list[5*(1:ceiling(length(lambda.list)/5))], 3)
        # lambda_col[-5*(1:ceiling(length(lambda_col)/5))] = ""
        if(sign.lambda) colnames(val) = exp(lambda_col)
        else colnames(val) = lambda_col
        colnames(val)[-5*(1:ceiling(length(lambda_col)/5))] = ""
      } else{
        colnames(val) = round(lambda.list, 3)
        if(sign.lambda) colnames(val) = exp(as.numeric(colnames(val)))
      }
      s_row = s.list
      if(length(s_row)>15){
        s_row[-5*(1:ceiling(length(s_row)/5))] = ""
      }
      rownames(val) = s_row
      colnames(val) = lambda_col
      #if(is.null(col)) col =heat.colors(length(x$lambda.all))
      if(x$ic.type == "cv"){
        # heatmap(val, Colv=NA, Rowv = NA, scale="none", col = col, xlab = "lambda", ylab = "model size", main = "Cross-validation error")
        pheatmap(val, cluster_cols = FALSE, cluster_rows = FALSE, scale="none", xlab = "lambda",
                 ylab = "model size", main = "Cross-validation error",na_col = "gray")
        # setHook("grid.newpage", NULL, "replace")
        # grid.text("xlabel example", y=-0.07, gp=gpar(fontsize=16))
        # grid.text("ylabel example", x=-0.07, rot=90, gp=gpar(fontsize=16))
      } else{
        main = x$ic.type
        # heatmap(val, Colv=NA, Rowv = NA ,scale="none", col = col, xlab = "lambda", ylab = "model size", main = main)
        pheatmap(val, cluster_cols = FALSE, cluster_rows = FALSE, scale="none", xlab = "lambda",
                 ylab = "model size", main = main,na_col = "gray")
      }
      ## line search = gsection
    }else{
      lambda.list = exp(seq(log(x$lambda.min), log(x$lambda.max),length.out = 100))
      s.list = x$s.min : x$s.max
      # val = matrix(NA, nrow = length(s.list), ncol = 100)
      # for(i in 1:length(x$lambda.all)){
      #   lower_ind = which(lambda.list <= x$lambda.all[i])[1]
      #   upper_ind = ifelse(lower_ind + 1 > 100, 100, lower_ind + 1)
      #   col_ind = ifelse(abs(lambda.list[lower_ind] - x$lambda.all[i]) < abs(lambda.list[lower_ind] - x$lambda.all[i]), lower_ind, upper_ind)
      #   row_ind = which(s.list == df_list[i])
      #   if(is.na(val[row_ind, col_ind])){
      #     val[row_ind, col_ind] = x$ic.all[i]
      #   } else{
      #     val[row_ind, col_ind] = pmin(val[row_ind, col_ind], x$ic.all[i])
      #   }
      # }
      val = x$ic_mat
      lambda_breaks = matrix(lambda.list, nrow = length(lambda.list))
      lambda_breaks[5*(1:20)] <-  round(lambda_breaks[5*(1:20)], 3)
      lambda_breaks[-(5*(1:20))] <-  ""
      colnames(val) <- lambda_breaks
      if(sign.lambda) colnames(val) = exp(lambda_breaks)
      colnames(val)[-(5*(1:20))] <-  ""
      s_row = s.list
      if(length(s_row)>15){
        s_row[-5*(1:ceiling(length(s_row)/5))] = ""
      }
      rownames(val) = s_row
      #val[which(is.na(val))] = min(x$ic.all - 10)
      #if(is.null(col)) col = heat.colors(length(x$lambda.all))
      if(x$ic.type == "cv"){
        pheatmap(val, cluster_cols = FALSE, cluster_rows = FALSE, scale="none", #color = col,
                 xlab = "lambda", ylab = "model size", main = "Cross-validation error", na_col = "gray"
        )
      } else{
        main = x$ic.type
        pheatmap(val, cluster_cols = FALSE, cluster_rows = FALSE, scale="none", #color = col,
                 xlab = "lambda", ylab = "model size", main = main, na_col = "gray"
        )
      }
    }
  }
  # par(oldpar)
}

# plot_heatmap <- function(x, col){
#   # sequential path
#   if(x$method == "sequential"){
#     val = x$ic.all
#     if(length(x$lambda.list>15)){
#       lambda_col = x$lambda.list
#       lambda_col[5*(1:ceiling(length(lambda_col)/5))] = round(x$lambda.list[5*(1:ceiling(length(x$lambda.list)/5))], 3)
#       lambda_col[-5*(1:ceiling(length(lambda_col)/5))] = ""
#       colnames(val) = lambda_col
#     } else{
#       colnames(val) = round(x$lambda.list, 3)
#     }
#     rownames(val) = x$s.list
#     if(is.null(col)) col = heat.colors(nrow(x$ic.all) * ncol(x$ic.all))
#     if(x$ic.type == "cv"){
#       pheatmap(val, cluster_cols = FALSE, cluster_rows = FALSE, scale="none", color = col, xlab = "lambda", ylab = "model size", main = "Cross-validation error")
#     } else{
#       main = x$ic.type
#       pheatmap(val, cluster_cols = FALSE, cluster_rows = FALSE, scale="none", color = col, xlab = "lambda", ylab = "model size", main = main)
#     }
#   # powell path
#   } else{
#     df_list <- apply(x$beta.all, 2, function(x){sum(ifelse(abs(x) < 1e-8, 0, 1))})
#     ## line search = sequential
#     if(x$line.search == "sequential"){
#       lambda.list = exp(seq(log(x$lambda.max), log(x$lambda.min),length.out = x$nlambda))
#       s.list = x$s.min : x$s.max
#       val = matrix(NA, nrow = length(s.list), ncol = x$nlambda)
#       for(i in 1:length(x$lambda.all)){
#         print(i)
#         row_ind = which(s.list == df_list[i])
#         col_ind = which(Isequal(x$lambda.all[i], lambda.list))
#         # print(paste("col",col_ind))
#         if(is.na(val[row_ind, col_ind])){
#           val[row_ind, col_ind] = x$ic.all[i]
#         } else{
#           val[row_ind, col_ind] = pmin(val[row_ind, col_ind], x$ic.all[i])
#         }
#       }
#
#       if(length(lambda.list>15)){
#         lambda_col = lambda.list
#         lambda_col[5*(1:ceiling(length(lambda_col)/5))] = round(lambda.list[5*(1:ceiling(length(lambda.list)/5))], 3)
#         lambda_col[-5*(1:ceiling(length(lambda_col)/5))] = ""
#         colnames(val) = lambda_col
#       } else{
#         colnames(val) = round(lambda.list, 3)
#       }
#       rownames(val) = s.list
#       if(is.null(col)) col =heat.colors(length(x$lambda.all))
#       if(x$ic.type == "cv"){
#         # heatmap(val, Colv=NA, Rowv = NA, scale="none", col = col, xlab = "lambda", ylab = "model size", main = "Cross-validation error")
#         pheatmap(val, cluster_cols = FALSE, cluster_rows = FALSE, scale="none", color = col, xlab = "lambda",
#                  ylab = "model size", main = "Cross-validation error", na_col = "white")
#       } else{
#         main = x$ic.type
#         # heatmap(val, Colv=NA, Rowv = NA ,scale="none", col = col, xlab = "lambda", ylab = "model size", main = main)
#         pheatmap(val, cluster_cols = FALSE, cluster_rows = FALSE, scale="none", color = col, xlab = "lambda",
#                  ylab = "model size", main = main, na_col = "white")
#       }
#     ## line search = gsection
#     }else{
#       lambda.list = exp(seq(log(x$lambda.max), log(x$lambda.min),length.out = 100))
#       s.list = x$s.min : x$s.max
#       val = matrix(NA, nrow = length(s.list), ncol = 100)
#       for(i in 1:length(x$lambda.all)){
#         lower_ind = which(lambda.list <= x$lambda.all[i])[1]
#         upper_ind = ifelse(lower_ind + 1 > 100, 100, lower_ind + 1)
#         col_ind = ifelse(abs(lambda.list[lower_ind] - x$lambda.all[i]) < abs(lambda.list[lower_ind] - x$lambda.all[i]), lower_ind, upper_ind)
#         row_ind = which(s.list == df_list[i])
#         if(is.na(val[row_ind, col_ind])){
#           val[row_ind, col_ind] = x$ic.all[i]
#         } else{
#           val[row_ind, col_ind] = pmin(val[row_ind, col_ind], x$ic.all[i])
#         }
#       }
#       lambda_breaks = matrix(lambda.list, nrow = length(lambda.list))
#       lambda_breaks[5*(1:20)] <-  round(lambda_breaks[5*(1:20)], 3)
#       lambda_breaks[-(5*(1:20))] <-  ""
#       colnames(val) <- lambda_breaks
#       rownames(val) = s.list
#       #val[which(is.na(val))] = min(x$ic.all - 10)
#       if(is.null(col)) col = heat.colors(length(x$lambda.all))
#       if(x$ic.type == "cv"){
#         pheatmap(val, cluster_cols = FALSE, cluster_rows = FALSE, scale="none", color = col,
#                  xlab = "lambda", ylab = "model size", main = "Cross-validation error", na_col = "white")
#       } else{
#         main = x$ic.type
#         pheatmap(val, cluster_cols = FALSE, cluster_rows = FALSE, scale="none", color = col,
#                  xlab = "lambda", ylab = "model size", main = main, na_col = "white")
#       }
#     }
#   }
# }

# Isequal <- function(x, y){
#   return(ifelse(abs(x-y)<1e-5, TRUE, FALSE))
# }


# plot_l0l2 <- function(x, sign.lambda, threeD){
#   # 3D
#   if(threeD == TRUE){
#     # powell
#     if(x$method == "powell"){
#       if(sign.lambda == 1){
#         lam_list <- log(x$lambda.all)
#       } else{
#         lam_list <- x$lambda.all
#       }
#       df_list <- apply(x$beta.all, 2, function(x){sum(ifelse(abs(x) < 1e-8, 0, 1))}) # x
#       if(x$ic.type == "cv"){
#         z <- x$cvm.all
#         #open3d()
#         plot3d(df_list, lam_list, z, type = "l", col = "blue", xlab = "DF", ylab =ifelse(sign.lambda == 1, expression(Log(lambda)), expression(lambda)), zlab = "mean cross validation error")
#         text3d(df_list,lam_list,z, texts= 1:length(x$lambda.all),adj = 0.1
#                , font=5, cex=1)
#         # z = x$cvm.all
#         # x0 = df_list
#         # y0 = lam_list
#         # z0 = z
#         # x1 = c(df_list[-1], df_list[length(df_list)])
#         # y1 = c(lam_list[-1], lam_list[length(lam_list)])
#         # z1 = c(z0[-1], z0[length(z0)])
#         # xyz = cbind(c(df_list,  df_list[length(df_list)]), c(lam_list, lam_list[length(lam_list)]), c(z, z[length(z)]))
#         # plot3d(df_list, lam_list, z, type = "l", col = "blue", xlab = "DF", ylab =ifelse(sign.lambda == 1, expression(Log(lambda)), expression(lambda)), zlab = "mean cross validation error")
#         # text3d(df_list,lam_list,z, text= 1:length(x$lambda.all),adj = 0.1
#         #        , font=5, cex=1)
#         # for(i in 1:length(z)){
#         #   arrow3d(xyz[i, ], xyz[i+1, ], col = "blue")
#         # }
#
#       } else{
#         z <- x$ic.all
#         zlab <- x$ic.type
#         #open3d()
#         plot3d(df_list, lam_list, z, type="l", col = "blue", xlab = "DF", ylab = ifelse(sign.lambda == 1, expression(Log(lambda)), expression(lambda)), zlab = zlab)
#         text3d(df_list,lam_list,z, texts= 1:length(x$lambda.all),adj = 0.1,
#                font=5, cex=1)
#       }
#       # sequential
#     } else{
#       if(sign.lambda == 1){
#         lam_list <- log(x$lambda.list)
#       } else{
#         lam_list <- x$lambda.list
#       }
#       lam_order <- order(lam_list)
#       lam_list <- lam_list[lam_order]
#       df_list <- x$s.list
#       if(x$ic.type == "cv"){
#         z <- x$cvm.all
#         nrz <- nrow(z)
#         ncz <- ncol(z)
#         nbcol <- 100
#         color <- jet.colors(nbcol)
#         zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
#         facetcol <- cut(zfacet, nbcol)
#         persp3d(df_list, lam_list, z, col = "blue", phi = 30, theta = -30, xlab = "DF", ylab =ifelse(sign.lambda == 1, expression(Log(lambda)), expression(lambda)), zlab = "mean cross validation error")
#       } else{
#         z <- x$ic.all
#         nrz <- nrow(z)
#         ncz <- ncol(z)
#         nbcol <- 100
#         color <- jet.colors(nbcol)
#         zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
#         facetcol <- cut(zfacet, nbcol)
#         zlab <- x$ic.type
#         z <- z[, lam_order]
#         persp3d(df_list, lam_list, z, col = "blue", phi = 30, theta = -30, xlab = "DF", ylab =ifelse(sign.lambda == 1, expression(Log(lambda)), expression(lambda)), zlab = zlab)
#       }
#     }
#     # 2D
#   } else{
#     # powell
#     if(x$method == "powell"){
#       if(sign.lambda == 1){
#         lam_list <- log(x$lambda.all)
#       } else{
#         lam_list <- x$lambda.all
#       }
#       df_list <- apply(x$beta.all, 2, function(x){sum(ifelse(abs(x) < 1e-8, 0, 1))}) # x
#       if(x$ic.type == "cv"){
#         z <- x$cvm.all
#         x0 <- df_list
#         y0 <- lam_list
#         z0 <- z
#         x1 <- c(df_list[-1], df_list[length(df_list)])
#         y1 <- c(lam_list[-1], lam_list[length(lam_list)])
#         z1 <- c(z0[-1], z0[length(z0)])
#         arrows3D(x0, y0, z0, x1, y1, z1,ticktype="detailed", xlab = "DF", ylab =ifelse(sign.lambda == 1, expression(Log(lambda)), expression(lambda)), zlab = "mean cross validation error")
#         # text3D(df_list,lam_list,z, labels = 1:length(x$lambda.all),add=TRUE)
#
#       } else{
#         z <- x$ic.all
#         zlab <- x$ic.type
#         x0 <- df_list
#         y0 <- lam_list
#         z0 <- z
#         x1 <- c(df_list[-1], df_list[length(df_list)])
#         y1 <- c(lam_list[-1], lam_list[length(lam_list)])
#         z1 <- c(z[-1], z[length(z)])
#         arrows3D(x0, y0, z0, x1, y1, z1,ticktype="detailed", xlab = "DF", ylab =ifelse(sign.lambda == 1, expression(Log(lambda)), expression(lambda)), zlab = zlab)
#         # text3D(df_list,lam_list,z, labels = 1:length(x$lambda.all),add=TRUE)
#       }
#       # sequential
#     } else{
#       if(sign.lambda == 1){
#         lam_list <- log(x$lambda.list)
#       } else{
#         lam_list <- x$lambda.list
#       }
#       lam_order <- order(lam_list)
#       lam_list <- lam_list[lam_order]
#       df_list <- x$s.list
#       if(x$ic.type == "cv"){
#         z <- x$cvm.all
#         nrz <- nrow(z)
#         ncz <- ncol(z)
#         jet.colors <- colorRampPalette(c("blue", "orangeRed"))
#         nbcol <- 100
#         color <- jet.colors(nbcol)
#         zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
#         facetcol <- cut(zfacet, nbcol)
#         persp(df_list, lam_list, z, col = color[facetcol], phi = 30, theta = -30, ticktype = "detailed", xlab = "DF", ylab =ifelse(sign.lambda == 1, expression(Log(lambda)), expression(lambda)), zlab = "mean cross validation error")
#       } else{
#         z <- x$ic.all
#         nrz <- nrow(z)
#         ncz <- ncol(z)
#         jet.colors <- colorRampPalette(c("blue", "orangeRed"))
#         nbcol <- 100
#         color <- jet.colors(nbcol)
#         zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
#         facetcol <- cut(zfacet, nbcol)
#         zlab <- x$ic.type
#         z <- z[, lam_order]
#         persp(df_list, lam_list, z, col = color[facetcol], phi = 30, theta = -30, ticktype = "detailed", xlab = "DF", ylab =ifelse(sign.lambda == 1, expression(Log(lambda)), expression(lambda)), zlab = zlab)
#
#        }
#     }
#   }
# }
