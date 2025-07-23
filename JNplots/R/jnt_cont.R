#' Calculation and visualization of regions of non-significance to assess the
#' influence of continuous moderators
#'
#' Produces a plot showing how changes in the moderator affect the slope and
#' significance of the relationship between the dependent variable and the predictor.
#' @param X A character string defining the name of the covariate (e.g., size in
#'  an allometry analysis). Must be the same as the name of the variable in the
#'  dataset (see argument “data”).
#' @param Y A character string defining the name of the dependent variable. Must
#'  be the same as the name of the variable in the dataset (see argument “data”).
#' @param m A character string defining the name of a continuous moderator.
#'  Must be the same as the name of the variable in the dataset (see argument “data”).
#'  The variable must be continuous.
#' @param data A dataframe containing the variables in the model.
#' @param alpha.sig A value representing the significance value (alpha) to be considered.
#' @param correlation an optional \link{corStruct} object describing the within-group
#'  correlation structure. See the documentation of \link{corClasses} for a description of
#'  the available corStruct classes. If a grouping variable is to be used, it must be
#'  specified in the form argument to the corStruct constructor. Defaults to NULL,
#'  corresponding to uncorrelated errors.
#' @param res A numerical value that aids in the plotting of regions of (non)significance.
#'  Default=100, higher numbers increase the number of fitted regression lines plotted (N=res-1).
#' @param xlab A title for the X axis. Defaults to the name of the predictor variable
#'  in the data.
#' @param ylab A title for the Y axis. Defaults to the name of the dependent variable
#'  in the data.
#' @param col.gradient A logical indicating whether the significant regression lines
#'  should be plotted with a gradient of colors representing moderator values. Defaults to 'TRUE'.
#' @param sig_color If col.gradient = FALSE, a character string indicating the color of the significant
#'  regression lines. Defaults to 'lightblue'.
#' @param nonsig_color If col.gradient = FALSE, a character string indicating the color of the non-significant
#'  regression lines. Defaults to 'grey'.
#' @param max_col_grad If col.gradient = TRUE, a character string indicating the maximum color of
#'  the gradient.
#' @param min_col_grad If col.gradient = TRUE, a character string indicating the minimum color of
#'  the gradient.
#' @param legend A logical indicating whether a legend should appear on top of the plot. Defaults to 'TRUE'.
#' @return List with six elements: (1) results from the linear model, (2) lower and
#' (3) upper limits of (non)significance in the moderator, (4) lower and (5) upper
#' data limit in the data, and (6) a graphical output.
#' @import nlme scales ape
#' @importFrom grDevices colorRampPalette rgb recordPlot
#' @importFrom graphics abline par points polygon
#' @importFrom stats complete.cases na.omit qf vcov
#' @examples
#' #### non-phylogenetic model ####
#' data(lizard_home_range)
#' jnt_cont(X='PHR95_overlap_z', Y='hrsize95', m='degree_z',
#' data=lizard_home_range, xlab = 'home range overlap 95',
#' ylab='home range size 95')
#'
#' #### phylogenetic model ####
#' jnt_cont(X='bio12', Y='back_bright', m='bio1', data=bird_colors,
#' correlation=corPagel(1, tree_Furnariidae),xlab='precipitation (mm)',
#' ylab='back brightness (scaled)',res=200)
#' @references Toyama, K. S. (2023). JNplots: an R package to visualize outputs
#' from the Johnson-Neyman technique for categorical and continuous moderators,
#' including options for phylogenetic regressions. bioRxiv, 2023-05.
#' @export

jnt_cont <- function(X,Y,m,data,alpha.sig=0.05,correlation=NULL,res=100,xlab=X,ylab=Y,col.gradient=TRUE,
                     sig_color="lightblue",nonsig_color="grey",max_col_grad="red",min_col_grad="blue",
                     legend=TRUE){

  Xi <- data[,X]
  Yi <- data[,Y]
  gi <- data[,m]
  mod <- nlme::gls(Yi~Xi*gi, correlation=correlation,
             na.action = na.omit)
  mod.out <- summary(mod)
  varcov <- vcov(mod.out)
  coef1 <- mod.out$coefficients[2]
  coef3 <- mod.out$coefficients[4]
  var_coef1 <- varcov[2,2]
  var_coef3 <- varcov[4,4]
  cov_coefs1_3 <- varcov[2,4]

  crit <- qt(p = alpha.sig/2, df = Inf)

  a <- (crit^2)*(var_coef3)-(coef3)^2
  b <- 2*((crit^2)*(cov_coefs1_3)-(coef1*coef3))
  c <- (crit^2)*var_coef1-(coef1)^2

  x1 <- (-b-sqrt((b^2)-(4*a*c)))/(2*a)
  x2 <- (-b+sqrt((b^2)-(4*a*c)))/(2*a)

  mm <- c("It was not possible to calculate regions of non-significance. The difference between slopes might not be statistically significant")
  if(((b^2)-4*a*c)<0){
    warning(mm)
  }

  plot(data[,X],data[,Y],xlab=xlab,ylab=ylab)

  val <- min(min(data[,m],na.rm = TRUE),min(x1,x2)) # minimum value to plot
  valmin <- min(data[,m],na.rm = TRUE) # minimum value in data
  val_max <- max(max(data[,m],na.rm = TRUE),max(x1,x2)) # maximum value to plot
  valmax <- max(data[,m],na.rm = TRUE) # maximum value in data

  # test if significant values are inside or outside x1 and x2
  inside_color_initial <- nonsig_color
  outside_color_initial <- sig_color
  test_x <- min(x1,x2)
  list_b <- c()
  inside_sig = TRUE
  nn <- (val_max-val)/res
  c <- 1
  while(test_x<=max(x1,x2)){
    list_b[c] <- (mod.out$coefficients[2]+mod.out$coefficients[4]*test_x)
    c <- c+1
    test_x <- test_x+nn
  }
  if (min(list_b)<0 & max(list_b)>0){
    inside_sig <- FALSE
  }

  if (inside_sig==TRUE){
    inside_color_initial <- sig_color
    outside_color_initial <- nonsig_color
  }

  if (inside_sig==TRUE){ # how many regression lines will be plotted as significant? (need for color gradient)
    gradient_sep <- res/length(list_b)
  } else {
    gradient_sep <- res/(res-length(list_b))
  }

  nn <- (val_max-val)/res
  colfunc <- colorRampPalette(c(max_col_grad, min_col_grad))
  col.gradient.list <- colfunc(res)
  col.gradient.list <- rev(col.gradient.list) # reversing so colors align with direction of values
  aaa <- c()
  c <- 1
  c_col <- 1
  while(val+nn<val_max){
    if (col.gradient==TRUE){
      if(val+nn>min(x1,x2) & val+nn<max(x1,x2)){ # does it fall within the region*?
        if(inside_color_initial == sig_color){ # is it a region* of significance?
          inside_color <- col.gradient.list[c_col*gradient_sep] # if it is, use the col gradient
          c_col <- c_col+1
        }else{ # if it is not, use the color for non significant regions
          inside_color <- nonsig_color
        }
        abline(a=(mod.out$coefficients[1]+mod.out$coefficients[3]*val),b=(mod.out$coefficients[2]+mod.out$coefficients[4]*val),col=scales::alpha(inside_color,0.5))
        aaa[c] <- val
        val <- val+nn
        c <- c+1
      }else{
        if(col.gradient==TRUE & outside_color_initial == sig_color){ # is the outside region one of significance?
          outside_color <- col.gradient.list[c_col*gradient_sep] # if it is, use the color gradient
          c_col <- c_col+1
        }else{ # if it is not, use the color for non significant regions
          outside_color <- nonsig_color
        }
        abline(a=(mod.out$coefficients[1]+mod.out$coefficients[3]*val),b=(mod.out$coefficients[2]+mod.out$coefficients[4]*val),col=scales::alpha(outside_color,0.5))
        aaa[c] <- val
        val <- val+nn
        c <- c+1
      }
    }else{
      if(val+nn>min(x1,x2) & val+nn<max(x1,x2)){ # does it fall within the region*?
        if(inside_color_initial == sig_color){ # is it a region* of significance?
          inside_color <- sig_color # if it is, use the specified sig color
        }else{
          inside_color <- nonsig_color
        }
        abline(a=(mod.out$coefficients[1]+mod.out$coefficients[3]*val),b=(mod.out$coefficients[2]+mod.out$coefficients[4]*val),col=scales::alpha(inside_color,0.5))
        aaa[c] <- val
        val <- val+nn
        c <- c+1
      }else{ # if it is outside the region*...
        if(outside_color_initial == sig_color){ # is the outside region one of significance?
          outside_color <- sig_color # if it is, use the specified sig color
        }else{
          outside_color <- nonsig_color
        }
        abline(a=(mod.out$coefficients[1]+mod.out$coefficients[3]*val),b=(mod.out$coefficients[2]+mod.out$coefficients[4]*val),col=scales::alpha(outside_color,0.5))
        aaa[c] <- val
        val <- val+nn
        c <- c+1
      }
    }
  }
  abline(a=(mod.out$coefficients[1]+mod.out$coefficients[3]*min(data[,m],na.rm = TRUE)),
         b=(mod.out$coefficients[2]+mod.out$coefficients[4]*min(data[,m],na.rm = TRUE)),
         col="black",lwd=1,lty=2)
  abline(a=(mod.out$coefficients[1]+mod.out$coefficients[3]*max(data[,m],na.rm = TRUE)),
         b=(mod.out$coefficients[2]+mod.out$coefficients[4]*max(data[,m],na.rm = TRUE)),
         col="black",lwd=1,lty=1)
  if(legend==TRUE){
    if(col.gradient==TRUE){
      legend(par('usr')[1],par('usr')[4]+((par('usr')[4]-par('usr')[3])/5), bty='n', xpd=NA,
             c("max mod value", "min mod value", "non-sig relationships"),
             lty=c(1,2,1),lwd=c(1.5,1.5,1.5),cex=0.7,col=c("black","black",nonsig_color))
      legend(par('usr')[2]-((par('usr')[2]-par('usr')[1])/2),par('usr')[4]+((par('usr')[4]-par('usr')[3])/5), bty='n', xpd=NA,
             c("higher mod values", "lower mod values"),
             lty=c(1,1),lwd=c(1.5,1.5),cex=0.7,col=c(max_col_grad,min_col_grad))
    }else{
      legend(par('usr')[1],par('usr')[4]+((par('usr')[4]-par('usr')[3])/5), bty='n', xpd=NA,
             c("max mod value", "min mod value", "non-sig relationships"),
             lty=c(1,2,1),lwd=c(1.5,1.5,1.5),cex=0.7,col=c("black","black",nonsig_color))
      legend(par('usr')[2]-((par('usr')[2]-par('usr')[1])/2),par('usr')[4]+((par('usr')[4]-par('usr')[3])/5), bty='n', xpd=NA,
             c("sig relationships"),
             lty=c(1,1),lwd=c(1.5,1.5),cex=0.7,col=sig_color)
    }

  }
  pl <- recordPlot()
  results <- list("coeff" = mod.out,"lower (non)significance limit of moderator" = min(x1,x2),
                  "upper (non)significance limit of moderator" = max(x1,x2),
                  "lower data limit" = valmin, "upper data limit" = valmax, pl)
  return(results)
}
