Draw <-  function(Model, Plot_wrt, LB = NULL, UB = NULL, Values = NULL,
                Response_ID = NULL, res = 15, X1Label = NULL, X2Label = NULL,
                YLabel = NULL, Title = NULL, PI95 = NULL){

  if (class(Model) != "GPM"){
    stop('The 1st input should be a model of class GMP built by Fit.')
  }
  dx <- ncol(Model$Data$XN)
  dy <- ncol(Model$Data$YN)

  if (!is.vector(Plot_wrt)) Plot_wrt <- as.vector(Plot_wrt)
  if (length(Plot_wrt) != dx){
    stop(paste('     The size of Plot_wrt is incorrect! It should be', toString(dx)))
  } else if (any(Plot_wrt>1) || any(Plot_wrt<0) || any((Plot_wrt<1)*(Plot_wrt>0))){
    stop('The elements of Plot_wrt should be either 1 or 0.');
  } else if (sum(Plot_wrt)>2 || sum(Plot_wrt)<1){
    stop('A maximum (minimum) of 2 (1) elements of Plot_wrt (corresponding to the dimensions used in plotting) should be 1.')
  }
  Axes <- (Plot_wrt==1)
  NotAxes <- (Plot_wrt==0)
  SorL <- sum(Plot_wrt)

  ## range of drawing
  if (is.null(LB)){LB <- Model$Data$Xmin[Axes]}
  LB <- as.vector(LB)
  if (is.null(UB)){UB <- Model$Data$Xmax[Axes]}
  UB <- as.vector(UB)
  if (SorL == 1){
    if (length(LB)!=1 || length(UB)!=1){
      stop('LB and UB need to be a scalar for drawing a 1D plot')
    }
  } else {
    if (length(LB)!=2 || length(UB)!=2){
      stop('LB and UB need to be of size [1, 2] for drawing a 2D surface')
    }
  }
  if (any(LB>UB)){
    stop('The elements of LB must be smaller than the corresponding elements of UB')
  }
  ## settings for the fixed variables
  if ((dx - SorL)>0){
    if (is.null(Values)){
      Values <- (Model$Data$Xmin[NotAxes] + Model$Data$Xmax[NotAxes])/2
      print('The fixed inputs, are set to the default values.')
    }
    if (length(Values)!=(dx - SorL)){
      stop(paste('The size of Values should be [1, ', toString(dx - SorL), '].'))
    }
  } else {
    if (!is.null(Values)){
      warning('The values assigned to "Values" for the variable fixed at plotting, are ignored.')
      Values <- NULL
    }
  }
  if (any(Values < Model$Data$Xmin[NotAxes]) || any(Values > Model$Data$Xmax[NotAxes])){
    warning('(Some of) The values assigned to the fixed variables are beyond the region where the model is fitted.')
  }
  ## Output ID, Resolution, axis labels, title, PI95
  if (dy>1){
    if (is.null(Response_ID)){
      Response_ID <- 1
      print('There is more than 1 output in the model. The 1st one is chosen for drawing.')
    } else {
      if (length(Response_ID) != 1){
        stop(paste('Response_ID should be an integer between 1 and ', toString(dy), '.'))
      } else if (Response_ID>dy || Response_ID<1){
          stop(paste('Response_ID should be an integer between 1 and ', toString(dy), '.'))
      } else if (round(Response_ID) != Response_ID){
        stop(paste('Response_ID should be an integer between 1 and ', toString(dy), '.'))
      }
    }
  } else {
    Response_ID <- 1
  }
  if (!is.null(res)){
    if (length(res)!=1){
      stop('"res" should be a scalar.')
    }
  }
  temp <- which(Axes)
  if (is.null(X1Label)){
    X1Label <- paste('X', toString(temp[1]))
  } else if (!is.character(X1Label)){
    stop('X1Label should be a string.')
  }
  if (is.null(X2Label)){
    X2Label <- paste('X', toString(temp[2]))
  } else if (!is.character(X2Label)){
    stop('X2Label should be a string.')
  }
  if (is.null(YLabel)){
    YLabel <- 'Response'
  } else if (!is.character(YLabel)){
    stop('YLabel should be a string.')
  }
  temp <- NULL; j<-0;
  for (i in which(NotAxes)){
    j <- j + 1
    temp <- paste(temp, '  X', toString(i), '=', toString(round(Values[j]*100)/100))
  }
  if (is.null(Title)){
    Title <- temp
  } else if (!is.character(Title)){
    stop('Title should be a string.')
  }
  if (is.null(PI95)){
    PI95 <- 1
  } else if (length(PI95)!=1){
    stop('PI95 should be a scaler. Set it to a non-zero value to turn the flag "on".')
  } else if (PI95!=0 && PI95!=1){
    PI95 <- 1
  }
  ## set the colors
  mycolors.trans <- grDevices::rgb(c(0,255,0),
                       c(255,0,0),
                       c(0,0,255),alpha = 50,maxColorValue = 255)

  mycolors <- grDevices::rgb(c(0,255,0),
                 c(255,0,0),
                 c(0,0,255),maxColorValue = 255)

  ## draw
  if (SorL == 1){
    x1 <- as.matrix(seq(LB, UB, length.out = res), res, 1)
    input <- matrix(0, res, dx)
    input[, Axes] <- x1
    if (!is.null(Values)){
      input[, NotAxes]<- t(replicate(res, Values))
    }
    Y <- Predict(input, Model, MSE_on = PI95)
    y <- Y$YF[, Response_ID]
    graphics::plot(x=x1, y=y, type = "l", lty = 1, lwd = 2, col = "blue",
                   main = Title, xlab = X1Label, ylab = YLabel)
    if (PI95 == 1){
      yp <- y + 1.96*sqrt(Y$MSE[, Response_ID]);
      ym <- y - 1.96*sqrt(Y$MSE[, Response_ID]);
      graphics::lines(x1, yp, type = "l", lty = 2, lwd = 1, col = "red")
      graphics::lines(x1, ym, type = "l", lty = 2, lwd = 1, col = "red")
      graphics::legend(x= "topright", legend = c('Prediction', '95% PI'), col = c("blue", "red"),
             lty = c(1, 2))
    } else {
      graphics::legend(x = "topright", legend = 'Prediction')
    }
  } else {
    if (PI95==1){
      x1 <- seq(LB[1], UB[2], length.out = res)
      x2 <- seq(LB[1], UB[2], length.out = res)
      g <- expand.grid(x = x1, y = x2, gr = 1:3)
      input <- matrix(0, res^2, dx)
      input[, Axes] <- as.matrix(g[1:res^2, -3])
      if (!is.null(Values)){
        input[, NotAxes]<- t(replicate(res^2, Values))
      }
      Y <- Predict(input, Model, PI95)
      y <- matrix(Y$YF[, Response_ID], res^2, 1)
      yp <- y + matrix(sqrt(Y$MSE[, Response_ID]), res^2, 1)
      ym <- y - matrix(sqrt(Y$MSE[, Response_ID]), res^2, 1)
      g$z[1:res^2] <- y
      g$z[(res^2+1):(2*res^2)] <- yp
      g$z[(2*res^2+1):(3*res^2)] <- ym

      wireframe(z ~ x * y, data = g, groups = g$gr, col.groups=mycolors.trans,
                xlab = list(X1Label, rot=30),
                ylab = list(X2Label, rot=-30), zlab = list(YLabel, rot=90),
                main = Title, scales = list(arrows = FALSE),
                key=list(text=list(c("Mean Prediction","95% Upper Bound","95% Lower Bound"),col=mycolors), space = "right",
                         lines=list(lty=c(1,1,1),col=mycolors), border = TRUE),
                par.settings = list(axis.line = list(col = "transparent")))
    } else {
      x1 <- seq(LB[1], UB[2], length.out = res)
      x2 <- seq(LB[1], UB[2], length.out = res)
      g <- expand.grid(x = x1, y = x2)
      input <- matrix(0, res^2, dx)
      input[, Axes] <- as.matrix(g)
      if (!is.null(Values)){
        input[, NotAxes]<- t(replicate(res^2, Values))
      }
      Y <- Predict(input, Model, PI95)
      y <- matrix(Y$YF[, Response_ID], res^2, 1)
      g$z <- y

      wireframe(z~x*y, data = g, xlab = list(X1Label, rot=30),
                ylab = list(X2Label, rot=-30), zlab = list(YLabel, rot=90),
                main = Title, scales = list(arrows = FALSE),
                drape = TRUE, colorkey = TRUE)
    }
  }
  }
