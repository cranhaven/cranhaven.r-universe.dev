library(deSolve)

N = 10^0.31
x0 = c(N, 0.00001, 0.00001, 0.00002)
y <- c(X = x0)
times <- c( 0.0208,  0.1098,   0.2696,    0.4999,    0.8002,    1.1697,    1.6077,    2.1129,    2.6843,    3.3205,    4.0200,    4.7811,    5.6020,    6.4808,    7.4154,    8.4035,    9.4429,   10.5310, 11.6653,   12.8431,   14.0616,   15.3179,   16.6090,   17.9319,   19.2834,   20.6603,   22.0594,   23.4773,   24.9107,   26.3561,   27.8102,   29.2695,   30.7305,   32.1898,   33.6439,   35.0893, 36.5227,   37.9406,   39.3397,   40.7166,   42.0681,   43.3910,   44.6821,   45.9384,   47.1569,   48.3347,   49.4690,   50.5571,   51.5965,   52.5846,   53.5192,   54.3980,   55.2189,   55.9800, 56.6795,   57.3157,   57.8871,   58.3923,   58.8303,   59.1998,   59.5001,   59.7304,   59.8902,   59.9792)
parameters = 10^c("k1"=0.31, "k2"=-1, "k3"=-0.49, "k4"= 0.42, "s1"=-0.21, "s2"=-0.34)


inputData <- read.table('http://jeti.uni-freiburg.de/PNAS_Swameye_Data/DATA1_hall_inp')
inputData[nrow(inputData),2] = 0.009
colnames(inputData) <- c('t','u')
measure <- read.table('http://jeti.uni-freiburg.de/PNAS_Swameye_Data/DATA1_hall')
colnames(measure) <- c("t","y1","y1sd","y2","y2sd")



modelJakStat  <- function(t, x, parameters, input) {
  with (as.list(parameters),{
    
    u <- input$u(t)

    dx1 = -k1 * x[1]  * u
    dx2 = k1 *  x[1]  * u - k2 * x[2]^2
    dx3 = -k3*x[3] + 0.5*k2*x[2]*x[2]
    dx4 = k3 * x[3]

    list(c(dx1 ,dx2 ,dx3 ,dx4 ))
  })
}

measJakStat <- function(x) {

  s1 <- 10^(-0.21)
  s2 <- 10^(-0.34)

  y1 = s1*(x[,2]+ 2*x[,3])
  y2 = s2*(x[,1] + x[,2] + 2*x[,3])

  return(cbind(y1,y2))
}

y <- data.frame(measure[,1], measure[,2], measure[,4])
sd <- cbind(measure[,1] ,measure[,3], measure[,5])

JakStatConst <- '2*x4+ 2*x3 + x1 + x2 == N'
system.time(
results <- DEN(alphaStep = 0.01, alpha2 = 0.4, Beta = 0.8, epsilon = 0.2,
               x0 = x0, optW = c(1,1,1,1) ,
               measFunc= measJakStat,  measData = y, sd = sd,
               parameters = parameters, systemInput = inputData,
               modelFunc = modelJakStat, plotEstimates = TRUE, conjGrad = FALSE, cString = JakStatConst, nnStates = c(0,0,0,0)))

statesAnno <- c("STAT5 cyt.", "STAT5p cyt.", "STAT5p-d cyt.", "stat5-d nucl")
measurAnno <- c("total STAT5p", "total STAT5")

plotAnno(results[[2]],stateAnno = statesAnno, measAnno =  measurAnno)

