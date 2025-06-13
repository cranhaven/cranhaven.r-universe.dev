## ---- eval = FALSE------------------------------------------------------------
#  library(seeds)
#  
#  # usb network example
#  uvbParameter = c(  ks1=0.23,
#                   ks2=4.0526,
#                   kdr1=0.1,
#                   kdr2=0.2118,
#                   k1=0.0043,
#                   k2=161.62,
#                   ka1=0.0372,
#                   ka2=0.0611,
#                   ka3=4.7207,
#                   kd1=94.3524,
#                   kd2=50.6973,
#                   kd3=0.5508,
#                   ks3=0.4397,
#                   kdr3=1.246,
#                   uv=1,
#                   ka4=10.1285,
#                   kd4=1.1999,
#                   n1=3,
#                   n2=2,
#                   n3=3.5,
#                   kdr3a=0.9735,
#                   kdr3b=0.406,
#                   ksr=0.7537,
#                   fhy3_s=5)
#  # initial state
#  x0 = c(0.2,10,2,0,0,20,0,0,0,4.2,0.25,20,0)
#  
#  # model ode
#  uvbModel <- function(t,x,parameters) {
#    with (as.list(parameters),{
#  
#    dx1 = ((-2) * ((ka1 * (x[1]^2) * (x[4]^2)) - (kd1 * x[5])) +
#              (-2) * ((ka2 * (x[1]^2) * x[2]) - (kd2 * x[3])) +
#              ((ks1 *((1) + (uv * n3 * (x[11] + fhy3_s))))  -
#              (kdr1 * ((1) + (n1 * uv)) * x[1])))
#    dx2 = ((-1) * ((ka2*(x[1]^2) * x[2]) - (kd2 * x[3])) +
#              (-1) * ((ka4 * x[2] * x[12]) - (kd4 * x[13])))
#    dx3 = (((ka2 * (x[1]^2) * x[2]) - (kd2*  x[3])))
#    dx4 = ((-2) * (k1*(x[4]^2)) + (2) * (k2 * x[6]) +
#             (-2) * ((ka1 * (x[1]^2)* (x[4]^2)) - (kd1 * x[5])) +
#             (-1)* (ka3 * x[4] *x[7]))
#    dx5 =  (((ka1 * (x[1]^2) * (x[4]^2)) -(kd1 * x[5])))
#    dx6 = ((-1) * (k2 * x[6]) +  (k1 * (x[4]^2)) +(kd3 * (x[8]^2)))
#    dx7 = ((-1) * (ka3 * x[4] * x[7]) + ((ks2 * ((1) + (uv * x[5]))) -
#                (kdr2 * x[7])) + (2) * (kd3 * (x[8]^2)))
#    dx8 = ((-2) * (kd3 * x[8]^2) + (ka3 * x[4] * x[7]))
#    dx9  = 0
#    dx10 = 0
#    dx11 =  (((ks3 * ((1) + (n2 * uv))) -(kdr3 * (((x[3] / (kdr3a + x[3])) +
#              (x[13] / (kdr3b + x[13]))) -(x[5] / (ksr + x[5]))) *  x[11])))
#    dx12 = ((-1) * (ka4 * x[2] * x[12]) + (kd4 * x[13]))
#    dx13 =((ka4 * x[2] * x[12]) - (kd4 * x[13]))
#  
#    list(c(dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,dx9,dx10,dx11,dx12,dx13))
#    })
#  }
#  
#  # measurement function
#  uvbMeasure <- function(x) {
#  
#    y1 = 2*x[,5] + x[,4] + x[,8]
#    y2 = 2*x[,5] + 2* x[,3] + x[,1]
#    y3 = x[,6]
#    y4 = x[,11]
#    y5 = x[,4]
#  
#    return(cbind(y1,y2,y3,y4,y5))
#  }
#  
#  # measurement data
#  y <- uvbData[,1:6]
#  # timesteps
#  t <- uvbData$t
#  # standard deviation of the measurements
#  sd <- uvbData[,7:11]
#  
#  uvbModel <- odeModel(func = uvbModel, parms = uvbParameter, times = t,
#             measFunc = uvbMeasure, y = x0, meas = y, sd = sd)
#  
#  
#  res <- DEN(odeModel = uvbModel, alphaStep = 500, alpha2 = 0.0001, epsilon = 0.2, plotEstimates = FALSE)
#  
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  # plot the solution of the second iteration
#  plot(res[[2]])
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  library(seeds)
#  
#  
#  uvbParameter = c(  ks1=0.23,
#                     ks2=4.0526,
#                     kdr1=0.1,
#                     kdr2=0.2118,
#                     k1=0.0043,
#                     k2=161.62,
#                     ka1=0.0372,
#                     ka2=0.0611,
#                     ka3=4.7207,
#                     kd1=94.3524,
#                     kd2=50.6973,
#                     kd3=0.5508,
#                     ks3=0.4397,
#                     kdr3=1.246,
#                     uv=1,
#                     ka4=10.1285,
#                     kd4=1.1999,
#                     n1=3,
#                     n2=2,
#                     n3=3.5,
#                     kdr3a=0.9735,
#                     kdr3b=0.406,
#                     ksr=0.7537,
#                     fhy3_s=5)
#  
#  x0 = c(0.2,10,2,0,0,20,0,0,0,4.2,0.25,20,0)+0.00001
#  
#  uvbModel <- function(t,x,parameters) {
#    with (as.list(parameters),{
#  
#      dx1 = ((-2) * ((ka1 * (x[1]^2) * (x[4]^2)) - (kd1 * x[5])) +
#               (-2) * ((ka2 * (x[1]^2) * x[2]) - (kd2 * x[3])) +
#               ((ks1 *((1) + (uv * n3 * (x[11] + fhy3_s))))  -
#                  (kdr1 * ((1) + (n1 * uv)) * x[1])))
#      dx2 = ((-1) * ((ka2*(x[1]^2) * x[2]) - (kd2 * x[3])) +
#               (-1) * ((ka4 * x[2] * x[12]) - (kd4 * x[13])))
#      dx3 = (((ka2 * (x[1]^2) * x[2]) - (kd2*  x[3])))
#      dx4 = ((-2) * (k1*(x[4]^2)) + (2) * (k2 * x[6]) +
#               (-2) * ((ka1 * (x[1]^2)* (x[4]^2)) - (kd1 * x[5])) +
#               (-1)* (ka3 * x[4] *x[7]))
#      dx5 =  (((ka1 * (x[1]^2) * (x[4]^2)) -(kd1 * x[5])))
#      dx6 = ((-1) * (k2 * x[6]) +  (k1 * (x[4]^2)) +(kd3 * (x[8]^2)))
#      dx7 = ((-1) * (ka3 * x[4] * x[7]) + ((ks2 * ((1) + (uv * x[5]))) -
#                                             (kdr2 * x[7])) + (2) * (kd3 * (x[8]^2)))
#      dx8 = ((-2) * (kd3 * x[8]^2) + (ka3 * x[4] * x[7]))
#      dx9  = 0
#      dx10 = 0
#      dx11 =  (((ks3 * ((1) + (n2 * uv))) -(kdr3 * (((x[3] / (kdr3a + x[3])) +
#                                                       (x[13] / (kdr3b + x[13]))) -(x[5] / (ksr + x[5]))) *  x[11])))
#      dx12 = ((-1) * (ka4 * x[2] * x[12]) + (kd4 * x[13]))
#      dx13 =((ka4 * x[2] * x[12]) - (kd4 * x[13]))
#  
#      list(c(dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,dx9,dx10,dx11,dx12,dx13))
#    })
#  }
#  
#  
#  uvbMeasure <-   function(y,parameter){
#  
#    y1 = 2*y[,5] + y[,4] + y[,8]
#    y2 = 2*y[,5] + 2* y[,3] + y[,1]
#    y3 = y[,6]
#    y4 = y[,11]
#    y5 = y[,4]
#  
#    return(cbind(y1,y2,y3,y4,y5))
#  }
#  
#  
#  testNN = rep(0,length(x0))
#  testNN[1] = 1
#  
#  
#  y <- uvbData[,1:6]
#  t <- uvbData$t
#  sd <- uvbData[,7:11]
#  
#  
#  Model <- odeModel(func = uvbModel, parms = uvbParameter,times=c(0.025),
#                           measFunc = uvbMeasure, y = x0, meas = y, sd = sd,custom=TRUE)
#  
#  
#  
#  A <- BDEN(odeModel          = Model,
#            lambda            = .001,
#            beta_init         = c(1,1,1,1,1),
#            numbertrialsstep  = 15,
#            numbertrialseps   = 2000,
#            numbertrialinner  = 10)
#  

