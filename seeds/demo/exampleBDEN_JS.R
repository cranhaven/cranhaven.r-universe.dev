library(deSolve)

# parameter des models
N = 10^0.31
parameters = 10^c("k1"=0.31, "k2"=-1, "k3"=-0.49, "k4"= 0.42, "s1"=-0.21, "s2"=-0.34)

x0 = c(N, 0, 0, 0)+0.00001

inputData                    <- read.table('http://jeti.uni-freiburg.de/PNAS_Swameye_Data/DATA1_hall_inp')#csv
#inputData                    <- read.table('../data/DATA1_hall_inp.txt')#csv
inputData[nrow(inputData),2] <- 0.009
colnames(inputData)          <- c('t','u')
measure                      <- read.table('http://jeti.uni-freiburg.de/PNAS_Swameye_Data/DATA1_hall')#csv
#measure                      <- read.table('../data/DATA1_hall.txt')#csv
colnames(measure)            <- c("time","STAT5p_cyt" ,"sd_STAT5p_cyt","STAT5ptot_cyt","sd_STAT5ptot_cyt")

sd                           <- cbind(measure['time'],measure['sd_STAT5p_cyt'],measure['sd_STAT5ptot_cyt'])

# uncertainty per timepoint and endpoint

y                            <- cbind(measure['time'],
                                      ((measure['STAT5ptot_cyt']/parameters['s2'])-(measure['STAT5p_cyt']/parameters['s1'])),
                                      measure['STAT5p_cyt'],
                                      measure['STAT5ptot_cyt'],
                                      (x0[1]-(measure['STAT5ptot_cyt']/parameters['s2']))/2/(1400/450)
)


y[y<0]                       <- 0
colnames(y)                  <- c("time", "STAT5" ,"STAT5p_cyt","STAT5ptot_cyt","STAT5_n")


modelJakStat  <- function(t, x, parameters, input) {
  with (as.list(parameters),{
    
    k1 = parameters[1]
    k2 = parameters[2]
    k3 = parameters[3]
    
    
    
    u <- input$u(t)
    
    dx1 = -k1 * x[1]  * u
    dx2 = k1 *  x[1]  * u - k2 * x[2]^2
    dx3 = -k3*x[3] + 0.5*k2*x[2]*x[2]
    dx4 = k3 * x[3]
    
    
    list(c(dx1 ,dx2 ,dx3 ,dx4 ))
  })
}



objectiveJakStat<- function(x,parameter=c(0,0)) {
  
  y1 = x[,1]
  y2 = parameter[5] * (x[,2] + 2 * x[,3])
  y3 = parameter[6] * (x[,1] + x[,2] + 2 * x[,3])
  y4 = x[,4]
  
  return(cbind(y1,y2,y3,y4))
}



JakStatModel <- odeModel(func = modelJakStat, parms = parameters, input = inputData, times=c(0,60),
                         measFunc = objectiveJakStat, y = x0, meas = y, sd = sd,custom=TRUE)


A <- BDEN(odeModel               = JakStatModel,
          lambda            = .0001,
          beta_init         = c(1,1,1,0.1),
          numbertrialsstep = 15,
          numbertrialseps  = 1000,
          numbertrialinner  = 25,
          printstatesignore =  c(1,4))



