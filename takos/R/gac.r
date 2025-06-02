#' Title sbAC
#' @description Performs simulation according to several kinetic models
#' @param time.start Starting time for the simulations
#' @param T0 Temperature start
#' @param T.end End temperature
#' @param qqq Heating rate
#' @param A Parameter in the equation
#' @param Ea Parameter in the equation
#' @param m Parameter in the equation
#' @param n Parameter in the equation
#' @param K Parameter in the equation
#' @param npoints Number of points
#' @param prec Starting value for the equation "prec"
#' @param rmod Kinetic model (default = Isoda)
#' @param ... Parameters to pass to ode function for choosing solver method
#'
#' @return startgin temperature "T","fi",degree of crystallization "alfa",differential alfa in T "dadT",time in seconds "time.s",differential equation solution "sol"
#' @export
#' @rawNamespace import(deSolve,except=rk4)
#' @examples  \donttest{
#'
#' gAC(npoints=5000,prec=10^(-4.30095790876))
#'
#' }

#library(deSolve)
gAC<-  function (time.start=0,
                   T0=0, T.end=500,
                   qqq=50,
                   A=10^(6.3),
                   Ea=80000,
                   m=1,n=2,K=0,
                   npoints=10000,
                   prec=10^(-4.30095790876),rmod="SB",
                   ...)
{
  R <- 8.314        #iupac gold book gas constant
  Ts <- 273.15+ T0  #transformation in K

  time.e=(T.end - T0)/(qqq/60) #estimated end time for the analysis depending on the rate
  time.s=seq(time.start,time.e, length.out=npoints) #vector with all the times for solving the equations time in seconds
  tm = time.s
  Temp = Ts+(time.s*(qqq/60)) #temperatures calculated at each time

    fa <- switch(rmod,
				"RO1"="(1-y1)^n*(1+K*y1)",
				"RO2"="m*(1-y1)*((-log(1-y1))^(1-(1/m)))",
				"RO3"="(1-y1)^n",
				"SB"="y1^m*(1-y1)^n",
				"P1"="4*y1^(3/4)",
				"P2"="3*y1^(2/3)",
				"P3"="2*y1^(1/2)",
				"P4"="(2/3)*y1^-(1/2)",
				"D1"="(1/2)*(y1^-1)",
				"F1"="1-y1",
				"y14"="4*(1-y1)*((-log(1-y1))^(3/4))",
				"y13"="3*(1-y1)*((-log(1-y1))^(2/3))",
				"y12"="2*(1-y1)*((-log(1-y1))^(1/2))",
				"D3"="((3/2)*(1-y1)^(2/3))*(1-(1-y1)^(1/3))^-1",
				"D4"="(3/2)/(((1-y1)^(-1/3))-1)",
				"R3"="3*(1-y1)^2/3",
				"R2"="2*(1-y1)^1/2",
				"D2"="(-log(1-y1))^-1",
				"JMA"="m*(1-y1)*((-log(1-y1))^(1-(1/m)))",
				"Ih"="y1*(1-y1)",
				"F2"="(1-y1)^2")


####
#function to solve
SBf = function(tm, state, parms)
{
with(as.list(c(state, parms)),
{

   a1_tm = Ts+(tm*qqq/60)   # Karline: this can be calculated here based on the true time.

   dy1 =   A* exp(-Ea/(R*a1_tm))  * eval(parse(text=fa)) #this should do the trick
   return(list(dy1,                 # first return value = derivative (used to integrate)
               Temp = a1_tm,        # second value = calculated temperature
               fi = dy1))           # the derivative again - now used for output
   })
}
#end of the function
####

state = c(y1 = prec) #
P <- list(Ts = Ts, qqq = qqq, A = A)
sol <- ode(y = state, times = tm, parms = P, func = SBf, ...)
###
plot(sol) #a visual check of the results obtained
###
T.C <- Temp-273.15
T.K <- Temp
timef <- sol[,1]
alfa <- sol[,2]*100
dadT=c(0,diff(sol[,2])/diff(T.C))
my.list <- list("T.C" = T.C, "T.K"=T.K,"sol"=sol,"fi"=sol[,4],"alfa"=alfa, "dadT"=dadT, "time.s"=timef)
return(my.list)
}



