tseg <- function(n, which=c("BJAR2","BJAR1", "BJAR3", "PWAR4", 
            "BJARMA11", "MHAR9", "NileMin", "SB32")) {
  which <- match.arg(which)
  ans <- switch(which,
         BJAR1 =   (1.17/(1-0.87)) + 
                  arima.sim(model=list(ar=0.87), n=n, sd=sqrt(0.09)),
         BJAR2 =   (14.35/(1-sum(c(1.42, -0.73)))) + 
              arima.sim(model=list(ar=c(1.42, -0.73)), n=n, sd=sqrt(227.8)),
         #sqrt(227.8) = 15.09304
         BJAR3 =   (11.31/(1-sum(c(1.57, -1.02, 0.21)))) + 
           arima.sim(model=list(ar=c(1.57, -1.02, 0.21)), n=n, sd=sqrt(218.1)),
         PWAR4 = arima.sim(model=list(ar=c(2.7607,-3.8106,2.6535,-0.9238)), 
                           n=n), #Percival and Walden, p.45
         BJARMA11 =  1.45/(1-0.92) +
           arima.sim(model=list(ar=0.92, ma=-0.58), n=n, sd=sqrt(0.097), 
                     n.start=1000), #sqrt(0.097)=0.3114482
         MHAR9 = 11.17 +
           arima.sim(model=list(ar=c(1.2434, -0.5192, 0,0,0,0,0,0, 0.1954)), 
                     n=n, sd=2.0569, n.start=1000),
           #McLeod, Hipel & Lennox (1978, p.581)
         NileMin = 11.48+artsim(n, d=0.393, sigma2=0.4894),
         SB32 = -0.5559+artsim(n, d=5/6, lambda=0.045, sigma2 = 3.573)
           )
  as.vector(ans)
}

