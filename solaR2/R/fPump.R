fPump <- function(pump, H){

    w1 <- 3000 ##synchronous rpm frequency
    wm <- 2870 ##rpm frequency with slip when applying voltage at 50 Hz
    s <- (w1-wm)/w1
    fen <- 50 ##Nominal electrical frequency
    fmin <- sqrt(H/pump$a)
    fmax <- with(pump, (-b*Qmax+sqrt(b^2*Qmax^2-4*a*(c*Qmax^2-H)))/(2*a))
    ##fb is rotation frequency (Hz) of the pump,  
    ##fe is the electrical frequency applied to the motor
    ##which makes it rotate at a frequency fb (and therefore also the pump).
    fb <- seq(fmin,min(60,fmax),length = 1000) #The maximum frequency is 60
    fe <- fb/(1-s)
    
###Flow
    Q <- with(pump, (-b*fb-sqrt(b^2*fb^2-4*c*(a*fb^2-H)))/(2*c))
    Qmin <- 0.1*pump$Qn*fb/50
    Q <- Q+(Qmin-Q)*(Q<Qmin)
    
###Hydraulic power
    Ph <- 2.725*Q*H
    
###Mechanical power
    Q50 <- 50*Q/fb
    H50 <- H*(50/fb)^2
    etab <- with(pump, j*Q50^2+k*Q50+l)
    Pb50 <- 2.725*H50*Q50/etab
    Pb <- Pb50*(fb/50)^3
    
###Electrical power
    Pbc <- Pb*50/fe
    etam <- with(pump, g*(Pbc/Pmn)^2+h*(Pbc/Pmn)+i)
    Pmc <- Pbc/etam
    Pm <- Pmc*fe/50
    Pac <- Pm
    ##Pdc = Pm/(etac*(1-cab))
    
###I build functions for flow, frequency and powers
###to adjust the AC power.
    fQ <- splinefun(Pac,Q)
    fFreq <- splinefun(Pac,fe)
    fPb <- splinefun(Pac,Pb)
    fPh <- splinefun(Pac,Ph)
    lim <- c(min(Pac),max(Pac))
    ##lim marks the operating range of the pump
    result <- list(lim = lim,
                 fQ = fQ,
                 fPb = fPb,
                 fPh = fPh,
                 fFreq = fFreq)
}




