## HQCurve: no visible binding for global variable ‘fb’
## HQCurve: no visible binding for global variable ‘Q’
## HQCurve: no visible binding for global variable ‘x’
## HQCurve: no visible binding for global variable ‘y’
## HQCurve: no visible binding for global variable ‘group.value’

if(getRversion() >= "2.15.1") globalVariables(c('fb', 'Q', 'x', 'y', 'group.value'))

HQCurve<-function(pump){
  w1 <- 3000 #synchronous rpm frequency
  wm <- 2870 #rpm frequency with slip when applying voltage at 50 Hz
  s <- (w1-wm)/w1
  fen <- 50 #Nominal electrical frequency

  f <- seq(35, 50, by = 5)
  Hn <- with(pump, a*50^2+b*50*Qn+c*Qn^2) #height corresponding to flow rate and nominal frequency

  kiso <- Hn/pump$Qn^2 #To paint the isoyield curve I take into account the laws of similarity
  Qiso <- with(pump,seq(0.1*Qn,Qmax,l=10))
  Hiso <- kiso*Qiso^2 #Isoperformance curve

  Curva <- expand.grid(fb=f,Q=Qiso)

  Curva <- within(Curva,{
      fe = fb/(1-s)
      H = with(pump,a*fb^2+b*fb*Q+c*Q^2)

    is.na(H) <- (H<0)
    Q50 <- 50*Q/fb
    H50 <- H*(50/fb)^2
    etab <- with(pump,j*Q50^2+k*Q50+l)
    Pb50 <- 2.725*H50*Q50/etab
    Pb <- Pb50*(fb/50)^3

    Pbc <- Pb*50/fe
    etam <- with(pump,g*(Pbc/Pmn)^2+h*(Pbc/Pmn)+i)
    Pmc <- Pbc/etam
    Pm <- Pmc*fe/50

    etac <- 0.95 #Variable frequency drive performance
    cab <- 0.05  #Cable losses
    Pdc <- Pm/(etac*(1-cab))
    rm(etac,cab,Pmc,Pbc,Pb50,Q50,H50)
  })

###H-Q curve at different frequencies
  ##I check if I have the lattice package available, which should have been loaded in .First.lib
  lattice.disp <- ("lattice" %in% .packages())
  latticeExtra.disp <- ("latticeExtra" %in% .packages())
  if (lattice.disp && latticeExtra.disp) {
      p <- xyplot(H~Q, groups = factor(fb), data = Curva,
                  type = 'l', par.settings = custom.theme.2(),
                  panel = function(x, y, groups, ...){
                      panel.superpose(x, y, groups, ...)
                      panel.xyplot(Qiso, Hiso, col='black', ...)
                      panel.text(Qiso[1], Hiso[1], 'ISO', pos = 3)}
              )
    p=p+glayer(panel.text(x[1], y[1], group.value, pos=3))
    print(p)
    result <- list(result = Curva, plot = p)
  } else {
    warning('lattice and/or latticeExtra packages are not available. Thus, the plot could not be created')
    result <- Curva}
}
