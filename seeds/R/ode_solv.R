#Wrapper to the ODE Solver
ode_solv <- function(TIME,x_0,parameter,input,w_estimate){
  myEvent <- eval(parse(text = createEvent(tolerance = 0., value = 0.0001)))
  

  
  times = TIME


  if(!is.null(input)){
    inputApprox <- apply(X = input[,-1, drop=F], MARGIN = 2, FUN = function(x) stats::approx(x = input[,1], y = x, xout = times, rule = 2))
    inputApprox = list(cbind(times,inputApprox$u$y))
  } else {
    inputApprox <- list(cbind(times,rep(0,length(times))))
  }
  

  
  
  parametersW = c(TIME[1])
  NAMES <- 't0'
  for (i in 1:length(w_estimate[1,])) {
    NAMES <- cbind(NAMES,paste0('w',i,'t0'))
    parametersW <-cbind(parametersW, w_estimate[1,i])}
  
  colnames(parametersW) <- NAMES
  

  wSplit <- split(w_estimate, rep(1:ncol(w_estimate), each = nrow(w_estimate)))
  
  
  
  wList <- lapply(wSplit, FUN = function(x) cbind(times,x))


  
  forcings <- c(inputApprox, wList)

  parameters = c(parameter, parametersW)

  
  runSilent <- function() {

    
   sol <- deSolve::lsoda(y = x_0, time=times, func = "derivsc",
                                       parms = parameters, dllname = "model", initforc="forcc",
                                       forcings = forcings, initfunc = "parmsc",
                                       nroot = length(x_0),
                                       rootfunc = "myroot",
                                       events = list(func = myEvent, root = TRUE))
    
    sol

     # fcontrol=list(method="constant", f = 0),
  }
  
  
  sol <- runSilent()

    if (any(is.na(sol))|is.null(sol)|((sum(sol< 0)!=0))){
      
      print('Integration failed :: No success :: skip sample')
      
      return(NA)}
    
  
  
  return(as.data.frame(sol[,1:length(x_0)+1]))
  
  
  
}

