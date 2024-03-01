print.nonparasccs <-
  function(x, digits = max(getOption('digits')-3, 3),  
           signif.stars = getOption("show.signif.stars"), ...) {
    
    
    #cat("Smoothing parameter of the spline based age relative incidence = ", x$smoothingpara)
    
    cat("Non parametric self controlled case series", "\n") 
    cat("Age related relative incidence function:", "\n", "Smoothing parameter = ", format(x$lambda1,  scientific = T,digits=2), "\n", "Cross validation score = ", round(x$cv1, 2), "\n", "\n")
    cat("Exposure related relative incidence function:", "\n", "Smoothing parameter = ", format(x$lambda2,  scientific = T,digits=2), "\n", "Cross validation score = ", round(x$cv2, 2), "\n")
    
    
    
    invisible()
  }
