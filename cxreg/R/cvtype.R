cvtype <- function(type.measure="mse",subclass="classofit"){

      type.measures <- c("mse","deviance")
      devname <- switch(subclass, classofit="Mean-Absolute Error")
      typenames <- c(deviance = devname, mse = "Mean-Absolute Error")
      subclass.ch <- switch(subclass, classofit=c(1,2,5))
      subclass.type <- type.measures[subclass.ch]

      if(type.measure=="default"){
        type.measure=subclass.type[1]
      }

      model.name <- switch(subclass, classofit="Gaussian")
      if(!match(type.measure,subclass.type,FALSE)){
        type.measure <- subclass.type[1]
        warning(paste("Only ",paste(subclass.type,collapse=", "),"
                      available as type.measure for ",model.name," models; ",
                      type.measure," used instead",sep=""),call.=FALSE)
      }
      names(type.measure) <- typenames[type.measure]
      type.measure
}
