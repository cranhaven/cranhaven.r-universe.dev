assign("idw.cv",
      function(formula, locations, data, nmax = Inf, nmin = 0, p = 2, progress=FALSE, ...){
      z = extractFormula(formula, data, newdata=data)$z
      idw.pred <- as.data.frame(matrix(NA,nrow= nrow(data), ncol=4))
      colnames(idw.pred) <- c("x","y","var1.pred","var1.var")
      if(progress)
      pb <- txtProgressBar(min = 0, max = length(z), char = "=", style = 3)
      for(i in 1:length(z)){
      idw.pred[i,] <- idw(formula, locations, data[-i,], data[i,], nmax, nmin, idp=p, debug.level=0, ...)
      if(progress)
      setTxtProgressBar(pb, i)
      }
      if(progress)
      close(pb)
      RMSPE <-  sqrt(sum((idw.pred$var1.pred-z)^2)/length(z))
      RMSPE
}
)
