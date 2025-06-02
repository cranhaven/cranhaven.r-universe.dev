assign("graph.idw",
       function(formula, data, locations, np, p.dmax, P.T=NULL, nmax=Inf, nmin=0, pleg, progress=F, iter, ...)
       {
         if (!is.logical(P.T) & !is.null(P.T))
           stop(paste("P.T must be logical"))
         Opt <- optimize(idw.cv, c(0,p.dmax), formula=formula, locations=locations, data=data, nmax=nmax, 
                         nmin=nmin, progress=F, ...)
         Datos <- as.data.frame(matrix(NA, nrow = length(seq(1e-25, p.dmax, length.out = np)), ncol = 2))
         p <- seq(1e-25, p.dmax, length.out = np)
         colnames(Datos) <- c("P", "RMSPE")
         pb <- txtProgressBar(min = 0, max = np, char = "=", style = 3)
         for (i in 1:np) {
           Datos[i, 1] <- p[i]
           Datos[i, 2] <- idw.cv(formula, locations, data, p=p[i], nmax, nmin, progress=F)
           setTxtProgressBar(pb, i)
         }
         close(pb)
         Table0 <- rbind(Datos, c(Opt$minimum, Opt$objective))
         orden <- order(Table0$P)
         Table <- Table0[orden, ]
         plot(Table, lty = 1, ylab = "RMSPE", col = 4, xlab = "p", lwd=1.5, type = "l")      
         p <- recordPlot()
         Optim <- Table[which.min(Table[, 2]), ]
         legend(x=pleg,legend=c(list(paste("Optimal p =  ",round(Optim$P,4)),paste("RMSPE   = ",
                round(Optim$RMSPE,4)))), box.col=F, cex=0.8)
         cat("Optimal p IDW: ", "\n", "p  = ", Optim$P, "\n", "RMSPE   = ", Optim$RMSPE, "\n")
         list(table=Optim, p=p)
}
)

