plot.expectreg <-
    function(x, rug = TRUE, xlab = NULL, ylab = NULL, ylim = NULL,
             legend = TRUE, ci = FALSE, ask = NULL, cex.main = 2, 
             mar.min = 5, main = NULL, cols = "rainbow", 
             hcl.par = list(h=c(260,0), c=185, l=c(30,85)), 
             ylim_spat = NULL, ylim_factor = NULL, 
             range_warning = TRUE, add_intercept = TRUE, ... ) {
        
        op_save <- par(no.readonly = TRUE)
        on.exit(par(op_save))
        
        if(is.null(ask)) {
            ask = prod(par("mfcol")) < sum(unlist(x$effects) != "parametric") && dev.interactive() # sum(1-unlist(lapply(x$cov,is.factor))) 
        }
        if (ask) {
            oask <- devAskNewPage(TRUE)
            on.exit(devAskNewPage(oask))
        }
        
        if(inherits(x,"boost") && ci)
            warning("no confidence intervals are calculated while boosting.")
        yy = x$response
        cov = x$covariates
        Z = x$values
        coefficients = x$coefficients
        formula = x$formula
        intercept = x$intercepts
        m = length(yy)
        types = x$effects
        helper = x$helper
        ind_50 <- 1
        if(0.5 %in% x$asymmetries) { ind_50 <- which(x$asymmetries==0.5)}
        
        pp = x$asymmetries
        np <- length(pp)
        
        if(is.null(ylab))
            ylab = attr(yy,"name")
        
        ylim2 = ylim
        
        intercept2 <- intercept
        if(!add_intercept) intercept2[] <- 0
        
        if(identical(pp,seq(0.01, 0.99, by=0.01))) {
            pp.plot <- c(1,2,5,10,20,50,80,90,95,98,99)
            
            row.grid = 3
            col.grid = 4
        }  else  {
            if(identical(pp,c(0.01,0.02,0.05,0.1,0.2,0.5,0.8,0.9,0.95,0.98,0.99)))    {
                pp.plot <- 1:length(pp)
                
                row.grid = 3
                col.grid = 4
            } else {
                if(np > 15) {
                    pp.plot = seq(1,np,length=15)
                } else {
                    pp.plot <- 1:length(pp)
                }
                row.grid = floor(sqrt(length(pp)))
                col.grid = ceiling(sqrt(length(pp)))
                if (length(pp) > row.grid * col.grid) 
                    row.grid = row.grid + 1
            }
        }
        np.plot <- length(pp.plot)
        col_vector <- rainbow(np.plot+1)[1:np.plot]
        if(cols == "hcl") {
            col_vector <- diverge_hcl(n=np.plot,h=hcl.par$h,c=hcl.par$c,l=hcl.par$l)[1:np.plot]
        }
        
        if(is.null(xlab)){
            xlab = names(coefficients)
        } else {
            if(length(xlab) < length(types)){
                xlab = rep(xlab[1],length(types))
            }
        }
        
        main2 <- main
        if(!is.null(main)) {
            if(length(main) != length(types)) {
                main2 <- NULL
            }
            if(length(main == 1)) {
                main2 <- rep(main,times=length(types))
            }
        }
        
        
        nb = vector()
        for(k in 1:length(types))
        {
            # reset par() to the options defined before starting plotting
            # drawmap() changes them!!!
            if(all(op_save$mfrow == c(1,1)) & all(op_save$mcol==c(1,1)))
                par(op_save)
            
            #if(types[[k]] != "parametric" && !ask && all(par(no.readonly=TRUE)$mfrow == c(1,1)))# && dev.cur() == 1)
            #  dev.new()
            if(!inherits(x,"boost"))
            {
                nb[k] = ncol(x$bases[[k]]$B)
                partbasis = (sum(nb[0:(k-1)])+1):(sum(nb[0:k]))
            }
            
            if(types[[k]] == "pspline" || types[[k]] == "penalizedpart_pspline"  || types[[k]] == "tp") {
                
                
                
                if(inherits(x,"boost")) {
                    ZZZ = Z[[k]][order(cov[[k]])[seq(1,m,length=min(m,100,na.rm=T))],pp.plot,drop=F]
                } else {
                    ndat = data.frame(seq(min(cov[[k]],na.rm=T),max(cov[[k]],na.rm=T),length=100))
                    names(ndat) = names(cov)[k]
                    Bpred = predict(x$bases[[k]],ndat)
                    ZZZ = Bpred %*% coefficients[[k]]
                }
                
                lower=NA
                upper=NA
                if(ci) {
                    lower = matrix(NA,nrow=100,ncol=np)
                    upper = matrix(NA,nrow=100,ncol=np)
                    for(i in 1:np)
                        for(nn in 1:nrow(Bpred))
                        {
                            deviation = qnorm(0.975) * sqrt(t(c(1,Bpred[nn,])) %*% x$covmat[[i]][c(1,partbasis+1),c(1,partbasis+1)] %*% c(1,Bpred[nn,]))
                            lower[nn,i] = ZZZ[nn,i] - deviation
                            upper[nn,i] = ZZZ[nn,i] + deviation
                        }
                }
                
                for(i in 1:np)
                {
                    ZZZ[,i] = ZZZ[,i] + intercept2[i]
                    if(ci)
                    {
                        lower[,i] = lower[,i] + intercept2[i]
                        upper[,i] = upper[,i] + intercept2[i]
                    }
                }
                
                if(rug)
                {
                    if(is.null(ylim))
                        ylim2 = range(ZZZ-intercept2[ind_50],lower-intercept2[ind_50],upper-intercept2[ind_50],na.rm=TRUE)
                    
                    #par(mfrow=c(1,1),mar=c(5,5,5,5))
                    matplot(cov[[k]],Z[[k]],type="n",xlab=xlab[k], ylab=ylab,ylim=ylim2, main=main2[k], ...)
                    rug(cov[[k]])
                    matlines(seq(min(cov[[k]],na.rm=T),max(cov[[k]],na.rm=T),length=100), ZZZ[,pp.plot] - intercept2[ind_50], col=col_vector, lty=1,lwd=2)
                    if(ci)
                    {
                        matlines(seq(min(cov[[k]],na.rm=T),max(cov[[k]],na.rm=T),length=100), lower-intercept2[ind_50], col=col_vector, lty=2,lwd=2)
                        matlines(seq(min(cov[[k]],na.rm=T),max(cov[[k]],na.rm=T),length=100), upper-intercept2[ind_50], col=col_vector, lty=2,lwd=2)
                    }
                    
                } else {
                    if(is.null(ylim))
                        ylim2 = range(yy,Z[[k]],lower,upper,na.rm=TRUE)
                    
                    plot(cov[[k]], yy, cex=0.5, pch=20, col="grey42", xlab=xlab[k], ylab=ylab,ylim=ylim2, main=main2[k], ...)
                    matlines(seq(min(cov[[k]],na.rm=T),max(cov[[k]],na.rm=T),length=100), ZZZ, col=col_vector, lty=1,lwd=2)
                    if(ci)
                    {
                        matlines(seq(min(cov[[k]],na.rm=T),max(cov[[k]],na.rm=T),length=100), lower, col=col_vector, lty=2,lwd=2)
                        matlines(seq(min(cov[[k]],na.rm=T),max(cov[[k]],na.rm=T),length=100), upper, col=col_vector, lty=2,lwd=2)
                    }
                }
                if(legend)
                    legend(x="topright", pch=19, cex=0.8, col=rev(col_vector), legend=rev(pp[pp.plot]), bg="white", bty="n")
            } 
            else if(types[[k]] == "markov") {
                z = NULL
                Zspathelp = helper[[k]][[2]]
                bnd = helper[[k]][[1]]
                
                if(inherits(x,"boost"))
                {
                    for(i in 1:np)
                    {
                        z = cbind(z,coefficients[[k]][,i] + intercept2[i])
                    }
                    
                    if(!inherits(bnd,"bnd"))
                    {
                        if(is.numeric(cov[[k]])) {
                            plot(seq(0,1.1*max(cov[[k]],na.rm=T),length=10), seq(0,max(z[,pp.plot],na.rm=T),length=10), type = "n", xlab = "District", ylab = "coefficients", main=main2[k], ...)
                            points(rep(as.numeric(attr(bnd,"regions")),times=np),z[,pp.plot],col=col_vector)
                        }
                        if(is.na(bnd)) {
                            if(is.numeric(cov[[k]])) {
                                plot(seq(1.1*min(cov[[k]],na.rm=T),1.1*max(cov[[k]],na.rm=T),length=10), seq(min(z[,pp.plot],na.rm=T),max(z[,pp.plot],na.rm=T),length=10), type = "n", xlab = "District", ylab = "coefficients", main=main2[k], ...)
                                matpoints((unique(cov[[k]])), z, col = col_vector, pch = 19, cex = 1)
                            }
                            if(is.factor(cov[[k]])) {
                                plot(seq(0, 1.1 * length(levels(cov[[k]])), length = 10), seq(min(z,na.rm=T), max(z,na.rm=T), length = 10), type = "n", xlab = "District", ylab = "coefficients")
                                matpoints(1:length(levels(cov[[k]])), z, col = col_vector, pch = 19, cex = 1)
                            }
                        }
                        if(legend)
                            legend(x="right", pch=19, cex=1, col=rev(col_vector), legend=rev(pp[pp.plot]), bg="white", bty="n")
                    }
                    else
                    {
                        #par(mfrow=(c(row.grid,col.grid)))
                        plot.limits = range(coefficients[[k]])
                        #n = attr(bnd,"regions")
                        for(i in 1:np.plot) 
                        {
                            re = data.frame(attr(bnd,"regions"),coefficients[[k]][,i])
                            
                            drawmap(re,bnd,regionvar=1,plotvar=2,limits=plot.limits,
                                    main=pp[pp.plot[i]],swapcolors=TRUE,legend=legend, 
                                    mar.min=mar.min,cex.legend=1.5,cols = "grey",
                                    cex.main=cex.main)
                        }
                        
                    }
                }
                else
                {
                    #z = z - mean(z)
                    
                    z = matrix(NA,nrow=nrow(Zspathelp),ncol=np)
                    if(ci)
                    {
                        warning("ci not available at the moment for markov")
                        #                     for(i in 1:np)
                        #                     {
                        #                         z[,i] = (Zspathelp %*% (coefficients[[k]][,i] - qnorm(0.975) * sqrt(diag(x$covmat[[i]][c(partbasis+1),c(partbasis+1)]))) > 0)*1 - 1*(Zspathelp %*% (coefficients[[k]][,i] + qnorm(0.975) * sqrt(diag(x$covmat[[i]][c(partbasis+1),c(partbasis+1)]))) < 0)
                        #                     }
                    }
                    else
                    {
                        regions <- data.frame(x = rownames(x$bases[[k]]$P_orig))
                        colnames(regions) <- x$bases[[k]]$xname_orig
                        #rownames(x$bases[[k]]$P_orig)
                        
                        coefficients_markov <- predict.regbase(x$bases[[k]],regions)
                        z <- coefficients_markov %*% x$coefficients[[k]]
                        
                    }
                    
                    if(!inherits(bnd,"bnd"))
                    {
                        if(!is.na(bnd)){
                            if(is.numeric(cov[[k]])) {
                                plot(seq(0,1.1*max(cov[[k]],na.rm=T),length=10), seq(0,max(z[,pp.plot],na.rm=T),length=10), 
                                     type = "n", xlab = "District", ylab = "coefficients", main=main2[k], ...)
                                points(rep(as.numeric(attr(bnd,"regions")),times=np),z[,pp.plot],col=col_vector)
                            }
                        }
                        if(is.na(bnd)) {
                            if(is.numeric(cov[[k]])) {
                                plot(seq(1.1*min(cov[[k]],na.rm=T),1.1*max(cov[[k]],na.rm=T),length=10), seq(min(z[,pp.plot],na.rm=T),max(z[,pp.plot],na.rm=T),length=10), 
                                     type = "n", xlab = "District", ylab = "coefficients", main=main2[k], ...)
                                matpoints((unique(cov[[k]])), z, col = col_vector, pch = 19, cex = 1)
                            }
                            if(is.factor(cov[[k]])) {
                                plot(seq(0, 1.1 * length(levels(cov[[k]])), length = 10), seq(min(z,na.rm=T), max(z,na.rm=T), length = 10), 
                                     type = "n", xlab = "District", ylab = "coefficients")
                                matpoints(1:length(levels(cov[[k]])), z, col = col_vector, pch = 19, cex = 1)
                            }
                        }
                        if(legend)
                            legend(x="right", pch=19, cex=1, col=rev(col_vector), legend=rev(pp[pp.plot]), bg="white", bty="n")
                    }
                    else
                    {
                        #split.screen(c(3,4))
                        #par(mfrow=(c(row.grid,col.grid)))
                        plot.limits = range(z)
                        if(is.null(ylim_spat)) {
                            plot.limits = range(z,na.rm=T)
                        } else {
                            plot.limits <- ylim_spat
                        }
                        for(i in 1:np.plot) 
                        {
                            re = data.frame(attr(bnd,"regions"),z[,pp.plot[i]])
                            drawmap(re,bnd,regionvar=1, plotvar=2, limits=plot.limits, 
                                    main=pp[pp.plot[i]],swapcolors=TRUE,legend=legend,
                                    cex.legend=1.5,cols = "grey",mar.min=mar.min,cex.main=cex.main)
                            if((min(z,na.rm=T) < plot.limits[1] || max(z,na.rm=T) > plot.limits[2]) && range_warning) {
                                mtext(paste("Range wider than ylim. Range: ",min(z,na.rm=T), " , " , max(z,na.rm=T)))
                            }
                        }
                    }
                }
            }
            else if(types[[k]] == "2dspline")
            {
                if(inherits(x,"boost"))
                {
                    #par(mfrow=(c(row.grid,col.grid)))
                    for(i in 1:np)
                    {
                        if(i %in% pp.plot)
                        {
                            gitter = 20
                            x.min = apply(cov[[k]],2,min,na.rm=TRUE)
                            x.max = apply(cov[[k]],2,max,na.rm=TRUE)
                            z = x$plotpredict(k)[[i]]
                            
                            plot.limits = range(z,na.rm=TRUE)
                            
                            if(is.null(ylim_spat)) {
                                ylim2 = plot.limits
                                
                            } else {
                                ylim2 <- ylim_spat
                            }
                            
                            #  persp(z[[1]],z[[2]],z[[3]],ticktype="detailed",phi=40,theta=35,zlim=ylim2,col = "lightblue",xlab = "X", ylab = "Y", zlab = ylab,main=pp[pp.plot[i]])
                            persp(seq(x.min[1],x.max[1],length=gitter),seq(x.min[2],x.max[2],length=gitter),z,
                                  ticktype="detailed",phi=40,theta=35,zlim=ylim2,col = "lightblue",
                                  xlab = names(cov[[k]])[1], ylab = names(cov[[k]])[2], zlab = ylab,main=pp[pp.plot[i]])
                            if((min(z,na.rm=T) < plot.limits[1] || max(z,na.rm=T) > plot.limits[2]) && range_warning) {
                                mtext(paste("Range wider than ylim. Range: ",min(z,na.rm=T), " , " , max(z,na.rm=T)))
                            }
                        }
                    }
                }
                else
                {
                    gitter = 20
                    x.min = apply(cov[[k]],2,min,na.rm=TRUE)
                    x.max = apply(cov[[k]],2,max,na.rm=TRUE)
                    x.gitter = cbind(rep(seq(x.min[1],x.max[1],length=gitter),times=gitter),rep(seq(x.min[2],x.max[2],length=gitter),each=gitter))
                    
                    ndat = as.data.frame(x.gitter)
                    
                    names(ndat) = rep(xlab[k],2)
                    B.gitter = predict(x$bases[[k]],ndat)
                    
                    #B.gitter = rb(x.gitter,"2dspline")[[1]]
                    
                    #par(mfrow=(c(row.grid,col.grid)))
                    for(i in 1:np)
                    {
                        if(i %in% pp.plot)
                        {
                            z <- B.gitter %*% coefficients[[k]][,i]# + intercept[i]
                            
                            
                            if(ci)
                            {
                                #lower = matrix(NA,nrow=400,ncol=np)
                                #upper = matrix(NA,nrow=400,ncol=np)
                                
                                for(nn in 1:nrow(B.gitter))
                                {
                                    deviation = qnorm(0.975) * sqrt(t(c(1,B.gitter[nn,])) %*% x$covmat[[i]][c(1,partbasis+1),c(1,partbasis+1)] %*% c(1,B.gitter[nn,]))
                                    #lower[nn,i] = ZZZ[nn,i] - deviation
                                    #upper[nn,i] = ZZZ[nn,i] + deviation
                                    z[nn] = 1*((z - deviation) > 0) - 1*((z + deviation) < 0)
                                }
                                z = t(matrix(z,nrow=gitter,ncol=gitter))
                                
                                image(seq(x.min[1],x.max[1],length=gitter),seq(x.min[2],x.max[2],length=gitter),z,zlim=range(z),main=pp[pp.plot[i]])
                            }
                            else
                            {
                                
                                z = t(matrix(z,nrow=gitter,ncol=gitter))
                                plot.limits = range(z,na.rm=TRUE)
                                
                                if(is.null(ylim_spat)) {
                                    ylim2 = plot.limits
                                    
                                } else {
                                    ylim2 <- ylim_spat
                                }
                                
                                persp(seq(x.min[1],x.max[1],length=gitter),seq(x.min[2],x.max[2],length=gitter),z,
                                      ticktype="detailed", phi=40, theta=35, zlim=ylim2, col = "lightblue",
                                      xlab = names(cov[[k]])[1], ylab = names(cov[[k]])[2], zlab = ylab, main=pp[pp.plot[i]])
                                if((min(z,na.rm=T) < plot.limits[1] || max(z,na.rm=T) > plot.limits[2]) && range_warning) {
                                    mtext(paste("Range wider than ylim. Range: ",min(z,na.rm=T), " , " , max(z,na.rm=T)))
                                }
                            }
                        }
                    }
                    
                }      
            }
            else if(types[[k]] == "radial")
            {
                if(inherits(x,"boost"))
                {
                    #par(mfrow=(c(row.grid,col.grid)))
                    for(i in 1:np)
                    {
                        if(i %in% pp.plot)
                        {
                            
                            #z = interp(cov[[k]][,1],cov[[k]][,2],Z[[k]][,i],duplicate="mean")
                            z = x$plotpredict(k)[[i]]
                            plot.limits = range(cbind(yy,z),na.rm=TRUE)
                            
                            if(is.null(ylim_spat)) {
                                ylim2 = plot.limits
                            } else {
                                ylim2 <- ylim_spat
                            }
                            
                            #  persp(z[[1]],z[[2]],z[[3]],ticktype="detailed",phi=40,theta=35,zlim=ylim2,col = "lightblue",xlab = "X", ylab = "Y", zlab = ylab,main=pp[pp.plot[i]])
                            
                            persp(seq(x.min[1],x.max[1],length=gitter),seq(x.min[2],x.max[2],length=gitter),z,
                                  ticktype="detailed",phi=40,theta=35,zlim=ylim2,col = "lightblue",
                                  xlab = names(cov[[k]])[1], ylab = names(cov[[k]])[2], zlab = ylab,main=pp[pp.plot[i]])
                            if((min(z,na.rm=T) < plot.limits[1] || max(z,na.rm=T) > plot.limits[2]) && range_warning) {
                                mtext(paste("Range wider than ylim. Range: ",min(z,na.rm=T), " , " , max(z,na.rm=T)))
                            }
                        }
                    }
                }
                else
                {
                    gitter = 20
                    x.min = apply(cov[[k]],2,min,na.rm=T)
                    x.max = apply(cov[[k]],2,max,na.rm=T)
                    x.gitter = cbind(rep(seq(x.min[1],x.max[1],length=gitter),times=gitter),rep(seq(x.min[2],x.max[2],length=gitter),each=gitter))
                    
                    #                 cov[[k]] = cov[[k]][order(cov[[k]][,1]),]
                    #                 #knots = unique(cov[[k]])
                    #                 #knots = knots[seq(1,nrow(knots),length=min(50,nrow(knots))),]
                    #                 knots = helper[[k]]
                    #                 B.gitter = matrix(NA,nrow=dim(x.gitter)[1],ncol=dim(knots)[1])
                    #                 
                    #                 #for(i in 1:dim(x.gitter)[1])
                    #                 #for(j in 1:dim(knots)[1])
                    #                 #{
                    #                 #  r = sqrt(sum((x.gitter[i,] - knots[j,])^2))
                    #                 #  B.gitter[i,j] = r^2*log(r)
                    #                 #}
                    #                 #for(i in 1:dim(x.gitter)[1])
                    #                 for(j in 1:dim(knots)[1])
                    #                 {
                    #                     r = sqrt(rowSums((x.gitter - matrix(unlist(knots[j,]),nrow=nrow(x.gitter),ncol=ncol(knots),byrow=T))^2))
                    #                     r[r==0] = 1
                    #                     #r = sqrt(sum((x.gitter[i,] - knots[j,])^2)
                    #                     B.gitter[,j] = r^2*log(r)
                    #                 }
                    
                    ndat = as.data.frame(x.gitter)
                    
                    names(ndat) = rep(xlab[k],2)
                    B.gitter = predict(x$bases[[k]],ndat)
                    
                    par(mfrow=(c(row.grid,col.grid)))
                    for(i in 1:np)
                    {
                        if(i %in% pp.plot)
                        {
                            z <- B.gitter %*% coefficients[[k]][,i] + intercept2[i]
                            z = t(matrix(z,nrow=gitter,ncol=gitter))
                            
                            plot.limits = range(cbind(z),na.rm=TRUE)
                            
                            if(is.null(ylim_spat)) {
                                ylim2 = plot.limits
                            } else {
                                ylim2 <- ylim_spat
                            }
                            persp(seq(x.min[1],x.max[1],length=gitter),seq(x.min[2],x.max[2],length=gitter),z,
                                  ticktype="detailed",phi=40,theta=35,zlim=ylim2,col = "lightblue",
                                  xlab = names(cov[[k]])[1], ylab = names(cov[[k]])[2], zlab = ylab,main=pp[pp.plot[i]])
                            if((min(z,na.rm=T) < plot.limits[1] || max(z,na.rm=T) > plot.limits[2]) && range_warning) {
                                mtext(paste("Range wider than ylim. Range: ",min(z,na.rm=T), " , " , max(z,na.rm=T)))
                            }
                        }
                    }
                }
            }
            else if(types[[k]] == "krig")
            {
                gitter = 20
                krig.phi = helper[[k]][[1]]
                x.min = apply(cov[[k]],2,min,na.rm=T)
                x.max = apply(cov[[k]],2,max,na.rm=T)
                x.gitter = cbind(rep(seq(x.min[1],x.max[1],length=gitter),times=gitter),rep(seq(x.min[2],x.max[2],length=gitter),each=gitter))
                
                
                ndat = as.data.frame(x.gitter)
                
                names(ndat) = rep(xlab[k],2)
                B.gitter = predict(x$bases[[k]],ndat)
                
                #par(mfrow=(c(row.grid,col.grid)))
                for(i in 1:np)
                {
                    if(i %in% pp.plot)
                    {
                        z <- B.gitter %*% coefficients[[k]][,i] + intercept2[i]
                        z = t(matrix(z,nrow=gitter,ncol=gitter))
                        plot.limits = range(cbind(z),na.rm=TRUE)
                        
                        if(is.null(ylim_spat)) {
                            ylim2 = plot.limits
                        } else {
                            ylim2 <- ylim_spat
                        }
                        persp(seq(x.min[1],x.max[1],length=gitter),seq(x.min[2],x.max[2],length=gitter),z,
                              ticktype="detailed",phi=40,theta=35,zlim=ylim2,col = "lightblue",
                              xlab = names(cov[[k]])[1], ylab = names(cov[[k]])[2], zlab = ylab,main=pp[pp.plot[i]])
                        if((min(z,na.rm=T) < plot.limits[1] || max(z,na.rm=T) > plot.limits[2]) && range_warning) {
                            mtext(paste("Range wider than ylim. Range: ",min(z,na.rm=T), " , " , max(z,na.rm=T)))
                        }
                    }
                }
            }
            else if(types[[k]] == "random")
            {
                if(inherits(x,"boost"))
                {
                    #plot(seq(0,1.1*max(cov[[k]]),length=10), seq(0,1.1*max(Z[[k]]),length=10), type = "n", xlab = "Group", ylab = "coefficients")
                    #points(rep(sort(unique(cov[[k]])),times=np),Z[[k]],col=col_vector)
                    matplot(cov[[k]],Z[[k]],col=col_vector, xlab = xlab[k], ylab = ylab,pch=15, main=main2[k], ...)
                    legend(x="right", pch=19, cex=1, col=rev(col_vector), legend=rev(pp[pp.plot]), bg="white", bty="n")
                }
                else
                {
                    plot(seq(0,1.1*max(cov[[k]],na.rm=T),length=10), seq(0,max(coefficients[[k]] + intercept2 - intercept2[1],na.rm=T),length=10), type = "n", xlab = xlab[k], ylab = "coefficients", main=main2[k], ...)
                    points(rep(sort(unique(cov[[k]])),times=np.plot),(coefficients[[k]] + intercept2 - intercept2[1])[,pp.plot],col=col_vector)
                    if(legend)
                        legend(x="right", pch=19, cex=1, col=rev(col_vector), legend=rev(pp[pp.plot]), bg="white", bty="n")
                }
                
            }
            else if(types[[k]] == "ridge")
            {
                plot(seq(0,1.1*dim(cov[[k]])[2],length=10), seq(0,max(coefficients[[k]] + intercept2 - intercept2[1],na.rm=T),length=10), type = "n", xlab = xlab[k], ylab = "coefficients", main=main2[k], ...)
                points(rep(1:dim(cov[[k]])[2],times=np.plot),(coefficients[[k]] + intercept2 - intercept2[1])[,pp.plot],col=col_vector)
                if(legend)
                    legend(x="right", pch=19, cex=1, col=rev(col_vector), legend=rev(pp[pp.plot]), bg="white", bty="n")
            }
            else if(types[[k]] == "parametric" && !is.factor(cov[[k]]))
            { 
                if(inherits(x,"boost"))
                {
                    plot(cov[[k]], yy, cex=0.5, pch=20, col="grey42", xlab=names(cov)[k], ylab=attr(yy,"name"),ylim=range(cbind(yy,Z[[k]])), main=main2[k], ...)
                    matlines(sort(cov[[k]]), Z[[k]][order(cov[[k]]),pp.plot], col=col_vector, lty=1)
                    if(legend)
                        legend(x="bottomright", pch=19, cex=0.8, col=rev(col_vector), legend=rev(pp[pp.plot]), bg="white", bty="n")
                }
                else
                {
                    
                    if(is.null(ylim) && rug) { ylim2 = range(yy,Z[[k]]-intercept2[ind_50],na.rm=TRUE)}
                    if(is.null(ylim) && !rug) { ylim2 = range(yy,Z[[k]],na.rm=TRUE)}
                    
                    if(!is.vector(cov[[k]])) {
                        for(i in 1:ncol(cov[[k]]))
                        {
                            if(rug)
                            {
                                matplot(cov[[k]][,i],Z[[k]] - intercept2[ind_50],type="n",xlab=xlab[k], ylab=ylab,ylim=range(Z[[k]]-intercept2[ind_50],na.rm=TRUE), main=main2[k], ...)
                                rug(cov[[k]][,i])
                                matlines(cov[[k]][,i], Z[[k]][,pp.plot] - intercept2[ind_50], col=col_vector, lty=1,lwd=2)
                            }
                            else
                            {
                                plot(cov[[k]][,i], yy, cex=0.5, pch=20, col="grey42", xlab=names(cov)[k], ylab=ylab,ylim=ylim2, main=main2[k], ...)
                                matlines(sort(cov[[k]][,i]), Z[[k]][order(cov[[k]][,i]),pp.plot], col=col_vector, lty=1,lwd=2)
                            }
                            if(legend)
                                legend(x="topright", pch=19, cex=0.8, col=rev(col_vector), legend=rev(pp[pp.plot]), bg="white", bty="n")
                        }
                    }
                    if(is.vector(cov[[k]])) {
                        i <-1
                        if(rug)
                        {
                            ndat = data.frame(seq(min(cov[[k]],na.rm=T),max(cov[[k]],na.rm=T),length=100))
                            names(ndat) = names(cov)[k]
                            Bpred = predict(x$bases[[k]],ndat)
                            ZZZ = Bpred %*% coefficients[[k]]
                            for(i in 1:np)
                            {
                                ZZZ[,i] = ZZZ[,i] + intercept2[i]
                            }
                            if(is.null(ylim) && rug) { ylim2 = range(ZZZ-intercept2[ind_50],na.rm=TRUE)}
                            matplot(ndat,ZZZ - intercept2[ind_50],type="n",xlab=xlab[k], ylab=ylab,ylim=ylim2, main=main2[k], ...)
                            rug(cov[[k]])
                            matlines(ndat, ZZZ[,pp.plot] - intercept2[ind_50], col=col_vector, lty=1,lwd=2)
                        }
                        else
                        {
                            plot(cov[[k]], yy, cex=0.5, pch=20, col="grey42", xlab=names(cov)[k], ylab=ylab,ylim=ylim2, main=main2[k], ...)
                            matlines(sort(cov[[k]]), Z[[k]][order(cov[[k]]),pp.plot], col=col_vector, lty=1,lwd=2)
                        }
                        if(legend)
                            legend(x="topright", pch=19, cex=0.8, col=rev(col_vector), legend=rev(pp[pp.plot]), bg="white", bty="n")
                        
                    }
                }
            }
            else if(types[[k]] == "parametric" && is.factor(cov[[k]]))
            { 
                if(inherits(x,"boost"))
                {
                    cat("Sorry factor plot for Boosting not implemented yet.")
                }
                else
                {
                    ylim3 <- ylim_factor
                    if(is.null(ylim_factor))
                        ylim3 = range(coefficients[[k]],na.rm=TRUE)
                    
                    
                    plot(rep(1,times=np),coefficients[[k]][1,],col=col_vector,xlim=c(0.75,length(levels(cov[[k]]))-(0.75)),pch=19,lwd=2,axes=F,xlab=xlab[k], ylab=ylab,ylim=ylim3, main=main2[k], ...)
                    if(length(levels(cov[[k]])) > 2) {
                        for(k1 in 2:(length(levels(cov[[k]]))-1)) {
                            points(rep(k1,times=np),coefficients[[k]][k1,],col=col_vector,pch=19,lwd=2)
                        }
                    }
                    box()
                    abline(h=0) 
                    axis(side=2)
                    axis(side=1,at=seq(1,length(levels(cov[[k]]))-1),labels=levels(cov[[k]])[-1],cex=1)
                    if(!is.null(ylim_factor) & (min(coefficients[[k]],na.rm=TRUE) < ylim_factor[1] || max(coefficients[[k]],na.rm=TRUE) > ylim_factor[2]) && range_warning) {
                        mtext(paste("Range wider than ylim_factor. Range: ",min(coefficients[[k]],na.rm=TRUE), " , " , max(coefficients[[k]],na.rm=TRUE)))
                    }
                    if(legend)
                        legend(x="topright", pch=19, cex=0.8, col=rev(col_vector), legend=rev(pp[pp.plot]), bg="white", bty="n")
                    
                }
            }
            else if(types[[k]] == "special")
            {    
                
                if(is.null(ylim))
                    ylim2 = range(cbind(yy,Z[[k]]),na.rm=TRUE)
                plot(cov[[k]], yy, cex=0.5, pch=20, col="grey42", xlab=xlab[k], ylab=ylab,ylim=ylim2, main=main2[k], ...)
                matlines(sort(cov[[k]]), Z[[k]][order(cov[[k]]),pp.plot], col=col_vector, lty=1,lwd=2)
                if(legend)
                    legend(x="topright", pch=19, cex=1, col=rev(col_vector), legend=rev(pp[pp.plot]), bg="white", bty="n")
            }
            else if (types[[k]] == "random_interc") {
                hist(coefficients[[k]], xlab = xlab[k], ...)
            }
        }
    }
