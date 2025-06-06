predict.regbase <-
function(object,newdata=NULL,...) {
    type        <- object$type
    bnd         <- object$bnd
    Zspathelp   <- object$Zspathelp
    phi         <- object$phi
    center      <- object$center
    x           <- object$x
    P           <- object$P
    P_orig      <- object$P_orig
    B_size      <- object$B_size
    B_mean      <- object$B_mean
    
    param_center <- object$param_center
    
    D_new <- param_center$D_new
    qrc   <- param_center$qrc
    ind1  <- param_center$ind1
    j_center     <- param_center$j
    k_center     <- param_center$k
    predict_ind1 <- T
    
     
    if(!is.null(newdata)){
        if(all(object$xname_orig %in% names(newdata)) & predict_ind1){
            newdata = newdata[,names(newdata) %in% object$xname_orig]
            predict_ind1 <- F
        }
        if(all(object$xname %in% names(newdata)) & predict_ind1){
            newdata = newdata[,names(newdata) %in% object$xname]
            names(newdata) <- object$xname_orig
            predict_ind1 <- F
        }
       
        if(!all(object$xname %in% names(newdata)) && predict_ind1) {
            if(!is.vector(newdata) && !(object$xname == "(Intercept)") && !(type=="2dspline" || type=="radial" || type=="krig")) {
                stop(paste("Names of newdata not consistent with original."))#,object$xname,length(names(newdata)),names(newdata)))
            }
            if(!is.vector(newdata) && !(object$xname == "(Intercept)") && (type=="2dspline" || type=="radial" || type=="krig")) {
                names_temp <- strsplit(object$xname,split="_",fixed=T)
                if(length(names_temp) !=1) {stop("names_temp has wrong length")}
                if(length(names_temp) ==1) {
                    if(length(names_temp[[1]]) != 3 ) {stop("names_temp[[1]] has wrong length")}
                    if(length(names_temp[[1]]) == 3 ) {
                        if(!all(names_temp[[1]][c(2,3)] %in% names(newdata))) {
                            stop("Names of newdata not consistent with original (2dspline, radial, krig).")
                        }
                        if(all(names_temp[[1]][c(2,3)] %in% names(newdata))) {
                            newdata    <- data.frame(newdata[[which(names(newdata)==names_temp[[1]][2])]],newdata[[which(names(newdata)==names_temp[[1]][3])]])
                            names(newdata) <- c(names_temp[[1]][2],names_temp[[1]][3])
                        }
                    }
                }
            }
        }
    }
    if(is.null(newdata)) {
        newdata = x
    }
    
    if(type == "pspline" || type == "penalizedpart_pspline" || type == "tp")
    {
        B.deg = 3
        B.size = B_size
        diff.size = 2
        
        x0 <- min(x,na.rm=TRUE)-0.001
        x1 <- max(x,na.rm=TRUE)+0.001
        dx = (x1 - x0)/(B.size-1)
        
        B = splineDesign(knots=seq(x0-dx*B.deg,x1+dx*B.deg,by=dx),x=newdata,ord=B.deg+1,outer.ok=TRUE)
        
        P <- diag(dim(B)[2])
        P <- diff(P,diff = diff.size)
        P <- t(P) %*% P
    
        if(center)
        {
            XZ   <- t(qr.qty(qrc,t(B))[(j_center+1):k_center,]) ## form XZ
            
            
            B <- XZ%*%D_new 
            
            B <- B[,ncol(B):1,drop=FALSE] # put the unpenalized part in the front
            
            if(type == "penalizedpart_pspline") {
                B <- B[,(1 + length(ind1) - sum(ind1)):ncol(B),drop=FALSE] # remove the unpenalized part
            }
            
        }
    }
    else if(type == "2dspline")
    {
        
        B.deg = 3
        B.size = B_size
        diff.size = 2
        
        x0 <- min(x[,1],na.rm=TRUE)-0.001
        x1 <- max(x[,1],na.rm=TRUE)+0.001
        dx = (x1 - x0)/(B.size-B.deg)
        Bx = splineDesign(knots=seq(x0-dx*B.deg,x1+dx*B.deg,by=dx),x=newdata[,1],ord=B.deg+1,outer.ok=TRUE)
        
        y0 <- min(x[,2],na.rm=TRUE)-0.001
        y1 <- max(x[,2],na.rm=TRUE)+0.001
        dy = (y1 - y0)/(B.size-B.deg)
        By = splineDesign(knots=seq(y0-dy*B.deg,y1+dy*B.deg,by=dy),x=newdata[,2],ord=B.deg+1,outer.ok=TRUE)
        
        B = matrix(NA,nrow=dim(newdata)[1],ncol=dim(Bx)[2]*dim(By)[2])
        
        for(i in 1:dim(Bx)[1])
            B[i,] = as.vector(Bx[i,] %o% By[i,])
        
        D = diff(diag(dim(Bx)[2]),diff = diff.size)
        P = t(D) %*% D
        P = diag(dim(Bx)[2]) %x% P + P %x% diag(dim(Bx)[2])
        
        if(center)
        {
            XZ   <- t(qr.qty(qrc,t(B))[(j_center+1):k_center,]) ## form XZ
            
            
            B <- XZ%*%D_new 
            
            B <- B[,ncol(B):1] # put the unpenalized part in the front
            
            
        }
    }
    else if(type == "radial")
    {
        x = x[order(x[,1]),]
        knots = Zspathelp
        B = matrix(NA,nrow=dim(newdata)[1],ncol=dim(knots)[1])
        
        
        for(j in 1:dim(knots)[1])
        {
            r = sqrt(rowSums((newdata - matrix(unlist(knots[j,]),nrow=nrow(newdata),ncol=ncol(knots),byrow=T))^2))
            r[r==0] = 1
            B[,j] = r^2*log(r)
        }
    }
    else if(type == "krig")
    {
        c = 9.233
        
        x = x[order(x[,1]),]
        knots = Zspathelp
        B = matrix(NA,nrow=dim(newdata)[1],ncol=dim(knots)[1])
        
        P = matrix(0,nrow=dim(B)[2],ncol=dim(B)[2])
        for(i in 1:dim(B)[2]){
            for(j in 1:dim(B)[2])
            {
                P[i,j] = sqrt(sum((knots[i,] - knots[j,])^2))
            }
        }
        phi = max(P)/c
        
        for(j in 1:dim(knots)[1])
        {
            r = sqrt(rowSums((newdata - matrix(unlist(knots[j,]),nrow=nrow(newdata),ncol=ncol(knots),byrow=T))^2))
            B[,j] = exp(-r/phi)*(1+r/phi)
        }
        
    }
    else if(type == "markov")
    {
        
        if(any(!is.na(bnd)) && any(is.na(P_orig)))
            P2 = bnd2gra(bnd)
        
        if(!is.null(P_orig) & !any(is.na(P_orig)))
            P2 = P_orig
        
        if(all(is.na(P2)))
            stop("No Neighbourhood defined.")
        
        if(any(diag(P2) < 0) || any(P2-diag(P2) > 0) || is.null(dimnames(P2)[[1]]))
            stop("Maldefined Neighbourhood.")
        
        districts = dimnames(P2)[[1]]
        B = matrix(0,nrow=length(newdata),ncol=dim(P2)[2])
        for(i in 1:length(newdata))
            B[i,which(districts == newdata[i])] = 1
        
        if(center)
        {
            XZ   <- t(qr.qty(qrc,t(B))[(j_center+1):k_center,]) ## form XZ
            B <- XZ%*%D_new 
            B <- B[,ncol(B):1] # put the unpenalized part in the front
            
        }
    }
    else if(type == "random")
    {
        districts = sort(unique(x))
        B = matrix(0,nrow=length(newdata),ncol=length(districts))
        for(i in 1:length(newdata))
            B[i,which(districts == newdata[i])] = 1
        
        P = diag(nrow=dim(B)[2])
        if(center)
        {
            XZ   <- t(qr.qty(qrc,t(B))[(j_center+1):k_center,]) ## form XZ
            B <- XZ%*%D_new 
            B <- B[,ncol(B):1] # put the unpenalized part in the front
            
        }
    }
    else if(type == "ridge")
    {
        B = matrix(NA,nrow=dim(newdata)[1],ncol=dim(x)[2])
        for(i in 1:dim(x)[2])
            B[,i] = x[,i]
        P = diag(nrow=dim(B)[2])
    }
    else if(type == "special")
    {
        if(is.na(B) || is.na(P))
            stop("In 'special' case: Base and Penalty matrix have to be specified.")
        
        if(is.vector(newdata))
            l = length(newdata)
        else if(is.matrix(newdata) || is.data.frame(newdata))
            l = nrow(newdata)
        
        B = as.matrix(object$B)[1:l,,drop=FALSE]
        P = as.matrix(P)
    }
    else if(type == "parametric")
    {
        
        x = data.frame(1,x)
        newdata = data.frame(1,newdata)
        B = model.matrix(formula(newdata),newdata)[,-1,drop=FALSE]
        if(center){
            #if(nrow(B) > 1) {
                for(i in 1:ncol(B)){
                    B[,i] <- B[,i,drop=FALSE] - B_mean[i]
                }
           # }
        }
        
        #B = apply(B,2,function(b){b-sum(b)/length(b)})
        P = matrix(0,nrow=ncol(B),ncol=ncol(B))
    }
    if(object$xname == "(Intercept)") {
        B[,1] <- rep(1,times=nrow(B))
        B <- B[,1]
    }
    #if (any(!is.na(object$by))) 
    #      B = B * object$by
    B
}
