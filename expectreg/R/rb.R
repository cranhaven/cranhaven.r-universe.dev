rb <-
function(x,type=c("pspline","2dspline","markov","krig","random","ridge",
                        "special","parametric","penalizedpart_pspline"), 
               B_size = 20, B=NA,P=NA,bnd=NA,center=TRUE,by=NA, ...)
{
    #"radial", is not possible at the moment
    type = match.arg(type)
    Zspathelp = NA
    phi = NA
    cll = match.call()
    
    P_orig <- P # for Prediction of markov random fields
    
    xname = deparse(as.list(match.call())$x)
    xname_orig <- xname
 
    B_mean <- NULL
    param_center <- NULL
    
    if(type %in% c("pspline","penalizedpart_pspline"))
    {
        B.deg = 3
        B.size = B_size
        diff.size = 2
        
        x0 <- min(x,na.rm=TRUE)-0.001
        x1 <- max(x,na.rm=TRUE)+0.001
        dx = (x1 - x0)/(B.size-1)
        B = matrix(0,nrow=length(x),ncol=B.size+B.deg-1)
        notnas = which(!is.na(x))
        B[notnas,] = splineDesign(knots=seq(x0-dx*B.deg,x1+dx*B.deg,by=dx),x=x[notnas],ord=B.deg+1)
        
        P <- diag(dim(B)[2])
        P <- diff(P,diff = diff.size)
        P <- t(P) %*% P
        nbunp <- 0
        nbp <- ncol(P)
        
        if(center)
        {
            centered_rb <- rb_centering(B=B, P=P, min.eigenvalue = 1e-10)
            B <- centered_rb$B
            P <- centered_rb$P
            param_center <- centered_rb$param_center
            ind1 <- param_center$ind1
            B_mean <- param_center$B_mean
            nbunp <- param_center$nbunp
            nbp <- param_center$nbp
            if(type == "penalizedpart_pspline") {
                xname <- paste(xname,"_prb",sep="")
                B <- B[,(1 + length(ind1) - sum(ind1)):ncol(B)] # remove the unpenalized part
                P <- diag(rep(1,ncol(B)))
                nbunp <- 0
            }
            if(type == "pspline") {
                xname <- paste(xname,"_rb",sep="")
                P <- diag(c(rep(0,length(ind1) - sum(ind1)), rep(1,ncol(B)-(length(ind1) - sum(ind1)))))
            }
            
        }
    }    
    else if(type == "2dspline")
    {
        
        if(inherits(x,"matrix") || inherits(x,"data.frame")) {
            xname = paste("2dspline",paste(sort(colnames(x)),collapse="_"),sep="_")
        }
        
        B.deg = 3
        B.size = B_size
        diff.size = 2
        
        x0 <- min(x[,1],na.rm=TRUE)-0.001
        x1 <- max(x[,1],na.rm=TRUE)+0.001
        dx = (x1 - x0)/(B.size-B.deg)
        Bx = matrix(0,nrow=nrow(x),ncol=B.size)
        notnas = which(!is.na(x[,1]))
        Bx[notnas,] = splineDesign(knots=seq(x0-dx*B.deg,x1+dx*B.deg,by=dx),x=x[notnas,1],ord=B.deg+1)
        #Bx = splineDesign(knots=seq(x0-dx*B.deg,x1+dx*B.deg,by=dx),x=x[,1],ord=B.deg+1)
        
        y0 <- min(x[,2],na.rm=TRUE)-0.001
        y1 <- max(x[,2],na.rm=TRUE)+0.001
        dy = (y1 - y0)/(B.size-B.deg)
        By = matrix(0,nrow=nrow(x),ncol=B.size)
        notnas = which(!is.na(x[,2]))
        By[notnas,] = splineDesign(knots=seq(y0-dy*B.deg,y1+dy*B.deg,by=dy),x=x[notnas,2],ord=B.deg+1)
        
        B = matrix(NA,nrow=dim(x)[1],ncol=dim(Bx)[2]*dim(By)[2])
        
        for(i in 1:dim(Bx)[1])
            B[i,] = as.vector(Bx[i,] %o% By[i,])
        
        D = diff(diag(dim(Bx)[2]),diff = diff.size)
        P = t(D) %*% D
        P = diag(dim(Bx)[2]) %x% P + P %x% diag(dim(Bx)[2])
        nbunp <- 0
        nbp <- ncol(P)
        
        if(!center){
            ek = eigen(P)
            
            ek$values = ek$values[1:(length(ek$values)-2*diff.size)]
            ek$vectors = ek$vectors[,1:(dim(ek$vectors)[2]-2*diff.size)]
            P = t(ek$vectors %*% sqrt(diag(ek$values)))
        }
        if(center)
        {
            centered_rb <- rb_centering(B=B, P=P, min.eigenvalue = 1e-10)
            B <- centered_rb$B
            P <- centered_rb$P
            
            param_center <- centered_rb$param_center
            nbunp <- param_center$nbunp
            nbp <- param_center$nbp
            
            B_mean <- param_center$B_mean
            }
    }
    else if(type == "radial")
    {
        if(inherits(x,"matrix") || inherits(x,"data.frame")) {
            xname = paste("radial",paste(sort(colnames(x)),collapse="_"),sep="_")
        }
        
        x = x[order(x[,1]),]
        
        #require(fields)
        
        knots = unique(x)
        knots <- cover.design(R = knots, nd = min(50,nrow(knots)))$design
        Zspathelp = knots
        B = matrix(NA,nrow=dim(x)[1],ncol=dim(knots)[1])
        
        for(j in 1:dim(knots)[1])
        {
            r = sqrt(rowSums((x - matrix(unlist(knots[j,]),nrow=nrow(x),ncol=ncol(knots),byrow=T))^2))
            r[r==0] = 1
            B[,j] = r^2*log(r)
        }
        
        P = matrix(0,nrow=dim(B)[2],ncol=dim(B)[2])
        for(j in 1:dim(B)[2])
        {
            r = sqrt(rowSums((matrix(unlist(knots),nrow=nrow(knots),ncol=ncol(knots)) - 
                                  matrix(unlist(knots[j,]),nrow=nrow(knots),ncol=ncol(knots),byrow=T))^2))
            r[r==0] = 1
            P[,j] = r^2*log(r)
        }
        nbunp <- 0
        nbp <- ncol(P)
#         if(center)
#         {     
#             centered_rb <- rb_centering(B=B, P=P, min.eigenvalue = 1e-10)
#             B <- centered_rb$B
#             P <- centered_rb$P
#             param_center <- centered_rb$param_center
#             B_mean <- param_center$B_mean
#             nbunp <- param_center$nbunp
#             nbp <- param_center$nbp
#         
#         } else {
            e <- eigen(P)
            P <- t(e$vectors[,-dim(e$vectors)[2]]*sqrt(abs(e$values[-length(e$values)])))
        #}
        
        
    }
    else if(type == "krig")
    {
        if(inherits(x,"matrix") || inherits(x,"data.frame")) {
            xname = paste("krig",paste(sort(colnames(x)),collapse="_"),sep="_")
        }
        
        
        cons = 9.233
        
        x = x[order(x[,1]),]
        
        #require(fields)
        
        knots = unique(x)
        knots <- cover.design(R = knots, nd = min(50,nrow(knots)))$design
        Zspathelp = knots
        
        #knots = knots[seq(1,nrow(knots),length=min(50,nrow(knots))),]
        #knots = x[seq(1,dim(x)[1],length=min(50,dim(x)[1])),]
        B = matrix(NA,nrow=dim(x)[1],ncol=dim(knots)[1])
        
        P = matrix(0,nrow=dim(B)[2],ncol=dim(B)[2])
        #for(i in 1:dim(B)[2])
        for(j in 1:dim(B)[2])
        {
            #P[i,j] = sqrt(sum((knots[i,] - knots[j,])^2))
            P[,j] = sqrt(rowSums((matrix(unlist(knots),nrow=nrow(knots),ncol=ncol(knots)) - 
                                      matrix(unlist(knots[j,]),nrow=nrow(knots),ncol=ncol(knots),byrow=T))^2))
        }
        phi = max(P)/cons
        P = P/phi
        P = exp(-P)*(1+P)
        nbunp <- 0
        nbp <- ncol(P)
        
        #for(i in 1:dim(x)[1])
        for(j in 1:dim(knots)[1])
        {
            #r = sqrt(sum((x[i,] - knots[j,])^2))
            r = sqrt(rowSums((x - matrix(unlist(knots[j,]),nrow=nrow(x),ncol=ncol(knots),byrow=T))^2))
            B[,j] = exp(-r/phi)*(1+r/phi)
        }
#         if(center)
#         {     
#             centered_rb <- rb_centering(B=B, P=P, min.eigenvalue = 1e-10)
#             B <- centered_rb$B
#             P <- centered_rb$P
#             param_center <- centered_rb$param_center
#             B_mean <- param_center$B_mean
#             nbunp <- param_center$nbunp
#             nbp <- param_center$nbp
#         
#         } else {
            e <- eigen(P)
            P <- t(e$vectors[,-dim(e$vectors)[2]]*sqrt(abs(e$values[-length(e$values)])))
        #}
        
        #P = matrix(0,nrow=dim(B)[2],ncol=dim(B)[2])
        #for(i in 1:dim(B)[2])
        #  for(j in 1:dim(B)[2])
        #  {
        #    r = sqrt(sum((knots[i,] - knots[j,])^2))
        #    P[i,j] = exp(-r)*(1+r)
        #  }
    }
    else if(type == "markov")
    {
        
        if(any(!is.na(bnd)) && any(is.na(P)))
            P = bnd2gra(bnd)
        
        if(any(is.na(P_orig)))
            P_orig <- P
            
        if(all(is.na(P)))
            stop("No Neighbourhood defined.")
        
        if(any(diag(P) < 0) || any(P-diag(P) > 0) || is.null(dimnames(P)[[1]]))
            stop("Maldefined Neighbourhood.")
        
        districts = dimnames(P)[[1]]
        B = matrix(0,nrow=length(x),ncol=dim(P)[2])
        for(i in 1:length(x))
            B[i,which(districts == x[i])] = 1
        
        Zspathelp = diag(ncol(P))
    
        e <- eigen(P)
        nbunp <- 0
        nbp <- ncol(P)
        
        if(center)
        {     
            centered_rb <- rb_centering(B=B, P=P, min.eigenvalue = 1e-10)
            B <- centered_rb$B
            P <- centered_rb$P
            param_center <- centered_rb$param_center
            B_mean <- param_center$B_mean
            nbunp <- param_center$nbunp
            nbp <- param_center$nbp
        
        }
        else
            P <- t(e$vectors[,-dim(e$vectors)[2]]*sqrt(abs(e$values[-length(e$values)])))
        
    }
    else if(type == "random")
    {
        districts = sort(unique(x))
        B = matrix(0,nrow=length(x),ncol=length(districts))
        for(i in 1:length(x))
            B[i,which(districts == x[i])] = 1
        
        P = diag(nrow=dim(B)[2])
        nbunp <- 0
        nbp <- ncol(P)
        if(center)
        {     
            centered_rb <- rb_centering(B=B, P=P, min.eigenvalue = 1e-10)
            B <- centered_rb$B
            P <- centered_rb$P
            param_center <- centered_rb$param_center
            B_mean <- param_center$B_mean
            nbunp <- param_center$nbunp
            nbp <- param_center$nbp
        
        }
        
        
    }
    else if(type == "ridge")
    {
        B = matrix(NA,nrow=dim(x)[1],ncol=dim(x)[2])
        for(i in 1:dim(x)[2])
            B[,i] = x[,i]
        P = diag(nrow=dim(B)[2])
        nbunp <- 0
        nbp <- ncol(P)
        
        
        
    }
    else if(type == "special")
    {
        if(is.na(B) || is.na(P))
            stop("In 'special' case: Base and Penalty matrix have to be specified.")
        
        B = as.matrix(B)
        P = as.matrix(P)
        nbunp <- 0
        nbp <- ncol(P)
        
        
        
    }
    else if(type == "parametric")
    {
        # if(is.vector(x))
        # {
        #   B = matrix(x,ncol=1)
        # }
        # else if(is.matrix(x))
        # {
        #   B = x
        # }
        # else if(is.data.frame(x) || is.matrix(x))
        # {
        if(class(x)[1] == "matrix")
            xname = colnames(x)
        
        xdata = data.frame(1,x)
        names(xdata) = c("X1",xname)
        B = model.matrix(formula(xdata),xdata)[,-1,drop=FALSE]
        #colnames(B) = gsub("`","",colnames(B),fixed=TRUE)
        #x = x[,-1,drop=FALSE]
        #xname = names(x)
        #      B = as.matrix(x)
        # }
        B_mean <- colMeans(B)
        if(center){
            if(nrow(B) > 1) {
                for(i in 1:ncol(B)){
                    B[,i] <- B[,i] - B_mean[i]
                }
            }
        }
        
        #  B = apply(B,2,function(x){x-sum(x)/length(x)})
        P = matrix(0,nrow=ncol(B),ncol=ncol(B))
        nbunp <- ncol(P)
        nbp <- 0
        
    }
    
    
    constraint = matrix(0,nrow=2,ncol=ncol(P))
    
    rb = list("B"=B,"P"=P,"x"=x,"type"=type,"bnd"=bnd,"Zspathelp"=Zspathelp,"phi"=phi,
              "center"=center,"by"=by,"xname"=xname,"constraint"=constraint,
              "B_size"=B_size,"P_orig"=P_orig,"B_mean"=B_mean,"xname_orig"=xname_orig,
              "param_center"=param_center,"nbp"=nbp,"nbunp"=nbunp,"call" = cll) 
    class(rb) = c("regbase")
    
    rb
}
