expectreg.ls <-
function(formula, data = NULL,estimate = c("laws", "restricted", "bundle", "sheets"),
                         smooth = c("schall", "ocv", "gcv", "cvgrid", "aic", "bic", "lcurve", "fixed"),
                         lambda = 1, expectiles = NA, ci = FALSE, LAWSmaxCores = 1, ...)
{  
    dot_in <- list(...)
    delta_garrote <- NULL
    if(!is.null(dot_in) && length(dot_in) > 0 && "delta_garrote" %in% names(dot_in)) {
        delta_garrote <- dot_in$delta_garrote
    }
    build_onemodel <- FALSE
    if(!is.null(dot_in) && length(dot_in) > 0 && "list_models" %in% names(dot_in)  && length(dot_in$list_models) > 0) {
        list_models <- dot_in$list_models
        build_onemodel <- TRUE
    }
    
    smooth = match.arg(smooth)
    estimate = match.arg(estimate)
    
    if(estimate != "laws" && build_onemodel) stop("build_onemodel only possible for laws")
    
    smooth_orig <- smooth

    if(!is.na(charmatch(expectiles[1], "density")) && charmatch(expectiles[1], "density") > 0) {
        pp <- seq(0.01, 0.99, by = 0.01)
    }  else if(any(is.na(expectiles)) || !is.vector(expectiles) || any(expectiles > 1) || any(expectiles < 0)) {
        pp <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 0.8, 0.9, 0.95, 0.98, 0.99)
    }  else {
        pp <- expectiles
    }
    np <- length(pp)
    
    yy = eval(as.expression(formula[[2]]), envir = data, enclos = environment(formula))
    attr(yy, "name") = deparse(formula[[2]])
    
    m = length(yy)
    
    design = list()
    x = list()
    types = list()
    bnd = list()
    Zspathelp = list()
    nb = vector()
    nbp = vector()
    nbunp = vector()
    krig.phi = list()
    center = TRUE
    varying = list()
    Blist = list()
    Plist = list()
    terms_formula <- labels(terms(formula))
    if (formula[[3]] == "1") {
        design[[1]] = rb(matrix(1, nrow = m, ncol = 1), "parametric", center = F)
        smooth = "fixed"
        design[[1]]$xname <- "(Intercept)"
    } else if(formula[[3]] == ".") {
        design[[1]] = rb(data[,names(data) != all.vars(formula[[2]])],"parametric")
        smooth = "fixed"
        
    } else {
        for(i in 1:length(terms_formula)) {
            types[[i]] = strsplit(terms_formula,"(",fixed=TRUE)[[i]][1]
            temp_formula <- terms_formula[i]
            if(types[[i]] == terms_formula[i]) {
                temp_formula <- paste("rb(",terms_formula[i],", type = 'parametric')")
             }
            design[[i]] = eval(parse(text=temp_formula),envir=data,enclos=environment(formula))
        }
    }
    nterms = length(design)
    
    varying[[1]] = design[[1]][[9]]
    if(any(!is.na(varying[[1]]))) {
        B = design[[1]][[1]] * varying[[1]]
        Blist[[1]] = design[[1]][[1]] * varying[[1]]
    } else {
        B = design[[1]][[1]]
        Blist[[1]] = design[[1]][[1]]
    }
    
    DD = as.matrix(design[[1]][[2]])
    Plist[[1]] = DD
    x[[1]] = design[[1]][[3]]
    names(x)[1] = design[[1]]$xname_orig[1]
    types[[1]] = design[[1]][[4]]
    bnd[[1]] = design[[1]][[5]]
    Zspathelp[[1]] = design[[1]][[6]]
    nb[1] = ncol(design[[1]][[1]])
    nbp[1] <- design[[1]]$nbp
    nbunp[1] <- design[[1]]$nbunp
    krig.phi[[1]] = design[[1]][[7]]
    center = center && design[[1]][[8]]
    constmat = as.matrix(design[[1]]$constraint)
    ########### Begin: testing for not well defined combinations of parameters #####################
    
    vec_s_xname       <- rep(NA,times=length(design))
    vec_s_xname_orig  <- rep(NA,times=length(design))
    vec_s_type        <- rep(NA,times=length(design))
    vec_s_B_size      <- rep(NA,times=length(design))
    
    
    for(i in 1:length(design)) {
        zzzzz                <- design[[i]]
        if(!"regbase" %in% class(zzzzz)) {
            if(grepl(x=class(zzzzz), pattern="smooth.spec")) {
                stop("In expectreg smooth term are defined via rb() and not s() as in mgcv!")
            } else {
                stop("Wrong definition of covariates!")
            }
        }
        
        vec_s_xname      [i] <- zzzzz$xname[1]
        vec_s_xname_orig [i] <- zzzzz$xname_orig
        vec_s_type       [i] <- zzzzz$type
        vec_s_B_size     [i] <- zzzzz$B_size

        #if(vec_s_Alternative[i] == 2 && vec_s_split[i]) {
        #    stop("Model selection with Alternative == 2 and split == T is not well defined, \n 
        #         due to the fact that with Alternative 2 the decomposition is not linear + penalized term") 
        #    }
    }
    

    unique_xname <- unique(vec_s_xname_orig)
    
    for(u in unique_xname) {
        indices <- which(vec_s_xname_orig == u)
        if(length(vec_s_type[indices])        != 1 & length(unique(vec_s_type[indices])) == 1) {
            stop("Duplicated covariate!")
        }
        if(length(unique(vec_s_B_size[indices]))      != 1) {stop("Do not mix different B_size types for one covariate!")} 
        if(length(vec_s_type[indices])        != 1) {
            if(!(length(unique(vec_s_type[indices])) == 2 & identical(sort(vec_s_type[indices]), sort(c("parametric","penalizedpart_pspline"))))) {
                stop("Do not mix different types for one covariate!")
            }
        }
        
    }
    
    ########### End: testing for not well defined combinations of parameters #####################
    
    
    
    if(length(design) > 1)
        for(i in 2:length(terms_formula))
        {
            varying[[i]] = design[[i]][[9]]
            if(any(!is.na(varying[[i]]))) {
                B = cbind(B, design[[i]][[1]] * varying[[i]])
                Blist[[i]] = design[[i]][[1]] * varying[[i]]
            } else {
                B = cbind(B, design[[i]][[1]])
                Blist[[i]] = design[[i]][[1]]
            }
            
            design[[i]][[2]] = as.matrix(design[[i]][[2]])
            Plist[[i]] = design[[i]][[2]]
            DD = rbind(cbind(DD,matrix(0,nrow=nrow(DD),ncol=ncol(design[[i]][[2]]))),
                       cbind(matrix(0,nrow=nrow(design[[i]][[2]]),ncol=ncol(DD)),design[[i]][[2]]))
            constmat = rbind(cbind(constmat,matrix(0,nrow=nrow(constmat),ncol=ncol(design[[i]]$constraint))),
                             cbind(matrix(0,nrow=nrow(design[[i]]$constraint),ncol=ncol(constmat)),design[[i]]$constraint))
            x[[i]] = design[[i]][[3]]
            names(x)[i] = design[[i]]$xname_orig[1]
            types[[i]] = design[[i]][[4]]
            bnd[[i]] = design[[i]][[5]]
            Zspathelp[[i]] = design[[i]][[6]]
            nb[i] = ncol(design[[i]][[1]])
            nbp[i] <- design[[i]]$nbp
            nbunp[i] <- design[[i]]$nbunp
            krig.phi[[i]] = design[[i]][[7]]
            center = center && design[[i]][[8]]
                    
        }
    for(i in 1:length(design)) {
        if(nb[i] != (nbunp[i]+nbp[i])) stop("Major error of implementation: \n\r Number of effects does not fit to sum of number of (un)penalized effects")
    }
    
    if(center) {
        B = cbind(1,B)
        DD = rbind(0,cbind(0,DD))
        constmat = rbind(0,cbind(0,constmat))
    }
    
    if(!build_onemodel) {
    if(estimate == "laws"){
        if(all(constmat == 0)) {
            coef.vector = laws(B,DD,yy,pp,lambda,smooth,nb,nbp,nbunp,center,types,LAWSmaxCores=LAWSmaxCores)
        } else {
            coef.vector = laws_constmat(B,DD,yy,pp,lambda,smooth,nb,center,constmat,types,LAWSmaxCores=LAWSmaxCores)
        }
    } else {
		if(estimate == "restricted") {
            coef.vector = restricted(B, DD, yy, pp, lambda, smooth, nb, center, constmat, types)
            trend.coef = coef.vector[[4]]
            residual.coef = coef.vector[[5]]
            asymmetry = coef.vector[[6]]
		} else {
    		if(estimate == "bundle")  {
                coef.vector = bundle(B,DD,yy,pp,lambda,smooth,nb,center,constmat,types)
                trend.coef = coef.vector[[4]]
                residual.coef = coef.vector[[5]]
                asymmetry = coef.vector[[6]]
            } else {
                if(estimate == "sheets") {
                coef.vector = sheets(Blist,Plist,yy,pp,lambda,smooth,nb,center,types)
                }
            }
        }
    }

    vector.a.ma.schall = coef.vector[[1]]
    lala = coef.vector[[2]]
    diag.hat = coef.vector[[3]]
    }
    
    ## Build one model out of list of selected models
    if(build_onemodel) {
        ci <- FALSE
        vector.a.ma.schall <- matrix(0,nrow=ncol(B),ncol=np)
         
        lala <- matrix(1,nrow=length(design),ncol=np)
         
        diag.hat <- matrix(0,nrow=nrow(design[[1]]$B),ncol=length(pp))
        
        
        Inter_ind <- 0
        if(center){
            Inter_ind <- 1
            for(tt in 1:np) {
                vector.a.ma.schall[1,tt] <- list_models[[tt]]$intercept
                }
            }
        if(length(nb) != length(design)) {stop("length(np) != length(design)")}
        for(k in 1:length(nb)) {
            partbasis <- (sum(nb[0:(k-1)])+1):(sum(nb[0:k])) + Inter_ind
            if(1 %in% partbasis){ stop("EEE") }
            
            names_partbasis <- design[[k]]$xname[1]
            for(tt in 1:np) {
                if(length(list_models) == np) {
                    if(!is.null(list_models[[tt]]$coefficients[[names_partbasis]])){
                        vector.a.ma.schall[partbasis,tt] <- list_models[[tt]]$coefficients[[names_partbasis]][,1]
                        lala[k,tt] <- list_models[[tt]]$lambda[[names_partbasis]]
                    }
                } else {stop("wrong list_models")}
            }
        }
        
        for(tt in 1:np) {
            diag.hat[,tt] <- list_models[[tt]]$diag.hatma
        }
    }
    
    
    
    ######################
    # manipulate vector.a.ma.schall with given delta_garrote
    
    if(!is.null(delta_garrote)) {
        if(!(inherits(delta_garrote,c("list","numeric")))) {stop("class of delta_garrote does not fit")}
        if((inherits(delta_garrote,"list"))) {
            if(!(length(delta_garrote) %in% c(1,np))) {stop("length of list delta_garrote does not fit")}
            if(length(delta_garrote) == np) {
                for(i in 1:np) {
                    if(length(delta_garrote[[i]]) != length(nb)){ stop("length of delta_garrote does not fit to number of covariates")}
                }
            }
            if(length(delta_garrote) == 1) {            
                if(length(delta_garrote[[1]]) != length(nb)){ stop("length of list delta_garrote does not fit to number of covariates")}
            }
        }
        if(inherits(delta_garrote,"numeric")) {
            if(length(delta_garrote) != length(nb)) {stop("length of vector delta_garrote does not fit to number of covariates")}
        }
        
        Inter_ind <- 0
        if(center) {
            Inter_ind <- 1
            }
        if(length(nb) != length(design)) {stop("length(np) != length(design)")}
        for(k in 1:length(nb)) {
            partbasis = (sum(nb[0:(k-1)])+1):(sum(nb[0:k]))+Inter_ind
            names_partbasis <- design[[k]]$xname[1]
            for(i in 1:np) {
                if(inherits(delta_garrote,"list")) {
                    if(length(delta_garrote) == np) {
                        vector.a.ma.schall[partbasis,i] <- vector.a.ma.schall[partbasis,i] * delta_garrote[[i]][which(gsub(pattern=" (fixed)",replacement="",x=names(delta_garrote[[i]]),fixed=T) == names_partbasis)]
                    }
                    if(length(delta_garrote) == 1) {
                        vector.a.ma.schall[partbasis,i] <- vector.a.ma.schall[partbasis,i] * delta_garrote[[1]][which(gsub(pattern=" (fixed)",replacement="",x=names(delta_garrote[[1]]),fixed=T) == names_partbasis)]
                    }
                }
                if(inherits(delta_garrote,"numeric")) {
                    vector.a.ma.schall[partbasis,i] <- vector.a.ma.schall[partbasis,i] * delta_garrote[which(gsub(pattern=" (fixed)",replacement="",x=names(delta_garrote),fixed=T) == names_partbasis)]
                }
            }
        }
        
    }
    
    
    ######################
    
    
    covariance = NULL
    ##############################
    if(ci) {
        W = list()
        covariance = list()
        
        for(i in 1:np) {
            
            W = as.vector(ifelse(yy > B %*% vector.a.ma.schall[,i], pp[i], 1 - pp[i]))
            square.dev = (yy - B %*% vector.a.ma.schall[,i])^2
            correct = 1/(1-diag.hat[,i])
            
            if(any(is.na(W))) {
                correct[!is.na(W)] = correct[1:(length(correct)-length(which(is.na(W))))]
                correct[is.na(W)] = 1
                W[which(is.na(W))] = 1
                square.dev[which(is.na(square.dev))] = 0
            }
            
            lahmda = rep(lala[,i],times=nb)
            if(center)
                lahmda = c(0,lahmda)
            K = lahmda * t(DD) %*% DD
            helpmat = solve(t(W * B) %*% B + K)
            
            
            covariance[[i]] = helpmat %*% (t(B * (W^2 * (correct^2* square.dev)[,1])) %*% B) %*% helpmat
            
        }
        
    }
    ##############################
    
    
    
    
    Z <- list()
    coefficients <- list()
    final.lambdas <- list()
    helper <- list()
    
    fitted = B %*% vector.a.ma.schall
    
    if(center)
    {
        intercept = vector.a.ma.schall[1,]
        B = B[,-1,drop=FALSE]
        vector.a.ma.schall = vector.a.ma.schall[-1,,drop=FALSE]
    } else
        intercept = rep(0,np)
    
    for(k in 1:length(design)) {
        final.lambdas[[k]] = lala[k, ]
        names(final.lambdas)[k] = design[[k]]$xname
        
        partbasis = (sum(nb[0:(k-1)])+1):(sum(nb[0:k]))
        
        if(types[[k]] == "pspline" || types[[k]] == "penalizedpart_pspline" || types[[k]] == "tp")
        {
            Z[[k]] <- matrix(NA, m, np)
            coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
            helper[[k]] = NA
            for(i in 1:np)
            {
                Z[[k]][,i] <- design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
                coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
            }
        } else if(types[[k]] == "markov")
        {
            Z[[k]] <- matrix(NA, m, np)
            coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
            helper[[k]] = list(bnd[[k]],Zspathelp[[k]])
            for(i in 1:np)
            {
                Z[[k]][,i] = design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
                coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
            }
        } else if(types[[k]] == "2dspline")
        {
            Z[[k]] <- matrix(NA, m, np)
            coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
            helper[[k]] = NA
            for(i in 1:np)
            {
                Z[[k]][,i] = design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
                coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
            }
            
        } else if(types[[k]] == "radial")
        {
            Z[[k]] <- matrix(NA, m, np)
            coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
            helper[[k]] = Zspathelp[[k]]
            for(i in 1:np)
            {
                Z[[k]][,i] = design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
                coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
            }
        } else if(types[[k]] == "krig")
        {
            Z[[k]] <- matrix(NA, m, np)
            coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
            helper[[k]] = list(krig.phi[[k]],Zspathelp[[k]])
            for(i in 1:np)
            {
                Z[[k]][,i] = design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
                coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
            }
        } else if(types[[k]] == "random")
        {
            Z[[k]] <- matrix(NA, m, np)
            coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
            helper[[k]] = NA
            for(i in 1:np)
            {
                Z[[k]][,i] <- design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
                coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
            }
            
        } else if(types[[k]] == "ridge")
        {
            Z[[k]] <- matrix(NA, m, np)
            coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
            helper[[k]] = NA
            for(i in 1:np)
            {
                Z[[k]][,i] <- design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
                coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
            }
        } else if(types[[k]] == "parametric")
        {    
            Z[[k]] <- matrix(NA, m, np)
            coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
            helper[[k]] = NA
            for(i in 1:np)
            {
                Z[[k]][,i] <- design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
                coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
            }
        } else if(types[[k]] == "special")
        {    
            Z[[k]] <- matrix(NA, m, np)
            coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
            helper[[k]] = NA
            for(i in 1:np)
            {
                Z[[k]][,i] <- design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
                coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
            }
        }
        names(Z)[k]            = design[[k]]$xname[1]
        names(coefficients)[k] = design[[k]]$xname[1]
        names(final.lambdas)[k] = design[[k]]$xname[1]
        names(design)[k] = design[[k]]$xname[1]
        
    }
    
    desmat = B
    if(center)
        desmat = cbind(1,B)
    
    result = list("lambda"=final.lambdas, "intercepts"=intercept, 
                  "coefficients"=coefficients, "values"=Z, "response"=yy, 
                  "covariates"=x, "formula"=formula, "asymmetries"=pp, 
                  "effects"=types, "helper"=helper, "design"=desmat, 
                  "bases"=design, "fitted"=fitted, "covmat"=covariance, 
                  "diag.hatma" = diag.hat, "data"=data, "smooth_orig"=smooth_orig, 
                  "delta_garrote" = delta_garrote)
    

    if (estimate == "restricted" || estimate == "bundle") {
        result$trend.coef = trend.coef
        result$residual.coef = residual.coef
        result$asymmetry.coef = asymmetry
    }
    
    result$predict <- function(newdata = NULL, with_intercept = T) {
        BB = list()
        values = list()
        bmat = NULL
        for(k in 1:length(coefficients))
        {
            BB[[k]] = predict(design[[k]],newdata=newdata)
            values[[k]]  <- BB[[k]] %*% coefficients[[k]]
            if(with_intercept) {
                values[[k]] = t(apply(values[[k]],1,function(x) { x + intercept } ))
            }
            bmat = cbind(bmat,BB[[k]])
        }
        if (center) {
            bmat = cbind(1, bmat)
            vector.a.ma.schall = rbind(intercept, vector.a.ma.schall)
        }
        fitted = bmat %*% vector.a.ma.schall
        names(values) = names(coefficients)
        
        list("fitted"=fitted,"values"=values)
    }
    
    if (formula[[3]] == "1")
    {
        result$intercepts <- as.vector(coefficients[[1]])
        result$coefficients <- NULL
        result$covariates <- NULL 
    }  
    
    
    
    class(result) = c("expectreg",estimate,smooth)
    
    result
}
