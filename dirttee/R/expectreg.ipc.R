#' @title 
#' Expectile regression for right-censored data
#' 
#' @description 
#' This function extends expectile regression with inverse probability of censoring (IPC) weights to right-censored data.
#' 
#' @export
#' @import expectreg
#' @importFrom stats complete.cases hat
#' 
#' @param formula A formula object, with the response on the left of the `~'
#' operator, and the terms on the right.  The response must be a
#' \code{\link[survival]{Surv}} object as returned by the \code{\link[survival]{Surv}} 
#' function. Only right censored data are allowed. Splines can be specified through the function \code{\link[expectreg]{rb}}.
#' @param data Optional data frame containing the variables used in the model, if the data is not explicitly given in the formula.
#' @param smooth The smoothing method that shall be used.
#' There are different smoothing algorithms that should prevent overfitting. The '\code{schall}' algorithm balances 
#' variance of errors and contrasts. Ordinary cross- validation '\code{ocv}' minimizes a score-function using \code{\link[stats]{nlminb}} or with a grid search by '\code{cvgrid}'
#' or the function uses a fixed penalty. The numerical minimizatioin is also possible with AIC or BIC as score. 
#' The L-curve is an experimental grid search by Frasso and Eilers.
#' @param lambda The fixed penalty can be adjusted. Also serves as starting value for the smoothing algorithms.
#' @param expectiles In default setting, the expectiles (0.01,0.02,0.05,0.1,0.2,0.5,0.8,0.9,0.95,0.98,0.99) are 
#' calculated. You may specify your own set of expectiles in a vector. 
#' @param LAWSmaxCores How many cores should maximally be used by parallelization. Currently only implemented for Unix-like OS.
#' @param IPC_weights Denotes the kind of IPC weights to use. \code{IPCRR} weights differ from \code{IPCKM} weights by modifying the weights for the last observation if it is censored.
#' @param KMweights Custom IPC weights can be supplied here. This argument is used by \code{\link{modreg}}.
#' @param ci If TRUE, calculates the covariance matrix
#' @param hat1 If TRUE, the hat matrix for the last asymetry level is calculated. This argument is mainly used by \code{\link{modreg}}.
#' 
#' @returns 
#' A list with the following elements.
#' \item{lambda}{The final smoothing parameters for all expectiles and for all effects in a list.}
#' \item{intercepts}{The intercept for each expectile.}
#' \item{coefficients}{A matrix of all the coefficients, for each base element a row and for each expectile a column.}
#' \item{values}{The fitted values for each observation and all expectiles, separately in a list for each effect in the model, sorted in order of ascending covariate values.}
#' \item{response}{Vector of the response variable.}
#' \item{covariates}{List with the values of the covariates.}
#' \item{formula}{The formula object that was given to the function.}
#' \item{asymmetries}{Vector of fitted expectile asymmetries as given by argument \code{expectiles}.}
#' \item{effects}{List of characters giving the types of covariates.}
#' \item{helper}{List of additional parameters like neighbourhood structure for spatial effects or \eqn{\phi} for kriging.}
#' \item{design}{Complete design matrix.}
#' \item{bases}{Bases components of each covariate.}
#' \item{fitted}{Fitted values.}
#' \item{covmat}{Covariance matrix.}
#' \item{diag.hatma}{Diagonal of the hat matrix. Used for model selection criteria.}
#' \item{data}{Original data.}
#' \item{smooth_orig}{Unchanged original type of smoothing.}
#' \item{KMweights}{Vector with IPC weights used in fitting.}
#' \item{aic}{Area under the AIC, approximated with a Riemannian sum.}
#' \item{hat}{The hat matrix for the last asymmetry level. This is used by \code{\link{modreg}}.}
#' 
#' @details 
#' Fits least asymmetrically weighted squares (LAWS) for each expectile. This function is intended
#' for right-censored data. For uncensored data, \code{\link[expectreg]{expectreg.ls}} should be used instead.
#' This function modifies \code{\link[expectreg]{expectreg.ls}} by adding IPC weights. See Seipp et al. (2021) for details on
#' the IPC weights. P-splines can be used with \code{\link[expectreg]{rb}}. The Schall algorithm is used for choosing the penalty.
#' 
#' @references 
#' Seipp, A, Uslar, V, Weyhe, D, Timmer, A, Otto-Sobotka, F. Weighted expectile regression for right-censored data. Statistics in Medicine. 2021; 40(25): 5501- 5520. https://doi.org/10.1002/sim.9137
#'
#' @examples
#' 
#' data(colcancer)
#' 
#' # linear effect
#' expreg <- expectreg.ipc(Surv(logfollowup, death) ~ sex + age, data = colcancer, 
#'                         expectiles = c(0.05, 0.2, 0.5, 0.8, 0.95))
#' coef(expreg)
#' 
#' \donttest{
#' # with p-splines, smoothing parameter selection with schall algorithm
#' expreg2 <- expectreg.ipc(Surv(logfollowup, death) ~ sex + rb(age), data = colcancer)
#' # smoothing parameter selection with AIC
#' expreg3 <- expectreg.ipc(Surv(logfollowup, death) ~ sex + rb(age), data = colcancer, smooth = "aic")
#' # manually selected smoothing parameter
#' expreg4 <- expectreg.ipc(Surv(logfollowup, death) ~ sex + rb(age), data = colcancer, 
#'                          smooth = "fixed", lambda = 2)
#' 
#' plot(expreg2)
#' plot(expreg3)
#' plot(expreg4)
#' }
#' 
#' 
#' 
expectreg.ipc <-
function(formula, data = NULL, smooth = c("schall", "ocv", "aic", "bic", "cvgrid", "lcurve","fixed"),
                         lambda = 1, expectiles = NA, LAWSmaxCores = 1, IPC_weights = c("IPCRR","IPCKM"), KMweights = NULL, ci = FALSE, hat1 = FALSE) #!
{
    smooth <- match.arg(smooth) #!
    estimate = "laws" #!
    #if(is.null(data)) stop("A data frame has to be provided.")
    
    IPC_weights <- match.arg(IPC_weights)
    
    fdat        <- prepare_formula(formula, data, expect = TRUE)
    yy          <- fdat[[1]]
    delta       <- fdat[[2]]
    data        <- fdat[[3]]
    formula     <- fdat[[4]]
    
    #formula.tools::lhs(formula) <- eval(parse(text = paste0('quote(',attr(yy,"name"),')')))
    
    if(!all(complete.cases(data))) stop("Missing data is not allowed")
    
    if(is.null(KMweights)){
      if(IPC_weights == "IPCRR"){ #
        KMweights <- weightsKM(yy, delta)$weightsIPC_RR
      }else{
        KMweights <- weightsKM(yy, delta)$weightsIPC
      }
    }
    nonzero <- KMweights != 0
    data <- data[nonzero, , drop = FALSE]
    KMweights <- KMweights[nonzero]
    dim(KMweights) <- NULL

    
    #smooth = match.arg(smooth) #!
    #estimate = match.arg(estimate) #!
    
    #if(estimate != "laws" && build_onemodel) stop("build_onemodel only possible for laws")
    
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
    dim(yy) <- NULL
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
        design[[1]] = rb(matrix(1, nrow = m, ncol = 1), "parametric", center = FALSE)
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
    
    coef.vector = laws_ipc(B,DD,yy,pp,lambda,smooth,nb,nbp,nbunp,center,types,LAWSmaxCores=LAWSmaxCores, KMweights = KMweights, hat1 = hat1) #!
    vector.a.ma.schall = coef.vector[[1]]
    lala = coef.vector[[2]]
    diag.hat = coef.vector[[3]]

    
    
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
            W <- W * KMweights #!
            lahmda = rep(lala[,i],times=nb)
            if(center)
                lahmda = c(0,lahmda)
            K = lahmda * t(DD) %*% DD
            helpmat = solve(t(W * B) %*% B + K)
            df <- (1 + sum(diag.hat[, i])) #!
            covariance[[i]] = m / (m - df) * helpmat %*% (t(B * (W^2 * (correct * square.dev)[, 1])) %*% B) %*% helpmat #!
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
        #names(final.lambdas)[k] = design[[k]]$xname[1]
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
                  "diag.hatma" = diag.hat, "data"=data, "smooth_orig"=smooth_orig 
                  ) #!
    
    result$KMweights <- KMweights

    
    result$predict <- function(newdata = NULL, with_intercept = TRUE) {
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
    
    result$aic <- coef.vector$aic_area #!
    result$hat <- coef.vector$hat
    
    class(result) = c("expectreg", "laws") #!
    
    result
}
