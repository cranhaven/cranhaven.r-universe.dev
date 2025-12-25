#' Perform MCMC algorithm to generate the posterior samples for longitudinal ordinal data
#'
#' This function is used to generate the posterior samples using MCMC algorithm from the  
#' cumulative probit model with the hypersphere decomposition applied to model the correlation structure 
#' in the serial dependence of repeated responses.
#'
#' @param fixed a two-sided linear formula object to describe fixed-effects with the response on the left of 
#' a \samp{~} operator and the terms separated by \samp{+} or \samp{*} operators, on the right. 
#' The specification \code{first*second} indicates the cross of \code{first} and \code{second}. 
#' This is the same as \code{first + second + first:second}.
#' @param data  an optional data frame containing the variables named in \samp{fixed} and \samp{random}. 
#' It requires an ``integer'' variable named by \samp{id} to denote the identifications of subjects. 
#' @param random a one-sided linear formula object to describe random-effects with the terms separated by 
#' \samp{+} or \samp{*} operators on the right of a \samp{~} operator.
#' @param Robustness logical. If 'TRUE' the distribution of random effects is assumed to be \cr 
#' t-distribution; otherwise normal distribution. 
#' @param na.action a function that indicates what should happen when the data contain NA’s. 
#' The default action (\samp{na.omit}, inherited from the \samp{factory fresh} value of \cr
#' \samp{getOption("na.action")}) strips any observations with any missing values in any variables.
#' @param subset an optional expression indicating the subset of the rows of \samp{data} that should be used in the fit. 
#' This can be a logical vector, or a numeric vector indicating which observation numbers are to be included, 
#' or a character vector of the row names to be included.  All observations are included by default.
#' @param HS.model a specification of the correlation structure in HSD model: 
#' \itemize{
#'   \item \code{HS.model = ~0} denotes independence, that is, \eqn{R_i} is an identity matrix, 
#'   \item \code{HS.model = ~IndTime+}\eqn{\cdots}\code{+IndTimer} denotes AR(r) correlation structure, 
#'   \item \code{HS.model = ~DiffTime1+}\eqn{\cdots}\code{+DiffTimer} denotes correlation structure related to \eqn{r}th order 
#' of time difference. 
#' }
#' @param hyper.params specify the values in hyperparameters in priors. 
#' @param num.of.iter an integer to specify the total number of iterations; default is 20000.      
#' @param Interactive logical. If 'TRUE' when the program is being run interactively for progress bar and 'FALSE' otherwise.
#' @return a list of posterior samples, parameters estimates, AIC, BIC, CIC, DIC, MPL, RJR, predicted values, 
#' and the acceptance rates in MH are returned.
#'
#' @note Only a model either HSD (\samp{HS.model}) or ARMA (\samp{arma.order}) model should be specified in the function. 
#' We'll provide the reference for details of the model and the algorithm for performing 
#' model estimation whenever the manuscript is accepted. 
#'
#' @author Kuo-Jung Lee <kuojunglee@ncku.edu.tw> 
#' @references{
#'   \insertRef{Lee:etal:2021}{BayesRGMM} 
#'   
#'   \insertRef{Lee:etal:2020}{BayesRGMM}
#'
#'} 
#'
#' @examples
#' \dontrun{
#' library(BayesRGMM)
#' rm(list=ls(all=TRUE))
#' 
#' Fixed.Effs = c(-0.1, 0.1, -0.1) #c(-0.8, -0.3, 1.8, -0.4) 
#' P = length(Fixed.Effs) 
#' q = 1 #number of random effects
#' T = 7 #time points
#' N = 100 #number of subjects
#' Num.of.Cats = 3 #in KBLEE simulation studies, please fix it. 
#' num.of.iter = 1000 #number of iterations
#' 
#' HSD.para = c(-0.9, -0.6) #the parameters in HSD model
#' a = length(HSD.para)
#' w = array(runif(T*T*a), c(T, T, a)) #design matrix in HSD model
#'  
#' for(time.diff in 1:a)
#' w[, , time.diff] = 1*(as.matrix(dist(1:T, 1:T, method="manhattan")) ==time.diff)
#' 
#' x = array(0, c(T, P, N))
#' for(i in 1:N){
#'     #x[,, i] = t(rmvnorm(P, rep(0, T), AR1.cor(T, Cor.in.DesignMat)))
#'     x[, 1, i] = 1:T
#'     x[, 2, i] = rbinom(1, 1, 0.5)
#'     x[, 3, i] = x[, 1, i]*x[, 2, i]
#' }
#' 
#' DesignMat = x
#' 
#' #Generate a data with HSD model
#' 
#' 
#' #MAR
#' CPREM.sim.data = SimulatedDataGenerator.CumulativeProbit(
#'  Num.of.Obs = N, Num.of.TimePoints = T, Num.of.Cats = Num.of.Cats, 
#'  Fixed.Effs = Fixed.Effs, Random.Effs = list(Sigma = 0.5*diag(1), df=3), 
#'  DesignMat = DesignMat, Missing = list(Missing.Mechanism = 2, 
#'  MissingRegCoefs=c(-0.7, -0.2, -0.1)), 
#'  HSD.DesignMat.para = list(HSD.para = HSD.para, DesignMat = w))
#' 
#' print(table(CPREM.sim.data$sim.data$y))
#' print(CPREM.sim.data$classes)
#' 
#' BCP.output = BayesCumulativeProbitHSD(
#'     fixed = as.formula(paste("y~", paste0("x", 1:P, collapse="+"))), 
#'     data=CPREM.sim.data$sim.data, random = ~ 1, Robustness = TRUE, 
#'     subset = NULL, na.action='na.exclude', HS.model = ~IndTime1+IndTime2, 
#'     hyper.params=NULL, num.of.iter=num.of.iter, Interactive=0)
#' 
#' BCP.Est.output = BayesRobustProbitSummary(BCP.output)
#' }



BayesCumulativeProbitHSD = function(fixed, data, random, Robustness, subset, na.action, HS.model, hyper.params, num.of.iter, Interactive)
{

# process data: reponse, fixed and random effects matrices. 

    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)  

    m <- match(c("fixed", "data", "subset", "na.action"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(model.frame)
    names(mf)[2] = "formula"


    fixed.eff = all.vars.character(fixed[-2])$m[[2]]

    #fixed.eff.intercept.included = !any(grepl("-1", fixed.eff))
    
    random.eff = all.vars.character(random)$m[[2]]

    HS.model.cov = all.vars.character(HS.model)$m[[2]]
    #cat("HS.model.cov = ", HS.model.cov, "\n")

    TimeOrder = sort(gsub("IndTime", "", HS.model.cov[HS.model.cov %in% paste0("IndTime", 1:10)]))
    #cat("TimeOrder = ", TimeOrder, "\n")

    DiffTime = sort(gsub("DiffTime", "", HS.model.cov[HS.model.cov %in% paste0("DiffTime", 1:10)]))
    #cat("DiffTime = ", DiffTime, "\n")
    
    #cat("HS.model = \n")
    #print(as.formula(HS.model))

    interaction.terms = attr(terms.formula(as.formula(HS.model)), "term.labels")

    #cat("HS.model = ", interaction.terms, "\n")
    mf2 = eval(mf, parent.frame())
    Terms = attr(mf2, "terms")
    fixed.eff = colnames(model.matrix(Terms, mf2))


    fixed.eff = fixed.eff[-1]


    mf[[2L]] = update(fixed, as.formula(paste("~.+", paste(random.eff, collapse="+") )))
    mf[[2L]] = update(mf[[2L]], ~.+id)
    mf  <- eval(mf, parent.frame())

    m.design.mat <- attr(mf, "terms")

    #cat("mfixed.design.mat = \n")
    #print(mfixed.design.mat)

    yy <- model.response(mf, "numeric") #model.response(mf, "numeric")
    xx <- model.matrix(m.design.mat, mf)


    #fixed.eff = attr(terms.formula(fixed), "term.labels")
    #if(fixed.eff.intercept.included)
    #    fixed.eff = c("(Intercept)", fixed.eff)

    #cat("fixed.eff = ", fixed.eff, "\n")

    random.eff[random.eff=="1"] = "(Intercept)"

    #cat("random.eff = ", random.eff, "\n")

    x.fixed = xx[, colnames(xx)%in%fixed.eff, drop=FALSE]


    z.random = xx[, colnames(xx)%in%random.eff, drop=FALSE]

    id = xx[, colnames(xx)%in%"id"]



    p = dim(x.fixed)[2]
    q = dim(z.random)[2]
    N = length(table(id))
    T = range(table(id))[2]



    a = length(interaction.terms) 

    #cat("a = ", a, "\n")
    u = NULL

    delta.num = 0
    # for HSD model 

    TimeOrder = unique(TimeOrder)
    DiffTime = unique(DiffTime)

 
    if(a>0){
        u = array(0, c(T, T, N, a))
        if(length(TimeOrder)>0){
            for(t in 1:length(TimeOrder))
                u[,,1:N, t][as.matrix(dist(1:T, method="euclidean", diag = TRUE, upper = TRUE))==t] = 1
            delta.num = delta.num + length(TimeOrder)
        }
        #cat("length(DiffTime) = ", length(DiffTime)>0, "\n")
        if(length(DiffTime)>0){
            for(t in 1:length(DiffTime))
                u[,,1:N, delta.num+t] = (as.matrix(dist(1:T, method="euclidean", diag = TRUE, upper = TRUE)))^t
            delta.num = delta.num + length(DiffTime)
        }

        main.terms = interaction.terms[-grep("Time",interaction.terms)]
        int.terms = interaction.terms[grep(":",interaction.terms)]

        #cat("main = ", main.terms, "\n")
        #cat("int.terms = ", int.terms, "\n")
        
        
        if(length(main.terms)>0){
            #cat("=============== 1 ============\n")
            for(t in 1:length(main.terms)){
                uu = data[, names(data)%in%c("id", main.terms[t]), drop=FALSE]
                HSD.cov = unique(uu)[, , drop=FALSE]
                HSD.cov = HSD.cov[HSD.cov$id%in%id, ]
                HSD.cov = as.matrix(HSD.cov[, -1])
                for(sub in 1:N)
                    u[,,sub, delta.num+t] = matrix(HSD.cov[sub], T, T)
            }
            delta.num = delta.num + length(main.terms)
        }
        if(length(int.terms)>0){
            #cat("=============== 2 ============\n")
            for(t in 1:length(int.terms)){
                #cat("int.terms[t]= ", int.terms[t], "\n")
                int.terms.tmp = strsplit(int.terms[t], ":")[[1]]
                #cat("int.terms.tmp = ", int.terms.tmp, "\n")
                int.terms.tmp.IndTime.TF = int.terms.tmp %in% paste0("IndTime", 1:10)
                int.terms.tmp.DiffTime.TF = int.terms.tmp %in% paste0("DiffTime", 1:10)
                int.terms.tmp.IndTime = int.terms.tmp[int.terms.tmp %in% paste0("IndTime", 1:10)]
                int.terms.tmp.DiffTime = int.terms.tmp[int.terms.tmp %in% paste0("DiffTime", 1:10)]
                #cat("any(int.terms.tmp.IndTime) = ", length(int.terms.tmp.IndTime), "\n")
                #cat("any(int.terms.tmp.DiffTime) = ", length(int.terms.tmp.DiffTime), "\n")

                if(length(int.terms.tmp.IndTime)>0){
                    #cat("=============== 3 ============\n")
                    #cat("int.terms.tmp.IndTime = ", int.terms.tmp.IndTime, "\n")
                    IndTime.tmp = as.numeric(gsub("\\D", "", int.terms.tmp.IndTime))
                    #as.numeric(int.terms.tmp.IndTime) #as.numeric(gsub("IndTime", "", int.terms.tmp.IndTime))
                    #cat("IndTime.tmp = ", IndTime.tmp, "\n")
                    HSD.cov.tmp = int.terms.tmp[!int.terms.tmp.IndTime.TF]
                    #cat("HSD.cov.tmp = ", HSD.cov.tmp, "\n")
                    
                    uu.tmp = matrix(0, T, T)
                    uu.tmp[as.matrix(dist(1:T, method="euclidean", diag = TRUE, upper = TRUE))==IndTime.tmp] = 1

                    uu = data[, names(data)%in%c("id", HSD.cov.tmp), drop=FALSE]

                    uu = uu[complete.cases(uu), ]
                    #cat("uu = \n")
                    #print(uu)

                    HSD.cov = unique(uu)[, , drop=FALSE]

                    #cat("HSD.cov 1= \n")
                    #print(HSD.cov)

                    HSD.cov = HSD.cov[HSD.cov$id%in%id, ]
                    HSD.cov = as.matrix(HSD.cov[, -1])

                    #cat("HSD.cov 2= \n")
                    #print(HSD.cov)

                    for(sub in 1:N){
                        u[,, sub, delta.num+1] = uu.tmp*matrix(HSD.cov[sub], T, T)
                    }

                    delta.num = delta.num + 1

                }
                else if(length(int.terms.tmp.DiffTime)>0){
                    #cat("=============== 4 ============\n")
                    DiffTime.tmp = as.numeric(gsub("DiffTime", "", int.terms.tmp.DiffTime))
                    HSD.cov.tmp = int.terms.tmp[!int.terms.tmp.DiffTime.TF]
                    uu.tmp = matrix(0, T, T)
                    uu.tmp[as.matrix(dist(1:T, method="euclidean", diag = TRUE, upper = TRUE))==DiffTime.tmp] = 1

                    uu = data[, names(data)%in%c("id", HSD.cov.tmp), drop=FALSE]
                    HSD.cov = unique(uu)[, , drop=FALSE]
                    HSD.cov = HSD.cov[HSD.cov$id%in%id, ]
                    HSD.cov = as.matrix(HSD.cov[, -1])

                    for(sub in 1:N)
                        u[,, sub, delta.num + 1] = uu.tmp*matrix(HSD.cov[sub], T, T)

                    delta.num = delta.num + 1
                }
                else{
                    #cat("=============== 5 ============\n")
                    #cat("int.terms.tmp = ", int.terms.tmp, "\n")
                    uu = data[, names(data)%in%c("id", int.terms.tmp), drop=FALSE]
                    HSD.cov1 = unique(uu)[, 1, drop=FALSE]
                    #print(head(HSD.cov1))
                    HSD.cov2 = unique(uu)[, 2, drop=FALSE]
                    #print(head(HSD.cov2))
                    HSD.cov = cbind(HSD.cov1, HSD.cov2)
                    HSD.cov = HSD.cov[HSD.cov$id%in%id, ]
                    #print(head(HSD.cov))
                    HSD.cov = as.matrix(HSD.cov[, -1])
                    for(sub in 1:N)
                        u[,, sub ,delta.num + 1] = matrix(prod(HSD.cov[sub, ]), T, T)


                }


            }

        }

    }

    #print(dim(u))

    #cat("delta.num = ", delta.num, "\n")
    #cat("=============== 6 ============\n")
    if(a != delta.num)
        stop("Something wrong to assing the design matrix in HSD model.\n")
    if(any(HS.model.cov==1)){
        a = a+1
        u = abind(array(1, c(T, T, N)), u, along=4)
    }

    uu = u

    #cat("dim(u) = ", dim(u), "\n")
    #cat("u = \n")

    if(a>0)
        dim(u) = c(T, T, N*a)



    TimePointsAvailable = as.vector(table(id))

    y = matrix(NA, T, N)
    x = array(0, c(T, p, N)) #intercept,
    z = array(0, c(T, length(random.eff), N)) #intercept,

    id.index = unique(id)

    for(i in 1:N){
        y[1:TimePointsAvailable[i], i] = yy[id==id.index[i]]
        x[1:TimePointsAvailable[i], , i]  = as.matrix(x.fixed[id==id.index[i], ])
        z[1:TimePointsAvailable[i], , i]  = as.matrix(z.random[id==id.index[i], ], drop=FALSE)
    }

    #Defult values for hyperparameters
        #sigma2.alpha = 0.1
        #sigma2.beta = 1
        #sigma2.delta = 1
        #v.gamma = 5
        #InvWishart.df = 5
        #InvWishart.Lambda = diag(q)

        sigma2.alpha = ifelse(is.null(hyper.params$sigma2.alpha), 0.1, hyper.params$sigma2.alpha)
        sigma2.beta = ifelse(is.null(hyper.params$sigma2.beta), 1, hyper.params$sigma2.beta)
        sigma2.delta = ifelse(is.null(hyper.params$sigma2.delta), 1, hyper.params$sigma2.delta)
        v.gamma = ifelse(is.null(hyper.params$v.gamma), 5, hyper.params$v.gamma)
        InvWishart.df = ifelse(is.null(hyper.params$InvWishart.df), 5, hyper.params$InvWishart.df)
        InvWishart.Lambda = if(is.null(hyper.params$InvWishart.Lambda)) diag(q) else hyper.params$InvWishart.Lambda


    UpdateYstar = TRUE
    UpdateAlpha = TRUE
    UpdateRandomEffect = TRUE
    UpdateBeta = TRUE
    UpdateSigma = TRUE 
    UpdateNu = TRUE
    UpdateDelta = ifelse(is.null(u), FALSE, TRUE)


    Num.of.Cats = length(unique(na.omit(c(y))))


    y.star.ini = matrix(0, T, N)


    alpha.ini = c(-Inf, seq(-5, 5, length = Num.of.Cats-1), Inf)

    #print(alpha.ini)

    y = y-min(y, na.rm=TRUE)+1 # to make the categorical variable begin with 1

    #y[is.na(y)] = 1000 # missing values specified by 1000
    y[!is.finite(y)] = 1000
    #y[is.nan(y)] = 1000

    #print(head(is.finite(y)))
    #print(head(y))

    for(i in 1:Num.of.Cats)
        y.star.ini[y%in%i] = rtnorm(sum(y%in%i), lower=alpha.ini[i], upper=alpha.ini[i+1])

    #print(head(y.star.ini))

    b.ini = NULL
    Sigma = diag(q)
    for(i in 1:N)
        b.ini = cbind(b.ini, t(rmvnorm(1, rep(0, q), Sigma)))

    

    nu.ini = rgamma(N, 5, 5)

    beta.ini = matrix(rep(0, p), ncol=1) 

    Sigma.ini = as.matrix(rWishart(1,q,diag(q))[,,1])

    delta.ini = rep(0, a)#runif(a, -1, 1)

    Data = list(Y = y, X = x, Z=z, U = u, TimePointsAvailable = TimePointsAvailable)


    InitialValues = list(y.star = y.star.ini, alpha = alpha.ini, b = b.ini, nu = nu.ini, beta = beta.ini , Sigma = Sigma.ini, delta = delta.ini)


    HyperPara = list(sigma2.beta = sigma2.beta, sigma2.delta=sigma2.delta, sigma2.alpha=sigma2.alpha, v.gamma = v.gamma, InvWishart.df = InvWishart.df, InvWishart.Lambda=InvWishart.Lambda)


    UpdatePara = list(UpdateYstar = UpdateYstar, UpdateAlpha = UpdateAlpha, UpdateRandomEffect = UpdateRandomEffect, UpdateNu = UpdateNu, 
                      UpdateBeta = UpdateBeta, UpdateSigma = UpdateSigma, UpdateDelta = UpdateDelta)

    TuningPara = list(TuningDelta = 0.01)


 if(1){  
    start.time <- Sys.time()

    PosteriorSamplesCP = CumulativeProbitMCMC(num.of.iter, Data, Robustness, InitialValues, HyperPara, UpdatePara, TuningPara, Interactive)

    end.time <- Sys.time()




    #cat("\nCall:\n", printCall(cl), "\n\n", sep = "")
    cat("\nData Descriptives:\n")
    cat("Longitudinal Data Information:")
    cat("\nNumber of Observations: ", sum(TimePointsAvailable), "\tNumber of Covariates: ", p-1)
    cat("\nNumber of subjects:", N, "\n\n")


    out <- list(Posterior.Samples = PosteriorSamplesCP, Fixed.Effects.Names = fixed.eff, 
                Random.Effects.Names = random.eff, 
                Response = y, Fixed.Effects.Mat = x, Random.Effects.Mat = z, 
                HS.model.Mat = uu, call = cl, Num.of.Iter = num.of.iter)
    
    #class(out)
    out 
}

}



