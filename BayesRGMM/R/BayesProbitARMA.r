BayesProbitARMA = function(fixed, data, random, Robustness, subset, na.action, arma.order, hyper.params, num.of.iter, Interactive)
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
    #cat("fixed.eff = ", fixed.eff, "\n")

    fixed.eff.intercept.included = !any(grepl("-1", fixed.eff))

    random.eff = all.vars.character(random)$m[[2]]

    #cat("random.eff = ", random.eff, "\n")

    mf[[2L]] = update(fixed, as.formula(paste("~.+", paste(random.eff, collapse="+") )))
    mf[[2L]] = update(mf[[2L]], ~.+id)
    mf  <- eval(mf, parent.frame())

    m.design.mat <- attr(mf, "terms")

    #cat("mfixed.design.mat = \n")
    #print(mfixed.design.mat)

    yy <- model.response(mf, "numeric") #model.response(mf, "numeric")
    xx <- model.matrix(m.design.mat, mf)
    
    fixed.eff = attr(terms.formula(fixed), "term.labels")
    if(fixed.eff.intercept.included)
        fixed.eff = c("(Intercept)", fixed.eff)

    random.eff[random.eff=="1"] = "(Intercept)"

    #cat("random.eff = ", random.eff, "\n")

    x.fixed = xx[, colnames(xx)%in%fixed.eff, drop=FALSE]

    z.random = xx[, colnames(xx)%in%random.eff, drop=FALSE]

    id = xx[, colnames(xx)%in%"id"]

    p = dim(x.fixed)[2]
    q = dim(z.random)[2]
    N = length(table(id))
    T = range(table(id))[2]

    TimePointsAvailable = as.vector(table(id))

    y = matrix(NA, T, N)
    x = array(0, c(T, p, N)) #intercept,
    z = array(0, c(T, length(random.eff), N)) #intercept,

    for(i in 1:N){
        y[1:TimePointsAvailable[i], i] = yy[id==i]
        x[1:TimePointsAvailable[i], , i]  = as.matrix(x.fixed[id==i, ])
        z[1:TimePointsAvailable[i], , i]  = as.matrix(z.random[id==i, ], drop=FALSE)
    }

    AR.order = arma.order[1]
    MA.order = arma.order[2]


    SinglePhiPsi = 1 #0: different phi's and psi's 1: same for every subject. 

    if(length(hyper.params)==0){
        sigma2.beta = 1
        v.gamma = 1
        InvWishart.df = 1
        InvWishart.Lambda = diag(q)
    }
    else{
        sigma2.beta = hyper.params$sigma2.beta
        v.gamma = hyper.params$v.gamma
        InvWishart.df = hyper.params$InvWishart.df
        InvWishart.Lambda = hyper.params$InvWishart.Lambda
    }

    UpdateYstar = TRUE
    UpdateRandomEffect = TRUE
    UpdateBeta = TRUE
    UpdateSigma = TRUE 
    UpdateNu = TRUE
    UpdatePhi = (AR.order>0) 
    UpdatePsi = (MA.order>0)


    y.star.ini = matrix(0, T, N)
    if(UpdateYstar){
        y.star.ini[y%in%1] = rtnorm(sum(y%in%1), lower=0, upper=Inf)
        y.star.ini[y%in%0] = rtnorm(sum(y%in%0), lower=-Inf, upper=0)
    }

    b.ini = NULL
    Sigma = diag(q)
    for(i in 1:N)
        b.ini = cbind(b.ini, t(rmvnorm(1, rep(0, q), Sigma)))

    nu.ini = rgamma(N, 5, 5)


    beta.ini = matrix(rnorm(p), ncol=1) 

    Sigma.ini = as.matrix(rWishart(1,q,diag(q))[,,1])


    phi.ini = matrix(0, AR.order, N)
    psi.ini = matrix(0, MA.order, N)

    if(AR.order==0)
        phi.ini[]=0
    if(MA.order==0)
        psi.ini[]=0

    ARMAorder = c(AR.order, MA.order)


    Data = list(Y = y, X = x, Z=z, TimePointsAvailable = TimePointsAvailable)

    InitialValues = list(y.star = y.star.ini, b = b.ini, nu = nu.ini, beta = beta.ini , Sigma = Sigma.ini, phi = phi.ini, psi = psi.ini)

    HyperPara = list(sigma2.beta = sigma2.beta, v.gamma = v.gamma, InvWishart.df = InvWishart.df, InvWishart.Lambda=InvWishart.Lambda)


    UpdatePara = list(UpdateYstar = UpdateYstar, UpdateRandomEffect = UpdateRandomEffect, UpdateNu = UpdateNu, 
                          UpdateBeta = UpdateBeta, UpdateSigma = UpdateSigma, 
                          UpdatePhi = UpdatePhi, UpdatePsi = UpdatePsi, SinglePhiPsi=SinglePhiPsi)


    
    TuningPara = list(TuningPhi = 0.05, TuningPsi = 0.05) 


    #cat("\nCall:\n", printCall(cl), "\n\n", sep = "")
    #cat("Data Descriptives:\n")
    #cat("Longitudinal Process\t\tEvent Process")
    #cat("\nNumber of Observations: ", sum(TimePointsAvailable), "\tNumber of Covariates: ", p-1)
    #cat("\nNumber of subjects:", N, "\n\n")
    cat("\nData Descriptives:\n")
    cat("Longitudinal Data Information:")
    cat("\nNumber of Observations: ", sum(TimePointsAvailable), "\tNumber of Covariates: ", p-1)
    cat("\nNumber of subjects:", N, "\n\n")



    PosteriorSamplesARMA = ProbitMCMCARMAKB(num.of.iter, Data, Robustness, InitialValues, HyperPara, UpdatePara, TuningPara, ARMAorder, Interactive)

    out <- list(Posterior.Samples = PosteriorSamplesARMA, Fixed.Effects.Names = fixed.eff, Random.Effects.Names = random.eff, 
                Response = y, Fixed.Effects.Mat = x, Random.Effects.Mat = z, call = cl, Num.of.Iter = num.of.iter)
    
    #class(out)
    out 
}









