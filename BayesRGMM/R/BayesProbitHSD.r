BayesProbitHSD = function(fixed, data, random, Robustness, subset, na.action, HS.model, hyper.params, num.of.iter, Interactive)
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

    fixed.eff.intercept.included = !any(grepl("-1", fixed.eff))
    
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



    for(i in 1:N){
        y[1:TimePointsAvailable[i], i] = yy[id==i]
        x[1:TimePointsAvailable[i], , i]  = as.matrix(x.fixed[id==i, ])
        z[1:TimePointsAvailable[i], , i]  = as.matrix(z.random[id==i, ], drop=FALSE)
    }

    if(length(hyper.params)==0){
        sigma2.beta = 1
        sigma2.delta = rep(1, length(a))
        v.gamma = 5
        InvWishart.df = 5
        InvWishart.Lambda = diag(q)
    }
    else{
        sigma2.beta = hyper.params$sigma2.beta
        sigma2.delta = hyper.params$sigma2.delta
        v.gamma = hyper.params$v.gamma
        InvWishart.df = hyper.params$InvWishart.df
        InvWishart.Lambda = hyper.params$InvWishart.Lambda       
    }

    UpdateYstar = TRUE
    UpdateRandomEffect = TRUE
    UpdateBeta = TRUE
    UpdateSigma = TRUE 
    UpdateNu = TRUE
    UpdateDelta = ifelse(is.null(u), FALSE, TRUE)


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

    beta.ini = matrix(rep(0, p), ncol=1) 

    Sigma.ini = as.matrix(rWishart(1,q,diag(q))[,,1])

    delta.ini = rep(0, a)#runif(a, -1, 1)

    Data = list(Y = y, X = x, Z=z, U = u, TimePointsAvailable = TimePointsAvailable)


    InitialValues = list(y.star = y.star.ini, b = b.ini, nu = nu.ini, beta = beta.ini , Sigma = Sigma.ini, delta = delta.ini)


    HyperPara = list(sigma2.beta = sigma2.beta, sigma2.delta=sigma2.delta, v.gamma = v.gamma, InvWishart.df = InvWishart.df, InvWishart.Lambda=InvWishart.Lambda)


    UpdatePara = list(UpdateYstar = UpdateYstar, UpdateRandomEffect = UpdateRandomEffect, UpdateNu = UpdateNu, 
                      UpdateBeta = UpdateBeta, UpdateSigma = UpdateSigma, UpdateDelta = UpdateDelta)

    TuningPara = list(TuningDelta = 0.01)

 if(1){  
    start.time <- Sys.time()

    PosteriorSamplesHSD = ProbitMCMCHSD(num.of.iter, Data, Robustness, InitialValues, HyperPara, UpdatePara, TuningPara, Interactive)

    end.time <- Sys.time()




    #cat("\nCall:\n", printCall(cl), "\n\n", sep = "")
    cat("\nData Descriptives:\n")
    cat("Longitudinal Data Information:")
    cat("\nNumber of Observations: ", sum(TimePointsAvailable), "\tNumber of Covariates: ", p-1)
    cat("\nNumber of subjects:", N, "\n\n")


    out <- list(Posterior.Samples = PosteriorSamplesHSD, Fixed.Effects.Names = fixed.eff, 
                Random.Effects.Names = random.eff, 
                Response = y, Fixed.Effects.Mat = x, Random.Effects.Mat = z, 
                HS.model.Mat = uu, call = cl, Num.of.Iter = num.of.iter)
    
    #class(out)
    out 
}

}


