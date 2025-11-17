#' @import glmnet crayon selectiveInference
#' @importFrom stats optim coef var predict
mxtune.fit <- function(X, Y, Z, U, c, epsilon, max_s, margin_s, alpha.est.init, maxstep, margin,
                      maxstep_inner, margin_inner, compute.likelihood, verbosity, standardize = standardize,
                      intercept = intercept) {

        n = nrow(X)
        p=ncol(X)
        k = length(unique(Y))

        ##------------ Multiclass elastic-net regression
        ## Initialize
        alpha.est.old = alpha.est.init
        likelihood.score = c()
        s = 1

        ## calculate alpha.max
        a.max = NULL
        for (i in 1:k) {
          y.temp = ifelse(Y==unique(Y)[i],1,0)
          a.max = cbind(a.max, max( abs(t(y.temp - mean(y.temp)*(1-mean(y.temp))) %*% X ) )/ (c * n))
        }

        alpha.max = min(max(abs(a.max))/abs(ifelse(Z==0,0.01,Z)))

        ## start estimation
        cat(yellow$italic$bold("Start estimating alpha:\n"))
        while (s < max_s){ # iteratively compute alpha and beta mode
          # Given alpha, update beta mode, Y hat etc
          lambda = exp(Z%*%alpha.est.old)
          gamma = 2/(lambda^2*c^2 + 2*lambda*(1-c))
          A = (lambda^2*c^2 + 2*lambda*(1-c))/2

          # use the glmnet to update beta.mode
          mode.est=coef(glmnet(X,Y,alpha=0, family = 'multinomial',penalty.factor = A,lambda = sum(A)/p/n, standardize = F, intercept = FALSE))

          cat.prob = sapply(mode.est, function(g) exp(X%*%g[-1]))
          t.mode = cat.prob/rowSums(cat.prob) # softmax function

          B_inv = apply(t.mode, 2, function(g) as.vector(1/(g*(1-g))))

          Y_HAT = NULL
          for (i in 1:k) {
            y = ifelse(Y==names(mode.est)[i],1,0)
            cat.y = X%*%mode.est[[i]][-1] + B_inv[,i]*(y - t.mode[,i])
            Y_HAT = cbind(Y_HAT,cat.y)
          }


          if(compute.likelihood == TRUE){
            V_inv = diag(as.vector(gamma))
            likelihood.score = c(approx_likelihood.mxtune(B_inv,X,Y_HAT,V_inv,k,n),likelihood.score)
          }

          # Given beta mode, Y hat etc, update alpha
          alpha.est.new <- estimate.alpha.mxtune(X,Y_HAT,Z,c = c,B_inv,alpha.init = alpha.est.old, alpha.max,
                                          epsilon = epsilon,
                                          maxstep = maxstep,
                                          margin = margin,
                                          maxstep_inner = maxstep_inner,
                                          margin_inner = margin_inner,
                                          compute.likelihood = F,verbosity = verbosity)

          # Check convergence
          if(sum(abs(alpha.est.new - alpha.est.old)) < margin_s ){
            cat(red$bold("Done!\n"))
            break
          }
          alpha.est.old <- alpha.est.new

          # Track iteration progress
          if (verbosity == TRUE) {
                  cat(green$italic("#---"),green$bold("Outer loop Iteration",s,"Done"),green$italic("---#\n"),sep = "")
          }

          s <- s+1
        }


        tauEst = exp(Z%*%alpha.est.old)
        pen_vec= tauEst/n
        if(is.null(U)) {pen_vec_cov = pen_vec} else {pen_vec_cov = c(pen_vec, rep(0,ncol(U)))}

        pen_vec_cov[pen_vec_cov > 1e3] <- 1e3
        C = sum(pen_vec_cov)/p

        obj = glmnet(cbind(X, U),Y,family = "multinomial",alpha = c, lambda = C, penalty.factor = pen_vec_cov, type.measure="mae")
        cus.coef = coef(obj,x=cbind(X, U),y=Y,family = "multinomial",alpha = c, exact=TRUE,s= C, penalty.factor = pen_vec_cov,standardize=standardize,intercept = intercept)

        return(list(model = obj, beta.est = cus.coef, penalty.vector = pen_vec_cov, lambda = C, alpha.est = alpha.est.old,
                    n_iter = s-1,likelihood.score = likelihood.score, sigma.square = NULL))

}
