#' @import glmnet crayon selectiveInference
#' @importFrom stats optim coef var predict
xtune.fit <- function(X, Y, Z, U, c, epsilon, sigma.square, alpha.est.init, maxstep, margin,
                      maxstep_inner, margin_inner, compute.likelihood, verbosity, standardize = standardize,
                      intercept = intercept) {

        n = nrow(X)
        p = ncol(X)

        ##------------ Elastic-net regression
        ## Initialize
        alpha.old = alpha.est.init
        likelihood.score = c()
        s = 1

        ## calculate alpha.max
        alpha.max = max(abs(colSums(X*as.vector(Y))))/ (c * n)

        ## start estimation
        cat(yellow$italic$bold("Start estimating alpha:\n"))
        while(s < maxstep){
                # Given alpha, update theta
                lambda = exp(Z%*%alpha.old)
                gamma = 2/(lambda^2*c^2 + 2*lambda*(1-c))
                Sigma_y = sigma.square * diag(n) + (t(t(X) * c(gamma))) %*% t(X)
                theta = colSums(X * solve(Sigma_y, X))

                # Compute likelihood
                if (compute.likelihood == TRUE) {
                        likelihood.score = c(likelihood.score, approx_likelihood.xtune(alpha.old,
                                                                                    X, Y, Z, sigma.square, c))
                }

                # Given theta, update alpha
                update.result <- update_alpha.xtune(X, Y, Z,c=c, alpha.old = alpha.old, alpha.max = alpha.max, epsilon = epsilon,
                                                 sigma.square = sigma.square, theta = theta, maxstep_inner = maxstep_inner,
                                                 margin_inner = margin_inner, verbosity = verbosity)
                alpha.new <- update.result$alpha.est

                # Check convergence
                if (sum(abs(alpha.new - alpha.old)) < margin) {
                        cat(red$bold("Done!\n"))
                        break
                }
                alpha.old <- alpha.new

                # Track iteration progress
                if (verbosity == TRUE) {
                        cat(green$italic("#---"),green$bold("Outer loop Iteration",s,"Done"),green$italic("---#\n"),sep = "")
                }
                s <- s + 1
        }

        tauEst = exp(Z%*%alpha.old)
        pen_vec = tauEst * sigma.square/n
        pen_vec[pen_vec>1e6] <- 1e6

        if(is.null(U)) {pen_vec_cov = pen_vec} else {pen_vec_cov = c(pen_vec, rep(0,ncol(U)))}
        C = sum(pen_vec_cov)/p

        obj <- glmnet(cbind(X, U), Y, alpha = c,family="gaussian",lambda = C, penalty.factor = pen_vec_cov, standardize = standardize, intercept = intercept)
        cus.coef <- coef(obj,x=cbind(X, U),y=Y,alpha = c, exact=TRUE,s= C, penalty.factor = pen_vec_cov,standardize=standardize,intercept = intercept)

        return(list(model = obj, beta.est = cus.coef, penalty.vector = pen_vec_cov, lambda = C, alpha.est = alpha.old,
                    n_iter = s - 1, sigma.square = sigma.square, likelihood.score = likelihood.score))
}
