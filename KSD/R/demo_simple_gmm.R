#' Tests 1-dimensional Gaussian Distribution with customized parameters
#'
#' @description We generate a gaussian distribution with given parameters,
#' and add noise to this dataset. We then compute the score
#' of each dataset for the original true distribution.
#'
#' @param truemu mean of true distribution
#' @param truesd standard deviation of true distribution
#' @param noisemu mean of gaussian noise to add
#' @param noisesd standard deviation of gaussian noise to add
#' @param n number of samples to generate
#'
#'
#'
#'
#' @export
demo_simple_gaussian <- function(truemu=5, truesd=1, noisemu=0, noisesd=2, n=100){
      data <- rnorm(n,mean=truemu,sd=truesd)
      noise <- rnorm(n,mean=noisemu,sd=noisesd)
      noisydata <- data + noise

      truemodel <- gmm(nComp=1, mu=truemu, sigma=(truesd^2), weights=1, d=1)
      score_function = pryr::partial(scorefunctiongmm, model=truemodel)
      originalresult <- KSD(data,score_function=score_function,'rbf',-1.0)
      noisyresult <- KSD(noisydata,score_function=score_function,'rbf',-1.0)

      print(sprintf('p for fitting true distribution with original data: %f',originalresult$p))
      print(sprintf('p for fitting true distribution with data with noise: %f',noisyresult$p))
}

#' Shows KSD p value change with respect variation in noise
#'
#' @description We generate a standard normal distribution,
#' and add varying gaussian noise to this dataset and see the change in pvalues.
#'
#'
#' @export
demo_normal_performance <- function(){
      n=100; truemu=0; truesd=1;noisemu=0;
      data <- rnorm(n, mean=truemu, sd=truesd)
      model <- gmm(nComp=1, mu=truemu, sigma=truesd, weights=1, d=1)
      score_function = pryr::partial(scorefunctiongmm, model=model)
      #noisemu=seq(0,1,0.01);noisesd=rep(1,length(noisemu)); noiseaxis = noisemu;
      noisesd=seq(0,1,0.01);noisemu=rep(0,length(noisesd)); noiseaxis = noisesd
      pval = rep(0,length(noisesd))
      for (i in 1:length(noisesd)){
            noise <- rnorm(n,mean=noisemu[i],sd=noisesd[i])
            noisydata <- data + noise
            result <- KSD(noisydata,score_function=score_function,'rbf',-1.0)
            pval[i] = result$p
      }

      par(mar=c(3,3,3,1))
      plot(noiseaxis,pval,main='Change of KSD p-values of Standard Normal Distribution \n with change in SD of Gaussian noise',
           xlab = 'SD of Gaussian Noise',ylab='p-value')
      abline(h=0.05,col=2)
}

#' Tests 1-dimensional Gamma Distribution with customized parameters
#'
#' @description We generate a gamma distribution with given parameters,
#' and add gaussian noise to this dataset. We then compute the score
#' of each dataset for the original true distribution.
#'
#' @param trueshape shape of true gamma distribution
#' @param truescale scale of true gamma distribution
#' @param noisemu mean of gaussian noise to add
#' @param noisesd standard deviation of gaussian noise to add
#' @param n number of samples to generate
#'
#'
#' @export
demo_simple_gamma <- function(trueshape=10, truescale=3, noisemu=5, noisesd=2, n=100){
      data <- rgamma(n,shape = trueshape,scale = truescale)
      noise <- rnorm(n,mean=noisemu,sd=noisesd)
      noisydata <- data + noise

      score_function = pryr::partial(gamma_score, shape=trueshape, scale=truescale)
      originalresult <- KSD(data,score_function=score_function,'rbf',-1.0)
      noisyresult <- KSD(noisydata,score_function=score_function,'rbf',-1.0)

      print(sprintf('p for fitting true distribution with original data: %f',originalresult$p))
      print(sprintf('p for fitting true distribution with data with noise: %f',noisyresult$p))
}



#' Tests 1-dimensional Gaussian Mixture Models.
#' @export

demo_gmm <- function(){
      print('1. 1-dimensional Gaussian, 100 samples')
      model <- gmm()
      X <- rgmm(model, n=100)
      score_function = pryr::partial(scorefunctiongmm, model=model)
      result <- KSD(X,score_function=score_function, 'rbf',-1.0)

      model2 <- perturbgmm(model)
      score_function = pryr::partial(scorefunctiongmm, model=model2)
      result2 <- KSD(X,score_function=score_function, 'rbf',-1.0)
      print(sprintf('Original p : %f',result$p))
      print(sprintf('Original p : %f',result2$p))

      print('2. 1-dimensional Gaussian, 300 samples')
      model <- gmm()
      X <- rgmm(model, n=300)
      score_function = pryr::partial(scorefunctiongmm, model=model)
      result <- KSD(X,score_function=score_function, 'rbf',-1.0)

      model2 <- perturbgmm(model)
      score_function = pryr::partial(scorefunctiongmm, model=model2)
      result2 <- KSD(X,score_function=score_function, 'rbf',-1.0)
      print(sprintf('Original p : %f',result$p))
      print(sprintf('Original p : %f',result2$p))

      print('3. 1-dimensional Gaussian, 100 samples, median-heuristic')
      model <- gmm()
      X <- rgmm(model, n=100)
      score_function = pryr::partial(scorefunctiongmm, model=model)
      result <- KSD(X,score_function=score_function, 'rbf',-2.0)

      model2 <- perturbgmm(model)
      score_function = pryr::partial(scorefunctiongmm, model=model2)
      result2 <- KSD(X,score_function=score_function, 'rbf',-2.0)
      print(sprintf('Original p : %f',result$p))
      print(sprintf('Original p : %f',result2$p))

      print('4. 1-dimensional Gaussian, 300 samples, median-heuristic')
      model <- gmm()
      X <- rgmm(model, n=300)
      score_function = pryr::partial(scorefunctiongmm, model=model)
      result <- KSD(X,score_function=score_function, 'rbf',-2.0)

      model2 <- perturbgmm(model)
      score_function = pryr::partial(scorefunctiongmm, model=model2)
      result2 <- KSD(X,score_function=score_function, 'rbf',-2.0)
      print(sprintf('Original p : %f',result$p))
      print(sprintf('Original p : %f',result2$p))
}

#' Tests multidimensional Gaussian Mixture Models.
#' @export

demo_gmm_multi <- function(){
      print('1. 3-dimensional Gaussian, 100 samples')
      model <- gmm(nComp=5,d=3)
      X <- rgmm(model, n=100)
      score_function = pryr::partial(scorefunctiongmm, model=model)
      result <- KSD(X,score_function=score_function, 'rbf',-1.0)

      model2 <- perturbgmm(model)
      score_function = pryr::partial(scorefunctiongmm, model=model2)
      result2 <- KSD(X,score_function=score_function, 'rbf',-1.0)
      print(sprintf('Original p : %f',result$p))
      print(sprintf('Original p : %f',result2$p))

      print('2. 3-dimensional Gaussian, 300 samples')
      model <- gmm(nComp=5,d=3)
      X <- rgmm(model, n=300)
      score_function = pryr::partial(scorefunctiongmm, model=model)
      result <- KSD(X,score_function=score_function, 'rbf',-1.0)

      model2 <- perturbgmm(model)
      score_function = pryr::partial(scorefunctiongmm, model=model2)
      result2 <- KSD(X,score_function=score_function, 'rbf',-1.0)
      print(sprintf('Original p : %f',result$p))
      print(sprintf('Original p : %f',result2$p))

      print('3. 3-dimensional Gaussian, 100 samples, median-heuristic')
      model <- gmm(nComp=5,d=3)
      X <- rgmm(model, n=100)
      score_function = pryr::partial(scorefunctiongmm, model=model)
      result <- KSD(X,score_function=score_function, 'rbf',-2.0)

      model2 <- perturbgmm(model)
      score_function = pryr::partial(scorefunctiongmm, model=model2)
      result2 <- KSD(X,score_function=score_function, 'rbf',-2.0)
      print(sprintf('Original p : %f',result$p))
      print(sprintf('Original p : %f',result2$p))

      print('4. 3-dimensional Gaussian, 300 samples, median-heuristic')
      model <- gmm(nComp=5,d=3)
      X <- rgmm(model, n=300)
      score_function = pryr::partial(scorefunctiongmm, model=model)
      result <- KSD(X,score_function=score_function, 'rbf',-2.0)

      model2 <- perturbgmm(model)
      score_function = pryr::partial(scorefunctiongmm, model=model2)
      result2 <- KSD(X,score_function=score_function, 'rbf',-2.0)
      print(sprintf('Original p : %f',result$p))
      print(sprintf('Original p : %f',result2$p))
}
