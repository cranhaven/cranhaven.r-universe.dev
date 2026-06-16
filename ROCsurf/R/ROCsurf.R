#' ROC Surface Analysis Under the Three-class Problems
#' @export
#' @name ROCsurf
#' @param x,y,z vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If `length(n) > 1`, the length is taken
#'  to be the number required.
#' @param alpha shape parameter.
#' @param beta scale parameter.
#' @param alpha1 shape parameter of distribution of first sample.
#' @param beta1 scale parameter of distribution of first sample.
#' @param alpha2 shape parameter of distribution of second sample.
#' @param beta2 scale parameter of distribution of second sample.
#' @param alpha3 location parameter of distribution of third sample.
#' @param beta3 scale parameter of distribution of third sample.
#' @param init_index initial index value for the optimization calculation.
#' @param init_param initial paremeter values for the estimation method.
#' @param true_param true parameter values.
#' @param method estimation method. The default value for the method is "MLE".
#' @param model selected model. The default value for the model is "GWL".
#' @param empirical empirical must be TRUE or FALSE.
#' @description ROC surface analysis is performed under the three-class
#' classification problems. The volume under the ROC surface and true positive
#' fractions values are evaulated by ROC surface analysis.
#' @details
#' The Gamma, Weibull, and Logistic distributions are widely used in statistical
#'  modeling and analysis. Below are the descriptions of their probability
#'  density functions (PDF), cumulative distribution functions (CDF), and
#'  quantile functions, including their parameter constraints.
#'
#' **Gamma Distribution**
#'
#' The Gamma distribution is a continuous probability distribution characterized
#'  by its shape (\eqn{\alpha}) and scale (\eqn{\beta}) parameters. It is
#'  commonly used to model waiting times or lifetimes of objects.
#'
#' - **Probability Density Function (PDF)**:
#'   \deqn{f(x; \alpha, \beta) = \frac{1}{\Gamma(\alpha) \beta^\alpha}
#'   x^{\alpha - 1} \exp\left(-\frac{x}{\beta}\right),{x > 0}}
#'   ,where \eqn{\alpha > 0} and \eqn{\beta > 0}.
#'
#' - **Cumulative Distribution Function (CDF)**:
#'   \deqn{F(x; \alpha, \beta) = \frac{1}{\Gamma(\alpha)}
#'   \gamma\left(\alpha, \frac{x}{\beta}\right)}
#'  ,where \eqn{\gamma(\alpha, x/\beta)} is the lower incomplete Gamma function.
#'
#' - **Quantile Function**:
#'
#'   The quantile function is the inverse of the CDF, denoted as
#'  \eqn{Q(p; \alpha, \beta) = F^{-1}(p; \alpha, \beta)}
#'  ,where \eqn{0 < p < 1}.
#'
#' **Weibull Distribution**
#'
#' The Weibull distribution is used in reliability analysis and failure time
#' analysis, characterized by its shape (\eqn{\alpha}) and scale (\eqn{\beta})
#'  parameters.
#'
#' - **Probability Density Function (PDF)**:
#'   \deqn{f(x; \alpha, \beta) = \frac{\alpha}{\beta}
#'   \left( \frac{x}{\beta} \right)^{\alpha - 1}
#'   \exp\left( - \left( \frac{x}{\beta} \right)^\alpha \right), {x \geq 0}}
#'   ,where \eqn{\alpha > 0}, and \eqn{\beta > 0}.
#'
#' - **Cumulative Distribution Function (CDF)**:
#'   \deqn{F(x; \alpha, \beta) = 1 - \exp\left( - \left( \frac{x}{\beta}
#'   \right)^\alpha \right)}
#'   ,where \eqn{\alpha > 0}, and \eqn{\beta > 0}.
#'
#' - **Quantile Function**:
#'   \deqn{Q(p; \alpha, \beta) = \beta \left( -\log(1 - p) \right)^{1/\alpha}}
#'   ,where \eqn{0 < p < 1}, \eqn{\alpha > 0}, and \eqn{\beta > 0}.
#'
#' **Logistic Distribution**
#'
#' The Logistic distribution is used for growth models and logistic regression,
#' characterized by its location (\eqn{\alpha}) and scale (\eqn{\beta})
#' parameters.
#'
#' - **Probability Density Function (PDF)**:
#'   \deqn{f(x; \alpha, \beta) = \frac{\exp \left( - \frac{x - \alpha}{\beta}
#'   \right)}{\beta \left( 1 + \exp \left( - \frac{x - \alpha}{\beta} \right)
#'   \right)^2},{-\infty < x < \infty}}
#'   ,where \eqn{\alpha \in \mathbb{R}}, and \eqn{\beta > 0}.
#'
#' - **Cumulative Distribution Function (CDF)**:
#'   \deqn{F(x; \alpha, \beta) = \frac{1}{1 +
#'   \exp \left( - \frac{x - \alpha}{\beta} \right)}}
#'   ,where \eqn{\alpha \in \mathbb{R}}, and \eqn{\beta > 0}.
#'
#' - **Quantile Function**:
#'   \deqn{Q(p; \alpha, \beta) = \alpha + \beta \log
#'   \left( \frac{p}{1 - p} \right)}
#'   ,where \eqn{0 < p < 1}, \eqn{\alpha \in \mathbb{R}}, and \eqn{\beta > 0}.
#'
#'Additionally, the estimation methods Anderson-Darling "AD", Cramér-von Mises
#'"CvM", least squares "LS" and weighted least squares "WLS" as well as the
#'"TRUE" option for the true value, are available. Please note that the default
#'value for the method parameter is maximum likelihood "ML" estimation. Also,
#'models such as "GWL," "WWW," and "WGW" are defined for evaluating ROC surface
#'analysis under three-class classification problems.
#'
#'The cut-off point values corresponding to the generalized Youden's J index
#'(J), The Perfection method (PM), The Maximum Volume (MV), and the newly
#'proposed indices (NI, M) are provided.
#' @references
#' Akgenç, E., and Kuş, C., 2023,
#' *Statistical Inference for ROC Surface Analysis Under the Three-Class*
#' *Problems*,
#' 7th International Congress of Researchers, Statisticians and
#' Young Statisticians (IRSYSC-2023).
#'
#' B. R. Mosier and L. E. Bantis., 2021,
#' *Estimation and construction of confidence intervals for biomarker*
#' *cutoff-points under the shortest euclidean distance from the roc surface to*
#' *the perfection corner*,
#' Statistics in medicine, 40(20):4522–4539.
#'<doi:10.1002/sim.9077>
#'
#' G. Jameson., 2016,
#' *The incomplete gamma functions.*,
#' The Mathematical Gazette, 100(548):298–306.
#'<doi:10.1017/mag.2016.67>
#'
#' T. Dong., 2014,
#' *Selected Topics on Statistical Methods for Three and Multiple Class*
#' *Diagnostic Studies*,
#' State University of New York at Buffalo.
#'
#' J. Luo and C. Xiong., 2013,
#' *Youden index and associated cut-points for three ordinal diagnostic groups*,
#' Communications in Statistics-Simulation and Computation, 42(6):1213–1234.
#'<doi:10.1080/03610918.2012.661906>
#'
#' F. Edition, A. Papoulis, and S. U. Pillai., 2002,
#' *Pobability, random variables, and stochastic processes*,
#' McGraw-Hill Europe: New York, NY, USA.
#'
#' A. J. Hallinan Jr., 1993,
#' *A review of the weibull distribution*,
#' Journal of Quality Technology, 25(2):85–93.
#'<doi:10.1080/00224065.1993.11979431>
#'
#' N. Balakrishnan., 1991,
#' *Handbook of the logistic distribution*,
#' CRC Press.
#'
#' @return  `dG` gives the probability density function of Gamma
#' Distribution.
#' @examples
#' dG(c(1,2,3,4,5,200,1000),alpha=6,beta=.8)
dG<-function(x,alpha,beta){
  if(any(alpha<=0)) {stop(paste("alpha value must be greater than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greater than 0","\n",""))}
  pdf<-stats::dgamma(x,shape=alpha,scale=beta)
  return(pdf)
}
#' ROC Surface Analysis Under the Three-class Problems
#' @export
#' @rdname ROCsurf
#' @return `dW` gives the probability density function of Weibull
#' Distribution.
#' @examples
#' dW(c(1,2,3,4,5,200,10000),alpha=1,beta=2)
dW<- function(x,alpha,beta) {
  if(any(alpha<=0)) {stop(paste("alpha value must be greater than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greater than 0","\n",""))}
  pdf<-stats::dweibull(x,shape=alpha,scale=beta)
  return(pdf)
}
#' ROC Surface Analysis Under the Three-class Problems
#' @export
#' @rdname ROCsurf
#' @return `dL` gives the probability density function of Logistic
#' Distribution.
#' @examples
#' dL(c(1,2,3,4,5,200),alpha=1,beta=.1)
dL<-function(z,alpha,beta){
  if(any(alpha<=0)) {stop(paste("alpha value must be greater than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greater than 0","\n",""))}
  pdf<-stats::dlogis(z,location = alpha, scale = beta)
  return(pdf)
}
#' ROC Surface Analysis Under the Three-class Problems
#' @export
#' @rdname ROCsurf
#' @return `pG` gives the cumulative density function of
#' Gamma Distribution.
#' @examples
#' pG(c(.5,1,2,3,4,25),alpha=6,beta=.8)
pG<-function(x,alpha,beta){
  if(any(alpha<=0)) {stop(paste("alpha value must be greater than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greater than 0","\n",""))}
  cdf<-stats::pgamma(x,shape=alpha,scale=beta)
  return(cdf)
}
#' ROC Surface Analysis Under the Three-class Problems
#' @export
#' @rdname ROCsurf
#' @return `pW` gives the cumulative density function of
#' Weibull Distribution.
#' @examples
#' pW(c(.5,1,2,3,4,100),alpha=1,beta=2)
pW<- function(y,alpha,beta) {
  if(any(alpha<=0)) {stop(paste("alpha value must be greater than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greater than 0","\n",""))}
  cdf<-stats::pweibull(y,shape=alpha,scale=beta)
  return(cdf)
}
#' ROC Surface Analysis Under the Three-class Problems
#' @export
#' @rdname ROCsurf
#' @return `pL` gives the cumulative density function of
#' Logistic Distribution.
#' @examples
#' pL(c(.5,1,2),alpha=1,beta=.1)
pL<- function(y,alpha,beta) {
  if(any(alpha<=0)) {stop(paste("alpha value must be greater than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greater than 0","\n",""))}
  cdf<-stats::plogis(y,location = alpha, scale = beta)
  return(cdf)
}
#' ROC Surface Analysis Under the Three-class Problems
#' @export
#' @rdname ROCsurf
#' @return `qG` gives the quantile function of
#' Gamma Distribution.
#' @examples
#' qG(c(.9971,0.5,0.3),alpha=6,beta=.8)
qG<-function(p,alpha,beta){
  if(any(alpha<=0)) {stop(paste("alpha value must be greater than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greater than 0","\n",""))}
  if(any(p<0)|any(p>1)) {stop("p must be between 0 and 1")}
  quan<-stats::qgamma(p,shape=alpha,scale=beta)
  return(quan)
}
#' ROC Surface Analysis Under the Three-class Problems
#' @export
#' @rdname ROCsurf
#' @return `qW` gives the quantile function of
#' Weibull Distribution.
#' @examples
#' qW(c(.9971,0.5,0.3),alpha=1,beta=2)
qW<-function(p,alpha,beta){
  if(any(alpha<=0)) {stop(paste("alpha value must be greater than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greater than 0","\n",""))}
  if(any(p<0)|any(p>1)) {stop("p must be between 0 and 1")}
  quan<-stats::qweibull(p,shape=alpha,scale=beta)
  return(quan)
}
#' ROC Surface Analysis Under the Three-class Problems
#' @export
#' @rdname ROCsurf
#' @return `qL` gives the quantile function of
#' Logistic Distribution.
#' @examples
#' qL(c(.9971,0.5,0.3),alpha=1,beta=.1)
qL<-function(p,alpha,beta){
  if(any(alpha<=0)) {stop(paste("alpha value must be greater than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greater than 0","\n",""))}
  if(any(p<0)|any(p>1)) {stop("p must be between 0 and 1")}
  quan<-stats::qlogis(p,location = alpha, scale = beta)
  return(quan)
}
#' ROC Surface Analysis Under the Three-class Problems
#' @export
#' @rdname ROCsurf
#' @return `rG` gives random numbers from Gamma Distribution.
#' @examples
#' rG(10,alpha=6,beta=.8)
rG<-function(n,alpha,beta){
  n<-base::floor(n)
  if(any(n<1)) {stop(paste("n value must be >=1","\n",""))}
  if(any(alpha<=0)) {stop(paste("alpha value must be greater than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greater than 0","\n",""))}
  rnd<-stats::rgamma(n,shape=alpha,scale=beta)
  return(rnd)
}
#' ROC Surface Analysis Under the Three-class Problems
#' @export
#' @rdname ROCsurf
#' @return `rW` gives random numbers from Weibull Distribution.
#' @examples
#' rW(10,alpha=1,beta=2)
rW<-function(n,alpha,beta){
  n<-base::floor(n)
  if(any(n<1)) {stop(paste("n value must be >=1","\n",""))}
  if(any(alpha<=0)) {stop(paste("alpha value must be greater than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greater than 0","\n",""))}
  rnd<-stats::rweibull(n,shape=alpha,scale=beta)
  return(rnd)
}
#' ROC Surface Analysis Under the Three-class Problems
#' @export
#' @rdname ROCsurf
#' @return `rL` gives random numbers from Logistic Distribution.
#' @examples
#' rL(10,alpha=1,beta=.1)
rL<-function(n,alpha,beta){
  n<-base::floor(n)
  if(any(n<1)) {stop(paste("n value must be >=1","\n",""))}
  if(any(alpha<=0)) {stop(paste("alpha value must be greater than 0","\n",""))}
  if(any(beta<=0)) {stop(paste("beta value must be greater than 0","\n",""))}
  rnd<-stats::rlogis(n,location = alpha, scale = beta)
  return(rnd)
}
#' ROC Surface Analysis Under the Three-class Problems
#' @export
#' @rdname ROCsurf
#' @return `r.tc_vus` gives the Volume Under the Surface (VUS) when the
#' data conforms to the proposed three distributions.
#' @examples
#'x<- rW(100,  2,  1)
#'y <- rG(100,  2, 2)
#'z <- rW(100,  6,  9)
#'r.tc_vus(x=x,y=y,z=z,
#'         init_param=c(alpha1=2,beta1=1,alpha2=2,beta2=2,
#'                      alpha3=6,beta3=9),
#'         model=c("WGW"), method=c("MLE"))
r.tc_vus<- function(x,y,z,
                    init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,
                                 beta3=1),
                    true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,
                                 beta3=1),
                    model=c("GWL","GGW","WGW","WWW","GGG","LLL"),
                    method=c("MLE","AD","CvM","LSE","WLSE","TRUE")
)
{
  alpha1<-init_param[[1]]
  beta1<-init_param[[2]]
  alpha2<-init_param[[3]]
  beta2<-init_param[[4]]
  alpha3<-init_param[[5]]
  beta3<-init_param[[6]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}
  model<- base::match.arg(model)
  method<- base::match.arg(method)
  if (model=="GWL") {
    if (method=="MLE") {
      lfxyz<-function(par) {
        alpha1<-par[1]
        beta1<-par[2]
        alpha2<-par[3]
        beta2<-par[4]
        alpha3<-par[5]
        beta3<-par[6]
        t<-(-sum(log(dG(x,alpha1,beta1)))-sum(log(dW(y,alpha2,beta2)))-
              sum(log(dL(z,alpha3,beta3))))
        return(t)
      }
      mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                                lfxyz,method = "L-BFGS-B",hessian = TRUE)),
                  silent=TRUE)
      if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- mlexyz$par[1]
        beta1 <- mlexyz$par[2]
        alpha2 <- mlexyz$par[3]
        beta2 <- mlexyz$par[4]
        alpha3<- mlexyz$par[5]
        beta3<- mlexyz$par[6]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pW(qL(1-c2,alpha3,beta3),alpha2,beta2)-pW(qG(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="AD") {
      QADx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        adx<-sort(x,decreasing=TRUE)
        n<-NROW(x)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(x,alpha1,beta1))+log(1-pG(adx,alpha1,
                                                                   beta1))))
        return(AD)
      }
      QADy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        ady<-sort(y,decreasing=TRUE)
        n<-NROW(y)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pW(y,alpha2,beta2))+log(1-pW(ady,alpha2,
                                                                   beta2))))
        return(AD)
      }
      QADz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        adz<-sort(z,decreasing=TRUE)
        n<-NROW(z)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pL(z,alpha3,beta3))+log(1-pL(adz,alpha3,
                                                                   beta3))))
        return(AD)
      }
      adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
      adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
      adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)
      if (is.character(adex)|is.character(adey)|is.character(adez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      }
      else {
        alpha1 <- adex$par[1]
        beta1 <- adex$par[2]
        alpha2 <- adey$par[1]
        beta2 <- adey$par[2]
        alpha3<- adez$par[1]
        beta3<- adez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pW(qL(1-c2,alpha3,beta3),alpha2,beta2)-pW(qG(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="CvM") {
      QCVx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(x,alpha1,beta1)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pW(y,alpha2,beta2)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pL(z,alpha3,beta3)-(i-0.5)/n)^2)
        return(CV)
      }
      cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
      cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
      cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)
      if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- cvx$par[1]
        beta1 <- cvx$par[2]
        alpha2 <- cvy$par[1]
        beta2 <- cvy$par[2]
        alpha3 <- cvz$par[1]
        beta3 <- cvz$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pW(qL(1-c2,alpha3,beta3),alpha2,beta2)-pW(qG(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="LSE") {
      QLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        Q<-sum((pG(x,alpha1,beta1)-i/(n+1))^2)
        return(Q)
      }
      QLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        Q<-sum((pW(y,alpha2,beta2)-i/(n+1))^2)
        return(Q)
      }
      QLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        Q<-sum((pL(z,alpha3,beta3)-i/(n+1))^2)
        return(Q)
      }
      lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
      lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
      lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)
      if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- lsex$par[1]
        beta1 <- lsex$par[2]
        alpha2 <- lsey$par[1]
        beta2 <- lsey$par[2]
        alpha3 <- lsez$par[1]
        beta3 <- lsez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pW(qL(1-c2,alpha3,beta3),alpha2,beta2)-pW(qG(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="WLSE") {
      QWLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(x,alpha1,beta1)-i/(n+1))^2)
        return(QW)
      }
      QWLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(y,alpha2,beta2)-i/(n+1))^2)
        return(QW)
      }
      QWLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pL(z,alpha3,beta3)-i/(n+1))^2)
        return(QW)
      }
      wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
      wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
      wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
      if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- wlsex$par[1]
        beta1 <- wlsex$par[2]
        alpha2 <- wlsey$par[1]
        beta2 <- wlsey$par[2]
        alpha3 <- wlsez$par[1]
        beta3 <- wlsez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pW(qL(1-c2,alpha3,beta3),alpha2,beta2)-pW(qG(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="TRUE") {
      x<-NULL
      y<-NULL
      z<-NULL
      init_param <- NULL
      alpha1<-true_param[[1]]
      beta1<-true_param[[2]]
      alpha2<-true_param[[3]]
      beta2<-true_param[[4]]
      alpha3<-true_param[[5]]
      beta3<-true_param[[6]]
 if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
   if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
 if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
   if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
 if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
   if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}
      pracma::integral2(function(c1,c2) {
        vus<-(pW(qL(1-c2,alpha3,beta3),alpha2,beta2)-pW(qG(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
  }
  else if (model=="GGW") {
    if (method=="MLE") {
      lfxyz<-function(par) {
        alpha1<-par[1]
        beta1<-par[2]
        alpha2<-par[3]
        beta2<-par[4]
        alpha3<-par[5]
        beta3<-par[6]
        t<-(-sum(log(dG(x,alpha1,beta1)))-sum(log(dG(y,alpha2,beta2)))-
              sum(log(dW(z,alpha3,beta3))))
        return(t)
      }
      mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                                lfxyz,hessian = TRUE)),silent=TRUE)
      if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- mlexyz$par[1]
        beta1 <- mlexyz$par[2]
        alpha2 <- mlexyz$par[3]
        beta2 <- mlexyz$par[4]
        alpha3<- mlexyz$par[5]
        beta3<- mlexyz$par[6]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qW(1-c2,alpha3,beta3),alpha2,beta2)-pG(qG(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="AD") {
      QADx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        adx<-sort(x,decreasing=TRUE)
        n<-NROW(x)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(x,alpha1,beta1))+log(1-pG(adx,alpha1,
                                                                   beta1))))
        return(AD)
      }
      QADy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        ady<-sort(y,decreasing=TRUE)
        n<-NROW(y)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(y,alpha2,beta2))+log(1-pG(ady,alpha2,
                                                                   beta2))))
        return(AD)
      }
      QADz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        adz<-sort(z,decreasing=TRUE)
        n<-NROW(z)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pW(z,alpha3,beta3))+log(1-pW(adz,alpha3,
                                                                   beta3))))
        return(AD)
      }
      adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
      adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
      adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)
      if (is.character(adex)|is.character(adey)|is.character(adez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      }
      else {
        alpha1 <- adex$par[1]
        beta1 <- adex$par[2]
        alpha2 <- adey$par[1]
        beta2 <- adey$par[2]
        alpha3<- adez$par[1]
        beta3<- adez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qW(1-c2,alpha3,beta3),alpha2,beta2)-pG(qG(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="CvM") {
      QCVx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(x,alpha1,beta1)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(y,alpha2,beta2)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pW(z,alpha3,beta3)-(i-0.5)/n)^2)
        return(CV)
      }
      cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
      cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
      cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)
      if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
     stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- cvx$par[1]
        beta1 <- cvx$par[2]
        alpha2 <- cvy$par[1]
        beta2 <- cvy$par[2]
        alpha3 <- cvz$par[1]
        beta3 <- cvz$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qW(1-c2,alpha3,beta3),alpha2,beta2)-pG(qG(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="LSE") {
      QLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        Q<-sum((pG(x,alpha1,beta1)-i/(n+1))^2)
        return(Q)
      }
      QLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        Q<-sum((pG(y,alpha2,beta2)-i/(n+1))^2)
        return(Q)
      }
      QLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        Q<-sum((pW(z,alpha3,beta3)-i/(n+1))^2)
        return(Q)
      }
      lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
      lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
      lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)
      if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
     stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- lsex$par[1]
        beta1 <- lsex$par[2]
        alpha2 <- lsey$par[1]
        beta2 <- lsey$par[2]
        alpha3 <- lsez$par[1]
        beta3 <- lsez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qW(1-c2,alpha3,beta3),alpha2,beta2)-pG(qG(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="WLSE") {
      QWLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(x,alpha1,beta1)-i/(n+1))^2)
        return(QW)
      }
      QWLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(y,alpha2,beta2)-i/(n+1))^2)
        return(QW)
      }
      QWLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(z,alpha3,beta3)-i/(n+1))^2)
        return(QW)
      }
      wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
      wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
      wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
      if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
     stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- wlsex$par[1]
        beta1 <- wlsex$par[2]
        alpha2 <- wlsey$par[1]
        beta2 <- wlsey$par[2]
        alpha3 <- wlsez$par[1]
        beta3 <- wlsez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qW(1-c2,alpha3,beta3),alpha2,beta2)-pG(qG(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="TRUE") {
      x<-NULL
      y<-NULL
      z<-NULL
      init_param <- NULL
      alpha1<-true_param[[1]]
      beta1<-true_param[[2]]
      alpha2<-true_param[[3]]
      beta2<-true_param[[4]]
      alpha3<-true_param[[5]]
      beta3<-true_param[[6]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
  if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
  if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
  if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qW(1-c2,alpha3,beta3),alpha2,beta2)-pG(qG(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
  }
  else if (model=="WGW") {
    if (method=="MLE") {
      lfxyz<-function(par) {
        alpha1<-par[1]
        beta1<-par[2]
        alpha2<-par[3]
        beta2<-par[4]
        alpha3<-par[5]
        beta3<-par[6]
        t<-(-sum(log(dW(x,alpha1,beta1)))-sum(log(dG(y,alpha2,beta2)))-
              sum(log(dW(z,alpha3,beta3))))
        return(t)
      }
      mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                                lfxyz,hessian = TRUE)),silent=TRUE)
      if (is.character(mlexyz)) {
   stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- mlexyz$par[1]
        beta1 <- mlexyz$par[2]
        alpha2 <- mlexyz$par[3]
        beta2 <- mlexyz$par[4]
        alpha3<- mlexyz$par[5]
        beta3<- mlexyz$par[6]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qW(1-c2,alpha3,beta3),alpha2,beta2)-pG(qW(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="AD") {
      QADx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        adx<-sort(x,decreasing=TRUE)
        n<-NROW(x)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pW(x,alpha1,beta1))+
                                    log(1-pW(adx,alpha1,beta1))))
        return(AD)
      }
      QADy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        ady<-sort(y,decreasing=TRUE)
        n<-NROW(y)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(y,alpha2,beta2))+
                                    log(1-pG(ady,alpha2,beta2))))
        return(AD)
      }
      QADz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        adz<-sort(z,decreasing=TRUE)
        n<-NROW(z)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pW(z,alpha3,beta3))+
                                    log(1-pW(adz,alpha3,beta3))))
        return(AD)
      }
      adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
      adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
      adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)
      if (is.character(adex)|is.character(adey)|is.character(adez)) {
     stop("Optimization did not converge.Please check your initial parameters.")
      }
      else {
        alpha1 <- adex$par[1]
        beta1 <- adex$par[2]
        alpha2 <- adey$par[1]
        beta2 <- adey$par[2]
        alpha3<- adez$par[1]
        beta3<- adez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qW(1-c2,alpha3,beta3),alpha2,beta2)-pG(qW(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q

    }
    else if (method=="CvM") {
      QCVx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pW(x,alpha1,beta1)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(y,alpha2,beta2)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pW(z,alpha3,beta3)-(i-0.5)/n)^2)
        return(CV)
      }
      cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
      cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
      cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)
      if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
     stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- cvx$par[1]
        beta1 <- cvx$par[2]
        alpha2 <- cvy$par[1]
        beta2 <- cvy$par[2]
        alpha3 <- cvz$par[1]
        beta3 <- cvz$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qW(1-c2,alpha3,beta3),alpha2,beta2)-pG(qW(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="LSE") {
      QLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        Q<-sum((pW(x,alpha1,beta1)-i/(n+1))^2)
        return(Q)
      }
      QLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        Q<-sum((pG(y,alpha2,beta2)-i/(n+1))^2)
        return(Q)
      }
      QLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        Q<-sum((pW(z,alpha3,beta3)-i/(n+1))^2)
        return(Q)
      }
      lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
      lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
      lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)
      if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
     stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- lsex$par[1]
        beta1 <- lsex$par[2]
        alpha2 <- lsey$par[1]
        beta2 <- lsey$par[2]
        alpha3 <- lsez$par[1]
        beta3 <- lsez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qW(1-c2,alpha3,beta3),alpha2,beta2)-pG(qW(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="WLSE") {
      QWLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(x,alpha1,beta1)-i/(n+1))^2)
        return(QW)
      }
      QWLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(y,alpha2,beta2)-i/(n+1))^2)
        return(QW)
      }
      QWLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(z,alpha3,beta3)-i/(n+1))^2)
        return(QW)
      }
      wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
      wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
      wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
      if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- wlsex$par[1]
        beta1 <- wlsex$par[2]
        alpha2 <- wlsey$par[1]
        beta2 <- wlsey$par[2]
        alpha3 <- wlsez$par[1]
        beta3 <- wlsez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qW(1-c2,alpha3,beta3),alpha2,beta2)-pG(qW(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="TRUE") {
      x<-NULL
      y<-NULL
      z<-NULL
      init_param <- NULL
      alpha1<-true_param[[1]]
      beta1<-true_param[[2]]
      alpha2<-true_param[[3]]
      beta2<-true_param[[4]]
      alpha3<-true_param[[5]]
      beta3<-true_param[[6]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
  if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
 if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
   if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
 if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
   if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qW(1-c2,alpha3,beta3),alpha2,beta2)-pG(qW(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
  }
  else if (model=="WWW") {
    if (method=="MLE") {
      lfxyz<-function(par) {
        alpha1<-par[1]
        beta1<-par[2]
        alpha2<-par[3]
        beta2<-par[4]
        alpha3<-par[5]
        beta3<-par[6]
        t<-(-sum(log(dW(x,alpha1,beta1)))-sum(log(dW(y,alpha2,beta2)))-
              sum(log(dW(z,alpha3,beta3))))
        return(t)
      }
      mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                                lfxyz,hessian = TRUE)),silent=TRUE)
      if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- mlexyz$par[1]
        beta1 <- mlexyz$par[2]
        alpha2 <- mlexyz$par[3]
        beta2 <- mlexyz$par[4]
        alpha3<- mlexyz$par[5]
        beta3<- mlexyz$par[6]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pW(qW(1-c2,alpha3,beta3),alpha2,beta2)-pW(qW(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="AD") {
      QADx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        adx<-sort(x,decreasing=TRUE)
        n<-NROW(x)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pW(x,alpha1,beta1))+
                                    log(1-pW(adx,alpha1,beta1))))
        return(AD)
      }
      QADy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        ady<-sort(y,decreasing=TRUE)
        n<-NROW(y)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pW(y,alpha2,beta2))+
                                    log(1-pW(ady,alpha2,beta2))))
        return(AD)
      }
      QADz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        adz<-sort(z,decreasing=TRUE)
        n<-NROW(z)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pW(z,alpha3,beta3))+
                                    log(1-pW(adz,alpha3,beta3))))
        return(AD)
      }
      adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
      adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
      adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)
      if (is.character(adex)|is.character(adey)|is.character(adez)) {
   stop("Optimization did not converge.Please check your initial parameters.")
      }
      else {
        alpha1 <- adex$par[1]
        beta1 <- adex$par[2]
        alpha2 <- adey$par[1]
        beta2 <- adey$par[2]
        alpha3<- adez$par[1]
        beta3<- adez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pW(qW(1-c2,alpha3,beta3),alpha2,beta2)-
                pW(qW(c1,alpha1,beta1),alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="CvM") {
      QCVx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pW(x,alpha1,beta1)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pW(y,alpha2,beta2)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pW(z,alpha3,beta3)-(i-0.5)/n)^2)
        return(CV)
      }
      cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
      cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
      cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)
      if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- cvx$par[1]
        beta1 <- cvx$par[2]
        alpha2 <- cvy$par[1]
        beta2 <- cvy$par[2]
        alpha3 <- cvz$par[1]
        beta3 <- cvz$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pW(qW(1-c2,alpha3,beta3),alpha2,beta2)-
                pW(qW(c1,alpha1,beta1),alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="LSE") {
      QLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        Q<-sum((pW(x,alpha1,beta1)-i/(n+1))^2)
        return(Q)
      }
      QLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        Q<-sum((pW(y,alpha2,beta2)-i/(n+1))^2)
        return(Q)
      }
      QLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        Q<-sum((pW(z,alpha3,beta3)-i/(n+1))^2)
        return(Q)
      }
      lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
      lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
      lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)
      if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
     stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- lsex$par[1]
        beta1 <- lsex$par[2]
        alpha2 <- lsey$par[1]
        beta2 <- lsey$par[2]
        alpha3 <- lsez$par[1]
        beta3 <- lsez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pW(qW(1-c2,alpha3,beta3),alpha2,beta2)-
                pW(qW(c1,alpha1,beta1),alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="WLSE") {
      QWLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(x,alpha1,beta1)-i/(n+1))^2)
        return(QW)
      }
      QWLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(y,alpha2,beta2)-i/(n+1))^2)
        return(QW)
      }
      QWLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(z,alpha3,beta3)-i/(n+1))^2)
        return(QW)
      }
      wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
      wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
      wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
      if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
     stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- wlsex$par[1]
        beta1 <- wlsex$par[2]
        alpha2 <- wlsey$par[1]
        beta2 <- wlsey$par[2]
        alpha3 <- wlsez$par[1]
        beta3 <- wlsez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pW(qW(1-c2,alpha3,beta3),alpha2,beta2)-
                pW(qW(c1,alpha1,beta1),alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="TRUE") {
      x<-NULL
      y<-NULL
      z<-NULL
      init_param <- NULL
      alpha1<-true_param[[1]]
      beta1<-true_param[[2]]
      alpha2<-true_param[[3]]
      beta2<-true_param[[4]]
      alpha3<-true_param[[5]]
      beta3<-true_param[[6]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
 if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
   if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
 if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
   if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}
      pracma::integral2(function(c1,c2) {
        vus<-(pW(qW(1-c2,alpha3,beta3),alpha2,beta2)-
                pW(qW(c1,alpha1,beta1),alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
  }
  else if (model=="GGG") {
    if (method=="MLE") {
      lfxyz<-function(par) {
        alpha1<-par[1]
        beta1<-par[2]
        alpha2<-par[3]
        beta2<-par[4]
        alpha3<-par[5]
        beta3<-par[6]
        t<-(-sum(log(dG(x,alpha1,beta1)))-sum(log(dG(y,alpha2,beta2)))-
              sum(log(dG(z,alpha3,beta3))))
        return(t)
      }
      mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                                lfxyz,hessian = TRUE)),silent=TRUE)
      if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- mlexyz$par[1]
        beta1 <- mlexyz$par[2]
        alpha2 <- mlexyz$par[3]
        beta2 <- mlexyz$par[4]
        alpha3<- mlexyz$par[5]
        beta3<- mlexyz$par[6]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qG(1-c2,alpha3,beta3),alpha2,beta2)-pG(qG(c1,alpha1,beta1),
                                                        alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="AD") {
      QADx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        adx<-sort(x,decreasing=TRUE)
        n<-NROW(x)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(x,alpha1,beta1))+
                                    log(1-pG(adx,alpha1,beta1))))
        return(AD)
      }
      QADy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        ady<-sort(y,decreasing=TRUE)
        n<-NROW(y)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(y,alpha2,beta2))+
                                    log(1-pG(ady,alpha2,beta2))))
        return(AD)
      }
      QADz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        adz<-sort(z,decreasing=TRUE)
        n<-NROW(z)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(z,alpha3,beta3))+
                                    log(1-pG(adz,alpha3,beta3))))
        return(AD)
      }
      adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
      adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
      adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)
      if (is.character(adex)|is.character(adey)|is.character(adez)) {
   stop("Optimization did not converge.Please check your initial parameters.")
      }
      else {
        alpha1 <- adex$par[1]
        beta1 <- adex$par[2]
        alpha2 <- adey$par[1]
        beta2 <- adey$par[2]
        alpha3<- adez$par[1]
        beta3<- adez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qG(1-c2,alpha3,beta3),alpha2,beta2)-
                pG(qG(c1,alpha1,beta1),alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="CvM") {
      QCVx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(x,alpha1,beta1)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(y,alpha2,beta2)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(z,alpha3,beta3)-(i-0.5)/n)^2)
        return(CV)
      }
      cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
      cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
      cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)
      if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- cvx$par[1]
        beta1 <- cvx$par[2]
        alpha2 <- cvy$par[1]
        beta2 <- cvy$par[2]
        alpha3 <- cvz$par[1]
        beta3 <- cvz$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qG(1-c2,alpha3,beta3),alpha2,beta2)-
                pG(qG(c1,alpha1,beta1),alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="LSE") {
      QLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        Q<-sum((pG(x,alpha1,beta1)-i/(n+1))^2)
        return(Q)
      }
      QLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        Q<-sum((pG(y,alpha2,beta2)-i/(n+1))^2)
        return(Q)
      }
      QLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        Q<-sum((pG(z,alpha3,beta3)-i/(n+1))^2)
        return(Q)
      }
      lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
      lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
      lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)
      if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
  stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- lsex$par[1]
        beta1 <- lsex$par[2]
        alpha2 <- lsey$par[1]
        beta2 <- lsey$par[2]
        alpha3 <- lsez$par[1]
        beta3 <- lsez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qG(1-c2,alpha3,beta3),alpha2,beta2)-
                pG(qG(c1,alpha1,beta1),alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="WLSE") {
      QWLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(x,alpha1,beta1)-i/(n+1))^2)
        return(QW)
      }
      QWLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(y,alpha2,beta2)-i/(n+1))^2)
        return(QW)
      }
      QWLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(z,alpha3,beta3)-i/(n+1))^2)
        return(QW)
      }
      wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
      wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
      wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
      if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- wlsex$par[1]
        beta1 <- wlsex$par[2]
        alpha2 <- wlsey$par[1]
        beta2 <- wlsey$par[2]
        alpha3 <- wlsez$par[1]
        beta3 <- wlsez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qG(1-c2,alpha3,beta3),alpha2,beta2)-
                pG(qG(c1,alpha1,beta1),alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="TRUE") {
      x<-NULL
      y<-NULL
      z<-NULL
      init_param <- NULL
      alpha1<-true_param[[1]]
      beta1<-true_param[[2]]
      alpha2<-true_param[[3]]
      beta2<-true_param[[4]]
      alpha3<-true_param[[5]]
      beta3<-true_param[[6]]
 if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
   if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
 if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
   if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
   if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}
      pracma::integral2(function(c1,c2) {
        vus<-(pG(qG(1-c2,alpha3,beta3),alpha2,beta2)-
                pG(qG(c1,alpha1,beta1),alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
  }
  else if (model=="LLL") {
    if (method=="MLE") {
      lfxyz<-function(par) {
        alpha1<-par[1]
        beta1<-par[2]
        alpha2<-par[3]
        beta2<-par[4]
        alpha3<-par[5]
        beta3<-par[6]
        t<-(-sum(log(dL(x,alpha1,beta1)))-sum(log(dL(y,alpha2,beta2)))-
              sum(log(dL(z,alpha3,beta3))))
        return(t)
      }
      mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                                lfxyz,hessian = TRUE)),silent=TRUE)
      if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- mlexyz$par[1]
        beta1 <- mlexyz$par[2]
        alpha2 <- mlexyz$par[3]
        beta2 <- mlexyz$par[4]
        alpha3<- mlexyz$par[5]
        beta3<- mlexyz$par[6]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pL(qL(1-c2,alpha3,beta3),alpha2,beta2)-
                pL(qL(c1,alpha1,beta1),alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="AD") {
      QADx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        adx<-sort(x,decreasing=TRUE)
        n<-NROW(x)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pL(x,alpha1,beta1))+
                                    log(1-pL(adx,alpha1,beta1))))
        return(AD)
      }
      QADy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        ady<-sort(y,decreasing=TRUE)
        n<-NROW(y)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pL(y,alpha2,beta2))+
                                    log(1-pL(ady,alpha2,beta2))))
        return(AD)
      }
      QADz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        adz<-sort(z,decreasing=TRUE)
        n<-NROW(z)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pL(z,alpha3,beta3))+
                                    log(1-pL(adz,alpha3,beta3))))
        return(AD)
      }
      adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
      adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
      adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)
      if (is.character(adex)|is.character(adey)|is.character(adez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      }
      else {
        alpha1 <- adex$par[1]
        beta1 <- adex$par[2]
        alpha2 <- adey$par[1]
        beta2 <- adey$par[2]
        alpha3<- adez$par[1]
        beta3<- adez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pL(qL(1-c2,alpha3,beta3),alpha2,beta2)-
                pL(qL(c1,alpha1,beta1),alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="CvM") {
      QCVx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pL(x,alpha1,beta1)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pL(y,alpha2,beta2)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pL(z,alpha3,beta3)-(i-0.5)/n)^2)
        return(CV)
      }
      cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
      cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
      cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)
      if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- cvx$par[1]
        beta1 <- cvx$par[2]
        alpha2 <- cvy$par[1]
        beta2 <- cvy$par[2]
        alpha3 <- cvz$par[1]
        beta3 <- cvz$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pL(qL(1-c2,alpha3,beta3),alpha2,beta2)-
                pL(qL(c1,alpha1,beta1),alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="LSE") {
      QLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        Q<-sum((pL(x,alpha1,beta1)-i/(n+1))^2)
        return(Q)
      }
      QLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        Q<-sum((pL(y,alpha2,beta2)-i/(n+1))^2)
        return(Q)
      }
      QLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        Q<-sum((pL(z,alpha3,beta3)-i/(n+1))^2)
        return(Q)
      }
      lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
      lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
      lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)
      if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
   stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- lsex$par[1]
        beta1 <- lsex$par[2]
        alpha2 <- lsey$par[1]
        beta2 <- lsey$par[2]
        alpha3 <- lsez$par[1]
        beta3 <- lsez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pL(qL(1-c2,alpha3,beta3),alpha2,beta2)-
                pL(qL(c1,alpha1,beta1),alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="WLSE") {
      QWLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pL(x,alpha1,beta1)-i/(n+1))^2)
        return(QW)
      }
      QWLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pL(y,alpha2,beta2)-i/(n+1))^2)
        return(QW)
      }
      QWLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pL(z,alpha3,beta3)-i/(n+1))^2)
        return(QW)
      }
      wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
      wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
      wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
      if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
  stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- wlsex$par[1]
        beta1 <- wlsex$par[2]
        alpha2 <- wlsey$par[1]
        beta2 <- wlsey$par[2]
        alpha3 <- wlsez$par[1]
        beta3 <- wlsez$par[2]
      }
      pracma::integral2(function(c1,c2) {
        vus<-(pL(qL(1-c2,alpha3,beta3),alpha2,beta2)-
                pL(qL(c1,alpha1,beta1),alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
    else if (method=="TRUE") {
      x<-NULL
      y<-NULL
      z<-NULL
      init_param <- NULL
      alpha1<-true_param[[1]]
      beta1<-true_param[[2]]
      alpha2<-true_param[[3]]
      beta2<-true_param[[4]]
      alpha3<-true_param[[5]]
      beta3<-true_param[[6]]
 if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
  if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
 if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
   if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
 if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
   if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}
      pracma::integral2(function(c1,c2) {
        vus<-(pL(qL(1-c2,alpha3,beta3),alpha2,beta2)-
                pL(qL(c1,alpha1,beta1),alpha2,beta2))
        return(vus)
      },0,1,0,1)$Q
    }
  }
}
#' ROC Surface Analysis Under the Three-class Problems
#' @export
#' @rdname ROCsurf
#' @return `r.tc_index` gives index values when the
#' data conforms to the proposed three distributions.
#' @examples
#'x<- rW(100,  2,  1)
#'y <- rG(100,  2, 2)
#'z <- rW(100,  6,  9)
#'r.tc_index(x=x,y=y,z=z,
#'           init_param=c(alpha1=2,beta1=1,alpha2=2,
#'                        beta2=2,alpha3=6,beta3=9),
#'           init_index=c(median(x),median(y)),
#'           model=c("WGW"),
#'           method=c("MLE"))
r.tc_index<- function(x,y,z,
              init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
              true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
                      init_index=c(x,y),
                      model=c("GWL","GGW","WGW","WWW","GGG","LLL"),
                      method=c("MLE","AD","CvM","LSE","WLSE","TRUE")
)
{
  alpha1<-init_param[[1]]
  beta1<-init_param[[2]]
  alpha2<-init_param[[3]]
  beta2<-init_param[[4]]
  alpha3<-init_param[[5]]
  beta3<-init_param[[6]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}
  model<- base::match.arg(model)
  method<- base::match.arg(method)
  if (model=="GWL") {
    if (method=="MLE") {
      lfxyz<-function(par) {
        alpha1<-par[1]
        beta1<-par[2]
        alpha2<-par[3]
        beta2<-par[4]
        alpha3<-par[5]
        beta3<-par[6]
        t<-(-sum(log(dG(x,alpha1,beta1)))-sum(log(dW(y,alpha2,beta2)))-
              sum(log(dL(z,alpha3,beta3))))
        return(t)
      }
      mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                        lfxyz,method = "L-BFGS-B",hessian = TRUE)),silent=TRUE)
      if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- mlexyz$par[1]
        beta1 <- mlexyz$par[2]
        alpha2 <- mlexyz$par[3]
        beta2 <- mlexyz$par[4]
        alpha3<- mlexyz$par[5]
        beta3<- mlexyz$par[6]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
          pW(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])
      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])
      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          (((1-pL(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])
      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          ((1-pL(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*
          (1-(pW(cut[2],alpha2,beta2)-
                pW(cut[1],alpha2,beta2)))*(1-(1-pL(cut[2],alpha3,beta3)))
        return(-a)
      }
 NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])
      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
               pW(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                  pW(cut[1],alpha2,beta2)))*(((1-pL(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
              pW(cut[1],alpha2,beta2)))^2+(1-((1-pL(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])
      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pW(Jr[2],alpha2,beta2)-pW(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pL(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pW(EDr[2],alpha2,beta2)-pW(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pL(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pW(MVr[2],alpha2,beta2)-pW(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pL(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pW(NIr[2],alpha2,beta2)-pW(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pL(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pW(Mr[2],alpha2,beta2)-pW(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pL(Mr[2],alpha3,beta3))
      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="AD") {
      QADx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        adx<-sort(x,decreasing=TRUE)
        n<-NROW(x)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(x,alpha1,beta1))+
                                    log(1-pG(adx,alpha1,beta1))))
        return(AD)
      }
      QADy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        ady<-sort(y,decreasing=TRUE)
        n<-NROW(y)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pW(y,alpha2,beta2))+
                                    log(1-pW(ady,alpha2,beta2))))
        return(AD)
      }
      QADz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        adz<-sort(z,decreasing=TRUE)
        n<-NROW(z)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pL(z,alpha3,beta3))+
                                    log(1-pL(adz,alpha3,beta3))))
        return(AD)
      }
      adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
      adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
      adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)
      if (is.character(adex)|is.character(adey)|is.character(adez)) {
 stop("Optimization did not converge.Please check your initial parameters.")
      }
      else {
        alpha1 <- adex$par[1]
        beta1 <- adex$par[2]
        alpha2 <- adey$par[1]
        beta2 <- adey$par[2]
        alpha3<- adez$par[1]
        beta3<- adez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
          pW(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])
      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                              pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])
      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                      pW(cut[1],alpha2,beta2)))*(((1-pL(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])
      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          ((1-pL(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pW(cut[2],alpha2,beta2)-
                                            pW(cut[1],alpha2,beta2)))*
          (1-(1-pL(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])
      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
               pW(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                             pW(cut[1],alpha2,beta2)))*
                 (((1-pL(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])
      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pW(Jr[2],alpha2,beta2)-pW(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pL(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pW(EDr[2],alpha2,beta2)-pW(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pL(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pW(MVr[2],alpha2,beta2)-pW(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pL(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pW(NIr[2],alpha2,beta2)-pW(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pL(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pW(Mr[2],alpha2,beta2)-pW(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pL(Mr[2],alpha3,beta3))
      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="CvM") {
      QCVx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(x,alpha1,beta1)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pW(y,alpha2,beta2)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pL(z,alpha3,beta3)-(i-0.5)/n)^2)
        return(CV)
      }
      cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
      cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
      cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)
      if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- cvx$par[1]
        beta1 <- cvx$par[2]
        alpha2 <- cvy$par[1]
        beta2 <- cvy$par[2]
        alpha3 <- cvz$par[1]
        beta3 <- cvz$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
          pW(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])
      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])
      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          (((1-pL(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])
      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          ((1-pL(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pW(cut[2],alpha2,beta2)-
                                            pW(cut[1],alpha2,beta2)))*
          (1-(1-pL(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])
      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
               pW(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                             pW(cut[1],alpha2,beta2)))*
                 (((1-pL(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])
      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pW(Jr[2],alpha2,beta2)-pW(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pL(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pW(EDr[2],alpha2,beta2)-pW(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pL(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pW(MVr[2],alpha2,beta2)-pW(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pL(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pW(NIr[2],alpha2,beta2)-pW(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pL(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pW(Mr[2],alpha2,beta2)-pW(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pL(Mr[2],alpha3,beta3))
      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="LSE") {
      QLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        Q<-sum((pG(x,alpha1,beta1)-i/(n+1))^2)
        return(Q)
      }
      QLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        Q<-sum((pW(y,alpha2,beta2)-i/(n+1))^2)
        return(Q)
      }
      QLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        Q<-sum((pL(z,alpha3,beta3)-i/(n+1))^2)
        return(Q)
      }
      lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
      lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
      lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)
      if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
     stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- lsex$par[1]
        beta1 <- lsex$par[2]
        alpha2 <- lsey$par[1]
        beta2 <- lsey$par[2]
        alpha3 <- lsez$par[1]
        beta3 <- lsez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
          pW(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])
      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2)
        return(d)
      }
 EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])
      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          (((1-pL(cut[2],alpha3,beta3))))
        return(-a)
      }
 MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])
      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          ((1-pL(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pW(cut[2],alpha2,beta2)-
                                            pW(cut[1],alpha2,beta2)))*
          (1-(1-pL(cut[2],alpha3,beta3)))
        return(-a)
      }
 NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])
      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
               pW(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                             pW(cut[1],alpha2,beta2)))*
                 (((1-pL(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
 Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])
      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pW(Jr[2],alpha2,beta2)-pW(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pL(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pW(EDr[2],alpha2,beta2)-pW(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pL(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pW(MVr[2],alpha2,beta2)-pW(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pL(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pW(NIr[2],alpha2,beta2)-pW(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pL(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pW(Mr[2],alpha2,beta2)-pW(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pL(Mr[2],alpha3,beta3))
      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="WLSE") {
      QWLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(x,alpha1,beta1)-i/(n+1))^2)
        return(QW)
      }
      QWLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(y,alpha2,beta2)-i/(n+1))^2)
        return(QW)
      }
      QWLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pL(z,alpha3,beta3)-i/(n+1))^2)
        return(QW)
      }
      wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
      wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
      wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
      if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
   stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- wlsex$par[1]
        beta1 <- wlsex$par[2]
        alpha2 <- wlsey$par[1]
        beta2 <- wlsey$par[2]
        alpha3 <- wlsez$par[1]
        beta3 <- wlsez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
          pW(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])
      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2)
        return(d)
      }
 EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])
      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          (((1-pL(cut[2],alpha3,beta3))))
        return(-a)
      }
 MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])
      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          ((1-pL(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pW(cut[2],alpha2,beta2)-
                                            pW(cut[1],alpha2,beta2)))*
          (1-(1-pL(cut[2],alpha3,beta3)))
        return(-a)
      }
 NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])
      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
               pW(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                             pW(cut[1],alpha2,beta2)))*
                 (((1-pL(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
   Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])
      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pW(Jr[2],alpha2,beta2)-pW(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pL(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pW(EDr[2],alpha2,beta2)-pW(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pL(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pW(MVr[2],alpha2,beta2)-pW(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pL(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pW(NIr[2],alpha2,beta2)-pW(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pL(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pW(Mr[2],alpha2,beta2)-pW(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pL(Mr[2],alpha3,beta3))
      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="TRUE") {
      x<-NULL
      y<-NULL
      z<-NULL
      init_param <- NULL
      alpha1<-true_param[[1]]
      beta1<-true_param[[2]]
      alpha2<-true_param[[3]]
      beta2<-true_param[[4]]
      alpha3<-true_param[[5]]
      beta3<-true_param[[6]]
 if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
 if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
 if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
  if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
  if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
          pW(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])
      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])
      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          (((1-pL(cut[2],alpha3,beta3))))
        return(-a)
      }
 MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])
      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          ((1-pL(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pW(cut[2],alpha2,beta2)-
                                            pW(cut[1],alpha2,beta2)))*
          (1-(1-pL(cut[2],alpha3,beta3)))
        return(-a)
      }
 NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])
      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
               pW(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                             pW(cut[1],alpha2,beta2)))*
                 (((1-pL(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])
      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pW(Jr[2],alpha2,beta2)-pW(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pL(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pW(EDr[2],alpha2,beta2)-pW(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pL(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pW(MVr[2],alpha2,beta2)-pW(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pL(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pW(NIr[2],alpha2,beta2)-pW(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pL(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pW(Mr[2],alpha2,beta2)-pW(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pL(Mr[2],alpha3,beta3))
      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
  }
  else if (model=="GGW") {
    if (method=="MLE") {
      lfxyz<-function(par) {
        alpha1<-par[1]
        beta1<-par[2]
        alpha2<-par[3]
        beta2<-par[4]
        alpha3<-par[5]
        beta3<-par[6]
        t<-(-sum(log(dG(x,alpha1,beta1)))-sum(log(dG(y,alpha2,beta2)))-
              sum(log(dW(z,alpha3,beta3))))
        return(t)
      }
      mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                                lfxyz,hessian = TRUE)),silent=TRUE)

      if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- mlexyz$par[1]
        beta1 <- mlexyz$par[2]
        alpha2 <- mlexyz$par[3]
        beta2 <- mlexyz$par[4]
        alpha3<- mlexyz$par[5]
        beta3<- mlexyz$par[6]
      }

      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-pG(cut[1],
                                                              alpha2,beta2)+
          (1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
 Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-pG(cut[1],alpha2,
                                                                  beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
 NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
               pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                             pG(cut[1],alpha2,beta2)))*
                 (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }

    else if (method=="AD") {

      QADx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        adx<-sort(x,decreasing=TRUE)
        n<-NROW(x)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(x,alpha1,beta1))+
                                    log(1-pG(adx,alpha1,beta1))))
        return(AD)
      }
      QADy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        ady<-sort(y,decreasing=TRUE)
        n<-NROW(y)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(y,alpha2,beta2))+
                                    log(1-pG(ady,alpha2,beta2))))
        return(AD)
      }
      QADz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        adz<-sort(z,decreasing=TRUE)
        n<-NROW(z)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pW(z,alpha3,beta3))+
                                    log(1-pW(adz,alpha3,beta3))))
        return(AD)
      }
      adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
      adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
      adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)

      if (is.character(adex)|is.character(adey)|is.character(adez)) {
   stop("Optimization did not converge.Please check your initial parameters.")
      }
      else {
        alpha1 <- adex$par[1]
        beta1 <- adex$par[2]
        alpha2 <- adey$par[1]
        beta2 <- adey$par[2]
        alpha3<- adez$par[1]
        beta3<- adez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
 MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
               pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                             pG(cut[1],alpha2,beta2)))*
                 (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)

    }
    else if (method=="CvM") {

      QCVx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(x,alpha1,beta1)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(y,alpha2,beta2)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pW(z,alpha3,beta3)-(i-0.5)/n)^2)
        return(CV)
      }
      cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
      cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
      cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)

      if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
  stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- cvx$par[1]
        beta1 <- cvx$par[2]
        alpha2 <- cvy$par[1]
        beta2 <- cvy$par[2]
        alpha3 <- cvz$par[1]
        beta3 <- cvz$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
               pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                             pG(cut[1],alpha2,beta2)))*
                 (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="LSE") {

      QLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        Q<-sum((pG(x,alpha1,beta1)-i/(n+1))^2)
        return(Q)
      }
      QLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        Q<-sum((pG(y,alpha2,beta2)-i/(n+1))^2)
        return(Q)
      }
      QLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        Q<-sum((pW(z,alpha3,beta3)-i/(n+1))^2)
        return(Q)
      }
      lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
      lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
      lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)

      if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- lsex$par[1]
        beta1 <- lsex$par[2]
        alpha2 <- lsey$par[1]
        beta2 <- lsey$par[2]
        alpha3 <- lsez$par[1]
        beta3 <- lsez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
   Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
 EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
               pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                             pG(cut[1],alpha2,beta2)))*
                 (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
   Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="WLSE") {


      QWLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(x,alpha1,beta1)-i/(n+1))^2)
        return(QW)
      }
      QWLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(y,alpha2,beta2)-i/(n+1))^2)
        return(QW)
      }
      QWLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(z,alpha3,beta3)-i/(n+1))^2)
        return(QW)
      }
      wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
      wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
      wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
      if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- wlsex$par[1]
        beta1 <- wlsex$par[2]
        alpha2 <- wlsey$par[1]
        beta2 <- wlsey$par[2]
        alpha3 <- wlsez$par[1]
        beta3 <- wlsez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                   pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
 NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
               pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
                 (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                    pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="TRUE") {
      x<-NULL
      y<-NULL
      z<-NULL
      init_param <- NULL
      alpha1<-true_param[[1]]
      beta1<-true_param[[2]]
      alpha2<-true_param[[3]]
      beta2<-true_param[[4]]
      alpha3<-true_param[[5]]
      beta3<-true_param[[6]]
 if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
 if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
               pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                             pG(cut[1],alpha2,beta2)))*
                 (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
   Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
  }

  else if (model=="WGW") {
    if (method=="MLE") {
      lfxyz<-function(par) {
        alpha1<-par[1]
        beta1<-par[2]
        alpha2<-par[3]
        beta2<-par[4]
        alpha3<-par[5]
        beta3<-par[6]
        t<-(-sum(log(dW(x,alpha1,beta1)))-sum(log(dG(y,alpha2,beta2)))-
              sum(log(dW(z,alpha3,beta3))))
        return(t)
      }
      mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                                lfxyz,hessian = TRUE)),silent=TRUE)

      if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- mlexyz$par[1]
        beta1 <- mlexyz$par[2]
        alpha2 <- mlexyz$par[3]
        beta2 <- mlexyz$par[4]
        alpha3<- mlexyz$par[5]
        beta3<- mlexyz$par[6]
      }

      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pW(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
   Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
 EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pW(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pW(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
              pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
             ((pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
                (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
   Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pW(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pW(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pW(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pW(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pW(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }

    else if (method=="AD") {

      QADx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        adx<-sort(x,decreasing=TRUE)
        n<-NROW(x)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pW(x,alpha1,beta1))+
                                    log(1-pW(adx,alpha1,beta1))))
        return(AD)
      }
      QADy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        ady<-sort(y,decreasing=TRUE)
        n<-NROW(y)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(y,alpha2,beta2))+
                                    log(1-pG(ady,alpha2,beta2))))
        return(AD)
      }
      QADz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        adz<-sort(z,decreasing=TRUE)
        n<-NROW(z)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pW(z,alpha3,beta3))+
                                    log(1-pW(adz,alpha3,beta3))))
        return(AD)
      }
      adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
      adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
      adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)

      if (is.character(adex)|is.character(adey)|is.character(adez)) {
  stop("Optimization did not converge.Please check your initial parameters.")
      }
      else {
        alpha1 <- adex$par[1]
        beta1 <- adex$par[2]
        alpha2 <- adey$par[1]
        beta2 <- adey$par[2]
        alpha3<- adez$par[1]
        beta3<- adez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pW(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
   Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pW(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pW(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
              pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
             ((pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
                (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
   Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pW(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pW(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pW(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pW(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pW(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)

    }
    else if (method=="CvM") {

      QCVx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pW(x,alpha1,beta1)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(y,alpha2,beta2)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pW(z,alpha3,beta3)-(i-0.5)/n)^2)
        return(CV)
      }
      cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
      cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
      cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)

      if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- cvx$par[1]
        beta1 <- cvx$par[2]
        alpha2 <- cvy$par[1]
        beta2 <- cvy$par[2]
        alpha3 <- cvz$par[1]
        beta3 <- cvz$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pW(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
 MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pW(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
 NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pW(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
              pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
             ((pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
                (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pW(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pW(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pW(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pW(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pW(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="LSE") {

      QLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        Q<-sum((pW(x,alpha1,beta1)-i/(n+1))^2)
        return(Q)
      }
      QLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        Q<-sum((pG(y,alpha2,beta2)-i/(n+1))^2)
        return(Q)
      }
      QLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        Q<-sum((pW(z,alpha3,beta3)-i/(n+1))^2)
        return(Q)
      }
      lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
      lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
      lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)

      if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- lsex$par[1]
        beta1 <- lsex$par[2]
        alpha2 <- lsey$par[1]
        beta2 <- lsey$par[2]
        alpha3 <- lsez$par[1]
        beta3 <- lsez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pW(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pW(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pW(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
              pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
             ((pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
                (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pW(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pW(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pW(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pW(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pW(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="WLSE") {


      QWLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(x,alpha1,beta1)-i/(n+1))^2)
        return(QW)
      }
      QWLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(y,alpha2,beta2)-i/(n+1))^2)
        return(QW)
      }
      QWLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(z,alpha3,beta3)-i/(n+1))^2)
        return(QW)
      }
      wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
      wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
      wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
      if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
  stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- wlsex$par[1]
        beta1 <- wlsex$par[2]
        alpha2 <- wlsey$par[1]
        beta2 <- wlsey$par[2]
        alpha3 <- wlsez$par[1]
        beta3 <- wlsez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pW(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
   Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
 EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pW(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pW(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
              pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
             ((pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
                (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
   Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pW(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pW(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pW(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pW(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pW(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="TRUE") {
      x<-NULL
      y<-NULL
      z<-NULL
      init_param <- NULL

      alpha1<-true_param[[1]]
      beta1<-true_param[[2]]
      alpha2<-true_param[[3]]
      beta2<-true_param[[4]]
      alpha3<-true_param[[5]]
      beta3<-true_param[[6]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pW(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pW(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pW(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
              pG(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
             ((pW(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
                (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pW(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pW(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pW(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pW(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pW(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
  }
  else if(model=="WWW") {
    if (method=="MLE") {
      lfxyz<-function(par) {
        alpha1<-par[1]
        beta1<-par[2]
        alpha2<-par[3]
        beta2<-par[4]
        alpha3<-par[5]
        beta3<-par[6]
        t<-(-sum(log(dW(x,alpha1,beta1)))-sum(log(dW(y,alpha2,beta2)))-
              sum(log(dW(z,alpha3,beta3))))
        return(t)
      }
      mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                                lfxyz,hessian = TRUE)),silent=TRUE)

      if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- mlexyz$par[1]
        beta1 <- mlexyz$par[2]
        alpha2 <- mlexyz$par[3]
        beta2 <- mlexyz$par[4]
        alpha3<- mlexyz$par[5]
        beta3<- mlexyz$par[6]
      }

      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pW(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
          pW(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
 EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pW(cut[1],alpha1,beta1))*(1-(pW(cut[2],alpha2,beta2)-
                                            pW(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pW(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
               pW(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
              ((pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                             pW(cut[1],alpha2,beta2)))*
                 (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
   Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pW(Jr[1],alpha1,beta1)
      J_tpf2<-(pW(Jr[2],alpha2,beta2)-pW(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pW(EDr[1],alpha1,beta1)
      ED_tpf2<-(pW(EDr[2],alpha2,beta2)-pW(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pW(MVr[1],alpha1,beta1)
      MV_tpf2<-(pW(MVr[2],alpha2,beta2)-pW(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pW(NIr[1],alpha1,beta1)
      NI_tpf2<-(pW(NIr[2],alpha2,beta2)-pW(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pW(Mr[1],alpha1,beta1)
      M_tpf2<-(pW(Mr[2],alpha2,beta2)-pW(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }

    else if (method=="AD") {

      QADx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        adx<-sort(x,decreasing=TRUE)
        n<-NROW(x)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pW(x,alpha1,beta1))+
                                    log(1-pW(adx,alpha1,beta1))))
        return(AD)
      }
      QADy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        ady<-sort(y,decreasing=TRUE)
        n<-NROW(y)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pW(y,alpha2,beta2))+
                                    log(1-pW(ady,alpha2,beta2))))
        return(AD)
      }
      QADz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        adz<-sort(z,decreasing=TRUE)
        n<-NROW(z)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pW(z,alpha3,beta3))+
                                    log(1-pW(adz,alpha3,beta3))))
        return(AD)
      }
      adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
      adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
      adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)

      if (is.character(adex)|is.character(adey)|is.character(adez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      }
      else {
        alpha1 <- adex$par[1]
        beta1 <- adex$par[2]
        alpha2 <- adey$par[1]
        beta2 <- adey$par[2]
        alpha3<- adez$par[1]
        beta3<- adez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pW(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
          pW(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                    pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                    pW(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pW(cut[1],alpha1,beta1))*(1-(pW(cut[2],alpha2,beta2)-
                                            pW(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pW(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
               pW(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
              ((pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                             pW(cut[1],alpha2,beta2)))*
                 (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pW(Jr[1],alpha1,beta1)
      J_tpf2<-(pW(Jr[2],alpha2,beta2)-pW(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pW(EDr[1],alpha1,beta1)
      ED_tpf2<-(pW(EDr[2],alpha2,beta2)-pW(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pW(MVr[1],alpha1,beta1)
      MV_tpf2<-(pW(MVr[2],alpha2,beta2)-pW(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pW(NIr[1],alpha1,beta1)
      NI_tpf2<-(pW(NIr[2],alpha2,beta2)-pW(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pW(Mr[1],alpha1,beta1)
      M_tpf2<-(pW(Mr[2],alpha2,beta2)-pW(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)

    }
    else if (method=="CvM") {

      QCVx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pW(x,alpha1,beta1)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pW(y,alpha2,beta2)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pW(z,alpha3,beta3)-(i-0.5)/n)^2)
        return(CV)
      }
      cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
      cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
      cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)

      if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
     stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- cvx$par[1]
        beta1 <- cvx$par[2]
        alpha2 <- cvy$par[1]
        beta2 <- cvy$par[2]
        alpha3 <- cvz$par[1]
        beta3 <- cvz$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pW(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
          pW(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
   Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
 EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pW(cut[1],alpha1,beta1))*(1-(pW(cut[2],alpha2,beta2)-
                                            pW(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pW(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
               pW(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
              ((pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                             pW(cut[1],alpha2,beta2)))*
                 (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pW(Jr[1],alpha1,beta1)
      J_tpf2<-(pW(Jr[2],alpha2,beta2)-pW(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pW(EDr[1],alpha1,beta1)
      ED_tpf2<-(pW(EDr[2],alpha2,beta2)-pW(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pW(MVr[1],alpha1,beta1)
      MV_tpf2<-(pW(MVr[2],alpha2,beta2)-pW(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pW(NIr[1],alpha1,beta1)
      NI_tpf2<-(pW(NIr[2],alpha2,beta2)-pW(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pW(Mr[1],alpha1,beta1)
      M_tpf2<-(pW(Mr[2],alpha2,beta2)-pW(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="LSE") {

      QLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        Q<-sum((pW(x,alpha1,beta1)-i/(n+1))^2)
        return(Q)
      }
      QLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        Q<-sum((pW(y,alpha2,beta2)-i/(n+1))^2)
        return(Q)
      }
      QLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        Q<-sum((pW(z,alpha3,beta3)-i/(n+1))^2)
        return(Q)
      }
      lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
      lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
      lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)

      if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- lsex$par[1]
        beta1 <- lsex$par[2]
        alpha2 <- lsey$par[1]
        beta2 <- lsey$par[2]
        alpha3 <- lsez$par[1]
        beta3 <- lsez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pW(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
          pW(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                    pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pW(cut[1],alpha1,beta1))*(1-(pW(cut[2],alpha2,beta2)-
                                            pW(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pW(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
               pW(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
              ((pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                        pW(cut[1],alpha2,beta2)))*
                 (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pW(Jr[1],alpha1,beta1)
      J_tpf2<-(pW(Jr[2],alpha2,beta2)-pW(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pW(EDr[1],alpha1,beta1)
      ED_tpf2<-(pW(EDr[2],alpha2,beta2)-pW(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pW(MVr[1],alpha1,beta1)
      MV_tpf2<-(pW(MVr[2],alpha2,beta2)-pW(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pW(NIr[1],alpha1,beta1)
      NI_tpf2<-(pW(NIr[2],alpha2,beta2)-pW(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pW(Mr[1],alpha1,beta1)
      M_tpf2<-(pW(Mr[2],alpha2,beta2)-pW(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="WLSE") {


      QWLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(x,alpha1,beta1)-i/(n+1))^2)
        return(QW)
      }
      QWLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(y,alpha2,beta2)-i/(n+1))^2)
        return(QW)
      }
      QWLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(z,alpha3,beta3)-i/(n+1))^2)
        return(QW)
      }
      wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
      wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
      wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
      if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- wlsex$par[1]
        beta1 <- wlsex$par[2]
        alpha2 <- wlsey$par[1]
        beta2 <- wlsey$par[2]
        alpha3 <- wlsez$par[1]
        beta3 <- wlsez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pW(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
          pW(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pW(cut[1],alpha1,beta1))*(1-(pW(cut[2],alpha2,beta2)-
                                            pW(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pW(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
               pW(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
              ((pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                             pW(cut[1],alpha2,beta2)))*
                 (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pW(Jr[1],alpha1,beta1)
      J_tpf2<-(pW(Jr[2],alpha2,beta2)-pW(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pW(EDr[1],alpha1,beta1)
      ED_tpf2<-(pW(EDr[2],alpha2,beta2)-pW(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pW(MVr[1],alpha1,beta1)
      MV_tpf2<-(pW(MVr[2],alpha2,beta2)-pW(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pW(NIr[1],alpha1,beta1)
      NI_tpf2<-(pW(NIr[2],alpha2,beta2)-pW(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pW(Mr[1],alpha1,beta1)
      M_tpf2<-(pW(Mr[2],alpha2,beta2)-pW(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="TRUE") {
      x<-NULL
      y<-NULL
      z<-NULL
      init_param <- NULL

      alpha1<-true_param[[1]]
      beta1<-true_param[[2]]
      alpha2<-true_param[[3]]
      beta2<-true_param[[4]]
      alpha3<-true_param[[5]]
      beta3<-true_param[[6]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
  if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
  if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
  if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pW(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
          pW(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1
        return(-a)
      }
   Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2)
        return(d)
      }
 EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                        pW(cut[1],alpha2,beta2)))*
          (((1-pW(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                         pW(cut[1],alpha2,beta2)))*
          ((1-pW(cut[2],alpha3,beta3)))-
          (1-pW(cut[1],alpha1,beta1))*(1-(pW(cut[2],alpha2,beta2)-
                                            pW(cut[1],alpha2,beta2)))*
          (1-(1-pW(cut[2],alpha3,beta3)))
        return(-a)
      }
 NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pW(cut[1],alpha1,beta1)+pW(cut[2],alpha2,beta2)-
               pW(cut[1],alpha2,beta2)+(1-pW(cut[2],alpha3,beta3))-1)+
              ((pW(cut[1],alpha1,beta1))*((pW(cut[2],alpha2,beta2)-
                                             pW(cut[1],alpha2,beta2)))*
                 (((1-pW(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pW(cut[1],alpha1,beta1)))^2+(1-(pW(cut[2],alpha2,beta2)-
                                                  pW(cut[1],alpha2,beta2)))^2+
                  (1-((1-pW(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pW(Jr[1],alpha1,beta1)
      J_tpf2<-(pW(Jr[2],alpha2,beta2)-pW(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pW(Jr[2],alpha3,beta3))
      ED_tpf1<-pW(EDr[1],alpha1,beta1)
      ED_tpf2<-(pW(EDr[2],alpha2,beta2)-pW(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pW(EDr[2],alpha3,beta3))
      MV_tpf1<-pW(MVr[1],alpha1,beta1)
      MV_tpf2<-(pW(MVr[2],alpha2,beta2)-pW(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pW(MVr[2],alpha3,beta3))
      NI_tpf1<-pW(NIr[1],alpha1,beta1)
      NI_tpf2<-(pW(NIr[2],alpha2,beta2)-pW(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pW(NIr[2],alpha3,beta3))
      M_tpf1<-pW(Mr[1],alpha1,beta1)
      M_tpf2<-(pW(Mr[2],alpha2,beta2)-pW(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pW(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
  }
  else if (model=="GGG") {
    if (method=="MLE") {
      lfxyz<-function(par) {
        alpha1<-par[1]
        beta1<-par[2]
        alpha2<-par[3]
        beta2<-par[4]
        alpha3<-par[5]
        beta3<-par[6]
        t<-(-sum(log(dG(x,alpha1,beta1)))-sum(log(dG(y,alpha2,beta2)))-
              sum(log(dG(z,alpha3,beta3))))
        return(t)
      }
      mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                                lfxyz,hessian = TRUE)),silent=TRUE)

      if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- mlexyz$par[1]
        beta1 <- mlexyz$par[2]
        alpha2 <- mlexyz$par[3]
        beta2 <- mlexyz$par[4]
        alpha3<- mlexyz$par[5]
        beta3<- mlexyz$par[6]
      }

      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pG(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pG(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pG(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pG(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pG(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
               pG(cut[1],alpha2,beta2)+(1-pG(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                             pG(cut[1],alpha2,beta2)))*
                 (((1-pG(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pG(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
   Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pG(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pG(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pG(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pG(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pG(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }

    else if (method=="AD") {

      QADx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        adx<-sort(x,decreasing=TRUE)
        n<-NROW(x)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(x,alpha1,beta1))+
                                    log(1-pG(adx,alpha1,beta1))))
        return(AD)
      }
      QADy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        ady<-sort(y,decreasing=TRUE)
        n<-NROW(y)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(y,alpha2,beta2))+
                                    log(1-pG(ady,alpha2,beta2))))
        return(AD)
      }
      QADz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        adz<-sort(z,decreasing=TRUE)
        n<-NROW(z)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(z,alpha3,beta3))+
                                    log(1-pG(adz,alpha3,beta3))))
        return(AD)
      }
      adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
      adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
      adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)

      if (is.character(adex)|is.character(adey)|is.character(adez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      }
      else {
        alpha1 <- adex$par[1]
        beta1 <- adex$par[2]
        alpha2 <- adey$par[1]
        beta2 <- adey$par[2]
        alpha3<- adez$par[1]
        beta3<- adez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pG(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pG(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pG(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pG(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pG(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
               pG(cut[1],alpha2,beta2)+(1-pG(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                             pG(cut[1],alpha2,beta2)))*
                 (((1-pG(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pG(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pG(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pG(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pG(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pG(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pG(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)

    }
    else if (method=="CvM") {

      QCVx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(x,alpha1,beta1)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(y,alpha2,beta2)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(z,alpha3,beta3)-(i-0.5)/n)^2)
        return(CV)
      }
      cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
      cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
      cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)

      if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
   stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- cvx$par[1]
        beta1 <- cvx$par[2]
        alpha2 <- cvy$par[1]
        beta2 <- cvy$par[2]
        alpha3 <- cvz$par[1]
        beta3 <- cvz$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pG(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+
                  (1-(pG(cut[2],alpha2,beta2)-pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pG(cut[2],alpha3,beta3))))^2)
        return(d)
      }
 EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pG(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pG(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pG(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
               pG(cut[1],alpha2,beta2)+(1-pG(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                             pG(cut[1],alpha2,beta2)))*
                 (((1-pG(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pG(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pG(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pG(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pG(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pG(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pG(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="LSE") {

      QLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        Q<-sum((pG(x,alpha1,beta1)-i/(n+1))^2)
        return(Q)
      }
      QLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        Q<-sum((pG(y,alpha2,beta2)-i/(n+1))^2)
        return(Q)
      }
      QLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        Q<-sum((pG(z,alpha3,beta3)-i/(n+1))^2)
        return(Q)
      }
      lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
      lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
      lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)

      if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
     stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- lsex$par[1]
        beta1 <- lsex$par[2]
        alpha2 <- lsey$par[1]
        beta2 <- lsey$par[2]
        alpha3 <- lsez$par[1]
        beta3 <- lsez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pG(cut[2],alpha3,beta3))-1
        return(-a)
      }
   Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pG(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pG(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pG(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pG(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
               pG(cut[1],alpha2,beta2)+(1-pG(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                             pG(cut[1],alpha2,beta2)))*
                 (((1-pG(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pG(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pG(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pG(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pG(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pG(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pG(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="WLSE") {


      QWLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(x,alpha1,beta1)-i/(n+1))^2)
        return(QW)
      }
      QWLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(y,alpha2,beta2)-i/(n+1))^2)
        return(QW)
      }
      QWLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(z,alpha3,beta3)-i/(n+1))^2)
        return(QW)
      }
      wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
      wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
      wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
      if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
   stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- wlsex$par[1]
        beta1 <- wlsex$par[2]
        alpha2 <- wlsey$par[1]
        beta2 <- wlsey$par[2]
        alpha3 <- wlsez$par[1]
        beta3 <- wlsez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pG(cut[2],alpha3,beta3))-1
        return(-a)
      }
   Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pG(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pG(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pG(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pG(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
               pG(cut[1],alpha2,beta2)+(1-pG(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                             pG(cut[1],alpha2,beta2)))*
                 (((1-pG(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pG(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pG(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pG(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pG(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pG(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pG(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="TRUE") {
      x<-NULL
      y<-NULL
      z<-NULL
      init_param <- NULL

      alpha1<-true_param[[1]]
      beta1<-true_param[[2]]
      alpha2<-true_param[[3]]
      beta2<-true_param[[4]]
      alpha3<-true_param[[5]]
      beta3<-true_param[[6]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
          pG(cut[1],alpha2,beta2)+(1-pG(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pG(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          (((1-pG(cut[2],alpha3,beta3))))
        return(-a)
      }
 MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                         pG(cut[1],alpha2,beta2)))*
          ((1-pG(cut[2],alpha3,beta3)))-
          (1-pG(cut[1],alpha1,beta1))*(1-(pG(cut[2],alpha2,beta2)-
                                            pG(cut[1],alpha2,beta2)))*
          (1-(1-pG(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pG(cut[1],alpha1,beta1)+pG(cut[2],alpha2,beta2)-
               pG(cut[1],alpha2,beta2)+(1-pG(cut[2],alpha3,beta3))-1)+
              ((pG(cut[1],alpha1,beta1))*((pG(cut[2],alpha2,beta2)-
                                             pG(cut[1],alpha2,beta2)))*
                 (((1-pG(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pG(cut[1],alpha1,beta1)))^2+(1-(pG(cut[2],alpha2,beta2)-
                                                  pG(cut[1],alpha2,beta2)))^2+
                  (1-((1-pG(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pG(Jr[1],alpha1,beta1)
      J_tpf2<-(pG(Jr[2],alpha2,beta2)-pG(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pG(Jr[2],alpha3,beta3))
      ED_tpf1<-pG(EDr[1],alpha1,beta1)
      ED_tpf2<-(pG(EDr[2],alpha2,beta2)-pG(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pG(EDr[2],alpha3,beta3))
      MV_tpf1<-pG(MVr[1],alpha1,beta1)
      MV_tpf2<-(pG(MVr[2],alpha2,beta2)-pG(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pG(MVr[2],alpha3,beta3))
      NI_tpf1<-pG(NIr[1],alpha1,beta1)
      NI_tpf2<-(pG(NIr[2],alpha2,beta2)-pG(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pG(NIr[2],alpha3,beta3))
      M_tpf1<-pG(Mr[1],alpha1,beta1)
      M_tpf2<-(pG(Mr[2],alpha2,beta2)-pG(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pG(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
  }
  else if (model=="LLL") {
    if (method=="MLE") {
      lfxyz<-function(par) {
        alpha1<-par[1]
        beta1<-par[2]
        alpha2<-par[3]
        beta2<-par[4]
        alpha3<-par[5]
        beta3<-par[6]
        t<-(-sum(log(dL(x,alpha1,beta1)))-sum(log(dL(y,alpha2,beta2)))-
              sum(log(dL(z,alpha3,beta3))))
        return(t)
      }
      mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                                lfxyz,hessian = TRUE)),silent=TRUE)

      if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- mlexyz$par[1]
        beta1 <- mlexyz$par[2]
        alpha2 <- mlexyz$par[3]
        beta2 <- mlexyz$par[4]
        alpha3<- mlexyz$par[5]
        beta3<- mlexyz$par[6]
      }

      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pL(cut[1],alpha1,beta1)+pL(cut[2],alpha2,beta2)-
          pL(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pL(cut[1],alpha1,beta1)))^2+(1-(pL(cut[2],alpha2,beta2)-
                                                  pL(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                         pL(cut[1],alpha2,beta2)))*
          (((1-pL(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                         pL(cut[1],alpha2,beta2)))*
          ((1-pL(cut[2],alpha3,beta3)))-
          (1-pL(cut[1],alpha1,beta1))*(1-(pL(cut[2],alpha2,beta2)-
                                            pL(cut[1],alpha2,beta2)))*
          (1-(1-pL(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pL(cut[1],alpha1,beta1)+pL(cut[2],alpha2,beta2)-
               pL(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1)+
              ((pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                             pL(cut[1],alpha2,beta2)))*
                 (((1-pL(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pL(cut[1],alpha1,beta1)))^2+(1-(pL(cut[2],alpha2,beta2)-
                                                  pL(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pL(Jr[1],alpha1,beta1)
      J_tpf2<-(pL(Jr[2],alpha2,beta2)-pL(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pL(Jr[2],alpha3,beta3))
      ED_tpf1<-pL(EDr[1],alpha1,beta1)
      ED_tpf2<-(pL(EDr[2],alpha2,beta2)-pL(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pL(EDr[2],alpha3,beta3))
      MV_tpf1<-pL(MVr[1],alpha1,beta1)
      MV_tpf2<-(pL(MVr[2],alpha2,beta2)-pL(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pL(MVr[2],alpha3,beta3))
      NI_tpf1<-pL(NIr[1],alpha1,beta1)
      NI_tpf2<-(pL(NIr[2],alpha2,beta2)-pL(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pL(NIr[2],alpha3,beta3))
      M_tpf1<-pL(Mr[1],alpha1,beta1)
      M_tpf2<-(pL(Mr[2],alpha2,beta2)-pL(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pL(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }

    else if (method=="AD") {

      QADx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        adx<-sort(x,decreasing=TRUE)
        n<-NROW(x)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pL(x,alpha1,beta1))+
                                    log(1-pL(adx,alpha1,beta1))))
        return(AD)
      }
      QADy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        ady<-sort(y,decreasing=TRUE)
        n<-NROW(y)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pL(y,alpha2,beta2))+
                                    log(1-pL(ady,alpha2,beta2))))
        return(AD)
      }
      QADz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        adz<-sort(z,decreasing=TRUE)
        n<-NROW(z)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pL(z,alpha3,beta3))+
                                    log(1-pL(adz,alpha3,beta3))))
        return(AD)
      }
      adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
      adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
      adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)

      if (is.character(adex)|is.character(adey)|is.character(adez)) {
  stop("Optimization did not converge.Please check your initial parameters.")
      }
      else {
        alpha1 <- adex$par[1]
        beta1 <- adex$par[2]
        alpha2 <- adey$par[1]
        beta2 <- adey$par[2]
        alpha3<- adez$par[1]
        beta3<- adez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pL(cut[1],alpha1,beta1)+pL(cut[2],alpha2,beta2)-
          pL(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pL(cut[1],alpha1,beta1)))^2+(1-(pL(cut[2],alpha2,beta2)-
                                                  pL(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2)
        return(d)
      }
 EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                         pL(cut[1],alpha2,beta2)))*
          (((1-pL(cut[2],alpha3,beta3))))
        return(-a)
      }
 MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                         pL(cut[1],alpha2,beta2)))*
          ((1-pL(cut[2],alpha3,beta3)))-
          (1-pL(cut[1],alpha1,beta1))*(1-(pL(cut[2],alpha2,beta2)-
                                            pL(cut[1],alpha2,beta2)))*
          (1-(1-pL(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pL(cut[1],alpha1,beta1)+pL(cut[2],alpha2,beta2)-
               pL(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1)+
              ((pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                             pL(cut[1],alpha2,beta2)))*
                 (((1-pL(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pL(cut[1],alpha1,beta1)))^2+(1-(pL(cut[2],alpha2,beta2)-
                                                  pL(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pL(Jr[1],alpha1,beta1)
      J_tpf2<-(pL(Jr[2],alpha2,beta2)-pL(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pL(Jr[2],alpha3,beta3))
      ED_tpf1<-pL(EDr[1],alpha1,beta1)
      ED_tpf2<-(pL(EDr[2],alpha2,beta2)-pL(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pL(EDr[2],alpha3,beta3))
      MV_tpf1<-pL(MVr[1],alpha1,beta1)
      MV_tpf2<-(pL(MVr[2],alpha2,beta2)-pL(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pL(MVr[2],alpha3,beta3))
      NI_tpf1<-pL(NIr[1],alpha1,beta1)
      NI_tpf2<-(pL(NIr[2],alpha2,beta2)-pL(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pL(NIr[2],alpha3,beta3))
      M_tpf1<-pL(Mr[1],alpha1,beta1)
      M_tpf2<-(pL(Mr[2],alpha2,beta2)-pL(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pL(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)

    }
    else if (method=="CvM") {

      QCVx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pL(x,alpha1,beta1)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pL(y,alpha2,beta2)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pL(z,alpha3,beta3)-(i-0.5)/n)^2)
        return(CV)
      }
      cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
      cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
      cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)

      if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
     stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- cvx$par[1]
        beta1 <- cvx$par[2]
        alpha2 <- cvy$par[1]
        beta2 <- cvy$par[2]
        alpha3 <- cvz$par[1]
        beta3 <- cvz$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pL(cut[1],alpha1,beta1)+pL(cut[2],alpha2,beta2)-
          pL(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pL(cut[1],alpha1,beta1)))^2+(1-(pL(cut[2],alpha2,beta2)-
                                                  pL(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                         pL(cut[1],alpha2,beta2)))*
          (((1-pL(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                         pL(cut[1],alpha2,beta2)))*
          ((1-pL(cut[2],alpha3,beta3)))-
          (1-pL(cut[1],alpha1,beta1))*(1-(pL(cut[2],alpha2,beta2)-
                                            pL(cut[1],alpha2,beta2)))*
          (1-(1-pL(cut[2],alpha3,beta3)))
        return(-a)
      }
 NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pL(cut[1],alpha1,beta1)+pL(cut[2],alpha2,beta2)-
               pL(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1)+
              ((pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                             pL(cut[1],alpha2,beta2)))*
                 (((1-pL(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pL(cut[1],alpha1,beta1)))^2+(1-(pL(cut[2],alpha2,beta2)-
                                                  pL(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pL(Jr[1],alpha1,beta1)
      J_tpf2<-(pL(Jr[2],alpha2,beta2)-pL(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pL(Jr[2],alpha3,beta3))
      ED_tpf1<-pL(EDr[1],alpha1,beta1)
      ED_tpf2<-(pL(EDr[2],alpha2,beta2)-pL(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pL(EDr[2],alpha3,beta3))
      MV_tpf1<-pL(MVr[1],alpha1,beta1)
      MV_tpf2<-(pL(MVr[2],alpha2,beta2)-pL(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pL(MVr[2],alpha3,beta3))
      NI_tpf1<-pL(NIr[1],alpha1,beta1)
      NI_tpf2<-(pL(NIr[2],alpha2,beta2)-pL(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pL(NIr[2],alpha3,beta3))
      M_tpf1<-pL(Mr[1],alpha1,beta1)
      M_tpf2<-(pL(Mr[2],alpha2,beta2)-pL(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pL(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="LSE") {

      QLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        Q<-sum((pL(x,alpha1,beta1)-i/(n+1))^2)
        return(Q)
      }
      QLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        Q<-sum((pL(y,alpha2,beta2)-i/(n+1))^2)
        return(Q)
      }
      QLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        Q<-sum((pL(z,alpha3,beta3)-i/(n+1))^2)
        return(Q)
      }
      lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
      lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
      lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)

      if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- lsex$par[1]
        beta1 <- lsex$par[2]
        alpha2 <- lsey$par[1]
        beta2 <- lsey$par[2]
        alpha3 <- lsez$par[1]
        beta3 <- lsez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pL(cut[1],alpha1,beta1)+pL(cut[2],alpha2,beta2)-
          pL(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pL(cut[1],alpha1,beta1)))^2+(1-(pL(cut[2],alpha2,beta2)-
                                                  pL(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2)
        return(d)
      }
 EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                         pL(cut[1],alpha2,beta2)))*
          (((1-pL(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                         pL(cut[1],alpha2,beta2)))*
          ((1-pL(cut[2],alpha3,beta3)))-
          (1-pL(cut[1],alpha1,beta1))*(1-(pL(cut[2],alpha2,beta2)-
                                            pL(cut[1],alpha2,beta2)))*
          (1-(1-pL(cut[2],alpha3,beta3)))
        return(-a)
      }
 NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pL(cut[1],alpha1,beta1)+pL(cut[2],alpha2,beta2)-
               pL(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1)+
              ((pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                             pL(cut[1],alpha2,beta2)))*
                 (((1-pL(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pL(cut[1],alpha1,beta1)))^2+(1-(pL(cut[2],alpha2,beta2)-
                                                  pL(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pL(Jr[1],alpha1,beta1)
      J_tpf2<-(pL(Jr[2],alpha2,beta2)-pL(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pL(Jr[2],alpha3,beta3))
      ED_tpf1<-pL(EDr[1],alpha1,beta1)
      ED_tpf2<-(pL(EDr[2],alpha2,beta2)-pL(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pL(EDr[2],alpha3,beta3))
      MV_tpf1<-pL(MVr[1],alpha1,beta1)
      MV_tpf2<-(pL(MVr[2],alpha2,beta2)-pL(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pL(MVr[2],alpha3,beta3))
      NI_tpf1<-pL(NIr[1],alpha1,beta1)
      NI_tpf2<-(pL(NIr[2],alpha2,beta2)-pL(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pL(NIr[2],alpha3,beta3))
      M_tpf1<-pL(Mr[1],alpha1,beta1)
      M_tpf2<-(pL(Mr[2],alpha2,beta2)-pL(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pL(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="WLSE") {


      QWLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pL(x,alpha1,beta1)-i/(n+1))^2)
        return(QW)
      }
      QWLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pL(y,alpha2,beta2)-i/(n+1))^2)
        return(QW)
      }
      QWLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pL(z,alpha3,beta3)-i/(n+1))^2)
        return(QW)
      }
      wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
      wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
      wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
      if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- wlsex$par[1]
        beta1 <- wlsex$par[2]
        alpha2 <- wlsey$par[1]
        beta2 <- wlsey$par[2]
        alpha3 <- wlsez$par[1]
        beta3 <- wlsez$par[2]
      }
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pL(cut[1],alpha1,beta1)+pL(cut[2],alpha2,beta2)-
          pL(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pL(cut[1],alpha1,beta1)))^2+(1-(pL(cut[2],alpha2,beta2)-
                                                  pL(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                         pL(cut[1],alpha2,beta2)))*
          (((1-pL(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                         pL(cut[1],alpha2,beta2)))*
          ((1-pL(cut[2],alpha3,beta3)))-
          (1-pL(cut[1],alpha1,beta1))*(1-(pL(cut[2],alpha2,beta2)-
                                            pL(cut[1],alpha2,beta2)))*
          (1-(1-pL(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pL(cut[1],alpha1,beta1)+pL(cut[2],alpha2,beta2)-
               pL(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1)+
              ((pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                             pL(cut[1],alpha2,beta2)))*
                 (((1-pL(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pL(cut[1],alpha1,beta1)))^2+(1-(pL(cut[2],alpha2,beta2)-
                                                  pL(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pL(Jr[1],alpha1,beta1)
      J_tpf2<-(pL(Jr[2],alpha2,beta2)-pL(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pL(Jr[2],alpha3,beta3))
      ED_tpf1<-pL(EDr[1],alpha1,beta1)
      ED_tpf2<-(pL(EDr[2],alpha2,beta2)-pL(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pL(EDr[2],alpha3,beta3))
      MV_tpf1<-pL(MVr[1],alpha1,beta1)
      MV_tpf2<-(pL(MVr[2],alpha2,beta2)-pL(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pL(MVr[2],alpha3,beta3))
      NI_tpf1<-pL(NIr[1],alpha1,beta1)
      NI_tpf2<-(pL(NIr[2],alpha2,beta2)-pL(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pL(NIr[2],alpha3,beta3))
      M_tpf1<-pL(Mr[1],alpha1,beta1)
      M_tpf2<-(pL(Mr[2],alpha2,beta2)-pL(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pL(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
    else if (method=="TRUE") {
      x<-NULL
      y<-NULL
      z<-NULL
      init_param <- NULL

      alpha1<-true_param[[1]]
      beta1<-true_param[[2]]
      alpha2<-true_param[[3]]
      beta2<-true_param[[4]]
      alpha3<-true_param[[5]]
      beta3<-true_param[[6]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
  if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
  if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
  if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}
      J<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-pL(cut[1],alpha1,beta1)+pL(cut[2],alpha2,beta2)-
          pL(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1
        return(-a)
      }
  Jr<-stats::optim(init_index,J,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Jrpar<-c(Jr[1],Jr[2])

      ED<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        d<-sqrt((1-(pL(cut[1],alpha1,beta1)))^2+(1-(pL(cut[2],alpha2,beta2)-
                                                  pL(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2)
        return(d)
      }
EDr<-stats::optim(init_index,ED,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      EDrpar<-c(EDr[1],EDr[2])

      MV<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                         pL(cut[1],alpha2,beta2)))*
          (((1-pL(cut[2],alpha3,beta3))))
        return(-a)
      }
MVr<-stats::optim(init_index,MV,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      MVrpar<-c(MVr[1],MVr[2])

      NI<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-(pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                         pL(cut[1],alpha2,beta2)))*
          ((1-pL(cut[2],alpha3,beta3)))-
          (1-pL(cut[1],alpha1,beta1))*(1-(pL(cut[2],alpha2,beta2)-
                                            pL(cut[1],alpha2,beta2)))*
          (1-(1-pL(cut[2],alpha3,beta3)))
        return(-a)
      }
NIr<-stats::optim(init_index,NI,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      NIrpar<-c(NIr[1],NIr[2])

      M<-function(cut){
        c1<-cut[1]
        c2<-cut[2]
        a<-((pL(cut[1],alpha1,beta1)+pL(cut[2],alpha2,beta2)-
               pL(cut[1],alpha2,beta2)+(1-pL(cut[2],alpha3,beta3))-1)+
              ((pL(cut[1],alpha1,beta1))*((pL(cut[2],alpha2,beta2)-
                                             pL(cut[1],alpha2,beta2)))*
                 (((1-pL(cut[2],alpha3,beta3))))))/
          (sqrt((1-(pL(cut[1],alpha1,beta1)))^2+(1-(pL(cut[2],alpha2,beta2)-
                                                  pL(cut[1],alpha2,beta2)))^2+
                  (1-((1-pL(cut[2],alpha3,beta3))))^2))
        return(-a)
      }
  Mr<-stats::optim(init_index,M,method="L-BFGS-B",lower = -Inf,upper = Inf)$par
      Mrpar<-c(Mr[1],Mr[2])


      J_tpf1<-pL(Jr[1],alpha1,beta1)
      J_tpf2<-(pL(Jr[2],alpha2,beta2)-pL(Jr[1],alpha2,beta2))
      J_tpf3<-(1-pL(Jr[2],alpha3,beta3))
      ED_tpf1<-pL(EDr[1],alpha1,beta1)
      ED_tpf2<-(pL(EDr[2],alpha2,beta2)-pL(EDr[1],alpha2,beta2))
      ED_tpf3<-(1-pL(EDr[2],alpha3,beta3))
      MV_tpf1<-pL(MVr[1],alpha1,beta1)
      MV_tpf2<-(pL(MVr[2],alpha2,beta2)-pL(MVr[1],alpha2,beta2))
      MV_tpf3<-(1-pL(MVr[2],alpha3,beta3))
      NI_tpf1<-pL(NIr[1],alpha1,beta1)
      NI_tpf2<-(pL(NIr[2],alpha2,beta2)-pL(NIr[1],alpha2,beta2))
      NI_tpf3<-(1-pL(NIr[2],alpha3,beta3))
      M_tpf1<-pL(Mr[1],alpha1,beta1)
      M_tpf2<-(pL(Mr[2],alpha2,beta2)-pL(Mr[1],alpha2,beta2))
      M_tpf3<-(1-pL(Mr[2],alpha3,beta3))

      row1<-base::cbind(Jrpar[1],Jrpar[2],J_tpf1,J_tpf2,J_tpf3)
      row2<-base::cbind(EDrpar[1],EDrpar[2],ED_tpf1,ED_tpf2,ED_tpf3)
      row3<-base::cbind(MVrpar[1],MVrpar[2],MV_tpf1,MV_tpf2,MV_tpf3)
      row4<-base::cbind(NIrpar[1],NIrpar[2],NI_tpf1,NI_tpf2,NI_tpf3)
      row5<-base::cbind(Mrpar[1],Mrpar[2],M_tpf1,M_tpf2,M_tpf3)
      col<-base::rbind(row1,row2,row3,row4,row5)
      base::colnames(col)<-c("c\u2081","c\u2082",
                             "TPF\u2081","TPF\u2082","TPF\u2083")
      base::rownames(col)<-c("J","PM","MV","NI","M")
      return(col)
    }
  }
}
#' ROC Surface Analysis Under the Three-class Problems
#' @export
#' @rdname ROCsurf
#' @return `r.tc_graph` gives the ROC curve when the data conforms to the
#' proposed three distributions.
#' @examples
#'x<- rW(100,  2,  1)
#'y <- rG(100,  2, 2)
#'z <- rW(100,  6,  9)
#'r.tc_graph(x=x,y=y,z=z,
#'           init_param=c(alpha1=2,beta1=1,alpha2=2,
#'                        beta2=2,alpha3=6,beta3=9),
#'           empirical=FALSE,model=c("WGW"),
#'           method=c("MLE"))
r.tc_graph<- function(x,y,z,
              init_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
              true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
             empirical=TRUE,model=c("GWL","GGW","WGW","WWW","GGG","LLL"),
                      method=c("MLE","AD","CvM","LSE","WLSE","TRUE"))
{

  alpha1<-init_param[[1]]
  beta1<-init_param[[2]]
  alpha2<-init_param[[3]]
  beta2<-init_param[[4]]
  alpha3<-init_param[[5]]
  beta3<-init_param[[6]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}
  model<- base::match.arg(model)
  method<- base::match.arg(method)

  if (model=="GWL") {
    if (method=="MLE") {
      lfxyz<-function(par) {
        alpha1<-par[1]
        beta1<-par[2]
        alpha2<-par[3]
        beta2<-par[4]
        alpha3<-par[5]
        beta3<-par[6]
        t<-(-sum(log(dG(x,alpha1,beta1)))-sum(log(dW(y,alpha2,beta2)))-
              sum(log(dL(z,alpha3,beta3))))
        return(t)
      }
      mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                        lfxyz,method = "L-BFGS-B",hessian = TRUE)),silent=TRUE)

      if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- mlexyz$par[1]
        beta1 <- mlexyz$par[2]
        alpha2 <- mlexyz$par[3]
        beta2 <- mlexyz$par[4]
        alpha3<- mlexyz$par[5]
        beta3<- mlexyz$par[6]
      }
      if (empirical==TRUE) {

        c1 <- base::seq(0.00001, 1, length.out = 100)
        c2 <- base::seq(0.00001, 1, length.out = 100)


        colorscalemp <- base::list(
          list(0, "rgb(255,255,255)"),
          list(0.2, "rgb(230,230,230)"),
          list(0.4, "rgb(200,200,200)"),
          list(0.6, "rgb(150,150,150)"),
          list(0.8, "rgb(100,100,100)"),
          list(1, "rgb(0,0,0)")
        )

        Fn <- stats::ecdf(y)
        vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
          base::pmax(result, 0)
        }))

        fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                              colorscale = colorscalemp)

        fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig2$x$layout$scene$xaxis$range <- c(0, 1)
        fig2$x$layout$scene$yaxis$range <- c(0, 1)
        fig2$x$layout$scene$zaxis$range <- c(0, 1)

        fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig2

      }
      else if (empirical==FALSE) {

        c1 <- seq(0.00001, 1, length.out = 100)
        c2 <- seq(0.00001, 1, length.out = 100)

        vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- pW(qL( c2, alpha3, beta3), alpha2, beta2) -
            pW(qG(1-c1, alpha1, beta1), alpha2, beta2)
          base::pmax(result, 0)
        }))
        fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                              colorscale = "Greys")

        fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig1$x$layout$scene$xaxis$range <- c(0, 1)
        fig1$x$layout$scene$yaxis$range <- c(0, 1)
        fig1$x$layout$scene$zaxis$range <- c(0, 1)

        fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig1
      }

    }

    else if (method=="AD") {

      QADx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        adx<-sort(x,decreasing=TRUE)
        n<-NROW(x)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(x,alpha1,beta1))+
                                    log(1-pG(adx,alpha1,beta1))))
        return(AD)
      }
      QADy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        ady<-sort(y,decreasing=TRUE)
        n<-NROW(y)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pW(y,alpha2,beta2))+
                                    log(1-pW(ady,alpha2,beta2))))
        return(AD)
      }
      QADz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        adz<-sort(z,decreasing=TRUE)
        n<-NROW(z)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pL(z,alpha3,beta3))+
                                    log(1-pL(adz,alpha3,beta3))))
        return(AD)
      }
      adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
      adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
      adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)

      if (is.character(adex)|is.character(adey)|is.character(adez)) {
     stop("Optimization did not converge.Please check your initial parameters.")
      }
      else {
        alpha1 <- adex$par[1]
        beta1 <- adex$par[2]
        alpha2 <- adey$par[1]
        beta2 <- adey$par[2]
        alpha3<- adez$par[1]
        beta3<- adez$par[2]
      }
      if (empirical==TRUE) {

        c1 <- base::seq(0.00001, 1, length.out = 100)
        c2 <- base::seq(0.00001, 1, length.out = 100)


        colorscalemp <- base::list(
          list(0, "rgb(255,255,255)"),
          list(0.2, "rgb(230,230,230)"),
          list(0.4, "rgb(200,200,200)"),
          list(0.6, "rgb(150,150,150)"),
          list(0.8, "rgb(100,100,100)"),
          list(1, "rgb(0,0,0)")
        )

        Fn <- stats::ecdf(y)
        vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
          base::pmax(result, 0)
        }))

        fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2,
                              type = 'surface', colorscale = colorscalemp)

        fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig2$x$layout$scene$xaxis$range <- c(0, 1)
        fig2$x$layout$scene$yaxis$range <- c(0, 1)
        fig2$x$layout$scene$zaxis$range <- c(0, 1)

        fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig2

      }
      else if (empirical==FALSE) {

        c1 <- seq(0.00001, 1, length.out = 100)
        c2 <- seq(0.00001, 1, length.out = 100)

        vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- pW(qL( c2, alpha3, beta3), alpha2, beta2) -
            pW(qG(1-c1, alpha1, beta1), alpha2, beta2)
          base::pmax(result, 0)
        }))
        fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                              colorscale = "Greys")

        fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig1$x$layout$scene$xaxis$range <- c(0, 1)
        fig1$x$layout$scene$yaxis$range <- c(0, 1)
        fig1$x$layout$scene$zaxis$range <- c(0, 1)

        fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig1
      }

    }
    else if (method=="CvM") {

      QCVx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(x,alpha1,beta1)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pW(y,alpha2,beta2)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pL(z,alpha3,beta3)-(i-0.5)/n)^2)
        return(CV)
      }
      cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
      cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
      cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)

      if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- cvx$par[1]
        beta1 <- cvx$par[2]
        alpha2 <- cvy$par[1]
        beta2 <- cvy$par[2]
        alpha3 <- cvz$par[1]
        beta3 <- cvz$par[2]
      }
      if (empirical==TRUE) {

        c1 <- base::seq(0.00001, 1, length.out = 100)
        c2 <- base::seq(0.00001, 1, length.out = 100)


        colorscalemp <- base::list(
          list(0, "rgb(255,255,255)"),
          list(0.2, "rgb(230,230,230)"),
          list(0.4, "rgb(200,200,200)"),
          list(0.6, "rgb(150,150,150)"),
          list(0.8, "rgb(100,100,100)"),
          list(1, "rgb(0,0,0)")
        )

        Fn <- stats::ecdf(y)
        vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
          base::pmax(result, 0)
        }))

        fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                              colorscale = colorscalemp)

        fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig2$x$layout$scene$xaxis$range <- c(0, 1)
        fig2$x$layout$scene$yaxis$range <- c(0, 1)
        fig2$x$layout$scene$zaxis$range <- c(0, 1)

        fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig2

      }
      else if (empirical==FALSE) {

        c1 <- seq(0.00001, 1, length.out = 100)
        c2 <- seq(0.00001, 1, length.out = 100)

        vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- pW(qL( c2, alpha3, beta3), alpha2, beta2) -
            pW(qG(1-c1, alpha1, beta1), alpha2, beta2)
          base::pmax(result, 0)
        }))
        fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                              colorscale = "Greys")

        fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig1$x$layout$scene$xaxis$range <- c(0, 1)
        fig1$x$layout$scene$yaxis$range <- c(0, 1)
        fig1$x$layout$scene$zaxis$range <- c(0, 1)

        fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig1
      }
    }
    else if (method=="LSE") {

      QLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        Q<-sum((pG(x,alpha1,beta1)-i/(n+1))^2)
        return(Q)
      }
      QLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        Q<-sum((pW(y,alpha2,beta2)-i/(n+1))^2)
        return(Q)
      }
      QLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        Q<-sum((pL(z,alpha3,beta3)-i/(n+1))^2)
        return(Q)
      }
      lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
      lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
      lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)

      if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- lsex$par[1]
        beta1 <- lsex$par[2]
        alpha2 <- lsey$par[1]
        beta2 <- lsey$par[2]
        alpha3 <- lsez$par[1]
        beta3 <- lsez$par[2]
      }
      if (empirical==TRUE) {

        c1 <- base::seq(0.00001, 1, length.out = 100)
        c2 <- base::seq(0.00001, 1, length.out = 100)


        colorscalemp <- base::list(
          list(0, "rgb(255,255,255)"),
          list(0.2, "rgb(230,230,230)"),
          list(0.4, "rgb(200,200,200)"),
          list(0.6, "rgb(150,150,150)"),
          list(0.8, "rgb(100,100,100)"),
          list(1, "rgb(0,0,0)")
        )

        Fn <- stats::ecdf(y)
        vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
          base::pmax(result, 0)
        }))

        fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                              colorscale = colorscalemp)

        fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig2$x$layout$scene$xaxis$range <- c(0, 1)
        fig2$x$layout$scene$yaxis$range <- c(0, 1)
        fig2$x$layout$scene$zaxis$range <- c(0, 1)

        fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig2

      }
      else if (empirical==FALSE) {

        c1 <- seq(0.00001, 1, length.out = 100)
        c2 <- seq(0.00001, 1, length.out = 100)

        vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- pW(qL( c2, alpha3, beta3), alpha2, beta2) -
            pW(qG(1-c1, alpha1, beta1), alpha2, beta2)
          base::pmax(result, 0)
        }))
        fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                              colorscale = "Greys")

        fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig1$x$layout$scene$xaxis$range <- c(0, 1)
        fig1$x$layout$scene$yaxis$range <- c(0, 1)
        fig1$x$layout$scene$zaxis$range <- c(0, 1)

        fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig1
      }
    }
    else if (method=="WLSE") {


      QWLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(x,alpha1,beta1)-i/(n+1))^2)
        return(QW)
      }
      QWLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(y,alpha2,beta2)-i/(n+1))^2)
        return(QW)
      }
      QWLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pL(z,alpha3,beta3)-i/(n+1))^2)
        return(QW)
      }
      wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
      wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
      wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
      if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- wlsex$par[1]
        beta1 <- wlsex$par[2]
        alpha2 <- wlsey$par[1]
        beta2 <- wlsey$par[2]
        alpha3 <- wlsez$par[1]
        beta3 <- wlsez$par[2]
      }
      if (empirical==TRUE) {

        c1 <- base::seq(0.00001, 1, length.out = 100)
        c2 <- base::seq(0.00001, 1, length.out = 100)


        colorscalemp <- base::list(
          list(0, "rgb(255,255,255)"),
          list(0.2, "rgb(230,230,230)"),
          list(0.4, "rgb(200,200,200)"),
          list(0.6, "rgb(150,150,150)"),
          list(0.8, "rgb(100,100,100)"),
          list(1, "rgb(0,0,0)")
        )

        Fn <- stats::ecdf(y)
        vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
          base::pmax(result, 0)
        }))

        fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                              colorscale = colorscalemp)

        fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig2$x$layout$scene$xaxis$range <- c(0, 1)
        fig2$x$layout$scene$yaxis$range <- c(0, 1)
        fig2$x$layout$scene$zaxis$range <- c(0, 1)

        fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig2

      }
      else if (empirical==FALSE) {

        c1 <- seq(0.00001, 1, length.out = 100)
        c2 <- seq(0.00001, 1, length.out = 100)

        vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- pW(qL( c2, alpha3, beta3), alpha2, beta2) -
            pW(qG(1-c1, alpha1, beta1), alpha2, beta2)
          base::pmax(result, 0)
        }))
        fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                              colorscale = "Greys")

        fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig1$x$layout$scene$xaxis$range <- c(0, 1)
        fig1$x$layout$scene$yaxis$range <- c(0, 1)
        fig1$x$layout$scene$zaxis$range <- c(0, 1)

        fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig1
      }
    }
    else if (method=="TRUE") {
      x<-NULL
      y<-NULL
      z<-NULL
      init_param <- NULL
      empirical<-NULL
      alpha1<-true_param[[1]]
      beta1<-true_param[[2]]
      alpha2<-true_param[[3]]
      beta2<-true_param[[4]]
      alpha3<-true_param[[5]]
      beta3<-true_param[[6]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}

      c1 <- seq(0.00001, 1, length.out = 100)
      c2 <- seq(0.00001, 1, length.out = 100)
      vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
        result <- pW(qL( c2, alpha3, beta3), alpha2, beta2) -
          pW(qG(1-c1, alpha1, beta1), alpha2, beta2)
        base::pmax(result, 0)
      }))
      fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                            colorscale = "Greys")

      fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
      fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
      fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

      fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
      fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
      fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

      fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
      fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
      fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

      fig1$x$layout$scene$xaxis$range <- c(0, 1)
      fig1$x$layout$scene$yaxis$range <- c(0, 1)
      fig1$x$layout$scene$zaxis$range <- c(0, 1)

      fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                              tickvals = c(0, 0.5, 1))
      fig1


    }
  }
  else if (model=="GGW") {
    if (method=="MLE") {
      lfxyz<-function(par) {
        alpha1<-par[1]
        beta1<-par[2]
        alpha2<-par[3]
        beta2<-par[4]
        alpha3<-par[5]
        beta3<-par[6]
        t<-(-sum(log(dG(x,alpha1,beta1)))-sum(log(dG(y,alpha2,beta2)))-
              sum(log(dW(z,alpha3,beta3))))
        return(t)
      }
      mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                                lfxyz,hessian = TRUE)),silent=TRUE)

      if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- mlexyz$par[1]
        beta1 <- mlexyz$par[2]
        alpha2 <- mlexyz$par[3]
        beta2 <- mlexyz$par[4]
        alpha3<- mlexyz$par[5]
        beta3<- mlexyz$par[6]
      }

      if (empirical==TRUE) {

        c1 <- base::seq(0.00001, 1, length.out = 100)
        c2 <- base::seq(0.00001, 1, length.out = 100)


        colorscalemp <- base::list(
          list(0, "rgb(255,255,255)"),
          list(0.2, "rgb(230,230,230)"),
          list(0.4, "rgb(200,200,200)"),
          list(0.6, "rgb(150,150,150)"),
          list(0.8, "rgb(100,100,100)"),
          list(1, "rgb(0,0,0)")
        )

        Fn <- stats::ecdf(y)
        vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
          base::pmax(result, 0)
        }))

        fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                              colorscale = colorscalemp)

        fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig2$x$layout$scene$xaxis$range <- c(0, 1)
        fig2$x$layout$scene$yaxis$range <- c(0, 1)
        fig2$x$layout$scene$zaxis$range <- c(0, 1)

        fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig2

      }
      else if (empirical==FALSE) {

        c1 <- seq(0.00001, 1, length.out = 100)
        c2 <- seq(0.00001, 1, length.out = 100)

        vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- pG(qW( c2, alpha3, beta3), alpha2, beta2) -
            pG(qG(1-c1, alpha1, beta1), alpha2, beta2)
          base::pmax(result, 0)
        }))
        fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                              colorscale = "Greys")

        fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig1$x$layout$scene$xaxis$range <- c(0, 1)
        fig1$x$layout$scene$yaxis$range <- c(0, 1)
        fig1$x$layout$scene$zaxis$range <- c(0, 1)

        fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig1
      }
    }

    else if (method=="AD") {

      QADx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        adx<-sort(x,decreasing=TRUE)
        n<-NROW(x)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(x,alpha1,beta1))+
                                    log(1-pG(adx,alpha1,beta1))))
        return(AD)
      }
      QADy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        ady<-sort(y,decreasing=TRUE)
        n<-NROW(y)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pG(y,alpha2,beta2))+
                                    log(1-pG(ady,alpha2,beta2))))
        return(AD)
      }
      QADz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        adz<-sort(z,decreasing=TRUE)
        n<-NROW(z)
        i<-seq(1:n)
        AD<--n-(1/n)*sum((2*i-1)*(log(pW(z,alpha3,beta3))+
                                    log(1-pW(adz,alpha3,beta3))))
        return(AD)
      }
      adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
      adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
      adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)

      if (is.character(adex)|is.character(adey)|is.character(adez)) {
     stop("Optimization did not converge.Please check your initial parameters.")
      }
      else {
        alpha1 <- adex$par[1]
        beta1 <- adex$par[2]
        alpha2 <- adey$par[1]
        beta2 <- adey$par[2]
        alpha3<- adez$par[1]
        beta3<- adez$par[2]
      }
      if (empirical==TRUE) {

        c1 <- base::seq(0.00001, 1, length.out = 100)
        c2 <- base::seq(0.00001, 1, length.out = 100)


        colorscalemp <- base::list(
          list(0, "rgb(255,255,255)"),
          list(0.2, "rgb(230,230,230)"),
          list(0.4, "rgb(200,200,200)"),
          list(0.6, "rgb(150,150,150)"),
          list(0.8, "rgb(100,100,100)"),
          list(1, "rgb(0,0,0)")
        )

        Fn <- stats::ecdf(y)
        vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
          base::pmax(result, 0)
        }))

        fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                              colorscale = colorscalemp)

        fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig2$x$layout$scene$xaxis$range <- c(0, 1)
        fig2$x$layout$scene$yaxis$range <- c(0, 1)
        fig2$x$layout$scene$zaxis$range <- c(0, 1)

        fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig2

      }
      else if (empirical==FALSE) {

        c1 <- seq(0.00001, 1, length.out = 100)
        c2 <- seq(0.00001, 1, length.out = 100)

        vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- pG(qW( c2, alpha3, beta3), alpha2, beta2) -
            pG(qG(1-c1, alpha1, beta1), alpha2, beta2)
          base::pmax(result, 0)
        }))
        fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                              colorscale = "Greys")

        fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig1$x$layout$scene$xaxis$range <- c(0, 1)
        fig1$x$layout$scene$yaxis$range <- c(0, 1)
        fig1$x$layout$scene$zaxis$range <- c(0, 1)

        fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig1
      }

    }
    else if (method=="CvM") {

      QCVx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(x,alpha1,beta1)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pG(y,alpha2,beta2)-(i-0.5)/n)^2)
        return(CV)
      }
      QCVz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        CV<-1/(12*n)+sum((pW(z,alpha3,beta3)-(i-0.5)/n)^2)
        return(CV)
      }
      cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
      cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
      cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)

      if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- cvx$par[1]
        beta1 <- cvx$par[2]
        alpha2 <- cvy$par[1]
        beta2 <- cvy$par[2]
        alpha3 <- cvz$par[1]
        beta3 <- cvz$par[2]
      }
      if (empirical==TRUE) {

        c1 <- base::seq(0.00001, 1, length.out = 100)
        c2 <- base::seq(0.00001, 1, length.out = 100)


        colorscalemp <- base::list(
          list(0, "rgb(255,255,255)"),
          list(0.2, "rgb(230,230,230)"),
          list(0.4, "rgb(200,200,200)"),
          list(0.6, "rgb(150,150,150)"),
          list(0.8, "rgb(100,100,100)"),
          list(1, "rgb(0,0,0)")
        )

        Fn <- stats::ecdf(y)
        vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
          base::pmax(result, 0)
        }))

        fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                              colorscale = colorscalemp)

        fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig2$x$layout$scene$xaxis$range <- c(0, 1)
        fig2$x$layout$scene$yaxis$range <- c(0, 1)
        fig2$x$layout$scene$zaxis$range <- c(0, 1)

        fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig2

      }
      else if (empirical==FALSE) {

        c1 <- seq(0.00001, 1, length.out = 100)
        c2 <- seq(0.00001, 1, length.out = 100)

        vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- pG(qW( c2, alpha3, beta3), alpha2, beta2) -
            pG(qG(1-c1, alpha1, beta1), alpha2, beta2)
          base::pmax(result, 0)
        }))
        fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                              colorscale = "Greys")

        fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig1$x$layout$scene$xaxis$range <- c(0, 1)
        fig1$x$layout$scene$yaxis$range <- c(0, 1)
        fig1$x$layout$scene$zaxis$range <- c(0, 1)

        fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig1
      }
    }
    else if (method=="LSE") {

      QLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        Q<-sum((pG(x,alpha1,beta1)-i/(n+1))^2)
        return(Q)
      }
      QLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        Q<-sum((pG(y,alpha2,beta2)-i/(n+1))^2)
        return(Q)
      }
      QLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        Q<-sum((pW(z,alpha3,beta3)-i/(n+1))^2)
        return(Q)
      }
      lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
      lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
      lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)

      if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- lsex$par[1]
        beta1 <- lsex$par[2]
        alpha2 <- lsey$par[1]
        beta2 <- lsey$par[2]
        alpha3 <- lsez$par[1]
        beta3 <- lsez$par[2]
      }
      if (empirical==TRUE) {

        c1 <- base::seq(0.00001, 1, length.out = 100)
        c2 <- base::seq(0.00001, 1, length.out = 100)


        colorscalemp <- base::list(
          list(0, "rgb(255,255,255)"),
          list(0.2, "rgb(230,230,230)"),
          list(0.4, "rgb(200,200,200)"),
          list(0.6, "rgb(150,150,150)"),
          list(0.8, "rgb(100,100,100)"),
          list(1, "rgb(0,0,0)")
        )

        Fn <- stats::ecdf(y)
        vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
          base::pmax(result, 0)
        }))

        fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                              colorscale = colorscalemp)

        fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig2$x$layout$scene$xaxis$range <- c(0, 1)
        fig2$x$layout$scene$yaxis$range <- c(0, 1)
        fig2$x$layout$scene$zaxis$range <- c(0, 1)

        fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig2

      }
      else if (empirical==FALSE) {

        c1 <- seq(0.00001, 1, length.out = 100)
        c2 <- seq(0.00001, 1, length.out = 100)

        vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- pG(qW( c2, alpha3, beta3), alpha2, beta2) -
            pG(qG(1-c1, alpha1, beta1), alpha2, beta2)
          base::pmax(result, 0)
        }))
        fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                              colorscale = "Greys")

        fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig1$x$layout$scene$xaxis$range <- c(0, 1)
        fig1$x$layout$scene$yaxis$range <- c(0, 1)
        fig1$x$layout$scene$zaxis$range <- c(0, 1)

        fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig1
      }
    }
    else if (method=="WLSE") {


      QWLSEx<-function(par,x){
        alpha1<-par[1]
        beta1<-par[2]
        x<-sort(x)
        n<-NROW(x)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(x,alpha1,beta1)-i/(n+1))^2)
        return(QW)
      }
      QWLSEy<-function(par,y){
        alpha2<-par[1]
        beta2<-par[2]
        y<-sort(y)
        n<-NROW(y)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(y,alpha2,beta2)-i/(n+1))^2)
        return(QW)
      }
      QWLSEz<-function(par,z){
        alpha3<-par[1]
        beta3<-par[2]
        z<-sort(z)
        n<-NROW(z)
        i<-seq(1:n)
        QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(z,alpha3,beta3)-i/(n+1))^2)
        return(QW)
      }
      wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
      wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
      wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
      if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
      } else {
        alpha1 <- wlsex$par[1]
        beta1 <- wlsex$par[2]
        alpha2 <- wlsey$par[1]
        beta2 <- wlsey$par[2]
        alpha3 <- wlsez$par[1]
        beta3 <- wlsez$par[2]
      }
      if (empirical==TRUE) {

        c1 <- base::seq(0.00001, 1, length.out = 100)
        c2 <- base::seq(0.00001, 1, length.out = 100)


        colorscalemp <- base::list(
          list(0, "rgb(255,255,255)"),
          list(0.2, "rgb(230,230,230)"),
          list(0.4, "rgb(200,200,200)"),
          list(0.6, "rgb(150,150,150)"),
          list(0.8, "rgb(100,100,100)"),
          list(1, "rgb(0,0,0)")
        )

        Fn <- stats::ecdf(y)
        vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
          base::pmax(result, 0)
        }))

        fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                              colorscale = colorscalemp)

        fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig2$x$layout$scene$xaxis$range <- c(0, 1)
        fig2$x$layout$scene$yaxis$range <- c(0, 1)
        fig2$x$layout$scene$zaxis$range <- c(0, 1)

        fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig2

      }
      else if (empirical==FALSE) {

        c1 <- seq(0.00001, 1, length.out = 100)
        c2 <- seq(0.00001, 1, length.out = 100)

        vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- pG(qW( c2, alpha3, beta3), alpha2, beta2) -
            pG(qG(1-c1, alpha1, beta1), alpha2, beta2)
          base::pmax(result, 0)
        }))
        fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                              colorscale = "Greys")

        fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig1$x$layout$scene$xaxis$range <- c(0, 1)
        fig1$x$layout$scene$yaxis$range <- c(0, 1)
        fig1$x$layout$scene$zaxis$range <- c(0, 1)

        fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig1
      }
    }
    else if (method=="TRUE") {
      x<-NULL
      y<-NULL
      z<-NULL
      init_param <- NULL
      empirical<- NULL
      alpha1<-true_param[[1]]
      beta1<-true_param[[2]]
      alpha2<-true_param[[3]]
      beta2<-true_param[[4]]
      alpha3<-true_param[[5]]
      beta3<-true_param[[6]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
   if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
 if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
  if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
  if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}


      c1 <- seq(0.00001, 1, length.out = 100)
      c2 <- seq(0.00001, 1, length.out = 100)

      vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
        result <- pG(qW( c2, alpha3, beta3), alpha2, beta2) -
          pG(qG(1-c1, alpha1, beta1), alpha2, beta2)
        base::pmax(result, 0)
      }))
      fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                            colorscale = "Greys")

      fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
      fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
      fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

      fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
      fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
      fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

      fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
      fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
      fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

      fig1$x$layout$scene$xaxis$range <- c(0, 1)
      fig1$x$layout$scene$yaxis$range <- c(0, 1)
      fig1$x$layout$scene$zaxis$range <- c(0, 1)

      fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                              tickvals = c(0, 0.5, 1))
      fig1

    }
  }

    else if (model=="WGW") {
      if (method=="MLE") {
        lfxyz<-function(par) {
          alpha1<-par[1]
          beta1<-par[2]
          alpha2<-par[3]
          beta2<-par[4]
          alpha3<-par[5]
          beta3<-par[6]
          t<-(-sum(log(dW(x,alpha1,beta1)))-sum(log(dG(y,alpha2,beta2)))-
                sum(log(dW(z,alpha3,beta3))))
          return(t)
        }
        mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                                  lfxyz,hessian = TRUE)),silent=TRUE)

        if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        } else {
          alpha1 <- mlexyz$par[1]
          beta1 <- mlexyz$par[2]
          alpha2 <- mlexyz$par[3]
          beta2 <- mlexyz$par[4]
          alpha3<- mlexyz$par[5]
          beta3<- mlexyz$par[6]
        }

        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pG(qW( c2, alpha3, beta3), alpha2, beta2) -
              pG(qW(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }
      }

      else if (method=="AD") {

        QADx<-function(par,x){
          alpha1<-par[1]
          beta1<-par[2]
          x<-sort(x)
          adx<-sort(x,decreasing=TRUE)
          n<-NROW(x)
          i<-seq(1:n)
          AD<--n-(1/n)*sum((2*i-1)*(log(pW(x,alpha1,beta1))+
                                      log(1-pW(adx,alpha1,beta1))))
          return(AD)
        }
        QADy<-function(par,y){
          alpha2<-par[1]
          beta2<-par[2]
          y<-sort(y)
          ady<-sort(y,decreasing=TRUE)
          n<-NROW(y)
          i<-seq(1:n)
          AD<--n-(1/n)*sum((2*i-1)*(log(pG(y,alpha2,beta2))+
                                      log(1-pG(ady,alpha2,beta2))))
          return(AD)
        }
        QADz<-function(par,z){
          alpha3<-par[1]
          beta3<-par[2]
          z<-sort(z)
          adz<-sort(z,decreasing=TRUE)
          n<-NROW(z)
          i<-seq(1:n)
          AD<--n-(1/n)*sum((2*i-1)*(log(pW(z,alpha3,beta3))+
                                      log(1-pW(adz,alpha3,beta3))))
          return(AD)
        }
        adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
        adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
        adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)

        if (is.character(adex)|is.character(adey)|is.character(adez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        }
        else {
          alpha1 <- adex$par[1]
          beta1 <- adex$par[2]
          alpha2 <- adey$par[1]
          beta2 <- adey$par[2]
          alpha3<- adez$par[1]
          beta3<- adez$par[2]
        }
        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pG(qW( c2, alpha3, beta3), alpha2, beta2) -
              pG(qW(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }

      }
      else if (method=="CvM") {

        QCVx<-function(par,x){
          alpha1<-par[1]
          beta1<-par[2]
          x<-sort(x)
          n<-NROW(x)
          i<-seq(1:n)
          CV<-1/(12*n)+sum((pW(x,alpha1,beta1)-(i-0.5)/n)^2)
          return(CV)
        }
        QCVy<-function(par,y){
          alpha2<-par[1]
          beta2<-par[2]
          y<-sort(y)
          n<-NROW(y)
          i<-seq(1:n)
          CV<-1/(12*n)+sum((pG(y,alpha2,beta2)-(i-0.5)/n)^2)
          return(CV)
        }
        QCVz<-function(par,z){
          alpha3<-par[1]
          beta3<-par[2]
          z<-sort(z)
          n<-NROW(z)
          i<-seq(1:n)
          CV<-1/(12*n)+sum((pW(z,alpha3,beta3)-(i-0.5)/n)^2)
          return(CV)
        }
        cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
        cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
        cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)

        if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        } else {
          alpha1 <- cvx$par[1]
          beta1 <- cvx$par[2]
          alpha2 <- cvy$par[1]
          beta2 <- cvy$par[2]
          alpha3 <- cvz$par[1]
          beta3 <- cvz$par[2]
        }
        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pG(qW( c2, alpha3, beta3), alpha2, beta2) -
              pG(qW(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }
      }
      else if (method=="LSE") {

        QLSEx<-function(par,x){
          alpha1<-par[1]
          beta1<-par[2]
          x<-sort(x)
          n<-NROW(x)
          i<-seq(1:n)
          Q<-sum((pW(x,alpha1,beta1)-i/(n+1))^2)
          return(Q)
        }
        QLSEy<-function(par,y){
          alpha2<-par[1]
          beta2<-par[2]
          y<-sort(y)
          n<-NROW(y)
          i<-seq(1:n)
          Q<-sum((pG(y,alpha2,beta2)-i/(n+1))^2)
          return(Q)
        }
        QLSEz<-function(par,z){
          alpha3<-par[1]
          beta3<-par[2]
          z<-sort(z)
          n<-NROW(z)
          i<-seq(1:n)
          Q<-sum((pW(z,alpha3,beta3)-i/(n+1))^2)
          return(Q)
        }
        lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
        lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
        lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)

        if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        } else {
          alpha1 <- lsex$par[1]
          beta1 <- lsex$par[2]
          alpha2 <- lsey$par[1]
          beta2 <- lsey$par[2]
          alpha3 <- lsez$par[1]
          beta3 <- lsez$par[2]
        }
        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pG(qW( c2, alpha3, beta3), alpha2, beta2) -
              pG(qW(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }
      }
      else if (method=="WLSE") {


        QWLSEx<-function(par,x){
          alpha1<-par[1]
          beta1<-par[2]
          x<-sort(x)
          n<-NROW(x)
          i<-seq(1:n)
          QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(x,alpha1,beta1)-i/(n+1))^2)
          return(QW)
        }
        QWLSEy<-function(par,y){
          alpha2<-par[1]
          beta2<-par[2]
          y<-sort(y)
          n<-NROW(y)
          i<-seq(1:n)
          QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(y,alpha2,beta2)-i/(n+1))^2)
          return(QW)
        }
        QWLSEz<-function(par,z){
          alpha3<-par[1]
          beta3<-par[2]
          z<-sort(z)
          n<-NROW(z)
          i<-seq(1:n)
          QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(z,alpha3,beta3)-i/(n+1))^2)
          return(QW)
        }
        wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
        wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
        wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
        if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        } else {
          alpha1 <- wlsex$par[1]
          beta1 <- wlsex$par[2]
          alpha2 <- wlsey$par[1]
          beta2 <- wlsey$par[2]
          alpha3 <- wlsez$par[1]
          beta3 <- wlsez$par[2]
        }
        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pG(qW( c2, alpha3, beta3), alpha2, beta2) -
              pG(qW(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }
      }
      else if (method=="TRUE") {
        x<-NULL
        y<-NULL
        z<-NULL
        init_param <- NULL
        empirical<-NULL
        alpha1<-true_param[[1]]
        beta1<-true_param[[2]]
        alpha2<-true_param[[3]]
        beta2<-true_param[[4]]
        alpha3<-true_param[[5]]
        beta3<-true_param[[6]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
  if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
  if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
  if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}


        c1 <- seq(0.00001, 1, length.out = 100)
        c2 <- seq(0.00001, 1, length.out = 100)

        vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- pG(qW( c2, alpha3, beta3), alpha2, beta2) -
            pG(qW(1-c1, alpha1, beta1), alpha2, beta2)
          base::pmax(result, 0)
        }))
        fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                              colorscale = "Greys")

        fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig1$x$layout$scene$xaxis$range <- c(0, 1)
        fig1$x$layout$scene$yaxis$range <- c(0, 1)
        fig1$x$layout$scene$zaxis$range <- c(0, 1)

        fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig1

      }
    }
    else if(model=="WWW") {
      if (method=="MLE") {
        lfxyz<-function(par) {
          alpha1<-par[1]
          beta1<-par[2]
          alpha2<-par[3]
          beta2<-par[4]
          alpha3<-par[5]
          beta3<-par[6]
          t<-(-sum(log(dW(x,alpha1,beta1)))-sum(log(dW(y,alpha2,beta2)))-
                sum(log(dW(z,alpha3,beta3))))
          return(t)
        }
        mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                                  lfxyz,hessian = TRUE)),silent=TRUE)

        if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        } else {
          alpha1 <- mlexyz$par[1]
          beta1 <- mlexyz$par[2]
          alpha2 <- mlexyz$par[3]
          beta2 <- mlexyz$par[4]
          alpha3<- mlexyz$par[5]
          beta3<- mlexyz$par[6]
        }

        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pW(qW( c2, alpha3, beta3), alpha2, beta2) -
              pW(qW(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }
      }

      else if (method=="AD") {

        QADx<-function(par,x){
          alpha1<-par[1]
          beta1<-par[2]
          x<-sort(x)
          adx<-sort(x,decreasing=TRUE)
          n<-NROW(x)
          i<-seq(1:n)
          AD<--n-(1/n)*sum((2*i-1)*(log(pW(x,alpha1,beta1))+
                                      log(1-pW(adx,alpha1,beta1))))
          return(AD)
        }
        QADy<-function(par,y){
          alpha2<-par[1]
          beta2<-par[2]
          y<-sort(y)
          ady<-sort(y,decreasing=TRUE)
          n<-NROW(y)
          i<-seq(1:n)
          AD<--n-(1/n)*sum((2*i-1)*(log(pW(y,alpha2,beta2))+
                                      log(1-pW(ady,alpha2,beta2))))
          return(AD)
        }
        QADz<-function(par,z){
          alpha3<-par[1]
          beta3<-par[2]
          z<-sort(z)
          adz<-sort(z,decreasing=TRUE)
          n<-NROW(z)
          i<-seq(1:n)
          AD<--n-(1/n)*sum((2*i-1)*(log(pW(z,alpha3,beta3))+
                                      log(1-pW(adz,alpha3,beta3))))
          return(AD)
        }
        adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
        adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
        adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)

        if (is.character(adex)|is.character(adey)|is.character(adez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        }
        else {
          alpha1 <- adex$par[1]
          beta1 <- adex$par[2]
          alpha2 <- adey$par[1]
          beta2 <- adey$par[2]
          alpha3<- adez$par[1]
          beta3<- adez$par[2]
        }
        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pW(qW( c2, alpha3, beta3), alpha2, beta2) -
              pW(qW(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }

      }
      else if (method=="CvM") {

        QCVx<-function(par,x){
          alpha1<-par[1]
          beta1<-par[2]
          x<-sort(x)
          n<-NROW(x)
          i<-seq(1:n)
          CV<-1/(12*n)+sum((pW(x,alpha1,beta1)-(i-0.5)/n)^2)
          return(CV)
        }
        QCVy<-function(par,y){
          alpha2<-par[1]
          beta2<-par[2]
          y<-sort(y)
          n<-NROW(y)
          i<-seq(1:n)
          CV<-1/(12*n)+sum((pW(y,alpha2,beta2)-(i-0.5)/n)^2)
          return(CV)
        }
        QCVz<-function(par,z){
          alpha3<-par[1]
          beta3<-par[2]
          z<-sort(z)
          n<-NROW(z)
          i<-seq(1:n)
          CV<-1/(12*n)+sum((pW(z,alpha3,beta3)-(i-0.5)/n)^2)
          return(CV)
        }
        cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
        cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
        cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)

        if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        } else {
          alpha1 <- cvx$par[1]
          beta1 <- cvx$par[2]
          alpha2 <- cvy$par[1]
          beta2 <- cvy$par[2]
          alpha3 <- cvz$par[1]
          beta3 <- cvz$par[2]
        }
        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pW(qW( c2, alpha3, beta3), alpha2, beta2) -
              pW(qW(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }
      }
      else if (method=="LSE") {

        QLSEx<-function(par,x){
          alpha1<-par[1]
          beta1<-par[2]
          x<-sort(x)
          n<-NROW(x)
          i<-seq(1:n)
          Q<-sum((pW(x,alpha1,beta1)-i/(n+1))^2)
          return(Q)
        }
        QLSEy<-function(par,y){
          alpha2<-par[1]
          beta2<-par[2]
          y<-sort(y)
          n<-NROW(y)
          i<-seq(1:n)
          Q<-sum((pW(y,alpha2,beta2)-i/(n+1))^2)
          return(Q)
        }
        QLSEz<-function(par,z){
          alpha3<-par[1]
          beta3<-par[2]
          z<-sort(z)
          n<-NROW(z)
          i<-seq(1:n)
          Q<-sum((pW(z,alpha3,beta3)-i/(n+1))^2)
          return(Q)
        }
        lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
        lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
        lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)

        if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        } else {
          alpha1 <- lsex$par[1]
          beta1 <- lsex$par[2]
          alpha2 <- lsey$par[1]
          beta2 <- lsey$par[2]
          alpha3 <- lsez$par[1]
          beta3 <- lsez$par[2]
        }
        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pW(qW( c2, alpha3, beta3), alpha2, beta2) -
              pW(qW(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }
      }
      else if (method=="WLSE") {


        QWLSEx<-function(par,x){
          alpha1<-par[1]
          beta1<-par[2]
          x<-sort(x)
          n<-NROW(x)
          i<-seq(1:n)
          QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(x,alpha1,beta1)-i/(n+1))^2)
          return(QW)
        }
        QWLSEy<-function(par,y){
          alpha2<-par[1]
          beta2<-par[2]
          y<-sort(y)
          n<-NROW(y)
          i<-seq(1:n)
          QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(y,alpha2,beta2)-i/(n+1))^2)
          return(QW)
        }
        QWLSEz<-function(par,z){
          alpha3<-par[1]
          beta3<-par[2]
          z<-sort(z)
          n<-NROW(z)
          i<-seq(1:n)
          QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pW(z,alpha3,beta3)-i/(n+1))^2)
          return(QW)
        }
        wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
        wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
        wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
        if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        } else {
          alpha1 <- wlsex$par[1]
          beta1 <- wlsex$par[2]
          alpha2 <- wlsey$par[1]
          beta2 <- wlsey$par[2]
          alpha3 <- wlsez$par[1]
          beta3 <- wlsez$par[2]
        }
        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pW(qW( c2, alpha3, beta3), alpha2, beta2) -
              pW(qW(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }
      }
      else if (method=="TRUE") {
        x<-NULL
        y<-NULL
        z<-NULL
        init_param <- NULL
        empirical<-NULL
        alpha1<-true_param[[1]]
        beta1<-true_param[[2]]
        alpha2<-true_param[[3]]
        beta2<-true_param[[4]]
        alpha3<-true_param[[5]]
        beta3<-true_param[[6]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
  if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
  if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}


        c1 <- seq(0.00001, 1, length.out = 100)
        c2 <- seq(0.00001, 1, length.out = 100)

        vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- pW(qW( c2, alpha3, beta3), alpha2, beta2) -
            pW(qW(1-c1, alpha1, beta1), alpha2, beta2)
          base::pmax(result, 0)
        }))
        fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                              colorscale = "Greys")

        fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig1$x$layout$scene$xaxis$range <- c(0, 1)
        fig1$x$layout$scene$yaxis$range <- c(0, 1)
        fig1$x$layout$scene$zaxis$range <- c(0, 1)

        fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig1

      }
    }
    else if (model=="GGG") {
      if (method=="MLE") {
        lfxyz<-function(par) {
          alpha1<-par[1]
          beta1<-par[2]
          alpha2<-par[3]
          beta2<-par[4]
          alpha3<-par[5]
          beta3<-par[6]
          t<-(-sum(log(dG(x,alpha1,beta1)))-sum(log(dG(y,alpha2,beta2)))-
                sum(log(dG(z,alpha3,beta3))))
          return(t)
        }
        mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                                  lfxyz,hessian = TRUE)),silent=TRUE)

        if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        } else {
          alpha1 <- mlexyz$par[1]
          beta1 <- mlexyz$par[2]
          alpha2 <- mlexyz$par[3]
          beta2 <- mlexyz$par[4]
          alpha3<- mlexyz$par[5]
          beta3<- mlexyz$par[6]
        }

        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pG(qG( c2, alpha3, beta3), alpha2, beta2) -
              pG(qG(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }
      }

      else if (method=="AD") {

        QADx<-function(par,x){
          alpha1<-par[1]
          beta1<-par[2]
          x<-sort(x)
          adx<-sort(x,decreasing=TRUE)
          n<-NROW(x)
          i<-seq(1:n)
          AD<--n-(1/n)*sum((2*i-1)*(log(pG(x,alpha1,beta1))+
                                      log(1-pG(adx,alpha1,beta1))))
          return(AD)
        }
        QADy<-function(par,y){
          alpha2<-par[1]
          beta2<-par[2]
          y<-sort(y)
          ady<-sort(y,decreasing=TRUE)
          n<-NROW(y)
          i<-seq(1:n)
          AD<--n-(1/n)*sum((2*i-1)*(log(pG(y,alpha2,beta2))+
                                      log(1-pG(ady,alpha2,beta2))))
          return(AD)
        }
        QADz<-function(par,z){
          alpha3<-par[1]
          beta3<-par[2]
          z<-sort(z)
          adz<-sort(z,decreasing=TRUE)
          n<-NROW(z)
          i<-seq(1:n)
          AD<--n-(1/n)*sum((2*i-1)*(log(pG(z,alpha3,beta3))+
                                      log(1-pG(adz,alpha3,beta3))))
          return(AD)
        }
        adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
        adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
        adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)

        if (is.character(adex)|is.character(adey)|is.character(adez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        }
        else {
          alpha1 <- adex$par[1]
          beta1 <- adex$par[2]
          alpha2 <- adey$par[1]
          beta2 <- adey$par[2]
          alpha3<- adez$par[1]
          beta3<- adez$par[2]
        }
        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pG(qG( c2, alpha3, beta3), alpha2, beta2) -
              pG(qG(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }

      }
      else if (method=="CvM") {

        QCVx<-function(par,x){
          alpha1<-par[1]
          beta1<-par[2]
          x<-sort(x)
          n<-NROW(x)
          i<-seq(1:n)
          CV<-1/(12*n)+sum((pG(x,alpha1,beta1)-(i-0.5)/n)^2)
          return(CV)
        }
        QCVy<-function(par,y){
          alpha2<-par[1]
          beta2<-par[2]
          y<-sort(y)
          n<-NROW(y)
          i<-seq(1:n)
          CV<-1/(12*n)+sum((pG(y,alpha2,beta2)-(i-0.5)/n)^2)
          return(CV)
        }
        QCVz<-function(par,z){
          alpha3<-par[1]
          beta3<-par[2]
          z<-sort(z)
          n<-NROW(z)
          i<-seq(1:n)
          CV<-1/(12*n)+sum((pG(z,alpha3,beta3)-(i-0.5)/n)^2)
          return(CV)
        }
        cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
        cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
        cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)

        if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        } else {
          alpha1 <- cvx$par[1]
          beta1 <- cvx$par[2]
          alpha2 <- cvy$par[1]
          beta2 <- cvy$par[2]
          alpha3 <- cvz$par[1]
          beta3 <- cvz$par[2]
        }
        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pG(qG( c2, alpha3, beta3), alpha2, beta2) -
              pG(qG(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }
      }
      else if (method=="LSE") {

        QLSEx<-function(par,x){
          alpha1<-par[1]
          beta1<-par[2]
          x<-sort(x)
          n<-NROW(x)
          i<-seq(1:n)
          Q<-sum((pG(x,alpha1,beta1)-i/(n+1))^2)
          return(Q)
        }
        QLSEy<-function(par,y){
          alpha2<-par[1]
          beta2<-par[2]
          y<-sort(y)
          n<-NROW(y)
          i<-seq(1:n)
          Q<-sum((pG(y,alpha2,beta2)-i/(n+1))^2)
          return(Q)
        }
        QLSEz<-function(par,z){
          alpha3<-par[1]
          beta3<-par[2]
          z<-sort(z)
          n<-NROW(z)
          i<-seq(1:n)
          Q<-sum((pG(z,alpha3,beta3)-i/(n+1))^2)
          return(Q)
        }
        lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
        lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
        lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)

        if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        } else {
          alpha1 <- lsex$par[1]
          beta1 <- lsex$par[2]
          alpha2 <- lsey$par[1]
          beta2 <- lsey$par[2]
          alpha3 <- lsez$par[1]
          beta3 <- lsez$par[2]
        }
        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pG(qG( c2, alpha3, beta3), alpha2, beta2) -
              pG(qG(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }
      }
      else if (method=="WLSE") {


        QWLSEx<-function(par,x){
          alpha1<-par[1]
          beta1<-par[2]
          x<-sort(x)
          n<-NROW(x)
          i<-seq(1:n)
          QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(x,alpha1,beta1)-i/(n+1))^2)
          return(QW)
        }
        QWLSEy<-function(par,y){
          alpha2<-par[1]
          beta2<-par[2]
          y<-sort(y)
          n<-NROW(y)
          i<-seq(1:n)
          QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(y,alpha2,beta2)-i/(n+1))^2)
          return(QW)
        }
        QWLSEz<-function(par,z){
          alpha3<-par[1]
          beta3<-par[2]
          z<-sort(z)
          n<-NROW(z)
          i<-seq(1:n)
          QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pG(z,alpha3,beta3)-i/(n+1))^2)
          return(QW)
        }
        wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
        wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
        wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
        if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        } else {
          alpha1 <- wlsex$par[1]
          beta1 <- wlsex$par[2]
          alpha2 <- wlsey$par[1]
          beta2 <- wlsey$par[2]
          alpha3 <- wlsez$par[1]
          beta3 <- wlsez$par[2]
        }
        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pG(qG( c2, alpha3, beta3), alpha2, beta2) -
              pG(qG(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }
      }
      else if (method=="TRUE") {
        x<-NULL
        y<-NULL
        z<-NULL
        init_param <- NULL
        empirical<-NULL
        alpha1<-true_param[[1]]
        beta1<-true_param[[2]]
        alpha2<-true_param[[3]]
        beta2<-true_param[[4]]
        alpha3<-true_param[[5]]
        beta3<-true_param[[6]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
  if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}


        c1 <- seq(0.00001, 1, length.out = 100)
        c2 <- seq(0.00001, 1, length.out = 100)

        vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- pG(qG( c2, alpha3, beta3), alpha2, beta2) -
            pG(qG(1-c1, alpha1, beta1), alpha2, beta2)
          base::pmax(result, 0)
        }))
        fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                              colorscale = "Greys")

        fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig1$x$layout$scene$xaxis$range <- c(0, 1)
        fig1$x$layout$scene$yaxis$range <- c(0, 1)
        fig1$x$layout$scene$zaxis$range <- c(0, 1)

        fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig1

      }
    }
    else if (model=="LLL") {
      if (method=="MLE") {
        lfxyz<-function(par) {
          alpha1<-par[1]
          beta1<-par[2]
          alpha2<-par[3]
          beta2<-par[4]
          alpha3<-par[5]
          beta3<-par[6]
          t<-(-sum(log(dL(x,alpha1,beta1)))-sum(log(dL(y,alpha2,beta2)))-
                sum(log(dL(z,alpha3,beta3))))
          return(t)
        }
        mlexyz<-try((stats::optim(c(alpha1,beta1,alpha2,beta2,alpha3,beta3),
                                  lfxyz,hessian = TRUE)),silent=TRUE)

        if (is.character(mlexyz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        } else {
          alpha1 <- mlexyz$par[1]
          beta1 <- mlexyz$par[2]
          alpha2 <- mlexyz$par[3]
          beta2 <- mlexyz$par[4]
          alpha3<- mlexyz$par[5]
          beta3<- mlexyz$par[6]
        }

        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pL(qL( c2, alpha3, beta3), alpha2, beta2) -
              pL(qL(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }
      }

      else if (method=="AD") {

        QADx<-function(par,x){
          alpha1<-par[1]
          beta1<-par[2]
          x<-sort(x)
          adx<-sort(x,decreasing=TRUE)
          n<-NROW(x)
          i<-seq(1:n)
          AD<--n-(1/n)*sum((2*i-1)*(log(pL(x,alpha1,beta1))+
                                      log(1-pL(adx,alpha1,beta1))))
          return(AD)
        }
        QADy<-function(par,y){
          alpha2<-par[1]
          beta2<-par[2]
          y<-sort(y)
          ady<-sort(y,decreasing=TRUE)
          n<-NROW(y)
          i<-seq(1:n)
          AD<--n-(1/n)*sum((2*i-1)*(log(pL(y,alpha2,beta2))+
                                      log(1-pL(ady,alpha2,beta2))))
          return(AD)
        }
        QADz<-function(par,z){
          alpha3<-par[1]
          beta3<-par[2]
          z<-sort(z)
          adz<-sort(z,decreasing=TRUE)
          n<-NROW(z)
          i<-seq(1:n)
          AD<--n-(1/n)*sum((2*i-1)*(log(pL(z,alpha3,beta3))+
                                      log(1-pL(adz,alpha3,beta3))))
          return(AD)
        }
        adex<-try(stats::optim(c(alpha1,beta1),QADx,x=x),silent=TRUE)
        adey<-try(stats::optim(c(alpha2,beta2),QADy,y=y),silent=TRUE)
        adez<-try(stats::optim(c(alpha3,beta3),QADz,z=z),silent=TRUE)

        if (is.character(adex)|is.character(adey)|is.character(adez)) {
     stop("Optimization did not converge.Please check your initial parameters.")
        }
        else {
          alpha1 <- adex$par[1]
          beta1 <- adex$par[2]
          alpha2 <- adey$par[1]
          beta2 <- adey$par[2]
          alpha3<- adez$par[1]
          beta3<- adez$par[2]
        }
        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pL(qL( c2, alpha3, beta3), alpha2, beta2) -
              pL(qL(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }

      }
      else if (method=="CvM") {

        QCVx<-function(par,x){
          alpha1<-par[1]
          beta1<-par[2]
          x<-sort(x)
          n<-NROW(x)
          i<-seq(1:n)
          CV<-1/(12*n)+sum((pL(x,alpha1,beta1)-(i-0.5)/n)^2)
          return(CV)
        }
        QCVy<-function(par,y){
          alpha2<-par[1]
          beta2<-par[2]
          y<-sort(y)
          n<-NROW(y)
          i<-seq(1:n)
          CV<-1/(12*n)+sum((pL(y,alpha2,beta2)-(i-0.5)/n)^2)
          return(CV)
        }
        QCVz<-function(par,z){
          alpha3<-par[1]
          beta3<-par[2]
          z<-sort(z)
          n<-NROW(z)
          i<-seq(1:n)
          CV<-1/(12*n)+sum((pL(z,alpha3,beta3)-(i-0.5)/n)^2)
          return(CV)
        }
        cvx<-try(stats::optim(c(alpha1,beta1),QCVx,x=x),silent=TRUE)
        cvy<-try(stats::optim(c(alpha2,beta2),QCVy,y=y),silent=TRUE)
        cvz<-try(stats::optim(c(alpha3,beta3),QCVz,z=z),silent=TRUE)

        if (is.character(cvx)|is.character(cvy)|is.character(cvz)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        } else {
          alpha1 <- cvx$par[1]
          beta1 <- cvx$par[2]
          alpha2 <- cvy$par[1]
          beta2 <- cvy$par[2]
          alpha3 <- cvz$par[1]
          beta3 <- cvz$par[2]
        }
        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pL(qL( c2, alpha3, beta3), alpha2, beta2) -
              pL(qL(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }
      }
      else if (method=="LSE") {

        QLSEx<-function(par,x){
          alpha1<-par[1]
          beta1<-par[2]
          x<-sort(x)
          n<-NROW(x)
          i<-seq(1:n)
          Q<-sum((pL(x,alpha1,beta1)-i/(n+1))^2)
          return(Q)
        }
        QLSEy<-function(par,y){
          alpha2<-par[1]
          beta2<-par[2]
          y<-sort(y)
          n<-NROW(y)
          i<-seq(1:n)
          Q<-sum((pL(y,alpha2,beta2)-i/(n+1))^2)
          return(Q)
        }
        QLSEz<-function(par,z){
          alpha3<-par[1]
          beta3<-par[2]
          z<-sort(z)
          n<-NROW(z)
          i<-seq(1:n)
          Q<-sum((pL(z,alpha3,beta3)-i/(n+1))^2)
          return(Q)
        }
        lsex<-try(stats::optim(c(alpha1,beta1),QLSEx,x=x),silent=TRUE)
        lsey<-try(stats::optim(c(alpha2,beta2),QLSEy,y=y),silent=TRUE)
        lsez<-try(stats::optim(c(alpha3,beta3),QLSEz,z=z),silent=TRUE)

        if (is.character(lsex)|is.character(lsey)|is.character(lsez)) {
     stop("Optimization did not converge.Please check your initial parameters.")
        } else {
          alpha1 <- lsex$par[1]
          beta1 <- lsex$par[2]
          alpha2 <- lsey$par[1]
          beta2 <- lsey$par[2]
          alpha3 <- lsez$par[1]
          beta3 <- lsez$par[2]
        }
        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pL(qL( c2, alpha3, beta3), alpha2, beta2) -
              pL(qL(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }
      }
      else if (method=="WLSE") {


        QWLSEx<-function(par,x){
          alpha1<-par[1]
          beta1<-par[2]
          x<-sort(x)
          n<-NROW(x)
          i<-seq(1:n)
          QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pL(x,alpha1,beta1)-i/(n+1))^2)
          return(QW)
        }
        QWLSEy<-function(par,y){
          alpha2<-par[1]
          beta2<-par[2]
          y<-sort(y)
          n<-NROW(y)
          i<-seq(1:n)
          QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pL(y,alpha2,beta2)-i/(n+1))^2)
          return(QW)
        }
        QWLSEz<-function(par,z){
          alpha3<-par[1]
          beta3<-par[2]
          z<-sort(z)
          n<-NROW(z)
          i<-seq(1:n)
          QW<-sum(((n+1)^2*(n+2)/(i*(n-i+1)))*(pL(z,alpha3,beta3)-i/(n+1))^2)
          return(QW)
        }
        wlsex<-try(stats::optim(c(alpha1,beta1),QWLSEx,x=x),silent=TRUE)
        wlsey<-try(stats::optim(c(alpha2,beta2),QWLSEy,y=y),silent=TRUE)
        wlsez<-try(stats::optim(c(alpha3,beta3),QWLSEz,z=z),silent=TRUE)
        if (is.character(wlsex)|is.character(wlsey)|is.character(wlsez)) {
    stop("Optimization did not converge.Please check your initial parameters.")
        } else {
          alpha1 <- wlsex$par[1]
          beta1 <- wlsex$par[2]
          alpha2 <- wlsey$par[1]
          beta2 <- wlsey$par[2]
          alpha3 <- wlsez$par[1]
          beta3 <- wlsez$par[2]
        }
        if (empirical==TRUE) {

          c1 <- base::seq(0.00001, 1, length.out = 100)
          c2 <- base::seq(0.00001, 1, length.out = 100)


          colorscalemp <- base::list(
            list(0, "rgb(255,255,255)"),
            list(0.2, "rgb(230,230,230)"),
            list(0.4, "rgb(200,200,200)"),
            list(0.6, "rgb(150,150,150)"),
            list(0.8, "rgb(100,100,100)"),
            list(1, "rgb(0,0,0)")
          )

          Fn <- stats::ecdf(y)
          vus2 <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- Fn(stats::quantile(z,  c2)) - Fn(stats::quantile(x, 1-c1))
            base::pmax(result, 0)
          }))

          fig2<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus2, type = 'surface',
                                colorscale = colorscalemp)

          fig2$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig2$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig2$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig2$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig2$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig2$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig2$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig2$x$layout$scene$xaxis$range <- c(0, 1)
          fig2$x$layout$scene$yaxis$range <- c(0, 1)
          fig2$x$layout$scene$zaxis$range <- c(0, 1)

          fig2<- plotly::colorbar(fig2,title = "Empirical ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig2

        }
        else if (empirical==FALSE) {

          c1 <- seq(0.00001, 1, length.out = 100)
          c2 <- seq(0.00001, 1, length.out = 100)

          vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
            result <- pL(qL( c2, alpha3, beta3), alpha2, beta2) -
              pL(qL(1-c1, alpha1, beta1), alpha2, beta2)
            base::pmax(result, 0)
          }))
          fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                                colorscale = "Greys")

          fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
          fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
          fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

          fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
          fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

          fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
          fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

          fig1$x$layout$scene$xaxis$range <- c(0, 1)
          fig1$x$layout$scene$yaxis$range <- c(0, 1)
          fig1$x$layout$scene$zaxis$range <- c(0, 1)

          fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                  tickvals = c(0, 0.5, 1))
          fig1
        }
      }
      else if (method=="TRUE") {
        x<-NULL
        y<-NULL
        z<-NULL
        init_param <- NULL
        empirical<- NULL
        alpha1<-true_param[[1]]
        beta1<-true_param[[2]]
        alpha2<-true_param[[3]]
        beta2<-true_param[[4]]
        alpha3<-true_param[[5]]
        beta3<-true_param[[6]]
if(any(alpha1<=0)) {stop(paste("alpha1 value must be greater than 0","\n",""))}
  if(any(beta1<=0)) {stop(paste("beta1 value must be greater than 0","\n",""))}
if(any(alpha2<=0)) {stop(paste("alpha2 value must be greater than 0","\n",""))}
if(any(beta2<=0)) {stop(paste("beta2 value must be greater than 0","\n",""))}
if(any(alpha3<=0)) {stop(paste("alpha3 value must be greater than 0","\n",""))}
  if(any(beta3<=0)) {stop(paste("beta3 value must be greater than 0","\n",""))}


        c1 <- seq(0.00001, 1, length.out = 100)
        c2 <- seq(0.00001, 1, length.out = 100)

        vus <- base::outer(c1, c2, Vectorize(function(c1, c2) {
          result <- pL(qL( c2, alpha3, beta3), alpha2, beta2) -
            pL(qL(1-c1, alpha1, beta1), alpha2, beta2)
          base::pmax(result, 0)
        }))
        fig1<-plotly::plot_ly(x = ~c1, y = ~c2, z = ~vus, type = 'surface',
                              colorscale = "Greys")

        fig1$x$layout$scene$xaxis$title <- "TPF\u2081"
        fig1$x$layout$scene$yaxis$title <- "TPF\u2083"
        fig1$x$layout$scene$zaxis$title <- "TPF\u2082"

        fig1$x$layout$scene$xaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$yaxis$tickvals <- c(0, 0.5, 1)
        fig1$x$layout$scene$zaxis$tickvals <- c(0, 0.5, 1)

        fig1$x$layout$scene$xaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$yaxis$ticktext <- c("0", "0.5", "1")
        fig1$x$layout$scene$zaxis$ticktext <- c("0", "0.5", "1")

        fig1$x$layout$scene$xaxis$range <- c(0, 1)
        fig1$x$layout$scene$yaxis$range <- c(0, 1)
        fig1$x$layout$scene$zaxis$range <- c(0, 1)

        fig1<- plotly::colorbar(fig1,title = "Fitted ROC Surface",
                                tickvals = c(0, 0.5, 1))
        fig1


  }
}
}


