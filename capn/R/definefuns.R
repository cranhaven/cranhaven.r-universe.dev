#' Unidimensional Chebyshev nodes
#'
#' @description The function generates uni-dimensional chebyshev nodes.
#' @param n A number of nodes
#' @param a The lower bound of inverval [a,b]
#' @param b The upper bound of interval [a,b]
#' @details A polynomial approximant \eqn{s_{i}} over a bounded interval [a,b] is constructed by:\cr
#' \cr
#' \eqn{s_{i} = \frac{b+a}{2} + \frac{b-a}{2}cos (\frac{n - i + 0.5 }{n} \pi )} for \eqn{i = 1,2,\cdots,n}. \cr
#' \cr
#' More detail explanation can be refered from Miranda and Fackler (2002, p.119).
#' @return An array n Chebyshev nodes
#' @references Miranda, Mario J. and Paul L. Fackler. (2002) \emph{Applied Computational Economics and Finance}. Cambridge: The MIT Press.
#' @examples
#' ## 10 Chebyshev nodes in [-1,1]
#' chebnodegen(10,-1,1)
#' ## 5 Chebyshev nodes in [1,5]
#' chebnodegen(5,1,5)
#' @export
chebnodegen <- function(n,a,b){
  d1 <- length(a)
  d2 <- length(b)

  si <- (a+b)*0.5 + (b-a)*0.5*cos(pi*((seq((n-1),0,by=-1)+0.5)/n))

  if (d1 != d2){
    print("Dimension Mismatch: dim(upper) != dim(lower)")
  }

  return(si)
}
#' Generating Chebyshev grids
#'
#' @description This function generates a grid of multi-dimensional Chebyshev nodes.
#' @param nnodes An array of numbers of nodes
#' @param lb An array of lower bounds
#' @param ub An array of upper bounds
#' @param rtype A type of results; default is NULL that returns a list class; if rtype = list, returns a list class;
#'        if rtype = grid, returns a matrix class.
#' @details  For the \eqn{i}-th dimension of \eqn{i = 1, 2, \cdots, d}, suppose a polynomial approximant
#' \eqn{s_{i}} over a bounded interval \eqn{[a_{i},b_{i}]} is defined by Chebysev nodes. Then, a \eqn{d}-dimensional
#' Chebyshev grids can be defined as:\cr
#' \cr
#' \eqn{\mathbf{S} = \left\{ (s_{1},s_{2},\cdots,s_{d}) \vert a_{i} \leq s_{1} \leq b_{i}, i = 1, 2, \cdots, d \right\} }.\cr
#' \cr
#' This is all combinations of \eqn{s_{i}}. Two types of results are provided. '\code{rtype = list}' provides a list
#' of \eqn{d} dimensions wherease '\code{rtype = grids}' creates a \eqn{ \left( \displaystyle \prod_{i=1}^{d} n_{i} \right) \times d} matrix.
#' @return A list with \eqn{d} elements of Chebyshev nodes or a \eqn{ \left( \displaystyle \prod_{i=1}^{d} n_{i} \right) \times d} matrix of Chebyshev grids
#' @seealso \code{\link{chebnodegen}}
#' @examples
#' ## Chebyshev grids with two-dimension
#' chebgrids(c(5,3), c(1,1), c(2,3))
#' # Returns the same results
#' chebgrids(c(5,3), c(1,1), c(2,3), rtype='list')
#' ## Returns a matrix grids with the same domain
#' chebgrids(c(5,3), c(1,1), c(2,3), rtype='grid')
#' ## Chebyshev grids with one-dimension
#' chebgrids(5,1,2)
#' chebnodegen(5,1,2)
#' ## Chebyshev grids with three stock
#' chebgrids(c(3,4,5),c(1,1,1),c(2,3,4),rtype='grid')
#' @export
chebgrids <- function(nnodes, lb, ub, rtype = NULL){
  dn <- length(nnodes)
  dl <- length(lb)
  du <- length(ub)

  if (dn != dl)
    stop("Dimension Mismatch: Stock dimension != lower bounds dimension")
  else if (dn != du)
    stop("Dimension Mismatch: Stock dimension != upper bounds dimension")
  else
    chebknots <- mapply(chebnodegen, nnodes, lb, ub, SIMPLIFY = FALSE)

  if (is.null(rtype)){
    return(chebknots)
  }
  else if (rtype == 'list'){
    return(chebknots)
  }
  else if (rtype == 'grid'){
    gridcomb <- as.matrix(do.call(expand.grid,chebknots),ncol=dn)
    return(gridcomb)
  }
  else
    stop('type should be: NULL, list, or grid!')
}
#' Generating unifrom grids
#'
#' @description This function generates a grid of multi-dimensional uniform grids.
#' @param nnodes An array of numbers of nodes
#' @param lb An array of lower bounds
#' @param ub An array of upper bounds
#' @param rtype A type of results; default is NULL that returns a list class; if rtype = list, returns a list class;
#'        if rtype = grid, returns a matrix class.
#' @details  For the \eqn{i}-th dimension of \eqn{i = 1, 2, \cdots, d}, suppose a polynomial approximant
#' \eqn{s_{i}} over a bounded interval \eqn{[a_{i},b_{i}]} is defined by evenly gridded nodes. Then, a \eqn{d}-dimensional
#' uniform grids can be defined as:\cr
#' \cr
#' \eqn{\mathbf{S} = \left\{ (s_{1},s_{2},\cdots,s_{d}) \vert a_{i} \leq s_{1} \leq b_{i}, i = 1, 2, \cdots, d \right\} }.\cr
#' \cr
#' This is all combinations of \eqn{s_{i}}. Two types of results are provided. '\code{rtype = list}' provides a list
#' of \eqn{d} dimensions wherease '\code{rtype = grids}' creates a \eqn{ \left( \displaystyle \prod_{i=1}^{d} n_{i} \right) \times d} matrix.
#' @return A list with \eqn{d} elements of Chebyshev nodes or a \eqn{ \left( \displaystyle \prod_{i=1}^{d} n_{i} \right) \times d} matrix of uniform grids
#' @examples
#' ## Uniform grids with two-dimension
#' unigrids(c(5,3), c(1,1), c(2,3))
#' ## Returns the same results
#' unigrids(c(5,3), c(1,1), c(2,3), rtype='list')
#' ## Returns a matrix grids with the same domain
#' unigrids(c(5,3), c(1,1), c(2,3), rtype='grid')
#' ## Uniform grid with one-dimension
#' unigrids(5,1,2)
#' ## Uniform grids with three stock
#' unigrids(c(3,4,5),c(1,1,1),c(2,3,4),rtype='grid')
#' @export
unigrids <- function(nnodes, lb, ub, rtype = NULL){
  dn <- length(nnodes)
  dl <- length(lb)
  du <- length(ub)

  if (dn != dl)
    stop("Dimension Mismatch: Stock dimension != lower bounds dimension")
  else if (dn != du)
    stop("Dimension Mismatch: Stock dimension != upper bounds dimension")
  else
    uniknots <- mapply(seq, from=lb, to=ub, len=nnodes, SIMPLIFY = FALSE)

  if (is.null(rtype)){
    return(uniknots)
  }
  else if (rtype == 'list'){
    return(uniknots)
  }
  else if (rtype == 'grid'){
    gridcomb <- as.matrix(do.call(expand.grid,uniknots),ncol=dn)
    return(gridcomb)
  }
  else
    stop('type should be: NULL, list, or grid!')
}
#' Generating Unidimensional Chebyshev polynomial (monomial) basis
#'
#' @description The function calculates the monomial basis of Chebyshev polynomials for the given unidimensional nodes,
#' \eqn{s_{i}}, over a bounded interval [a,b].
#' @param stock An array of Chebyshev polynomial nodes \eqn{s_{i}} \cr
#' (an array of stocks in \code{capn}-packages)
#' @param npol Number of polynomials (n polynomials = (n-1)-th degree)
#' @param a The lower bound of inverval [a,b]
#' @param b The upper bound of inverval [a,b]
#' @param dorder Degree of partial derivative of the basis; Default is NULL; if dorder = 1,
#'          returns the first order partial derivative
#' @details Suppose there are \eqn{m} numbers of Chebyshev nodes over a bounded interval [a,b]:\cr
#' \cr
#' \eqn{s_{i} \in [a,b],} for \eqn{i = 1,2,\cdots,m}.\cr
#' \cr
#' These nodes can be nomralized to the standard Chebyshev nodes over the domain [-1,1]:\cr
#' \cr
#' \eqn{z_{i} = \frac{2(s_{i} - a)}{(b - a)} - 1}.\cr
#' \cr
#' With normalized Chebyshev nodes, the recurrence relations of Chebyshev polynomials of other \eqn{n} is defined as:\cr
#' \cr
#' \eqn{T_{0} (z_{i}) = 1}, \cr
#' \eqn{T_{1} (z_{i}) = z_{i}}, and \cr
#' \eqn{T_{n} (z_{i}) = 2 z_{i} T_{n-1} (z_{i}) - T_{n-2} (z_{i})}.\cr
#' \cr
#' The interpolation matrix (Vandermonde matrix) of (n-1)-th Chebyshev polynomials with \eqn{m} numbers of nodes,
#' \eqn{\Phi_{mn}} is:\cr
#' \cr
#' \eqn{ \Phi_{mn} = \left[ \begin{array}{ccccc}
#' 1 & T_{1} (z_{1}) & \cdots & T_{n-1} (z_{1})\\
#' 1 & T_{1} (z_{2}) & \cdots & T_{n-1} (z_{2})\\
#' \vdots & \vdots & \ddots & \vdots\\
#' 1 & T_{1} (z_{m}) & \cdots & T_{n-1} (z_{m})
#' \end{array} \right] }.\cr
#' \cr
#' The partial derivative of the monomial basis matrix can be found by the relation:\cr
#' \cr
#' \eqn{(1-z_{i}^{2}) T'_{n} (z_{i}) = n[ T_{n-1} (z_{i}) - z_{i} T_{n} (z_{i}) ]}.\cr
#' \cr
#' The technical details of the monomial basis of Chebyshev polynomial can be referred from Amparo et al. (2007)
#' and Miranda and Fackler (2012).
#' @return A matrix (number of nodes (\eqn{m}) x npol (\eqn{n})) of (monomial) Chebyshev polynomial basis
#' @examples
#' ## Reef-fish example: see Fenichel and Abbott (2014)
#' data("GOM")
#' nodes <- chebnodegen(param$nodes,param$lowerK,param$upperK)
#' ## An example of Chebyshev polynomial basis
#' chebbasisgen(nodes,20,0.1,1.5)
#' ## The partial derivative of Chebyshev polynomial basis with the same function
#' chebbasisgen(nodes,20,0.1,1.5,1)
#' @seealso \code{\link{chebnodegen}}
#' @references Amparo, Gil, Javier Segura, and Nico Temme. (2007) \emph{Numerical Methods for Special Functions}. Cambridge: Cambridge University Press.\cr
#' Miranda, Mario J. and Paul L. Fackler. (2002) \emph{Applied Computational Economics and Finance}. Cambridge: The MIT Press.
#' @export
chebbasisgen <- function(stock,npol,a,b,dorder=NULL){
  nknots <- length(stock)
  z <- (2*stock-b-a)/(b-a)
  if (npol < 4)
    stop("Degree of Chebyshev polynomial should be greater than 3!")

  bvec <- cbind(matrix(rep(1,nknots),ncol=1), matrix(z, ncol=1))
  colnames(bvec) <- c("j=1","j=2")

  for (j in 2:(npol-1)){
    Tj <- matrix(2*z*bvec[,j] - bvec[,j-1],ncol=1)
    colnames(Tj) <- paste0("j=",j+1)
    bvec <- cbind(bvec,Tj)
  }

  if (is.null(dorder)){
    res <- bvec
  }
  else if (dorder==1){
    bvecp <- cbind(matrix(rep(0,nknots),ncol=1), matrix(rep(2/(b-a),nknots), ncol=1))
    colnames(bvecp) <- c("j=1","j=2")
    for (j in 2:(npol-1)){
      Tjp <- matrix((4/(b-a))*bvec[,j]+2*z*bvecp[,j] - bvecp[,j-1],ncol=1)
      colnames(Tjp) <- paste0("j=",j+1)
      bvecp <- cbind(bvecp,Tjp)
    }
    res <- bvecp
  }
  else
    stop("dorder should be NULL or 1!")

  return(res)
}
#' Defining Approximation Space
#'
#' @description The function defines an approximation space for all three approximation apporoaches (V, P, and Pdot).
#' @param deg An array of degrees of approximation function: degrees of Chebyshev polynomials
#' @param lb An array of lower bounds
#' @param ub An array of upper bounds
#' @param delta discount rate
#' @details For the \eqn{i}-th dimension of \eqn{i = 1, 2, \cdots, d}, suppose a polynomial approximant
#' \eqn{s_{i}} over a bounded interval \eqn{[a_{i},b_{i}]} is defined by Chebysev nodes. Then, a \eqn{d}-dimensional
#' Chebyshev grids can be defined as:\cr
#' \cr
#' \eqn{\mathbf{S} = \left\{ (s_{1},s_{2},\cdots,s_{d}) \vert a_{i} \leq s_{1} \leq b_{i}, i = 1, 2, \cdots, d \right\} }.\cr
#' \cr
#' Suppose we impletement \eqn{n_{i}} numbers of polynomials (i.e., \eqn{(n_{i}-1)}-th order) for the \eqn{i}-th dimension.
#' The approximation space is defined as:\cr
#' \cr
#' \code{deg = c(\eqn{n_{1},n_{2},\cdots,n_{d}})},\cr
#' \code{lb = c(\eqn{a_{1},a_{2},\cdots,a_{d}})}, and\cr
#' \code{ub = c(\eqn{b_{1},b_{2},\cdots,b_{d}})}.\cr
#' \cr
#' \code{delta} is the given constant discount rate.
#' @return A list of approximation space
#' @seealso \code{\link{vaprox}, \link{vsim}, \link{paprox}, \link{psim}, \link{pdotaprox}, \link{pdotsim}}
#' @examples
#' ## Reef-fish example: see Fenichel and Abbott (2014)
#' delta <- 0.02
#' upper <- 359016000     # upper bound on approximation space
#' lower <- 5*10^6        # lower bound on approximation space
#' myspace <- aproxdef(50,lower,upper,delta)
#' ## Two dimensional example
#' ub <- c(1.5,1.5)
#' lb <- c(0.1,0.1)
#' deg <- c(20,20)
#' delta <- 0.03
#' myspace <- aproxdef(deg,lb,ub,delta)
#' @references Fenichel, Eli P. and Joshua K. Abbott. (2014) "\href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Natural Capital: From Metaphor to Measurement}."
#'              \emph{Journal of the Association of Environmental Economists}. 1(1/2):1-27.
#' @export
aproxdef <- function(deg,lb,ub,delta){
  if (delta > 1 | delta < 0)
    stop("delta should be in [0,1]!")

  dn <- length(deg)
  dl <- length(lb)
  du <- length(ub)

  if (dn != dl)
    stop("Dimension Mismatch: Stock dimension != lower bounds dimension")
  else if (dn != du)
    stop("Dimension Mismatch: Stock dimension != upper bounds dimension")
  else
    param <- list(degree = deg, lowerB = lb, upperB = ub, delta = delta)
  return(param)
}
#' Calculating V-approximation coefficients
#'
#' @description The function provides the V-approximation coefficients of the defined Chebyshev polynomials in \code{aproxdef}.
#' @param aproxspace An approximation space defined by \code{aproxdef} function
#' @param sdata A data.frame or matrix of [stock,sdot,benefit]=[\eqn{\mathbf{S}},\eqn{\mathbf{\dot{S}}},\eqn{W}]
#' @details The V-approximation is finding the shadow price of \eqn{i}-th stock, \eqn{p_{i}} for \eqn{i=1,\cdots,d}
#' from the relation:\cr
#' \cr
#' \eqn{\delta V = W(\mathbf{S}) + p_{1}\dot{s}_{1} + p_{2}\dot{s}_{2} + \cdots + p_{d}\dot{s}_{d}},\cr
#' \cr
#' where \eqn{\delta} is the given discount rate, \eqn{V} is the value function, \eqn{\mathbf{S} = (s_{1}, s_{2},
#' \cdots, s_{d})} is a vector of stocks, \eqn{W(\mathbf{S})} is the net benefits accruing to society,
#' and \eqn{\dot{s}_{i}} is the growth of stock \eqn{s_{i}}. By the definition of the shadow price, we know:\cr
#' \cr
#' \eqn{p_{i} = \frac{\partial V}{\partial s_{i}}}.\cr
#' \cr
#' Consider approximation \eqn{V(\mathbf{S}) = \mathbf{\mu}(\mathbf{S})\mathbf{\beta}}, \eqn{\mathbf{\mu}(\mathbf{S})}
#' is Chebyshev polynomials and \eqn{\mathbf{\beta}} is their coeffcients.
#' Then, \eqn{p_{i} = \mathbf{\mu}_{s_{i}}(\mathbf{S})\mathbf{\beta}} by the orthogonality of Chebyshev basis.
#' Adopting the properties above, we can get the unknown coefficient vector \eqn{\beta} from:\cr
#' \cr
#' \eqn{\delta \mathbf{\mu}(\mathbf{S})\mathbf{\beta} = W(\mathbf{S}) + \displaystyle \sum_{i=1}^{d} \mathbf{\mu}_{s_{i}}(\mathbf{S})\mathbf{\beta}}, and thus,\cr
#' \cr
#' \eqn{\beta = \left( \delta \mathbf{\mu}(\mathbf{S}) - \displaystyle \sum_{i=1}^{d} \mathbf{\mu}_{s_{i}}(\mathbf{S}) \right)^{-1} W(\mathbf{S}) }.\cr
#' \cr
#' In a case of over-determined (more nodes than approaximation degrees),\cr
#' \cr
#' \eqn{\beta = \left( \left( \delta \mathbf{\mu}(\mathbf{S}) - \displaystyle \sum_{i=1}^{d} \mathbf{\mu}_{s_{i}}(\mathbf{S}) \right)^{T}
#' \left( \delta \mathbf{\mu}(\mathbf{S}) - \displaystyle \sum_{i=1}^{d} \mathbf{\mu}_{s_{i}}(\mathbf{S}) \right) \right)^{-1}
#' \left( \delta \mathbf{\mu}(\mathbf{S}) - \displaystyle \sum_{i=1}^{d} \mathbf{\mu}_{s_{i}}(\mathbf{S}) \right)^{T} W(\mathbf{S}) }.\cr
#' \cr
#' For more detils see Fenichel and Abbott (2014) and Fenichel et al. (2016).
#' @return A list of approximation resuts: deg, lb, ub, delta, and coefficients
#' @seealso \code{\link{aproxdef}, \link{vsim}}
#' @examples
#' ## 1-D Reef-fish example: see Fenichel and Abbott (2014)
#' data("GOM")
#' nodes <- chebnodegen(param$nodes,param$lowerK,param$upperK)
#' simuDataV <- cbind(nodes,sdot(nodes,param),profit(nodes,param))
#' Aspace <- aproxdef(param$order,param$lowerK,param$upperK,param$delta)
#' vC <- vaprox(Aspace,simuDataV)
#'
#' ## 2-D Prey-Predator example
#' data("lvdata")
#' aproxdeg <- c(20,20)
#' lower <- c(0.1,0.1)
#' upper <- c(1.5,1.5)
#' delta <- 0.03
#' lvspace <- aproxdef(aproxdeg,lower,upper,delta)
#' vaproxc <- vaprox(lvspace,lvaproxdata)
#' @references Fenichel, Eli P. and Joshua K. Abbott. (2014) "\href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Natural Capital: From Metaphor to Measurement}."
#'              \emph{Journal of the Association of Environmental Economists}. 1(1/2):1-27.\cr
#' Fenichel, Eli P., Joshua K. Abbott, Jude Bayham, Whitney Boone, Erin M. K. Haacker, and Lisa Pfeiffer. (2016) "\href{http://www.pnas.org/content/113/9/2382}{Measuring the Value of Groundwater and Other Forms of Natural Capital}."
#'              \emph{Proceedings of the National Academy of Sciences }.113:2382-2387.
#' @export
vaprox <- function(aproxspace,sdata){
  deg <- aproxspace[["degree"]]
  lb <- aproxspace[["lowerB"]]
  ub <- aproxspace[["upperB"]]
  delta <- aproxspace[["delta"]]

  dd <- length(deg)
  if (is.data.frame(sdata)){
    sdata <- as.matrix(sdata)
  }

  if (!is.matrix(sdata))
    stop("sdata should be a data.frame or matrix of [stock,sdot,w]!")

  if (dim(sdata)[2] != (2*dd+1))
    stop("The number of columns in sdata is not right!")

  if (dd > 1){
    ordername <- paste0("sdata[,",dd,"]")
    for (di in 2:dd){
      odtemp <- paste0("sdata[,",dd-di+1,"]")
      ordername <- paste(ordername,odtemp,sep=",")
    }
    ordername <- paste("sdata[order(",ordername,"),]",sep="")
    sdata <- eval(parse(text=ordername))
  }
  else sdata <- sdata[order(sdata[,1]),]

  st <- lapply(1:dd, function(k) unique(sdata[,k]))
  sdot <- lapply((dd+1):(2*dd), function(k) sdata[,k])
  w <- sdata[,(2*dd+1)]

  fphi <- matrix(1)
  sphi <- matrix(rep(0, prod(sapply(st, length))*prod(deg)),ncol=prod(deg))
  for (di in 1:dd){
    dk <- dd - di + 1

    ftemp <- chebbasisgen(st[[dk]],deg[dk],lb[dk],ub[dk])
    fphi <- kronecker(fphi,ftemp)

    stempi <- chebbasisgen(st[[di]],deg[di],lb[di],ub[di], dorder=1)

    sphitemp <- matrix(1)
    for (dj in 1:dd){
      dk2 <- dd - dj + 1
      if (dk2 != di){
        stemp <- chebbasisgen(st[[dk2]],deg[dk2],lb[dk2],ub[dk2])
      }
      else
        stemp <- stempi

      sphitemp <- kronecker(sphitemp,stemp)
    }
    sphi <- sphi + sphitemp*sdot[[di]]
  }

  nsqr <- delta*fphi - sphi
  if (dim(fphi)[1] == dim(fphi)[2]){
    coeff <- solve(nsqr,w)
    res <- list(degree = deg, lowerB = lb, upperB = ub, delta = delta, coefficient = coeff)
  }
  else if (dim(fphi)[1] != dim(fphi)[2]){
    coeff <- solve(t(nsqr)%*%nsqr, t(nsqr)%*%w)
    res <- list(degree = deg, lowerB = lb, upperB = ub, delta = delta, coefficient = coeff)
  }
  return(res)
}
#' Simulation of V-approximation
#'
#' @description The function provides the V-approximation simulation by adopting the results of \code{vaprox}. Available for multiple stock problems.
#' @param vcoeff An approximation result from \code{varpox} function
#' @param adata A data.frame or matrix of [stock]=[\eqn{\mathbf{S}}]
#' @param wval (Optional for \code{plotgen}) An array of \eqn{W}-value
#' @details Let \eqn{\hat{\beta}} be the approximation coefficent from the results of \code{vaprox} function.
#' The estimated shadow (accounting) price of \eqn{i}-th stock over the given approximation intervals of \eqn{s_{i} \in [a_{i},b_{i}]},
#' \eqn{\hat{p}_{i}} can be calcuated as:\cr
#' \cr
#' \eqn{\hat{p}_{i} = \mathbf{\mu}(\mathbf{S})\mathbf{\hat{\beta}}} where \eqn{\mathbf{\mu}(\mathbf{S})} Chebyshev polynomial basis.\cr
#' \cr
#' The Inclusive wealth of \eqn{i}-th stock is:\cr
#' \cr
#' \eqn{IW = \displaystyle \sum_{i}^{d} \hat{p}_{i} s_{i} }, and \cr
#' \cr
#' the value function is:\cr
#' \cr
#' \eqn{\hat{V} = \delta \mathbf{\mu}(\mathbf{S})\mathbf{\hat{\beta}}}.\cr
#' \cr
#' For more detils see Fenichel and Abbott (2014) and Fenichel et al. (2016).
#' @return A list of simulation resuts: shadow (accounting) prices, inclusive wealth, Value function,\cr
#' stock, and W values.
#' @seealso \code{\link{aproxdef}, \link{vsim}}
#' @examples
#' ## 1-D Reef-fish example: see Fenichel and Abbott (2014)
#' data("GOM")
#' nodes <- chebnodegen(param$nodes,param$lowerK,param$upperK)
#' simuDataV <- cbind(nodes,sdot(nodes,param),profit(nodes,param))
#' Aspace <- aproxdef(param$order,param$lowerK,param$upperK,param$delta)
#' vC <- vaprox(Aspace,simuDataV)
#' # Note vcol function requries a data.frame or matrix!
#' GOMSimV <- vsim(vC,as.matrix(simuDataV[,1],ncol=1),profit(nodes,param))
#'
#' # plot shadow (accounting) price: Figure 4 in Fenichel and Abbott (2014)
#' plotgen(GOMSimV, xlabel="Stock size, s", ylabel="Shadow price")
#'
#' ## 2-D Prey-Predator example
#' data("lvdata")
#' aproxdeg <- c(20,20)
#' lower <- c(0.1,0.1)
#' upper <- c(1.5,1.5)
#' delta <- 0.03
#' lvspace <- aproxdef(aproxdeg,lower,upper,delta)
#' lvaproxc <- vaprox(lvspace,lvaproxdata)
#' lvsim <- vsim(lvaproxc,lvsimdata.time[,2:3])
#'
#' # plot Biomass
#' plot(lvsimdata.time[,1], lvsimdata.time[,2], type='l', lwd=2, col="blue",
#'      xlab="Time",
#'      ylab="Biomass")
#' lines(lvsimdata.time[,1], lvsimdata.time[,3], lwd=2, col="red")
#' legend("topright", c("Prey", "Predator"), col=c("blue", "red"),
#'        lty=c(1,1), lwd=c(2,2), bty="n")
#'
#' # plot shadow (accounting) prices
#' plot(lvsimdata.time[,1],lvsim[["shadowp"]][,1],type='l', lwd=2, col="blue",
#'      ylim = c(-5,7),
#'      xlab="Time",
#'      ylab="Shadow price")
#' lines(lvsimdata.time[,1],lvsim[["shadowp"]][,2], lwd=2, col="red")
#' legend("topright", c("Prey", "Predator"), col=c("blue", "red"),
#'        lty=c(1,1), lwd=c(2,2), bty="n")
#'
#' # plot inclusive weath and value function
#' plot(lvsimdata.time[,1],lvsim[["iw"]],type='l', lwd=2, col="blue",
#'      ylim = c(-0.5,1.2),
#'      xlab="Time",
#'      ylab="Inclusive Wealth / Value Function ($)")
#' lines(lvsimdata.time[,1],lvsim[["vfun"]], lwd=2, col="red")
#' legend("topright", c("Inclusive Wealth", "Value Function"),
#'        col=c("blue", "red"), lty=c(1,1), lwd=c(2,2), bty="n")
#' @references Fenichel, Eli P. and Joshua K. Abbott. (2014) "\href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Natural Capital: From Metaphor to Measurement}."
#'              \emph{Journal of the Association of Environmental Economists}. 1(1/2):1-27.\cr
#' Fenichel, Eli P., Joshua K. Abbott, Jude Bayham, Whitney Boone, Erin M. K. Haacker, and Lisa Pfeiffer. (2016) "\href{http://www.pnas.org/content/113/9/2382}{Measuring the Value of Groundwater and Other Forms of Natural Capital}."
#'              \emph{Proceedings of the National Academy of Sciences }.113:2382-2387.
#' @export
vsim <- function(vcoeff,adata,wval=NULL){
  deg <- vcoeff[["degree"]]
  lb <- vcoeff[["lowerB"]]
  ub <- vcoeff[["upperB"]]
  delta <- vcoeff[["delta"]]
  coeff <- vcoeff[["coefficient"]]
  nnodes <- nrow(adata)

  dd <- length(deg)

  if (is.data.frame(adata)){
    st <- as.matrix(adata)
  }
  else if (is.matrix(adata)){
    st <-adata
  }
  else
    stop("st is not a matrix or data.frame!")

  accp <- matrix(rep(0,nnodes*dd), ncol=dd)
  Bmat <- matrix(rep(0,nnodes*prod(deg)),ncol=prod(deg))
  for (di in 1:dd){
    Bprime <- matrix(rep(0,nnodes*prod(deg)),ncol=prod(deg))
    for (ni in 1:nnodes){
      sti <- st[ni,]
      dk <- dd - di + 1

      fphi <- matrix(1)
      ftemp <- chebbasisgen(sti[di],deg[di],lb[di],ub[di])

      sphi <- matrix(1)
      stempd <- chebbasisgen(sti[di],deg[di],lb[di],ub[di], dorder=1)
      for (dj in 1:dd){
        dk2 <- dd - dj + 1
        if (dk2 != di){
          ftemp <- chebbasisgen(sti[dk2],deg[dk2],lb[dk2],ub[dk2])
          stemp <- ftemp
        }
        else
          stemp <- stempd

        fphi <- kronecker(fphi,ftemp)
        sphi <- kronecker(sphi,stemp)
      }
      Bmat[ni,] <- fphi
      Bprime[ni,] <- sphi
    }
    accp[,di] <- Bprime%*%coeff
  }
  iwhat <- accp*st
  iw <- matrix(rowSums(iwhat),ncol=1)
  vhat <- Bmat%*%coeff
  colnames(accp) <- paste("acc.price", 1:dd, sep="")
  colnames(iwhat) <- paste("iw", 1:dd, sep="")
  colnames(iw) <- c("iw")

  if (is.null(wval) == 1){
    wval <- "wval is not provided"
  }

  res <- list(shadowp = accp, iweach = iwhat, iw = iw, vfun = vhat, stock = st, wval = wval)
  return(res)
}
#' Calculating P-approximation coefficients
#'
#' @description The function provides the P-approximation coefficients of the defined Chebyshev polynomials in \code{aproxdef}.
#'              For now, only unidimensional case is developed.
#' @param aproxspace An approximation space defined by \code{aproxdef} function
#' @param stock An array of stock, \eqn{s}
#' @param sdot An array of ds/dt, \eqn{\dot{s}=\frac{ds}{dt}}
#' @param dsdotds An array of d(sdot)/ds, \eqn{\frac{d \dot{s}}{d s}}
#' @param dwds An array of dw/ds, \eqn{\frac{dW}{ds}}
#' @details The P-approximation is finding the shadow price of a stock, \eqn{p} from the relation:\cr
#' \cr
#' \eqn{p(s) = \frac{W_{s}(s) + \dot{p}(s)}{\delta - \dot{s}_{s}}}, \cr
#' \cr
#' where \eqn{W_{s} = \frac{dW}{ds}}, \eqn{ \dot{p}(s) = \frac{dp}{ds}},
#' \eqn{\dot{s}_{s} = \frac{d\dot{s}}{ds} }, and \eqn{\delta} is the given discount rate.\cr
#' \cr
#' Consider approximation \eqn{p(s) = \mathbf{\mu}(s)\mathbf{\beta}}, \eqn{\mathbf{\mu}(s)}
#' is Chebyshev polynomials and \eqn{\mathbf{\beta}} is their coeffcients.
#' Then, \eqn{\dot{p} = \mathbf{\mu}_{s}(s)\mathbf{\beta}} by the orthogonality of Chebyshev basis.
#' Adopting the properties above, we can get the unknown coefficient vector \eqn{\beta} from:\cr
#' \cr
#' \eqn{\mathbf{\mu}\mathbf{\beta} = \left( W_{s} + \dot{s} \mathbf{\mu}_{s} \mathbf{\beta} \right)
#' \left( \delta - \dot{s}_{s} \right)^{-1} }, and thus, \cr
#' \cr
#' \eqn{\mathbf{\beta} = \left( \delta \mathbf{\mu} - \dot{s}_{s} \mathbf{\mu} - \dot{s} \mathbf{\mu}_{s} \right)^{-1} W_{s} }.\cr
#' \cr
#' In a case of over-determined (more nodes than approaximation degrees),\cr
#' \cr
#' \eqn{\left( \left( \delta \mathbf{\mu} - \dot{s}_{s} \mathbf{\mu} - \dot{s} \mathbf{\mu}_{s} \right)^{T}
#' \left( \delta \mathbf{\mu} - \dot{s}_{s} \mathbf{\mu} - \dot{s} \mathbf{\mu}_{s} \right) \right)^{-1}
#' \left( \delta \mathbf{\mu} - \dot{s}_{s} \mathbf{\mu} - \dot{s} \mathbf{\mu}_{s} \right)^{T} W_{s}} \cr
#' \cr
#' For more detils see Fenichel and Abbott (2014) and Fenichel et al. (2016).
#' @return A list of approximation resuts: deg, lb, ub, delta, and coefficients
#' @examples
#' ## 1-D Reef-fish example: see Fenichel and Abbott (2014)
#' data("GOM")
#' nodes <- chebnodegen(param$nodes,param$lowerK,param$upperK)
#' simuDataP <- cbind(nodes,sdot(nodes,param),
#'                    dsdotds(nodes,param),dwds(nodes,param))
#' Aspace <- aproxdef(param$order,param$lowerK,param$upperK,param$delta)
#' pC <- paprox(Aspace,simuDataP[,1],simuDataP[,2],
#'              simuDataP[,3],simuDataP[,4])
#' @seealso \code{\link{aproxdef},\link{psim}}
#' @references Fenichel, Eli P. and Joshua K. Abbott. (2014) "\href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Natural Capital: From Metaphor to Measurement}."
#'              \emph{Journal of the Association of Environmental Economists}. 1(1/2):1-27.\cr
#' Fenichel, Eli P., Joshua K. Abbott, Jude Bayham, Whitney Boone, Erin M. K. Haacker, and Lisa Pfeiffer. (2016) "\href{http://www.pnas.org/content/113/9/2382}{Measuring the Value of Groundwater and Other Forms of Natural Capital}."
#'              \emph{Proceedings of the National Academy of Sciences }.113:2382-2387.
#' @export
paprox <- function(aproxspace,stock,sdot,dsdotds,dwds){
  deg <- aproxspace[["degree"]]
  lb <- aproxspace[["lowerB"]]
  ub <- aproxspace[["upperB"]]
  delta <- aproxspace[["delta"]]

  if (is.matrix(stock)){
    stock <- as.vector(stock)
  }
  if (is.matrix(sdot)){
    sdot <- as.vector(sdot)
  }
  if (is.matrix(dsdotds)){
    dsdotds <- as.vector(dsdotds)
  }
  if (is.matrix(dwds)){
    dwds <- as.vector(dwds)
  }

  fphi <- chebbasisgen(stock,deg,lb,ub)
  sphi <- chebbasisgen(stock,deg,lb,ub,dorder=1)

  nsqr <- (delta-dsdotds)*fphi-sdot*sphi
  if (deg == length(stock)){
    coeff <- t(solve(nsqr,dwds))
    res <- list(degree = deg, lowerB = lb, upperB = ub, delta = delta, coefficient = coeff)
  }
  else if (deg < length(stock)){
    coeff <- t(solve(t(nsqr)%*%nsqr, t(nsqr)%*%dwds))
    res <- list(degree = deg, lowerB = lb, upperB = ub, delta = delta, coefficient = coeff)
  }
  return(res)
}
#' Simulation of P-approximation
#'
#' @description The function provides the P-approximation simulation.
#' @param pcoeff An approximation result from \code{paprox} function
#' @param stock An array of stock variable
#' @param wval (Optional for vfun) An array of \eqn{W}-value (need \code{sdot} simultaneously)
#' @param sdot (Optional for vfun) An array of ds/dt, \eqn{\dot{s}=\frac{ds}{dt}} (need \code{W} simultaneously)
#' @details Let \eqn{\hat{\beta}} be the approximation coefficent from the results of \code{paprox} function.
#' The estimated shadow price (accounting) price of stock over the given approximation intervals of
#' \eqn{s \in [a,b]}, \eqn{\hat{p}} can be calculated as:\cr
#' \cr
#' \eqn{\hat{p} = \mathbf{\mu}(s) \mathbf{\hat{\beta}}}.\cr
#' \cr
#' The inclusive wealth is:\cr
#' \cr
#' \eqn{IW = \hat{p}s }. \cr
#' \cr
#' For more detils see Fenichel and Abbott (2014) and Fenichel et al. (2016).
#' @return A list of approximation resuts: shadow (accounting) prices and inclusive wealth
#' @examples
#' ## 1-D Reef-fish example: see Fenichel and Abbott (2014)
#' data("GOM")
#' nodes <- chebnodegen(param$nodes,param$lowerK,param$upperK)
#' simuDataP <- cbind(nodes,sdot(nodes,param),
#'                    dsdotds(nodes,param),dwds(nodes,param))
#' Aspace <- aproxdef(param$order,param$lowerK,param$upperK,param$delta)
#' pC <- paprox(Aspace,simuDataP[,1],simuDataP[,2],
#'              simuDataP[,3],simuDataP[,4])
#' GOMSimP <- psim(pC,simuDataP[,1],profit(nodes,param),simuDataP[,2])
#'
#' # Shadow Price
#' plotgen(GOMSimP, xlabel="Stock size, s", ylabel="Shadow price")
#'
#' # Value function and profit
#' plotgen(GOMSimP,ftype="vw",
#'         xlabel="Stock size, s",
#'         ylabel=c("Value Function","Profit"))
#' @seealso \code{\link{aproxdef}, \link{paprox}}
#' @references Fenichel, Eli P. and Joshua K. Abbott. (2014) "\href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Natural Capital: From Metaphor to Measurement}."
#'              \emph{Journal of the Association of Environmental Economists}. 1(1/2):1-27.\cr
#' Fenichel, Eli P., Joshua K. Abbott, Jude Bayham, Whitney Boone, Erin M. K. Haacker, and Lisa Pfeiffer. (2016) "\href{http://www.pnas.org/content/113/9/2382}{Measuring the Value of Groundwater and Other Forms of Natural Capital}."
#'              \emph{Proceedings of the National Academy of Sciences }.113:2382-2387.
#' @export
psim <- function(pcoeff,stock,wval=NULL,sdot=NULL){
  deg <- pcoeff[["degree"]]
  lb <- pcoeff[["lowerB"]]
  ub <- pcoeff[["upperB"]]
  delta <- pcoeff[["delta"]]
  coeff <- t(pcoeff[["coefficient"]])
  nnodes <- length(stock)

  nullcheck <- is.null(wval) != is.null(sdot)

  if(nullcheck != 0)
    stop("wval and sdot are both NULL or the same length arrays!")

  dd <- length(deg)

  accp <- matrix(rep(0,nnodes*dd), ncol=dd)
  iw <- matrix(rep(0,nnodes*dd), ncol=dd)
  vhat <- "wval and sdot not provided"

  for (ni in 1:nnodes){
    sti <- stock[ni]
    accp[ni,] <- chebbasisgen(sti,deg,lb,ub)%*%coeff
    iw[ni,] <- accp[ni,]*sti
  }

  if (is.null(wval) != 1){
    vhat <- matrix(rep(0,nnodes*dd), ncol=dd)
    for (ni in 1:nnodes){
      vhat[ni,] <- (wval[ni] + accp[ni,]*sdot[ni])/delta
    }
  }

  if (is.null(wval) == 1){
    wval <- "wval and sdot not provided"
  }

  res <- list(shadowp = accp, iw = iw, vfun = vhat, stock = as.matrix(stock,ncol=1), wval=wval)
  return(res)
}
#' Calculating Pdot-approximation coefficients
#'
#' @description The function provides the Pdot-approximation coefficients of the defined Chebyshev polynomials in \code{aproxdef}.
#'              For now, only unidimensional case is developed.
#' @param aproxspace An approximation space defined by \code{aproxdef} function
#' @param stock An array of stock, \eqn{s}
#' @param sdot An array of ds/dt, \eqn{\dot{s}=\frac{ds}{dt}}
#' @param dsdotds An array of d(sdot)/ds, \eqn{\frac{d \dot{s}}{d s}}
#' @param dsdotdss An array of d/ds(d(sdot)/ds), \eqn{ \frac{d}{ds} \left( \frac{d \dot{s}}{ds} \right)}
#' @param dwds An array of dw/ds, \eqn{\frac{dW}{ds}}
#' @param dwdss An array of d/ds(dw/ds), \eqn{\frac{d}{ds} \left( \frac{dW}{ds} \right)}
#' @details The Pdot-approximation is finding the shadow price of a stock, \eqn{p} from the relation:\cr
#' \cr
#' \eqn{p(s) = \frac{W_{s}(s) + \dot{p}(s)}{\delta - \dot{s}_{s}}}, \cr
#' \cr
#' where \eqn{W_{s} = \frac{dW}{ds}}, \eqn{ \dot{p}(s) = \frac{dp}{ds}},
#' \eqn{\dot{s}_{s} = \frac{d\dot{s}}{ds} }, and \eqn{\delta} is the given discount rate.\cr
#' \cr
#' In order to operationhalize this approach, we take the time derivative of this expression:\cr
#' \cr
#' \eqn{ \dot{p} = \frac{ \left( \left(W_{ss}\dot{s} + \ddot{p} \right) \left( \delta - \dot{s}_{s} \right) +
#'    \left( W_{s} + \dot{p} \right) \left(\dot{s}_{ss} \dot{s} \right)  \right)  }
#'    { \left( \delta - \dot{s}_{s} \right)^{2} } } \cr
#' \cr
#' Consider approximation \eqn{ \dot{p}(s) = \mathbf{\mu}(s)\mathbf{\beta}}, \eqn{\mathbf{\mu}(s)}
#' is Chebyshev polynomials and \eqn{\mathbf{\beta}} is their coeffcients.
#' Then, \eqn{ \ddot{p} = \frac{ d \dot{p}}{ds} \frac{ds}{dt} = \dot{s} \mathbf{\mu}_{s}(s) \mathbf{\beta}} by the orthogonality of Chebyshev basis.
#' Adopting the properties above, we can get the unknown coefficient vector \eqn{\beta} from:\cr
#' \cr
#' \eqn{ \mathbf{\mu \beta} =  \left( \delta - \dot{s}_{s} \right)^{-2}
#'    \left[ \left(W_{ss}\dot{s} + \dot{s} \mathbf{\mu}_{s} \mathbf{\beta} \right)\left( \delta - \dot{s}_{s} \right) +
#'    \left( W_{s} + \mathbf{\mu \beta} \right) \left(\dot{s}_{ss} \dot{s} \right)  \right] }, and \cr
#' \cr
#' \eqn{\mathbf{\beta} = \left[ \left( \delta - \dot{s}_{s} \right)^{2} \mathbf{\mu} - \dot{s}\left( \delta - \dot{s}_{s} \right) \mathbf{\mu}_{s}
#'     - \dot{s}_{ss} \dot{s} \mathbf{\mu}   \right]^{-1}
#'     \left( W_{ss} \dot{s} \left( \delta - \dot{s}_{s} \right) + W_{s} \dot{s}_{ss} \dot{s} \right) }.  \cr
#' \cr
#' If we suppose \eqn{ A = \left[ \left( \delta - \dot{s}_{s} \right)^{2} \mathbf{\mu} - \dot{s}\left( \delta - \dot{s}_{s} \right) \mathbf{\mu}_{s}
#'     - \dot{s}_{ss} \dot{s} \mathbf{\mu}   \right] } and \eqn{ B = \left( W_{ss} \dot{s} \left( \delta - \dot{s}_{s} \right) + W_{s} \dot{s}_{ss} \dot{s} \right) },
#' then over-determined case can be calculated:\cr
#' \cr
#' \eqn{ \mathbf{\beta} = \left( A^{T}A \right)^{-1} A^{T}B }.\cr
#' \cr
#' For more detils see Fenichel and Abbott (2014) and Fenichel et al. (2016).
#' @return A list of approximation results: deg, lb, ub, delta, and coefficients
#' @examples
#' ## 1-D Reef-fish example: see Fenichel and Abbott (2014)
#' data("GOM")
#' nodes <- chebnodegen(param$nodes,param$lowerK,param$upperK)
#' simuDataPdot <- cbind(nodes,sdot(nodes,param),
#'                       dsdotds(nodes,param),dsdotdss(nodes,param),
#'                       dwds(nodes,param),dwdss(nodes,param))
#' Aspace <- aproxdef(param$order,param$lowerK,param$upperK,param$delta)
#' pdotC <- pdotaprox(Aspace,simuDataPdot[,1],simuDataPdot[,2],
#'                    simuDataPdot[,3],simuDataPdot[,4],
#'                    simuDataPdot[,5],simuDataPdot[,6])
#' @seealso \code{\link{aproxdef}, \link{pdotsim}}
#' @references Fenichel, Eli P. and Joshua K. Abbott. (2014) "\href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Natural Capital: From Metaphor to Measurement}."
#'              \emph{Journal of the Association of Environmental Economists}. 1(1/2):1-27.\cr
#' Fenichel, Eli P., Joshua K. Abbott, Jude Bayham, Whitney Boone, Erin M. K. Haacker, and Lisa Pfeiffer. (2016) "\href{http://www.pnas.org/content/113/9/2382}{Measuring the Value of Groundwater and Other Forms of Natural Capital}."
#'              \emph{Proceedings of the National Academy of Sciences }.113:2382-2387.
#' @export
pdotaprox <- function(aproxspace,stock,sdot,dsdotds,dsdotdss,dwds,dwdss){
  deg <- aproxspace[["degree"]]
  lb <- aproxspace[["lowerB"]]
  ub <- aproxspace[["upperB"]]
  delta <- aproxspace[["delta"]]

  if (is.matrix(stock)){
    stock <- as.vector(stock)
  }
  if (is.matrix(sdot)){
    sdot <- as.vector(sdot)
  }
  if (is.matrix(dsdotds)){
    dsdotds <- as.vector(dsdotds)
  }
  if (is.matrix(dsdotdss)){
    dsdotdss <- as.vector(dsdotdss)
  }
  if (is.matrix(dwds)){
    dwds <- as.vector(dwds)
  }
  if (is.matrix(dwdss)){
    dwdss <- as.vector(dwdss)
  }

  fphi <- chebbasisgen(stock,deg,lb,ub)
  sphi <- chebbasisgen(stock,deg,lb,ub,dorder=1)

  nsqr <- (((delta-dsdotds)^2)*fphi - sdot*(delta-dsdotds)*sphi - dsdotdss*sdot*fphi)
  nsqr2 <- dwdss*sdot*(delta-dsdotds) + dwds*dsdotdss*sdot
  if (deg == length(stock)){
    coeff <- t(solve(nsqr,nsqr2))
    res <- list(degree = deg, lowerB = lb, upperB = ub, delta = delta, coefficient = coeff)
  }
  else if (deg < length(stock)){
    coeff <- t(solve(t(nsqr)%*%nsqr, t(nsqr)%*%nsqr2))
    res <- list(degree = deg, lowerB = lb, upperB = ub, delta = delta, coefficient = coeff)
  }
  return(res)
}
#' Simulation of Pdot-approximation
#'
#' @description The function provides the Pdot-approximation simulation.
#' @param pdotcoeff An approximation result from \code{pdotaprox} function
#' @param stock An array of stock
#' @param sdot An array of ds/dt, \eqn{\dot{s}=\frac{ds}{dt}}
#' @param dsdotds An array of d(sdot)/ds, \eqn{\frac{d \dot{s}}{d s}}
#' @param wval An array of \eqn{W}-value
#' @param dwds An array of dw/ds, \eqn{\frac{dW}{ds}}
#' @details Let \eqn{\hat{\beta}} be the approximation coefficent from the results of \code{pdotaprox} function.
#' The estimated shadow price (accounting) price of stock over the given approximation intervals of
#' \eqn{s \in [a,b]}, \eqn{\hat{p}} can be calculated as:\cr
#' \cr
#' \eqn{\hat{p} = \frac{ W_{s} + \mathbf{\mu \beta} }{ \delta - \dot{s}_{s} } }.\cr
#' \cr
#' The estimated inclusive wealth is:\cr
#' \cr
#' \eqn{IW = \hat{p}s }. \cr
#' \cr
#' The estimated value function is:\cr
#' \cr
#' \eqn{ \hat{V} = \frac{1}{\delta} \left( W + \hat{p} \dot{s} \right)  }.\cr
#' \cr
#' For more detils see Fenichel and Abbott (2014) and Fenichel et al. (2016).
#' @return A list of approximation resuts: shadow (accounting) prices, inclusive wealth, and Value function
#' @examples
#' ## 1-D Reef-fish example: see Fenichel and Abbott (2014)
#' data("GOM")
#' nodes <- chebnodegen(param$nodes,param$lowerK,param$upperK)
#' simuDataPdot <- cbind(nodes,sdot(nodes,param),
#'                       dsdotds(nodes,param),dsdotdss(nodes,param),
#'                       dwds(nodes,param),dwdss(nodes,param))
#' Aspace <- aproxdef(param$order,param$lowerK,param$upperK,param$delta)
#' pdotC <- pdotaprox(Aspace,simuDataPdot[,1],simuDataPdot[,2],
#'                    simuDataPdot[,3],simuDataPdot[,4],
#'                    simuDataPdot[,5],simuDataPdot[,6])
#' GOMSimPdot <- pdotsim(pdotC,simuDataPdot[,1],simuDataPdot[,2],
#'                       simuDataPdot[,3],profit(nodes,param),simuDataPdot[,5])
#'
#' # Shadow Price
#' plotgen(GOMSimPdot, xlabel="Stock size, s", ylabel="Shadow price")
#'
#' # Value function and profit
#' plotgen(GOMSimPdot,ftype="vw",
#'         xlabel="Stock size, s",
#'         ylabel=c("Value Function","Profit"))
#' @seealso \code{\link{pdotaprox}}
#' @references Fenichel, Eli P. and Joshua K. Abbott. (2014) "\href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Natural Capital: From Metaphor to Measurement}."
#'              \emph{Journal of the Association of Environmental Economists}. 1(1/2):1-27.\cr
#' Fenichel, Eli P., Joshua K. Abbott, Jude Bayham, Whitney Boone, Erin M. K. Haacker, and Lisa Pfeiffer. (2016) "\href{http://www.pnas.org/content/113/9/2382}{Measuring the Value of Groundwater and Other Forms of Natural Capital}."
#'              \emph{Proceedings of the National Academy of Sciences }.113:2382-2387.
#' @export
pdotsim <- function(pdotcoeff,stock,sdot,dsdotds,wval,dwds){
  deg <- pdotcoeff[["degree"]]
  lb <- pdotcoeff[["lowerB"]]
  ub <- pdotcoeff[["upperB"]]
  delta <- pdotcoeff[["delta"]]
  coeff <- t(pdotcoeff[["coefficient"]])
  nnodes <- length(stock)

  dd <- length(deg)

  accp <- matrix(rep(0,nnodes*dd), ncol=dd)
  iw <- accp
  vhat <- accp
  for (ni in 1:nnodes){
    sti <- stock[ni]
    pdoti <- chebbasisgen(sti,deg,lb,ub)%*%coeff
    accp[ni,] <- (dwds[ni]+pdoti)/(delta - dsdotds[ni])
    iw[ni,] <- accp[ni,]*sti
    vhat[ni,] <- (wval[ni] + accp[ni,]*sdot[ni])/delta
  }
  res <- list(shadowp = accp, iw = iw, vfun = vhat, stock = as.matrix(stock,ncol=1), wval=wval)
  return(res)
}
#' Plot Generator for Shadow Price or Value Function
#'
#' @description The function draws shadowp or vfun-w plot from the simulation results of \code{vsim}, \code{psim}, or \code{pdotsim}.
#' @param simres A simulation results from \code{vsim}, \code{psim}, or \code{pdotsim}
#' @param ftype Plot type (ftype=NULL (default) or ftype="p" for shadow price; ftype="vw" for vfun-w plot)
#' @param whichs A pisitive integer for indicating a specific stock for multi-sotck cases (ftype=NULL (default) or 1<= whichs <= the number of stocks)
#' @param tvar An array of time variable if simulation result is a time-base simulation
#' @param xlabel A character for x-label of a plot (xlabel=NULL (default); "Stock" or "Time")
#' @param ylabel An array of characters for y-label of a plot (ylabel=NULL (default); "Shadow Price", "Value Function" or "W-value")
#' @details This function provides an one-dimensional plot for "shadow price-stock", "shadow price-time", "Value function-stock", "Value function-time", "Value function-stock-W value", or "Value function-time-W value" depending on input arguments.
#' @return A plot of approximation resuts: shadow (accounting) prices, inclusive wealth, and Value function
#' @examples
#' ## 1-D Reef-fish example: see Fenichel and Abbott (2014)
#' data("GOM")
#' nodes <- chebnodegen(param$nodes,param$lowerK,param$upperK)
#' simuDataP <- cbind(nodes,sdot(nodes,param),
#'                    dsdotds(nodes,param),dwds(nodes,param))
#' Aspace <- aproxdef(param$order,param$lowerK,param$upperK,param$delta)
#'
#' # p-approximation
#' pC <- paprox(Aspace,simuDataP[,1],simuDataP[,2],
#'              simuDataP[,3],simuDataP[,4])
#'
#' # Without prividing W-value
#' GOMSimP <- psim(pC,simuDataP[,1])
#' # With W-value
#' GOMSimP2 <- psim(pC,simuDataP[,1],profit(nodes,param),simuDataP[,2])
#'
#' # Shadow price-Stock plot
#' plotgen(GOMSimP)
#' plotgen(GOMSimP,ftype="p")
#' plotgen(GOMSimP,xlabel="Stock Size, S", ylabel="Shadow Price (USD/Kg)")
#'
#' # Value-Stock-W plot
#' plotgen(GOMSimP2,ftype="vw")
#' plotgen(GOMSimP2,ftype="vw",xlabel="Stock Size, S", ylabel="Value Function")
#' plotgen(GOMSimP2,ftype="vw",xlabel="Stock Size, S", ylabel="Value Function")
#'
#'## 2-D Prey-Predator example
#' data("lvdata")
#' aproxdeg <- c(20,20)
#' lower <- c(0.1,0.1)
#' upper <- c(1.5,1.5)
#' delta <- 0.03
#' lvspace <- aproxdef(aproxdeg,lower,upper,delta)
#' lvaproxc <- vaprox(lvspace,lvaproxdata)
#' lvsim <- vsim(lvaproxc,lvsimdata.time[,2:3])
#'
#' # Shadow price-Stock plot
#' plotgen(lvsim)
#' plotgen(lvsim,ftype="p")
#' plotgen(lvsim,whichs=2,xlabel="Stock Size, S",ylabel="Shadow Price (USD/Kg)")
#'
#' # Shadow price-time plot
#' plotgen(lvsim,whichs=2,tvar=lvsimdata.time[,1])
#'
#' # Value Function-Stock plot
#' plotgen(lvsim,ftype="vw")
#' plotgen(lvsim,ftype="vw",whichs=2,
#'         xlabel="Stock Size, S",ylabel="Shadow Price (USD/Kg)")
#'
#' # Value Function-time plot
#' plotgen(lvsim,ftype="vw",tvar=lvsimdata.time[,1])
#' plotgen(lvsim,ftype="vw",whichs=2,tvar=lvsimdata.time[,1],
#'         xlabel="Stock Size, S",ylabel="Shadow Price (USD/Kg)")
#' @seealso \code{\link{vsim}, \link{psim}, \link{pdotsim}}
#' @export
plotgen <- function(simres, ftype = NULL, whichs = NULL, tvar = NULL, xlabel = NULL, ylabel = NULL){
  ## price-stock curve
  if(is.null(ftype) == 1){
    ftype <- "p"
  }

  ## stock index
  nums <- dim(simres[["stock"]])[2]
  if(is.null(whichs)==1){
    sidx <- 1
  }
  else if(whichs > nums || whichs < 1){
    stop("whichs is a positive integer between 1 and number of stocks, or NULL!")
  }
  else{
    sidx <- whichs
  }

  if(ftype == "p"){
    xval <- simres[["stock"]][,sidx]
    yval <- simres[["shadowp"]][,sidx]

    xname <- "Stock"
    if(is.null(tvar)!=1){
      xval <- tvar
      xname <- "Time"
    }
    yname <- "Shadow Price"

    if(is.null(xlabel)!=1){
      xname = xlabel
    }
    if(is.null(ylabel)!=1){
      yname = ylabel
    }
    plot(xval,yval,xlab=xname,ylab=yname,type='l',lwd=2)
  }
  ## value-w curve
  else if(ftype == "vw"){
    xval <- simres[["stock"]][,sidx]
    y1val <- simres[["vfun"]][,1]
    y2val <- simres[["wval"]]

    xname <- "Stock"
    if(is.null(xlabel)!=1){
      xname <- xlabel
    }
    if(is.null(tvar)!=1){
      xval <- tvar
      xname <- "Time"
    }

    y1name <- "Value Function"
    if(is.null(ylabel[1])!=1){
      y1name <- ylabel[1]
    }

    y2name <- "W-value"
    if(length(ylabel)==2){
      y2name <- ylabel[2]
    }

    if(is.character(y2val)){
      plot(xval,y1val,xlab=xname,ylab=y1name,col="blue",type='l',lwd=2)
    }
    else{
      par(mar=c(5,4,4,5)+.1)
      plot(xval,y1val,xlab=xname,ylab=y1name,col="blue",type='l',lwd=2)
      par(new=TRUE)
      plot(xval,y2val,type="l",col="black",lwd=2,xaxt="n",yaxt="n",xlab="",ylab="")
      axis(4)
      mtext(y2name,side=4,line=3)
      legend("topleft",col=c("blue","black"),lty=1,legend=c("V-Fun","W"),bty = "n")
    }
  }
  else
    stop("ftype should be NULL, p, or vw!")
}
NULL
