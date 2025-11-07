#' Creates an object of class \code{fts.timedom}. This object corresponds to a sequence of linear operators.
#'
#' This class is used to describe a functional linear filter, i.e. a sequence of linear operators, each of which corresponds to a certain lag. 
#' Formally we consider an object of class \code{timedom} and add some basis functions. Depending on the context, we have different interpretations for the new object.
#' 
#' (I) In order to define operators which maps between two functions spaces, the we
#' interpret \code{A$operators} as coefficients in the basis function expansion of the
#' kernel of some finite rank operators \deqn{\mathcal{A}_k:\mathrm{span}(\code{basisY})\to\mathrm{span}(\code{basisX}).} The kernels are \eqn{a_k(u,v)=\boldsymbol{b}_1^\prime(u)\, A_k\, \boldsymbol{b}_2(v)}, where \eqn{\boldsymbol{b_1}(u)=(b_{11}(u),\ldots, b_{1d_1}(u))^\prime} and \eqn{\boldsymbol{b_2}(u)=(b_{21}(u),\ldots, b_{2d_1}(u))^\prime} are the basis functions provided by the arguments  basisX and basisY, respectively. Moreover, we consider lags \eqn{\ell_1<\ell_2<\cdots<\ell_K}. The object this function creates corresponds to the mapping \eqn{\ell_k \mapsto a_k(u,v)}.
#' 
#' (II) We may ignore basisX, and represent the linear mapping
#' \deqn{\mathcal{A}_k:\mathrm{span}(\code{basisY})\to R^{d_1},}
#' by considering \eqn{a_k(v):=A_k\,\boldsymbol{b}_2(v)} and \eqn{\mathcal{A}_k(x)=\int a_k(v)x(v)dv}.
#' 
#' (III) We may ignore basisY, and represent the linear mapping
#' \deqn{\mathcal{A}_k: R^{d_1}\to\mathrm{span}(\code{basisX}),}
#' by considering \eqn{a_k(u):=\boldsymbol{b}_1^\prime(u)A_k} and \eqn{\mathcal{A}_k(y)=a_k(u)y}.
#' 
#' @title Object of class \code{fts.timedom}
#' 
#' @param A an object of class timedom.
#' @param basisX an object of class \code{basis.fd} (see \code{\link[fda]{create.basis}})
#' @param basisY an object of class \code{basis.fd} (see \code{\link[fda]{create.basis}})
#' @return Returns an object of class  \code{fts.freqdom}. An object of class  \code{fts.freqdom} is a list containing the following components:
#' * \code{operators} \eqn{\quad} returns the array \code{A$operators}.
#' * \code{basisX} \eqn{\quad} returns basisX as given in the argument.
#' * \code{basisY} \eqn{\quad} returns basisY as given in the argument.
#' * \code{lags} \eqn{\quad} returns \code{A$lags}.
#' @seealso The multivariate equivalent in the \code{freqdom} package: \code{\link[freqdom]{timedom}}
#' @export
#' @keywords classes
fts.timedom = function (A, basisX, basisY=basisX)
{
	nX=basisX$nbasis
	nY=basisY$nbasis

	if(!is.timedom(A)) 
	stop("A must be an object of class timedom")  
	if(!is.basis(basisX))
	stop("basisX must be a functional basis")  

	if(!is.basis(basisY))
	stop("basisY must be a functional basis")  

	res = A
	res$basisX=basisX
	res$basisY=basisY
	class(res) = c("fts.timedom","timedom")
	res
}

