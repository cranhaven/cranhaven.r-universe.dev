\name{sbpsi}
\alias{sbpsi}
\alias{sbpsi.poly}
\alias{sbpsi.sing}
\alias{sbpsi.sphe}
\alias{sbpsi.generic}
\alias{sbmodelnames}
\title{Model Specification Functions}
\description{
  \code{sbpsi.poly} and \code{sbpsi.sing} are \eqn{\psi} functions to
  specify a polynomial model and a singular model, respectively.
}
\usage{

sbpsi.poly(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)

sbpsi.sing(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)

sbpsi.sphe(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)

sbpsi.generic(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE,zfun,eps=0.01)

sbmodelnames(m=1:3,one.sided=TRUE,two.sided=FALSE,rev.sided=FALSE,
  poly,sing,poa,pob,poc,pod,sia,sib,sic,sid,sphe,pom,sim)


}
\arguments{
  \item{beta}{numeric vector of parameters;
    \eqn{\beta_0}=\code{beta[1]}, \eqn{\beta_1}=\code{beta[2]},...
    \eqn{\beta_{m-1}}=\code{beta[m]}, where \eqn{m} is the number of
    parameters.
  }
  \item{s}{\eqn{\sigma_0^2}.}
  \item{k}{numeric to specify the order of derivatives.}
  \item{sp}{\eqn{\sigma_p^2}.}
  \item{lambda}{a numeric of specifying the type of p-values; Bayesian (lambda=0) Frequentist (lambda=1).}
  \item{aux}{auxiliary parameter. Currently not used.}
  \item{check}{logical for boundary check.}
  \item{zfun}{z-value function with (s,beta) as parameters.}
  \item{eps}{delta for numerical computation of derivatives.}
  \item{m}{numeric vector to specify the numbers of parameters.}
  \item{one.sided}{logical to include poly and sing models.}
  \item{two.sided}{logical to include poa and sia models.}
  \item{rev.sided}{logical to include pob and sib models.}
  \item{poly}{maximum number of parameters in poly models.}
  \item{sing}{maximum number of parameters in sing models.}
  \item{sphe}{maximum number of parameters in sphe models.}
  \item{poa}{maximum number of parameters in poa models.}
  \item{pob}{maximum number of parameters in pob models.}
  \item{poc}{maximum number of parameters in poc models.}
  \item{pod}{maximum number of parameters in pod models.}
  \item{sia}{maximum number of parameters in sia models.}
  \item{sib}{maximum number of parameters in sib models.}
  \item{sic}{maximum number of parameters in sic models.}
  \item{sid}{maximum number of parameters in sid models.}
  \item{pom}{maximum number of parameters in pom models.}
  \item{sim}{maximum number of parameters in sim models.}
}
\details{
  For \eqn{k=1}, the \code{sbpsi} functions return their \eqn{\psi} function
  values at \eqn{\sigma^2=\sigma_0^2}.  Currently, four types of
  \code{sbpsi} functions are
  implemented. \code{sbpsi.poly} defines the polynomial model;
  \deqn{\psi(\sigma^2 | \beta) =
    \sum_{j=0}^{m-1} \beta_j \sigma^{2j}}
  for \eqn{m\ge1}.
  \code{sbpsi.sing} defines the singular model;
  \deqn{\psi(\sigma^2 | \beta) = \beta_0 +
    \sum_{j=1}^{m-2} \frac{\beta_j \sigma^{2j}}{1 + \beta_{m-1}(\sigma-1)}}
  for \eqn{m\ge3} and \eqn{0\le\beta_{m-1}\le1}.
  \code{sbpsi.sphe} defines the spherical model; currently the number of
  parameters must be $m=3$.
  \code{sbpsi.generic} is a generic sbpsi function for specified \code{zfun}.

  For \eqn{k>1}, the \code{sbpsi} functions return values extrapolated at
  \eqn{\sigma^2=\sigma_p^2} using derivatives up to order \eqn{k-1}
  evaluated at \eqn{\sigma^2=\sigma_0^2};
  \deqn{q_k = \sum_{j=0}^{k-1} \frac{(\sigma_p^2-\sigma_0^2)^j}{j!}
    \frac{d^j \psi(x|\beta)}{d x^j}\Bigr|_{\sigma_0^2},}
  which reduces to \eqn{\psi(\sigma_0^2|\beta)} for \eqn{k=1}. In the
  \code{\link{summary.scaleboot}}, the AU p-values are defined
  by \eqn{p_k = 1-\Phi(q_k)} for \eqn{k\ge1}.
}
\value{
  \code{sbpsi.poly} and \code{sbpsi.sing} are examples of a sbpsi
  function; users can develop their own sbpsi functions for better
  model fitting by preparing \code{sbpsi.foo} and \code{sbini.foo}
  functions for model \code{foo}.
  If check=FALSE, a sbpsi function returns
  the \eqn{\psi} function value or the extrapolation value.
  If check=TRUE, a sbpsi function returns NULL when all
  the elements of beta are included in the their valid
  intervals. Otherwise, a \code{sbpsi} function returns a list with components
  \code{beta} for the parameter value being modified to be on a boundary
  of the interval and \code{mask}, a logical vector indicating which
  elements are not on the boundary.

  \code{sbmodelnames} returns a character vector of model names.
}
\author{Hidetoshi Shimodaira}
\seealso{\code{\link{sbfit}}.}
\keyword{ models }
