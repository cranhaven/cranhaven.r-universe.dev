\name{DynamicGP-package}
\alias{DynamicGP-package}
\docType{package}

\title{Modelling and Analysis of Dynamic Computer Experiments}

\description{For emulating dynamic computer experiments, three functions
  are included. The function \code{svdGP} fits full SVD-based GP model
  which is computationally demanding for large-scale dyanmic computer
  experiments. As is well known, the time complexity of fitting a GP
  model is \eqn{O(N^3)} where \eqn{N} is the number of training/design
  points. Since fitting a common GP model for really large \eqn{N} would
  be computationally burdensome, we fit local SVD-based GP models on a
  sequentially selected small neighborhood set for every test
  inputs. The function \code{knnsvdGP} fits K-nearest neighbor SVD-based
  GP models which selects neighborhood sets based on the Euclidean
  distance with repect to the test points. The function \code{lasvdGP}
  fits local approximate SVD-based GP model using the new algorithm
  proposed by Zhang et al. (2018).

  The lasvdGP is an extension of the local approximate GP (laGP) model
  developed by Gramacy and Lee (2015) for the emulation of large-scale
  scalar valued computer experiments. The neighborhood selection and
  SVD-based GP model fitting algorithm is suitable for
  parallelization. We use both the R package "parallel" and the OpenMP
  library for this task. The parallelization can achieve nearly linear
  speed since the procedure on each test point is independent and
  identical.

  For the inverse problem in dynamic computer experiments, we also
  provide three functions. The function \code{ESL2D} minimizes the
  expected squared \eqn{L_{2}} discrepancy between the target response
  and the simulator outputs to estimate the solution to the inverse
  problem, where the expectation is taken with respect to the predictive
  distribution of the \code{svdGP} model. A naive estimation approach
  \code{SL2D} simply minimizes the squared \eqn{L_{2}} discrepancy
  between the target response and the predicted mean response of the
  SVD-based GP model. The function \code{saEI} performs the squential
  design procedure for the inverse problem. It selects the follow-up
  design points as per an expected improvement criterion whose values
  are numerically approximated by the saddlepoint approximation
  technique. Details of the three methods for the inverse problem are
  provided in Chapter 4 of Zhang (2018).  }

\author{Ru Zhang \email{heavenmarshal@gmail.com},

  C. Devon Lin \email{devon.lin@queensu.ca},

  Pritam Ranjan \email{pritamr@iimidr.ac.in}}

\references{Gramacy, R. B. and Apley, D. W. (2015) \emph{Local Gaussian
    process approximation for large computer experiments}, Journal of
    Computational and Graphical Statistics 24(2), 561-578.

  Zhang, R., Lin, C. D. and Ranjan, P. (2018) \emph{Local Gaussian
    Process Model for Large-scale Dynamic Computer Experiments},
  Journal of Computational and Graphical Statistics, \cr DOI:
  10.1080/10618600.2018.1473778.

  Zhang, R. (2018) \emph{Modeling and Analysis of Dynamic Computer Experiments},
    PhD thesis, Queen's University, ON, Canada.
}

\keyword{GP model}
\keyword{package}
\keyword{parallelization}
\keyword{SVD}
