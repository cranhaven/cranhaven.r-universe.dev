\name{MultiVarSel-package}
\alias{MultiVarSel-package}
\alias{MultiVarSelect}
\docType{package}
\title{Package}
\description{
MultiVarSel consists of four functions: "whitening.R", "whitening_test.R",
"whitening_choice.R" and "variable_selection.R".
For further information on how to use these functions, 
we refer the reader to the vignette of the package.
}
\details{
This package consists of four functions: "whitening.R", "whitening_test.R",
"whitening_choice.R" and "variable_selection.R".
For further information on how to use these functions, 
we refer the reader to the vignette of the package.
}
\author{
Marie Perrot-Dockes, Celine Levy-Leduc, Julien Chiquet

Maintainer: Marie Perrot-Dockes <marie.perrocks@gmail.com>
}
\references{
M. Perrot-Dockes et al. "A multivariate variable selection approach 
for analyzing LC-MS metabolomics data", arXiv:1704.00076
}
\keyword{ package }

\examples{
data("copals_camera")
Y <- scale(Y[, 1:50])
X <- model.matrix(~ group + 0)
residuals <- lm(as.matrix(Y) ~ X - 1)$residuals
S12_inv <- whitening(residuals, "AR1", pAR = 1, qMA = 0)
Frequencies <- variable_selection(
  Y = Y, X = X,
  square_root_inv_hat_Sigma = S12_inv,
  nb_repli = 10,  nb.cores = 1, parallel = FALSE
)
\dontrun{
# Parallel computing
require(doMC)
registerDoMC(cores=4)
Freqs <- variable_selection(Y,X,square_root_inv_hat_Sigma,
                    nb_repli=10,parallel=TRUE,nb.cores=4)
}                   
}
