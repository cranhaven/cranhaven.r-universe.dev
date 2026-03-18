\name{basis}
\alias{basis}
\title{slp: Pre-computed basis sets}
\description{
    Shared name for a number of pre-computed basis sets included with the 
    \code{slp} package. For large \code{N}, significant speed-ups can be 
    obtained by pre-computing the basis set and simply loading it from disk. 
}
\format{Each file named \code{basis_N_XXX_W_X_K_XX.RData},
    with the \code{X} entries integers indicating the \code{N}, \code{W} (in
    \code{df/}year) and \code{K} parameters, can be loaded via \code{data(...)}
    as a \code{basis} object in the environment of your choice.

    Currently, these pre-computed bases are used as speed-up aids within 
    \code{{slp(...)}} and \code{{s(..., bs='slp', ...)}}.
    A full list of available bases can be obtained by examining
    \code{slpSavedObjects}, an additional \code{data(...)} object included
    with the package. 
}
\keyword{datasets}
