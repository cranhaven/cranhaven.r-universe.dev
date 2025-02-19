\encoding{UTF-8}
\name{cortical}
\alias{cortical}
\title{Brain Shape Data}
\description{
  Axial brain slices gathered via magnetic resonance images (MRI) with 500
  points on each outline, for 30 schizophrenia patients and 38 healthy
  controls.
}
\usage{cortical}
\format{
  The \code{cortical} list has the following variables: 	
  \itemize{
    \item{\code{cortical$age}}{: 
      age, in years. 
    }
    \item{\code{cortical$group}}{:
      control patient (\code{Con}) or schizophrenia patient (\code{Scz}).
    }
    \item{\code{cortical$sex}}{:
      male (\code{1}) or female (\code{2}).
    }
    \item{\code{cortical$symm}}{:
      symmetry score obtained from raw 3D brain surface.
    }
    \item{\code{cortical$x} and \code{cortical$y}}{
      \code{x}, \code{y} coordinates of slice from brain
      surface that intersects the AC (anterior commissure) and PC (posterior
      commissure).
    }    
    \item{\code{cortical$r}}{
      500 radii from angular polar coordinates.
    }    
  }
}
\details{
  The data were gathered from a neuroscience study conducted at the
  University of British Columbia, Canada, and documented in Brignell
  \emph{et al.} (2010) and Martos and de Carvalho (2018). Each brain was
  registered into the so-called Talairach space so that brains can be
  compared on the same three-dimensional referential coordinate space.
}
\references{
  Brignell, C.J., Dryden, I.L., Gattone, S.A., Park, B., Leask, S.,
  Browne, W.J. and Flynn, S. (2010) Surface shape analysis, with an
  application to brain surface asymmetry in
  schizophrenia. \emph{Biostatistics}, \bold{11}, 609-630.
  
  Martos, G. & de Carvalho, M. (2018) Discrimination surfaces with
  application to region-specific brain asymmetry
  analysis. \emph{Statistics in Medicine}, \bold{37}, 1859-1873.
}
\examples{
  ## Martos and de Carvalho (2018; Fig 1 a)
  library(scales)
  data(cortical)
  m <- 500  
  n <- 68
  plot(cortical$r[,1] * cos(2 * pi * 1:m / m),
       cortical$r[,1] * sin(2 * pi * 1:m / m) , type = "l",
       col = alpha("gray", 1 / n), xlab = "z", ylab = "x")
  for(i in 2:n) 
  lines(cortical$r[, i] * cos(2 * pi * 1:m / m),
        cortical$r[, i] * sin(2 * pi * 1:m / m), type = "l",
        col = alpha("gray", i / n))
}
\keyword{Medical}
