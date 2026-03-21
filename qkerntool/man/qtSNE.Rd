\name{qtSNE}
\alias{qtSNE}
\alias{qtSNE,matrix-method}
\alias{qtSNE,qkernmatrix-method}
\alias{qtSNE,cndkernmatrix-method}
\title{qKernel t-Distributed Stochastic Neighbor Embedding}
\description{
Wrapper for the qkernel t-distributed stochastic neighbor embeddingg. qtSNE is a method for constructing a low dimensional embedding of high-dimensional data, distances or similarities.
}
\usage{

\S4method{qtSNE}{matrix}(x,kernel = "rbfbase", qpar = list(sigma = 0.1, q = 0.9),
        initial_config = NULL, no_dims=2, initial_dims=30, perplexity=30, max_iter= 1300,
         min_cost=0, epoch_callback=NULL, epoch=100, na.action = na.omit, ...)
\S4method{qtSNE}{cndkernmatrix}(x,initial_config = NULL, no_dims=2, initial_dims=30,
        perplexity=30, max_iter = 1000, min_cost=0, epoch_callback=NULL,epoch=100)
\S4method{qtSNE}{qkernmatrix}(x,initial_config = NULL, no_dims=2, initial_dims=30,
        perplexity=30, max_iter = 1000, min_cost=0, epoch_callback=NULL,epoch=100)
}
\arguments{
\item{x}{the matrix of data to be clustered or a kernel Matrix of class
    \code{qkernmatrix} or \code{cndkernmatrix}.}
\item{kernel}{the kernel function used in computing the affinity matrix.
    This parameter can be set to any function, of class kernel, which computes a kernel function value between two
    vector arguments. kernlab provides the most popular kernel functions
    which can be used by setting the kernel parameter to the following strings:
    \itemize{
      \item \code{rbfbase} Radial Basis qkernel function "Gaussian"
      \item \code{nonlbase} Non Linear qkernel function
      \item \code{laplbase} Laplbase qkernel function
      \item \code{ratibase} Rational Quadratic qkernel function
      \item \code{multbase} Multiquadric qkernel function
      \item \code{invbase} Inverse Multiquadric qkernel function
      \item \code{wavbase} Wave qkernel function
      \item \code{powbase} Power qkernel function
      \item \code{logbase} Log qkernel function
      \item \code{caubase} Cauchy qkernel function
      \item \code{chibase} Chi-Square qkernel function
      \item \code{studbase} Generalized T-Student qkernel function
      \item \code{nonlcnd} Non Linear cndkernel function
      \item \code{polycnd} Polynomial cndkernel function
      \item \code{rbfcnd} Radial Basis cndkernel function "Gaussian"
      \item \code{laplcnd} Laplacian cndkernel function
      \item \code{anocnd} ANOVA cndkernel function
      \item \code{raticnd} Rational Quadratic cndkernel function
      \item \code{multcnd} Multiquadric cndkernel function
      \item \code{invcnd} Inverse Multiquadric cndkernel function
      \item \code{wavcnd} Wave cndkernel function
      \item \code{powcnd} Power cndkernel function
      \item \code{logcnd} Log cndkernel function
      \item \code{caucnd} Cauchy cndkernel function
      \item \code{chicnd} Chi-Square cndkernel function
      \item \code{studcnd} Generalized T-Student cndkernel function

    }
    The kernel parameter can also be set to a user defined function of
    class kernel by passing the function name as an argument.
  }

  \item{qpar}{a character string or the list of hyper-parameters (kernel parameters).
    The default character string \code{list(sigma = 2, q = 0.9)} uses a heuristic to determine a
    suitable value for the width parameter of the RBF kernel.
    The second option \code{"local"} (local scaling) uses a more advanced heuristic
     and sets a width parameter for every point in the data set. This is
    particularly useful when the data incorporates multiple scales.
    A list can also be used containing the parameters to be used with the
    kernel function. Valid parameters for existing kernels are :
    \itemize{
      \item \code{sigma}  for the Radial Basis qkernel function "rbfbase" , the Laplacian qkernel function "laplbase" the Cauchy qkernel function "caubase" and for the ANOVA cndkernel function "anocnd".
      \item \code{alpha}  for the Non Linear qkernel function "nonlbase",for the Non Linear cndkernel function "nonlcnd",and for the Polynomial cndkernel function "polycnd".
      \item \code{c}  for the Rational Quadratic qkernel function "ratibase" , the Multiquadric qkernel function "multbase", the Inverse Multiquadric qkernel function "invbase",for the Polynomial cndkernel function "polycnd",for the Rational Quadratic cndkernel function "raticnd" , the Multiquadric cndkernel function "multcnd" and the Inverse Multiquadric cndkernel function "invcnd".
      \item \code{d}   for qkernel function "powbase" , the Log qkernel function "logbase", the Generalized T-Student qkernel function "studbase", for the Polynomial cndkernel function "polycnd", for the ANOVA cndkernel function "anocnd",for the d cndkernel function "powcnd" , the Log cndkernel function "logcnd" and the Generalized T-Student cndkernel function "studcnd".
      \item \code{theta}  for the Wave qkernel function "wavbase" and for the Wave cndkernel function "wavcnd".
      \item \code{gamma}  for the Chi-Square qkernel function "chibase",for the Radial Basis cndkernel function "rbfcnd" and the Laplacian cndkernel function "laplcnd" and the Cauchy cndkernel function "caucnd".
      \item \code{q}  For all qkernel Function.
       where length is the length of the strings considered, lambda the
       decay factor and normalized a logical parameter determining if the
       kernel evaluations should be normalized.
    }

    Hyper-parameters for user defined kernels can be passed through the
    qkpar parameter as well.}

\item{initial_config}{An intitial configure about x (default: NULL)}

\item{no_dims}{the dimension of the resulting embedding. (default: 2)}

\item{initial_dims}{ The number of dimensions to use in reduction method. (default: 30)}

\item{perplexity}{Perplexity parameter}
\item{max_iter}{Number of iterations (default: 1300)}
\item{min_cost}{The minimum cost for every object after the final iteration}
\item{epoch_callback}{A callback function used after each epoch (an epoch here means a set number of iterations)}
\item{epoch}{The interval of the number of iterations displayed (default: 100)}
\item{na.action}{the action to perform on NA}

\item{...}{Other arguments that can be passed to qtSNE}
}
\value{
	 \code{qtSNE} gives out an S4 object which is a LIST with components
\item{dimRed}{ Matrix containing the new representations for the objects after qtSNE}
\item{cndkernf}{The kernel function used}
}

\details{
When the initial_config argument is specified, the algorithm will automatically enter the final momentum stage. This stage has less large scale adjustment to the embedding, and is intended for small scale tweaking of positioning. This can greatly speed up the generation of embeddings for various similar X datasets, while also preserving overall embedding orientation.
}


\examples{\dontrun{
#use iris data set
data(iris)
testset <- sample(1:150,20)
train <- as.matrix(iris[,1:4])

colors = rainbow(length(unique(iris$Species)))
names(colors) = unique(iris$Species)
#for matrix
ecb = function(x,y){
  plot(x,t='n');
  text(x,labels=iris$Species, col=colors[iris$Species])
}
kpc2 <- qtSNE(train, kernel = "rbfbase", qpar = list(sigma=1,q=0.8),
              epoch_callback = ecb, perplexity=10, max_iter = 500)

}
}

\references{
Maaten, L. Van Der, 2014. Accelerating t-SNE using Tree-Based Algorithms. Journal of Machine Learning Research, 15, p.3221-3245.

van der Maaten, L.J.P. & Hinton, G.E., 2008. Visualizing High-Dimensional Data Using t-SNE. Journal of Machine Learning Research, 9, pp.2579-2605.
}
\author{Yusen Zhang \cr \email{yusenzhang@126.com}}
