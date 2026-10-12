# Path
path = getwd()

# Parameters
K = 4 # number of matching variables
N = 500 # sample size
mu = rep(0, 2*K) # means of the data generating process
Sigma = matrix(c(1, -0.0992, 0.0443, -0.0246, -0.8145, 0.083, -0.0438, 0.0357, -0.0992, 1, 0.0699, -0.0043, 0.083, 0.8463, 0.0699, -0.0129, 0.0443, 0.0699, 1, -0.0434, -0.0438, 0.0699, 0.5127, -0.0383, -0.0246, -0.0043, -0.0434, 1, 0.0357, -0.0129, -0.0383, 0.6259, -0.8145, 0.083, -0.0438, 0.0357, 1, -0.0992, 0.0443, -0.0246, 0.083, 0.8463, 0.0699, -0.0129, -0.0992, 1, 0.0699, -0.0043, -0.0438, 0.0699, 0.5127, -0.0383, 0.0443, 0.0699, 1, -0.0434, 0.0357, -0.0129, -0.0383, 0.6259, -0.0246, -0.0043, -0.0434, 1),
               nrow=Kx+Ky) # (normalized) variance-covariance matrix of the data generating process with a block symmetric structure
labels = c("Sex", "Age", "Educ.", "Black") # labels for matching variables

# Sample
data = MASS::mvrnorm(N, mu, Sigma) # generating sample
X = data[,1:K]; Y = data[,K+1:K] # men's and women's sample data
w = sort(runif(N-1)); w = c(w,1) - c(0,w) # sample weights

# Main estimation
res = estimate.affinity.matrix.unipartite(X, Y, w = w, nB = 500)
