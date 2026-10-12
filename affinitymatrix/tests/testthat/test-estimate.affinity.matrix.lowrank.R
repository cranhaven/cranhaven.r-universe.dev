# Path
path = getwd()

# Parameters
Kx = 4; Ky = 4; # number of matching variables on both sides of the market
N = 500 # sample size
mu = rep(0, Kx+Ky) # means of the data generating process
Sigma = matrix(c(1, 0.326, 0.1446, -0.0668, 0.5712, 0.4277, 0.1847, -0.2883, 0.326, 1, -0.0372, 0.0215, 0.2795, 0.8471, 0.1211, -0.0902, 0.1446, -0.0372, 1, -0.0244, 0.2186, 0.0636, 0.1489, -0.1301, -0.0668, 0.0215, -0.0244, 1, 0.0192, 0.0452, -0.0553, 0.2717, 0.5712, 0.2795, 0.2186, 0.0192, 1, 0.3309, 0.1324, -0.1896, 0.4277, 0.8471, 0.0636, 0.0452, 0.3309, 1, 0.0915, -0.1299, 0.1847, 0.1211, 0.1489, -0.0553, 0.1324, 0.0915, 1, -0.1959, -0.2883, -0.0902, -0.1301, 0.2717, -0.1896, -0.1299, -0.1959, 1),
               nrow=Kx+Ky) # (normalized) variance-covariance matrix of the data generating process
labels_x = c("Educ.", "Age", "Height", "BMI") # labels for men's matching variables
labels_y = c("Educ.", "Age", "Height", "BMI") # labels for women's matching variables

# Sample
data = MASS::mvrnorm(N, mu, Sigma) # generating sample
X = data[,1:Kx]; Y = data[,Kx+1:Ky] # men's and women's sample data
w = sort(runif(N-1)); w = c(w,1) - c(0,w) # sample weights

# Main estimation
res = estimate.affinity.matrix.lowrank(X, Y, w = w, tol_level = 1e-03, nB = 100, Nfolds = 3)
