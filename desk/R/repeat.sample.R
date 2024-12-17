repeat.sample = function(x, true.par, omit = 0, mean = 0, sd = 1, rep = 100,
                       xnew = x, sig.level = 0.05, seed = NULL){

# Function to calculate variances of rows (faster than apply(u,2,var))
col.var = function(x, na.rm = FALSE, unbiased = TRUE) {
  N = colSums(!is.na(x), F, 1)
  c = if (unbiased) N-1 else N
  sweep(x, 2:length(dim(x)), colMeans(x,na.rm,1))
  (colSums(x^2, na.rm, 1) - colSums(x, na.rm, 1)^2/N) / c
  }

##########################
## Pre-calculations     ##
##########################

# Static data
set.seed(seed)
x = as.matrix(x)
X = cbind(1, x) # design matrix regular data
n = nrow(X) # Sample size
k = ncol(X) # Number of parameters
xnew = as.matrix(xnew)
xnew = cbind(1, xnew) # design matrix prediction data

# Calculate t-value
t.val = qt(1-sig.level/2, n-k)

# Create sample names and coefficient names
if (is.null(names(true.par))){
  coef.names = c("alpha", if(k == 2){"beta"} else {paste("beta", 1:(k-1), sep = "")})
  } else {
    coef.names = names(true.par)
  }
if(omit != 0){
  coef.names = coef.names[-(omit+1)]
}
sample.names = paste("SMPL",1:rep, sep = "")

# Random draw of n errors u over repeated samples
u = replicate(rep, rnorm(n, mean, sd))
colnames(u) = sample.names
var.u = col.var(u)

# Generate simulated real y values (with error)
y = matrix(NA, nrow = n, ncol = rep, dimnames = list(NULL, sample.names))
for (i in 1:rep){
  y[,i] = X %*% true.par + u[,i]
}

# Calculate static data conditional on omitted variables
if(omit == 0){
  B = rep(0,k)
  XXi = chol2inv(chol(t(X)%*%X))
  M = diag(n) - X %*% XXi %*% t(X)
  n.omit = 0
} else {
  if (omit < 0 || omit > k-1) stop("Argument omit is misspecified, see help.")
  omit = omit + 1 # first variable is second column
  Xo = X[,omit] # copy ommited variables to Xo
  X = X[,-omit] # delete omitted variables from X
  xnew = xnew[,-omit] # delete omitted variables from xnew
  XXi = chol2inv(chol(t(X)%*%X))
  M = diag(n) - X %*% XXi %*% t(X)
  true.par.o = true.par[omit] #copy ommited parameter to true.par.o
  true.par = true.par[-omit] #delete ommited parameter from true.par
  B = as.vector(XXi %*% t(X) %*% Xo %*% true.par.o) # Bias term of coefs
  n.omit = length(omit)
}

# Name the rows of the bias vector
names(B) = coef.names

##########################
## Init data structures ##
##########################

# Regular x-data:
#################

# Estimated y values (regression line, without error)
y.fit = matrix(NA, nrow = n, ncol = rep, dimnames = list(NULL, sample.names))

# Residuals
resid = matrix(NA, nrow = n, ncol = rep, dimnames = list(NULL, sample.names))

# Estimated parameters
coef = matrix(NA, nrow = k - n.omit, ncol = rep, dimnames = list(coef.names, sample.names))

# Estimated standard deviation of estimated parameters
se.coef = coef

# Estimated standard deviation of error term (sigma hat)
s.hat = rep(NA, rep)

# Estimated variances and covariances of all estimated parameters
cov.coef = array(NA, dim = c(k - n.omit, k - n.omit, rep), dimnames = list(coef.names, coef.names, sample.names))

# Confidence intervals
ci = array(NA, dim = c(k - n.omit, 2, rep), dimnames = list(coef.names, c("lower","upper"), sample.names))

# Is true parameter value outside confidence interval?
is.out.ci = coef

# Prediction data xnew:
#######################

# True future y value (true line plus true error, simulated)
y0 = matrix(NA, nrow = dim(xnew)[1], ncol = rep, dimnames = list(NULL, sample.names))

# Estimated future y value (regression line without error), Point prediction
y0.fit = matrix(NA, nrow = dim(xnew)[1], ncol = rep, dimnames = list(NULL, sample.names)) # Punktprognose

# Estimated standard deviation of prediction error y0 - y0.fit
sd.pe = matrix(NA, nrow = dim(xnew)[1], ncol = rep, dimnames = list(as.character(1:dim(xnew)[1]), sample.names))

# Prediction interval
pi = array(NA, dim = c(dim(xnew)[1], 2, rep), dimnames = list(as.character(1:dim(xnew)[1]), c("lower","upper"), sample.names))

# Is true parameter value outside prediction interval?
is.out.pi = y0

##########################
## Fill data structures ##
##########################

# Regular x-data:
#################

# Loop over all samples drawn (rep)
for (i in 1:rep){

  # Residuals
  resid[,i] = M %*% y[,i] # M in the regular case and M1 if omitted variable

  # Estimated y values (regression line, without error)
  y.fit[,i] = y[,i] - resid[,i]

  # Estimated parameters
  coef[,i] = XXi %*% t(X) %*% y[,i] # X in the regular case and X1 if omitted variable

  # Estimated standard deviation of error term (sigma hat)
  s.hat[i] = as.numeric(t(resid[,i]) %*% resid[,i] / (n-k-n.omit))

  # Estimated variances and covariances of all estimated parameters
  cov.coef[,,i] = s.hat[i] * XXi

  # Estimated standard deviation of estimated parameters
  se.coef[,i] = sqrt(diag(cov.coef[,,i]))

  # Confidence intervals
  c = t.val * se.coef[,i]
  ci[,,i] = cbind(coef[,i] - c, coef[,i] + c)
  is.out.ci[,i] = (true.par < ci[,"lower",i]) | (true.par > ci[,"upper",i])

  # Prediction data xnew:
  #######################
  # True future y value (true line plus true error, simulated)
  y0[,i] = xnew %*% true.par + rnorm(dim(xnew)[1], mean, sd)

  # Point prediction: Estimated future y value (regression line without error)
  y0.fit[,i] = as.vector(xnew %*% coef[,i]) # Vector of fitted y-values

  # Est. standard deviation of prediction error y0 - y0.fit (Vector of quadr. forms)
  sd.pe[,i] = sqrt(diag(xnew %*% cov.coef[,,i] %*% t(xnew)) + s.hat[i])

  # Prediction interval
  c = sd.pe[,i] %o% t.val
  pi[,,i] = cbind(y0.fit[,i] - c, y0.fit[,i] + c)
  is.out.pi[,i] = (y0[,i] < pi[,"lower",i]) | (y0[,i] > pi[,"upper",i])
}

# Calculate percentage of intervals including the true parameter
outside.ci = rowSums(is.out.ci)/rep
outside.pi = rowSums(is.out.pi)/rep

out = list( "x" = x,
            "y" = y,
            "fitted" = y.fit,
            "coef" = coef,
            "true.par" = true.par,
            "u" = u,
            "residuals" = resid,
            "sig.squ" = s.hat,
            "var.u" = var.u,
            "se" = se.coef,
            "vcov.coef" = cov.coef,
            "confint" = ci,
            "outside.ci" = outside.ci,
            "y0" = y0,
            "y0.fitted" = y0.fit,
            "predint" = pi,
            "sd.pe" = sd.pe,
            "outside.pi" = outside.pi,
            "bias.coef" = B
            )

attr(out, "title") = NULL
attr(out, "type") = "repsamp"
attr(out, "details") = F
#attr(out, "c.type") = c.type
class(out) = c("desk")

return(out)
}
