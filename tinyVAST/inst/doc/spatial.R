## ----include = FALSE----------------------------------------------------------
has_lattice = requireNamespace("lattice", quietly = TRUE)
has_pdp = requireNamespace("pdp", quietly = TRUE)
EVAL <- has_lattice && has_pdp
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = EVAL,
  purl = EVAL
)
start_time = Sys.time()
# Install locally
#  devtools::install_local( R'(C:\Users\James.Thorson\Desktop\Git\tinyVAST)', force=TRUE )
# Build
#  setwd(R'(C:\Users\James.Thorson\Desktop\Git\tinyVAST)'); devtools::build_rmd("vignettes/spatial.Rmd"); rmarkdown::render( "vignettes/spatial.Rmd", rmarkdown::pdf_document())

## ----setup, echo=TRUE, warning=FALSE, message=FALSE---------------------------
library(tinyVAST)
library(mgcv)
library(fmesher)
library(pdp)  # approx = TRUE gives effects for average of other covariates
library(lattice)
library(ggplot2)
set.seed(101)
options("tinyVAST.verbose" = FALSE)

## ----simulate_data, eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6----
# Simulate a 2D AR1 spatial process with a cyclic confounder w
n_x = n_y = 25
n_w = 10
R_xx = exp(-0.4 * abs(outer(1:n_x, 1:n_x, FUN="-")) )
R_yy = exp(-0.4 * abs(outer(1:n_y, 1:n_y, FUN="-")) )
z = mvtnorm::rmvnorm(1, sigma=kronecker(R_xx,R_yy) )

# Simulate nuissance parameter z from oscillatory (day-night) process
w = sample(1:n_w, replace=TRUE, size=length(z))
Data = data.frame( expand.grid(x=1:n_x, y=1:n_y), w=w, z=as.vector(z) + cos(w/n_w*2*pi))
Data$n = Data$z + rnorm(nrow(Data), sd=1)

# Add columns for multivariate and temporal dimensions
Data$var = "density"
Data$time = 2020

## ----make_mesh, eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6----
# make mesh
mesh = fm_mesh_2d( Data[,c('x','y')], cutoff = 2 )

# Plot it
plot(mesh)

## ----fit_tinyVAST, eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6----
# Define sem, with just one variance for the single variable
sem = "
  density <-> density, spatial_sd
"

# fit model
out = tinyVAST( data = Data,
           formula = n ~ s(w),
           spatial_domain = mesh,
           control = tinyVASTcontrol(getsd=FALSE),
           space_term = sem)

## ----calculate_total, eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6----
# Predicted sample-weighted total
integrate_output(out, newdata = out$data)
# integrate_output(out, apply.epsilon=TRUE )
# predict(out)

# True (latent) sample-weighted total
sum( Data$z )

## ----show_deviance_explained, eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6----
# Percent deviance explained
out$deviance_explained

## ----run_gam, eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6----
start_time = Sys.time()
mygam = gam( n ~ s(w) + s(x,y), data=Data ) #
Sys.time() - start_time
summary(mygam)$dev.expl

## ----run_reduced_tinyVAST, eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6----
out_reduced = tinyVAST( data = Data,
                        formula = n ~ s(w) + s(x,y) )

# Extract PDE for GAM-style spatial smoother in tinyVAST
out_reduced$deviance_explained

## ----show_predict, eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6----
predict(out, newdata=data.frame(x=1, y=1, time=1, w=1, var="density") )

## ----show_image, eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6----
# Prediction grid
pred = outer( seq(1,n_x,len=51),
              seq(1,n_y,len=51),
              FUN=\(x,y) predict(out,newdata=data.frame(x=x,y=y,w=1,time=1,var="density")) )
image( x=seq(1,n_x,len=51), y=seq(1,n_y,len=51), z=pred, main="Predicted response" )

# True value
image( x=1:n_x, y=1:n_y, z=matrix(Data$z,ncol=n_y), main="True response" )

## ----show_partial, eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6----
# compute partial dependence plot
Partial = partial( object = out,
                   pred.var = "w",
                   pred.fun = \(object,newdata) predict(object,newdata),
                   train = Data,
                   approx = TRUE )

# Lattice plots as default option
plotPartial( Partial )

## ----show_ggplot, eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6----
# create new data frame
newdata <- data.frame(w = seq(min(Data$w), max(Data$w), length.out = 100))
newdata = cbind( newdata, 'x'=13, 'y'=13, 'var'='density', 'time'=2020 )

# make predictions
p <- predict( out, newdata=newdata, se.fit=TRUE, what="p_g" )

# Format as data frame and plot
p = data.frame( newdata, as.data.frame(p) )
ggplot(p, aes(x=w, y=fit,
  ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit)) +
  geom_line() + geom_ribbon(alpha = 0.4)

## ----include = FALSE, warning=FALSE, message=FALSE----------------------------
run_time = Sys.time() - start_time

