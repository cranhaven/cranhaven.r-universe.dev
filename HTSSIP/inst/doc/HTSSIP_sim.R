## ---- message=FALSE, warning=FALSE---------------------------------------
library(dplyr)
library(ggplot2)
library(HTSSIP)

## ------------------------------------------------------------------------
# setting parameters for tests
set.seed(318)                              # reproduciblility
M = 6                                      # number of OTUs (species)
ming = 1.67                                # gradient minimum...
maxg = 1.78                                # ...and maximum
nfrac = 24                                 # number of gradient fractions
locs = seq(ming, maxg, length=nfrac)       # Fraction BD's
tol  = rep(0.005, M)                       # species tolerances
h    = ceiling(rlnorm(M, meanlog=11))      # max abundances

opt = rnorm(M, mean=1.71, sd=0.008)        # species optima (drawn from a normal dist.)
params = cbind(opt=opt, tol=tol, h=h)      # put in a matrix

## ------------------------------------------------------------------------
df_OTU = gradient_sim(locs, params)
df_OTU

## ------------------------------------------------------------------------
opt1 = rnorm(M, mean=1.7, sd=0.005)      # species optima
params1 = cbind(opt=opt1, tol=tol, h=h)  # put in a matrix
opt2 = rnorm(M, mean=1.7, sd=0.005)     
params2 = cbind(opt=opt2, tol=tol, h=h) 
opt3 = rnorm(M, mean=1.7, sd=0.005)     
params3 = cbind(opt=opt3, tol=tol, h=h) 
opt4 = rnorm(M, mean=1.72, sd=0.008)    
params4 = cbind(opt=opt4, tol=tol, h=h) 
opt5 = rnorm(M, mean=1.72, sd=0.008)    
params5 = cbind(opt=opt5, tol=tol, h=h) 
opt6 = rnorm(M, mean=1.72, sd=0.008)    
params6 = cbind(opt=opt6, tol=tol, h=h) 

# we need a named list of parameters for the next step. The names in the list will be used as sample IDs
params_all = list(
  '12C-Con_rep1' = params1,
  '12C-Con_rep2' = params2,
  '12C-Con_rep3' = params3,
  '13C-Glu_rep1' = params4,
  '13C-Glu_rep2' = params5,
  '13C-Glu_rep3' = params6
)

## ------------------------------------------------------------------------
meta = data.frame(
  'Gradient' = c('12C-Con_rep1', '12C-Con_rep2', '12C-Con_rep3',
                 '13C-Glu_rep1', '13C-Glu_rep2', '13C-Glu_rep3'),
  'Treatment' = c(rep('12C-Con', 3), rep('13C-Glu', 3)),
  'Replicate' = c(1:3, 1:3)
)

# do the names match?
all(meta$Gradient %in% names(params_all))

## ------------------------------------------------------------------------
## physeq object
physeq_rep3 = HTSSIP_sim(locs, params_all, meta=meta)
physeq_rep3

## ------------------------------------------------------------------------
phyloseq::sample_data(physeq_rep3) %>% head

## ------------------------------------------------------------------------
# unlabeled control qPCR values
control_mean_fun = function(x) dnorm(x, mean=1.70, sd=0.01) * 1e8
# error will scale with the mean 
control_sd_fun = function(x) control_mean_fun(x) / 3
# labeled treatment values
treat_mean_fun = function(x) dnorm(x, mean=1.75, sd=0.01) * 1e8
# error will scale with the mean 
treat_sd_fun = function(x) treat_mean_fun(x) / 3

## ------------------------------------------------------------------------
physeq_rep3_qPCR = qPCR_sim(physeq_rep3,
                control_expr='Treatment=="12C-Con"',
                control_mean_fun=control_mean_fun,
                control_sd_fun=control_sd_fun,
                treat_mean_fun=treat_mean_fun,
                treat_sd_fun=treat_sd_fun)

physeq_rep3_qPCR %>% names

## ------------------------------------------------------------------------
physeq_rep3_qPCR$raw %>% head

## ------------------------------------------------------------------------
physeq_rep3_qPCR$summary %>% head

## ---- fig.height=3, fig.width=7------------------------------------------
x_lab = bquote('Buoyant density (g '* ml^-1*')')
y_lab = '16S rRNA gene copies'

ggplot(physeq_rep3_qPCR$summary, aes(Buoyant_density, qPCR_tech_rep_mean,
                      ymin=qPCR_tech_rep_mean-qPCR_tech_rep_sd,
                      ymax=qPCR_tech_rep_mean+qPCR_tech_rep_sd,
                      color=IS_CONTROL, shape=Replicate)) +
  geom_pointrange() +
  scale_color_discrete('Unlabeled\ncontrol') +
  labs(x=x_lab, y=y_lab) +
  theme_bw()

## ------------------------------------------------------------------------
# using the Cauchy distribution instead of normal distributions
control_mean_fun = function(x) dcauchy(x, location=1.70, scale=0.01) * 1e8
control_sd_fun = function(x) control_mean_fun(x) / 3
treat_mean_fun = function(x) dcauchy(x, location=1.75, scale=0.01) * 1e8
treat_sd_fun = function(x) treat_mean_fun(x) / 3
# simulating qPCR values
physeq_rep3_qPCR = qPCR_sim(physeq_rep3,
                control_expr='Treatment=="12C-Con"',
                control_mean_fun=control_mean_fun,
                control_sd_fun=control_sd_fun,
                treat_mean_fun=treat_mean_fun,
                treat_sd_fun=treat_sd_fun)

## ---- fig.height=3, fig.width=7------------------------------------------
ggplot(physeq_rep3_qPCR$summary, aes(Buoyant_density, qPCR_tech_rep_mean,
                      ymin=qPCR_tech_rep_mean-qPCR_tech_rep_sd,
                      ymax=qPCR_tech_rep_mean+qPCR_tech_rep_sd,
                      color=IS_CONTROL, shape=Replicate)) +
  geom_pointrange() +
  scale_color_discrete('Unlabeled\ncontrol') +
  labs(x=x_lab, y=y_lab) +
  theme_bw()

## ------------------------------------------------------------------------
sessionInfo()

