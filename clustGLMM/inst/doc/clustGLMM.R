## ----setup, include=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.path = file.path(tempdir(), "figure-latex/"))
oldopt <- options(tinytex.verbose = TRUE, width = 90, digits = 3)
# options(width = 110) # does not change the template
library("kableExtra")
library("clustGLMM")

opar <- par(no.readonly = TRUE)

combine <- function(x, sep, width) {
  cs <- cumsum(nchar(x))
  remaining <- if (any(cs[-1] > width)) combine(x[c(FALSE, cs[-1] > width)], sep, width)
  c(paste(x[c(TRUE, cs[-1] <= width)], collapse= sep), remaining)
}
prettyPrint <- function(x, sep = " ", linebreak = "\n\t", width = getOption("width")) {
    x <- strsplit(x, sep)[[1]]
    paste(combine(x, sep, width), collapse = paste(sep, linebreak, collapse = ""))
}  

## ----prior-table, tab.cap = "Overview of prior distributions and their hyperparameters including the default values."----
ptab <- data.frame(Notation = c("$\\beta_{lr}^F$, $\\beta_{lr}^{(g)}$",
                                "$\\tau_r$",
                                "$\\beta_{lr}^F$, $\\beta_{lr}^{(g)}$",
                                "$\\mathbf{c}_r$",
                                "$\\mathbf{b}_i$",
                                "$\\boldsymbol{\\Sigma}^{-1}$",
                                "$\\mathbb{Q}^{-1}$",
                                "$w_g$", 
                                "$e_0$"), 
                   Distribution = c("$\\mathsf{N}(0,\\tau_r^{-1} s_{\\mathsf{Num}}^2)$", 
                                    "$\\Gamma(a_\\tau,b_\\tau)$",
                                    "$\\mathsf{N}(0,s_r^2)$", 
                                    "transformed $\\mathsf{Dir}(\\alpha)$",
                                    "$\\mathsf{N}_{d^R}(0,\\boldsymbol{\\Sigma})$",
                                    "$\\mathsf{W}_{d^R}(\\nu_0+d^R, \\mathbb{Q})$",
                                    "$\\mathsf{W}_{d^R}(\\nu_1+d^R, v\\mathbb{I})$",
                                    "$\\mathsf{Dir}(e_0)$",
                                    "$\\Gamma(a_e,b_e)$"), 
                   Hyperparameter = c("$s_{\\mathsf{Num}} = 1$",
                                      "$a_\\tau = 1$, $b_\\tau = 10$",
                                      "$s_{r} = 1$",
                                      "$\\alpha = 1$",
                                      "",
                                      "$\\nu_0=1$",
                                      "$\\nu_1=1$, $v=100$",
                                      "",
                                      "$a_e = 1, b_e = 100$"),
                   `**clustGLMM** name` = c("$\\mathtt{beta\\_num\\_fix\\_sd}$, $\\mathtt{beta\\_num\\_sd}$",
                                 "$\\mathtt{prec\\_num\\_shp}$, $\\mathtt{prec\\_num\\_rte}$",
                                 "$\\mathtt{beta\\_type\\_fix\\_sd}$, $\\mathtt{beta\\_type\\_sd}$",
                                 "$\\mathtt{api\\_prior}$",
                                 "",
                                 "$\\mathtt{InvSigma\\_df}$",
                                 "$\\mathtt{InvQ\\_df}$, $\\mathtt{InvV}$",
                                 "",
                                 "$\\mathtt{e0\\_shp}$, $\\mathtt{e0\\_rte}$"),
                   Note = c("or with $\\tau_r^{(g)}$, for conjugacy",
                            "or for $\\tau_r^{(g)}$",
                            "$s_r$ specific to type not each outcome",
                            "for $\\pi_{r,k}^{(g)} = \\mathsf{logit}^{-1}(c_{r,k-1}^{(g)}) - \\mathsf{logit}^{-1}(c_{r,k}^{(g)})$",
                            "or with $\\boldsymbol{\\Sigma}^{(g)}$",
                            "or for $\\boldsymbol{\\Sigma}^{-(g)} := \\left(\\boldsymbol{\\Sigma}^{(g)}\\right)^{-1}$",
                            "or for $\\mathbb{Q}^{-(g)} := \\left(\\mathbb{Q}^{(g)}\\right)^{-1}$",
                            "symmetric, $e_0$ randomized",
                            "sparse finite mixture with $a_e/b_e$ low"),
                   row.names = c("Fixed effects ($\\mathsf{Num}$)", 
                                 "Error precision",
                                 "Fixed effects (other)", 
                                 "Ordered intercepts",
                                 "Random effects", 
                                 "Precision matrix",
                                 "Inverse scale matrix",
                                 "Component sizes",
                                 "Sparsity"),
                   check.names = FALSE)
if (requireNamespace("kableExtra", quietly = TRUE) & requireNamespace("dplyr", quietly = TRUE)) {
  kable(ptab, booktabs = TRUE, escape = FALSE) |> kableExtra::landscape()
}else{
  kable(ptab, booktabs = TRUE, escape = FALSE)
}

## ----workflow-diagram, fig.cap="Typical workflow with **clustGLMM** and key functions.", out.width="100%", dev="png"----
if (requireNamespace("DiagrammeR", quietly = TRUE) & 
    requireNamespace("DiagrammeRsvg", quietly = TRUE) & 
    requireNamespace("rsvg", quietly = TRUE)) {
  graph <- DiagrammeR::grViz("digraph {
    graph[layout = dot, rankdir = TB]
    
    node[shape = rectangle, style = rounded, fontname = Helvetica, penwidth = 2, fontsize = 15, fillcolor = gold, style = filled]  
    rec1[label = '@@1']
    rec2[label = '@@2']
    rec3[label = 'Visual diagnostics']
    rec3a[label = 'Satisfied', shape = diamond, fillcolor = LemonChiffon, style = filled]
    rec4[label = 'Within chain post-processing']
    rec5[label = 'Across chain post-processing', fillcolor = LemonChiffon]
    
    node[shape = rectangle, style = rounded, fontname = courier, penwidth = 2, fillcolor = grey70, style = filled] 
    code1[label = 'data, family, formula, varying, ...']
    code2[label = 'clustGLMM()']
    code3[label = 'as_coda(), plot()']
    code3a[label = 'If not, return to previous steps (adjust settings or continue in sampling)', fillcolor = white, fontname = Helvetica]
    code4[label = 'post_processing()']
    code5[label = 'plot(, which=clusters_kerneldensity), permute_cluster_labels()']
    
    node[shape = rectangle, style = rounded, fontname = courier, penwidth = 2, fillcolor = grey70, style = filled]
    code6a[label = 'summary()']
    code6b[label = 'predict()']
    code6c[label = 'clustering_probabilities_and_deviance()']
    
    subgraph cluster6 {
      graph[layout = fdp, shape = rectangle, style = rounded, fillcolor = gold, style = filled]
      
      label = ''
      node[shape = rectangle, style = rounded, fontname = Helvetica, penwidth = 2, fillcolor = LemonChiffon, style = filled]
      rec6[label = 'Inference', fillcolor = gold, shape = none]
      rec6a[label = 'Posterior description with the use of coda']
      rec6b[label = 'Reconstruct linear predictor']
      rec6c[label = 'Approximate posterior of clustering probabilities and deviance']
    }
    
    # edge definitions with the node IDs
    edge[arrowhead = normal, arrowtail = none]
    rec1 -> rec2 -> rec3 -> rec3a -> rec4 -> rec5 -> rec6
    
    edge[arrowhead = none, arrowtail = none]
    {{rank=same rec1 -> code1}}
    {{rank=same rec2 -> code2}}
    {{rank=same rec3 -> code3}}
    {{rank=same rec3a -> code3a}}
    {{rank=same rec4 -> code4}}
    {{rank=same rec5 -> code5}}
    {{rank=same rec6a -> code6a}}
    {{rank=same rec6b -> code6b}}
    {{rank=same rec6c -> code6c}}
    
    edge[arrowhead = normal, arrowtail = none, color = white, maxlen=1]
    rec6 -> rec6a -> rec6b -> rec6c 
    
    }
    
    # Substitutions with R code are possible:
    [1]: paste0('Prepare inputs')
    [2]: paste0('Sample chains')
    ",
    height = 500, width = 600)
  svg_text <- DiagrammeRsvg::export_svg(graph)
  rsvg::rsvg_png(charToRaw(svg_text), file = file.path(tempdir(), "workflow-diagram.png"))
  knitr::include_graphics(file.path(tempdir(), "workflow-diagram.png"))
}else{
  knitr::include_graphics(file.path(tempdir(), "workflow-diagram.png"))
}

## ---------------------------------------------------------------------------------------
cat(prettyPrint(prompt(clustGLMM::clustGLMM, 
                       filename = NA)$usage[[2]], sep = ", ", 
                linebreak = paste("\n", paste(rep(" ", 2), collapse = ""), sep= ""),
                width = 60))

## ----matrix-column-name, eval = FALSE, include = TRUE, echo = TRUE----------------------
# paste0(parameter, # parameter ~ character(1)
#        if (is.outcome.specific) {"_", outcome}, # outcome ~ character(1)
#        if (is.group.specific) {"(", g, ")")}, # g ~ integer(1)
#        if (is.not.0D) {"[", d[1], if(is.2D) {",", d[2]}, "]"} # d ~ integer(2)
# ) # e.g., "beta_num_income(1)[1]"

## ----get-some-mcmc, echo = FALSE, results = FALSE, eval = TRUE, message = FALSE---------
data("longitudinal_mixed_type_data")
id <- "i"

family <- c("num", "poi", "bin", "ord", "cat")
names(family) <- Ys <- c("ynum", "ypoi", "ybin", "yord", "ycat")

## Create list of formulae for each outcome
formula <- list()
for(y in Ys){
  formula[[y]] <- list()
  formula[[y]]$fixed <- ~ f
  formula[[y]]$group <- ~ x
  formula[[y]]$random <- ~ 1
  formula[[y]]$offset <- ""
}

Gmax <- 5     # maximal number of mixture components to be considered
nchains <- 2  # number of chains to be sampled 

## No inits specified
set.seed(123456789)
mcmc <- clustGLMM(formula = formula, id = id, family = family, 
                  data = longitudinal_mixed_type_data, 
                  G = Gmax, iter = 10, nchains = nchains)

## ----settings-table, echo = FALSE, tab.cap = "Information on the model parameters stored in \\texttt{settings}: D = 0 indicates scalar, D = 1 vector, D = 2 matrix. g indicates group-specificity (? depends on choice). y indicates if specific to an outcome. yd1, yd2 indicates if specific to an outcome in the first or second dimension. N, P, B, O, C indicate the outcome type. sym indicates whether the matrix is symmetric and diag whether it contains the diagonal. "----
m <-mcmc$settings[[1]][c("D", "gspec", "isy", "ydepd1", "ydepd2", "ynums", "ypois", "ybins", "yords", "ycats", "sym", "diag")]
m[c("prec_num", "var_num", "sd_num", "c_ord", "a_ord", "pi_ord", "InvSigma", "Sigma", "sdSigma", "corSigma", "detInvSigma", "InvQ", "Q", "detInvQ", "naY"), "gspec"] <- "?"
for(col in c("gspec", "isy", "ydepd1", "ydepd2", "ynums", "ypois", "ybins", "yords", "ycats", "sym", "diag")){
  m[m[,col] == TRUE, col] <- "T"
  m[m[,col] == FALSE, col] <- "F"
}
colnames(m) <- c("D", "g", "y", "yd1", "yd2", "N", "P", "B", "O", "C", "sym", "diag")
kable(m, booktabs = TRUE)

## ----psurvey-load, echo = TRUE----------------------------------------------------------
data("psurvey", package = "clustGLMM")
head(psurvey)

## ----psurvey-n--------------------------------------------------------------------------
n <- length(unique(psurvey$i))

## ----psurvey-n-nmis, warning=FALSE, echo=TRUE-------------------------------------------
set.seed(31415)
nmis <- 3
na_indices <- na_true_vals <- list()
for (y in c("income", "ntrips", "satisfaction")) {
  na_indices[[y]] <- sort(sample(seq_len(nrow(psurvey)), nmis))
  na_true_vals[[y]] <- psurvey[na_indices[[y]], y]
  is.na(psurvey[na_indices[[y]], y]) <- TRUE
}

## ----psurvey-outcomes-vs-age, echo=TRUE, fig.align="center", fig.width=10, fig.height=4, fig.cap="The evolution of income, number of trips and satisfaction level by age.", out.width="100%"----
par(mar = c(4, 4, 1, 0.5), mfrow = c(1, 3))
plot_num_vs_x_grouped(psurvey, "income", x = "age", id = "i", add_lowess = TRUE)
plot_num_vs_x_grouped(psurvey, "ntrips", x = "age", id = "i", add_lowess = TRUE)
plot_cat_vs_x_grouped(psurvey, "satisfaction", x = "age") 

## ----psurvey-family, echo=TRUE----------------------------------------------------------
family <- c(income = "num", ntrips = "poi", satisfaction = "ord") 
family <- c(income = "gaussian", ntrips = "poisson", satisfaction = "cumulative")

## ----psurvey-splines, echo=TRUE---------------------------------------------------------
formula <- list()
formula[["income"]] <- list(fixed = ~ 0, group = ~ age + I(age^2), random = ~ 1)
for (y in c("ntrips", "satisfaction")) {
  formula[[y]] <- list(fixed = ~ 0, group = ~ age, random = ~ 1)
}

## ----psurvey-defaults, echo = TRUE------------------------------------------------------
varying <- default_varying()
varying

## ----psurvey-defaults-1, echo = TRUE----------------------------------------------------
varying["prec_num"] <- FALSE

## ----psurvey-defaults-2, echo = TRUE----------------------------------------------------
save <- default_save(naY = TRUE)

## ----psurvey-defaults-3, echo=TRUE------------------------------------------------------
param <- default_param(prec_num_rte = 15)  

## ----psurvey-mcmc-initial, echo=TRUE, eval=TRUE, results=FALSE--------------------------
set.seed(12345678)
mcmc_burnin <- clustGLMM(formula, id = "i", family, data = psurvey, G = 10, 
                         varying = varying, save = save, param = param, nchains = 1, 
                         iter = 1000, standardize = TRUE, howsave = "data.frame")

## ----psurvey-mcmc-burnin-plot-ng, echo=TRUE, results=FALSE, fig.align="center", fig.width=10, fig.height=4, fig.cap="Traceplots - component occupancy numbers during MCMC sampling for the 10 components.", fig.pos="H", out.width="100%"----
plot(mcmc_burnin, ng_trace_chain_split = FALSE)

## ----psurvey-mcmc, echo=TRUE, results=FALSE---------------------------------------------
set.seed(1234)
mcmc <- clustGLMM(formula, id = "i", family, data = psurvey, G = 10, 
                  varying = varying, save = save, param = param, iter = 1000, 
                  inits = mcmc_burnin$last, nchains = 1, standardize = TRUE)

## ----psurvey-mcmc-print, echo = TRUE, results=TRUE--------------------------------------
print(mcmc)

## ----psurvey-mcmc-ng-prec-num, echo=TRUE, results = FALSE, fig.align="center", fig.width=10, fig.height=4, fig.cap="Trace plots - cluster occupancy numbers (left) and precision of the errors for income (right).", fig.pos="H", out.width="80%"----
par(mfrow = c(1, 2))
plot(mcmc, ng_trace_chain_split = TRUE, setparmfrow = FALSE)
plot(mcmc, what = "prec_num", yspec = "income", setparmfrow = FALSE)

## ----psurvey-mcmc-beta-3-num, echo=TRUE, results=FALSE, fig.align="center", fig.width=8, fig.height=5.6, fig.cap="The four implemented plots - regression coefficient for squared age for income.", fig.pos="t", out.width="80%"----
plot_diagnostics(mcmc, what = "beta_num", yspec = "income", dimspec = 3, gspec = 3,
                 burnin = 0, thin = 2)

## ----psurvey-post-processing, echo = TRUE, results = TRUE-------------------------------
set.seed(123456)
(mcmc_pp <- post_processing(mcmc))

## ----psurvey-mcmc-summary-inv-param, echo = TRUE, results = TRUE, R.options = list(digits = 2)----
sum_mcmc <- summary(mcmc_pp, burnin = 0, thin = 1)
print(sum_mcmc, which = "inv_param", pattern = "income")

## ----psurvey-summary-group-param, echo = TRUE, results = TRUE, R.options = list(digits = 2)----
print(sum_mcmc, which = "group_param", clusters = 1, pattern = "ntrips")

## ----psurvey-mcmc-pp-beta-num-1, echo = TRUE, results=TRUE, fig.align="center", fig.width=10, fig.height=4, fig.cap="The regression coefficients of income - posterior kernel density estimates, clusters distinguished by color.", fig.pos="t", out.width="100%"----
plot(mcmc_pp, what = "beta_num", yspec = "income", which = "clusters_kerneldensity")

## ----psurvey-mcmc-summary-clust-param, echo = TRUE, results = TRUE----------------------
print(sum_mcmc, which = "clust_param")

## ----psurvey-mcmc-summary-latent-param, echo = TRUE, results = TRUE---------------------
print(sum_mcmc, which = "latent_param", pattern = "^naY_income")

## ----psurvey-mcmc-naY-income, echo=TRUE, results=TRUE, fig.align="center", fig.width=10, fig.height=4, fig.cap="Predictive distribution for missing outcome values of `income`.", fig.pos="H", out.width="100%"----
par(mar = c(4, 4, 0.5, 0.5), mfrow = c(1, 3))
y <- "income"
for (j in seq_len(nmis)) {
  plot_kerneldensity(mcmc_pp, what = "naY", yspec = "income", dimspec = j, labcex = 0.8)
  abline(v = as.numeric(na_true_vals[["income"]][j]), col = "seagreen", lty = 2)
  abline(v = sum_mcmc$coda_summary[[1]]$quantiles[paste0("naY_income[", j, "]"),
         c("2.5%", "97.5%")], col = "black", lty = 3)
}

## ----psurvey-mcmc-names, echo=TRUE------------------------------------------------------
str(mcmc$param_names, vec.len = 1)

## ----echo=TRUE--------------------------------------------------------------------------
U <- mcmc$draws[[1]][, grep("^U", colnames(mcmc$draws[[1]]))]
w <- mcmc$draws[[1]][, grep("^w", colnames(mcmc$draws[[1]]))]
# or using dplyr
library("dplyr")
U <- mcmc$draws[[1]] |> select(starts_with("U"))
w <- mcmc$draws[[1]] |> select(starts_with("w"))

## ----psurvey-predict-basic-use, echo = TRUE, eval = TRUE--------------------------------
newdata <- data.frame(age = seq(18, 40, by = 0.2))
pred1 <- predict(mcmc_pp, newdata, y = "ntrips", type = "link")

## ----psurvey-predict-basic-use-2, echo = TRUE, eval = TRUE------------------------------
pred2 <- predict(mcmc_pp, newdata, y = "ntrips", type = "response") 
pred3 <- predict(mcmc_pp, newdata, y = "ntrips",                    
                 type = "posterior_predictive_response")            

## ----psurvey-prediction, echo=FALSE, results=FALSE, fig.align="center", fig.width=10, fig.height=4, fig.cap="Cluster-specific predictions of `ntrips`. Linear predictor (left), predicted mean response with 95% credible intervals (middle) and samples from the predictive distribution (right).", fig.pos="H", out.width="100%", echo = TRUE, eval = TRUE----
par(mar = c(4, 4, 1, 0.5), mfrow = c(1, 3))
COL <- colorspace::rainbow_hcl(mcmc_pp$modeGplus[1], c = 80, l = 50)

plot(c(18, 40), c(0, 3), type = "n", xlab = "age", ylab = expression(x * beta))
for (g in 1:mcmc_pp$modeGplus[1]) {
  lines(newdata$age, pred1$fit[, g], col = COL[g], lwd = 2)
  lines(newdata$age, pred1$lwr[, g], col = COL[g], lty = 2, lwd = 1)
  lines(newdata$age, pred1$upr[, g], col = COL[g], lty = 2, lwd = 1)
}  
legend("topleft", legend = 1:mcmc_pp$modeGplus[1], title = "Cluster", 
       col = COL, lty = 1, lwd = 2, bty = "n")
plot(c(18, 40), c(0, 8), type = "n", xlab = "age", ylab = expression(exp(x * beta)))
for (g in 1:mcmc_pp$modeGplus[1]) {
  lines(newdata$age, pred2$fit[, g], col = COL[g], lwd = 2)
  lines(newdata$age, pred2$lwr[, g], col = COL[g], lty = 2, lwd = 1)
  lines(newdata$age, pred2$upr[, g], col = COL[g], lty = 2, lwd = 1)
}
plot(c(18, 40), c(0, 20), type = "n", xlab = "age",
     ylab = expression(Y ~ "~" ~ Pois(exp(x * beta))))
for (g in 1:mcmc_pp$modeGplus[1]) {
  lines(newdata$age, pred3$fit[, g], col = COL[g], lwd = 2)
  lines(newdata$age, pred3$lwr[, g], col = COL[g], lty = 2, lwd = 1)
  lines(newdata$age, pred3$upr[, g], col = COL[g], lty = 2, lwd = 1)
}    

## ----psurvey-predict-income-and-ntrips, echo=FALSE, results=FALSE, fig.align="center", fig.width=10, fig.height=4, fig.cap="The estimated spline curves for income (left) and number of trips (right).", fig.pos="H", out.width="100%"----
# psurvey[, "Uclustering"] <- mcmc_pp$clustering[psurvey$i, 1]
# 
# age_grid <- seq(18, 40, by = 0.1)
# newdata <- data.frame(age = age_grid)
# 
# burnin <- 0
# thin <- 1
# COL <- colorspace::rainbow_hcl(mcmc_pp$modeGplus[1], c = 80, l = 50)
# 
# par(mfrow = c(1, 2), mar = c(4, 4, 1, 0.5))
# for (y in c(mcmc_pp$Nums, mcmc_pp$Pois)) {
#   plot_num_vs_x_grouped(psurvey, y = y, x = "age", g = "Uclustering",
#                         id = "i", legend_placement = "topleft", add_lowess = FALSE)
# 
#   pred <- predict(mcmc_pp, newdata, y = y, type = "response", method = "samples",
#                   burnin = burnin, thin = thin, chain = 1)
# 
#   for (g in mcmc_pp$clusters[[1]]) {
#     col <- paste0("(", g, ")")
#     lines(x = age_grid, y = pred$fit[, col], col = COL[g], lwd = 3)
#     lines(x = age_grid, y = pred$lwr[, col], col = COL[g], lty = 2)
#     lines(x = age_grid, y = pred$upr[, col], col = COL[g], lty = 2)
#   }
# 
#   ## Posterior predictive distribution for each cluster separately
#   pred <- predict(mcmc_pp, newdata, y = y,
#                   type = "posterior_predictive_response",
#                   posterior_predictive_by_cluster = TRUE,
#                   method = "samples",
#                   burnin = burnin, thin = thin, chain = 1)
#   for (g in mcmc_pp$clusters[[1]]) {
#     lines(x = age_grid, y = pred$lwr[, g], col = COL[g], lty = 1)
#     lines(x = age_grid, y = pred$upr[, g], col = COL[g], lty = 1)
#   }
# }

## ----psurvey-mcmc-pp-clustering, echo = TRUE, results = TRUE----------------------------
print(mcmc_pp, which = "clustering")

## ----psurvey-misclassification-U-samples, echo=TRUE, results=TRUE-----------------------
data("psurvey_latent", package = "clustGLMM")
psurvey1 <- merge(psurvey, psurvey_latent, by = c("i", "j"))
psurvey1 <- psurvey1[psurvey1$j == 1, ]
psurvey1[, "Uclustering"] <- mcmc_pp$clustering[psurvey1$i, 1]
with(psurvey1, table(g, Uclustering))
with(psurvey1, mclust::classError(g, Uclustering)$errorRate)

## ----psurvey-permuting-labels, echo = TRUE, eval = TRUE---------------------------------
perm <- list(c(3, 1, 2))             
mcmc_perm <- permute_cluster_labels(mcmc_pp, perm = perm)

## ----psurvey-final-clustering, echo=TRUE, results=FALSE, fig.align="center", fig.width=10, fig.height=4, fig.cap="Estimated clustering of the data. Colors refer to clusters, smoothed mean function is added.", fig.pos="H", out.width="100%"----
psurvey$Uclustering <- mcmc_pp$clustering[psurvey$i, 1]
par(mar = c(4, 4, 1, 0.5), mfrow = c(1, 3))
plot_num_vs_x_grouped(psurvey, "income", x = "age",  id = "i", group = "Uclustering",
                      add_lowess = TRUE, legend_placement = "topleft")
plot_num_vs_x_grouped(psurvey, "ntrips", x = "age",  id = "i", group = "Uclustering",
                      add_lowess = TRUE, legend_placement = "topleft")
plot_cat_vs_x_grouped(psurvey, "satisfaction", x = "age", group = "Uclustering")

## ----psurvey-clustering-probabilities-and-deviance, echo=TRUE, results = FALSE----------
pmcmc <- clustering_probabilities_and_deviance(
    mcmc_pp, data = psurvey, id = "i",
    start = 1, end = mcmc_pp$iter, thin = 10, chains = 1, NGQ = 1)

## ----psurvey-clustering-probabilities-confusion, echo=TRUE, eval = TRUE-----------------
psurvey1[, "clustering"] <- pmcmc$clustering[psurvey1$i, 1]
with(psurvey1, table(Uclustering, clustering))

## ----psurvey-clust-probs-posterior, echo=TRUE, eval=TRUE, results=TRUE, fig.align="center", fig.width=10, fig.height=4, fig.cap="Posterior distribution of clustering probabilities for subjects with the lowest certainty level.", fig.pos="H", out.width="100%"----
par(mar = c(4, 4, 2.5, 0.5), mfrow = c(1, 4))
for (i in order(pmcmc$certainty[, 1])[1:4]) {
  plot_clusters(pmcmc, what = "pUig_int", dimspec = i, 
                chains = 1, setparmfrow = FALSE, doKern = FALSE, thin = 10)
  mtext(paste0("true g=", perm[[1]][psurvey1$g[psurvey1$i == i]]), 
        side = 3, line = 0.5, font = 2, cex = 0.8)
  legend("bottomright", legend = 1:mcmc_pp$modeGplus[1], title = "Cluster", 
       col = COL, lty = 1, bty = "n")
}

## ----psurvey-probability-new-data, echo = TRUE, eval = TRUE, message = FALSE, warning = FALSE, results = FALSE----
newdata <- data.frame(i = 501, 
                      age = seq(19, 37, length.out = 10),
                      income = seq(110, 180, length.out = 10),
                      ntrips = 0:9,
                      satisfaction = c(0, 0, 0, 1, 1, 1, 1, 2, 2, 2))
mcmc_new <- clustering_probabilities_and_deviance(
    mcmc_pp, data = newdata, id = "i", dynamic_prob = TRUE,
    start = 1, end = mcmc_pp$iter, thin = 1, NGQ = 3)                                              

## ----psurvey-probability-new-data-plot, echo = TRUE, eval = TRUE, fig.width=10, fig.height=5, fig.align="center", fig.cap="Posterior quantiles (2.5%, 50%, 97.5%) of clustering probabilities (dynamically in time) for newly observed respondent.", fig.pos="t", out.width="80%"----
sum_new <- summary(as_coda(mcmc_new))$quantiles
fit <- matrix(sum_new[-c(1:2), "50%"], ncol = 3, byrow = FALSE)
lwr <- matrix(sum_new[-c(1:2), "2.5%"], ncol = 3, byrow = FALSE)
upr <- matrix(sum_new[-c(1:2), "97.5%"], ncol = 3, byrow = FALSE)

par(mfrow = c(1, 1), mar = c(4, 4, 0.5, 0.5))
matplot(x = newdata$age, y = fit, type = "l", col = COL, lwd = 3, lty = 1, 
        xlab = "Age", ylab = "Clustering probability")
matplot(x = newdata$age, y = lwr, type = "l", col = COL, lwd = 2, lty = 2, add = TRUE)
matplot(x = newdata$age, y = upr, type = "l", col = COL, lwd = 2, lty = 2, add = TRUE)
legend("topleft", legend = 1:3, title = "Cluster", col = COL, lwd = 3, lty = 1, bty = "n")

## ----final, include=FALSE---------------------------------------------------------------
par(opar)

