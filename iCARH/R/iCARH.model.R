#' @title Runs the integrative CAR Horseshoe model
#'
#' @description Infers treatment effects, association with heterogeneous omic variables, pathway perturbation
#' among other parameters (e.g. time dependence). Regression coefficients (beta parameter) are initialized
#' using a univariate regression ignoring time and metabolite dependence.
#'
#' @param X the metabolomics time-course data with dimensions timepoints x observations x variables
#' @param Y the additional omic time-course data with dimensions timepoints x observations x variables
#' @param drug treatment effect. Could be either continuous (an administered drug or other external factor) or
#' binary (cases vs controls). In the binary case the \code{groups} argument can be safely removed.
#' NA values not allowed in \code{drug}. Dimensions are timepoints x observations
#' @param groups grouping vector (binary). Use when \code{drug} is continuous.
#' @param pathways pathway adjacency matrices as returned by iCARH.getPathwaysMat
#' @param tau global sparsity parameter \eqn{\tau} as in Jendoubi, T., & Ebbels, T. (2018)
#' @param NA_value NA values are incompatible with stan. This is a wrapper to encode missing values in \code{X} and \code{Y}. 
#' NAs will be replaced by NA_value and will be inferred (only for X and Y data).
#' @param init If \code{TRUE} use iCARH provided initialization function. Passed to Stan otherwise. Please see Stan manual 
#' on \code{init} possible values.
#' @param ... additional stan parameters
#'
#' @return stan object
#'
#' @examples data.sim = iCARH.simulate(4, 8, 10, 2, 2, path.probs=0.3, Zgroupeff=c(0,4),
#' beta.val=c(1,-1,0.5, -0.5))
#' XX = data.sim$XX
#' Y = data.sim$Y
#' Z = data.sim$Z
#' pathways = data.sim$pathways
#' \donttest{
#' rstan_options(auto_write = TRUE)
#' options(mc.cores = 2)
#' fit = iCARH.model(XX, Y, Z,groups=rep(c(0,1), each=4), pathways,
#'  control = list(adapt_delta = 0.99, max_treedepth=10), iter = 2, chains = 2)}
#'
#' @export iCARH.model
#' @importFrom rstan stan
#' @importFrom utils packageVersion
#' @importFrom glue glue glue_collapse

iCARH.model = function(X, Y=NULL, drug, groups=NULL, pathways, tau=1.2, NA_value=-99999, init=T, ...){
  
  if ((packageVersion("rstan") <= "2.18.2") & (getRversion() >= "3.6.0")) {
    warning("rstan > 2.18.2 or R < 3.6.0 needed for this function. Program will exit promptly.", call. = FALSE)
    return(NULL)
  }
  
  if(is.null(groups)){
    if(is.logical(drug) | setequal(drug, c(0,1))) groups=!drug[1,]
    else warning("Need group information in logical or binary form.")
  }
  
  N=ncol(X)
  Tp=nrow(X)
  J=dim(X)[3]

  Xraw=X
  Yraw=Y
  if(is.null(dimnames(X))) Xraw = provideDimnames(Xraw)
  names(dimnames(Xraw))=c("timepoints", "observations", "variables") 
  
  st_data = list(N=ncol(X), Tp=nrow(X), J=dim(X)[3])
          
  out = stcode_set()
  
  stcode_append(out$data) = set_var("X", "matrix", dims=c(N,J), add_dims=Tp)  #glue("matrix[{N},{J}]  X[{Tp}];\n")
  if(any(is.na(X))) {
    rep_X_data = "XX"
    st_set = stcode_impute(out, X, NA_value, rep_X_data) 
    X[which(is.na(X))] = NA_value
    out = st_set$st_code
    stdata_append(st_data) = st_set$st_mis_data
  } else { 
    rep_X_data = "X"
  }
  stdata_append(st_data) = X
  
  if(!is.null(Y)){
    if(is.null(dimnames(Y))) Yraw = provideDimnames(Yraw)
    names(dimnames(Yraw))=c("timepoints", "observations", "variables") 
    if(any(is.na(Y))){
      rep_Y_data = "YY"
      st_set = stcode_impute(out, Y, NA_value, rep_Y_data)
      Y[which(is.na(Y))] = NA_value
      out = st_set$st_code
      stdata_append(st_data) = st_set$st_mis_data
    } else {
      rep_Y_data = "Y" #glue("matrix[{N},{K}]  YY[{Tp}];\n")
    }
    K=dim(Y)[3]
    out = stcode_horseshoe_block(out, J, K, tau) 
    out = stcode_Y_data(out, N, Tp, K, rep_Y_data);
    stdata_append(st_data) = list(nu=tau, Y=Y, K=K)
  }
  
  if(any(is.na(X))|any(is.na(Y))) stdata_append(st_data) = NA_value
  
  # Grouping observations
  st_groups = as.numeric(as.character(factor(groups, levels=sort(unique(groups), decreasing=T),
                                              labels=1:length(unique(groups)) )))
  G=as.integer(max(st_groups))
  P=length(pathways)
  stdata_append(st_data) = list(P=P, G=G, groups=st_groups)
  
  out = stcode_CAR_block(out, J, P, G, N, Tp)
  
  adjmat = lapply(pathways, function(x) 1/(x+1))
  adjmat = lapply(adjmat, function(x) {diag(x)=0; 1/max(rowSums(x>0))*x}) 
  # For a fixed pathway we have the same number of neighbors
  lambdas = lapply(adjmat,function(x) sort(eigen(x)$values)[c(1,nrow(x))])
  stdata_append(st_data) = list(adjmat=adjmat, lambdas=lambdas)

  out = stcode_mixedeff(out, N, Tp, J, !is.null(Y), rep_Y_data)
  stdata_append(st_data) = drug
  
  out = stcode_autoreg(out, N, Tp, J, rep_X_data)
  
  # Likelihood
  stcode_append(out$model) = glue(
    "for (i in 1:{Tp}){{",
    "for(n in 1:{N})",
    set_prior("{rep_X_data}[i,n]", "multi_normal_prec_lpdf", c("mu[i,n]", "Sigma[groups[n]]"), cond=T, "target"),
    "}}",
    .sep="\n")
  
  # checks
  stcode_append(out$genqt) = glue(
    set_var("log_lik", "real", add_dims=c(Tp,N)),
    set_var("mupred", "matrix", dims=c(J,N), add_dims=Tp),
    "for(i in 1:{Tp}){{",
    "for(n in 1:{N}){{",
    set_prior("{rep_X_data}[i,n]", "multi_normal_prec_lpdf", c("mu[i,n]","Sigma[groups[n]]"), cond=T, target="log_lik[i,n]"),
    "mupred[i,,n] = multi_normal_rng((mu[i,n])\', Sigma[groups[n]]);",
    "}}",
    "}}",
    .sep="\n"
  )
  out = stcode_end(out)
  
  if (init==T){
    if(is.null(Y)){
      initf = function(){
        #if(perturb)
        X[which(X== NA_value)] = NA
        X = X + array(rnorm(prod(dim(X)),0,sd(X,na.rm=T)/10), dim=dim(X))
        coeff = array(dim=c(3,J))
        for(i in 1:J){
          dd = lm(X~.,data=data.frame(X=as.vector(X[2:nrow(X),,i]), as.vector(X[1:(nrow(X)-1),,i]),
                                       as.vector(drug[1:(nrow(X)-1),])))
          coeff[,i] = dd$coefficients
        }
        return(list(inter=coeff[1,],theta=coeff[2,],alpha=coeff[3,]))
      }  
    }else{
    initf = function(){
      #if(perturb)
      X[which(X== NA_value)] = NA
      Y[which(Y== NA_value)] = NA
      X = X + array(rnorm(prod(dim(X)),0,sd(X,na.rm=T)/10), dim=dim(X))
      coeff = array(dim=c(4,J, K))
      for(k in 1:K){
        for(i in 1:J){
          dd = lm(X~.,data=data.frame(X=as.vector(X[2:nrow(X),,i]), as.vector(X[1:(nrow(X)-1),,i]),
                                      as.vector(Y[1:(nrow(X)-1),,k]), as.vector(drug[1:(nrow(X)-1),])))
          coeff[,i,k] = dd$coefficients
        }
      }
      return(list(inter=rowMeans(coeff[1,,]),theta=rowMeans(coeff[2,,]),beta=coeff[3,,],alpha=rowMeans(coeff[4,,])))
    }
    }
  } else{
    initf=init
  }

  stcode = glue_collapse(out)
  fit = tryCatch (rstan::stan(model_name="iCARH",model_code = stcode, 
                              data = st_data, init=initf, pars=c("Xmis","Ymis"), include=F, ...),
                  warning = function(w) {
                    print(paste("Stan warning:", w))
                    return(NULL)
                  },
                  error = function(e) {
                    print(paste("Stan error:", e))
                    return(NULL)
                  }
                  )
  fit = list(icarh=fit, X=Xraw, Y=Yraw, drug=drug, groups=groups)
  class(fit) = "icarh"
  return(fit)
}

#' @noRd
stcode_set = function(){
  out=list()
  out$soft = " // generated by iCARH \n"
  out$data = paste0("data", sep="{")
  out$tfdata = paste0("transformed data", sep="{")
  out$param = paste0("parameters", sep="{")
  out$tfparamdef = paste0("transformed parameters", sep="{")
  out$tfparam = ""
  out$modeldef = paste0("model", sep="{")
  out$model = ""
  out$genqt = paste0("generated quantities", sep="{")
  out
}

#' @noRd
stcode_end = function(out){
  blocks = c(which(!grepl( "def|soft", names(out))))
  out[blocks] = lapply(blocks, function(x) paste0(out[[x]], "\n}\n"))
  out
}

#' @noRd
stcode_impute = function(out, X, NA_value, rep_data){
  vv = deparse(substitute(X))
  N = ncol(X)
  Tp = nrow(X)
  J = dim(X)[3]
  x_i_mis = which(is.na(X), arr.ind = T)
  x_n_mis = NROW(x_i_mis)
  stcode_append(out$data) = glue(
  "// Indexing missing data",
  set_var(glue("{vv}_i_mis"), "int", lower=1, add_dims=c(x_n_mis,3L)),
  .sep="\n"
  #'int<lower=1> {vv}_i_mis[{x_n_mis},3];\n' 
  )
  stcode_append(out$param) = glue(
    "// Missing data in {vv}",
    set_var(glue("{vv}mis"), "real", add_dims=x_n_mis),
    .sep="\n"
    #'real {vv}mis[{x_n_mis}];\n'
  )
  stcode_append(out$tfparamdef) = glue(
    "// Imputed data",
    set_var(glue("{rep_data}"), "matrix", dims=c(N,J), add_dims=Tp),
    .sep="\n"
    #matrix[{N},{J}] {vv}{vv}[{Tp}];\n"
  )
  stcode_append(out$tfparam) = glue(
    " // Data Imputation",
    "for (i in 1:{Tp}){{",
      "for(j in 1:{J}) {{for(n in 1:{N}) if(({vv}[i,n,j])!= {NA_value}) {rep_data}[i,n,j]={vv}[i,n,j];}}",
    "}}",
     "for(l in 1:{x_n_mis}) {rep_data}[{vv}_i_mis[l,1], {vv}_i_mis[l,2], {vv}_i_mis[l,3]] = {vv}mis[l];",
    .sep="\n"
  )
  st_mis_data = list(x_i_mis, x_n_mis)
  names(st_mis_data) = c(glue("{vv}_i_mis"), glue("{vv}_n_mis"))
  return (list(st_code=out, st_mis_data=st_mis_data))
}

#' @noRd
stcode_Y_data = function(out, N, Tp, K, rep_data){
  stcode_append(out$data) = set_var("Y", "matrix", dims=c(N,K), add_dims=Tp)
    #"matrix[{N},{K}]  Y[{Tp}];\n"
  stcode_append(out$param) = set_var("sigma_y", "real", lower=0)
    #"real<lower=0> sigma_y;\n"
  stcode_append(out$modeldef) = glue(
    set_var("SY_1_{N}", "matrix", dims=c(N,N)),
    set_var("SY_{Tp}_{N}", "matrix", dims=c(N,N)),
    .sep="\n"
    #"cov_matrix[{N}] SY_1_{N} = diag_matrix(rep_vector(1,{N}))\n
    #cov_matrix[{N}] SY_{Tp}_{N} = diag_matrix(rep_vector(sqrt(sigma_y),{N}))\n"
  )
  stcode_append(out$model) = glue(
    set_prior("sigma_y", "inv_gamma", c(1,1)),
    glue("SY_1_{N}=diag_matrix(rep_vector(1,{N}));"),
    glue("SY_{Tp}_{N}=diag_matrix(rep_vector(sqrt(sigma_y),{N}));"),
    "// Inferring missing values in YY",
    "for(k in 1:{K})", 
    set_prior("{rep_data}[1,,k]", "multi_normal", params = c("rep_vector(0,{N})",glue("SY_1_{N}"))),
    #YY[1,,k] ~ multi_normal(rep_vector(0,N),SY_1_{N});\n
    "for(i in 2:{Tp}){{",
    "for(k in 1:{K})",
    set_prior("{rep_data}[i,,k]", "multi_normal", params=c("{rep_data}[i-1,,k]", glue("SY_{Tp}_{N}"))),
    #YY[i,,k] ~ multi_normal( YY[i-1,,k], SY_{Tp}_{N});
    "}}",
    .sep="\n"
  )
  out
}

#' @noRd
stcode_horseshoe_block = function(out, J, K, tau){
  stcode_append(out$param) = glue(
    "// group shrinkage parameters",
    set_var("nu1_g", "vector", dims=J, lower=0),
    set_var("nu2_g", "vector", dims=J, lower=0),
    set_var("nu1_l", "vector", dims=K, lower=0, add_dims=J),
    set_var("nu2_l", "vector", dims=K, lower=0, add_dims=J), 
    set_var("beta_std", "vector", dims=K, add_dims=J),
    .sep="\n"
  )
  stcode_append(out$tfparamdef) = glue(
    set_var("beta", "vector", dims=K, add_dims=J),
    set_var("sigma_beta", "vector", dims=J, lower=0),
    set_var("nu12_l", "vector", dims=K, lower=0, add_dims=J),
    .sep="\n"
    #"vector[{K}] beta[{J}];\n
    #vector<lower=0>[{J}] sigma_beta;\n
     #vector<lower=0>[{K}] nu12_l[{J}];\n"
  )
  stcode_append(out$tfparam) = glue(
    "// shrinkage prior, global and local levels",
    "sigma_beta = nu1_g .* sqrt(nu2_g);",
    "for(j in 1:{J}){{",
    "nu12_l[j] = nu1_l[j] .* sqrt(nu2_l[j]);",
    "beta[j] = beta_std[j].*nu12_l[j] * sigma_beta[j];",
    "}}",
    .sep="\n"
  )
  stcode_append(out$model) = glue(
    set_prior("nu1_g", "normal", c(0,1)),
    set_prior("nu2_g", "inv_gamma", c(0.5,0.5)),
    #'nu1_g ~ normal(0.0, 1.0);\n
    # nu2_g ~ inv_gamma(0.5, 0.5);\n
    #sigma_y ~ inv_gamma(1,1);\n
    "for(j in 1:{J}){{",
    set_prior("nu1_l[j]", "normal", c(0,1)),
    set_prior("nu2_l[j]", "inv_gamma", c(0.5*tau, 0.5*tau)),
    set_prior("beta_std[j]", "normal", c(0,1)),
    #nu1_l[j] ~ normal(0.0, 1.0);\n
    #nu2_l[j] ~ inv_gamma(0.5*{tau}, 0.5*{tau});\n
    #beta_std[j] ~ normal(0, 1);\n
    "}}",
    .sep="\n"
  )
  out
}

#' @noRd
stcode_CAR_block = function(out, J, P, G, N, Tp){
  stcode_append(out$data) = glue(
    '// Pathway data',
    set_var("adjmat", "matrix", dims= c(J,J), lower=0, add_dims=P),
    set_var("lambdas", "vector", dims=2L, add_dims=P),
    .sep="\n"
  )
  stcode_append(out$tfdata) = glue(
    #set_var("I", "matrix", dims=c(J,J), add_exp=glue("=diag_matrix(rep_vector(1,{J})"))
    set_var("I", "matrix", dims=c(J,J)),
    "I = diag_matrix(rep_vector(1,{J}));",
    .sep="\n"
  )
  stcode_append(out$param) = glue(
    "// Pathway coeff",
    set_var("phi_std", "vector", dims=2L, lower=0, upper=1, add_dims=P),
    set_var("sigma", "real", lower=0),
    .sep="\n"
     #vector<lower=0, upper=1>[2] phi_std[{P}];\n
     #real<lower=0> sigma;\n"
  )
  stcode_append(out$tfparamdef) = glue(
    set_var("phi", "vector", dims=G, add_dims=P),
    set_var("Sigma", "cov_matrix", dims=J, add_dims=G),
    .sep="\n"
    #"vector[{G}] phi[{P}];\n
    #cov_matrix[{J}] Sigma[{G}];\n"
  )
  for(g in 1:G){
  stcode_append(out$tfparam) = glue(
    "// covariance for CAR level",
    "Sigma[{g}] = rep_matrix(0,{J},{J});",
    "for(p in 1:{P}){{",
    "phi[p,{g}] = 1/(lambdas[p,1])+0.005 + (1/(lambdas[p,2]) - 1/(lambdas[p,1])-0.005) * phi_std[p,{g}];",
    "Sigma[{g}] = Sigma[{g}] + phi[p,{g}]*adjmat[p];",
    "}}",
    "Sigma[{g}] = (I-Sigma[{g}]/{P})/sigma;",
    .sep="\n"
  )}
  stcode_append(out$model) = glue(
    "// priors on pathway significance",
    "for(g in 1:{G}){{",
    "for(p in 1:{P})",
    set_prior("phi_std[p,g]", "beta", c(0.5,0.5)),
    #phi_std[p,g] ~ beta(0.5, 0.5);
    "}}",
    "// variances",
    set_prior("sigma", "inv_gamma", c(N*Tp/4.0,N*Tp/4.0-1)),
    .sep="\n"
    #sigma ~ inv_gamma({N*Tp}/4.0,{N*Tp}/4.0-1);\n"
  )
  out
}

#' @noRd
stcode_autoreg = function(out, N, Tp, J, rep_data){
  stcode_append(out$param) = glue(
    "// autoregression",
    set_var("theta", "real", lower=-1, upper=1, add_dims = J),
    .sep="\n"
    #real<lower=-1, upper=1> theta[{J}];\n'
  )
  stcode_append(out$tfparamdef) = glue(
    set_var("Xm", "matrix", dims=c(N,J), add_dims=Tp),
    set_var("mu", "matrix", dims=c(N,J), add_dims=Tp),
    .sep="\n"
    #'matrix[{N},{J}] Xm[{Tp}];\n
    #matrix[{N},{J}] mu[{Tp}];\n'
  )
  stcode_append(out$tfparam) = glue(
    "mu[1] = Xm[1]; // initialize",
    "for (i in 2:{Tp}){{",
      "for(j in 1:{J}) mu[i,1:{N},j] = Xm[i,1:{N},j] + theta[j]*({rep_data}[i-1,1:{N},j]-Xm[i-1,1:{N},j]);",
    "}}",
    .sep="\n"
  )
  out
}

#' @noRd
stcode_mixedeff = function(out, N, Tp, J, horseshoe=T, rep_data){
  stcode_append(out$data) = glue(
    "// level data",
    set_var("drug", "vector", dims=N, add_dims=Tp),
    set_var("groups", "int", add_dims=N, lower=1),
    .sep="\n"
    #'vector[{N}]  drug[{Tp}];\n
    #int<lower=1> groups[{N}];\n'
  )
  stcode_append(out$param) = glue(
    set_var("alpha", "real", add_dims=J),
    "//fixed intercept",
    set_var("inter", "real", add_dims=J),
    set_var("sigma_gamma", "real", lower=0, add_dims=J),
    set_var("err", "vector", dims=N, add_dims=J),
    .sep="\n"
    #'real alpha[{J}];\n
    #real inter[{J}]; // fixed intercept\n
    #// variances
    #real<lower=0> sigma_gamma[{J}];\n
    #vector[{N}] err[{J}];\n'
  )
  stcode_append(out$tfparamdef) = set_var("gamma", "vector", dims=N, add_dims=J)
  #vector[N] gamma[J]; // random intercept
  stcode_append(out$tfparam) = glue(
    "for(j in 1:{J}) gamma[j] = inter[j] + sqrt(sigma_gamma[j])*err[j];\n",
     "for (i in 1:{Tp}){{\n",
     "for(j in 1:{J}) Xm[i,1:{N},j] = gamma[j] + alpha[j]*drug[i]",
     {ifelse(horseshoe, " + {rep_data}[i]*(beta[j]);\n",";\n")},
    "}} // mixed eff \n"
  )
  stcode_append(out$model) = glue(
    set_prior("sigma_gamma", "inv_gamma", c(1,0.1)),
    "for(j in 1:{J})",
    set_prior("err[j]", "normal", c(0,1)),
    .sep="\n"
    #'sigma_gamma ~ inv_gamma(1,0.1);\n
    #for(j in 1:J) err[j] ~ normal(0,1);\n'
  )
  out
}



