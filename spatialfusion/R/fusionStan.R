fusion.dstan <- function(data, n.latent = 1, bans = 0,
                       pp.offset, verbose = FALSE, prior.pointbeta, prior.areabeta, prior.tausq, prior.phi, prior.z,
                       nsamples = 2000, nburnin = 1000, thinning = 1, nchain = 2, ncore = 2, adapt.delta = 0.95, ...){


  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package 'rstan' needed for this function to work. Please install it first.",
         call. = FALSE)
  }
# prepare -----------------------------------------------------------------

  data$n_w <- n.latent

  if (identical(bans, 0)){
    data$n_ban <- 0
    data$idx_bans <- NULL
  } else { # matrix class
    data$n_ban <- sum(bans)
    data$idx_bans <- which(bans == 1, arr.ind = TRUE)
  }

  if (data$n_pp_var > 0){
  if (missing(pp.offset)){
    data$offset <- matrix(rep(1, data$n_grid*data$n_pp_var), ncol = data$n_pp_var) # log(pp.offset) = 0
  } else {
    if (inherits(pp.offset, "numeric")){
      if (length(pp.offset) == data$n_pp_var){
        data$offset <- matrix(rep(pp.offset, each = data$n_grid), ncol = data$n_pp_var)
      } else if (length(pp.offset) == data$n_grid){
        data$offset <- matrix(rep(pp.offset, data$n_pp_var), ncol = data$n_pp_var)
      } else {stop("length of pp.offset must be either 1 or equal to the number of grids in grid_lrg in data")}
    } else if (inherits(pp.offset, "matrix")){
      if (nrow(pp.offset) != data$n_grid) stop("dimension of pp.offset must be either 1 or equal to the number of grids in grid_lrg in data")
    } else {
      stop("dimension of pp.offset must be of either class numeric or matrix")
    }
  }}

  stan.code <- function(distributions, priors, n_point_var, n_area_var, n_pp_var, n_ban, idx_bans, idx_norm, Z_pos){
    distributions[distributions == "poisson"] <- "poisson_log"
    distributions[distributions == "bernoulli"] <- "bernoulli_logit"
    #hyperparameters for pre-treatment mean mu
    a.point.beta <- priors$point.beta$pars[1]
    b.point.beta <- priors$point.beta$pars[2]
    dist.point.beta <- priors$point.beta$distr
    #hyperparameters  for overdispersion parameter kappa
    a.area.beta <- priors$area.beta$pars[1]
    b.area.beta <- priors$area.beta$pars[2]
    dist.area.beta <- priors$area.beta$distr
    #hyperparameters  for change in mean delta
    a.tausq <- priors$tausq$pars[1]
    b.tausq <- priors$tausq$pars[2]
    dist.tausq <- priors$tausq$distr
    #hyperparameters  for change in mean delta
    a.phi <- priors$phi$pars[1]
    b.phi <- priors$phi$pars[2]
    dist.phi <- priors$phi$distr
    #hyperparameters  for change in mean delta
    a.designmat <- priors$designmat$pars[1]
    b.designmat <- priors$designmat$pars[2]
    dist.designmat <- priors$designmat$distr

    paste0('data {
           int n_point;
           int n_area;
           int n_grid;
           int n_neighbor;
           int n_sample;
           int n_w;
           int n_point_var;
           int n_area_var;
           int n_pp_var;
           int n_ban;
           int n_norm; // number of normally distributed responses
           ',
           if (length(idx_norm) == 1){
             'int idx_norm;'
           } else {
             'int idx_norm[n_norm];'
           },
           if (n_point_var > 0){
             paste0(
             paste0(sapply(1:n_point_var, function(i){
               if (distributions[i] == "normal"){
                 paste0('vector[n_point] Y_point',i,';')
               } else if (distributions[i] == "poisson_log"){
                 paste0('int Y_point',i,'[n_point];')
               } else if (distributions[i] == "bernoulli_logit"){
                 paste0('int<lower=0,upper=1> Y_point',i,'[n_point];')
               }
             }), collapse = '\n '),
             'int p_point; // number of coefficient for point
              matrix[n_point, p_point] X_point; // design matrix for point')
           },
           if (n_point_var + n_pp_var > 0){'
           int nearind[n_point + n_grid - 1, n_neighbor];
           vector[n_neighbor] sC_site_nei[n_point + n_grid - 1];
           matrix[n_neighbor, n_neighbor] sC_nei[n_point + n_grid - 1];
           '},
           if (n_area_var > 0){
             paste0(
             paste0(sapply(1:n_area_var, function(i){
               if (distributions[i+n_point_var] == "normal"){
                 paste0('vector[n_area] Y_area',i,';')
               } else if (distributions[i+n_point_var] == "poisson_log"){
                 paste0('int Y_area',i,'[n_area];')
               } else if (distributions[i+n_point_var] == "bernoulli_logit"){
                 paste0('int<lower=0,upper=1> Y_area',i,'[n_area];')
               }
             }), collapse = '\n '),
             '
              matrix[n_area, n_sample] A1; // aggregation matrix for areal
              int p_area; // number of coefficient for area
              matrix[n_area, p_area] X_area; // design matrix for area
             ',
             if (n_point_var + n_pp_var > 0){
              'matrix[n_neighbor, n_neighbor] C_nei[n_sample];
             vector[n_neighbor] C_site_nei[n_sample];
             int nearind_sample[n_sample, n_neighbor];'
             } else {
              'matrix[n_neighbor, n_neighbor] C_nei[n_sample-1];
             vector[n_neighbor] C_site_nei[n_sample-1];
             int nearind_sample[n_sample-1, n_neighbor];
               '
             }
             )
           },
           if (n_pp_var > 0){
             paste0(paste0(sapply(1:n_pp_var, function(i){
                 paste0('int Y_pp',i,'[n_grid];')
             }), collapse = '\n '),
             '
              real offset[n_grid, n_pp_var];
              real area;')
           },
           '
           int Z_pos;
  }

           parameters{
           ',
           if (n_point_var > 0){
             paste0('vector[p_point] beta_p[n_point_var]; // coefficients
                    ')
           },
           if (n_area_var > 0){
             paste0('vector[p_area] beta_a[n_area_var];
                    ')
           },
           paste0(sapply(1:(n_point_var + n_area_var + n_pp_var), function(i){
              paste0('row_vector', if (i == Z_pos){'<lower = 0>'},'[n_w] Z_ban', i, ';')
           }), collapse = "\n "),
           '
           real<lower = 0> tau_sq[n_norm];
           positive_ordered[n_w] phi;
           matrix[n_w, n_point + n_grid + n_sample] noise;
           }

           transformed parameters{',
           if (n_point_var + n_pp_var > 0){'
           matrix[n_w, n_point + n_grid] w;
           matrix[n_w, n_point + n_grid] w_var;
           '
           },
           if (n_area_var > 0){
           paste0('
           matrix[n_w, n_sample] wa;
           matrix[n_w, n_sample] wa_var;
           vector[n_area] wA[n_area_var];
           ',
           if (n_point_var + n_pp_var > 0){
             'vector[n_neighbor] C_site_nei_C_nei_inv;
             vector[n_neighbor] C_site_nei_phi;
             matrix[n_neighbor,n_neighbor] C_nei_phi;
             '
           })
           },
           paste0(sapply(1:(n_point_var + n_area_var + n_pp_var), function(i){
             paste0('row_vector[n_w] Z_', i, ';')
           }), collapse = "\n "),
           '
           ',
           paste0(sapply(1:(n_point_var + n_area_var + n_pp_var), function(i){
             paste0('Z_', i, ' = Z_ban', i, ';')
           }), collapse = "\n "),
           if (n_ban > 0){
             paste0(sapply(1:n_ban, function(i){
               paste0('Z_',idx_bans[i,1], '[',idx_bans[i,2],'] = 0.0;')
             }), collapse = "\n ")
           },
           '
           for (l in 1:n_w){',
           if (n_point_var + n_pp_var> 0){'
           w_var[l, 1] = 1;
           w[l, 1] = sqrt(w_var[l, 1]) * noise[l, 1];
           // for transforming w
           for (i in 2:(n_point + n_grid)) {
           int dim;
           matrix[ i < (n_neighbor + 1)? (i - 1) : n_neighbor, i < (n_neighbor + 1)? (i - 1): n_neighbor] sC_nei_phi;
           vector[ i < (n_neighbor + 1)? (i - 1) : n_neighbor] sC_site_nei_phi;
           vector[ i < (n_neighbor + 1)? (i - 1) : n_neighbor] sC_site_nei_C_nei_inv;
           dim = (i < (n_neighbor + 1))? (i-1) : n_neighbor;
           if(dim == 1){sC_nei_phi[1, 1] = 1;}
           else{
           for (j in 1:dim){
           for (k in j:dim){
           sC_nei_phi[j, k] = exp(- sC_nei[(i - 1)][j,k] / phi[l]);
           sC_nei_phi[k, j] = sC_nei_phi[j, k];
           }}}
           sC_site_nei_phi = exp(- sC_site_nei[(i - 1)][1:dim] / phi[l]);
           sC_site_nei_C_nei_inv = mdivide_left_spd(sC_nei_phi, sC_site_nei_phi);// m by m times m by n
           w_var[l, i] = (1 - dot_product(sC_site_nei_C_nei_inv, sC_site_nei_phi));
           w[l, i] = dot_product(sC_site_nei_C_nei_inv, w[l, nearind[i-1, 1:dim]]) + sqrt(w_var[l, i]) * noise[l, i]; // 1 by m, m by 1
           }
           '
           },
           if (n_area_var > 0){
             paste0(
             if (n_point_var + n_pp_var == 0){
               '
               wa_var[l, 1] = 1;
               wa[l, 1] = sqrt(wa_var[l, 1]) * noise[l, 1];
               for (i in 2:n_sample) {
                 int dim;
                 matrix[ i < (n_neighbor + 1)? (i - 1) : n_neighbor, i < (n_neighbor + 1)? (i - 1): n_neighbor] C_nei_phi;
                 vector[ i < (n_neighbor + 1)? (i - 1) : n_neighbor] C_site_nei_phi;
                 vector[ i < (n_neighbor + 1)? (i - 1) : n_neighbor] C_site_nei_C_nei_inv;
                 dim = (i < (n_neighbor + 1))? (i-1) : n_neighbor;
                 if(dim == 1){C_nei_phi[1, 1] = 1;}
                 else{
                   for (j in 1:dim){
                     for (k in j:dim){
                       C_nei_phi[j, k] = exp(- C_nei[(i - 1)][j,k] / phi[l]);
                       C_nei_phi[k, j] = C_nei_phi[j, k];
                     }}}
                 C_site_nei_phi = exp(- C_site_nei[(i - 1)][1:dim] / phi[l]);
                 C_site_nei_C_nei_inv = mdivide_left_spd(C_nei_phi, C_site_nei_phi);// m by m times m by n
                 wa_var[l, i] = (1 - dot_product(C_site_nei_C_nei_inv, C_site_nei_phi));
                 wa[l, i] = dot_product(C_site_nei_C_nei_inv, wa[l, nearind_sample[i-1, 1:dim]]) + sqrt(wa_var[l, i]) * noise[l, i]; // 1 by m, m by 1
               '
             } else {
               '
           // for transforming wa
           for (i in 1:n_sample) { // for each predicted location
           for (j in 1:n_neighbor){
           C_site_nei_phi[j] = exp(- C_site_nei[i][j]/phi[l]);
           }
           for (j in 1:n_neighbor){
           for (k in j:n_neighbor){
           C_nei_phi[j,k] = exp(- C_nei[i][j,k]/phi[l]);
           C_nei_phi[k,j] = C_nei_phi[j,k];
           }}
           C_site_nei_C_nei_inv = mdivide_left_spd(C_nei_phi, C_site_nei_phi);// m by m times m by n
           wa_var[l, i] = (1 - dot_product(C_site_nei_C_nei_inv, C_site_nei_phi));
           wa[l, i] = dot_product(C_site_nei_C_nei_inv, append_row(to_vector(w[l, ]), to_vector(wa[l, ]))[nearind_sample[i,]]) + sqrt(wa_var[l, i]) * noise[l, n_point + n_grid + i]; // 1 by m, m by 1
           '
             },
           '
           }}
           ',
             paste0(sapply(1:n_area_var, function(i){
                      if (distributions[i + n_point_var] == "normal"){
                        paste0('wA[',i,'] = A1 * to_vector(Z_',i + n_point_var,' * wa); // identity link ')
                      } else if (distributions[i + n_point_var] == "poisson_log"){
                        paste0('wA[',i,'] = log(A1 * to_vector(exp(Z_',i + n_point_var,' * wa))); // log link ')
                      } else if (distributions[i + n_point_var] == "bernoulli_logit") {
                        paste0('wA[',i,'] = log(A1 * to_vector(exp(Z_',i + n_point_var,' * wa)./(1 + exp(Z_',i + n_point_var,' * wa))) ./(1 - A1 * to_vector(exp(Z_',i + n_point_var,' * wa)./(1 + exp(Z_',i + n_point_var,' * wa))))); // logit link ')
                      }
                    }), collapse = '\n '))} else {'}'},
           '
}

           model{
           tau_sq ~ ',dist.tausq,'(',a.tausq,',',b.tausq,');
           phi ~ ',dist.phi,'(',a.phi,',',b.phi,');
           for (i in 1:n_w){
           noise[i,] ~ normal(0, 1);
           }
  ',
           paste0(sapply(1:(n_point_var+n_area_var+n_pp_var),
                         function(i) paste0('Z_',i,' ~ ',dist.designmat,'(',a.designmat,',',b.designmat,');\n')), collapse = ''),
           if (n_point_var > 0){
             paste0(sapply(1:n_point_var, function(i){
               paste0('beta_p[',i,'] ~ ',dist.point.beta,'(',a.point.beta,',',b.point.beta,');
                      Y_point',i,' ~ ',distributions[i],'(X_point * beta_p[',i,'] + to_vector(Z_',i,' * w[,1:n_point])',if(i %in% idx_norm){paste0(', sqrt(tau_sq[',which(idx_norm==i),'])')},');\n')
             }), collapse = '')
           },
           if (n_area_var > 0){
             paste0(sapply(1:n_area_var, function(i){
               paste0('beta_a[',i,'] ~ ',dist.area.beta,'(',a.area.beta,',',b.area.beta,');
                      Y_area',i,' ~ ',distributions[i+n_point_var],'(X_area * beta_a[',i,'] + wA[',i,']',if((i+n_point_var) %in% idx_norm){paste0(', sqrt(tau_sq[',which(idx_norm == i+n_point_var),'])')},');\n')
             }), collapse = '')
           },
           if (n_pp_var > 0){
             paste0(sapply(1:n_pp_var, function(i){
               paste0('Y_pp',i,' ~ poisson_log','(log(area) + to_vector(log(offset[,',i,'])) + to_vector(Z_',i+n_point_var+n_area_var,' * w[,(n_point+1):(n_point + n_grid)]));\n')
             }), collapse = '')
           },'}
  '
    )}

  data$n_norm <- sum(data$distributions == "normal")
  data$idx_norm <- which(data$distributions == "normal")

  if (data$n_point > 0){
    for (i in 1:data$n_point_var){
      data[[paste0("Y_point",i)]] <- data$Y_point[,i]
    }
  }
  if (data$n_area > 0){
    for (i in 1:data$n_area_var){
      data[[paste0("Y_area",i)]] <- data$Y_area[,i]
    }
  }
  if (data$n_grid > 0){
    for (i in 1:data$n_pp_var){
      data[[paste0("Y_pp",i)]] <- data$Y_pp[,i]
    }
  }

  # find rows of Z to add >0 constraints on the design matrix for identifiability
  if (identical(bans, 0)){
    data$Z_pos <- 1
  } else {
    data$Z_pos <- min(which(rowSums(bans) == 0))
  }

  priors <- setPrior(prior.pointbeta, prior.areabeta, prior.tausq, prior.phi, prior.z)
  code <- stan.code(data$distributions, priors, data$n_point_var, data$n_area_var, data$n_pp_var, data$n_ban, data$idx_ban, data$idx_norm, data$Z_pos)

  stan.model <- stan_model(model_name = "fusion", model_code = code)

  if (verbose){
    samples <- sampling(stan.model, data = data, iter = nsamples, warmup=nburnin, chains=nchain,
                        thin=thinning,control = list(adapt_delta = adapt.delta), cores=ncore)

  } else {
    samples <- suppressMessages(
      suppressWarnings(
        sampling(stan.model, data = data, iter = nsamples,
                 warmup = nburnin, chains = nchain, thin = thinning,
                 control = list(adapt_delta = adapt.delta),cores = ncore, refresh = nsamples/10)))}

  checkDivergence(samples, adapt.delta)
  checkConvergence(samples)
  out <- list(model = samples, data = data, priors = priors)
  class(out) <- "fusionModel"
  return(out)

}
