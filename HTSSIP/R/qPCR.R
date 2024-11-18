# Simulate qPCR values
.qPCR_sim = function(Buoyant_density,
                     IS_CONTROL,
                     control_mean_fun,
                     control_sd_fun,
                     treat_mean_fun,
                     treat_sd_fun,
                     n_tech_rep=3,
                     ...){
  if(IS_CONTROL==TRUE){
    M = control_mean_fun(Buoyant_density, ...)
    S = control_sd_fun(Buoyant_density, ...)
    X = stats::rnorm(n=n_tech_rep, mean=M, sd=S)
  } else {
    M = treat_mean_fun(Buoyant_density, ...)
    S = treat_sd_fun(Buoyant_density, ...)
    X = stats::rnorm(n=n_tech_rep, mean=M, sd=S)
  }
  X = ifelse(X < 0, 0, X)
  return(X)
}

#' Simulate qPCR values
#'
#' qPCR values will be simulated for each sample in the provided phyloseq object.
#' The error distribution for each sample is drawn from a Gaussian distribution,
#' where the mean and standard deviation of the Gaussian distribution are set by
#' user-defined functions. The user-defined functions that take buoyant density
#' as input and returns a numeric value (see examples), which allows the qPCR
#' values to increase in mean & variance at certain buoyant densities.
#'
#' @param physeq  Object of class "phyloseq"
#' @param control_expr  Expression used to identify control samples based on sample_data.
#' @param control_mean_fun  Function used for simulating the qPCR normal distribution mean
#' for control samples.
#' @param control_sd_fun  Function used for simulating the qPCR normal distribution
#' standard deviation for control samples.
#' @param treat_mean_fun  Function used for simulating the qPCR normal distribution mean
#' for treatment samples.
#' @param treat_sd_fun  Function used for simulating the qPCR normal distribution
#' standard deviation for treatment samples.
#' @param n_tech_rep  Number of technical replicates.
#'
#' @return data.frame of qPCR values
#'
#' @export
#'
#' @examples
#' # making functions for simulating values
#' ## 'x' will be Buoyant_density as defined in the phyloseq object sample_data
#' control_mean_fun = function(x) dnorm(x, mean=1.70, sd=0.01) * 1e8
#' ## This will set sd to scale with the mean
#' control_sd_fun = function(x) control_mean_fun(x) / 3
#' ## This will 'shift' the gene copy distribution to 'heavier' BDs
#' treat_mean_fun = function(x) dnorm(x, mean=1.75, sd=0.01) * 1e8
#' treat_sd_fun = function(x) treat_mean_fun(x) / 3
#' # simulating qPCR values
#' df_qPCR = qPCR_sim(physeq_S2D2,
#'                 control_expr='Substrate=="12C-Con"',
#'                 control_mean_fun=control_mean_fun,
#'                 control_sd_fun=control_sd_fun,
#'                 treat_mean_fun=treat_mean_fun,
#'                 treat_sd_fun=treat_sd_fun)
#'
#' # using the Cauchy distribution instead of normal distributions
#' control_mean_fun = function(x) dcauchy(x, location=1.70, scale=0.01) * 1e8
#' control_sd_fun = function(x) control_mean_fun(x) / 3
#' treat_mean_fun = function(x) dcauchy(x, location=1.74, scale=0.01) * 1e8
#' treat_sd_fun = function(x) treat_mean_fun(x) / 3
#' # simulating qPCR values
#' df_qPCR = qPCR_sim(physeq_S2D2,
#'                 control_expr='Substrate=="12C-Con"',
#'                 control_mean_fun=control_mean_fun,
#'                 control_sd_fun=control_sd_fun,
#'                 treat_mean_fun=treat_mean_fun,
#'                 treat_sd_fun=treat_sd_fun)
#'
qPCR_sim = function(physeq,
                    control_mean_fun,
                    control_sd_fun,
                    treat_mean_fun,
                    treat_sd_fun,
                    n_tech_rep=3,
                    control_expr=NULL){
  # sample_metadata
  m = phyloseq2df(physeq, phyloseq::sample_data)
  if(is.null(m$IS_CONTROL)){
    if(is.null(control_expr)){
      stop('sample_data in phyloseq object must have IS_CONTROL column (logical), or you must provide the control_expr parameter')
    }
    m = m %>%
      dplyr::mutate_(IS_CONTROL = control_expr)
  }

  if(is.null(m$Buoyant_density)){
    stop('Buoyant_density column not found in phyloseq object sample_data')
  }
  m_f = m %>%
    dplyr::mutate_(Buoyant_density = "as.numeric(as.character(Buoyant_density))") %>%
    dplyr::select_("Buoyant_density", "IS_CONTROL")

  df_qPCR = plyr::mdply(m_f, .qPCR_sim,
                        control_mean_fun=control_mean_fun,
                        control_sd_fun=control_sd_fun,
                        treat_mean_fun=treat_mean_fun,
                        treat_sd_fun=treat_sd_fun,
                        n_tech_rep=n_tech_rep)

  colnames(df_qPCR)[3:(2+n_tech_rep)] = gsub('^', 'qPCR_tech_rep', 1:n_tech_rep)
  df_qPCR$Sample = phyloseq::sample_data(physeq) %>% rownames

  # gather & summarize
  sel_cols = colnames(df_qPCR)
  sel_cols = sel_cols[grepl("^qPCR_tech_rep", sel_cols)]
  df_qPCR_s = df_qPCR %>%
    tidyr::gather("qPCR_tech_rep_id", "qPCR_tech_rep_value", 
    			  -"Sample", -"Buoyant_density", -"IS_CONTROL") %>%
    dplyr::group_by_("IS_CONTROL", "Sample", "Buoyant_density") %>%
    dplyr::summarize_(qPCR_tech_rep_mean = "mean(qPCR_tech_rep_value)",
                      qPCR_tech_rep_sd = "stats::sd(qPCR_tech_rep_value)") %>%
    dplyr::ungroup() %>%
    as.data.frame

  # adding sample metadata
  m = phyloseq2df(physeq, phyloseq::sample_data)
  m$TABLE__JOIN = rownames(m)
  m$Buoyant_density = NULL
  m$Sample = NULL
  m$qPCR_tech_rep_mean = NULL
  m$qPCR_tech_rep_sd = NULL
  df_qPCR = dplyr::inner_join(df_qPCR, m, c('Sample'='TABLE__JOIN'))
  df_qPCR_s = dplyr::inner_join(df_qPCR_s, m, c('Sample'='TABLE__JOIN'))

  # checking output
  if(sum(df_qPCR_s$IS_CONTROL == TRUE) == 0){
    warning('No samples selected as controls')
  }

  return(list(raw=df_qPCR, summary=df_qPCR_s))
}



