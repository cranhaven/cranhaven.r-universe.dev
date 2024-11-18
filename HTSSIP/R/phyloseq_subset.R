#' Get parameters for subsetting the phyloseq dataset
#'
#' This function is needed if you want to make multiple
#' subsets of the phyloseq object in order to make specific
#' comparisons between isotopically labeled-treatments and
#'
#'  their corresponding controls (eg., from the same time point).
#'
#' Makes a data.frame of all of the parameter values that differ
#' among the treatment-control comparisons.
#'
#' For example, if you want to compare the gradient fractions from
#' each labeled-treatment to its corresponding unlabeled-Control (both from the
#' same time point).
#'
#' @param physeq  Phyloseq object
#' @param exp_params  a vector listing the columns in the phyloseq sample_data
#'   table that can subset the phyloseq dataset in order to make the specific
#'   labeled-treatment vs labeled-control comparisons that you would like to make.
#' @param treatment  This is an expression used to filter out the
#'   control-specific parameters (if needed).
#'
#' @export
#'
#' @examples
#' data(physeq_S2D2)
#' # Here, the treatment/controls (12C & 13C) are listed in substrate,
#' # and should be matched by 'Day'. The 13C-treatments can be identified by
#' # the expression: "Substrate != '12C-Con'"
#' get_treatment_params(physeq_S2D2, c('Substrate', 'Day'), "Substrate != '12C-Con'")
#'
get_treatment_params = function(physeq, exp_params, treatment=NULL){
  physeq_m = phyloseq2df(physeq, phyloseq::sample_data)
  # filter out control (if needed; depending on subsetting expression)
  if(!is.null(treatment)){
    physeq_m = dplyr::filter_(physeq_m, treatment)
  }

  # all pairwise params
  params = dplyr::distinct(physeq_m[,exp_params] %>% as.data.frame)
  colnames(params) = exp_params
  return(params)
}

#' Extract all quoted values in the expression used for phyloseq subsetting.
#'
#' This can be useful for creating custom (shorter) labels for each subset
#' relative to using the entire subsetting expression.
#'
#' @param ex  Expression for subsetting the phyloseq object
#' @param collapse  Similar to the collapse parameter in \code{base::paste}
#'
#' @return If \code{length(ex) == 1}, then a vector of quoted values in the input string.
#' If \code{length(ex) > 1}, then a matrix or list of quote values,
#' 1 column/index per input string.
#'
#' @export
#'
#' @examples
#' ex = '(Substrate=="12C-Con" & Day=="14")'
#' expr_param_extract(ex)
#'
#' ex = '(Substrate=="12C-Con" & Day=="14") | (Substrate=="13C-Cel" & Day == "14")'
#' expr_param_extract(ex)
#'
#' # returns a matrix
#' ex = c('(Substrate=="12C-Con" & Day=="14")',
#'        '(Substrate=="13C-Cel" & Day == "14")')
#' expr_param_extract(ex)
#'
#' # returns a list
#' ex = c('(Substrate=="12C-Con" & Day=="14")',
#'        '(Substrate=="13C-Cel" & Day == "14")',
#'        '(Substrate=="13C-Cel")')
#' expr_param_extract(ex)
#'
expr_param_extract = function(ex, collapse=NULL){
  if(length(ex)==1){
    x = stringr::str_extract_all(ex, "[\"'][^\"']+[\"']")
    x = gsub("[\"']", "", x[[1]])
    if(is.null(collapse)==FALSE){
      x = paste(x, collapse=collapse)
    }
  } else{
    x = sapply(ex, expr_param_extract, collapse=collapse)
  }
  return(x)
}

#' Make a list of phyloseq object subsets
#'
#' Create a list of phyloseq object subsets based on phyloseq
#' sample data parameters (e.g., a phyloseq subset for each treatment)
#'
#' @param physeq  Phyloseq object
#' @param params  data.frame of parameters supplies to \code{ex}
#' @param ex  Expression for subsetting the phyloseq object
#'
#' @return A list of Phyloseq objects
#'
#' @export
#'
#' @examples
#' data(physeq_S2D2)
#' # making subsets by substrate and time point
#' params = get_treatment_params(physeq_S2D2, c('Substrate', 'Day'))
#' # filtering out controls
#' params = dplyr::filter(params, Substrate!='12C-Con')
#' # making expression for subsetting labeled-unlabeled gradient comparisons
#' ex = "(Substrate=='12C-Con' & Day=='${Day}') | (Substrate=='${Substrate}' & Day == '${Day}')"
#' physeq_l = phyloseq_subset(physeq_S2D2, params, ex)
#' physeq_l
#'
phyloseq_subset = function(physeq, params, ex){

  if(is.data.frame(params)){
    params_n = apply(params, 1, function(x, ex){
      stringterpolate(ex, as.list(x))
    }, ex=ex)
    params = apply(params, 1, as.list)
    names(params) = params_n
  }

  physeq_l = plyr::llply(params, function(x){
    # x should be a list of parameters
    exx = stringterpolate(ex, x)
    physeq.m = phyloseq2df(physeq, phyloseq::sample_data)
    bool = dplyr::mutate_(physeq.m, exx)[,ncol(physeq.m)+1]
    phyloseq::prune_samples(bool, physeq)
  })
  return(physeq_l)
}
