#' Simulate HTS-SIP communities for 1 density gradient
#'
#' @param locs  Buoyant densities of each gradient fraction
#' @param params  A matrix of parameters for \code{coenocliner::coenocline()}.
#' See that function's documentation for more details.
#' @param responseModel  See \code{coenocliner::coenocline()}
#' @param countModel  See \code{coenocliner::coenocline()}
#' @param ...  Other parameters passed to \code{coenocliner::coenocline()}
#'
#' @return A data.frame of OTU counts.
#'
#' @export
#'
#' @examples
#' # setting parameters
#' set.seed(2)
#' M = 10                                  # number of species (OTUs)
#' ming = 1.67                             # gradient minimum...
#' maxg = 1.78                                # ...and maximum
#' nfrac = 24                                 # number of gradient fractions
#' locs = seq(ming, maxg, length=nfrac)       # gradient fraction BD values
#' tol = rep(0.005, M)                       # species tolerances
#' h = ceiling(rlnorm(M, meanlog=11))    # max abundances
#' opt = rnorm(M, mean=1.7, sd=0.005)      # species optima
#' params = cbind(opt=opt, tol=tol, h=h)  # put in a matrix
#' # simulate the OTU abundances
#' df_OTU = gradient_sim(locs, params)
#' head(df_OTU)
#'
gradient_sim = function(locs, params,
                        responseModel='gaussian',
                        countModel='poisson',
                        ...){
  df_OTU = coenocliner::coenocline(locs,
                                   params=params,
                                   responseModel=responseModel,
                                   countModel=countModel,
                                   ...)
  df_OTU = as.data.frame(df_OTU)
  colnames(df_OTU) = gsub('^', 'OTU.', 1:nrow(params))
  #print(df_OTU)
  df_OTU$Buoyant_density = locs   #as.character(round(locs, digits=4))
  df_OTU$Fraction = df_OTU$Buoyant_density %>% as.factor %>% as.numeric
  return(df_OTU)
}


#' Simulate a HTS-SIP dataset
#'
#' This is a simple method for simulating high thoughput sequencing
#' stable isotope probing datasets and is mainly used for package testing
#' purposes. See \code{SIPSim} for more detailed and simulation pipeline.
#'
#' @inheritParams gradient_sim
#' @param meta  Data.frame object of metadata to add to \code{sample_data} table.
#' The data.frame object must have a 'Gradient' column, which is used for joining
#' with \code{dplyr::left_join()}.
#' @param sim_tree  Simulate a tree?
#' @param parallel  Parallel processing. See \code{.parallel} option in
#' \code{dplyr::mdply()} for more details.
#'
#' @return A phyloseq object
#'
#' @export
#'
#' @examples
#' # setting parameters for tests
#' set.seed(2)
#' M = 10                                  # number of species
#' ming = 1.67                             # gradient minimum...
#' maxg = 1.78                                # ...and maximum
#' nfrac = 24                                 # number of gradient fractions
#' locs = seq(ming, maxg, length=nfrac)       # gradient locations
#' tol  = rep(0.005, M)                       # species tolerances
#' h    = ceiling(rlnorm(M, meanlog=11))    # max abundances
#' ## creating parameter matrices for each density gradient
#' opt1 = rnorm(M, mean=1.7, sd=0.005)      # species optima
#' params1 = cbind(opt=opt1, tol=tol, h=h)  # put in a matrix
#' opt2 = rnorm(M, mean=1.7, sd=0.005)      # species optima
#' params2 = cbind(opt=opt2, tol=tol, h=h)  # put in a matrix
#' param_l = list(
#'   '12C-Con_rep1' = params1,
#'   '13C-Cel_rep1' = params2
#' )
#' \dontrun{
#' # simulating phyloseq object
#' physeq = HTSSIP_sim(locs, param_l)
#' physeq
#' }
#'
HTSSIP_sim = function(locs, params,
                   responseModel='gaussian',
                   countModel='poisson',
                   meta=NULL,
                   sim_tree=FALSE,
                   parallel=FALSE,
                   ...){
  # making & combining OTU tables (1 per gradient)
  df_OTU = plyr::ldply(params, gradient_sim,
                       locs=locs,
                       responseModel=responseModel,
                       countModel=countModel,
                       .parallel=parallel,
                       .id='Gradient',
                       ...)

  # vary the BDs a bit
  x = stats::rnorm(nrow(df_OTU), mean=0, sd=0.002)
  df_OTU$Buoyant_density = as.Num(df_OTU$Buoyant_density) + x
  df_OTU$Buoyant_density = round(df_OTU$Buoyant_density, digits=6)

  # metadata
  X = c('Gradient', 'Buoyant_density', 'Fraction')
  df_meta = df_OTU[,X]
  df_OTU$Fraction = NULL

  # adding to metadata
  if(!is.null(meta)){
    df_meta = dplyr::left_join(df_meta, meta, c('Gradient'='Gradient')) %>%
      as.data.frame
  }

  # formatting OTU table
  rownames(df_OTU) = gsub(' ', '', apply(df_meta[,X], 1, paste, collapse="_"))
  df_OTU$Gradient = NULL
  df_OTU$Buoyant_density = NULL
  df_OTU = t(df_OTU)

  # sample matching between metdata & OTU table
  rownames(df_meta) = colnames(df_OTU)

  # making phyloseq object
  physeq = phyloseq::phyloseq(
    phyloseq::otu_table(df_OTU, taxa_are_rows=TRUE),
    phyloseq::sample_data(df_meta)
  )

  # simulating a tree
  if(sim_tree==TRUE){
    tree = ape::rtree(nrow(df_OTU))
    tree$tip.label = rownames(df_OTU)
    physeq = phyloseq::merge_phyloseq(physeq, tree)
  } else {
    tree = NULL
  }

  return(physeq)
}

