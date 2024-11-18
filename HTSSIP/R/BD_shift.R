#' Adjusting BD range size if negative.
#'
#' If BD (buoyant density) range size is negative,
#' use BD_to_set value to set new BD_max. The \code{BD_to_set}
#' determines the \code{BD_max} if BD range is negative
#'
#' @param BD_range  BD range size
#' @param BD_min  Minimum BD value
#' @param BD_max  Maximum BD value
#' @param BD_to_set  Value added to BD_min to set new BD_max
#' @return New max BD value
#'
max_BD_range = function(BD_range, BD_min, BD_max, BD_to_set){
  # if BD_range is negative, use BD_to_set value to set new BD_max
  if(is.infinite(BD_range) | is.na(BD_range) | BD_range <= 0){
    return(BD_min + BD_to_set)
  } else {
    return(BD_max)
  }
}


#' Format phyloseq metadata for calculating BD range overlaps.
#'
#' @param physeq  Phyloseq object
#' @param ex  Expression for selecting the control samples to
#' compare to the non-control samples.
#' @param rep  Column specifying gradient replicates. If the column
#' is not present, then all are considered "Replicate=1"
#' @return a data.frame object of formatted metadata
#'
#'
#' @examples
#' \dontrun{
#' data(physeq_S2D1)
#' ex = "Substrate=='12C-Con'"
#' metadata = HTSSIP:::format_metadata(physeq_S2D1, ex)
#' }
#'
format_metadata = function(physeq, ex="Substrate=='12C-Con'", rep='Replicate'){
  # converting phyloseq sample data to data.frame
  metadata = phyloseq2df(physeq, table_func=phyloseq::sample_data)
  metadata$METADATA_ROWNAMES = rownames(metadata)

  # assertions
  stopifnot(all(c('Buoyant_density', 'Fraction') %in% colnames(metadata)))

  # adding replicate (if provided)
  if(is.null(rep)){
    stop('rep cannot be null; it must be new or existing column name')
  }
  if(! rep %in% colnames(metadata)){
    metadata[,rep] = 1
  }

  # formatting
  metadata$BD_min = NULL
  metadata = metadata %>%
    dplyr::mutate_(IS__CONTROL = ex) %>%
    dplyr::rename_('BD_min' = "Buoyant_density") %>%
    dplyr::mutate_(Fraction = "as.numeric(as.character(Fraction))",
                   BD_min = "as.numeric(as.character(BD_min))") %>%
    dplyr::arrange_("BD_min") %>%
    dplyr::group_by_("IS__CONTROL", rep) %>%
    dplyr::mutate_(BD_max = "lead(BD_min)",
                   BD_max = "ifelse(is.na(BD_max), BD_min, BD_max)",
                   BD_range = "BD_max - BD_min") %>%
    dplyr::group_by() %>%
    dplyr::mutate_(median_BD_range = "stats::median(BD_range, na.rm=T)") %>%
    dplyr::ungroup()

  metadata$BD_max = mapply(max_BD_range, metadata$BD_range,
                           metadata$BD_min, metadata$BD_max,
                           BD_to_set = metadata$median_BD_range)
  metadata = metadata %>%
    dplyr::mutate_(BD_range = "BD_max - BD_min") %>%
    dplyr::select_("METADATA_ROWNAMES", rep, "IS__CONTROL", "BD_min", "BD_max", "BD_range")

  return(metadata)
}


#' Calculate the percent overlap between two ranges (x & y).
#'
#' The fraction of overlap is relative to Range X (see examples).
#'
#' @param x.start  The start value for Range X
#' @param x.end  The end value for Range X
#' @param y.start  The start value for Range Y
#' @param y.end  The end value for Range Y
#'
#' @return the percent overlap of the ranges
#'
#' @examples
#' \dontrun{
#' x = HTSSIP:::perc_overlap(0, 1, 0, 0.5)
#' stopifnot(x == 50)
#' x = HTSSIP:::perc_overlap(0, 0.5, 0, 1)
#' stopifnot(x == 100)
#' }
#'
perc_overlap = function(x.start, x.end, y.start, y.end){
#  if(x.start == y.start & x.end == y.end){
#    return(100)
#  }
  x.len = abs(x.end - x.start)
  # largest start
  max.start = max(c(x.start, y.start))
  min.end = min(c(x.end, y.end))
  overlap = min.end - max.start
  overlap = ifelse(overlap <= 0, 0, overlap)
  perc_overlap = overlap / x.len * 100
  return(perc_overlap)
}


#' Calculate the BD range overlap of gradient fractions
#'
#' @param metadata  Metdata data.frame object. See \code{format_metadata()}.
#' @return a data.frame object of metadata with fraction BD overlaps
#'
#' @examples
#' \dontrun{
#' data(physeq_S2D2)
#' ex = "Substrate=='12C-Con'"
#' metadata = HTSSIP:::format_metadata(physeq_S2D2, ex)
#' m = HTSSIP:::fraction_overlap(metadata)
#' head(m)
#' }
#'
fraction_overlap = function(metadata){
  stopifnot(all(c('METADATA_ROWNAMES', 'IS__CONTROL') %in%
                  colnames(metadata)))

  meta_cont = filter_(metadata, "IS__CONTROL==TRUE")
  stopifnot(nrow(meta_cont) > 0)
  meta_treat = filter_(metadata, "IS__CONTROL==FALSE")
  stopifnot(nrow(meta_treat) > 0)

  # merging; calculating fraction overlap; filtering
  metadata_j = base::merge(meta_cont, meta_treat, by=NULL)
  metadata_j$perc_overlap = mapply(perc_overlap,
                                   metadata_j$BD_min.x,
                                   metadata_j$BD_max.x,
                                   metadata_j$BD_min.y,
                                   metadata_j$BD_max.y)
  metadata_j = dplyr::filter(metadata_j, perc_overlap > 0)
  stopifnot(nrow(metadata_j) > 0)
  return(metadata_j)
}

#' Filtering out non-relevant distances in distance matrix
#'
#' @param d  a distance matrix object
#' @return a data.frame object of metadata with fraction BD overlaps
#'
#' @examples
#' \dontrun{
#' data(physeq_S2D2)
#' physeq_S2D2_d = phyloseq::distance(physeq_S2D2,
#'                              method='unifrac',
#'                              weighted=TRUE,
#'                              fast=TRUE,
#'                              normalized=FALSE)
#' physeq_S2D2_d = HTSSIP:::parse_dist(physeq_S2D2_d)
#' head(physeq_S2D2_d)
#' }
#'
parse_dist = function(d){
  stopifnot(class(d)=='dist')

  df = d %>% as.matrix %>% as.data.frame
  df$sample = rownames(df)
  df = df %>%
    tidyr::gather('sample.y', 'distance', -sample) %>%
    dplyr::rename_('sample.x' = "sample") %>%
    dplyr::filter_("sample.x != sample.y")
  return(df)
}


#' Calculating weighted mean beta-diversities of overlapping gradient fractions.
#'
#' @param df_dist  Filtered distance matrix in data.frame format.
#' See \code{parse_dist()}
#' @return a data.frame object of weighted mean distances
#'
overlap_wmean_dist = function(df_dist){

  # calculating weighted mean distance
  df_dist_s = df_dist %>%
    dplyr::group_by_("sample.x", "BD_min.x") %>%
    dplyr::mutate_(n_over_fracs = "n()",
                   wmean_dist = "stats::weighted.mean(distance, perc_overlap)") %>%
    dplyr::ungroup() %>%
    dplyr::distinct_("sample.x", "wmean_dist", .keep_all=TRUE)
  return(df_dist_s)
}


# permuting OTU abundance across samples
.perm_otu = function(physeq, replace=FALSE, adjacent=FALSE,
                     template=NULL, metadata=NULL, n_lead=3){

  # permute
  otu = phyloseq::otu_table(physeq) %>% as.data.frame
  otu_names = rownames(otu)
  samp_names = colnames(otu)
  n_otu = nrow(otu)
  n_samp = ncol(otu)
  otu = otu %>% as.matrix
  # if template, expanding OTU table to match template
  if(!is.null(template)){
    n_samp = phyloseq::otu_table(template) %>% ncol
    otu = otu[,base::sample(1:ncol(otu), n_samp, replace=TRUE)]
  }

  # permute
  if(adjacent == TRUE){
    if(is.null(metadata)){
      stop('metadata cannot be null for "adjacent"')
    }
    # ordering by BD_min
    metadata = metadata[metadata$METADATA_ROWNAMES %in%
                          colnames(phyloseq::otu_table(template)),]
    colnames(otu) = colnames(phyloseq::otu_table(template))
    metadata = metadata %>%
      dplyr::group_by_('Replicate') %>%
      dplyr::mutate_(Fraction = 'as.numeric(as.factor(BD_min))') %>%
      dplyr::ungroup() %>%
      dplyr::arrange_('Fraction')
    otu = otu[,metadata$METADATA_ROWNAMES] %>% t

    # permutating across just adjacent samples
    n_rep = metadata$Replicate %>% unique %>% length
    strata = rep(1:nrow(otu), each=n_rep * n_lead)[1:nrow(otu)]
    ## dealing with edge cases
    if(strata[length(strata)] != strata[length(strata)-1]){
      strata[length(strata)] = strata[length(strata)-1]
    }
    ## permuting
    otu = vegan::permatfull(otu,
                            fixedmar="both", shuffle="samp",
                            strata=strata, times=1)
    otu = otu$perm[[1]] %>% t
  } else {
    # permuting across all samples ('ind' by vegan orientation)
    otu = vegan::permatfull(otu, fixedmar="both", shuffle="ind", times=1)
    otu = otu$perm[[1]]
  }

  # re-making phyloseq object
  if(is.null(template)){
    rownames(otu) = otu_names
    colnames(otu) = samp_names
    otu = phyloseq::otu_table(otu, taxa_are_rows=TRUE)
    physeq = phyloseq::phyloseq(otu,
                                phyloseq::sample_data(physeq, errorIfNULL=FALSE),
                                phyloseq::phy_tree(physeq, errorIfNULL=FALSE))
  } else {
    rownames(otu) = phyloseq::otu_table(template) %>% rownames
    colnames(otu) = phyloseq::otu_table(template) %>% colnames
    otu = phyloseq::otu_table(otu, taxa_are_rows=TRUE)
    physeq = phyloseq::phyloseq(otu,
                                phyloseq::sample_data(template, errorIfNULL=FALSE),
                                phyloseq::phy_tree(template, errorIfNULL=FALSE))
  }
  # return
  return(physeq)
}

# creating a list of overlapping fractions; overlap determined by control
.overlap_fracs = function(cont_frac_id, metadata_overlap){
  treat_fracs = metadata_overlap[metadata_overlap$METADATA_ROWNAMES.x == cont_frac_id,
                                 'METADATA_ROWNAMES.y']
  fracs = unique(c(cont_frac_id, treat_fracs))
  return(fracs)
}

# permuting overlapping fractions; overlapping treatments for each control fraction
.perm_overlap = function(frac_ids, physeq, metadata_ord){
  physeq_sub = phyloseq::prune_samples(metadata_ord$METADATA_ROWNAMES %in% frac_ids,
                                       physeq)
  physeq_sub = .perm_otu(physeq_sub)
  return(physeq_sub)
}


# Calculating BD shift beta-diversity values
# @return a data.frame object of weighted mean distances
.BD_shift = function(perm_id, physeq, method='unifrac', weighted=TRUE,
                     fast=TRUE, normalized=FALSE, ex="Substrate=='12C-Con'",
                     perm_method = c('control', 'treatment', 'overlap', 'adjacent'),
                     parallel=FALSE){
  # param assertions
  perm_method = perm_method[1]

  # wrapper function
  ## formatting metadata
  physeq = physeq_format(physeq)
  metadata = format_metadata(physeq, ex)

  ## fraction overlap
  metadata_overlap = fraction_overlap(metadata)

  ## permuting OTU abundances in treatment fractions
  if(perm_id > 0){
    # ordering metadata
    metadata_ord = metadata %>% as.data.frame
    rownames(metadata_ord) = metadata_ord$METADATA_ROWNAMES
    metadata_ord = metadata_ord[phyloseq::sample_names(physeq),
                                1:ncol(metadata_ord)]
    # permutation method
    if(perm_method == 'adjacent'){ # null treatment = permuting OTU abundances among adjacent controls
      physeq_control = phyloseq::prune_samples(metadata_ord$IS__CONTROL==TRUE, physeq)
      physeq_treat = phyloseq::prune_samples(metadata_ord$IS__CONTROL==FALSE, physeq)
      physeq_treat = .perm_otu(physeq_control, adjacent=TRUE,
                               template=physeq_treat, metadata=metadata_ord)
      physeq = phyloseq::merge_phyloseq(physeq_control, physeq_treat)
    } else
    if(perm_method == 'overlap'){ # permuting OTU abundances across overlapping control & treatment
      # list of overlapping fractions
      cont_fracs = unique(metadata_overlap$METADATA_ROWNAMES.x)
      frac_ids = sapply(cont_fracs, .overlap_fracs, metadata_overlap=metadata_overlap)
      # parsing overlapping treatment fractions with each control
      physeq_l = lapply(frac_ids, .perm_overlap,
                          physeq=physeq, metadata_ord=metadata_ord)
      physeq_l[[length(physeq_l)+1]] = physeq  # for adding samples missing from perm
      # merging phyloseq objects back together
      n_samp = phyloseq::nsamples(physeq)
      n_tax = phyloseq::ntaxa(physeq)
      physeq = do.call(phyloseq::merge_phyloseq, physeq_l)
      ## assertions
      stopifnot(phyloseq::nsamples(physeq) == n_samp)
      stopifnot(phyloseq::ntaxa(physeq) == n_tax)
    } else
    if(perm_method == 'control'){ # permuting OTU abundances across all treatment samples
      physeq_control = phyloseq::prune_samples(metadata_ord$IS__CONTROL==TRUE, physeq)
      physeq_treat = phyloseq::prune_samples(metadata_ord$IS__CONTROL==FALSE, physeq)
      physeq_treat = .perm_otu(physeq_control, template=physeq_treat)
      physeq = phyloseq::merge_phyloseq(physeq_control, physeq_treat)
    } else
    if(perm_method == 'treatment'){ # permuting OTU abundances across all treatment samples
      physeq_control = phyloseq::prune_samples(metadata_ord$IS__CONTROL==TRUE, physeq)
      physeq_treat = phyloseq::prune_samples(metadata_ord$IS__CONTROL==FALSE, physeq)
      physeq_treat = .perm_otu(physeq_treat)
      physeq = phyloseq::merge_phyloseq(physeq_control, physeq_treat)
      } else {
      stop(sprintf('perm_method not recognized: %s', perm_method))
    }
  }

  # Calculating distances
  physeq_d = phyloseq::distance(physeq,
                                method='unifrac',
                                weighted=TRUE,
                                fast=TRUE,
                                normalized=FALSE,
                                parallel=FALSE)
  physeq_d = parse_dist(physeq_d)

  # joining dataframes
  physeq_d = dplyr::inner_join(physeq_d, metadata_overlap,
                             c('sample.x'='METADATA_ROWNAMES.x',
                               'sample.y'='METADATA_ROWNAMES.y'))

  # calculating weighted mean distance
  physeq_d_m = overlap_wmean_dist(physeq_d)


  # return
  return(physeq_d_m)
}


#' Assessing the magnitude of BD shifts with 16S rRNA community
#' data by calculating the beta diversity between unlabeled control
#' and labeled treatment gradient fraction communities.
#'
#' This function is meant to compare 16S rRNA sequence communities
#' of gradient fractions from 2 gradients: a labeled
#' treatment (eg., 13C-labeled DNA) and its corresponding unlabeled
#' control. First, the beta-diversity (e.g, weighted-Unifrac) is calculated
#' pairwise between fraction communities.
#'
#' The sample_data table of the user-provided phyloseq object
#' MUST contain the buoyant density (BD) of each sample
#' (a "Buoyant_density" column in the sample_data table).
#' The BD information is used to identify overlapping gradient fractions
#' (gradient fractions usually only partially overlap in BD between gradients)
#' between the labeled treatment gradient and the control gradient.
#' Beta diversity between overlapping fractions is calculated. Then,
#' to standardize the values relative to the unlabeled control
#' (1 beta-diversity value for each control gradient fraction), the
#' mean beta diversity of overlapping labeled treatment gradients is
#' calculated for each unlabeled control, and the percent overlap of
#' each labeled treatment fraction is used to weight the mean.
#'
#' A permutation test is used to determine "BD shift windows".
#' OTU abundances are permuted, and beta-diversity is calculated.
#' The permutations are used to calculate confidence intervals.
#' The possible permutation methods are:
#' \itemize{
#'  \item{"control" = }{
#'  OTU abundances are permuted among all control samples,
#'  and these new samples are used as a null treatment.
#'  Thus, this provides a baseline beta-diversity distribution
#'  that would result from comparing the control fractions to
#'  a randomly shuffled version of themselves.
#'  }
#'  \item{"treatment" = }{
#'  OTU abundances are permuted among all treatment samples.
#'  Thus, a "homogenized" treatment gradient null model.
#'  }
#'  \item{"overlap" = }{
#'  OTU abundances are permuted among overlapping control & treatment fractions.
#'  Thus, is beta-diversity higher than if the overlapping treatment & control
#'  samples were homogenized. This method tends to be too permisive.
#'  }
#'  \item{"adjacent" = }{
#'  The null "treatment" communities are generated by permuting OTU abundances
#'  among adjacent control fractions. Thus, null model is local gradient region
#'  was homogenized.
#'  }
#' }
#'
#'
#' @param physeq  phyloseq object
#' @param method  See phyloseq::distance
#' @param weighted  Weighted Unifrac (if calculating Unifrac)
#' @param fast  Fast calculation method
#' @param normalized  Normalized abundances
#' @param ex  Expression for selecting controls based on metadata
#' @param a  The alpha for calculating confidence intervals
#' @param perm_method  "BD shift window" permutation method. See description.
#' @param nperm  Number of bootstrap permutations
#' @param parallel_perm  Calculate bootstrap permutations in parallel
#' @param parallel_dist  Calculate beta-diveristy distances in parallel
#'
#' @return a data.frame object of weighted mean distances
#'
#' @export
#'
#' @examples
#' data(physeq_S2D2)
#' \dontrun{
#' # Subsetting phyloseq by Substrate and Day
#' params = get_treatment_params(physeq_S2D2, c('Substrate', 'Day'))
#' params = dplyr::filter(params, Substrate!='12C-Con')
#' ex = "(Substrate=='12C-Con' & Day=='${Day}') | (Substrate=='${Substrate}' & Day == '${Day}')"
#' physeq_S2D2_l = phyloseq_subset(physeq_S2D2, params, ex)
#'
#' # Calculating BD_shift on 1 subset (use lapply function to process full list)
#' wmean1 = BD_shift(physeq_S2D2_l[[1]], nperm=5)
#'
#' ggplot(wmean1, aes(BD_min.x, wmean_dist)) +
#'    geom_point()
#'
#' # Calculating BD_shift on all subsets; using just 5 permutations to speed up analysis
#' lapply(physeq_S2D2_l, BD_shift, nperm=5)
#' }
#'
BD_shift = function(physeq, method='unifrac', weighted=TRUE,
                    fast=TRUE, normalized=FALSE, ex="Substrate=='12C-Con'",
                    perm_method=c('control', 'treatment', 'overlap', 'adjacent'),
                    nperm=100, a=0.1, parallel_perm=FALSE, parallel_dist=FALSE){

  # calculating unpermuted & permuted
  df_perm_id = data.frame('perm_id' = 0:nperm)
  df_perm = plyr::mdply(df_perm_id, .BD_shift,
                        physeq=physeq,
                        method=method,
                        weighted=weighted,
                        fast=fast,
                        normalized=normalized,
                        ex=ex,
                        perm_method=perm_method[1],
                        parallel=parallel_dist,
                        .parallel=parallel_perm)
  ## parsing data
  ### actual data
  df_wmean = df_perm %>%
    dplyr::filter_('perm_id == 0') %>%
    dplyr::distinct_('sample.x', .keep_all=TRUE) %>%
    dplyr::select_('-dplyr::ends_with(".y")')
  ### permuted dataset
  df_perm = df_perm %>%
    dplyr::filter_('perm_id > 0')

  # perm CI
  mutate_call1 = lazyeval::interp(~ stats::quantile(wmean_dist, a/2, na.rm=TRUE),
                                  wmean_dist = as.name("wmean_dist"))
  mutate_call2 = lazyeval::interp(~ stats::quantile(wmean_dist, 1-a/2, na.rm=TRUE),
                                  wmean_dist = as.name("wmean_dist"))
  dots = stats::setNames(list(mutate_call1, mutate_call2), c("wmean_dist_CI_low", "wmean_dist_CI_high"))
  ## calculating global CIs
  df_perm_global = df_perm %>%
    dplyr::group_by_() %>%
    dplyr::summarize_(.dots=dots)
  ## calculating CIs for each control fraction
  df_perm = df_perm %>%
    dplyr::group_by_("sample.x", "BD_min.x") %>%
    dplyr::summarize_(.dots=dots)

  # joining
  df_wmean$wmean_dist_CI_low_global = df_perm_global$wmean_dist_CI_low[1]
  df_wmean$wmean_dist_CI_high_global = df_perm_global$wmean_dist_CI_high[1]
  df_wmean = dplyr::left_join(df_wmean, df_perm,
                              c("sample.x", "BD_min.x"))

  # return
  return(df_wmean)
}

