# binary qualitative test (present in 1 but not the other)
.binary_test = function(x, y, alternative){
  p = NA
  in_x = sum(x > 0) > 0
  in_y = sum(y > 0) > 0
  # in x but non y
  if(alternative == 'greater'){
    stat = ifelse((in_x == TRUE & in_y != TRUE), 1, 0)
  } else
  if(alternative == 'less'){
    stat = ifelse((in_y == TRUE & in_x != TRUE), 1, 0)
  } else {
    stop('Alternative option not supported for "binary"')
  }
  return(c(stat, p))
}

# t.test wrapper
.t_test = function(x, y, alternative){
  z = stats::t.test(x, y, alternative=alternative)
  stat = as.numeric(z$statistic)
  p = as.numeric(z$p.value)
  return(c(stat, p))
}

# wilcox.test wrapper
.wilcox_test = function(x, y, alternative){
  z = stats::wilcox.test(x, y, alternative=alternative)
  stat = as.numeric(z$statistic)
  p = as.numeric(z$p.value)
  return(c(stat, p))
}

#' Heavy-SIP analysis
#'
#' Compare taxon abundances in 'heavy' fractions versus specific controls.
#'
#' 'Heavy-SIP' encompasses the analyses often used in SIP studies prior
#' to new HTS-SIP methodologies. These methods all consisted of identifying
#' 'heavy' gradient fractions. This was often done by comparing
#' the distribution of DNA conc. or gene copies across gradient fractions
#' in labeled treatments versus unlabeled controls. Sometimes, the unlabeled
#' control was left out, and "heavy" gradients were identified based on
#' comparisons with theoretic distributions of unlabeled DNA.
#'
#' Although hypothesis testing was often used to assess increased
#' taxon abundances in "heavy" gradients of labled treatments (eg., one-tailed
#' t-tests), the hypothesis testing usually did not account for the compositional
#' nature of sequence data (relative abundances).
#'
#' Here, "heavy-SIP" can define incorporators as either:
#' \itemize{
#'  \item{"H" =}{
#'  Any taxa IN the "heavy" fractions of the labeled treatment gradients
#'  }
#'  \item{"H-v-L" =}{
#'  Any taxa IN the "heavy" fractions of the labeled treatment and NOT
#'  present in the "heavy" fractions of the control
#'  }
#'  \item{"H-v-H" =}{
#'  Any taxa IN the "heavy" fractions of the labeled treatment and NOT
#'  present in the "light" fractions of the labeled treatment
#'  }
#'}
#' Instead of binary comparisions (presence/absence),
#' one-tailed t-tests or Wilcoxon Rank Sum tests can be used to assess
#' differential abundance between "heavy" and controls. The hypothesis
#' testing methods require multiple replicate controls, will use the mean
#' taxon abundance in the "heavy" (and "light") window.
#'
#' @param physeq  A phyloseq object of just treatment vs control.
#' If you have a more complicated experimental design, subset the
#' phyloseq object into a list of treatment vs control comparisions.
#' @param ex  Expression for selecting controls based on metadata
#' @param rep  Column specifying gradient replicates. If the column
#' does not exiset, then the column will be created, and all will be considered
#' "replicate=1"
#' @param light_window  A vector designating the "light" BD window start and stop
#' @param heavy_window  A vector designating the "heavy" BD window start and stop
#' @param comparison  Which light/heavy BD windows to compare (see the description)?
#' @param hypo_test  Which hypothesis test to run on each OTU?
#' Note that "binary" isn't really a hypothesis test, but just qualitative.
#' @param alternative  The "alternative" option for the hypothesis test functions.
#' Note that "two.sided" doesn't work for the "binary" test.
#' @param sparsity_threshold  All OTUs observed in less than this portion (fraction: 0-1)
#'   of gradient fraction samples are pruned. A a form of indepedent filtering,
#'   The sparsity cutoff with the most rejected hypotheses is used.
#' @param sparsity_apply  Apply sparsity threshold to all gradient fraction samples ('all')
#'   or just heavy fraction samples ('heavy')
#' @param padj_method  Multiple hypothesis correction method. See `p.adjust()` for more
#' details.
#' @return a data.frame object of hypothesis test results
#'
#' @export
#'
#' @examples
#' data(physeq_S2D2)
#' data(physeq_rep3)
#' \dontrun{
#' # Calculating 'binary' for unreplicated experiment
#' ## Subsetting phyloseq by Substrate and Day
#' params = get_treatment_params(physeq_S2D2, c('Substrate', 'Day'))
#' params = dplyr::filter(params, Substrate!='12C-Con')
#' ex = "(Substrate=='12C-Con' & Day=='${Day}') | (Substrate=='${Substrate}' & Day == '${Day}')"
#' physeq_S2D2_l = phyloseq_subset(physeq_S2D2, params, ex)
#'
#' ## Calculating heavy-SIP on 1 subset (use lapply function to process full list)
#' incorps = heavy_SIP(physeq_S2D2_l[[1]])
#'
#' # Calculating wilcox test on replicated design
#' ## (comparing heavy-treatment versus heavy-control)
#' incorps = heavy_SIP(physeq_rep3, ex="Treatment=='12C-Con'", comparison='H-v-H', hypo_test='wilcox')
#' }
heavy_SIP = function(physeq,
                     ex="Substrate=='12C-Con'",
                     rep='Replicate',
                     light_window=c(1.68, 1.70),
                     heavy_window=c(1.73, 1.75),
                     comparison=c('H', 'H-v-L', 'H-v-H'),
                     hypo_test=c('binary', 't-test', 'wilcox'),
                     alternative = c('greater', 'two.sided', 'less'),
                     sparsity_threshold=0.1,
                     sparsity_apply=c('all', 'heavy'),
                     padj_method='BH'){

  # assertions
  stopifnot(length(light_window) >= 2)
  stopifnot(length(heavy_window) >= 2)
  comparison = comparison[1]
  hypo_test = hypo_test[1]

  ## selecting hypothesis test
  ### how to aggregate abundances within each replicate?
  agg_func = switch(hypo_test,
                        'binary' = function(x) sum(x > 0) > 0,
                        't-test' = mean,
                        'wilcox' = mean,
                        stop('Hypothesis test not recognized'))
  ### hypothesis test function
  hypo_func = switch(hypo_test,
                  'binary' = .binary_test,
                  't-test' = .t_test,
                  'wilcox' = .wilcox_test,
                  stop('Hypothesis test not recognized'))


  # sparsity cutoff applied to all gradient fractions
  prn = function(x) sum(x > 0) > sparsity_threshold * length(x)
  if(sparsity_apply[1] == 'all'){
    physeq = phyloseq::filter_taxa(physeq, prn, TRUE)
  }
  # removing 0-abundance taxa
  physeq = phyloseq::filter_taxa(physeq, function(x) sum(x > 0) > 0 * length(x), TRUE)

  # sparsity cutoff applied to just heavy fractions
  if(sparsity_apply[1] == 'heavy'){
    physeq = phyloseq::filter_taxa(physeq, prn, TRUE)
  }

  # formatting phyloseq & metadata
  physeq = physeq_format(physeq)
  metadata = format_metadata(physeq, ex, rep=rep)

  # parsing treatment & control
  physeq_control = phyloseq::prune_samples(metadata$IS__CONTROL==TRUE, physeq)
  physeq_treat = phyloseq::prune_samples(metadata$IS__CONTROL==FALSE, physeq)

  # window selection
  ## applying 'heavy' window pruning
  metadata_control = metadata[metadata$IS__CONTROL == TRUE,]
  metadata_treat = metadata[metadata$IS__CONTROL == FALSE,]
  physeq_treat_heavy = phyloseq::prune_samples((metadata_treat$BD_min >= heavy_window[1]) &
                                               (metadata_treat$BD_min <= heavy_window[2]),
                                               physeq_treat)

  # comparing 'heavy' (versus control)
  ## output format: OTU, statistic, p, padj
  if(comparison == 'H'){
    # just binary possible
    stopifnot(hypo_test == 'binary')
    # converting to binary
    res = phyloseq::otu_table(physeq_treat_heavy) %>%
      as.matrix %>%
      apply(1, agg_func) %>%
      sapply(function(x) as.numeric(x > 0)) %>%
      as.data.frame()
    # applying statistic
    colnames(res)[1] = 'statistic'
    res$p = NA
    res$padj = NA
    res$OTU = rownames(res)
    res = res[,c('OTU', 'statistic', 'p', 'padj')]
    return(res)
  }

  if(comparison == 'H-v-L'){
    # comparing heavy vs light from same gradient (no 'control' gradient)
    ## applying 'light' window pruning
    physeq_treat_light = phyloseq::prune_samples((metadata_treat$BD_min >= light_window[1]) &
                                                 (metadata_treat$BD_min <= light_window[2]),
                                                 physeq_treat)

    # aggregating all fractions in same replicates
    ## heavy fractions, per replicate
    metadata_treat_heavy = metadata[metadata$METADATA_ROWNAMES %in%
                                    phyloseq::sample_names(physeq_treat_heavy),]
    Replicate = as.data.frame(metadata_treat_heavy)[,rep]
    df_h = phyloseq::otu_table(physeq_treat_heavy) %>%
      as.matrix %>%
      as.data.frame %>% t
    df_h = stats::aggregate(df_h,
                            list(Replicate = Replicate),
                            agg_func)
    ## light fractions, per replicate
    metadata_treat_light = metadata[metadata$METADATA_ROWNAMES %in%
                                    phyloseq::sample_names(physeq_treat_light),]
    Replicate = as.data.frame(metadata_treat_light)[,rep]
    df_l = phyloseq::otu_table(physeq_treat_light) %>%
      as.matrix %>%
      as.data.frame %>% t
    df_l = stats::aggregate(df_l,
                            list(Replicate = Replicate),
                            agg_func)

    # applying statistical test
    otus = colnames(df_h)[2:ncol(df_h)]
    res = sapply(otus, function(x) hypo_func(df_h[,x], df_l[,x],
                                             alternative=alternative[1]))
    res = res %>% t %>% as.data.frame
    colnames(res) = c('statistic', 'p')
    res$padj = stats::p.adjust(res$p, padj_method)
    return(res)
  } else
  if(comparison == 'H-v-H'){
    # comparing heavy vs light from same gradient (no 'control' gradient)
    ## applying 'heavy' window pruning on control
    physeq_control_heavy = phyloseq::prune_samples((metadata_control$BD_min >= heavy_window[1]) &
                                                  (metadata_control$BD_min <= heavy_window[2]),
                                                  physeq_control)

    # aggregating all fractions in same replicates
    ## heavy treatment fractions, per replicate
    metadata_treat_heavy = metadata[metadata$METADATA_ROWNAMES %in%
                                      phyloseq::sample_names(physeq_treat_heavy),]
    Replicate = as.data.frame(metadata_treat_heavy)[,rep]
    df_ht = phyloseq::otu_table(physeq_treat_heavy) %>%
      as.matrix %>%
      as.data.frame %>% t
    df_ht = stats::aggregate(df_ht,
                             list(Replicate = Replicate),
                             agg_func)
    ## heavy control fractions, per replicate
    metadata_control_heavy = metadata[metadata$METADATA_ROWNAMES %in%
                                      phyloseq::sample_names(physeq_control_heavy),]
    Replicate = as.data.frame(metadata_control_heavy)[,rep]
    df_hc = phyloseq::otu_table(physeq_control_heavy) %>%
      as.matrix %>%
      as.data.frame %>% t
    df_hc = stats::aggregate(df_hc,
                             list(Replicate = Replicate),
                             agg_func)

    # applying statistical test
    otus = colnames(df_ht)[2:ncol(df_ht)]
    res = sapply(otus, function(x) hypo_func(df_ht[,x], df_hc[,x],
                                             alternative=alternative[1]))
    res = res %>% t %>% as.data.frame
    colnames(res) = c('statistic', 'p')
    res$padj = stats::p.adjust(res$p, method=padj_method)
    return(res)
  } else {
    stop('comparison not recognized')
  }
}

# unreplicated design
#data(physeq_S2D2)
# Subsetting phyloseq by Substrate and Day
#params = get_treatment_params(physeq_rep3, c('Substrate', 'Day'))
#params = dplyr::filter(params, Substrate!='12C-Con')
#ex = "(Substrate=='12C-Con' & Day=='${Day}') | (Substrate=='${Substrate}' & Day == '${Day}')"
#physeq_S2D2_l = phyloseq_subset(physeq_S2D2, params, ex)

# Calculating heavy-SIP on 1 subset (use lapply function to process full list)
#res = heavy_SIP(physeq_S2D2_l[[1]], ex="Substrate=='12C-Con'", comparison='H-v-L')
#res %>% head

# replicated design
#data(physeq_rep3)
# Calculating heavy-SIP on 1 subset (use lapply function to process full list)
# res = heavy_SIP(physeq_rep3, ex="Treatment=='12C-Con'", comparison='H-v-L')
#res = heavy_SIP(physeq_rep3, ex="Treatment=='12C-Con'", rep='Replicate', comparison='H')
#res = heavy_SIP(physeq_rep3, ex="Treatment=='12C-Con'",
#                rep='Replicate', comparison='H-v-H',
#                hypo_test='t-test')
#res %>% head

