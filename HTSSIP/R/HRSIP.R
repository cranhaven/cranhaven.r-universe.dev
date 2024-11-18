#' Filter l2fc table
#'
#' \code{filter_l2fc} filters a l2fc table to 'best' sparsity cutoffs &
#' bouyant density windows.
#'
#' @param df_l2fc  data.frame of log2 fold change values
#' @param padj_cutoff  Adjusted p-value cutoff for rejecting the null hypothesis
#'   that l2fc values were not greater than the l2fc_threshold.
#' @return filtered df_l2fc object
#'
filter_l2fc = function(df_l2fc, padj_cutoff=0.1){
  padj_cutoff = as.numeric(padj_cutoff)

  # filter to sparsity thresholds with > number of rej_hypo
  ## init dots
  mutate_call = lazyeval::interp(~ sum(padj<padj_cutoff),
                                 padj = as.name('padj'))
  dots = stats::setNames(list(mutate_call), 'n_rej_hypo')
  ## which sparsity cutoff and BD
  df_l2fc_s = df_l2fc %>%
    # number of rej hypo
    dplyr::group_by_("sparsity_threshold") %>%
    dplyr::summarize_(.dots=dots) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_(rank_n_rej_hypo = "dplyr::row_number(-n_rej_hypo)") %>%
    dplyr::filter_("rank_n_rej_hypo == 1")

  ## best sparsity cutoff
  BEST_SPAR_THRESH = as.numeric(df_l2fc_s[1,'sparsity_threshold'])
  cat('Sparsity threshold with the most rejected hypotheses:', BEST_SPAR_THRESH, '\n')
  ### filtering
  mutate_call = lazyeval::interp(~ x==BEST_SPAR_THRESH,
                                 x=as.name('sparsity_threshold'))
  dots = stats::setNames(list(mutate_call), NA)
  df_l2fc = df_l2fc %>%
    dplyr::filter_(.dots=dots)

  # For each sparsity threshold, selecting OTUs with highest l2fc
  ## filtering OTUs to just density window with the highest l2fc
  df_l2fc = df_l2fc %>%
    dplyr::group_by_("OTU", "sparsity_threshold") %>%
    dplyr::filter_("log2FoldChange == max(log2FoldChange)") %>%
    dplyr::ungroup()

  return(df_l2fc)
}



#' (MW-)HR-SIP analysis
#'
#' Conduct (multi-window) high resolution stable isotope probing (HR-SIP) analysis.
#'
#' The (MW-)HR-SIP workflow is as follows:
#'
#'\enumerate{
#'   \item For each sparsity threshold & BD window: calculate log2 fold change values (with DESeq2) for each OTU
#'   \item Globally adjust p-values with a user-defined method (see p.adjust())
#'   \item Select the sparsity cutoff with the most rejected hypotheses
#'   \item For each OTU, select the BD window with the greatest log2 fold change value
#' }
#'
#' @param physeq  Phyloseq object
#' @param design  \code{design} parameter used for DESeq2 analysis.
#'   This is usually used to differentiate labeled-treatment and unlabeld-control samples.
#'   See \code{DESeq2::DESeq} for more details on the option.
#' @param density_windows  The buoyant density window(s) used for for calculating log2
#' fold change values. Input can be a vector (length 2) or a data.frame with a 'density_min'
#' and a 'density_max' column (each row designates a density window).
#' @param sparsity_threshold  All OTUs observed in less than this portion (fraction: 0-1)
#'   of gradient fraction samples are pruned. This is a form of indepedent filtering.
#'   The sparsity cutoff with the most rejected hypotheses is used.
#' @param sparsity_apply  Apply sparsity threshold to all gradient fraction samples ('all')
#'   or just 'heavy' fraction samples ('heavy'), where 'heavy' samples are designated
#'   by the \code{density_windows}.
#' @param l2fc_threshold  log2 fold change (l2fc) values must be significantly above this
#'   threshold in order to reject the hypothesis of equal counts.
#'   See \code{DESeq2} for more information.
#' @param padj_method  Method for global p-value adjustment (See \code{p.adjust()}).
#' @param padj_cutoff  Adjusted p-value cutoff for rejecting the null hypothesis
#'   that l2fc values were not greater than the l2fc_threshold.
#'   Set to \code{NULL} to skip filtering of results to the sparsity cutoff with most
#'   rejected hypotheses and filtering each OTU to the buoyant density window with the
#'   greatest log2 fold change.
#' @param parallel  Process each parameter combination in parallel.
#'   See \code{plyr::mdply()} for more information.
#' @return dataframe of HRSIP results
#'
#' @export
#'
#' @examples
#' data(physeq_S2D2_l)
#'
#' \dontrun{
#' # HR-SIP on just 1 treatment-control comparison
#' ## 1st item in list of phyloseq objects
#' physeq = physeq_S2D2_l[[1]]
#' ## HR-SIP
#' ### Note: treatment-control samples differentiated with 'design=~Substrate'
#' df_l2fc = HRSIP(physeq, design=~Substrate)
#' head(df_l2fc)
#'
#' ## Same, but multiple BD windows (MW-HR-SIP) & run in parallel
#' ### Windows = 1.7-1.73 & 1.72-1.75
#' doParallel::registerDoParallel(2)
#' dw = data.frame(density_min=c(1.7, 1.72), density_max=c(1.73, 1.75))
#' df_l2fc = HRSIP(physeq_S2D1_l[[1]],
#'                 design=~Substrate,
#'                 density_windows=dw,
#'                 parallel=TRUE)
#' head(df_l2fc)
#' }
#'
HRSIP = function(physeq,
                 design,
                 density_windows=data.frame(density_min=c(1.7), density_max=c(1.75)),
                 sparsity_threshold=seq(0, 0.3, 0.1),
                 sparsity_apply='all',
                 l2fc_threshold=0.25,
                 padj_method='BH',
                 padj_cutoff=NULL,
                 parallel=FALSE){

  # assertions
  if(is.factor(density_windows)){
    density_windows = as.vector(density_windows)
  }
  if(is.vector(density_windows)){
    stopifnot(length(density_windows)>=2)
    density_windows = data.frame(start=c(density_windows[1]),
                                 end=c(density_windows[2]))
  }
  stopifnot(all(c('density_min','density_max') %in% colnames(density_windows)))
  stopifnot(is.numeric(l2fc_threshold))
  stopifnot(is.character(sparsity_apply))
  stopifnot(all(sapply(sparsity_threshold, function(x) x>=0 & x<=1))==TRUE)
  stopifnot(is.data.frame(density_windows))
  stopifnot(ncol(density_windows) >= 2)

  # building a matrix of variable parameters
  density_windows$.LEFT_JOIN_COLUMN = 1
  sparsity_threshold = as.data.frame(sparsity_threshold)
  sparsity_threshold$.LEFT_JOIN_COLUMN = 1
  m = dplyr::left_join(density_windows, sparsity_threshold, c('.LEFT_JOIN_COLUMN'))
  m$.LEFT_JOIN_COLUMN = NULL

  # calc l2fc
  df_l2fc = plyr::mdply(m, DESeq2_l2fc,
                        physeq=physeq,
                        design=design,
                        l2fc_threshold=l2fc_threshold,
                        sparsity_apply=sparsity_apply,
                        .parallel=parallel)

  # global p.adjust per sparsity_thresh
  mutate_call = lazyeval::interp(~ stats::p.adjust(x, method=padj_method),
                                 x = as.name('p'))
  dots = stats::setNames(list(mutate_call), 'padj')
  df_l2fc = df_l2fc %>%
    dplyr::group_by_("sparsity_threshold") %>%
    dplyr::mutate_(.dots=dots) %>%
    dplyr::ungroup()

  # filtering l2fc table (if padj_cutoff provided)
  if(!is.null(padj_cutoff)){
    df_l2fc = filter_l2fc(df_l2fc, padj_cutoff=padj_cutoff)
  }

  return(df_l2fc)
}

