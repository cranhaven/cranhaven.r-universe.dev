#'phyloseq-styled sample table
#'
#' Creates a phyloseq-styled sample table from fractions metadata
#' containing data on fraction number, number of replicates, buoyant density
#' calculated from a refractometer, type of isotope, and DNA concentration
#' of each fraction, and isotope type. See below for information on "fractions" file.
#'
#'@param fractions_df fractions data frame
#'A fractions file with the following columns
#' - Replicate: Depends on how many replicates the study has
#' - Fractions: Typically in the range of 2-24
#' - Buoyant_density: As calculated from the refractometer for each fraction and replicate
#' - Isotope: "12C", "13C", "14N", "15N" etc.
#' - DNA_concentration
#' - Sample: In the format "'isotope'_rep_#_fraction_#".
#'   For instance, "12C_rep_1_fraction_1"
#'@import magrittr
#'@importFrom rlang .data
#'@return  data  frame: phyloseq-style sample table
#'@export
#'
#'@examples
#'data(fractions)
#'
#'\donttest{
#'
#' ### Making phyloseq table from fractions metadata
#'samples.object = sample.table(fractions)
#'}

sample.table = function(fractions_df) {
  fractions_df = tibble::as_tibble(fractions_df) %>% #Ensure loaded file is in tibble format
                                    # Create two columns in the tibble.
                                    # One being a logical vector identifying control and heavy isotope
                                    # The other a sample name identifying each fraction and replicate
    dplyr::mutate(IS_CONTROL = stringr::str_detect(tibble::as_tibble(fractions_df)$Isotope, pattern = "12C|14N|16O")) %>% # Please edit this line to add isotope you may be using as control
    tibble::column_to_rownames(var = "Sample") # Make sample names as row names
  fractions_df.ps = phyloseq::sample_data(fractions_df) #Create the phyloseq-styled sample object
  return(fractions_df.ps)
}

#'phyloseq taxa table from GTDB taxonomy input
#'
#'A MAG table, similar to OTU table in phyloseq, will be generated from a
#'concantenated GTDB taxa table for bacteria and archaea
#'
#'@param taxonomy GTDB taxonomy data frame.  A taxonomy file in the GTDB output format. Load the bacteria and archaea taxonomy outputs separately.
#' The markdown requires loading the standard output files from GTDB-Tk separately for bacteria and archaea
#'@return phyloseq-style taxonomy table, but for MAGs
#'@export
#'
#'@examples
#'data(taxonomy_tibble)
#'
#'\donttest{
#'
#'### Making phyloseq table from taxonomy metadata
#'taxonomy.object = tax.table(taxonomy_tibble)
#'}
#'

tax.table = function(taxonomy) {
  `.` <- user_genome <- taxa <- NULL
  classification = dplyr::select(taxonomy, classification) # Get rid of everything except taxonomy
  empties = c("p__", "c__", "o__", "f__", "g__", "s__") #Create a list of prefixes for taxonomic classes to later extract them
  taxonomy_list = rep(NA, nrow(taxonomy)) #Create an empty vector to run a for loop to extract taxonomy
  for(tax in seq_along(taxonomy_list)) {
    taxonomy_list[[tax]] = unlist(strsplit(taxonomy$classification[[tax]], ";")) %>%
      setdiff(., empties) %>%
      utils::tail(., n = 1)
  }
  taxonomy = taxonomy %>% #Using the loaded file for taxonomy, obtain just the Bin and taxonomic classification
                          # Later make the bins as row names to convert the object to a phyloseq-style taxonomy object
    dplyr::mutate(taxa = taxonomy_list) %>%
    dplyr::select(user_genome, taxa) %>%
    tibble::column_to_rownames(var = "user_genome")
  taxonomy = as.matrix(taxonomy) # Ensure the object is a matrix
  tax.table = phyloseq::tax_table(taxonomy)
}

#'Master phyloseq object using the MAG phyloseq objects
#'
#'Creates a phyloseq-style object using processed phyloseq objects for otu
#'table (here, MAG table), taxa table, and sample table
#'
#'@param mag phyloseq-styled MAG table
#'@param taxa phyloseq-styled taxa table
#'@param samples sample information table
#'@return phyloseq object for MAGs
#'@export
#'
#'@examples
#'
#'data(mag.table,taxonomy.object,samples.object,fractions,taxonomy_tibble)
#'###Making phyloseq table from fractions metadata
#'samples.object = sample.table(fractions)
#'taxonomy.object = tax.table(taxonomy_tibble)
#'
#'
#'\donttest{
#'
#' ### Making master phyloseq table from scaled MAG data, taxa and fractions phyloseq data
#'phylo.qSIP = phylo.table(mag.table,taxonomy.object,samples.object)
#'}
#'
phylo.table = function(mag,taxa,samples) {
  phyloseq::phyloseq(mag, taxa, samples)
}

#' Calculate Mheavymax
#'
#' This script was adapted from https://github.com/buckleylab/HTSSIP/blob/master/R/qSIP_atom_excess.R
#' for use with genome-centric metagenomics. See Hungate et al., 2015 for more details
#'
#' @param Mlight  The molecular wight of unlabeled DNA
#' @param isotope  The isotope for which the DNA is labeled with ('13C' or '18O')
#' @param Gi  The G+C content of unlabeled DNA
#' @return numeric value: maximum molecular weight of fully-labeled DNA
#'
calc_Mheavymax_MAGs = function(Mlight, isotope='13C', Gi=Gi){
  isotope = toupper(isotope)
  if(isotope=='13C'){
    Mhm = -0.4987282 * Gi + 9.974564 + Mlight
  } else
    if(isotope=='18O'){
      Mhm = 12.07747 + Mlight
    } else {
      stop('isotope not recognized')
    }
  return(Mhm)
}

#' Calculate atom fraction excess
#'
#' See Hungate et al., 2015 for more details
#'
#' @param Mlab  The molecular wight of labeled DNA
#' @param Mlight  The molecular wight of unlabeled DNA
#' @param Mheavymax  The theoretical maximum molecular weight of fully-labeled DNA
#' @param isotope  The isotope for which the DNA is labeled with ('13C' or '18O')
#' @return numeric value: atom fraction excess (A)
#'
calc_atom_excess_MAGs = function(Mlab, Mlight, Mheavymax, isotope='13C'){
  isotope=toupper(isotope)
  if(isotope=='13C'){
    x = 0.01111233
  } else
    if(isotope=='18O'){
      x = 0.002000429
    } else {
      stop('isotope not recognized')
    }
  A = (Mlab - Mlight) / (Mheavymax - Mlight) * (1 - x)
  return(A)
}
#' Reformat a phyloseq object of qSIP_atom_excess_MAGs analysis
#'
#' @param physeq  A phyloseq object
#' @param control_expr  An expression for identifying unlabeled control
#' samples in the phyloseq object (eg., "Substrate=='12C-Con'")
#' @param  treatment_rep  Which column in the phyloseq sample data designates
#' replicate treatments
#' @return numeric value: atom fraction excess (A)

qSIP_atom_excess_format_MAGs = function(physeq, control_expr, treatment_rep){
  # formatting input
  cols = c('IS_CONTROL', 'Buoyant_density', treatment_rep)
  df_OTU = HTSSIP::phyloseq2table(physeq,
                          include_sample_data=TRUE,
                          sample_col_keep=cols,
                          control_expr=control_expr)
  # removing 'infinite' BD values
  tmp = colnames(df_OTU)
  df_OTU = df_OTU %>%
    dplyr::mutate_(Buoyant_density = "as.numeric(as.character(Buoyant_density))",
                   Count = "as.numeric(as.character(Count))") %>%
    dplyr::filter_('! is.infinite(Buoyant_density)') %>%
    dplyr::filter_('! is.na(Buoyant_density)') %>%
    as.data.frame
  colnames(df_OTU) = tmp
  # return
  return(df_OTU)
}

#' Calculate atom fraction excess using q-SIP method
#'
#' @param physeq  A phyloseq object
#' @param control_expr  Expression used to identify control samples based on sample_data.
#' @param treatment_rep  Which column in the phyloseq sample data designates
#' replicate treatments
#' @param isotope  The isotope for which the DNA is labeled with ('13C' or '18O')
#' @param df_OTU_W  Keep NULL
#' @param Gi GC content of the MAG
#' @return A list of 2 data.frame objects. 'W' contains the weighted mean buoyant density (W) values for each OTU in each treatment/control. 'A' contains the atom fraction excess values for each OTU. For the 'A' table, the 'Z' column is buoyant density shift, and the 'A' column is atom fraction excess.
#' @export
#'
#' @examples
#' data(phylo.qSIP,GC_content)
#' ### Making atomx table
#' \donttest{
#' ## Not run::
#' ### BD shift (Z) & atom excess (A)
#' atomX = qSIP_atom_excess_MAGs(phylo.qSIP,
#'                          control_expr='Isotope=="12C"',
#'                          treatment_rep='Replicate',
#'                          Gi = GC_content)
#'}

qSIP_atom_excess_MAGs = function(physeq,
                                 control_expr,
                                 treatment_rep=NULL,
                                 isotope='13C',
                                 df_OTU_W=NULL,
                                 Gi){
  # formatting input
  if(is.null(df_OTU_W)){
    no_boot = TRUE
  } else {
    no_boot = FALSE
  }
  if(no_boot){
    df_OTU = qSIP_atom_excess_format_MAGs(physeq, control_expr, treatment_rep)
    if(nrow(df_OTU) == 0){
      stop('No rows in OTU table after qSIP_atom_excess_format_MAGs(). Check control_exp & treatment_rep')
    }
    # BD shift (Z)
    df_OTU_W = df_OTU %>%
      # weighted mean buoyant density (W)
      dplyr::mutate_(Buoyant_density = "as.numeric(as.character(Buoyant_density))",
                     Count = "as.numeric(as.character(Count))") %>%
      dplyr::group_by_('IS_CONTROL', 'OTU', treatment_rep) %>%
      dplyr::summarize_(W = "stats::weighted.mean(Buoyant_density, Count, na.rm=TRUE)") %>%
      dplyr::ungroup()
  }
  df_OTU_s = df_OTU_W %>%
    # mean W of replicate gradients
    dplyr::group_by_('IS_CONTROL', 'OTU') %>%
    dplyr::summarize_(Wm = "mean(W, na.rm=TRUE)") %>%
    # BD shift (Z)
    dplyr::group_by_('OTU') %>%
    dplyr::mutate_(IS_CONTROL = "ifelse(IS_CONTROL==TRUE, 'Wlight', 'Wlab')") %>%
    tidyr::spread("IS_CONTROL", "Wm") %>%
    dplyr::mutate_(Z = "Wlab - Wlight") %>%
    dplyr::ungroup()
  # atom excess (A)
  ## pt1
  df_OTU_s = df_OTU_s %>%
    dplyr::left_join(Gi, by="OTU") %>%
    dplyr::mutate_(Mlight = "0.496 * Gi + 307.691")
  ## pt2
  MoreArgs = list(isotope=isotope)
  dots = list(~mapply(calc_Mheavymax_MAGs, Mlight=Mlight, Gi=Gi, MoreArgs=MoreArgs))
  dots = stats::setNames(dots, "Mheavymax")
  df_OTU_s = df_OTU_s %>%
    dplyr::mutate_(.dots=dots)
  ## pt3
  dots = list(~mapply(calc_atom_excess_MAGs, Mlab=Mlab, Mlight=Mlight,
                      Mheavymax=Mheavymax, MoreArgs=MoreArgs))
  dots = stats::setNames(dots, "A")
  df_OTU_s = df_OTU_s %>%
    dplyr::mutate_(Mlab = "(Z / Wlight + 1) * Mlight") %>%
    dplyr::mutate_(.dots=dots)
  ## flow control: bootstrap
  if(no_boot){
    return(list(W=df_OTU_W, A=df_OTU_s))
  } else {
    return(df_OTU_s)
  }
}

# sampling with replacement from control & treatment for each MAG
sample_W = function(df, n_sample){
  n_light = n_sample[1]
  n_lab = n_sample[2]
  # parsing df
  df_light = df[df$IS_CONTROL==TRUE,]
  df_lab = df[df$IS_CONTROL==FALSE,]
  # sampling
  if(length(df_light$W) > 1){
    W_light = base::sample(df_light$W, n_light, replace=TRUE)
  } else {
    W_light = rep(df_light$W, n_light)
  }
  if(length(df_lab$W) > 1){
    W_lab = base::sample(df_lab$W, n_lab, replace=TRUE)
  } else {
    W_lab = rep(df_lab$W, n_lab)
  }
  # creating new data.frames
  df_light = data.frame(IS_CONTROL=TRUE,
                        #OTU=rep(otu, n_light),
                        W = W_light)
  df_lab = data.frame(IS_CONTROL=FALSE,
                      #OTU=rep(otu, n_lab),
                      W = W_lab)
  return(rbind(df_light, df_lab))
}
# shuffling weighted mean densities (W)
.qSIP_bootstrap = function(atomX,
                           isotope='13C',
                           n_sample=c(3,3),
                           bootstrap_id = 1){
  data = NULL
  temp = NULL

  # making a new (subsampled with replacement) dataset
  n_sample = c(3,3)  # control, treatment
  dots = stats::setNames(list(~lapply(data, sample_W, n_sample=n_sample)), "ret")
  df_OTU_W = atomX$W %>%
    dplyr::group_by_("OTU") %>%
    tidyr::nest() %>%
    dplyr::mutate(temp = purrr::map(data, ~sample_W(.x, n_sample))) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(cols = c(temp))

  # calculating atom excess
  atomX = SIPmg::qSIP_atom_excess_MAGs(physeq=NULL,
                                        df_OTU_W=df_OTU_W,
                                        control_expr=NULL,
                                        treatment_rep=NULL,
                                        isotope=isotope, Gi = GC_content)
  atomX$bootstrap_id = bootstrap_id
  return(atomX)
}

#' Isotope incorporator list with GTDB taxonomy
#'
#' This function provides a table with MAGs and their corresponding GTDB taxonomy
#' as an output. This would be useful in identifying the taxa that have incorporation
#'
#' @param taxonomy A taxonomy tibble obtained in the markdown. This taxonomy tibble is
#' typically a concatenated list of archaeal and bacterial taxonomy from GTDB-Tk
#' Please check GTDB-Tk documentation for running the tool
#' @param bootstrapped_AFE_table A data frame indicating bootstrapped atom fraction excess values
#' @return A tibble with two columns, OTU and Taxonomy, with taxonomy of the incorporator MAGs
#' @export
#'
#'@examples
#'data(taxonomy_tibble,df_atomX_boot)
#'
#'\donttest{
#'
#' ### Making incorporator taxonomy list
#'incorporator_list = incorporators_taxonomy(taxonomy = taxonomy_tibble,
#'          bootstrapped_AFE_table = df_atomX_boot)
#'}
#'

incorporators_taxonomy = function(taxonomy, bootstrapped_AFE_table) {
  taxonomy_classification = taxonomy$classification
  empties <- c("p__", "c__", "o__", "f__", "g__", "s__")
  taxonomy_list <- rep(NA, length(taxonomy_classification))
  i = 1
  for(tax in taxonomy_classification) {
    spl <- strsplit(as.character(tax), ";")
    spl <- setdiff(spl[[1]], empties)
    cl <- utils::tail(spl, n=1)
    taxonomy_list[i] <- cl
    i = i + 1
  }
  bootstrapped_AFE_table = dplyr::mutate(bootstrapped_AFE_table, Taxonomy = taxonomy_list)
  incorporator_list = bootstrapped_AFE_table %>%
    dplyr::filter(.data$Incorporator == TRUE) %>%
    dplyr::select(.data$OTU, .data$Taxonomy)
  prefixes = c("p__|c__|o__|f__|g__|s__")
  incorporator_list = dplyr::mutate(incorporator_list, Taxonomy = stringr::str_remove(incorporator_list[[2]], prefixes)) %>%
    dplyr::rename("taxonomy" = "Taxonomy")
  return(incorporator_list)
}

#' Remove MAGs with NAs from atomX table
#'
#' This function enables removing NAs from the atomX table.
#'
#' @param atomX A list object created by \code{qSIP_atom_excess_MAGs()}
#' @importFrom dplyr if_else
#' @return A list of 2 data.frame objects without MAGs which have NAs. 'W' contains the weighted mean buoyant density (W) values for each OTU in each treatment/control. 'A' contains the atom fraction excess values for each OTU. For the 'A' table, the 'Z' column is buoyant density shift, and the 'A' column is atom fraction excess.
#' @export
#'
#' @examples
#'data(atomX)
#'
#'\donttest{
#'
#'### Remove NAs in atomX table
#'atomx_no_na = filter_na(atomX)
#'}
#'



filter_na = function(atomX) {
  W <- OTU <- OTU_na_list <- NULL

  a_w = atomX$W
  a_a = atomX$A

  a_w_1 = a_w %>% dplyr::mutate(OTU_na_list = if_else(is.nan(W), OTU, "A"))
  na_otu = unique(a_w_1 %>% dplyr::select(OTU_na_list) %>% dplyr::filter(stringr::str_detect(OTU_na_list, "A", negate = T)))

  a_w = a_w_1 %>% dplyr::filter(!OTU %in% na_otu$OTU_na_list) %>% dplyr::select(-OTU_na_list)
  a_a = a_a %>% dplyr::filter(!OTU %in% na_otu$OTU_na_list)

  a_list = list(W = a_w, A = a_a)
  return(a_list)
}


#' Calculate adjusted bootstrap CI after for multiple testing for atom fraction excess using q-SIP method. Multiple hypothesis tests are corrected by
#
#' @param atomX  A list object created by \code{qSIP_atom_excess_MAGs()}
#' @param isotope  The isotope for which the DNA is labeled with ('13C' or '18O')
#' @param n_sample  A vector of length 2. The sample size for data resampling (with replacement) for 1) control samples and 2) treatment samples.
#' @param n_boot  Number of bootstrap replicates.
#' @param a  A numeric value. The alpha for calculating confidence intervals.
#' @param parallel  Parallel processing. See \code{.parallel} option in \code{dplyr::mdply()} for more details.
#' @param ci_adjust_method Confidence interval adjustment method. Please choose 'FCR', 'Bonferroni', or 'none' (if no adjustment is needed). Default is FCR and also provides unadjusted CI.
#' @importFrom dplyr contains
#' @return A data.frame of atom fraction excess values (A) and atom fraction excess confidence intervals adjusted for multiple testing.
#' @export
#'
#' @examples
#' data(phylo.qSIP,GC_content)
#' \donttest{
#' ### BD shift (Z) & atom excess (A)
#' atomX = qSIP_atom_excess_MAGs(phylo.qSIP,
#'                         control_expr='Isotope=="12C"',
#'                         treatment_rep='Replicate', Gi = GC_content)
#'
#' ### Add doParallel::registerDoParallel(num_cores) if parallel bootstrapping is to be done
#' df_atomX_boot = qSIP_bootstrap_fcr(atomX, n_boot=5, parallel = FALSE)
#'}
#'

qSIP_bootstrap_fcr = function(atomX, isotope='13C', n_sample=c(3,3), ci_adjust_method ='fcr',
                                        n_boot=10, parallel=FALSE, a=0.1){
  A_CI_low <- Mlight <- Mlab <- Mheavymax <- A <- Z <- delbd_sd <- NULL
  # atom excess for each bootstrap replicate
  if (isotope == "13C" || isotope == "18O") {
  num_tests = nrow(atomX$A)
  a_bonferroni = a/num_tests
  df_boot_id = data.frame(bootstrap_id = 1:n_boot)
  df_boot = plyr::mdply(df_boot_id, .qSIP_bootstrap,
                        atomX = atomX,
                        isotope=isotope,
                        n_sample=n_sample,
                        .parallel=parallel)

  # calculating atomX CIs for each OTU
  mutate_call1 = lazyeval::interp(~ stats::quantile(A, a/2, na.rm=TRUE),
                                  A = as.name("A"))
  mutate_call2 = lazyeval::interp(~ stats::quantile(A, 1-a/2, na.rm=TRUE),
                                  A = as.name("A"))
  mutate_call3 = lazyeval::interp(~ stats::quantile(A, a_bonferroni/2, na.rm=TRUE),
                                  A = as.name("A"))
  mutate_call4 = lazyeval::interp(~ stats::quantile(A, 1-a_bonferroni/2, na.rm=TRUE),
                                  A = as.name("A"))
  mutate_call5 = lazyeval::interp(~ stats::sd(A),
                                  A = as.name("A"))
  #mutate_call6 = lazyeval::interp(~ stats::t.test(A, mu = 0, alternative = "greater")$p.value,
  #                                A = as.name("A"))
  mutate_call7 = lazyeval::interp(~ stats::sd(Z),
                                  Z = as.name("Z"))
  mutate_call8 = lazyeval::interp(~ mean(A),
                                  A = as.name("A"))

  dots = stats::setNames(list(mutate_call1, mutate_call2),
                         c("A_CI_low", "A_CI_high"))
  df_boot_1 = df_boot %>%
    dplyr::group_by_("OTU") %>%
    dplyr::summarize_(.dots=dots)

  discoveries = df_boot_1 %>%
    dplyr::filter(A_CI_low > 0) %>%
    nrow()

  a_fcr = a*discoveries/num_tests
  mutate_call9 = lazyeval::interp(~ stats::quantile(A, a_fcr/2, na.rm=TRUE),
                                  A = as.name("A"))
  mutate_call10 = lazyeval::interp(~ stats::quantile(A, 1-a_fcr/2, na.rm=TRUE),
                                   A = as.name("A"))

  dots = stats::setNames(list(mutate_call1, mutate_call2, mutate_call3, mutate_call4, mutate_call9, mutate_call10,
                              mutate_call5, mutate_call7, mutate_call8),
                         c("A_CI_low", "A_CI_high","A_CI_bonferroni_low", "A_CI_bonferroni_high","A_CI_fcr_low", "A_CI_fcr_high",
                           "A_sd", "delbd_sd", "A_mean"))
  df_boot = df_boot %>%
    dplyr::group_by_("OTU") %>%
    dplyr::summarize_(.dots=dots)

  #df_boot = df_boot %>%
  #  mutate(adj_p = stats::p.adjust(A_p_values, method = "BH"))

  # combining with atomX summary data
  df_boot = dplyr::inner_join(atomX$A, df_boot, c('OTU'='OTU'))
  } else if (isotope == "15N") {
    df_boot = atomX$A
    df_boot = df_boot %>%
      dplyr::select(-c(Mlight,Mlab,Mheavymax,A))
    message("qSIP model does not exist for 15N - only delta BD based AFE estimates are reported")
      } else {
    stop("You have used an isotope that does not have a qSIP model or perhaps the notation you used was wrong? In that case please check documentation on how to specify the isotope")
  }
  #df_boot = df_boot %>%
  #  mutate(significance = (adj_p < a/2))
  if (isotope == "13C") {
    df_boot = df_boot %>%
      dplyr::mutate(A_delbd = Z/0.036,
             A_delbd_sd = delbd_sd/0.036)
  } else if (isotope == "15N") {
    df_boot = df_boot %>%
      dplyr::mutate(A_delbd = Z/0.016)
  } else {
    df_boot = df_boot %>%
      dplyr::select(-delbd_sd)
    message("Isotope is not 13C or 15N so delta BD based AFE is not reported")
  }

  if (ci_adjust_method == "fcr") {
    df_boot = df_boot %>%
      dplyr::select(-contains("bonferroni"))
  } else if (ci_adjust_method == "bonferroni") {
    df_boot = df_boot %>%
      dplyr::select(-contains("fcr"))
  } else if (ci_adjust_method == "none") {
    df_boot = df_boot %>%
      dplyr::select(-contains(c("fcr", "bonferroni")))
  } else {
    stop("This package does not use the chosen multiple testing method. Please edit in the source code and upload if necessary, thanks!")
  }
  return(df_boot)
}
