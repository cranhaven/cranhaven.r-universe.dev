#' Main neuroimaGene query
#'
#' Using a user supplied vector of genes (Ensembl ID's or HUGO names), this 
#' function queries the NeuroimaGene resource for all statistically significant
#' GReX-NIDP associations and returns all findings as a data.table.
#' @param gene_list List of genes to work as inputs. There are no defaults.
#' @param modality Neuroimaging modality for NIDP query. Defaults to T1 structural 
#' NIDPs. Other common options include 'dMRI' for diffusion MRI imaging and 'fMRI'
#' for fucntional MRI image results. 
#' @param atlas Neuroimaging parcellation atlas for NIDP query. Defaults to 
#' Desikan structural atlas. Common T1 atlases include the 'DKT', and 'Destrieux'
#' cortical atlases and 'Subcortex' for freesurfer parcellation of subrotical regions. 
#' See package documentation or vignette for full list. 
#' @param mtc Statistical multiple testing correction for NIDP query. This defaults 
#' to 'BH' for the Benjamini Hochberg False Discovery Rate. Other options include
#' 'BF' for the bonferroni family wise error rate and 'nom' for nominal findings
#' at pvalue <= 0.05. 
#' @param nidps optional user defined vector of target NIDPs to query. Specific 
#' NIDP names can be obtained from the listNIDPnames() function. Use of this
#' parameter overrides the multiple testing correction, returning all nominally 
#' significant findings. 
#' @param filename optional user defined path/filename to which the script will
#' write the nueroimaGene output data table.
#' @param verbose print runtime messages to R console. Default to FALSE
#' @param vignette use for building vignette on installation. Default to FALSE
#' @keywords neuroimaging
#' @export
#' @import data.table DBI stringr RSQLite
#' @importFrom utils write.table
#' @returns a neuroimaGene object: data table with all significant associations between the user provided genes and the UKB NIDPs satisfying the multiple testing correction and atlas/modality/name filters.
#' @examples
#' gene_list <- c('TRIM35', 'PROSER3', 'EXOSC6', 'PICK1', 'UPK1A', 'ESPNL', 'ZIC4')
#' ng <- neuroimaGene(gene_list, atlas = NA, mtc = 'BH', vignette = TRUE)
#'
neuroimaGene <- function(gene_list, modality='T1', atlas='Desikan', mtc='BH', nidps = NA, filename = NA, verbose = FALSE, vignette = FALSE) {
# Use included sample data for building vignette prior to download of neuroimaGene data.
  if (vignette){
    return(ng_vignette)
  }

  # check to see if neuroimaGene database has been downloaded and prompt user to download
  pkg_dir <- system.file(package = "neuroimaGene")
  db_path <- file.path(pkg_dir, "extdata", "neuroimaGenefast.db")
  if (!file.exists(db_path)) {
    check_db()
    }

  if (modality %in% c('all', 'All')){ modality <- NA}
  if (atlas %in% c('all', 'All')){ atlas <- NA}

    if (!identical(nidps, NA)) {
    typ <- 'all'
    if(verbose){message(paste('INFO: querying NeuroimaGene for all neuroimaging derived phenotypes',
          '(NIDPs) from the set of provided NIDPs that are associated',
          'with the provided gene list.'))}
  } else if (is.na(atlas)) {
    if (!is.na(modality)){
      typ <- 'mod'
      if(verbose){message(paste('INFO: querying NeuroimaGene for all neuroimaging derived phenotypes',
                  '(NIDPs) from any atlas of the', modality, 'modality that are associated',
                 'with the provided gene list.'))}
       } else {
      typ <- 'all'
      if(verbose){message(paste('INFO: querying NeuroimaGene for all neuroimaging derived phenotypes',
                  '(NIDPs) from all modalities that are associated with the provided',
                  'gene list.'))}
      }
  } else {
    typ <- 'atl'
    if(verbose){message(paste('INFO: querying NeuroimaGene for all neuroimaging derived phenotypes',
                '(NIDPs) from the', atlas,'atlas that are associated with the provided',
                'gene list.'))}
  }

  # parse user parameters for curated set of NIDPs
  if (!identical(nidps, NA) & mtc != 'nom'){
    if(verbose){message(paste('WARNING: automated p-value threshold correction is not valid with',
          'user-defined NIDP sets. Providing results according to a nominal',
          "p-value threshold. (set mtc = 'nom' to avoid this message)"))}
    mtc <- 'nom'
  } else {
    if(verbose){message(paste('INFO: using the study wide', mtc, 'multiple testing correction.'))}
  }

  # parse user parameters for multiple testing,
  if(mtc == 'nom' | !identical(nidps, NA)) {
    colnm = 'pvalue'
    mtctbl = 'nomID'
  } else {
    colnm = paste0(typ, '_', mtc, 'pval')
    mtctbl = paste0(typ, '_', mtc, 'ID')
  }


  # read in TWAS data
  neuroimaGene_db = system.file("extdata", "NeuroimaGenefast.db", package = "neuroimaGene")
  nimg <- DBI::dbConnect(RSQLite::SQLite(), neuroimaGene_db)

  # Select GReX-NIDP associations using user provided parameters
  if (!identical(nidps, NA)) {
    sqlcmd = paste0("SELECT gene, gene_name, gwas_phenotype, training_model, zscore, ", colnm,
                    " FROM (SELECT * FROM Parent WHERE geneID IN (SELECT geneID FROM Genes",
                    " WHERE gene IN (", paste(shQuote(gene_list, type = "sh"), collapse = ', '),
                    ") OR gene_name IN (", paste(shQuote(gene_list, type = "sh"), collapse = ', '),"))",
                    " AND gwas_phenotypeID IN ( SELECT gwas_phenotypeID FROM Nidp WHERE gwas_phenotype IN (", paste(shQuote(nidps, type = "sh"), collapse = ', '),
                    "))) INNER JOIN ",mtctbl," USING(mainID)",
                    " INNER JOIN Tissue USING(training_modelID)",
                    " INNER JOIN Nidp USING(gwas_phenotypeID)" ,
                    " INNER JOIN Genes USING(geneID);")
  } else if (typ == 'atl') {
    sqlcmd = paste0("SELECT gene, gene_name, gwas_phenotype, training_model, zscore, ", colnm,
                    " FROM (SELECT * FROM Parent WHERE geneID IN (SELECT geneID FROM Genes",
                    " WHERE gene IN (", paste(shQuote(gene_list, type = "sh"), collapse = ', '),
                    ") OR gene_name IN (", paste(shQuote(gene_list, type = "sh"), collapse = ', '),")) ",
                    " AND subsetID IN ( SELECT subsetID FROM Subset WHERE atlas = '",atlas,"'))",
                    " INNER JOIN ",mtctbl," USING(mainID)",
                    " INNER JOIN Tissue USING(training_modelID)",
                    " INNER JOIN Nidp USING(gwas_phenotypeID)" ,
                    " INNER JOIN Genes USING(geneID);")

  } else if (typ == 'mod') {
    sqlcmd = paste0("SELECT gene, gene_name, gwas_phenotype, training_model, zscore, ", colnm,
                    " FROM (SELECT * FROM Parent WHERE geneID IN (SELECT geneID FROM Genes",
                    " WHERE gene IN (", paste(shQuote(gene_list, type = "sh"), collapse = ', '),
                    ") OR gene_name IN (", paste(shQuote(gene_list, type = "sh"), collapse = ', '),"))",
                    " AND subsetID IN ( SELECT subsetID FROM Subset WHERE modality = '",modality,"'))",
                    " INNER JOIN ",mtctbl," USING(mainID)",
                    " INNER JOIN Tissue USING(training_modelID)",
                    " INNER JOIN Nidp USING(gwas_phenotypeID)" ,
                    " INNER JOIN Genes USING(geneID);")
  } else {
    sqlcmd = paste0("SELECT gene, gene_name, gwas_phenotype, training_model, zscore, ", colnm,
                    " FROM (SELECT * FROM Parent WHERE geneID IN (SELECT geneID FROM Genes",
                    " WHERE gene IN (", paste(shQuote(gene_list, type = "sh"), collapse = ', '),
                    ") OR gene_name IN (", paste(shQuote(gene_list, type = "sh"), collapse = ', '),")))",
                    " INNER JOIN ",mtctbl," USING(mainID)",
                    " INNER JOIN Tissue USING(training_modelID)",
                    " INNER JOIN Nidp USING(gwas_phenotypeID)" ,
                    " INNER JOIN Genes USING(geneID);")
  }

  assoc = DBI::dbGetQuery(nimg, sqlcmd)
  setDT(assoc)

  if (!is.na(filename)) {
    write.table(assoc,
                file = filename,
                append = FALSE,
                quote = FALSE,
                sep = "\t",
                eol = "\n",
                na = '0',
                dec = ".",
                row.names = FALSE,
                col.names = TRUE)
  }

  #check for missing genes and report
  gn_nm = unique(gene_list)
  assoc_nm = unique(assoc$gene_name)
  assoc_gn = unique(assoc$gene)
  missed = gn_nm[!(gn_nm %in% assoc_nm | gn_nm %in% assoc_gn)]
  if (length(missed) > 0 & verbose) {
    message(paste('WARNING:', length(missed), 'genes in input list do not correspond to an NIDP'))
    message(paste(missed, collapse = ','))
  }
  dbDisconnect(nimg)
  return(assoc)
}
