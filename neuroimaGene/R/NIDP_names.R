#' List NIDPs
#'
#' Supplies a list of all NIDPs by name for any given modality or atlas
#' @param modality Neuroimaging modality. Defaults to NA; see README for additional options
#' @param atlas Neuroimaging parcellation atlas for NIDP query. Defaults to NA; 
#' see README for additional options
#' @param filename optional filename for writing data to a table
#' @param verbose print runtime messages to R console. Default to FALSE
#' @keywords reference
#' @export
#' @import data.table
#' @importFrom utils write.table
#' @returns a list of NIDP names satisfying the required criteria
#' @examples
#' dk_names <- listNIDPs(modality = 'T1', atlas = 'Desikan')
#'
#'


listNIDPs <- function(modality=NA, atlas=NA, filename = NA, verbose = FALSE) {
  mod <- modality
  atl <- atlas

  #parse input data to determine modality and atlas
    if(!is.na(mod)) {
    if(!(mod %in% unique(anno$modality))) {
      stop(paste(mod, 'modality is not in neuroimaGene repository. see README for all modality types'), call. = F)
    }
    if (!is.na(atl)){
      if(!(atl %in% unique(anno[modality == mod,]$atlas))) {
        stop(paste('The', mod, 'modality does not contain any NIDPs from the', atl,'atlas. see README for description of NIDPs'), call. = F)
      }
      if(verbose){message(paste('using', mod, 'modality and', atl, 'atlas'))}
      nidp_list <- anno[modality == mod & atlas == atl,]$gwas_phenotype
    } else {
      if(verbose){message(paste('using', mod, 'modality only'))}
      nidp_list <- anno[modality == mod,]$gwas_phenotype
    }
  } else if (!is.na(atl)){
    if(!(atl %in% unique(anno$atlas))) {
      stop(paste(atl, ' atlas is not in neuroimaGene repository. See README for all atlas types'), call. = F)
    }
    if(verbose){message(paste('using', atl, 'atlas only'))}
    nidp_list <- anno[atlas == atl,]$gwas_phenotype
  } else {
    if(verbose){message('returning all nidps')}
    nidp_list = anno$gwas_phenotype
  }

  # output .txt file if user requests a file as output.
  if (!is.na(filename)) {
    write.table(nidp_list,
                file = filename,
                append = FALSE,
                quote = FALSE,
                sep = "\t",
                eol = "\n",
                na = '0',
                dec = ".",
                row.names = FALSE,
                col.names = FALSE)
  }

  return(nidp_list)
}

