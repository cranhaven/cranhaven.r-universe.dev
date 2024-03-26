# RAbHIT functions -----------------------------------------------------

#' @include rabhit.R
#' @include internal_functions.R
NULL

##########################################################################
#' Anchor gene haplotype inference
#'
#' The \code{createFullHaplotype} functions infers haplotype based on an anchor gene.
#'
#'
#' @param    clip_db               a \code{data.frame} in AIRR format. See details.
#' @param    toHap_col             a vector of column names for which a haplotype should be inferred. Default is v_call and d_call
#' @param    hapBy_col             column name of the anchor gene. Default is j_call
#' @param    hapBy                 a string of the anchor gene name. Default is IGHJ6.
#' @param    toHap_GERM            a vector of named nucleotide germline sequences matching the allele calls in \code{toHap_col} columns in clip_db.
#' @param    relative_freq_priors  if TRUE, the priors for Bayesian inference are estimated from the relative frequencies in clip_db. Else, priors are set to \code{c(0.5,0.5)}. Default is TRUE
#' @param    kThreshDel            the minimum lK (log10 of the Bayes factor) to call a deletion. Default is 3.
#' @param    rmPseudo              if TRUE non-functional and pseudo genes are removed. Default is TRUE.
#' @param    deleted_genes         double chromosome deletion summary table. A \code{data.frame} created by \code{deletionsByBinom}.
#' @param    nonReliable_Vgenes    a list of known non reliable gene assignments. A \code{list} created by \code{nonReliableVGenes}.
#' @param    min_minor_fraction    the minimum minor allele fraction to be used as an anchor gene. Default is 0.3
#' @param    single_gene           if to only consider genes from single assignment. If true then calls where genes appear with others are discarded. If false then the calls are seperated an counted for all genes that appeared. Default is True.
#' @param    chain                 the IG/TR chain: IGH,IGK,IGL,TRB. Default is IGH.
#'
#' @return
#' A \code{data.frame}, in which each row is the haplotype inference summary of a gene from the column selected in \code{toHap_col}.
#'
#'The output containes the following columns:
#' \itemize{
#'  \item \code{subject}:        the subject name.
#'  \item \code{gene}:           the gene name.
#'  \item Anchor gene allele 1:  the haplotype inference for chromosome one. The column name is the anchor gene with the first allele.
#'  \item Anchor gene allele 2:  the haplotype inference for chromosome two. The column name is the anchor gene with the second allele.
#'  \item \code{alleles}:        allele calls for the gene.
#'  \item \code{proirs_row}:     priors based on relative allele usage of the anchor gene.
#'  \item \code{proirs_col}:     priors based on relative allele usage of the inferred gene.
#'  \item \code{counts1}:        the appereance count on each chromosome of the first allele from \code{alleles}, the counts are seperated by a comma.
#'  \item \code{k1}:             the Bayesian factor value for the first allele (from \code{alleles}) inference.
#'  \item \code{counts2}:        the appereance count on each chromosome of the second allele from \code{alleles}, the counts are seperated by a comma.
#'  \item \code{k2}:             the Bayesian factor value for the second allele (from \code{alleles}) inference.
#'  \item \code{counts3}:        the appereance count on each chromosome of the third allele from \code{alleles}, the counts are seperated by a comma.
#'  \item \code{k3}:             the Bayesian factor value for the third allele (from \code{alleles}) inference.
#'  \item \code{counts4}:        the appereance count on each chromosome of the fourth allele from \code{alleles}, the counts are seperated by a comma.
#'  \item \code{k4}:             the Bayesian factor value for the fourth allele (from \code{alleles}) inference.
#'}
#'
#' @details
#' Function accepts a \code{data.frame} in AIRR format (\url{https://changeo.readthedocs.io/en/stable/standard.html}) containing the following columns:
#' \itemize{
#'   \item \code{'subject'}: The subject name
#'   \item \code{'v_call'}: V allele call(s) (in an IMGT format)
#'   \item \code{'d_call'}: D allele call(s) (in an IMGT format, only for heavy chains)
#'   \item \code{'j_call'}: J allele call(s) (in an IMGT format)
#' }
#'
#' @examples
#' # Load example data and germlines
#' data(samples_db, HVGERM, HDGERM)
#'
#' # Selecting a single individual
#' clip_db = samples_db[samples_db$subject=='I5', ]
#'
#' # Infering haplotype
#' haplo_db = createFullHaplotype(clip_db,toHap_col=c('v_call','d_call'),
#' hapBy_col='j_call',hapBy='IGHJ6',toHap_GERM=c(HVGERM,HDGERM))
#'
#'
#' @export

createFullHaplotype <-
  function(clip_db,
           toHap_col = c("v_call", "d_call"),
           hapBy_col = "j_call",
           hapBy = "IGHJ6",
           toHap_GERM = NULL,
           relative_freq_priors = TRUE,
           kThreshDel = 3,
           rmPseudo = TRUE,
           deleted_genes = c(),
           nonReliable_Vgenes = c(),
           min_minor_fraction = 0.3,
           single_gene = TRUE,
           chain = c("IGH", "IGK", "IGL", "TRB")) {

    # Check if germline was inputed

    if (missing(chain)) {
      chain = "IGH"
    }
    chain <- match.arg(chain)

    if (missing(toHap_GERM)){
      message("Missing toHap_GERM, using Default germline from GERM by the given chain\n")
      toHap_GERM <-  unlist(unname(GERM[[chain]][toHap_col]))
    }

    if (!("subject" %in% names(clip_db))) {
      clip_db$subject <- "S1"
    }

    haplo_db <- c()
    clip_db <-
      clip_db %>% select(.data$subject,!!eval(c(hapBy_col, toHap_col)))
    for (sample_name in unique(clip_db$subject)) {
      if (is.list(nonReliable_Vgenes)) {
        nonReliable_Vgenes_vec <- nonReliable_Vgenes[[sample_name]]
      } else
        nonReliable_Vgenes_vec <- nonReliable_Vgenes

      if (is.data.frame(deleted_genes)) {
        deleted_genes_vec <-
          deleted_genes %>% filter(.data$subject == sample_name,
                                   .data$deletion == "Deletion") %>% select(.data$gene) %>% pull()
        if (is.null(nonReliable_Vgenes_vec))
          nonReliable_Vgenes_vec <-
            deleted_genes %>% filter(.data$subject == sample_name,
                                     .data$deletion == "Non reliable") %>% select(.data$gene) %>% pull()
      } else
        deleted_genes_vec <- c()


      ### Check if haplotype can be infered by the specific gene in the data set.  Only relevant genes with one assignment
      clip_db_sub <-
        clip_db %>% filter(.data$subject == sample_name,
                           !grepl(',',!!as.name(hapBy_col), perl = T))
      hapBy_priors <-
        clip_db_sub %>% filter(grepl(paste0(hapBy, "\\*"),!!as.name(hapBy_col), perl = T)) %>% dplyr::count(!!as.name(hapBy_col)) %>% mutate(freq = n / sum(n)) %>% select(-n)
      hapBy_priors <- setNames(hapBy_priors[[2]], hapBy_priors[[1]])
      hapBy_alleles <- names(hapBy_priors)
      if (length(hapBy_alleles) != 2) {
        if (sample_name == tail(unique(clip_db$subject), 1)) {
          message(
            paste0(
              "For sample ",
              sample_name,
              ", cannot haplotype by more or less than two alleles\n"
            )
          )
          next()
        } else{
          message(
            paste0(
              "For sample ",
              sample_name,
              ", there were ",
              length(hapBy_alleles),
              " alleles, can not haplotype by ",
              ifelse(length(hapBy_alleles) > 2, "more", "less"),
              " than two alleles."
            )
          )
          next()
        }
      } else{
        message(paste0(
          "For sample ",
          sample_name,
          ", haplotyping with ",
          paste0(hapBy_alleles, collapse = "/")
        ))
      }

      if (min(hapBy_priors) < min_minor_fraction) {
        if (sample_name == tail(unique(clip_db$subject), 1)) {
          stop("Can not haplotype, minor allele fraction lower than the cutoff set by the user")
        } else{
          message(
            paste0(
              "minor allele fraction lower than the cutoff set by the user for sample ",
              sample_name,
              ", try changing the parameters"
            )
          )
          next()
        }
      }


      GENES <-
        unique(gsub(
          "\\*.*",
          "\\1",
          grep(
            "^(?=.*IG)",
            unique(unlist(clip_db_sub[clip_db_sub[, hapBy_col] %in% hapBy_alleles, toHap_col], use.names = F)),
            value = T,
            perl = T
          ),
          perl = T
        ))

      GENES.ref <-
        unique(gsub("\\*.*", "\\1", names(toHap_GERM), perl = T))

      if (rmPseudo) {
        GENES <- GENES[!grepl("OR|NL", GENES)]
        GENES <- GENES[!(GENES %in% PSEUDO[[chain]])]

        GENES.ref <- GENES.ref[!grepl("OR|NL", GENES.ref)]
        GENES.ref <- GENES.ref[!(GENES.ref %in% PSEUDO[[chain]])]
      }

      GENES.df.num <-
        data.table::rbindlist(lapply(intersect(GENES, GENES.ref), function(G) {
          if (G %in% deleted_genes_vec || G %in% nonReliable_Vgenes_vec) {
            relFreqDf.tmp <- data.frame(matrix(c(
              sample_name, G,
              rep(ifelse(
                G %in% nonReliable_Vgenes_vec,
                'NR',
                ifelse(G %in% deleted_genes_vec, 'Del')
              ), 2),
              rep(NA, 11)
            ), nrow = 1), stringsAsFactors = F)

            relFreqDf.tmp <- asNum(relFreqDf.tmp)
            return(relFreqDf.tmp)
          } else{
            toHap_col_tmp <-
              toHap_col[stringi::stri_detect_fixed(pattern = substr(tolower(G), 4, 4), str = toHap_col)]

            if (single_gene) {
              clip_db_sub_g <-
                clip_db_sub %>% filter(grepl(
                  paste0("^(", G, "\\*[[:digit:]]*[\\_[[:alnum:]]*]*,?)+$"),
                  !!as.name(toHap_col_tmp),
                  perl = T
                ))
            } else {
              clip_db_sub_g <-
                clip_db_sub %>% filter(grepl(
                  paste0("^(", G, "\\*[[:digit:]]*[\\_[[:alnum:]]*]*,?)+$"),
                  !!as.name(toHap_col_tmp),
                  perl = T
                ))
            }
            if (substr(G, 4, 4) == 'V') {
              calls_single <-
                clip_db_sub_g %>% filter(stringi::stri_detect_regex(
                  pattern = ",",
                  str = !!as.name(toHap_col_tmp),
                  negate = T
                ))
              calls_multi <-
                data.table(clip_db_sub_g %>% filter(
                  stringi::stri_detect_regex(
                    pattern = ",",
                    str = !!as.name(toHap_col_tmp)
                  )
                ),
                key = toHap_col_tmp)
              calls_multi <-
                calls_multi[, n := .N, by = eval(toHap_col_tmp)][, "prop" := n / nrow(clip_db_sub_g)][get("prop") > 0.4, ]
              if (length(calls_multi[[eval(toHap_col_tmp)]]) > 0)
                calls_multi[[toHap_col_tmp]] <-
                alleleCollapse(calls_multi[[eval(toHap_col_tmp)]])
              clip_db_sub.G <-
                rbind(calls_single, as.data.frame(calls_multi[, c("n", "prop") := NULL]))

            } else
              clip_db_sub.G <-
                clip_db_sub_g %>% filter(!grepl(',',!!as.name(toHap_col_tmp)))

            tmp <-
              clip_db_sub.G %>% filter(grepl(
                paste0('^(?=.*', hapBy, '\\*)'),
                !!as.name(hapBy_col),
                perl = T
              )) %>% select(!!as.name(toHap_col_tmp),!!as.name(hapBy_col)) %>% table()

            if (nrow(tmp) == 0) {
              return()
            } else{
              # if one column add the second
              if (ncol(tmp) == 1) {
                toadd <- setdiff(hapBy_alleles, colnames(tmp))
                tmp <- cbind(tmp, rep(0, nrow(tmp)))
                colnames(tmp)[2] <- toadd

                tmp <- as.data.frame(tmp)
                tmp <- tmp[order(colnames(tmp))]
                tmp <- as.matrix(tmp)
              }

              if (relative_freq_priors) {
                clip_db_sub.hapBy <-
                  clip_db_sub.G[clip_db_sub.G[, toHap_col_tmp] %in% rownames(tmp),]
                toHap_priors <-
                  table(clip_db_sub.hapBy[, toHap_col_tmp]) / sum(table(clip_db_sub.hapBy[, toHap_col_tmp]))

                if (length(toHap_priors) != nrow(tmp)) {
                  toHap_priors_tmp <- c(rep(0, nrow(tmp)))
                  names(toHap_priors_tmp) <- rownames(tmp)
                  for (i in names(toHap_priors)) {
                    toHap_priors_tmp[i] <- toHap_priors[i]
                  }

                  toHap_priors <- toHap_priors_tmp
                }

                hap.df <-
                  createHaplotypeTable(
                    tmp,
                    HapByPriors = hapBy_priors,
                    toHapByCol = TRUE,
                    toHapPriors = toHap_priors
                  )

                relFreqDf.tmp <-
                  data.frame(c(sample_name, G, hap.df[, 2:length(hap.df)]),
                             stringsAsFactors = F)
                relFreqDf.tmp <- asNum(relFreqDf.tmp)
                return(relFreqDf.tmp)
              } else {
                hap.df <- createHaplotypeTable(tmp)
                relFreqDf.tmp <-
                  data.frame(c(sample_name, G, hap.df[, 2:length(hap.df)]),
                             stringsAsFactors = F)
                relFreqDf.tmp <- asNum(relFreqDf.tmp)
                return(relFreqDf.tmp)

              }
            }
          }
        }), use.names = FALSE) %>% as.data.frame()

      colnames(GENES.df.num) <-
        c(
          "subject",
          "gene",
          gsub(pattern = "*", "_", hapBy_alleles, fixed = T),
          'alleles',
          'proirs_row',
          'proirs_col',
          'counts1',
          'k1',
          'counts2',
          'k2',
          'counts3',
          'k3',
          'counts4',
          'k4'
        )
      # Check if toHap_col genes are in toHap_GERM
      if (length(GENES.df.num) == 0)
        stop("Genes in haplotype column to be infered do not match the genes germline given")


      ## Fill deleted according to a k thershold
      unkIDX <-
        which(GENES.df.num[gsub("*", "_", hapBy_alleles[1], fixed = T)][, 1] == "Unk")
      delIDX <-
        unkIDX[which(sapply(unkIDX, function(i)
          min(GENES.df.num[i, paste0('k', 1:4)], na.rm = T)) >= kThreshDel)]
      GENES.df.num[delIDX, paste(gsub("*", "_", hapBy_alleles[1], fixed = T))] <-
        "Del"

      unkIDX <-
        which(GENES.df.num[gsub("*", "_", hapBy_alleles[2], fixed = T)][, 1] == "Unk")
      delIDX <-
        unkIDX[which(sapply(unkIDX, function(i)
          min(GENES.df.num[i, paste0('k', 1:4)], na.rm = T)) >= kThreshDel)]
      GENES.df.num[delIDX, paste(gsub("*", "_", hapBy_alleles[2], fixed = T))] <-
        "Del"

      ## Add as unknown genes that do not appear in the individual and mark them as unknown

      GENES.MISSING <- GENES.ref[!(GENES.ref %in% GENES.df.num$gene)]
      if (length(GENES.MISSING) > 0) {
        m <- length(GENES.MISSING)

        sub.df <- data.frame(do.call(rbind, lapply(1:m, function(x) {
          c(sample_name, NA, "Unk", "Unk", rep(NA, 11))
        })))
        names(sub.df) <- names(GENES.df.num)

        sub.df$gene <- GENES.MISSING

        sub.df[, gsub("*", "_", hapBy_alleles, fixed = T)] <-
          matrix(rep(
            ifelse(
              GENES.MISSING %in% nonReliable_Vgenes_vec,
              'NR',
              ifelse(GENES.MISSING %in% deleted_genes_vec, 'Del', 'Unk')
            ),
            2
          ), ncol = 2)

        GENES.df.num <- rbind(GENES.df.num, sub.df)
      }

      haplo_db <- rbind(haplo_db, GENES.df.num)
    }

    return(haplo_db)

  }

##########################################################################
#' Double chromosome deletion by relative gene usage
#'
#' The \code{geneUsage} function calculates the relative gene usage.
#'
#'
#' @param    clip_db               a \code{data.frame} in AIRR format. See details.
#' @param    chain                 the IG/TR chain: IGH,IGK,IGL,TRB. Default is IGH.
#' @param    genes_order           A vector of the genes by the desired order. Default is by GENE.loc
#' @param    rmPseudo              if TRUE non-functional and pseudo genes are removed. Default is TRUE.
#'
#' @return  A \code{data.frame}, in which each row is the relative gene usage value per individual.
#'
#'The output containes the following columns:
#' \itemize{
#'  \item \code{subject}:       the subject name.
#'  \item \code{gene}:          the gene call
#'  \item \code{frac}:          the relative gene usage of the gene
#'}
#'
#' @details
#' The function accepts a \code{data.frame} in AIRR format (\url{https://changeo.readthedocs.io/en/stable/standard.html}) containing the following columns:
#' \itemize{
#'   \item \code{'subject'}: The subject name
#'   \item \code{'v_call'}: V allele call(s) (in an IMGT format)
#'   \item \code{'d_call'}: D allele call(s) (in an IMGT format, only for heavy chains)
#'   \item \code{'j_call'}: J allele call(s) (in an IMGT format)
#' }
#'
#' @export
geneUsage <- function(clip_db, chain = c("IGH", "IGK", "IGL", "TRB"), genes_order = NULL, rmPseudo = TRUE) {
  if (missing(chain)) {
    chain = "IGH"
  }
  chain <- match.arg(chain)

  if (!("subject" %in% names(clip_db))) {
    clip_db$subject <- "S1"
  }

  if(is.null(genes_order)){
    genes_order <- GENE.loc[[chain]]
  }

  GENE.loc.NoPseudo <-
    genes_order[!grepl("OR|NL", genes_order)]

  if(rmPseudo){
    GENE.loc.NoPseudo <-
      GENE.loc.NoPseudo[!(GENE.loc.NoPseudo %in% PSEUDO[[chain]])]
  }



  GENE.usage <- vector("list", length = length(GENE.loc.NoPseudo))
  names(GENE.usage) <- GENE.loc.NoPseudo
  for (samp in unique(clip_db$subject)) {
    clip_db_sub <- clip_db[clip_db$subject == samp,]
    # V gene distribution
    v_callS <-
      table(sapply(strsplit(clip_db_sub$v_call, "*", fixed = T), "[", 1))
    v_callS_freq <- v_callS / sum(v_callS)
    for (v in genes_order) {
      if (v %in% names(v_callS)) {
        GENE.usage[[v]] <- c(GENE.usage[[v]], v_callS_freq[v])
        names(GENE.usage[[v]])[length(GENE.usage[[v]])] <- samp
      } else {
        if (grepl(paste0(chain, "V"), v)) {
          GENE.usage[[v]] <- c(GENE.usage[[v]], 0)
          names(GENE.usage[[v]])[length(GENE.usage[[v]])] <- samp
        }
      }
    }
    # D gene distribution
    if (chain == "IGH") {
      D_SINGLE <- grep(pattern = ",",
                       clip_db_sub$d_call,
                       invert = T)
      d_callS <-
        table(sapply(strsplit(clip_db_sub$d_call[D_SINGLE], "*", fixed = T), "[", 1))
      d_callS_freq <- d_callS / sum(d_callS)
      for (d in genes_order) {
        if (d %in% names(d_callS)) {
          GENE.usage[[d]] <- c(GENE.usage[[d]], d_callS_freq[d])
          names(GENE.usage[[d]])[length(GENE.usage[[d]])] <- samp
        } else {
          if (grepl(paste0(chain, "D"), d)) {
            GENE.usage[[d]] <- c(GENE.usage[[d]], 0)
            names(GENE.usage[[d]])[length(GENE.usage[[d]])] <- samp
          }
        }
      }
    }
    # J gene distribution
    j_callS <-
      table(sapply(strsplit(clip_db_sub$j_call, "*", fixed = T), "[", 1))
    j_callS_freq <- j_callS / sum(j_callS)
    for (j in genes_order) {
      if (j %in% names(j_callS)) {
        GENE.usage[[j]] <- c(GENE.usage[[j]], j_callS_freq[j])
        names(GENE.usage[[j]])[length(GENE.usage[[j]])] <- samp
      } else {
        if (grepl(paste0(chain, "J"), j)) {
          GENE.usage[[j]] <- c(GENE.usage[[j]], 0)
          names(GENE.usage[[j]])[length(GENE.usage[[j]])] <- samp
        }
      }
    }
  }

  gusage <- unlist(GENE.usage)
  gusage.gene <-
    sapply(strsplit(names(unlist(GENE.usage)), ".", fixed = T), "[", 1)
  gusage.samp <-
    gsub(paste0(paste0(gusage.gene, "[.]"), collapse = "|"), "", names(unlist(GENE.usage)))
  GENE.usage.df <-
    data.frame(
      subject = gusage.samp,
      gene = gusage.gene,
      frac = gusage,
      stringsAsFactors = F,
      row.names = NULL
    )
  GENE.usage.df <-
    GENE.usage.df %>% filter(.data$gene %in% GENE.loc.NoPseudo)

  return(GENE.usage.df)
}


##########################################################################
#' Double chromosome deletion by relative gene usage
#'
#' The \code{deletionsByBinom} function inferes double chromosome deletion events by relative gene usage.
#'
#'
#' @param    clip_db               a \code{data.frame} in AIRR format. See details.
#' @param    chain                 the IG/TR chain: IGH,IGK,IGL,TRB. Default is IGH.
#' @param    nonReliable_Vgenes    a list of known non reliable gene assignments. A \code{list} created by \code{nonReliableVGenes}.
#' @param    genes_order           A vector of the genes by the desired order. Default is by GENE.loc
#'
#' @return  A \code{data.frame}, in which each row is the double chomosome deletion inference of a gene.
#'
#'The output containes the following columns:
#' \itemize{
#'  \item \code{subject}:       the subject name.
#'  \item \code{gene}:          the gene call
#'  \item \code{frac}:          the relative gene usage of the gene
#'  \item \code{cutoff}:        the the cutoff of for the binomial test
#'  \item \code{pval}:          the p-value of the binomial test
#'  \item \code{deletion}:      if a double chromosome deletion event of a gene occured.
#'}
#'
#' @details
#' The function accepts a \code{data.frame} in AIRR format (\url{https://changeo.readthedocs.io/en/stable/standard.html}) containing the following columns:
#' \itemize{
#'   \item \code{'subject'}: The subject name
#'   \item \code{'v_call'}: V allele call(s) (in an IMGT format)
#'   \item \code{'d_call'}: D allele call(s) (in an IMGT format, only for heavy chains)
#'   \item \code{'j_call'}: J allele call(s) (in an IMGT format)
#' }
#'
#' @examples
#' # Load example data and germlines
#' data(samples_db)
#'
#' # Selecting a single individual
#' clip_db = samples_db[samples_db$subject=='I5', ]
#' # Infering haplotype
#' del_binom_df = deletionsByBinom(clip_db)
#' head(del_binom_df)
#'
#' @export

deletionsByBinom <-
  function(clip_db,
           chain = c("IGH", "IGK", "IGL"),
           nonReliable_Vgenes = c(),
           genes_order = NULL) {

    if (missing(chain)) {
      chain = "IGH"
    }
    chain <- match.arg(chain)

    if (!("subject" %in% names(clip_db))) {
      clip_db$subject <- "S1"
    }

    if(is.null(genes_order)){
      genes_order <- GENE.loc[[chain]]
    }

    GENE.loc.NoPseudo <-
      genes_order[!grepl("OR|NL", genes_order)]

    GENE.loc.NoPseudo <-
      GENE.loc.NoPseudo[(GENE.loc.NoPseudo %in% Binom.test.gene.cutoff[[chain]]$gene)]

    GENE.usage.df <- geneUsage(clip_db, chain)

    GENE.usage.df <-
      GENE.usage.df %>% filter(.data$gene %in% Binom.test.gene.cutoff[[chain]][[1]])

    GENE.usage.df$min_frac <-
      sapply(1:nrow(GENE.usage.df), function(x) {
        unique(Binom.test.gene.cutoff[[chain]]$min_frac[Binom.test.gene.cutoff[[chain]][[1]] == GENE.usage.df$gene[x]])
      })


    SAMPLE.SIZE.V <- sample_size(clip_db, "v_call")
    SAMPLE.SIZE.J <- sample_size(clip_db, "j_call")
    SAMPLE.SIZE <-
      sapply(unique(clip_db$subject), function(x)
        nrow(clip_db[clip_db$subject == x,]))
    names(SAMPLE.SIZE) <- unique(clip_db$subject)
    GENE.usage.df$NREADS <- SAMPLE.SIZE[GENE.usage.df$subject]

    GENE.usage.df.V <-
      GENE.usage.df %>% filter(grepl(paste0(chain, "V"), .data$gene))
    GENE.usage.df.J <-
      GENE.usage.df %>% filter(grepl(paste0(chain, "J"), .data$gene))

    GENE.usage.df.V$NREADS <- SAMPLE.SIZE.V[GENE.usage.df.V$subject]
    GENE.usage.df.J$NREADS <- SAMPLE.SIZE.J[GENE.usage.df.J$subject]

    GENE.usage.df.V$NREADS_SAMP <-
      SAMPLE.SIZE[GENE.usage.df.V$subject]
    GENE.usage.df.J$NREADS_SAMP <-
      SAMPLE.SIZE[GENE.usage.df.J$subject]

    GENE.loc.V <- genes_order[grep("V", genes_order)]
    GENE.usage.df.V <-
      binomTestDeletion(
        GENE.usage.df.V,
        cutoff = 0.001,
        p.val.cutoff = 0.01,
        chain = chain,
        GENE.loc.V
      )

    if (is.list(nonReliable_Vgenes)) {
      for (sample_name in names(nonReliable_Vgenes)) {
        levels(GENE.usage.df.V$col) <-
          c(levels(GENE.usage.df.V$col), "Non reliable")
        idx <-
          which(GENE.usage.df.V$gene[GENE.usage.df.V$subject == sample_name] %in% nonReliable_Vgenes[[sample_name]])
        GENE.usage.df.V$col[GENE.usage.df.V$subject == sample_name][idx] <-
          "Non reliable"
      }
    }

    GENE.loc.J <- genes_order[grep("J", genes_order)]
    GENE.usage.df.J <-
      binomTestDeletion(
        GENE.usage.df.J,
        cutoff = 0.005,
        p.val.cutoff = 0.01,
        chain = chain,
        GENE.loc.J
      )

    if (chain == "IGH") {
      SAMPLE.SIZE.D <- sample_size(clip_db, "d_call")
      GENE.usage.df.D <-
        GENE.usage.df %>% filter(grepl(paste0(chain, "D"), .data$gene))
      GENE.usage.df.D$NREADS <-
        SAMPLE.SIZE.D[GENE.usage.df.D$subject]
      GENE.usage.df.D$NREADS_SAMP <-
        SAMPLE.SIZE[GENE.usage.df.D$subject]
      GENE.loc.D <-
        genes_order[grep("D", genes_order)]
      GENE.usage.df.D <-
        binomTestDeletion(GENE.usage.df.D,
                          cutoff = 0.005,
                          p.val.cutoff = 0.01,
                          chain,
                          GENE.loc.D)

      GENE.usage.df.V$col <-
        factor(GENE.usage.df.V$col, levels = levels(GENE.usage.df.V$col))
      GENE.usage.df.D$col <-
        factor(GENE.usage.df.D$col, levels = levels(GENE.usage.df.V$col))
      GENE.usage.df.J$col <-
        factor(GENE.usage.df.J$col, levels = levels(GENE.usage.df.V$col))

      GENE.usage.df.V$gene <-
        factor(GENE.usage.df.V$gene, levels = genes_order)
      GENE.usage.df.D$gene <-
        factor(GENE.usage.df.D$gene, levels = genes_order)
      GENE.usage.df.J$gene <-
        factor(GENE.usage.df.J$gene, levels = genes_order)
      GENE.usage.df <-
        rbind(GENE.usage.df.V, GENE.usage.df.D, GENE.usage.df.J)
    } else {
      GENE.usage.df.V$col <-
        factor(GENE.usage.df.V$col, levels = levels(GENE.usage.df.V$col))
      GENE.usage.df.J$col <-
        factor(GENE.usage.df.J$col, levels = levels(GENE.usage.df.V$col))

      GENE.usage.df.V$gene <-
        factor(GENE.usage.df.V$gene, levels = genes_order)
      GENE.usage.df.J$gene <-
        factor(GENE.usage.df.J$gene, levels = genes_order)
      GENE.usage.df <- rbind(GENE.usage.df.V, GENE.usage.df.J)
    }



    GENE.usage.df <-
      GENE.usage.df %>% ungroup() %>% select(
        .data$subject,
        .data$gene,
        .data$frac,
        cutoff = .data$min_frac,
        pval = .data$pval_adj,
        deletion = .data$col
      )
    return(GENE.usage.df)
  }

##########################################################################
#' Single chromosomal D or J gene deletions inferred by the V pooled method
#'
#' The \code{deletionsByVpooled} function inferes single chromosomal deletion for D and J gene .
#'
#'
#' @param  clip_db                  a \code{data.frame} in AIRR format. See details.
#' @param  chain                    the IG chain: IGH,IGK,IGL. Default is IGH.
#' @param  deletion_col             a vector of column names for which single chromosome deletions should be inferred. Default is j_call and d_call.
#' @param  count_thresh             integer, the minimun number of sequences mapped to a specific V gene to be included in the V pooled inference.
#' @param  deleted_genes            double chromosome deletion summary table. A \code{data.frame} created by \code{deletionsByBinom}.
#' @param  min_minor_fraction       the minimum minor allele fraction to be used as an anchor gene. Default is 0.3
#' @param  kThreshDel               the minimum lK (log10 of the Bayes factor) to call a deletion. Default is 3.
#' @param  nonReliable_Vgenes       a list of known non reliable gene assignments. A \code{list} created by \code{nonReliableVGenes}.
#'
#' @return
#' A \code{data.frame}, in which each row is the single chomosome deletion inference of a gene.
#'
#'The output containes the following columns:
#' \itemize{
#'  \item \code{subject}:       the subject name.
#'  \item \code{gene}:          the gene call
#'  \item \code{deletion}:      chromosome deletions inferred. Encoded 1 for deletion and 0 for no deletion.
#'  \item \code{k}:             the Bayesian factor value for the deletion inference.
#'  \item \code{counts}:        the appereance count of the gene on each chromosome, the counts are seperated by a comma.
#'}
#'
#' @details
#' The function accepts a \code{data.frame} in AIRR format (\url{https://changeo.readthedocs.io/en/stable/standard.html}) containing the following columns:
#' \itemize{
#'   \item \code{'subject'}: The subject name
#'   \item \code{'v_call'}: V allele call(s) (in an IMGT format)
#'   \item \code{'d_call'}: D allele call(s) (in an IMGT format, only for heavy chains)
#'   \item \code{'j_call'}: J allele call(s) (in an IMGT format)
#' }
#'
#' @examples
#' \donttest{
#' data(samples_db)
#'
#' # Infering V pooled deletions
#' del_db <- deletionsByVpooled(samples_db)
#' head(del_db)
#' }
#' @export
# not for light chain
deletionsByVpooled <-
  function(clip_db,
           chain = c("IGH", "IGK", "IGL"),
           deletion_col = c("d_call", "j_call"),
           count_thresh = 50,
           deleted_genes = "",
           min_minor_fraction = 0.3,
           kThreshDel = 3,
           nonReliable_Vgenes = c()) {
    if (missing(chain)) {
      chain = "IGH"
    }
    chain <- match.arg(chain)

    if (chain != "IGH")
      deletion_col = "j_call"
    deletion_col <- match.arg(deletion_col)

    if (!("subject" %in% names(clip_db))) {
      clip_db$subject <- "S1"
    }

    del.df <- c()
    for (sample_name in unique(clip_db$subject)) {
      clip_db_sub <- clip_db[clip_db$subject == sample_name,]
      if (is.data.frame(deleted_genes))
        deleted_genes_df <-
          deleted_genes %>% filter(.data$subject == sample_name,
                                   grepl(paste0(chain, c("D", "J"), collapse = "|"), .data$gene))
      else
        deleted_genes_df <- c()

      ### remove non reliable V genes prior to pooling
      if (is.list(nonReliable_Vgenes))
        nonReliable_Vgenes_vec <-
          nonReliable_Vgenes[[sample_name]]
      else
        nonReliable_Vgenes_vec <- nonReliable_Vgenes

      ### Only relevant genes with one assignment
      IND <- apply(clip_db_sub, 1, function(x) {
        sum(grep(",", x[c("v_call", deletion_col)], invert = F))
      })

      clip_db_sub <- clip_db_sub[IND == 0,]

      ### Test for heterozygous V genes

      VGENES <-
        grep(chain, unique(sapply(
          strsplit(clip_db_sub$v_call, split = "*", fixed = T), "[", 1
        )), value = T)
      VGENES <- VGENES[!VGENES %in% nonReliable_Vgenes_vec]
      GENES <- unlist(sapply(VGENES, function(x) {
        gene_counts <-
          table(grep(
            clip_db_sub$v_call,
            pattern = paste0(x, "*"),
            fixed = T,
            value = T
          ))
        if (length(gene_counts) == 2 &
            (min(gene_counts) / sum(gene_counts)) >= min_minor_fraction &
            sum(gene_counts) >= count_thresh) {
          return(x)
        }
      }))

      V.df <- list()
      if (length(GENES) > 0) {
        message(
          paste0(
            "The following genes used for pooled deletion detection for sample ",
            sample_name
          )
        )
        message(paste(GENES, sep = ","))
        toHapGerm <-
          unlist(unname(GERM[[chain]][deletion_col]))
        for (G in GENES) {
          full.hap <-
            createFullHaplotype(
              clip_db_sub,
              toHap_col = deletion_col,
              hapBy_col = "v_call",
              hapBy = G,
              toHap_GERM = toHapGerm,
              relative_freq_priors = T,
              kThreshDel = kThreshDel,
              rmPseudo = T,
              deleted_genes = deleted_genes_df,
              chain = chain
            )

          full.hap$V_ALLELE_1 <-
            strsplit(names(full.hap)[3], "_")[[1]][2]
          full.hap$V_ALLELE_2 <-
            strsplit(names(full.hap)[4], "_")[[1]][2]

          names(full.hap)[3] <- paste0(G, "_", 1)
          names(full.hap)[4] <- paste0(G, "_", 2)
          V.df[[G]] <- rbind(V.df[[G]], full.hap)

        }
      } else {
        if (sample_name == tail(unique(clip_db$subject), 1)) {
          stop(
            "No heterozygous V genes found for deletion detection, try changing the parameters"
          )
        } else{
          message(
            paste0(
              "No heterozygous V genes found for ",
              sample_name,
              " deletion detection, try changing the parameters"
            )
          )
          next()
        }
      }

      GENES <- unlist(sapply(names(V.df), function(G) {
        if (sample_name %in% V.df[[G]]$subject) {
          return(G)
        }
      }))

      if (!is.null(GENES)) {
        d.del.df <- c()

        for (G in GENES) {
          tmp <-
            V.df[[G]] %>% filter(.data$subject == sample_name,
                                 grepl(paste0(chain, c("D", "J"), collapse = "|"), .data$gene))
          tmp$deletion <- apply(tmp, 1, function(x) {
            if (x[3] == "Unk" & x[4] != "Unk" | x[3] != "Unk" & x[4] == "Unk") {
              return(1)
            }
            if (x[3] == "Del" &
                x[4] != "Del" | x[3] != "Del" & x[4] == "Del") {
              return(2)
            }
            if (x[3] == "Unk" & x[4] == "Unk") {
              return(3)
            }
            if (x[3] == "Del" & x[4] == "Del") {
              return(4)
            }
            return(0)
          })
          tmp$k <- sapply(1:nrow(tmp), function(i) {
            if (!is.na(tmp$counts2[i])) {
              return(max(tmp$k1[i], tmp$k2[i], na.rm = T))
            } else {
              return(tmp$k1[i])
            }
          })
          tmp$counts <- sapply(1:nrow(tmp), function(i) {
            if (!is.na(tmp$counts2[i])) {
              cnt1 <- as.numeric(strsplit(as.character(tmp$counts1[i]), ",")[[1]])
              cnt2 <-
                as.numeric(strsplit(as.character(tmp$counts2[i]), ",")[[1]])
              cnt <- cnt1 + cnt2
              return(paste0(cnt, collapse = ","))
            } else {
              return(as.character(tmp$counts1[i]))
            }
          })

          tmp <-
            tmp %>% mutate(subject = sample_name) %>% select(.data$subject,
                                                             .data$gene,
                                                             .data$deletion,
                                                             .data$k,
                                                             .data$counts)
          tmp$V_GENE <- rep(G, nrow(tmp))
          tmp$deletion2 <- ifelse(tmp$deletion == 0, 0, 1)
          d.del.df <- rbind(d.del.df, tmp)

        }


        tmp.df <-
          do.call("rbind", lapply(unique(d.del.df$gene), function(x) {
            x.df <-
              d.del.df %>% filter(.data$gene == x) %>% mutate(minCOUNT = (sapply(strsplit(as.character(.data$counts), ","), function(x) {
                min(as.numeric(x))
              })),
              maxCOUNT = (sapply(strsplit(as.character(.data$counts), ","), function(x) {
                max(as.numeric(x))
              })))
            x.df <- x.df %>% filter(!is.na(.data$k))
            if (nrow(x.df) != 0) {
              x.df <-
                x.df %>% group_by(.data$deletion2) %>% mutate(
                  minCOUNTsum = sum(.data$minCOUNT),
                  maxCOUNTsum = sum(.data$maxCOUNT)
                ) %>% slice(1) %>% mutate(
                  deletion3 = which.max(get_probabilites_with_priors(
                    setNames(c(.data$maxCOUNTsum, .data$minCOUNTsum), c("max","min"))
                  )[1:2]),
                  k2 = max(get_probabilites_with_priors(
                    setNames(c(.data$maxCOUNTsum, .data$minCOUNTsum), c("max","min"))
                  )[1:2]) - min(get_probabilites_with_priors(
                    setNames(c(.data$maxCOUNTsum, .data$minCOUNTsum), c("max","min"))
                  )[1:2])
                )
              return(x.df)
            }

          }))


        tmp.df.slct <-
          tmp.df %>% mutate(counts2 = paste0(.data$minCOUNTsum, ",", .data$maxCOUNTsum)) %>% select(
            .data$subject,
            .data$gene,
            .data$deletion2,
            .data$deletion3,
            .data$k2,
            .data$counts2,
            .data$V_GENE
          )
        tmp.df.slct$V_GENE <-
          ifelse(tmp.df.slct$deletion2 != 0, "POOLED_UNK", "POOLED_KNOWN")
        tmp.df.slct <-
          tmp.df.slct[names(tmp.df.slct) != "deletion2"]
        d.del.df <- d.del.df[names(d.del.df) != "deletion2"]
        names(tmp.df.slct) <- names(d.del.df)
        tmp.df.slct$deletion <-
          ifelse(tmp.df.slct$deletion == 2, 0, 1)
        ## ALL Vs
        tmp.df.slct.all <-
          tmp.df %>% ungroup() %>% group_by(.data$gene) %>% mutate(
            minCOUNTsum = sum(.data$minCOUNTsum),
            maxCOUNTsum = sum(.data$maxCOUNTsum)
          ) %>% slice(1) %>%
          mutate(
            deletion3 = which.max(get_probabilites_with_priors(
              setNames(c(.data$maxCOUNTsum, .data$minCOUNTsum), c("max","min"))
            )[1:2]),
            k2 = max(get_probabilites_with_priors(
              setNames(c(.data$maxCOUNTsum, .data$minCOUNTsum), c("max","min"))
            )[1:2]) - min(get_probabilites_with_priors(
              setNames(c(.data$maxCOUNTsum, .data$minCOUNTsum), c("max","min"))
            )[1:2])
          )
        tmp.df.slct.all <-
          tmp.df.slct.all %>% mutate(
            subject = sample_name,
            counts = paste0(.data$minCOUNTsum, ",", .data$maxCOUNTsum)
          ) %>% select(.data$subject,
                       .data$gene,
                       .data$deletion3,
                       .data$k2,
                       .data$counts) %>% rename(deletion = .data$deletion3, k = .data$k2)
        tmp.df.slct.all$V_GENE <- "V(pooled)"
        tmp.df.slct.all$deletion <-
          ifelse(tmp.df.slct.all$deletion == 2, 0, 1)
        d.del.df <- rbind(d.del.df, tmp.df.slct)
        d.del.df <-
          rbind(d.del.df, as.data.frame(tmp.df.slct.all))
        d.del.df.pooled <-
          d.del.df %>% filter(.data$V_GENE == "V(pooled)") %>% select(-.data$V_GENE)
        d.del.df.pooled$k <-
          round(as.numeric(d.del.df.pooled$k), digits = 2)
        d.del.df.pooled$gene <-
          factor(x = d.del.df.pooled$gene, levels = GENE.loc[[chain]])
      }
      del.df <- rbind(del.df, d.del.df.pooled)
    }
    return(del.df)

  }

##########################################################################
#' Detect non reliable gene assignment
#'
#' \code{nonReliableVGenes} Takes a \code{data.frame} in AIRR format  and detect non reliable IGHV genes. A non reliable gene is
#' when the ratio of the multiple assignments with a gene is below the threshold.
#'
#' @param  clip_db              a \code{data.frame} in AIRR format. See details.
#' @param  thresh               the threshold to consider non reliable gene. Default is 0.9
#' @param  appearance           the minimum fraction of gene appearance to be considered for reliability check. Default is 0.01.
#'
#' @return  a nested list of non reliable genes for all subject.
#'
#' @details
#'
#' The function accepts a \code{data.frame} in AIRR format (\url{https://changeo.readthedocs.io/en/stable/standard.html}) containing the following columns:
#' \itemize{
#'   \item \code{'subject'}: subject names
#'   \item \code{'v_call'}: V allele call(s) (in an IMGT format)
#' }
#'
#' @examples
#' # Example IGHV call data frame
#' clip_db <- data.frame(subject=rep('S1',6),
#' v_call=c('IGHV1-69*01','IGHV1-69*01','IGHV1-69*01,IGHV1-69*02',
#' 'IGHV4-59*01,IGHV4-61*01','IGHV4-59*01,IGHV4-31*02','IGHV4-59*01'))
#' # Detect non reliable genes
#' nonReliableVGenes(clip_db)
#' @export
# only heavy chain
nonReliableVGenes <-
  function(clip_db,
           thresh = 0.9,
           appearance = 0.01) {
    # if(missing(chain)) { chain='IGH' } chain <- match.arg(chain)

    if (!("subject" %in% names(clip_db))) {
      clip_db$subject <- "S1"
    }
    chain = "IGH"
    GENE.loc.NoPseudo <-
      GENE.loc[[chain]][grepl("V((?!NL).)*$", GENE.loc[[chain]], perl = T)]
    GENE.loc.NoPseudo <-
      GENE.loc.NoPseudo[!(GENE.loc.NoPseudo %in% PSEUDO[[chain]])]

    non_reliable_genes <-
      sapply(as.character(unique(clip_db$subject)), function(sample_name) {
        gene_call <- clip_db$v_call[clip_db$subject == sample_name]
        unlist(lapply(GENE.loc.NoPseudo, function(gene) {
          sub <-
            grep(
              paste0(
                "((?:,|^)",
                gene,
                "\\*[[:digit:]]*[\\_[[:alnum:]]*]*(?:,|$))"
              ),
              gene_call,
              perl = T,
              value = T
            )
          total <- length(sub)
          if (total / length(gene_call) >= appearance) {
            sa <-
              length(grep(
                paste0("^(", gene, "\\*[[:digit:]]*[\\_[[:alnum:]]*]*,?)+$"),
                sub,
                perl = T
              ))
            if (sa / total < thresh)
              return(gene)
          }

        }), FALSE, FALSE)
      }, USE.NAMES = T, simplify = F)
    return(non_reliable_genes)
  }
