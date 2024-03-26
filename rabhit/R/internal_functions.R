# Internal functions -----------------------------------------------------

#' @include rabhit.R
NULL

########################################################################################################
# Calculate models likelihood
#
# \code{createHaplotypeTable} calculate likelihoods
#
# @param    X      a vector of counts
# @param    alpha_dirichlet      alpha parameter for dirichlet distribution
# @param    epsilon    epsilon
# @param    priors      a vector of priors
# @param    homozygous  if the tested gene agaist the anchor is homozygous
# @return  log10 of the likelihoods
#
# @export
get_probabilites_with_priors <-
  function(X,
           alpha_dirichlet = c(0.5, 0.5) * 2,
           epsilon = 0.01,
           params = c(0.5, 0.5),
           homozygous = FALSE) {
    ## Hypotheses
    X <- sort(X, decreasing = TRUE)
    Number_Of_Divisions <- 0

    if (grepl("TRBD2", names(X)[1])) {
      # epsilon fix for TRBD
      epsilon <- X
      epsilon["TRBD2*01"] <- 0.125
      epsilon["TRBD2*02"] <- 0.04

      if (homozygous) {
        epsilon <- epsilon / 3
      }
    }

    H1 <- c(1, 0)
    H2 <- c(params[1], params[2])

    E1 <-
      ddirichlet((H1 + epsilon) / sum(H1 + epsilon), alpha_dirichlet + X)
    E2 <-
      ddirichlet((H2 + epsilon) / sum(H2 + epsilon), alpha_dirichlet + X)

    while (sort(c(E1, E2), decreasing = TRUE)[2] == 0) {
      Number_Of_Divisions <- Number_Of_Divisions + 1
      X <- X / 10
      E1 <-
        ddirichlet((H1 + epsilon) / sum(H1 + epsilon), alpha_dirichlet + X)
      E2 <-
        ddirichlet((H2 + epsilon) / sum(H2 + epsilon), alpha_dirichlet + X)
    }
    return(c(log10(c(E1, E2)), Number_Of_Divisions))
  }


##############################################################################################################
# Create haplotype table
#
# \code{createHaplotypeTable} Haplotype of a specific gene
#
# @details
#
# @param  df  table of counts
# @param  HapByPriors vector of frequencies of each of the anchor gene alleles
# @param  toHapByCol logical, haplotype each chromosome separetly to imrove the aligner assignmnet
# @param  toHapPriors vector of frequencies of the haplotyped gene alleles
#
# @return  data frame with chromosomal associasions of alleles of a specific gene
#
#
#
# @export
createHaplotypeTable <-
  function(df,
           HapByPriors = c(0.5, 0.5),
           toHapByCol = TRUE,
           toHapPriors = c(0.5, 0.5)) {
    hapBy <- colnames(df)
    tohap <- rownames(df)
    tohap.gene <- strsplit(tohap[1], "*", fixed = T)[[1]][1]

    GENES.df <-
      data.frame(gene = tohap.gene, "Unk", "Unk", stringsAsFactors = F)
    names(GENES.df)[2:3] <- gsub("*", ".", hapBy, fixed = T)
    GENES.df.num <- reshape2::melt(df)

    df.old <- df
    if (toHapByCol) {
      if (nrow(df) > 1) {
        for (j in 1:ncol(df)) {
          counts <- c(sort(df[, j], decreasing = T), 0, 0, 0)
          if (sum(counts) != counts[1]) {
            names(counts)[1:nrow(df)] <- names(sort(df[, j], decreasing = T))

            toHapPriors_srtd <-
              if (!is.null(names(toHapPriors)))
                toHapPriors[names(counts[1:2])]
            else
              toHapPriors
            resCol <-
              get_probabilites_with_priors(counts[1:2], params = toHapPriors_srtd)
            resMaxInd <- which.max(resCol[-(length(resCol))])
            if (resMaxInd < nrow(df)) {
              df[, j][order(df[, j], decreasing = T)[(resMaxInd + 1):nrow(df)]] <-
                0
            }
          }

        }
      }

    }

    counts.list <- list()
    res.list <- list()
    for (i in 1:nrow(df)) {
      allele <- rownames(df)[i]
      if (sum(df[i, ]) == 0) {
        tohap <- tohap[-which(tohap == allele)]
        next
      }
      gene <- strsplit(allele, "*", fixed = T)[[1]][1]


      counts <- c(sort(df.old[i,], decreasing = T), 0, 0, 0)

      if (ncol(df) == 1)
        names(counts)[1:ncol(df)] <- colnames(df)
      else
        names(counts)[1:ncol(df)] <- names(sort(df[i,], decreasing = T))

      HapByPriors_srtd <- if (!is.null(names(HapByPriors)))
        HapByPriors[names(counts[1:2])]
      else
        HapByPriors
      res <-
        get_probabilites_with_priors(counts[1:2],
                                     params = HapByPriors_srtd,
                                     homozygous = (nrow(df) == 1))


      # Assign allele for a chromosome If hetero in chomosome : check if anythong was assigned (equals 'unk'), if was (different than 'unk'), paste second
      # allele in the same chromosome
      if (res[1] > res[2]) {
        if (GENES.df[GENES.df$gene == gene, gsub(x = names(which.max(counts)), "*", ".", fixed = T) == names(GENES.df)] == "Unk") {
          GENES.df[GENES.df$gene == gene, gsub(x = names(which.max(counts)), "*", ".", fixed = T) == names(GENES.df)] <-
            strsplit(allele, "*", fixed = T)[[1]][2]
        } else {
          GENES.df[GENES.df$gene == gene, gsub(x = names(which.max(counts)), "*", ".", fixed = T) == names(GENES.df)] <-
            paste(c(GENES.df[GENES.df$gene ==
                               gene, gsub(x = names(which.max(counts)), "*", ".", fixed = T) == names(GENES.df)], strsplit(allele, "*", fixed = T)[[1]][2]), collapse = ",")
        }
      } else {
        if (GENES.df[GENES.df$gene == gene, 2] == "Unk") {
          GENES.df[GENES.df$gene == gene, 2] <-
            strsplit(allele, "*", fixed = T)[[1]][2]
        } else {
          GENES.df[GENES.df$gene == gene, 2] <-
            paste(c(GENES.df[GENES.df$gene == gene, 2], strsplit(allele, "*", fixed = T)[[1]][2]), collapse = ",")
        }

        if (GENES.df[GENES.df$gene == gene, 3] == "Unk") {
          GENES.df[GENES.df$gene == gene, 3] <-
            strsplit(allele, "*", fixed = T)[[1]][2]
        } else {
          GENES.df[GENES.df$gene == gene, 3] <-
            paste(c(GENES.df[GENES.df$gene == gene, 3], strsplit(allele, "*", fixed = T)[[1]][2]), collapse = ",")
        }

      }



      counts.list[[i]] <- counts
      res.list[[i]] <- res
    }
    counts.list[sapply(counts.list, is.null)] <- NULL
    res.list[sapply(res.list, is.null)] <- NULL

    len.counts.list <- length(counts.list)
    GENES.df <-
      cbind(
        GENES.df,
        data.frame(
          alleles = paste(sapply(strsplit(tohap, "*", fixed = T), "[", 2), collapse = ","),
          priors_row = paste(format(HapByPriors,
                                    digits = 2), collapse = ","),
          priors_col = paste(format(toHapPriors, digits = 2), collapse = ","),
          counts1 = paste(counts.list[[1]][order(names(counts.list[[1]])[1:2])],
                          collapse = ","),
          k1 = max(res.list[[1]][1:2]) - min(res.list[[1]][1:2]),
          counts2 = ifelse(
            length(counts.list) >
              1,
            paste(counts.list[[2]][order(names(counts.list[[2]])[1:2])], collapse = ","),
            NA
          ),
          k2 = ifelse(length(counts.list) > 1, max(res.list[[2]][1:2]) - min(res.list[[2]][1:2]), NA),
          counts3 = ifelse(
            length(counts.list) > 2,
            paste(counts.list[[3]][order(names(counts.list[[3]])[1:2])], collapse = ","),
            NA
          ),
          k3 = ifelse(length(counts.list) > 2, max(res.list[[3]][1:2]) - min(res.list[[3]][1:2]), NA),
          counts4 = ifelse(
            length(counts.list) > 3,
            paste(counts.list[[4]][order(names(counts.list[[4]])[1:2])], collapse = ","),
            NA
          ),
          k4 = ifelse(length(counts.list) > 3, max(res.list[[4]][1:2]) - min(res.list[[4]][1:2]),
                      NA),
          stringsAsFactors = F
        )
      )

    return(GENES.df)
  }

########################################################################################################
# Haplotype table to plot tables
#
# \code{parseHapTab} Parse the haplotype table for each panel in the haplotype plot
#
# @param    hap_table             haplotype summary table
# @param    chain                 the Ig chain: IGH,IGK,IGL. Default is IGH.
# @param    hapBy_alleles         Alleles columns haplotyped by
#
# @return   list of data frames for plotting
#
parseHapTab <-
  function(hap_table,
           chain = c("IGH", "IGK", "IGL", "TRB", "TRA"),
           count_df = TRUE,
           sample_name,
           hapBy_cols,
           hapBy_alleles) {
    if (missing(chain)) {
      chain = "IGH"
    }
    chain <- match.arg(chain)

    #hap_table <- data.frame(lapply(hap_table, as.character), stringsAsFactors = FALSE)
    #hap_table$alleles <-  sapply(gsub("[.][0-9]","",hap_table$alleles), function(a) ifelse(nchar(a)==1,paste0("0",a),a))
    #sample_name <- unique(hap_table$subject)
    # id_GENE_col <- which(names(hap_table)==gene)
    # hapBy_col_id <- c(id_GENE_col+1,id_GENE_col+2)
    # hapBy_cols = names(hap_table)[hapBy_col_id]
    # hapBy_alleles = gsub("_", "*", hapBy_cols)
    count.df <-
      setNames(
        data.frame(matrix(ncol = 6, nrow = 0), stringsAsFactors = FALSE),
        c("subject", "gene", "hapBy", "count", "alleles", "count2")
      )

    if (count_df) {
      count.df <- data.table::rbindlist(lapply(1:2, function(panel) {
        panel.alleles <- hap_table[[hapBy_cols[panel]]]

        return(data.table::rbindlist(lapply(1:length(panel.alleles), function(i) {
          if (panel.alleles[i] == "Unk" | panel.alleles[i] == "NR") {
            return(
              data.frame(
                subject = sample_name,
                gene = hap_table$gene[i],
                hapBy = hapBy_alleles[panel],
                count = 0,
                stringsAsFactors = FALSE
              )
            )
          } else {
            if (panel.alleles[i] == "Del") {
              return(
                data.frame(
                  subject = sample_name,
                  gene = hap_table$gene[i],
                  hapBy = hapBy_alleles[panel],
                  count = as.numeric(strsplit(hap_table$counts1[i], ",")[[1]][panel]),
                  stringsAsFactors = FALSE
                )
              )
            } else {
              alleles <- strsplit(panel.alleles[i], ",")[[1]]
              return(data.table::rbindlist(lapply(1:length(alleles), function(j) {
                count_id <-
                  which(strsplit(hap_table$alleles[i], ',')[[1]] == alleles[j])
                return(
                  data.frame(
                    subject = sample_name,
                    gene = paste0(hap_table$gene[i], "*", alleles[j]),
                    hapBy = hapBy_alleles[panel],
                    count = as.numeric(strsplit(hap_table[[paste0("counts", count_id)]][i], ",")[[1]][panel]),
                    stringsAsFactors = FALSE
                  )
                )
              })))
            }
          }
        })))
      })) %>% as.data.frame()

      count.df$alleles <-
        sapply(strsplit(as.character(count.df$gene), "*", fixed = T), "[", 2)
      count.df$alleles[is.na(count.df$alleles)] <- "01"  # Mock allele
      count.df$alleles <-
        factor(count.df$alleles, levels = c(sort(unique(
          count.df$alleles
        )), "NA"))
      count.df$gene <-
        sapply(strsplit(as.character(count.df$gene), "*", fixed = T), "[", 1)

      ## TO visualy make coutns of 1 not look like 0 , one is added
      count.df$count2 <-
        ifelse(
          count.df$hapBy == hapBy_alleles[1],
          -1 * log10(as.numeric(count.df$count) + 1),
          log10(as.numeric(count.df$count) + 1)
        )
      count.df$count2[count.df$count2 == Inf |
                        count.df$count2 == -Inf] <- 0
    }
    # K values panels data frame
    panel1.alleles <- hap_table[[hapBy_cols[1]]]
    # minimum of Ks if there is more than one allele

    hap_table[is.na(hap_table)] <- Inf

    panel1 <- sapply(1:length(panel1.alleles), function(i) {
      if (panel1.alleles[i] == "Unk" |
          panel1.alleles[i] == "Del" | panel1.alleles[i] == "NR") {
        min(as.numeric(hap_table[i, paste0("k", 1:4)]), na.rm = T)
      } else {
        min(as.numeric(hap_table[i, paste0("k", match(unlist(strsplit(
          panel1.alleles[i], ","
        )), unlist(strsplit(
          as.character(hap_table$alleles[i]), ","
        ))))]),
        na.rm = T)
      }
    })
    panel1[panel1 == Inf] <- "NA"

    panel2.alleles <- hap_table[[hapBy_cols[2]]]
    # minimum of Ks if there is more than one allele
    panel2 <- sapply(1:length(panel2.alleles), function(i) {
      if (panel2.alleles[i] == "Unk" |
          panel2.alleles[i] == "Del" | panel2.alleles[i] == "NR") {
        min(as.numeric(hap_table[i, paste0("k", 1:4)]), na.rm = T)
      } else {
        min(as.numeric(hap_table[i, paste0("k", match(unlist(strsplit(
          panel2.alleles[i], ","
        )), unlist(strsplit(
          as.character(hap_table$alleles[i]), ","
        ))))]))
      }
    })
    panel2[panel2 == Inf] <- "NA"

    kval.df <-
      data.frame(
        subject = sample_name,
        gene = c(hap_table$gene, hap_table$gene),
        k = c(panel1, panel2),
        hapBy = c(rep(hapBy_alleles[1], length(panel1)), rep(hapBy_alleles[2],
                                                             length(panel2))),
        stringsAsFactors = FALSE
      )

    bins_k <-
      cut(
        as.numeric(kval.df$k[kval.df$k != "NA"]),
        c(0, 1, 2, 3, 4, 5, 10, 20, 50, Inf),
        include.lowest = T,
        right = F
      )
    K_GROUPED <- gsub(",", ", ", levels(bins_k))
    kval.df$K_GROUPED[kval.df$k != "NA"] <- K_GROUPED[bins_k]
    kval.df$K_GROUPED[kval.df$k == "NA"] <- "NA"
    kval.df$K_GROUPED <-
      factor(kval.df$K_GROUPED, levels = c("NA", K_GROUPED))


    # Alleles panel data frame
    geno.df <-
      data.frame(
        mapply(c, hap_table[, c("subject", "gene", hapBy_cols[1])], hap_table[, c("subject", "gene", hapBy_cols[2])]),
        hapBy = c(rep(hapBy_alleles[1], nrow(hap_table)), rep(hapBy_alleles[2], nrow(hap_table))),
        stringsAsFactors = F
      )
    names(geno.df)[3] <- "alleles"
    geno.df <-
      splitstackshape::cSplit(
        geno.df,
        "alleles",
        sep = ",",
        direction = "long",
        fixed = T,
        type.convert = F
      )
    parsed_hap_table <-
      list(geno.df = geno.df,
           kval.df = kval.df,
           count.df = count.df)

  }

########################################################################################################
# Haplotype table to plot tables
#
# \code{parseHapTab} Parse the haplotype table for each panel in the haplotype plot
#
# @param    hap_table             haplotype summary table
# @param    chain                 the Ig chain: IGH,IGK,IGL. Default is IGH.
# @param    hapBy_alleles         Alleles columns haplotyped by
#
# @return   list of data frames for plotting
#
# parseHapTab_v2 <- function(hap_table, chain = c("IGH", "IGK", "IGL"), sample_name, hapBy_cols, hapBy_alleles) {
#
#   if (missing(chain)) {
#     chain = "IGH"
#   }
#   chain <- match.arg(chain)
#
#   hap_table[paste0("K", 1:4)][is.na(hap_table[paste0("K", 1:4)])] <- Inf
#
#   count.df <- do.call(rbind,
#                       lapply(1:2, function(panel){
#                         panel.alleles <- hap_table[[hapBy_cols[panel]]]
#                         return(
#                           do.call(rbind,
#                                   lapply(1:length(panel.alleles), function(i){
#                                     if (panel.alleles[i] %in% c("Unk","NR","Del")){
#                                       return(c(sample_name, hap_table$gene[i], hapBy_alleles[panel],
#                                                min(as.numeric(strsplit(hap_table$counts1[i],",")[[1]])),
#                                                "01", # Mock allele
#                                                min(as.numeric(hap_table[i, paste0("K", 1:4)]), na.rm = T)
#                                       ))
#                                     }else{
#                                       alleles <- strsplit(panel.alleles[i], ",")[[1]]
#                                       k <- min(as.numeric(hap_table[i, paste0("K", match(unlist(strsplit(panel.alleles[i], ",")),
#                                                                                           unlist(strsplit(hap_table$alleles[i], ","))))]),
#                                                na.rm = T)
#                                       return(do.call(rbind, lapply(1:length(alleles), function(j){
#                                         count_id <- which(strsplit(hap_table[i,'alleles'],',')[[1]]==alleles[j])
#                                         return(c(sample_name, hap_table$gene[i],
#                                                  hapBy_alleles[panel],
#                                                  as.numeric(strsplit(hap_table[i,paste0("COUNTS", count_id)], ",")[[1]][panel]),
#                                                  alleles[j],k
#                                         ))
#                                       })))
#                                     }
#                                   }
#                                   )))
#                       }))%>% as.data.frame(stringsAsFactors = FALSE) %>% `colnames<-`(c(subject, gene, "hapBy", "COUNT", "alleles","K"))
#
#   # K values panels data frame
#   kval.df <- count.df[,c(subject,gene,"hapBy","K")] %>% dplyr::distinct()
#
#   # Remove K
#   count.df <- count.df[,-6]
#   # Sort count alleles
#   count.df$alleles <- factor(count.df$alleles, levels = c(sort(unique(count.df$alleles)), "NA"))
#
#   # Turn counts to numeric
#   count.df$COUNT <- as.numeric(count.df$COUNT)
#
#   ## TO visualy make coutns of 1 not look like 0 , one is added
#   count.df$COUNT2 <- ifelse(count.df$hapBy == hapBy_alleles[1], -1 * log10(as.numeric(count.df$COUNT) + 1),
#                             log10(as.numeric(count.df$COUNT) + 1))
#   count.df$COUNT2[count.df$COUNT2 == Inf | count.df$COUNT2 == -Inf] <- 0
#
#   # Bin K values
#   kval.df$K[kval.df$K == Inf] <- "NA"
#
#   bins_k <- cut(as.numeric(kval.df$K[kval.df$K!="NA"]), c(0, 1, 2, 3, 4, 5, 10, 20, 50, Inf), include.lowest = T, right = F)
#   K_GROUPED <- gsub(",", ", ", levels(bins_k))
#   kval.df$K_GROUPED[kval.df$K!="NA"] <- K_GROUPED[bins_k]
#   kval.df$K_GROUPED[kval.df$K=="NA"] <- "NA"
#   kval.df$K_GROUPED <- factor(kval.df$K_GROUPED, levels = c("NA", K_GROUPED))
#
#
#   # Alleles panel data frame
#   geno.df <- data.frame(mapply(c,hap_table[, c(subject, gene, hapBy_cols[1])],hap_table[, c(subject, gene, hapBy_cols[2])]),
#                         hapBy = c(rep(hapBy_alleles[1], nrow(hap_table)),rep(hapBy_alleles[2], nrow(hap_table))), stringsAsFactors = F)
#   names(geno.df)[3] <- "alleles"
#   geno.df <- tidyr::separate_rows(geno.df, "alleles", sep = ",")
#   parsed_hap_table <- list(geno.df = geno.df, kval.df = kval.df, count.df = count.df)
#
# }

########################################################################################################
# Sort data frame by genes
#
# \code{sortDFByGene} Sort the \code{data.frame} by the genes names or position. For sorting by gene names the \code{sortAlleles} function by TIgGER is used.
# For sorting by position the defualt package gene location list is used.
#
# @param    DATA                 data frame to sort
# @param    chain                the Ig chain: IGH,IGK,IGL. Default is IGH.
# @param    genes_order           A vector of the genes by the desired order. Defualt is by GENE.loc
# @param    removeIGH            if TRUE, 'IGH'\'IGK'\'IGL' prefix is removed from gene names.
#
# @return   sorted \code{data.frame}
#
sortDFByGene <-
  function(DATA,
           chain = c("IGH", "IGK", "IGL", "TRB", "TRA"),
           genes_order = NULL,
           removeIGH = FALSE,
           geno = FALSE,
           peseudo_remove = F) {
    if (missing(chain)) {
      chain = "IGH"
    }
    chain <- match.arg(chain)

    if (peseudo_remove) {
      DATA <- DATA[!grepl("OR|NL", DATA$gene), ]
      DATA <- DATA[!(DATA$gene %in% PSEUDO[[chain]]), ]
    }

    DATA$order <-
      fastmatch::fmatch(DATA$gene, genes_order)
    DATA <- stats::na.omit(DATA, cols = "order")
    DATA <- DATA[order(DATA$order),]

    if (removeIGH) {
      GENE.loc.tmp <- gsub(chain, "", genes_order)
      DATA$gene <- gsub(chain, "", DATA$gene)
      DATA$gene <- factor(DATA$gene, levels = rev(GENE.loc.tmp))
      if (!geno)
        DATA$hapBy <- gsub(chain, "", DATA$hapBy)
    } else{
      DATA$gene <- factor(DATA$gene, levels = rev(genes_order))
    }

    return(DATA)
  }

########################################################################################################
# Calculate Jaacardian distance for haplotypes
#
# \code{calcJacc} Takes as an input two haplotypes and calculates the Jaacardian distance.
#
# @param   vec1A           chromosome A haplotype for first individual.
# @param   vec1B           chromosome B haplotype for first individual.
# @param   vec2A           chromosome A haplotype for second individual.
# @param   vec2B           chromosome B haplotype for second individual.
# @param   method          the method to be used for calculating. pooled - All alleles and all genes taken together (assuming that all genes appear and ordered the same in both vectors)
# geneByGene - For each gene separetly and then calculates average distance.
# @param  naRm             if 'TRUE' ingnores genes from both samples in which there is no data, else the Jaccardian distance for those gene defined as 1 (i.e the maximal distance)
# If both are NAs will remove it either way.
# @param   Kweight         if 'TRUE' the Jaacardian distance is weighted by the K value of the haplotype.
# @param   k1A             chromosome A haplotype K value for first individual.
# @param   k1B             chromosome B haplotype K value for first individual.
# @param   k2A             chromosome A haplotype K value for second individual.
# @param   k2B             chromosome B haplotype K value for second individual.
#
# @return   Jaacardian distance value
#
distJACC <- function(vecA, vecB, naRm = TRUE) {
  if ((!is.na(vecA) &
       !is.na(vecB)) &
      (!vecA %in% c("Unk", "NR") & !vecB %in% c("Unk", "NR"))) {
    v1 <- unlist(strsplit(vecA, ","))
    v2 <- unlist(strsplit(vecB, ","))

    if ((any(grepl("_[0-9][0-9]", v1)) |
         any(grepl("_[0-9][0-9]", v2))) &
        (length(intersect(v1, v2)) != length(unique(c(v1, v2))))) {
      inter = 0
      tot = length((c(v1, v2)))
      for (i in v1) {
        v1_n <- unlist(strsplit(i, "_"))
        for (ii in v2) {
          v2_n <- unlist(strsplit(ii, "_"))
          if (length(intersect(v1_n, v2_n)) != 0) {
            inter = inter + 1
            tot = tot - 1
          }

        }

      }
      dist <- inter / tot
    } else
      dist <- length(intersect(v1, v2)) / length(unique(c(v1, v2)))
  } else {
    if (naRm) {
      dist <- NA
    } else {
      if ((is.na(vecA) &
           is.na(vecB)) ||
          (vecA %in% c("Unk", "NR") & vecB %in% c("Unk", "NR"))) {
        dist <- NA
      } else {
        dist <- 0

      }
    }

  }

}

calcJacc <-
  function(vec1A,
           vec1B,
           vec2A,
           vec2B,
           method = c("pooled", "geneByGene"),
           naRm = TRUE,
           Kweight = FALSE,
           k1A,
           k1B,
           k2A,
           k2B,
           special_jacc = FALSE) {
    vec1A <- as.character(vec1A)
    vec1B <- as.character(vec1B)
    vec2A <- as.character(vec2A)
    vec2B <- as.character(vec2B)
    if (method == "geneByGene") {
      # Calculate Jaccardian distance for each gene and then
      jacc <- c()
      jacc <- sapply(1:length(vec1A), function(i) {
        distA <- distJACC(vec1A[i], vec2A[i], naRm)
        distB <- distJACC(vec1B[i], vec2B[i], naRm)
        jacc <-
          c(jacc, rowMeans(data.frame(distA, distB), na.rm = TRUE))
      })

      if (Kweight) {
        k1A[k1A == Inf] <- 0
        k2A[k2A == Inf] <- 0
        k1B[k1B == Inf] <- 0
        k2B[k2B == Inf] <- 0
        KavgA <- sapply(1:length(k1A), function(x) {
          mean(c(k1A[x], k2A[x]), na.rm = T)
        })
        if (is.list(KavgA)) {
          KavgA[sapply(KavgA, is.null)] <- NA
          KavgA <- unlist(KavgA)
        }
        KavgB <- sapply(1:length(k1B), function(x) {
          mean(c(k1B[x], k2B[x]), na.rm = T)
        })
        if (is.list(KavgB)) {
          KavgB[sapply(KavgB, is.null)] <- NA
          KavgB <- unlist(KavgB)
        }

        Kavg <- rowMeans(cbind(KavgA, KavgB), na.rm = T)
        jacc <- weighted.mean(jacc, Kavg, na.rm = T)
        return(1 - jacc)
      }
      return(mean(1 - jacc, na.rm = T))
    }
    if (method == "pooled") {
      v1 <- unlist(sapply(1:length(vec1A), function(x) {
        paste(paste0("G", x), unlist(strsplit(vec1A[[x]], ",")), sep = "_")
      }))
      v1 <- c(v1, unlist(sapply(1:length(vec1B), function(x) {
        paste(paste0("G", x), unlist(strsplit(vec1B[[x]], ",")), sep = "_")
      })))
      v2 <- unlist(sapply(1:length(vec2A), function(x) {
        paste(paste0("G", x), unlist(strsplit(vec2A[[x]], ",")), sep = "_")
      }))
      v2 <- c(v2, unlist(sapply(1:length(vec2B), function(x) {
        paste(paste0("G", x), unlist(strsplit(vec2B[[x]], ",")), sep = "_")
      })))

      v1 <- v1[-grep("NA", v1, fixed = T)]
      v2 <- v2[-grep("NA", v2, fixed = T)]

      jacc <- length(intersect(v1, v2)) / length(unique(c(v1, v2)))
      return(1 - jacc)
    }
  }

########################################################################################################
# Binom test for deletion infrence
#
# \code{binom_test_deletion} Infer deletion from binomial test
#
# @param    GENE.usage.df          a data frame of relative gene usage
# @param    cutoff                 a data frame of relative gene usage
# @param    p.val.cutoff           a p value cutoff to detect deletion
# @param    chain                  the IG chain: IGH,IGK,IGL. Default is IGH.
# @param    GENE.loc.IG            the genes by location
#
# @return   data frame with the binomial test results
#
binomTestDeletion <-
  function(GENE.usage.df,
           cutoff = 0.001,
           p.val.cutoff = 0.01,
           chain = "IGH",
           GENE.loc.IG) {
    GENE.usage.df$pval <- sapply(1:nrow(GENE.usage.df), function(i) {
      if ((GENE.usage.df$frac[i] < cutoff) &
          GENE.usage.df$min_frac[i] != Inf) {
        return(
          binom.test(
            x = round(GENE.usage.df$frac[i] * GENE.usage.df$NREADS[i]),
            n = GENE.usage.df$NREADS[i],
            p = GENE.usage.df$min_frac[i]
          )$p.value
        )
      }
      if (GENE.usage.df$min_frac[i] == Inf) {
        return(0)
      } else {
        return(1)
      }
    })

    ### P.binom to detect deletion or cnv
    GENE.usage.df$foradj <-
      sapply(1:nrow(GENE.usage.df), function(i) {
        if (GENE.usage.df$frac[i] < cutoff &
            GENE.usage.df$min_frac[i] != Inf) {
          return(paste0(GENE.usage.df$gene[i], "_", 0))
        }
        if (GENE.usage.df$min_frac[i] == Inf) {
          return(paste0(GENE.usage.df$gene[i], "_", 1))
        } else {
          return(paste0(GENE.usage.df$gene[i], "_", 2))
        }
      })
    GENE.usage.df <-
      GENE.usage.df %>% dplyr::group_by(.data$foradj) %>% mutate(pval_adj = p.adjust(.data$pval, method = "BH"))



    GENE.usage.df$col <- sapply(1:nrow(GENE.usage.df), function(i) {
      if (GENE.usage.df$pval_adj[i] <= p.val.cutoff) {
        if (GENE.usage.df$frac[i] < cutoff &
            GENE.usage.df$min_frac[i] != Inf) {
          return("Deletion")
        }
        if (GENE.usage.df$min_frac[i] == Inf) {
          return("NA")
        }
      } else {
        return("No Deletion")
      }
    })


    GENE.usage.df$gene <-
      factor(GENE.usage.df$gene, levels = GENE.loc.IG)
    GENE.usage.df$col <-
      factor(GENE.usage.df$col, levels = c("Deletion", "No Deletion", "NA"))

    return(GENE.usage.df)
  }

########################################################################################################
# sample size for binom deletion test
#
# \code{sample_size} cumputes the number of single assignments per subject
#
# @param    clip_db                a \code{data.frame} in AIRR format.
# @param    CALL                   the name of the gene call for which to compute the sample size.
#
# @return   list of subjects and sample sizes.
#
sample_size <- function(clip_db, CALL) {
  clip_db <-
    clip_db %>% select(.data$subject,!!as.name(CALL)) %>% dplyr::group_by(.data$subject) %>%
    dplyr::mutate(SAMPLE.SIZE = sum(!grepl(',',!!as.name(CALL)))) %>% slice(1) %>%
    dplyr::ungroup() %>% dplyr::select(.data$subject, .data$SAMPLE.SIZE)
  SAMPLE.SIZE <- clip_db$SAMPLE.SIZE
  names(SAMPLE.SIZE) <- unique(clip_db$subject)
  return(SAMPLE.SIZE)
}

########################################################################################################
# Creates the allele color palette for haplotype graphical output
#
# \code{alleleHapPalette} Takes a list of the haplotype alleles and returns the allele color palette.
#
# @param   hap_alleles          a list of the haplotype alleles.
#
# @return   Haplotype allele color palette
#
alleleHapPalette <- function(hap_alleles, NRA = TRUE) {
  Alleles <- grep("[012]",
                  unique(hap_alleles),
                  value = T,
                  perl = T)
  AlleleCol.tmp <-
    sort(unique(sapply(strsplit(Alleles, "_"), "[", 1)))
  ALLELE_PALETTE_specific <- ALLELE_PALETTE
  if(any(!AlleleCol.tmp %in% names(ALLELE_PALETTE))){
    missing_alleles <- AlleleCol.tmp[which(!AlleleCol.tmp %in% names(ALLELE_PALETTE))]
    na_id <- which(names(ALLELE_PALETTE)=="NA")
    names(ALLELE_PALETTE_specific)[na_id[1:length(missing_alleles)]] <- missing_alleles
  }
  tmp.col <- ALLELE_PALETTE_specific[AlleleCol.tmp]


  novels <- grep("_", Alleles, value = T)
  if (length(novels) > 0) {
    novels.col <- ALLELE_PALETTE_specific[sapply(strsplit(novels, "_"), "[", 1)]
    names(novels.col) <- novels
    alleles.comb <-
      c(tmp.col, novels.col)[order(names(c(tmp.col, novels.col)))]
  } else {
    alleles.comb <- c(tmp.col)[order(names(c(tmp.col)))]

  }

  AlleleCol <-
    names(c(
      alleles.comb,
      Unk = "#dedede",
      Del = "#6d6d6d",
      NR = "#000000",
      NRA = "#fbf7f5"
    ))
  names(AlleleCol) <-
    c(
      alleles.comb,
      Unk = "#dedede",
      Del = "#6d6d6d",
      NR = "#000000",
      NRA = "#fbf7f5"
    )
  rm_allele <- function(allele, alleles, AlleleCol) {
    if (!allele %in% alleles) {
      id <- which(allele == AlleleCol)
      return(AlleleCol[-id])
    }
    return(AlleleCol)
  }
  AlleleCol <- rm_allele("NR", hap_alleles, AlleleCol)
  AlleleCol <- rm_allele("Del", hap_alleles, AlleleCol)
  AlleleCol <- rm_allele("Unk", hap_alleles, AlleleCol)
  AlleleCol <- rm_allele("NRA", hap_alleles, AlleleCol)


  transper <- sapply(AlleleCol, function(x) {
    if (grepl("_", x)) {
      mom_allele <- strsplit(x, "_")[[1]][1]
      all_novel <-
        grep(paste0(mom_allele, "_"), AlleleCol, value = T)
      if (length(all_novel) == 1) {
        return(0.5)
      }
      if (length(all_novel) == 2) {
        m = which(all_novel == x)
        return(ifelse(m == 1, 0.6, 0.3))
      }
      if (length(all_novel) == 3) {
        m = which(all_novel == x)
        if (m == 1) {
          return(0.6)
        }
        return(ifelse(m == 2, 0.4, 0.2))
      }
      if (length(all_novel) > 9) {
        m = which(all_novel == x)
        if (m == 1) {
          return(1)
        }
        return(1 - m / 20)
      }
      if (length(all_novel) > 3) {
        m = which(all_novel == x)
        if (m == 1) {
          return(0.85)
        }
        return(0.85 - m / 10)
      }
    } else
      (1)
  })
  names(transper) <- AlleleCol

  # remove 'mother' allele if added (when there is no germline allele but there is a novel)

  special <-
    c("Unk", "Del", "NR", "NRA")[c("Unk", "Del", "NR", "NRA") %in% AlleleCol]

  AlleleCol <-
    AlleleCol[AlleleCol %in% c(sort(grep(
      "[012]",
      unique(hap_alleles),
      value = T,
      perl = T
    )), special)]

  transper <- transper[names(transper) %in% AlleleCol]

  return(list(transper = transper, AlleleCol = AlleleCol))
}

########################################################################################################
# Creates the non reliable allele text annotation for plots
#
# \code{nonReliableAllelesText} Takes the haplotype data frame
#
# @param   hap_table          a data frame of the haplotypes.
#
# @return   Non reliable alleles text data frame for plots annotation.
#
nonReliableAllelesText <-
  function(non_reliable_alleles_text, size = 4) {
    if (nrow(non_reliable_alleles_text) != 0) {
      non_reliable_alleles_text$text <- non_reliable_alleles_text$alleles
      non_reliable_alleles_text$pos <-
        ifelse(non_reliable_alleles_text$freq == 1, 0.5, 0.25)
      non_reliable_alleles_text <-
        non_reliable_alleles_text %>% ungroup() %>% dplyr::group_by(.data$gene, .data$subject, .data$hapBy) %>%
        mutate(
          pos = .data$pos + ifelse(
            dplyr::row_number() == 2,
            dplyr::row_number() - 1.5,
            dplyr::row_number() - 1
          )
        )
      non_reliable_alleles_text$size <-
        sapply(1:nrow(non_reliable_alleles_text), function(i) {
          if (non_reliable_alleles_text$freq[i] == 1) {
            if (length(strsplit(non_reliable_alleles_text$text[i], "_")[[1]]) < 5) {
              return(size)
            } else {
              return(size - 1)
            }
          } else {
            if (length(strsplit(non_reliable_alleles_text$text[i], "_")[[1]]) < 5) {
              return(size - 1)
            } else {
              return(size - 2)
            }
          }
        })

      non_reliable_alleles_text$alleles[grep("[0-9][0-9]_[0-9][0-9]",
                                             non_reliable_alleles_text$alleles)] <- "NRA"
      return(non_reliable_alleles_text)
    } else {
      return(setNames(
        data.frame(matrix(ncol = 8, nrow = 0)),
        c(
          "gene",
          "alleles",
          "hapBy",
          "n",
          "freq",
          "text",
          "pos",
          "size"
        )
      ))
    }
  }

nonReliableAllelesText_V2 <-
  function(non_reliable_alleles_text,
           size = 3,
           map = F) {
    if (nrow(non_reliable_alleles_text) != 0) {
      num_text <-
        sapply(1:length(unique(non_reliable_alleles_text$alleles)), function(i)
          paste0('[*', i, ']'))
      names(num_text) <- unique(non_reliable_alleles_text$alleles)
      non_reliable_alleles_text$text <-
        num_text[non_reliable_alleles_text$alleles]
      non_reliable_alleles_text$text_bottom <-
        paste(num_text[non_reliable_alleles_text$alleles], non_reliable_alleles_text$alleles)
      non_reliable_alleles_text$pos <-
        ifelse(
          non_reliable_alleles_text$freq == 1,
          0.5,
          ifelse(
            non_reliable_alleles_text$freq == 2,
            seq(0.25, 1, by = 0.5)[1:2],
            ifelse(
              non_reliable_alleles_text$freq == 3,
              seq(0.165, 1, by = 0.33)[1:3],
              seq(0.125, 1, by = 0.25)[1:4]
            )
          )
        )
      non_reliable_alleles_text$size = size
      if (!map) {
        non_reliable_alleles_text <-
          non_reliable_alleles_text %>% dplyr::ungroup() %>% dplyr::group_by(.data$gene, .data$subject, .data$hapBy) %>%
          dplyr::mutate(
            pos2 = .data$pos + 1 + ifelse(
              dplyr::row_number() == 2,
              dplyr::row_number() - 1.5,
              dplyr::row_number() - 1
            )
          )
      }
      else{
        non_reliable_alleles_text <-
          non_reliable_alleles_text %>% dplyr::ungroup() %>% dplyr::group_by(.data$gene, .data$subject) %>%
          dplyr::mutate(pos = ifelse(.data$n == 1, 0.5,
                                     ifelse(
                                       .data$n == 2,
                                       seq(0.25, 1, by = 0.5)[1:max(dplyr::row_number())],
                                       ifelse(
                                         .data$n == 3,
                                         seq(0.165, 1, by = 0.33)[1:max(dplyr::row_number())],
                                         seq(0.125, 1, by = 0.25)[1:max(dplyr::row_number())]
                                       )
                                     )))
      }

      non_reliable_alleles_text$alleles[grep("[0-9][0-9]_[0-9][0-9]",
                                             non_reliable_alleles_text$alleles)] <- "NRA"
      return(non_reliable_alleles_text)
    } else {
      if (!map)
        return(setNames(
          data.frame(matrix(ncol = 8, nrow = 0)),
          c(
            "gene",
            "alleles",
            "hapBy",
            "n",
            "freq",
            "text",
            "pos",
            "size"
          )
        ))
      else
        return(setNames(
          data.frame(matrix(ncol = 8, nrow = 0)),
          c("gene", "alleles", "n", "freq", "text", "pos", "size")
        ))
    }
  }

########################################################################################################
# Creates the novel allele text annotation for plots
#
# \code{novelAlleleAnnotation} Takes the haplotype data frame
#
# @param   novel_allele         data frame with the novel allele cordinates.
#
# @return   novel alleles text data frame for plots annotation.
#
novelAlleleAnnotation <-
  function(novel_allele, new_label, size = 3) {
    if (nrow(novel_allele) != 0) {
      novel_allele$text <-
        sapply(new_label[novel_allele$alleles], function(s)
          strsplit(s, '-')[[1]][1])
      novel_allele$text_bottom <-
        paste(new_label[novel_allele$alleles], novel_allele$alleles)
      novel_allele$pos <- ifelse(novel_allele$freq == 1,
                                 1,
                                 ifelse(
                                   novel_allele$freq == 2,
                                   0.5,
                                   ifelse(novel_allele$freq == 3, 0.33, 0.25)
                                 ))
      novel_allele$size = size
      novel_allele <-
        novel_allele %>% dplyr::ungroup() %>% dplyr::group_by(.data$gene, .data$subject, .data$hapBy) %>%
        mutate(pos = ifelse(.data$n == 1, 0.5,
                            ifelse(
                              .data$n == 2,
                              seq(0.25, 1, by = 0.5)[1:max(dplyr::row_number())],
                              ifelse(.data$n == 3, seq(0.165, 1, by = 0.33)[1:max(dplyr::row_number())],
                                     seq(0.125, 1, by = 0.25)[1:max(dplyr::row_number())])
                            )))
      return(novel_allele)
    } else {
      return(setNames(
        data.frame(matrix(ncol = 8, nrow = 0)),
        c(
          "gene",
          "alleles",
          "hapBy",
          "n",
          "freq",
          "text",
          "pos",
          "size"
        )
      ))
    }
  }


########################################################################################################
# Transform character column to numeric
#
asNum <- function(row, na.strings = c(NA, "NA")) {
  na <- row %in% na.strings
  row[na] <- 0
  row2 <- row
  ex_special <-
    !grepl('[_|,|-]|[A-Z]|[0-2][1-9]$', as.character(row))
  numIDX <- grepl('[0-9]*[^,]', as.character(row)) & ex_special
  row2[!numIDX] <- row[!numIDX]
  row2[numIDX] <- as.numeric(row[numIDX])
  row2[na] <- NA_real_
  return(row2)
}

########################################################################################################
# Get the number of unique genes assigned, modified from getSegment function from alakazam
#
getGeneCount <- function (segment_call, sep = ",")
{
  segment_regex <- "((IG[HLK]|TR[ABGD])[VDJ][A-Z0-9\\(\\)]+[-/\\w]*)"

  edge_regex <- paste0("[^", sep, "]*")
  r <- gsub(paste0(edge_regex, "(", segment_regex, ")", edge_regex),
            "\\1",
            segment_call,
            perl = T)
  r <- sapply(strsplit(r, sep), function(x)
    length(unique(x)))
  return(r)
}

########################################################################################################
# Collapse alleles, modified from getSegment and getAlleles functions from alakazam
#
alleleCollapse <-
  function(segment_call,
           sep = ",|_(?![A-Z])",
           collapse = "_",
           withGene = T) {
    r <- gsub("((IG[HLK]|TR[ABGD])[VDJ][A-Z0-9\\(\\)]+[-/\\w]*)[*]",
              "",
              segment_call,
              perl = T)
    r <-
      sapply(strsplit(r, sep, perl = T), function(x)
        paste(unique(x),
              collapse = collapse))
    if (withGene)
      r <- paste0(getGene(segment_call, strip_d = F), "*", r)

    return(r)
  }



########################################################################################################
# Get diagonal line for legend
#
getDigLegend <- function(color) {
  return(ggplotGrob(
    ggplot(data.frame(x = c(1, 2), y = c(3, 4)), aes_string("x", "y")) + geom_abline(
      aes_string(
        colour = "color",
        intercept = 1,
        slope = 1
      ),
      show.legend = T
    ) +
      scale_color_manual(
        values = c("white"),
        name = "lK",
        drop = FALSE
      ) + guides(color = guide_legend(
        override.aes = list(size = 0.5), order = 2
      )) +
      theme(
        legend.justification = "center",
        legend.key = element_rect(fill = "gray"),
        legend.position = "bottom"
      )
  ))
}

########################################################################################################
# Split lines of short reads assignments
#
# The \code{splitlines} function sliplits the text by line width
#
# @param    bottom_annot         annotation text.
# @param    line_width           the line width allowed.
#
# @return
# Seperated text to lines
#
splitlines <- function(bottom_annot, line_width = 60) {
  if (line_width <= max(sapply(bottom_annot, nchar))) {
    line_width = max(sapply(bottom_annot, nchar))
    print(paste0("Set line width to ", line_width))
  }
  collapsed_annot <- paste(bottom_annot, collapse = ", ")
  L <- nchar(collapsed_annot)
  if (L < line_width)
    return(collapsed_annot)
  vec_annot <- substring(collapsed_annot, 1:L, 1:L)
  ind <- grep("[", vec_annot, fixed = TRUE)
  aligned_text <- NULL
  #i<-line_width
  i_previous <- 1
  while (i_previous <= (L - line_width)) {
    temp <- findInterval(line_width * (length(aligned_text) + 1) + 1, ind)
    aligned_text <-
      c(aligned_text,
        substring(collapsed_annot, i_previous, ind[temp] - 1))
    i_previous <- ind[temp]
  }
  aligned_text <-
    c(aligned_text, substring(collapsed_annot, i_previous, L))
  return(aligned_text)
}
########################################################################################################
# Write text annotations for heatmap graph
#
# \code{Write_text} takes values for plotting text on heatmap
#
# @param   NR           number of rows.
# @param   NC           number of columns.
# @param   I            row index.
# @param   J            column index.
# @param   ALLELE       allele index in the individual gene box.
# @param   N_alleles    number of alleles for individual in gene box.
# @param   TEXT         annotation text.
#
# @return   plotting text annotation.
# @export
Write_text <- function(NR, NC, I, J, ALLELE, N_alleles, TEXT, ...) {
  STEP_X <- 1 / (NC - 1)
  STEP_Y <- 1 / (NR - 1)
  text(STEP_X * J - STEP_X / 2 + STEP_X * 12 / N_alleles * (ALLELE - 1 /
                                                              2),
       STEP_Y * I,
       TEXT,
       ...)
}
########################################################################################################
# Draw lk value lines on heatmap
#
# \code{draw_segment} takes values for plotting text on heatmap
#
# @param   NR           number of rows.
# @param   NC           number of columns.
# @param   I            row index.
# @param   J            column index.
#
# @return   plotting lk lines.
# @export
draw_segment <- function(NR, NC, I, J, ...) {
  STEP_X <- 1 / (NC - 1)
  STEP_Y <- 1 / (NR - 1)
  points(c(STEP_X * (J - 0.5), STEP_X * (J + 1.5)),
         c(STEP_Y * (I), STEP_Y * (I + 0.5)), type = "l", ...)
  points(c(STEP_X * (J - 0.5), STEP_X * (J + 3.5)),
         c(STEP_Y * (I - 0.5), STEP_Y * (I + 0.5)), type = "l", ...)
  points(c(STEP_X * (J + 1.5), STEP_X * (J + 5.5)),
         c(STEP_Y * (I - 0.5), STEP_Y * (I + 0.5)), type = "l", ...)
  points(c(STEP_X * (J + 3.5), STEP_X * (J + 7.5)),
         c(STEP_Y * (I - 0.5), STEP_Y * (I + 0.5)), type = "l", ...)
  points(c(STEP_X * (J + 5.5), STEP_X * (J + 9.5)),
         c(STEP_Y * (I - 0.5), STEP_Y * (I + 0.5)), type = "l", ...)
  points(c(STEP_X * (J + 7.5), STEP_X * (J + 11.5)),
         c(STEP_Y * (I - 0.5), STEP_Y * (I + 0.5)), type = "l", ...)
  points(c(STEP_X * (J + 9.5), STEP_X * (J + 11.5)),
         c(STEP_Y * (I - 0.5), STEP_Y * (I)), type = "l", ...)
}
########################################################################################################
# finds next dividor for an int number
next_divisor <- function(x, y) {
  if (x > y)
    return(y)
  while (T) {
    if (y %% x == 0)
      return(x)
    x <- x + 1
  }
}

########################################################################################################

haplotype_db_columns <- list(
  "subject" = "c",
  "gene" = "c",
  'alleles' = "c",
  'proirs_row' = "c",
  'proirs_col' = "c",
  'counts1' = "c",
  'k1' = "d",
  'counts2'  = "c",
  'k2' = "d",
  'counts3'  = "c",
  'k3' = "d",
  'counts4'  = "c",
  'k4' = "d"
)

#' Read a Change-O tab-delimited database file
#'
#' \code{readHaplotypeDb} reads a tab-delimited haplotype file created by a createFullHaplotype
#' into a data.frame. Based on readChangeoDb function from alakazam.
#'
#' @param    file       tab-delimited database file output by a Change-O tool.
#'
#' @return   A data.frame of the haplotype file. Columns will be imported as is, except for
#'           the following columns which will be explicitly converted into character
#'           values:
#'           \itemize{
#'             \item  alleles
#'             \item  subject
#'           }
#'
#'
#'
#' @export
readHaplotypeDb <- function(file) {
  # Define types
  header <- names(suppressMessages(readr::read_tsv(file, n_max = 1)))
  types <-
    do.call(readr::cols, haplotype_db_columns[intersect(names(haplotype_db_columns), header)])

  # Read file
  db <-
    suppressMessages(readr::read_tsv(file, col_types = types, na = c("", "NA", "None")))

  # check alleles column, if numeric re-create the column
  if(any(grepl(".", db[["alleles"]], fixed = T))){

    db[["alleles"]] <- sapply(1:nrow(db), function(i){
      alleles <- grep("[0-9]",as.list(db[i,3:4]),value=T)
      alleles <- unique(strsplit(alleles, split = ",")[[1]])
      paste0(alleles, collapse = ",")
    })

  }
  return(as.data.frame(db))
}
