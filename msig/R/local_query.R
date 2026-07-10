#' Query gene set names from local data
#'
#' @param geneset one sql format character for exact match
#' @param description one sql format character for exact match
#' @param collection one sql format character for exact match
#' @param sub_collection one sql format character for exact match
#' @param organism one sql format character for exact match
#' @param contributor one sql format character for exact match
#' @param contributor_org one sql format character for exact match
#' @param author one sql format character for exact match
#' @param chip one sql format character for exact match
#' @param gene one sql format character for exact match
#'
#' @param geneset_fuzzy fuzzy match
#' @param collection_fuzzy fuzzy match
#' @param sub_collection_fuzzy fuzzy match
#' @param organism_fuzzy fuzzy match
#' @param contributor_fuzzy fuzzy match
#' @param contributor_org_fuzzy fuzzy match
#' @param author_fuzzy fuzzy match
#' @param chip_fuzzy fuzzy match
#' @param gene_fuzzy fuzzy match
#'
#' @return one dataframe with attribute of msig_local.
#' @export
#' @examples
#' x <- local_msig('IMMUNE_RESPONSE')
#' x <- local_msig('IMMUNE_RESPONSE|IMMUNE_SYSTEM_PROCESS')
local_msig <- function(geneset = NULL,
                       description = NULL,
                       collection = NULL,
                       sub_collection = NULL,
                       organism = NULL,
                       contributor = NULL,
                       contributor_org = NULL,
                       author = NULL,
                       chip = NULL,
                       gene = NULL,

                       geneset_fuzzy = NULL,
                       collection_fuzzy = NULL,
                       sub_collection_fuzzy = NULL,
                       organism_fuzzy = NULL,
                       contributor_fuzzy = NULL,
                       contributor_org_fuzzy = NULL,
                       author_fuzzy = NULL,
                       chip_fuzzy = NULL,
                       gene_fuzzy = NULL){
    argl <- list(
        geneset = geneset,
        description = description,
        collection = collection,
        sub_collection = sub_collection,
        organism = organism,
        contributor = contributor,
        contributor_org = contributor_org,
        author = author,
        chip = chip,
        gene_fuzzy=gene_fuzzy,
        gene = gene,

        geneset_fuzzy = geneset_fuzzy,
        collection_fuzzy = collection_fuzzy,
        sub_collection_fuzzy = sub_collection_fuzzy,
        organism_fuzzy = organism_fuzzy,
        contributor_fuzzy = contributor_fuzzy,
        contributor_org_fuzzy = contributor_org_fuzzy,
        author_fuzzy = author_fuzzy,
        chip_fuzzy = chip_fuzzy,
        gene_fuzzy=gene_fuzzy,
        gene_fuzzy = gene_fuzzy
    )
    len <- sapply(argl,function(i) length(i))
    if (any(len >1)) stop('arguments must be one character')
    arg <- argl[len == 1]
    for (i in seq_len(length(arg))) {
        arg[[i]] <- do::Trim(arg[[i]])
    }
#####################################################
    hl <- list()

    ####  Exact ####
    if ('geneset' %in% names(arg)){
        key <- strsplit(do::Trim(arg[['geneset']]),' {0,}\\| {0,}')[[1]]
        key <- tolower(key[nchar(key)>0])
        ck <- (tolower(msigdb$standard_name) %in% key) | (tolower(msigdb$systematic_name) %in% key)
        msigdb <- msigdb[ck,]
        hl$standard_name <- key
        hl$systematic_name <- key
    }

    if ('collection' %in% names(arg)){
        key <- strsplit(do::Trim(arg[['collection']]),' {0,}\\| {0,}')[[1]]
        key <- tolower(key[nchar(key)>0])
        ck <- tolower(msigdb$collection) %in% key
        msigdb <- msigdb[ck,]
        hl$collection <- key
    }
    if ('sub_collection' %in% names(arg)){
        key <- strsplit(do::Trim(arg[['sub_collection']]),' {0,}\\| {0,}')[[1]]
        key <- tolower(key[nchar(key)>0])
        ck <- tolower(msigdb$sub_collection) %in% key
        msigdb <- msigdb[ck,]
        hl$sub_collection <- key
    }
    if ('organism' %in% names(arg)){
        key <- strsplit(do::Trim(arg[['organism']]),' {0,}\\| {0,}')[[1]]
        key <- tolower(key[nchar(key)>0])
        ck <- tolower(msigdb$organism) %in% key
        msigdb <- msigdb[ck,]
        hl$organism <- key
    }
    if ('contributor' %in% names(arg)){
        key <- strsplit(do::Trim(arg[['contributor']]),' {0,}\\| {0,}')[[1]]
        key <- tolower(key[nchar(key)>0])
        ck <- tolower(msigdb$contributor) %in% key
        msigdb <- msigdb[ck,]
        hl$contributor <- key
    }
    if ('contributor_org' %in% names(arg)){
        key <- strsplit(do::Trim(arg[['contributor_org']]),' {0,}\\| {0,}')[[1]]
        key <- tolower(key[nchar(key)>0])
        ck <- tolower(msigdb$contributor_org) %in% key
        msigdb <- msigdb[ck,]
        hl$contributor_org <- key
    }
    if ('author' %in% names(arg)){
        key <- strsplit(do::Trim(arg[['author']]),' {0,}\\| {0,}')[[1]]
        key <- tolower(key[nchar(key)>0])
        ck <- tolower(msigdb$author) %in% key
        msigdb <- msigdb[ck,]
        hl$author <- key
    }
    if ('chip' %in% names(arg)){
        key <- strsplit(do::Trim(arg[['chip']]),' {0,}\\| {0,}')[[1]]
        key <- tolower(key[nchar(key)>0])
        ck <- tolower(msigdb$chip) %in% key
        msigdb <- msigdb[ck,]
        hl$chip <- key
    }

    #### fuzzy  ###########
    if ('geneset_fuzzy' %in% names(arg)){
        string <- 'select * from msigdb where %s'
        sql1 <- to_sql(x = arg[['geneset_fuzzy']],name = 'standard_name')
        sql2 <- to_sql(x = arg[['geneset_fuzzy']],name = 'systematic_name')
        sql <- sprintf(string,
                       sprintf('(%s) OR (%s)',sql1,sql2))
        msigdb <- sqldf::sqldf(sql)
        hi <- strsplit(arg[['geneset_fuzzy']],' {0,}[\\|\\&\\(\\)] {0,}')[[1]]
        hi <- hi[nchar(hi)>0]
        hl$standard_name <- c(hl$standard_name,hi)
        hl$systematic_name <- c(hl$systematic_name,hi)

    }

    if ('description' %in% names(arg)){
        string <- 'select * from msigdb where %s'
        sql1 <- to_sql(x = arg[['description']],name = 'description_brief')
        sql2 <- to_sql(x = arg[['description']],name = 'description_full')
        sql <- sprintf(string,
                       sprintf('(%s) OR (%s)',sql1,sql2))
        msigdb <- sqldf::sqldf(sql)
        hi <- strsplit(arg[['description']],' {0,}[\\|\\&\\(\\)] {0,}')[[1]]
        hi <- hi[nchar(hi)>0]
        hl$description_brief <- hi
        hl$description_full <- hi

    }
    if ('collection_fuzzy' %in% names(arg)){
        string <- 'select * from msigdb where %s'
        sql <- to_sql(x = arg[['collection_fuzzy']],name = 'collection')
        sql <- sprintf(string,sql)

        msigdb <- sqldf::sqldf(sql)
        hi <- strsplit(arg[['collection_fuzzy']],' {0,}[\\|\\&\\(\\)] {0,}')[[1]]
        hi <- hi[nchar(hi)>0]
        hl$collection <- c(hl$collection,hi)
    }
    if ('sub_collection_fuzzy' %in% names(arg)){
        string <- 'select * from msigdb where %s'
        sql <- to_sql(x = arg[['sub_collection_fuzzy']],name = 'sub_collection')
        sql <- sprintf(string,sql)
        msigdb <- sqldf::sqldf(sql)

        hi <- strsplit(arg[['sub_collection_fuzzy']],' {0,}[\\|\\&\\(\\)] {0,}')[[1]]
        hi <- hi[nchar(hi)>0]
        hl$sub_collection <- c(hl$sub_collection,hi)
    }
    if ('organism_fuzzy' %in% names(arg)){
        string <- 'select * from msigdb where %s'
        sql <- to_sql(x = arg[['organism_fuzzy']],name = 'organism')
        sql <- sprintf(string,sql)
        msigdb <- sqldf::sqldf(sql)

        hi <- strsplit(arg[['organism_fuzzy']],' {0,}[\\|\\&\\(\\)] {0,}')[[1]]
        hi <- hi[nchar(hi)>0]
        hl$organism <- c(hl$organism,hi)
    }
    if ('contributor_fuzzy' %in% names(arg)){
        string <- 'select * from msigdb where %s'
        sql <- to_sql(x = arg[['contributor_fuzzy']],name = 'contributor')
        sql <- sprintf(string,sql)
        msigdb <- sqldf::sqldf(sql)

        hi <- strsplit(arg[['contributor_fuzzy']],' {0,}[\\|\\&\\(\\)] {0,}')[[1]]
        hi <- hi[nchar(hi)>0]
        hl$contributor <- c(hl$contributor,hi)
    }
    if ('contributor_org_fuzzy' %in% names(arg)){
        string <- 'select * from msigdb where %s'
        sql <- to_sql(x = arg[['contributor_org_fuzzy']],name = 'contributor_org')
        sql <- sprintf(string,sql)
        msigdb <- sqldf::sqldf(sql)

        hi <- strsplit(arg[['contributor_org_fuzzy']],' {0,}[\\|\\&\\(\\)] {0,}')[[1]]
        hi <- hi[nchar(hi)>0]
        hl$contributor_org <- c(hl$contributor_org,hi)
    }
    if ('author_fuzzy' %in% names(arg)){
        string <- 'select * from msigdb where %s'
        sql <- to_sql(x = arg[['author_fuzzy']],name = 'author')
        sql <- sprintf(string,sql)
        msigdb <- sqldf::sqldf(sql)

        hi <- strsplit(arg[['author_fuzzy']],' {0,}[\\|\\&\\(\\)] {0,}')[[1]]
        hi <- hi[nchar(hi)>0]
        hl$author <- c(hl$author,hi)
    }
    if ('chip_fuzzy' %in% names(arg)){
        string <- 'select * from msigdb where %s'
        sql <- to_sql(x = arg[['chip_fuzzy']],name = 'chip')
        sql <- sprintf(string,sql)
        msigdb <- sqldf::sqldf(sql)

        hi <- strsplit(arg[['chip_fuzzy']],' {0,}[\\|\\&\\(\\)] {0,}')[[1]]
        hi <- hi[nchar(hi)>0]
        hl$chip <- c(hl$chip,hi)
    }



    if ('gene_fuzzy' %in% names(arg)){
        string <- 'select * from msigdb where %s'
        sql <- to_sql(x = arg[['gene_fuzzy']],name = 'gene')
        sql <- sprintf(string,sql)
        msigdb <- sqldf::sqldf(sql)

        hi <- strsplit(arg[['gene_fuzzy']],' {0,}[\\|\\&\\(\\)] {0,}')[[1]]
        hi <- hi[nchar(hi)>0]
        hl$gene <- hi
    }

    if ('gene' %in% names(arg)){
        msigdb$gene <- gsub(' {0,}[\\|,] {0,}',',',msigdb$gene)
        msigdb$gene <- paste0(',',msigdb$gene,',')
        string <- 'select * from msigdb where %s'
        sql <- to_sql(x = arg[['gene']],name = 'gene',
                      before=',',after=',')
        sql <- sprintf(string,sql)
        msigdb <- sqldf::sqldf(sql)
        symbol <- strsplit(msigdb$gene,' {0,}, {0,}')
        gene <- arg[['gene']] |>
            strsplit('\\||\\&|\\(\\)') |>
            unlist() |>
            unique() |>
            do::Trim()
        msigdb$gene <- sapply(symbol, function(i) paste0(set::and(i,gene),collapse = ','))
        hi <- strsplit(arg[['gene']],' {0,}[\\|\\&\\(\\)] {0,}')[[1]]
        hi <- hi[nchar(hi)>0]
        hl$gene <- c(hl$gene,hi)
    }
    if (! 'gene' %in% names(arg)){
        nms <- c("standard_name", "systematic_name", "collection", "sub_collection",
          "organism", "description_brief", "description_full",
          "chip", "author", "contributor", "contributor_org")
        msigdb <- msigdb[,nms]
    }
    attr(msigdb,'msig_local') <- hl
    msigdb
}
