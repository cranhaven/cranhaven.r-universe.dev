

#' Retrieve detail information of gene set
#'
#' @param ... one or more gene set names, which can be little or capital.
#' @return Print detail information about the geneset, number of genes and return all gene names.
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#'
#' @examples
#' \donttest{
#' d <- msig_detail('izadpanah_stem_cell_adipose_vs_bone_dn',
#'                  'AAACCAC_MIR140')
#' }
#'


msig_detail <- function(...){
    names <- toupper(c(...))
    x <- lapply(seq_len(length(names)), function(i){
        message(i,' ',names[i])
        gsni <- di(names[i])
        gsni
    })
    do.call(plyr::rbind.fill,x)
}
di <- function(geneSetName){
    url <- sprintf('http://www.gsea-msigdb.org/gsea/msigdb/cards/%s.html',
                   geneSetName)
    html <- xml2::read_html(url)
    title <- c('Standard name',
               'Systematic name',
               'Brief description',
               'Full description or abstract',
               'Collection',
               'Source publication',
               'Exact source',
               'Organism',
               'Contributed by',
               'Source platform',
               'Dataset references',
               'Version history')
    df <- html |>
        rvest::html_nodes(xpath='//td[@class="body"]/table/tr') |>
        set::grep_or(paste0('<th>',title)) |>
        rvest::html_nodes('td') |>
        rvest::html_text() |>
        do::Replace0(c('\n {1,}','\u00A0')) |>
        matrix(nrow = 1,dimnames = list(NULL,title)) |>
        as.data.frame()
    df$`Show members` <- list(df_gene(html))
    df$`Related gene sets` <- list(df_related(html))
    df
}

df_similarity <- function(html){
    numb <- html |>
        rvest::html_nodes(xpath='//td[@class="body"]/table/tr') |>
        set::grep_and('Filtered by similarity') |>
        rvest::html_elements(xpath='td/a') |>
        rvest::html_text() |>
        do::Replace0(c('.*show ','\\)\n.*'))
    if (length(numb) == 0){
        ''
    }else{
        numb |>
            paste0(collapse = '\n') |>
            message()
        div <- html |>
            rvest::html_nodes(xpath='//td[@class="body"]/table/tr') |>
            set::grep_and('Filtered by similarity') |>
            rvest::html_elements(xpath='td') |>
            rvest::html_table() |>
            as.data.frame()
        colnames(div) <- c('External_ID','External_Name')
        link <- html |>
            rvest::html_nodes(xpath='//td[@class="body"]/table/tr') |>
            set::grep_and('Filtered by similarity') |>
            rvest::html_elements(xpath='td/div//a[@href]') |>
            rvest::html_attr('href')
        div$link <- link
        div
    }
}



df_related <- function(html){
    numb <- html |>
        rvest::html_nodes(xpath='//td[@class="body"]/table/tr') |>
        set::grep_and('Related gene sets') |>
        rvest::html_elements(xpath='td/a') |>
        rvest::html_text() |>
        do::Replace0(c('.*show ','\\).*'))
    if (length(numb) == 0){
        ''
    }else{
        div <- html |>
            rvest::html_nodes(xpath='//td[@class="body"]/table/tr') |>
            set::grep_and('Related gene sets') |>
            rvest::html_elements(xpath='td/div')
        for (i in seq_len(length(numb))) {
            if (i==1) rt <- list()
            ri <- div[i] |>
                rvest::html_elements(xpath='a') |>
                rvest::html_text() |>
                tolower() |>
                list()
            names(ri) <- numb[i]
            rt <- c(rt,ri)
        }
        rt
    }
}

df_gene <- function(html){

    df <- html |>
        rvest::html_nodes(xpath='//td[@class="body"]/table/tr') |>
        set::grep_and('Show members') |>
        do::Replace0('\u00A0') |>
        xml2::read_html() |>
        rvest::html_table() |>
        listn() |>
        as.data.frame()

    GeneId_href <- html |>
        rvest::html_nodes(xpath='//td[@class="body"]/table/tr') |>
        set::grep_and('Show members') |>
        do::Replace0('\u00A0') |>
        xml2::read_html() |>
        rvest::html_elements(xpath='//div/table/tr/td[2]') |>
        do::Replace0(c('.*href="','" title=.*'))
    GeneId_href[!grepl('https{0,}://',GeneId_href)]=''
    GeneSymbol_href <- html |>
        rvest::html_nodes(xpath='//td[@class="body"]/table/tr') |>
        set::grep_and('Show members') |>
        do::Replace0('\u00A0') |>
        xml2::read_html() |>
        rvest::html_elements(xpath='//div/table/tr/td[3]') |>
        do::Replace0(c('.*href="','" title=.*'))
    GeneSymbol_href[!grepl('https{0,}://',GeneSymbol_href)]=''
    df$GeneId_link <- GeneId_href
    df$GeneSymbol_link <- GeneSymbol_href
    df[is.na(df)]=''
    df
}


