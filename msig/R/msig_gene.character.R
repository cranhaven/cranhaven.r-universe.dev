

#' @export
#' @method msig_gene character
#' @rdname msig_gene
msig_gene.character <- function(...,list=TRUE,info=TRUE){
    names <- toupper(c(...))
    x <- sapply(seq_len(length(names)), function(i)  gi(names[i],info=info))
    attr(x,'msig_gene') <- names
    x
}


gi <- function(geneSetName,info=TRUE){
    url <- sprintf('http://www.gsea-msigdb.org/gsea/msigdb/cards/%s.html',
                   geneSetName)
    html <- xml2::read_html(url)
    if (info){
        cat(crayon::red(geneSetName),' ')
        html |>
            rvest::html_nodes(xpath='//td[@class="body"]/table/tr') |>
            set::grep_and('Show members') |>
            rvest::html_elements(xpath='td/a') |>
            rvest::html_text() |>
            do::Replace0(c('.*show ',')\n.*')) |>
            cat('\n')
    }


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
    list_name(df,tolower(geneSetName))
}

listn <- function(list,n=1){
    if (length(n==1)){
        list[[n]]
    }else{
        list[n]
    }
}
list_name <- function(x,name){
    l <- list(x)
    names(l) <- name
    l
}

