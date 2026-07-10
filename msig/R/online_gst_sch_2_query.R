
#' Query MSigDB database by cookie
#'
#' @param keywords one keywords see Detail field
#' @param collection one or more collections
#' @param organism one or more organisms
#' @param contributor one or more contributors
#' @param email email that registered for MSigDB database.
#' @return dataframe contains name, description and so on.
#'
#' @export
#'
#' @examples
#' \donttest{
#' email <- 'your email'
#' x <- search_msig('immune & response')
#' x |>
#'     msig_filt('system') |>
#'     msig_view('C2')
#' }
#'
search_msig <- function(keywords,
                          collection='',organism='',contributor='',
                         email){
    message('Loading HTML')
    keywords <- key(keywords)
    url <- constuct(keywords,collection,organism,contributor)
    if (missing(email)) email = get('email',envir = .GlobalEnv)
    h <- httr::GET(URLencode(url),httr::authenticate(email, "password"))
    x <- httr::content(h)
    if (!grepl('var myDataSource =',x)){
        message(' === your search returned no results=== ')
    }else{
        message('Clear javascript')
        f <- x |>
            do::Replace0('\r') |>
            do::Replace0('.*var myData =[\\[ ]{0,}') |>
            do::Replace0(c('[; ]{0,}\\][; ]{0,}var myDataSource =.*')) |>
            do::Replace0(c("<span class='highlight' style='padding-left: 0px; padding-right: 0px;'>",'</span>')) |>
            do::Replace('name:','"name":') |>
            do::Replace('nameRaw:','"nameRaw":') |>
            do::Replace('numGenes:','"numGenes":') |>
            do::Replace('description:','"description":') |>
            do::Replace('collections:','"collections":') |>
            do::Replace('organism:','"organism":') |>
            do::Replace('contributor:','"contributor":') |>
            do::Replace(' {0,}}','}') |>
            do::Replace(' {0,}, {1,}',', ') |>
            do::Replace('\\}, \\{','}}, {{') |>
            strsplit('\\}, \\{') |>
            unlist() |>
            lapply(function(i) as.data.frame(jsonlite::fromJSON(i))) |>
            do.call(what = plyr::rbind.fill)
        message('gene sets: ',nrow(f),'\n')
        message('add description_full')
        f[,2] <- tolower(f[,2])
        colnames(f)[2] <- 'standard_name'
        if ('NewMsigDB' %in% ls(envir = .GlobalEnv)) msigdb <- get('NewMsigDB',envir = .GlobalEnv)
        f <- dplyr::inner_join(f,msigdb[,c('standard_name','description_full')],'standard_name')
        f[,1] <- sprintf('http://www.gsea-msigdb.org/gsea/msigdb/geneset_page.jsp%s', do::Replace0(f[,1],'.*\\|msigdb/geneset_page.jsp')) |>            do::Replace('&','&amp;')
        colnames(f)[1]='link'
        f <- f[,c(colnames(f)[-1],colnames(f)[1])]
        f <- f[,c("standard_name", "numGenes", "description",'description_full',"collections", "organism", "contributor", "link")]
        hightlitht <- strsplit(keywords,' AND | OR | ')[[1]]
        hightlitht <- hightlitht[nchar(hightlitht)>0]
        attr(f,'search_msig') <- hightlitht
        f
    }
}

constuct <- function(keywords,collection,organism,contributor){
    if (length(keywords) != 1) stop('keywords must be one')
    keywords <- paste0('keywords=',keywords)
    collection <- paste0(paste0('&collection=',collection),collapse = '')
    organism <- paste0(paste0('&organism=',organism),collapse = '')
    contributor <- paste0(paste0('&contributor=',contributor),collapse = '')
    q <- paste0(keywords,collection,organism,contributor)

    sprintf('http://www.gsea-msigdb.org/gsea/msigdb/search.jsp?%s',
            q)
}


key <- function(keywords){
    if (missing(keywords)) stop('keywords can not be missing')
    do::Replace(keywords,pattern = c(' {1,}and {1,}: AND ',
                                     ' {1,}or {1,}: OR ',
                                     ' {1,}& {1,}: AND ',
                                     ' {1,}\\| {1,}: OR '))
}
