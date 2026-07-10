
#' read MSigDB xml data
#'
#' @param xml xml data path
#'
#' @return one dataframe contains gene infomation
#' @export
#'
read_msigdb_xml <- function(xml){
    x <- xml2::read_html(xml) |>
        rvest::html_nodes(xpath='//geneset')

    message('gene sets: ',length(x))

    pb <- txtProgressBar(max = length(x),width = 30,style = 3)

    lp <- lapply(seq_len(length(x)), function(i){
        setTxtProgressBar(pb,i)
        xi=x[i]
        class(xi)='html_node'
        atx <- rvest::html_attrs(xi[[1]])
        data.frame(matrix(atx,nrow=1,
                          dimnames = list(NULL,names(atx))))
    })

    df <- do.call(plyr::rbind.fill,lp)

    df$standard_name <- tolower(df$standard_name)

    df <- do::Replace0(df,'\uFFFD')

    char <- sapply(seq_len(ncol(df)), function(i) sum(nchar(df[,i])))
    df[,char>0]
}
