
#' Convert package Rd file under man directory into dataframe
#'
#' @param pkg source package path unzip from "tar.gz" file
#'
#' @return one dataframe
#' @export
#'
rd2df <- function(pkg){
    path <- paste0(pkg,'/man')
    rd <- list.files(path = path,pattern = '.rd',full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
    hold <- lapply(rd, tools::parse_Rd)
    x <- lapply(hold, rd2dfi)
    df <- do.call(plyr::rbind.fill,x)
    pkg <- file.name(pkg,extension = FALSE)
    df <- cbind(package=pkg,df)
    colnames(df)[colnames(df) == 'alias'] <- 'function'
    colnames(df)[colnames(df) == 'name'] <- 'rdname'
    f2 <- c("package","function",'title','description')
    df[,c(f2,colnames(df)[!colnames(df) %in% c(f2,'COMMENT','TEXT')])]
}

rd2dfi <- function(obj){
    content <- lapply(obj, function(i){
        unlist(i)  |> paste0(collapse = ' ') |> do::Replace0('\n')
    })
    name <- sapply(obj, function(i){
        attr(i,"Rd_tag") |> do::Replace0('\\\\')
    })
    name
    ck <- table(name) >= 2
    if (any(ck)){
        dupname <- names(table(name))[ck] |> unique()
        for (di in dupname) {
            dck <- name == di
            content[dck][1] <- content[name == di] |> unique() |> paste0(collapse = ', ')
        }
        content <- content[!duplicated(name)]
        name <- name[!duplicated(name)]
    }
    length(name)
    names(content) <- names(name)
    matrix(content,nrow = 1,dimnames = list(NULL,name)) |> data.frame(check.names = FALSE)
    
}

