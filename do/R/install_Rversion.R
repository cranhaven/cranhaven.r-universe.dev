
#' Install contributed packages by R version
#'
#' @param ... one or more package
#' @param platform windows or mac
#' @param Rversion version of R
#' @importFrom utils download.file untar unzip
#' @param lib path
#'
#' @return contributed packages
#' @export
#'
install_Rversion <- function(...,platform,Rversion=NULL,lib='.'){
    depends=TRUE
    pd <- c(...)
    if (is.null(pd)) stop(tmcn::toUTF8("\u6CA1\u6709\u8F93\u5165\u4E0B\u8F7D\u5305"))
    x <- Rversion_url(Rversion = Rversion,platform=platform)
    Rversion <- x$Rversion
    url <- x$url
    html <- xml2::read_html(url)
    pkgv <- html |>
        rvest::html_elements(xpath = '//a[@href]') |>
        rvest::html_text() |>
        grep_and('_')
    pkg <- do::Replace0(pkgv,c('_.*',' '))
    if (pd =='all packages'){
        pd <- pkg
    }else{
        pd <- do::Replace0(pd,' ')
        pd <- pkgv[tolower(pkg) %in% tolower(pd)]
        
    }
    if (length(pd)==0) message('no this package')
    # download
    install.pkg(pd,Rversion = Rversion,depends = depends,
                pkgv = pkgv,url = url,lib=lib,platform=platform)
}
Rversion_url <- function(Rversion,platform){
    if (left_equal(platform,'windows')){
        url1 <- 'https://cran-archive.r-project.org/bin/windows/contrib/'
        url2 <- 'https://cran.r-project.org/bin/windows/contrib/'
        v1 <- versionofR(url1)
        v2 <- versionofR(url2)
        if (is.null(Rversion)){
            Rversion <- select.list(c(v1,v2),multiple = FALSE,title = tmcn::toUTF8("\u8BF7\u9009\u62E9R\u8BED\u8A00\u7684\u7248\u672C,\u8F93\u5165\u5E8F\u53F7"))
        }
        url <- ifelse(Rversion %in% v1,url1,ifelse(Rversion %in% v2,url2,stop(tmcn::toUTF8("R\u8BED\u8A00\u7248\u672C\u4E0D\u5BF9"))))
        url <- sprintf('%s%s/',url,Rversion)
        list(Rversion=Rversion,
             url=url)
    }else if(left_equal(platform,'darwin')){
        url1 <- 'https://cran-archive.r-project.org/bin/macosx/'
        url2 <- 'https://cran-archive.r-project.org/bin/macosx/universal/contrib/'
        url3 <- 'https://cran-archive.r-project.org/bin/macosx/mavericks/contrib/'
        url4 <- 'https://cran.r-project.org/bin/macosx/contrib/'
        allurl <- c(url1,url2,url3,url4)
        vr <- c()
        for (i in allurl) {
            vi <- versionofR(i)
            names(vi) <- rep(i,length(vi))
            vr <- c(vr,vi)
        }
        
        if (is.null(Rversion)){
            Rversion <- select.list(vr,multiple = FALSE,title = tmcn::toUTF8("\u8BF7\u9009\u62E9R\u8BED\u8A00\u7684\u7248\u672C,\u8F93\u5165\u5E8F\u53F7"))
        }else{
            Rversion <- vr[vr %in% Rversion]
        }
        url <- sprintf('%s%s/',names(Rversion),Rversion)
        names(Rversion)=NULL
        list(Rversion=Rversion,
             url=url)
    }else{
        stop('platform can only be windows or darwin')
    }
}
install.pkg <- function(pd,Rversion=NULL,depends=FALSE,pkgv,url,lib='.',platform){
    pkg <- do::Replace0(pkgv,c('_.*',' '))
    pr <- do::Replace0(pd,c('_.*',' '))
    pd <- pr[! pr %in% list.files(lib)]
    if (!is.null(pd) & length(pd) >0){
        pd <- pkgv[tolower(pkg) %in% tolower(pd)]
        for (i in pd) {
            if (dir.exists(do::Replace0(i,c('_.*',' ')))) next(i)
            urlpd <- paste0(url,i)
            message('download ',i)
            download.file(url = urlpd,destfile = i,quiet = TRUE)
            if (missing(platform)){
                platform=tolower(Sys.info()['sysname'])
            }
            if (platform=='windows'){
                unzip(i)
            }else{
                untar(i)
            }
            
            unlink(i)
            if (depends){
                di <- depends.pkg(i)
                di <- pkg[tolower(pkg) %in% tolower(di)]
                di <- di[! di %in% list.files(lib)]
                if (!is.null(di) & length(di) >0){
                    cat('Install',do::Replace0(i,c('_.*',' ')),'dependency',paste0(di,collapse = ', '),'\n')
                    install.pkg(pd = di,Rversion = Rversion,
                                depends = depends,pkgv = pkgv,
                                url = url,lib=lib,platform=platform)
                }
            }
        }
    }else if(any(pr %in% list.files(lib)) & depends){
        prd <- pr[ pr %in% list.files(lib)]
        for (j in prd) {
            dj <- depends.pkg(j)
            dj <- dj[! pr %in% list.files(lib)]
            cat('install',do::Replace0(j,c('_.*',' ')),'dependency',paste0(dj,collapse = ', '),'\n')
            install.pkg(pd = dj,Rversion = Rversion,
                        depends = depends,pkgv = pkgv,
                        url = url,lib=lib,platform=platform)
        }
    }
}
depends.pkg <- function(pkg){
    filei <- do::Replace0(pkg,'_.*')
    dp <- desc::desc(file = filei)$.__enclos_env__$private$data$Depends$value
    im <- desc::desc(file = filei)$.__enclos_env__$private$data$Imports$value
    di <- c(dp,im)
    if (is.null(di)) return(NULL)
    di <- di |>
        strsplit(' {0,}, {0,}') |>
        unlist() |>
        do::Replace0(c('\n',' {0,}\\(.*')) |>
        do::Replace0(' ') |>
        not('R')
    if (length(di)==0) return(NULL)
    return(di)
}

versionofR <- function(url){
    html <- xml2::read_html(url)
    version <- html |>
        rvest::html_elements(xpath = '//a[@href]') |>
        rvest::html_text()
    v <- version |>
        grep_and('/') |>
        Replace0('/')
    if (length(v)==0){
        v <- version
    }
    v[!is.na(suppressWarnings(as.numeric(v)))]
}

