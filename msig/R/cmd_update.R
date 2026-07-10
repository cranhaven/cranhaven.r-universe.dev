#' Update local MsigDB database
#'
#' @param xml msigdb xml file
#' @param version version, if missing, the latest version will be used
#'
#' @return update local MsigDB database
#' @export
#'
msig_update<- function(xml=NULL,version=NULL){
    if (chinese()){
        catmsg <- tmcn::toUTF8("\n\u8FD0\u884C\u4EE5\u4E0B\u547D\u4EE4\u6765\u66F4\u65B0\u672C\u5730\u6570\u636E\u5E93")
        update <- tmcn::toUTF8("\n\u66F4\u65B0\u672C\u5730\u6570\u636E\u5E93")
        xmlversion <- tmcn::toUTF8("\u6307\u5B9A\u4E86xml\u7684\u65F6\u5019,\u9700\u8981\u540C\u65F6\u6307\u5B9Aversion")
    }else{
        catmsg <- '\nRun the following command to update the local database'
        update <- '\nupdate the local database'
        xmlversion <- 'When specifying the xml, you need to specify the version as well'
    }


    if (!is.null(xml) & is.null(version)) stop(xmlversion)
    if (is.null(version)) version=rev(suppressMessages(msig_version())[,1])[1]
    message('version: ',version)
    if (is.null(xml)){
        msig_download(version)
        xml <- sprintf('msigdb_v%s.xml',version)
        msigdb <- NewMsigDB(xml = xml)
        unlink(xml)
    }else{
        msigdb <- NewMsigDB(xml = xml)
    }



    # message(catmsg)
    libpath <- .libPaths()
    path <- libpath[sapply(libpath,function(i) "msig" %in% list.files(i))]
    msigpath <- paste0(path,'/msig/R/sysdata')
    msg <- paste0(sprintf("tools:::makeLazyLoadDB(from = x, filebase ='%s')",
                          msigpath),collapse = '\n\n')
    # cat('\n',msg,'\n')
    x <- list(msigdb=msigdb,version=version)
    message(update)
    for (i in msigpath) {
        cat(i,'\n')
        stringi <- sprintf("makeLazyLoadDB(from = x, filebase ='%s')",i)
        ps <- paste0('tools',do::rep_n(':',3),stringi)
        eval(parse(text = ps))
    }
}
