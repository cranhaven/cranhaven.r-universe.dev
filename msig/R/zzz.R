chinese <- function(){
    grepl('chinese',tolower(Sys.getlocale()))
}
.onAttach <- function(...){
    X=suppressWarnings(file.remove(list.files(tempdir(),pattern = 'local_query.txt',full.names = TRUE)))
    unlink(list.files(tempdir(),pattern = 'local_query.txt',full.names = TRUE))
    if (chinese()){
        m1 <- tmcn::toUTF8("\u68C0\u6D4BMsigDB\u6570\u636E\u5E93\u672C\u5730\u7248\u672C\u548C\u7EBF\u4E0A\u7248\u672C\u662F\u5426\u4E00\u81F4")
        concord <- tmcn::toUTF8("\u7248\u672C\u4E00\u81F4,\u672C\u5730\u641C\u7D22\u548C\u7EBF\u4E0A\u641C\u7D22\u5747\u53EF\u4F7F\u7528")
        noInternet <- tmcn::toUTF8("\u672A\u8FDE\u63A5\u7F51\u7EDC\u6216\u7F51\u901F\u8FC7\u6162")
        useOnline <- tmcn::toUTF8("\u5728\u7EBF\u6570\u636E\u5E93\u7248\u672C\u8F83\u65B0,\u5EFA\u8BAE\u4F7F\u7528\u5728\u7EBF\u641C\u7D22\u529F\u80FD,\u6216\u8005\u66F4\u65B0\u672C\u5730\u6570\u636E")
        testinfo <- tmcn::toUTF8("\u672C\u5730\u6570\u636E\u5E93\u4E3A\u6D4B\u8BD5\u7248\u672C,\u5982\u8981\u4F7F\u7528\u672C\u5730\u67E5\u8BE2,\u9700\u5148\u66F4\u65B0\u672C\u5730\u6570\u636E\u5E93")
    }else{
        m1 <- 'Check if the local and online versions of the MsigDB database are the same'
        concord <- 'Consistent version, available for both local search and online search'
        noInternet <- 'No internet connection or slow internet speed'
        useOnline <- 'The online database version is new, we recommend using the online search function or update the local database'
        testinfo <- 'The local database is a test version, if you want to use the local query, you need to update the local database first'
    }
    packageStartupMessage(m1)
    version_online <- tryCatch(suppressMessages(rev(msig_version()[,1])[1]),
                               error=function(e) 'Internet was not connected')
    packageStartupMessage(sprintf('Local: %s\nOnline: ',version),
                          version_online)
    if (version == 'test'){
        packageStartupMessage(testinfo)
    }else{
        if (version_online == version){
            packageStartupMessage(concord)
        }else if (version_online == 'Internet was not connected'){
            packageStartupMessage(noInternet)
        }else{
            packageStartupMessage(useOnline)
        }
    }

}


.onDetach <- function(...){
    X=suppressWarnings(file.remove(list.files(tempdir(),pattern = 'local_query.txt',full.names = TRUE)))
    unlink(list.files(tempdir(),pattern = 'local_query.txt',full.names = TRUE))

}
