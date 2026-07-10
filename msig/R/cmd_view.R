#' View data in viewer panel
#'
#' @param x dataframe
#' @param ... one or more hightliht words
#' @importFrom utils URLencode download.file
#' @return open data in view panel in rstudio
#' @export
#' @examples
#' #' browse_msig('immune') |>
#'     msig_view('response')
msig_view <- function(x,...){
    high0 <- c(...)
    if ('browse_msig' %in% names(attributes(x))){
        x0 <- x
        hl <- attr(x,"browse_msig")
        hl <- unique(c(hl,high0))
        rk <- order(nchar(hl),decreasing = TRUE)
        hl <- hl[rk]
        plt <- palette[seq_len(length(hl))]
        plt <- plt[rk]
        x <- add_color_to_text(x,hl,plt)
        xf <- data.frame(seq_len(length(x)),x)
        colnames(xf) <- c('id','gene set names')
        kableExtra::kbl(xf,escape = FALSE) |>
            kableExtra::kable_paper("striped") |>
            kableExtra::column_spec(1,
                                    link = sprintf('http://www.gsea-msigdb.org/gsea/msigdb/cards/%s.html',
                                                   x0))

    }else if ('search_msig' %in% names(attributes(x))){
        # dataframe
        x$standard_name <- do::Replace(x$standard_name,'_',' ')
        hl <- unique(c(attr(x,"search_msig"),high0))
        if (!is.null(hl)){
            rk <- order(nchar(hl),decreasing = TRUE)
            hl <- hl[rk]
            plt <- palette[seq_len(length(hl))]
            plt <- plt[rk]
            x3 <- do::paste0_columns(x[,seq_len(7)],'---------')
            for (i in seq_len(length(hl))) {
                x3 <- gsub(hl[i],.hlw(hl[i],plt[i]),x3)
            }
        }
        x4 <- do::col_split(x3,'---------',colnames = colnames(x)[seq_len(7)])
        kableExtra::kbl(x4,
                        escape = FALSE,
                        align=c('l','c','l','l','l','l','l')) |>
            kableExtra::kable_paper("striped") |>
            kableExtra::column_spec(2,link = x$link) |>
            kableExtra::row_spec(0,align = 'c')
    }else if ('similarity_geneset' %in% names(attributes(x))){
        hl <- unique(c(attr(x,"similarity_geneset")[-1],high0))
        if (length(hl)>0){
            rk <- order(nchar(hl),decreasing = TRUE)
            hl <- hl[rk]
            plt <- palette[seq_len(length(hl))]
            plt <- plt[rk]
            x[,1] <- add_color_to_text(x[,1],hl,plt)
            x[,2] <- add_color_to_text(x[,2],hl,plt)
        }

        kableExtra::kbl(x[,seq_len(2)],
                        escape = FALSE,
                        align=c('c','l')) |>
            kableExtra::kable_paper("striped") |>
            kableExtra::column_spec(1,link = x$link) |>
            kableExtra::row_spec(0,align = 'c')
    }else if ('related_geneset' %in% names(attributes(x))){
        nr <- sapply(x, function(i) length(i))
        at <- attr(x,"related_geneset")
        if (any(nr == 0)) x <- x[order(nr)]
        hl <- c(at[-1],high0) |>
            unique()
        link <- sprintf('http://www.gsea-msigdb.org/gsea/msigdb/cards/%s.html',
                        do.call('c',x))
        if (length(hl)>0){
            rk <- order(nchar(hl),decreasing = TRUE)
            hl <- hl[rk]
            plt <- palette[seq_len(length(hl))]
            plt <- plt[rk]
            for (j in seq_len(length(x))){
                x[[j]] <- add_color_to_text(x[[j]],hl,plt)
            }
        }
        x3 <- do.call('c',x)
        x4 <- data.frame(group=paste0(seq_len(length(x3)),'     '))
        x4 <- cbind(x4,data.frame(x3))
        row.names(x4) <- seq_len(nrow(x4))
        colnames(x4)[2] <- paste0('related genesets of --->  ',attr(x,"related_geneset")[1])
        len <- c(0,sapply(x, function(i) length(i)))
        pk <- sapply(2:length(len), function(i){
            ii <- c(sum(len[seq_len((i-1))])+1,sum(len[seq_len(i)]))
            sprintf('kableExtra::pack_rows("%s", %s, %s)',
                    names(x)[i-1],ii[1],ii[2])
            }) |>
            paste0(collapse = ' |> \n')


        pk0 <- 'kableExtra::kbl(x4,escape = FALSE) |>
            kableExtra::kable_paper("striped")  |>
            kableExtra::column_spec(1,link = link)'
        string=paste0(pk0,' |> \n',pk)
        eval(parse(text = string))

    }else if ('msig_gene' %in% names(attributes(x))){
        # list of dataframe
        nr <- sapply(x, function(i) nrow(i))
        at <- attr(x,"msig_gene")
        if (any(nr == 0)) x <- x[order(nr)]
        hl <- c(at[-c(seq_len(length(x)))],high0) |>
            unique()
        if (length(hl)>0){
            rk <- order(nchar(hl),decreasing = TRUE)
            hl <- hl[rk]
            plt <- palette[seq_len(length(hl))]
            plt <- plt[rk]
            for (m in seq_len(length(x))) {
                xm <- x[[m]]
                xm <- xm[,sapply(seq_len(ncol(xm)), function(i) sum(nchar(xm[,i]))) >1,drop=FALSE]
                nmf <- set::grep_not_and(colnames(xm),'_link')
                if (length(nmf)==1){
                    xmp=xm[,1]
                }else{
                    xmp <- do::paste0_columns(xm[,nmf],'----------')
                }
                xmpad <- add_color_to_text(xmp,hl,plt)
                nmdf <- col_split2(xmpad,'----------',colnames = nmf)
                x[[m]][,nmf] <- nmdf
            }
        }

        x3 <- do.call(plyr::rbind.fill,x)
        row.names(x3) <- NULL
        len <- c(0,sapply(x, function(i) nrow(i)))
        pk <- sapply(2:length(len), function(i){
            ii <- c(sum(len[seq_len((i-1))])+1,sum(len[seq_len(i)]))
            sprintf('kableExtra::pack_rows("%s", %s, %s)',
                    names(x)[i-1],ii[1],ii[2])
        }) |>
            paste0(collapse = ' |> \n')
        if (any(grepl('_link',colnames(x[[1]])))){
            pk0 <- 'kableExtra::kbl(x3[,seq_len(4)],escape = FALSE,align=c("c","c","c","l")) |>
            kableExtra::kable_paper("striped")  |>
            kableExtra::column_spec(2,link = x3$GeneId_link) |>
            kableExtra::column_spec(3,link = x3$GeneSymbol_link)'
        }else{
            link <- sprintf('https://www.ncbi.nlm.nih.gov/gene/%s',x3$EntrezId)
            if (length(link)==0){
                pk0 <- 'kableExtra::kbl(x3,escape = FALSE,align="c") |>
            kableExtra::kable_paper("striped")'
            }else{
                pk0 <- 'kableExtra::kbl(x3,escape = FALSE,align="c") |>
            kableExtra::kable_paper("striped")  |>
            kableExtra::column_spec(3,link = link)'
            }

        }

        string=paste0(pk0,' |> \n',pk)
        eval(parse(text = string))

    }else if ('msig_local' %in% names(attributes(x))){
        hl <- attr(x,'msig_local')
        ck <- sapply(seq_len(ncol(x)), function(i) sum(nchar(x[,i]))) > 1
        x <- x[,ck]
        hl <- hl[set::and(names(hl),colnames(x))]
        for (i in seq_len(length(hl))) {
            hi <- hl[[i]]
            rk <- order(nchar(hi),decreasing = TRUE)
            hi <- hi[rk]
            plt <- palette[seq_len(length(hi))][rk]
            x[,names(hl)[i]] <- add_color_to_text(x[,names(hl)[i]],hi,plt)

        }
        sdn <- x$standard_name
        x$standard_name <- gsub('_',' ',sdn)
        kableExtra::kbl(x,escape = FALSE) |>
            kableExtra::kable_paper("striped")
    }else{
        x
    }

}

# scales::alpha('red',0.5)
palette <- c("#FFFF0080", "#00FF0033", "#DEA28280",
             "#FF000080",
             "#C1C3EE80", "#C08BED80", "#7CF14180", "#E6EDDD80", "#8A5DB280",
             "#953BAE80", "#69EA9D80", "#888FAE80", "#4858E480", "#AAE0BE80",
             "#ADAA9F80", "#E2DE7E80", "#E1969D80", "#D765A380", "#67CBE680",
             "#5E1C9580", "#AD3DDE80", "#A565EE80", "#6FB32F80", "#BAD7E280",
             "#535B9980", "#E197DE80", "#E366E880", "#A89BE480", "#CDEC3480",
             "#DECF4280", "#E7C6D780", "#CF55B380", "#70B2E380", "#D2C79180",
             "#8CE46E80", "#DC562F80", "#F287EF80", "#E6EFB880", "#C8E97180",
             "#D8825180", "#8238EA80", "#E43B9180", "#89C27980", "#E4B3E380",
             "#EA456E80", "#EBC5B680", "#50A27B80", "#707BE680", "#E7AD4F80",
             "#5C94E680", "#E23AC580", "#56696380", "#E2706C80", "#59EBEC80"
)



.hlw <- function(word,color='yellow'){
    # high light words
    sprintf('<span style="background-color:%s">%s</span>',
            color,word)
}


replaceit <- function(data,from,to,pattern){
    Replace1<-function(data,from,to){
        if (any(is.data.frame(data),is.matrix(data))){
            for (i in seq_len(ncol(data))) {
                data[,i]=gsub(from,to,data[,i],ignore.case = TRUE)
            }
        }else{
            data=gsub(from,to,data,ignore.case = TRUE)
        }
        data
    }
    if (all(!missing(from),!missing(to))){
        for (i in seq_len(length(from))) {
            data=Replace1(data,from[i],to)
        }
    }
    if (!missing(pattern)){
        for (j in seq_len(length(pattern))) {
            from=gsub(":.*","",pattern[j],ignore.case = TRUE)
            to=gsub(".*:","",pattern[j],ignore.case = TRUE)
            data=Replace1(data,from,to)
        }
    }
    data
}
