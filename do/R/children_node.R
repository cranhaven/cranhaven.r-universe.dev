#' Wether children nodes exist
#'
#' @param ... one or more documents, nodes, or node sets.
#' @importFrom rvest html_name html_nodes
#' @importFrom xml2 xml_children
#' @return logical value
#' @export
#'
#' @examples
#' txt='<div class="activityBody postBody thing">
#'         <p>
#'             <a href="/forum/conversation" rel="post" >(22)</a>
#'             where?
#'         </p>
#'       <p>
#'         stays 
#'         <b>disappears</b>
#'         <a>disappears</a>D
#'         <span>disappears</span>
#'         stays
#'       </p>
#'     </div>'
#'  library(xml2)
#'  html=read_html(txt)
#'  has_children(html)
has_children <- function(...){
    parent <- list(...)
    if (length(html_name(parent[[1]]))==1){
        if (length(html_name(parent[[1]])) ==1){
            x0=html_nodes(parent[[1]],xpath = paste0('/',html_name(parent[[1]])))
            if (length(x0)==0){
                parent[[1]]=html_nodes(parent[[1]],xpath = paste0('//',html_name(parent[[1]])))
            }else{
                parent[[1]]=x0
            }
        }
    }
    sapply(parent[[1]], function(i) length(xml_children(i)) >0)
}

#' Extract all children nodes
#'
#' @param x one or more documents, nodes, or node sets.
#' @param res omit. do not make any change.
#' @param i must be 1
#'
#' @return nodeset
#' @export
#'
#' @examples
#' txt='<div class="activityBody postBody thing">
#'         <p>
#'             <a href="/forum/conversation" class="mqPostRef">(22)</a>
#'             where?
#'         </p>
#'       <p>
#'         stays 
#'         <b>disappears</b>
#'         <a>disappears</a>
#'         <span>disappears</span>
#'         stays
#'       </p>
#'     </div>'
#'     library(xml2)
#'     html=read_html(txt)
#'     
#'     all_children(html)

all_children <- function(x, res='do not change',i=1){
    if (length(html_name(x)) ==1){
        x0=html_nodes(x,xpath = paste0('/',html_name(x)))
        if (length(x0)==0){
            x=html_nodes(x,xpath = paste0('//',html_name(x)))
        }else{
            x=x0
        }
    }
    has <- x[has_children(x)]
    has_no <- x[!has_children(x)]
    if (length(has) == 0 & length(has_no) >0){
        # all have no children
        if (i == 1){
            res=has_no
        }else{
            res[(length(res)+1):(length(res)+length(has_no))]=has_no
        }
        i = i + 1
        return(res)
    }else if (length(has) == 0 & length(has_no) == 0){
        # all have no child and all have no child_no
        return(res)
    }else if (length(has) > 0 & length(has_no) > 0){
        if (i == 1){
            res=has_no
        }else{
            res[(length(res)+1):(length(res)+length(has_no))]=has_no
        }
        i = i + 1
        return(all_children(x = xml_children(has),
                            res = res,
                            i=i))
    }else if (length(has) > 0 & length(has_no) == 0){
        
        return(all_children(x = xml_children(has),
                            res = res,
                            i=i))
    }
}
