


#' @title Batch Process
#' 
#' @description
#' See workhorse function [op_hyperframe()].
#' 
#' @param X a \link[spatstat.geom]{hyperframe}
#' 
#' @param correction \link[base]{character} scalar,
#' see functions 
#' \link[spatstat.explore]{markcorr},
#' \link[spatstat.explore]{Gcross},
#' etc.
#' Default `'none'` to save computing time.
#' 
#' @param ... additional parameters of user operation
#' 
#' @details
#' User Interface of Operations on \link[spatstat.geom]{hyperframe} with One-and-Only-One \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}
#' 
#' 
#' @examples
#' # in \CRANpkg{spatstat.data}
#' # no good example for [Emark_]
#' # no hyperframe with ppp-hypercolumn with numeric marks
#' fluM = spatstat.data::flu |>
#'  spatstat.geom::subset.hyperframe(subset = (stain == 'M2-M1') & (virustype == 'wt'))
#' fluM
#' r = seq.int(from = 0, to = 100, by = 5)
#' fluM |>
#'  nncross_(i = 'M1', j = 'M2', mc.cores = 2L)
#'  
#'  
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#'  
#' @keywords internal
#' @name batch
#' @export
Emark_ <- function(X, ...) UseMethod(generic = 'Emark_')

#' @rdname batch
#' @importFrom spatstat.explore Emark
#' @export Emark_.hyperframe
#' @export
Emark_.hyperframe <- function(X, correction = 'none', ...) {
  X |> 
    op_hyperframe(op = ppp_numeric2fv, fun = Emark, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore Emark
#' @export Emark_.ppplist
#' @export
Emark_.ppplist <- function(X, correction = 'none', ...) {
  X |> 
    op_ppplist(op = ppp_numeric2fv, fun = Emark, correction = correction, ...)
}


#' @rdname batch
#' @importFrom spatstat.explore Emark
#' @export Emark_.ppp
#' @export
Emark_.ppp <- function(X, ...) {
  X |> 
    ppp_numeric2fv(fun = Emark, ...)
}






#' @rdname batch
#' @export
Vmark_ <- function(X, ...) UseMethod(generic = 'Vmark_')

#' @rdname batch
#' @importFrom spatstat.explore Vmark
#' @export Vmark_.hyperframe
#' @export
Vmark_.hyperframe <- function(X, correction = 'none', ...) {
  X |> 
    op_hyperframe(op = ppp_numeric2fv, fun = Vmark, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore Vmark
#' @export Vmark_.ppplist
#' @export
Vmark_.ppplist <- function(X, correction = 'none', ...) {
  X |> 
    op_ppplist(op = ppp_numeric2fv, fun = Vmark, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore Vmark
#' @export Vmark_.ppp
#' @export
Vmark_.ppp <- function(X, ...) {
  X |> 
    ppp_numeric2fv(fun = Vmark, ...)
}



#' @rdname batch
#' @export
Kmark_ <- function(X, ...) UseMethod(generic = 'Kmark_')

#' @rdname batch
#' @importFrom spatstat.explore Kmark
#' @export Kmark_.hyperframe
#' @export
Kmark_.hyperframe <- function(X, correction = 'none', ...) {
  X |> 
    op_hyperframe(op = ppp_numeric2fv, fun = Kmark, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore Kmark
#' @export Kmark_.ppplist
#' @export
Kmark_.ppplist <- function(X, correction = 'none', ...) {
  X |> 
    op_ppplist(op = ppp_numeric2fv, fun = Kmark, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore Kmark
#' @export Kmark_.ppp
#' @export
Kmark_.ppp <- function(X, ...) {
  X |> 
    ppp_numeric2fv(fun = Kmark, ...)
}



#' @rdname batch
#' @export
markcorr_ <- function(X, ...) UseMethod(generic = 'markcorr_')

#' @rdname batch
#' @importFrom spatstat.explore markcorr
#' @export markcorr_.hyperframe
#' @export
markcorr_.hyperframe <- function(X, correction = 'none', ...) {
  X |> 
    op_hyperframe(op = ppp_numeric2fv, fun = markcorr, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore markcorr
#' @export markcorr_.ppplist
#' @export
markcorr_.ppplist <- function(X, correction = 'none', ...) {
  X |> 
    op_ppplist(op = ppp_numeric2fv, fun = markcorr, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore markcorr
#' @export markcorr_.ppp
#' @export
markcorr_.ppp <- function(X, ...) {
  X |> 
    ppp_numeric2fv(fun = markcorr, ...)
}




#' @rdname batch
#' @export
markvario_ <- function(X, ...) UseMethod(generic = 'markvario_')

#' @rdname batch
#' @importFrom spatstat.explore markvario
#' @export markvario_.hyperframe
#' @export
markvario_.hyperframe <- function(X, correction = 'none', ...) {
  X |> 
    op_hyperframe(op = ppp_numeric2fv, fun = markvario, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore markvario
#' @export markvario_.ppplist
#' @export
markvario_.ppplist <- function(X, correction = 'none', ...) {
  X |> 
    op_ppplist(op = ppp_numeric2fv, fun = markvario, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore markvario
#' @export markvario_.ppp
#' @export
markvario_.ppp <- function(X, ...) {
  X |> 
    ppp_numeric2fv(fun = markvario, ...)
}



#' @rdname batch
#' @export
Gcross_ <- function(X, ...) UseMethod(generic = 'Gcross_')

#' @rdname batch
#' @importFrom spatstat.explore Gcross
#' @export Gcross_.hyperframe
#' @export
Gcross_.hyperframe <- function(X, correction = 'none', ...) {
  X |> 
    op_hyperframe(op = ppp_multitype2fv, fun = Gcross, correction = correction, ...)
}


#' @rdname batch
#' @importFrom spatstat.explore Gcross
#' @export Gcross_.ppplist
#' @export
Gcross_.ppplist <- function(X, correction = 'none', ...) {
  X |> 
    op_ppplist(op = ppp_multitype2fv, fun = Gcross, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore Gcross
#' @export Gcross_.ppp
#' @export
Gcross_.ppp <- function(X, ...) {
  X |> 
    ppp_multitype2fv(fun = Gcross, ...)
}


#' @rdname batch
#' @export
Jcross_ <- function(X, ...) UseMethod(generic = 'Jcross_')

#' @rdname batch
#' @importFrom spatstat.explore Jcross
#' @export Jcross_.hyperframe
#' @export
Jcross_.hyperframe <- function(X, correction = 'none', ...) {
  X |> 
    op_hyperframe(op = ppp_multitype2fv, fun = Jcross, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore Jcross
#' @export Jcross_.ppplist
#' @export
Jcross_.ppplist <- function(X, correction = 'none', ...) {
  X |> 
    op_ppplist(op = ppp_multitype2fv, fun = Jcross, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore Jcross
#' @export Jcross_.ppp
#' @export
Jcross_.ppp <- function(X, ...) {
  X |> 
    ppp_multitype2fv(fun = Jcross, ...)
}




#' @rdname batch
#' @export
Kcross_ <- function(X, ...) UseMethod(generic = 'Kcross_')

#' @rdname batch
#' @importFrom spatstat.explore Kcross
#' @export Kcross_.hyperframe
#' @export
Kcross_.hyperframe <- function(X, correction = 'none', ...) {
  X |> 
    op_hyperframe(op = ppp_multitype2fv, fun = Kcross, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore Kcross
#' @export Kcross_.ppplist
#' @export
Kcross_.ppplist <- function(X, correction = 'none', ...) {
  X |> 
    op_ppplist(op = ppp_multitype2fv, fun = Kcross, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore Kcross
#' @export Kcross_.ppp
#' @export
Kcross_.ppp <- function(X, ...) {
  X |> 
    ppp_multitype2fv(fun = Kcross, ...)
}


#' @rdname batch
#' @export
Lcross_ <- function(X, ...) UseMethod(generic = 'Lcross_')

#' @rdname batch
#' @importFrom spatstat.explore Lcross
#' @export Lcross_.hyperframe
#' @export
Lcross_.hyperframe <- function(X, correction = 'none', ...) {
  X |> 
    op_hyperframe(op = ppp_multitype2fv, fun = Lcross, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore Lcross
#' @export Lcross_.ppplist
#' @export
Lcross_.ppplist <- function(X, correction = 'none', ...) {
  X |> 
    op_ppplist(op = ppp_multitype2fv, fun = Lcross, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore Lcross
#' @export Lcross_.ppp
#' @export
Lcross_.ppp <- function(X, ...) {
  X |> 
    ppp_multitype2fv(fun = Lcross, ...)
}




#' @rdname batch
#' @export
markconnect_ <- function(X, ...) UseMethod(generic = 'markconnect_')

#' @rdname batch
#' @importFrom spatstat.explore markconnect
#' @export markconnect_.hyperframe
#' @export
markconnect_.hyperframe <- function(X, correction = 'none', ...) {
  X |> 
    op_hyperframe(op = ppp_multitype2fv, fun = markconnect, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore markconnect
#' @export markconnect_.ppplist
#' @export
markconnect_.ppplist <- function(X, correction = 'none', ...) {
  X |> 
    op_ppplist(op = ppp_multitype2fv, fun = markconnect, correction = correction, ...)
}

#' @rdname batch
#' @importFrom spatstat.explore markconnect
#' @export markconnect_.ppp
#' @export
markconnect_.ppp <- function(X, ...) {
  X |> 
    ppp_multitype2fv(fun = markconnect, ...)
}


# Inside \link[spatstat.explore]{Gcross} and \link[spatstat.explore]{Kcross}
# @param i type of the points *from* which distances are measured,
# i.e., `X` (or \emph{of}) in \link[spatstat.geom]{nncross}.
# @param j type of the points *to* which distances are measured,
# i.e., `Y` (or \emph{in}) in \link[spatstat.geom]{nncross}.


#' @rdname batch
#' @export
nncross_ <- function(X, ...) UseMethod(generic = 'nncross_')

#' @rdname batch
#' @export nncross_.hyperframe
#' @export
nncross_.hyperframe <- function(X, ...) {
  X |> 
    op_hyperframe(op = ppp2dist, fun = .nncross, ...)
}

#' @rdname batch
#' @export nncross_.ppplist
#' @export
nncross_.ppplist <- function(X, ...) {
  X |> 
    op_ppplist(op = ppp2dist, fun = .nncross, ...)
}

#' @rdname batch
#' @export nncross_.ppp
#' @export
nncross_.ppp <- function(X, ...) {
  X |> 
    ppp2dist(fun = .nncross, ...)
}








#' @title Operations on \link[spatstat.geom]{hyperframe} with One-and-Only-One \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}
#' 
#' @description
#' Create hypercolumn(s) of
#' \link[spatstat.explore]{fv.object}s 
#' or 
#' distances
#' from 
#' the one-and-only-one \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}
#' inside a \link[spatstat.geom]{hyperframe}.
#' 
#' @param X a \link[spatstat.geom]{hyperframe}, containing ***one-and-only-one*** \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}
#' 
#' @param ... additional parameters of the function [op_ppplist()]
#' 
#' @returns
#' The function [op_hyperframe()] returns a \link[spatstat.geom]{hyperframe}.
#' 
#' @keywords internal
#' @importFrom spatstat.geom is.ppplist cbind.hyperframe
#' @export
op_hyperframe <- function(X, ...) {
  
  hc <- unclass(X)$hypercolumns
  id <- hc |> 
    vapply(FUN = is.ppplist, FUN.VALUE = NA)
  if (sum(id) != 1L) stop('allow one-and-only-one ppp-hypercolumn, which may contain one or more mark(s)')
  
  ret <- hc[[which(id)]] |> 
    op_ppplist(x = _, ...)
  
  fname1 <- attr(ret[[1L]], which = 'fname', exact = TRUE)[1L]
  suffix <- attr(ret[[1L]], which = 'suffix', exact = TRUE)[1L]
  if (length(fname1)) {
    names(ret) <- paste(names(ret), fname1, sep = '.')
  } else if (length(suffix)) {
    names(ret) <- paste(names(ret), suffix, sep = '.')
  } #else do nothing

  return(do.call(
    what = cbind, # dispatch to \link[spatstat.geom]{cbind.hyperframe} or [cbind.groupedHyperframe()]
    args = c(list(X), ret)
  ))
  
}





# ?spatstat.geom::hyperframe drops derived classes from 'anylist'
# tzh thinks it's in `if (any(hypercolumns))`;
# .. |> lapply(FUN = as.solist)
# tzh will think if it's worth while to write to Dr. Baddeley
# best solution might be add an exception call in ?spatstat.geom::as.solist
# if (x is a-derived-class-of-anylist-other-than-solist) return(x)




