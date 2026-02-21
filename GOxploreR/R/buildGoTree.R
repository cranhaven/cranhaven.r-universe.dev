#### Build the Biological process ontologies (Go tree)  ###
#' Building tree one
#'
#' @param t The GO root term
#' @keywords internal
#'
#' @return returns GO terms
#'
#'
GOTermsApp.B <- function(t){
  h <- 1
  assign(t, h, envir = go2h)
  terms <- xx.ch[[t]]
  terms <- terms[!is.na(terms)]
  L <- length(terms)
  if(L > 0){
    h <- h + 1
    for(i in 1:L){
      assign(terms[i], h, envir = go2h)
    }
  }

  flag <- 1

  while(flag){
    L <- length(terms)
    if(L > 0){
      terms <- all_terms_level_h2(xx.ch, terms)
      L <- length(terms)
      if(L > 0){
        h <- h + 1
        for(i in 1:L){
          f <- exists(terms[i], envir=go2h)
          if(!f){
            assign(terms[i], h, envir = go2h)
          }else{
            aux <- get(terms[i], envir = go2h)
            aux <- c(aux, h)
            assign(terms[i], aux, envir = go2h)
          }
        }
      }
    }else{
      flag <- 0
    }
  }
}

####################################################

#' Title
#'
#' @param t The root GO term to build the GO tree
#' @keywords internal
#' @return Returns the GO tree for Molecular function
#'

GOTermsApp.M <- function(t){
  h <- 1
  assign(t, h, envir = go2h1)
  terms <- xx.ch1[[t]]
  terms <- terms[!is.na(terms)]
  L <- length(terms)
  if(L > 0){
    h <- h + 1
    for(i in 1:L){
      assign(terms[i], h, envir = go2h1)
    }
  }

  flag <- 1

  while(flag){
    L <- length(terms)
    if(L > 0){
      terms <- all_terms_level_h2(xx.ch1, terms)
      L <- length(terms)
      if(L > 0){
        h <- h + 1
        for(i in 1:L){
          f <- exists(terms[i], envir=go2h1)
          if(!f){
            assign(terms[i], h, envir = go2h1)
          }else{
            aux <- get(terms[i], envir = go2h1)
            aux <- c(aux, h)
            assign(terms[i], aux, envir = go2h1)
          }
        }
      }
    }else{
      flag <- 0
    }
  }
}

####  Build the Cellular Component Ontology (Go tree) ###

#' Title
#'
#' @param t The root GO term to build the GO tree
#' @keywords internal
#' @return Returns the GO tree for Cellular component
#'
GOTermsApp.C <- function(t){
  h <- 1
  assign(t, h, envir = go2h2)
  terms <- xx.ch2[[t]]
  terms <- terms[!is.na(terms)]
  L <- length(terms)
  if(L > 0){
    h <- h + 1
    for(i in 1:L){
      assign(terms[i], h, envir = go2h2)
    }
  }

  flag <- 1

  while(flag){
    L <- length(terms)
    if(L > 0){
      terms <- all_terms_level_h2(xx.ch2, terms)
      L <- length(terms)
      if(L > 0){
        h <- h + 1
        for(i in 1:L){
          f <- exists(terms[i], envir=go2h2)
          if(!f){
            assign(terms[i], h, envir = go2h2)
          }else{
            aux <- get(terms[i], envir = go2h2)
            aux <- c(aux, h)
            assign(terms[i], aux, envir = go2h2)
          }
        }
      }
    }else{
      flag <- 0
    }
  }
}

