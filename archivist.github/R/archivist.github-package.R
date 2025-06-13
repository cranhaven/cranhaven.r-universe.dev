##  archivist package for R
##  archivist.github package for R
##
#' @title Tools for Archiving, Managing and Sharing R Objects via GitHub
#'
#' @description
#' GitHub integration extension of the \pkg{archivist} package - tool for storing, 
#' restoring and searching for R objects. More about \pkg{archivis.github} can be found on 
#' \href{http://marcinkosinski.github.io/archivist.github/}{marcinkosinski.github.io/archivist.github/}
#'
#' @details
#' More about \pkg{archivis.github} can be found on 
#' \href{http://marcinkosinski.github.io/archivist.github/}{marcinkosinski.github.io/archivist.github/} 
#' and about \pkg{archivist} in posts' history on \href{https://pbiecek.github.io/archivist/articles/posts.html}{https://pbiecek.github.io/archivist/articles/posts.html}
#' 
#' @author
#' Marcin Kosinski [aut, cre] \email{m.p.kosinski@@gmail.com} \cr
#' Przemyslaw Biecek [aut] \email{przemyslaw.biecek@@gmail.com} 
#' 
#' @note 
#' Bug reports and feature requests can be sent to \href{https://github.com/MarcinKosinski/archivist.github/issues}{https://github.com/MarcinKosinski/archivist.github/issues}
#' 
#' @references 
#' More about \pkg{archivist.github} can be found on 
#' \href{http://marcinkosinski.github.io/archivist.github/}{marcinkosinski.github.io/archivist.github/} 
#' and about \pkg{archivist} in posts' history on \href{https://pbiecek.github.io/archivist/articles/posts.html}{https://pbiecek.github.io/archivist/articles/posts.html}
#' 
#' @section Posts:
#' 
#' This package is well explained on this \href{http://r-bloggers.com/r-hero-saves-backup-city-with-archivist-and-github}{http://r-bloggers.com/r-hero-saves-backup-city-with-archivist-and-github} blog post.
#' 
#' @importFrom httr DELETE
#' @importFrom httr config
#' @importFrom httr POST
#' @importFrom httr oauth_app
#' @importFrom httr oauth2.0_token
#' @importFrom httr oauth_endpoints
#' @importFrom jsonlite unbox
#' @importFrom digest digest
#' @importFrom utils tail
#' @importFrom utils getAnywhere
#' @import archivist
#' @importFrom git2r add
#' @importFrom git2r commit
#' @importFrom git2r push
#' @importFrom git2r pull
#' @importFrom git2r init
#' @importFrom git2r remote_add
#' @importFrom git2r clone
#' @importFrom git2r repository
#' @importFrom git2r discover_repository
#' @importFrom git2r in_repository
#' @importFrom git2r cred_user_pass
#'
#'
#' @family archivist.github
#' @name archivist.github-package
#' @docType package
invisible(NULL)
