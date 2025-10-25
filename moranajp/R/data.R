#' The first part of 'I Am a Cat' by Soseki Natsume
#'
#' @format A data frame with 9 rows and 1 variable: 
#' \describe{
#'   \item{text}{Body text. Escaped by stringi::stri_escape_unicode().}
#' }
#' @examples
#' data(neko)
#' neko |>
#'   unescape_utf()
"neko"

#' Analyzed data of neko by MeCab
#'
#' MeCab: https://taku910.github.io/mecab/
#'
#' @format A data frame with  2884 rows and 11 variable: 
#' (column names are escaped by stringi::stri_escape_unicode(), 
#' stringi::stri_unescape_unicode() or unescape_utf() will show Japanese)
#' \describe{
#'   \item{text_id}{id}
#'   \item{\\u8868\\u5c64\\u5f62}{result of MeCab}
#'   \item{\\u54c1\\u8a5e}{result of MeCab}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e1}{result of MeCab}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e2}{result of MeCab}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e3}{result of MeCab}
#'   \item{\\u6d3b\\u7528\\u578b}{result of MeCab}
#'   \item{\\u6d3b\\u7528\\u5f62}{result of MeCab}
#'   \item{\\u539f\\u5f62}{result of MeCab}
#'   \item{\\u8aad\\u307f}{result of MeCab}
#'   \item{\\u767a\\u97f3}{result of MeCab}
#' }
#' @examples
#' data(neko_mecab)
#' neko_mecab |>
#'   unescape_utf()
"neko_mecab"

#' Analyzed data of neko by GiNZA
#'
#' GiNZA: https://megagonlabs.github.io/ginza/
#' 
#' @format A data frame with 2945 rows and 13 variable: 
#' \describe{
#'   \item{text_id}{id}
#'   \item{id}{result of GiNZA}
#'   \item{\\u8868\\u5c64\\u5f62}{result of GiNZA}
#'   \item{\\u539f\\u5f62}{result of GiNZA}
#'   \item{UD\\u54c1\\u8a5e\\u30bf\\u30b0}{result of GiNZA}
#'   \item{\\u54c1\\u8a5e}{result of GiNZA}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e1}{result of GiNZA}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e2}{result of GiNZA}
#'   \item{\\u5c5e\\u6027}{result of GiNZA}
#'   \item{\\u4fc2\\u53d7\\u5143}{result of GiNZA}
#'   \item{\\u4fc2\\u53d7\\u30bf\\u30b0}{result of GiNZA}
#'   \item{\\u4fc2\\u53d7\\u30da\\u30a2}{result of GiNZA}
#'   \item{\\u305d\\u306e\\u4ed6}{result of GiNZA}
#' }
#' @examples
#' data(neko_ginza)
#' neko_ginza |>
#'   unescape_utf()
"neko_ginza"

#' Analyzed data of neko by Sudachi
#'
#' Sudachi: https://github.com/WorksApplications/Sudachi
#' 
#' @format A data frame with 3130 rows and 9 variable: 
#' \describe{
#'   \item{text_id}{id}
#'   \item{\\u8868\\u5c64\\u5f62}{result of Sudachi}
#'   \item{\\u54c1\\u8a5e}{result of Sudachi}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e1}{result of Sudachi}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e2}{result of Sudachi}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e3}{result of Sudachi}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e4}{result of Sudachi}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e5}{result of Sudachi}
#'   \item{\\u539f\\u5f62}{result of Sudachi}
#' }
#' @examples
#' data(neko_sudachi_a)
#' neko_sudachi_a |>
#'   unescape_utf()
"neko_sudachi_a"

#' @rdname neko_sudachi_a
#' @format A data frame with 3088 rows and 9 variable: 
"neko_sudachi_b"

#' @rdname neko_sudachi_a
#' @format A data frame with 3080 rows and 9 variable: 
"neko_sudachi_c"

#' Analyzed data of neko by chamame
#'
#' chamame: https://chamame.ninjal.ac.jp/index.html
#'
#' @format A data frame with  2959 rows and 7 variable: 
#' (column names are escaped by stringi::stri_escape_unicode(), 
#' stringi::stri_unescape_unicode() or unescape_utf() will show Japanese)
#' \describe{
#'   \item{text_id}{id}
#'   \item{\\u8868\\u5c64\\u5f62}{result of chamame}
#'   \item{\\u54c1\\u8a5e}{result of chamame}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e1}{result of chamame}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e2}{result of chamame}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e3}{result of chamame}
#'   \item{\\u539f\\u5f62}{result of chamame}
#' }
#' @examples
#' data(neko_chamame)
#' neko_chamame |>
#'   unescape_utf()
"neko_chamame"

#' Full text of review article
#'
#' @format A data frame with 457 rows and 4 variables: 
#' \describe{
#'   \item{text}{Body text. Escaped by stringi::stri_escape_unicode().
#'     Body text. Escaped by stringi::stri_escape_unicode().
#'     Citation is as below. 
#'     Matsumura et al. 2014. 
#'       Conditions and conservation for biodiversity of the semi-natural 
#'       grassland vegetation on rice paddy levees.
#'       Vegetation Science, 31, 193-218.
#'       doi = 10.15031/vegsci.31.193
#'     https://www.jstage.jst.go.jp/article/vegsci/31/2/31_193/_article/-char/en
#'   }
#'   \item{chap}{chapter}
#'   \item{sect}{section}
#'   \item{para}{paragraph}
#' }
#' @examples
#' data(review)
#' review |>
#'   unescape_utf()
"review"

#' Analyzed data of review by MeCab
#'
#' MeCab: https://taku910.github.io/mecab/
#'
#' @format A data frame with  199985 rows and 14 variable: 
#' (column names are escaped by stringi::stri_escape_unicode(), 
#' stringi::stri_unescape_unicode() or unescape_utf() will show Japanese)
#' \describe{
#'   \item{text_id}{id}
#'   \item{chap}{chapter}
#'   \item{sect}{section}
#'   \item{para}{paragraph}
#'   \item{\\u8868\\u5c64\\u5f62}{result of MeCab}
#'   \item{\\u54c1\\u8a5e}{result of MeCab}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e1}{result of MeCab}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e2}{result of MeCab}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e3}{result of MeCab}
#'   \item{\\u6d3b\\u7528\\u578b}{result of MeCab}
#'   \item{\\u6d3b\\u7528\\u5f62}{result of MeCab}
#'   \item{\\u539f\\u5f62}{result of MeCab}
#'   \item{\\u8aad\\u307f}{result of MeCab}
#'   \item{\\u767a\\u97f3}{result of MeCab}
#' }
#' @examples
#' data(review_mecab)
#' review_mecab |>
#'   unescape_utf()
"review_mecab"

#' Analyzed data of review by GiNZA
#'
#' GiNZA: https://megagonlabs.github.io/ginza/
#' 
#' @format A data frame with 19514 rows and 16 variable: 
#' \describe{
#'   \item{text_id}{id}
#'   \item{chap}{chapter}
#'   \item{sect}{section}
#'   \item{para}{paragraph}
#'   \item{id}{result of GiNZA}
#'   \item{\\u8868\\u5c64\\u5f62}{result of GiNZA}
#'   \item{\\u539f\\u5f62}{result of GiNZA}
#'   \item{UD\\u54c1\\u8a5e\\u30bf\\u30b0}{result of GiNZA}
#'   \item{\\u54c1\\u8a5e}{result of GiNZA}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e1}{result of GiNZA}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e2}{result of GiNZA}
#'   \item{\\u5c5e\\u6027}{result of GiNZA}
#'   \item{\\u4fc2\\u53d7\\u5143}{result of GiNZA}
#'   \item{\\u4fc2\\u53d7\\u30bf\\u30b0}{result of GiNZA}
#'   \item{\\u4fc2\\u53d7\\u30da\\u30a2}{result of GiNZA}
#'   \item{\\u305d\\u306e\\u4ed6}{result of GiNZA}
#' }
#' @examples
#' data(review_ginza)
#' review_ginza |>
#'   unescape_utf()
"review_ginza"

#' Analyzed data of review by Sudachi
#'
#' Sudachi: https://github.com/WorksApplications/Sudachi
#' 
#' @format A data frame with 20100 rows and 12 variable: 
#' \describe{
#'   \item{text_id}{id}
#'   \item{chap}{chapter}
#'   \item{sect}{section}
#'   \item{para}{paragraph}
#'   \item{\\u8868\\u5c64\\u5f62}{result of Sudachi}
#'   \item{\\u54c1\\u8a5e}{result of Sudachi}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e1}{result of Sudachi}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e2}{result of Sudachi}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e3}{result of Sudachi}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e4}{result of Sudachi}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e5}{result of Sudachi}
#'   \item{\\u539f\\u5f62}{result of Sudachi}
#' }
#' @examples
#' data(review_sudachi_a)
#' review_sudachi_a |>
#'   unescape_utf()
"review_sudachi_a"

#' @rdname review_sudachi_a
#' @format A data frame with 19565 rows and 12 variable: 
"review_sudachi_b"

#' @rdname review_sudachi_a
#' @format A data frame with 19526 rows and 12 variable: 
"review_sudachi_c"

#' Analyzed data of review by chamame
#'
#' chamame: https://chamame.ninjal.ac.jp/index.html
#'
#' @format A data frame with  21125 rows and 10 variable 
#' (column names are escaped by stringi::stri_escape_unicode(), 
#' stringi::stri_unescape_unicode() or unescape_utf() will show Japanese)
#' \describe{
#'   \item{text_id}{id}
#'   \item{chap}{chapter}
#'   \item{sect}{section}
#'   \item{para}{paragraph}
#'   \item{\\u8868\\u5c64\\u5f62}{result of chamame}
#'   \item{\\u54c1\\u8a5e}{result of chamame}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e1}{result of chamame}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e2}{result of chamame}
#'   \item{\\u54c1\\u8a5e\\u7d30\\u5206\\u985e3}{result of chamame}
#'   \item{\\u539f\\u5f62}{result of chamame}
#' }
#' @examples
#' data(review_chamame)
#' review_chamame |>
#'   unescape_utf()
"review_chamame"

#' Stop words for morphological analysis
#'
#' @format A data frame with 310 rows and 1 variable: 
#' \describe{
#'   \item{stop_word}{
#'     Stop words can be used with delete_stop_words(). 
#'     Escaped by stringi::stri_escape_unicode().
#'     Downloaded from 
#'     http://svn.sourceforge.jp/svnroot/slothlib/CSharp/Version1/SlothLib/NLP/Filter/StopWord/word/Japanese.txt
#'   }
#' }
#' @examples
#' data(stop_words)
#' stop_words |>
#'   unescape_utf()
"stop_words"

#' An example of synonym word pairs
#'
#' @format A data frame with 25 rows and 2 variables: 
#' \describe{
#'   \item{from}{
#'     Words to be replaced from.
#'     Escaped by stringi::stri_escape_unicode().
#'   }
#'   \item{to}{
#'     Words to be replaced to.
#'   }
#' }
#' @examples
#' data(synonym)
#' synonym |>
#'   unescape_utf()
"synonym"
