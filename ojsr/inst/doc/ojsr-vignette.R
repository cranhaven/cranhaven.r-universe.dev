## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(knitr)

## ----fullexample , eval = FALSE-----------------------------------------------
# # NOT RUN {
# 
# library(ojsr)
# 
# journal <- 'https://revistapsicologia.uchile.cl/index.php/RDP/'
# 
# issues <- ojsr::get_issues_from_archive(input_url = journal)
# 
# articles <- ojsr::get_articles_from_issue(input_url = issues$output_url[1:2]) # only first 2 issues
# 
# metadata <- ojsr::get_html_meta_from_article(input_url = articles$output_url[1:5]) # only first 5 articles
# 
# # }
# 

## ----get_issues_from_archive , eval = FALSE-----------------------------------
# journal <- 'https://revistapsicologia.uchile.cl/index.php/RDP/'
# 
# issues <- ojsr::get_issues_from_archive(input_url = journal)
# 

## ----get_articles_from_issue , eval = FALSE-----------------------------------
# issue <- 'https://revistapsicologia.uchile.cl/index.php/RDP/issue/view/6031/'
# 
# articles <- ojsr::get_articles_from_issue(input_url = issue)
# 

## ----get_articles_from_search , eval = FALSE----------------------------------
# journal <- 'https://revistapsicologia.uchile.cl/index.php/RDP/'
# 
# criteria <- "psicologia+social"
# 
# articles_search <- ojsr::get_articles_from_search(input_url = journal, search_criteria = criteria)
# 

## ----get_galleys_from_article , eval = FALSE----------------------------------
# article <- 'https://dspace.palermo.edu/ojs/index.php/psicodebate/article/view/516/311' # inline reader
# 
# galleys <- ojsr::get_galleys_from_article(input_url = article)
# 

## ----get_html_meta_from_article , eval = FALSE--------------------------------
# article <- 'https://revistapsicologia.uchile.cl/index.php/RDP/article/view/75178'
# 
# metadata <- ojsr::get_html_meta_from_article(input_url = article)
# 

## ----get_oai_meta_from_article , eval = FALSE---------------------------------
# article <- 'https://dspace.palermo.edu/ojs/index.php/psicodebate/article/view/516/311' # xml galley
# 
# metadata_oai <- ojsr::get_oai_meta_from_article(input_url = article)
# 

## ----parse_base_url , eval = FALSE--------------------------------------------
# mix_links <- c(
#    'https://dspace.palermo.edu/ojs/index.php/psicodebate/issue/archive',
#    'https://revistapsicologia.uchile.cl/index.php/RDP/article/view/75178'
# )
# 
# base_url <- ojsr::parse_base_url(input_url = mix_links)
# 

## ----parse_oai_url , eval = FALSE---------------------------------------------
# mix_links <- c(
#    'https://dspace.palermo.edu/ojs/index.php/psicodebate/issue/archive',
#    'https://revistapsicologia.uchile.cl/index.php/RDP/article/view/75178'
# )
# 
# oai_url <- ojsr::parse_oai_url(input_url = mix_links)
# 

