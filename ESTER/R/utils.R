# Quiets concerns of R CMD check concerning the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c(".") )
