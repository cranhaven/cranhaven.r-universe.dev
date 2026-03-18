## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo= FALSE--------------------------------------------------------------

tbls <- tibble::tibble(Function= paste0(c("abgd", "asap", "bgmyc", "gmyc", "locmin", "mptp"), "_tbl()"),
                       Software= c("ABGD", "ASAP", "bGMYC", "splits", "spider", "mptp"),
                       Language= c("C", "C", "R", "R" , "R", "C"),
                       Source= c("bioinfo.mnhn.fr/abi/public/abgd/abgdweb.html",
                                 "bioinfo.mnhn.fr/abi/public/asap/",
                                 "https://nreid.github.io/software/",
                                 "https://splits.r-forge.r-project.org/",
                                 "https://github.com/boopsboops/spider",
                                 "https://github.com/Pas-Kapli/mptp"))
knitr::kable(tbls, align = "llcl")


## ----eval= FALSE--------------------------------------------------------------
# install.packages("spider")
# 

## ----eval= FALSE--------------------------------------------------------------
# install.packages("splits", repos = "https://r-forge.r-project.org/", dependencies = TRUE)
# 

## ----eval= FALSE--------------------------------------------------------------
# install.packages("bGMYC", repos = "https://pedrosenna.github.io/drat/")
# 

