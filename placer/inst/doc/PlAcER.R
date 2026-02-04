## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

# Function to install packages if not installed
check.packages <- function(pkg){
        #new.pkg <- pkg[!(pkg %in% find.package()[, "Package"])]
        new.pkg <- pkg[!grepl(paste0(pkg,sep="|",collapse = ""), find.package(pkg))]
        if (length(new.pkg)) 
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)}

# Load needed packages
packages<-c("dplyr", "knitr", "kableExtra")
check.packages(packages)


## ---- echo=FALSE---------------------------------------------------------
library("placer"); library(rmarkdown)
library(knitr);
# data(ctern)
ctern[1:10,] %>% tbl_df %>% select(-nest_code) %>% kable(align=rep('c', 8), linesep = "\\addlinespace", booktabs = TRUE)
#  row_spec(0, bold = T, color = "white", background = "gray")

## ----plots, echo=FALSE, fig.cap = "Plastic Prevalence Probability in nests of Caspian tern"----
library(knitr)    # For knitting document and include_graphics function
ternsci<-placer::plastic.ci(ctern$debris_presence, 60, 100) 
placer::prevalence_plot(ternsci$prevprob,
ternsci$cidtf$N,
ternsci$cidtf$lower_ci,
ternsci$cidtf$upper_ci)

