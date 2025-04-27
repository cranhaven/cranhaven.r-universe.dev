## ---- eval=FALSE--------------------------------------------------------------
#  \dontrun{install.packages("LDAShiny")}

## ---- eval = FALSE------------------------------------------------------------
#  \dontrun{LDAShiny::runLDAShiny()}

## ----fig2,fig.cap='Figure 1. Search in Scopus ',fig.align='center', echo=FALSE, out.width = '90%'----
knitr::include_graphics("../inst/img/Fig2.jpg")

## ----fig3,fig.cap='Figure 2. Steps to export',fig.align='center', echo=FALSE, out.width = '90%'----
knitr::include_graphics("../inst/img/Fig3.jpg")

## ----eval=FALSE---------------------------------------------------------------
#  urlfile="https://raw.githubusercontent.com/JavierDeLaHoz/o.niloticus/main/O.niloticus.csv"
#  O.niloticus <-read_csv(url(urlfile))
#  O.niloticus <- write.csv(O.niloticus,file="O.niloticus.csv")

