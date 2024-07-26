params <-
list(EVAL = FALSE)

## ----extn_sel_2, echo = FALSE, message = FALSE----------------------------------------------------

# load packages
library(warbleR)
library(knitr)

cf <- read.csv("function_descrip_table.csv", stringsAsFactors = FALSE)


data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))


warbleR_options(wav.path = tempdir())

options(knitr.table.format = "html")
knitr::opts_chunk$set(
  comment = "",
  fig.width = 5, 
  fig.height = 3.5,
  dpi = 40,
  out.width = "80%"
)
opts_knit$set(root.dir = tempdir())
options(width = 100, max.print = 100)

## ----extn_sel_4.1, eval=FALSE---------------------------------------------------------------------
#  
#  data("lbh_selec_table")
#  
#  lbh_selec_table

## ----extn_sel_4.2, echo=FALSE, eval = TRUE--------------------------------------------------------

library(kableExtra)
kbl <- kable(lbh_selec_table, align = "c", row.names = F, format = "html")

kbl <- kable_styling(kbl, bootstrap_options = "striped", font_size = 14)

kbl <- scroll_box(kbl,
  width = "740px",
  box_css = "border: 1px solid #ddd; padding: 1px; ", extra_css = NULL
)

kbl

## ----eval = FALSE, echo = FALSE-------------------------------------------------------------------
#  
#  library(warbleR)
#  
#  wf <- ls("package:warbleR")
#  
#  wf <- wf[-c(2, 7, 8, 10, 12, 16, 17, 19, 20, 23, 24, 28, 31, 32, 33, 38, 42, 43, 44, 47, 50, 53, 59, 64, 66, 68, 68, 72, 74, 80, 81, 85, 90, 93, 94, 96)]
#  
#  df <- data.frame(funciones = wf, `Obtener-preparar grabaciones` = "", `Anotar` = "", `Medir` = "", `Revision` = "", `Inspeccion visual` = "", `Analisis estadistico` = "", `Otros` = "")
#  
#  df2 <- edit(df)
#  
#  
#  df2$`organizar.anotaciones` <- ""
#  
#  names(df2) <- names(df2)[c(1:3, 9, 4:8)]
#  
#  df3 <- edit(df2)
#  
#  df4 <- df3
#  
#  df4[is.na(df4)] <- ""
#  
#  df4 <- df4[df4$Obtener.preparar.grabaciones != "borrar", ]
#  
#  names(df4) <- c("Funcion", "Obtener-preparar grabaciones", "Anotar", "Organizar anotaciones", "Medir estructura", "Verificar", "Inspeccion visual", "Analisis estadistico", "Otros")
#  
#  rownames(df4) <- 1:nrow(df4)
#  
#  df5 <- df4[order(df4$`Obtener-preparar grabaciones`, df4$Anotar, df4$`Organizar anotaciones`, df4$`Medir estructura`, df4$Verificar, df4$`Inspeccion visual`, df4$`Analisis estadistico`, df4$Otros, decreasing = TRUE), ]
#  
#  df4 <- df4[c(5, 8, 18, 29, 34, 35, 37, 38, 39, 55, 56, 26, 1, 19, 40, 46, 4, 11, 16, 17, 24, 25, 32, 41, 45, 7, 12, 13, 14, 15, 23, 27, 30, 42, 47, 48, 57, 2, 3, 28, 44, 50, 51, 52, 58, 9, 10, 21, 22, 59, 6, 20, 31, 33, 36, 43, 49, 53, 54), ]
#  
#  # write.csv(df4, "cuadro de funciones warbleR.csv", row.names = FALSE)

## ----echo = FALSE, eval = TRUE--------------------------------------------------------------------

library(kableExtra)

names(cf) <- gsub("\\.", " ", names(cf))

cf2 <- cf[cf$`Obtener preparar grabaciones` == "x", c("Function", "Description", "Works on", "Output")]

cf2$Function <- cell_spec(x = cf2$Function, link = paste0("https://marce10.github.io/warbleR/reference/", cf2$Function, ".html"))

kbl <- kable(cf2, align = "c", row.names = F, format = "html", escape = F)

kbl <- column_spec(kbl, 1, bold = TRUE)

kbl <- column_spec(kbl, 2:4, italic = TRUE)

kbl <- kable_styling(kbl, bootstrap_options = "striped", font_size = 14)

kbl

## ----echo = FALSE, eval = TRUE--------------------------------------------------------------------

cf2 <- cf[cf$Anotar == "x", c("Function", "Description", "Works on", "Output")]

cf2$Function <- cell_spec(x = cf2$Function, link = paste0("https://marce10.github.io/warbleR/reference/", cf2$Function, ".html"))

kbl <- kable(cf2, align = "c", row.names = F, format = "html", escape = F)

kbl <- column_spec(kbl, 1, bold = TRUE)

kbl <- column_spec(kbl, 2:4, italic = TRUE)

kbl <- kable_styling(kbl, bootstrap_options = "striped", font_size = 14)

kbl

## ----echo = FALSE, eval = TRUE--------------------------------------------------------------------

cf2 <- cf[cf$`Organizar anotaciones` == "x", c("Function", "Description", "Works on", "Output")]

cf2$Function <- cell_spec(x = cf2$Function, link = paste0("https://marce10.github.io/warbleR/reference/", cf2$Function, ".html"))

kbl <- kable(cf2, align = "c", row.names = F, format = "html", escape = F)

kbl <- column_spec(kbl, 1, bold = TRUE)

kbl <- column_spec(kbl, 2:4, italic = TRUE)

kbl <- kable_styling(kbl, bootstrap_options = "striped", font_size = 14)

kbl

## ----echo = FALSE, eval = TRUE--------------------------------------------------------------------

cf2 <- cf[cf$`Medir estructura` == "x", c("Function", "Description", "Works on", "Output")]

cf2$Function <- cell_spec(x = cf2$Function, link = paste0("https://marce10.github.io/warbleR/reference/", cf2$Function, ".html"))

kbl <- kable(cf2, align = "c", row.names = F, format = "html", escape = F)

kbl <- column_spec(kbl, 1, bold = TRUE)

kbl <- column_spec(kbl, 2:4, italic = TRUE)

kbl <- kable_styling(kbl, bootstrap_options = "striped", font_size = 14)

kbl

## ----echo = FALSE, eval = TRUE--------------------------------------------------------------------

cf2 <- cf[cf$Verificar == "x", c("Function", "Description", "Works on", "Output")]

cf2$Function <- cell_spec(x = cf2$Function, link = paste0("https://marce10.github.io/warbleR/reference/", cf2$Function, ".html"))

kbl <- kable(cf2, align = "c", row.names = F, format = "html", escape = F)

kbl <- column_spec(kbl, 1, bold = TRUE)

kbl <- column_spec(kbl, 2:4, italic = TRUE)

kbl <- kable_styling(kbl, bootstrap_options = "striped", font_size = 14)

kbl

## ----echo = FALSE, eval = TRUE--------------------------------------------------------------------

cf2 <- cf[cf$`Inspeccion visual` == "x", c("Function", "Description", "Works on", "Output")]

cf2$Function <- cell_spec(x = cf2$Function, link = paste0("https://marce10.github.io/warbleR/reference/", cf2$Function, ".html"))

kbl <- kable(cf2, align = "c", row.names = F, format = "html", escape = F)

kbl <- column_spec(kbl, 1, bold = TRUE)

kbl <- column_spec(kbl, 2:4, italic = TRUE)

kbl <- kable_styling(kbl, bootstrap_options = "striped", font_size = 14)

kbl

## ----echo = FALSE, eval = TRUE--------------------------------------------------------------------

cf2 <- cf[cf$`Analisis estadistico` == "x" | cf$Otros == "x", c("Function", "Description", "Works on", "Output")]

cf2$Function <- cell_spec(x = cf2$Function, link = paste0("https://marce10.github.io/warbleR/reference/", cf2$Function, ".html"))

kbl <- kable(cf2, align = "c", row.names = F, format = "html", escape = F)

kbl <- column_spec(kbl, 1, bold = TRUE)

kbl <- column_spec(kbl, 2:4, italic = TRUE)

kbl <- kable_styling(kbl, bootstrap_options = "striped", font_size = 14)

kbl

## ----session info, echo=F-------------------------------------------------------------------------

sessionInfo()

