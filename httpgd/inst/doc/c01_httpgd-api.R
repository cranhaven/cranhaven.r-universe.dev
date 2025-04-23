## ----echo=FALSE---------------------------------------------------------------
df <- unigd::ugd_renderers()
df <- df[order(df$id),]
knitr::kable(data.frame(
   sprintf("`%s`", df$id),
   sprintf("`%s`", df$mime),
   df$descr,
   ifelse(df$text, "Text", "Binary")
), col.names = c("ID", "Mime-Type", "Renderer", "Format"))

