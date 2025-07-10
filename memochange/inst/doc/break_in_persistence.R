## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = TRUE--------------------------------------------------------------
oil=data.table::fread("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=MCOILWTICO&scale=left&cosd=1986-01-01&coed=2019-08-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2009-06-01&line_index=1&transformation=lin&vintage_date=2019-09-23&revision_date=2019-09-23&nd=1986-01-01")

## ----echo = TRUE, fig.height = 4, fig.width = 7, fig.align = "center"---------
oil=as.data.frame(oil)
oil$observation_date=zoo::as.Date(oil$observation_date)
oil_xts=xts::xts(oil[,-1],order.by = oil$observation_date)
zoo::plot.zoo(oil_xts, xlab="", ylab="Price", main="Crude Oil Price: West Texas Intermediate")

## ----echo = TRUE--------------------------------------------------------------
library(memochange)
x <- as.numeric(oil[,2])

## ----echo = TRUE--------------------------------------------------------------
ratio_test(x)

## ----echo = TRUE--------------------------------------------------------------
ratio_test(x, type="HLT")

## ----echo = TRUE--------------------------------------------------------------
MR_test(x)

## ----echo = TRUE--------------------------------------------------------------
MR_test(x, statistic="standard")

## ----echo = TRUE--------------------------------------------------------------
MR_test(x, serial=TRUE)

## ----echo = TRUE--------------------------------------------------------------
BP_estim(x, direction="01")

## ----echo = TRUE--------------------------------------------------------------
oil$DATE[151]

## ----echo = TRUE--------------------------------------------------------------
BP_estim(x, direction="01", type="LKT", m=0)

## ----echo = TRUE--------------------------------------------------------------
BP_estim(x, direction="01", d_estim="GPH")

## ----echo = TRUE--------------------------------------------------------------
BP_estim(x, direction="01", d_bw=0.75)
BP_estim(x, direction="01", d_bw=0.65)

