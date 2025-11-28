## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5, 
  fig.width = 4, 
  fig.align = 'center'
)

## ---- message=F, warning=F----------------------------------------------------
library(letsR)

## ---- fig.width=6, fig.height=4-----------------------------------------------
data(temp)
r <- terra::unwrap(temp) # example data

plot(r)

## -----------------------------------------------------------------------------
data(PAM)
plot(PAM, main = "Phyllomedusa\nRichness")

## -----------------------------------------------------------------------------
PAM_env <- lets.addvar(PAM, r, fun = mean)

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
library(knitr)
library(dplyr)
library(kableExtra)

## ---- eval=FALSE--------------------------------------------------------------
#  head(PAM_env)

## ---- echo = FALSE------------------------------------------------------------
kable(head(PAM_env), "html") %>%
  kable_styling() %>%
  scroll_box(width = "800px", height = "400px")

## -----------------------------------------------------------------------------
climate <- lets.addvar(PAM, r, fun = mean, onlyvar = TRUE)

## ---- eval=F------------------------------------------------------------------
#  head(climate)

## ---- echo = FALSE------------------------------------------------------------
kable(head(climate), "html") %>%
  kable_styling()

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(ggplot2)

## ---- warning = FALSE, message = FALSE, fig.width = 6-------------------------
rich <- rowSums(PAM$P[, -(1:2)])

mpg1 <- data.frame("Temperature" = climate[, 1]/10,
                   "Richness" = rich)
ggplot(mpg1, aes(Temperature, Richness)) + 
  geom_smooth() + 
  geom_point(col = rgb(0, 0, 0, .6)) + 
  theme_bw()

## ---- warning = FALSE---------------------------------------------------------
data("wrld_simpl")
SA <- c("Brazil", "Colombia",  "Argentina",
        "Peru", "Venezuela", "Chile",
        "Ecuador", "Bolivia", "Paraguay",
        "Uruguay", "Guyana", "Suriname",
        "French Guiana")
south_ame <- wrld_simpl[wrld_simpl$NAME %in% SA, ]
ggplot(data = south_ame) +
  geom_sf() +
  geom_sf_text(aes(label = ISO3)) +
  theme_bw()

## -----------------------------------------------------------------------------
PAM_pol <- lets.addpoly(PAM, south_ame, "NAME")

## ---- eval=F------------------------------------------------------------------
#  head(PAM_pol)

## ---- echo = FALSE------------------------------------------------------------
kable(head(PAM_pol), "html") %>%
  kable_styling() %>%
  scroll_box(width = "800px", height = "400px")

## -----------------------------------------------------------------------------
vars_col <- (ncol(PAM$P) + 1):ncol(PAM_pol)
n <- length(vars_col)
rich_count <- numeric(n)
for (i in 1:n) {
  rich_count[i] <- sum(colSums(PAM$P[PAM_pol[, vars_col[i]] > 0,
                                     -(1:2)]) > 0)
}
labs <- as.factor(colnames(PAM_pol)[vars_col])
names(rich_count) <- labs

## ---- fig.width = 7-----------------------------------------------------------
mpg <- data.frame("Richness" = rich_count, "Country" = as.factor(labs))
g <- ggplot(mpg, aes(labs, Richness))
g + geom_bar(stat = "identity") + labs(x = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

