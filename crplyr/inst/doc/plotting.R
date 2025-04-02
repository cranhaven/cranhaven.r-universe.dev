## ----message=FALSE------------------------------------------------------------
library(crunch)
library(crplyr)
library(ggplot2)

## ----include=FALSE------------------------------------------------------------
library(httptest)
set_crunch_opts(crunch.api = "https://app.crunch.io/api/")
start_vignette("plotting")

## ----setup, include = FALSE---------------------------------------------------
ds <- loadDataset("https://app.crunch.io/api/datasets/5c9336/")

## ----basic-card-plots, fig.width=8, fig.height=6------------------------------
autoplot(ds$CompanySize)

## ----CA-plotting,  fig.width=8, fig.height=6----------------------------------
autoplot(ds$ImportantHiring)
autoplot(ds$ImportantHiring, "tile")
autoplot(ds$ImportantHiring, "bar")

## ----themes, fig.height=6, fig.width=8, warning=FALSE-------------------------
p <- autoplot(ds$CompanySize)
p + theme_grey()
p +
    geom_point(color = "red", size = 3) +
    scale_x_log10()
autoplot(ds$ImportantHiring) +
    scale_color_brewer()

## ----fig.width= 10,  fig.width=8, fig.height=6--------------------------------
ds %>%
    group_by(CompanySize, Professional, TabsSpaces) %>%
    summarize(count = n(Country)) %>%
    autoplot()

## -----------------------------------------------------------------------------
cube <- crtabs(~ Country + Professional + TabsSpaces, ds)
cube_tbl <- as_tibble(cube)
cube_tbl

## ----fig.width=8, fig.height=6------------------------------------------------
cube_tbl %>%
    filter(
        !is_missing,
        Professional == "Professional developer") %>%
    arrange(desc(count)) %>%
    top_n(10) %>%
    ggplot(aes(y = Country, x = count, color = TabsSpaces)) +
    geom_point() +
    theme_crunch()

## ----fig.width=8, fig.height=6------------------------------------------------
cube <- crtabs(~ WantWorkMR + ImportantHiring, ds)
cube_tbl <- as_tibble(cube)
cube_tbl

## ----include=FALSE------------------------------------------------------------
end_vignette()

