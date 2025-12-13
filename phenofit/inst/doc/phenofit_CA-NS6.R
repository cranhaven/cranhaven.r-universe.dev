## ----load pkg, message=FALSE, echo=FALSE--------------------------------------
library(data.table)
library(lubridate)
library(purrr)
library(ggplot2)
library(phenofit)

## ----phenofit_parameters------------------------------------------------------
# lambda   <- 5    # non-parameter Whittaker, only suit for 16-day. Other time-scale
# should assign a lambda.
ymax_min   <- 0.1  # the maximum ymax shoud be greater than `ymax_min` 
rymin_less <- 0.8  # trough < ymin + A*rymin_less
nptperyear <- 23   # How many points for a single year
wFUN       <- wBisquare #wTSM #wBisquare # Weights updating function, could be one of `wTSM`, 'wBisquare', `wChen` and `wSELF`.

## ----tidy MOD13A1-------------------------------------------------------------
data('MOD13A1')
df <- MOD13A1$dt 
st <- MOD13A1$st

df[, `:=`(date = ymd(date), year = year(date), doy = as.integer(yday(date)))]
df[is.na(DayOfYear), DayOfYear := doy] # If DayOfYear is missing
    
# In case of last scene of a year, doy of last scene could in the next year
df[abs(DayOfYear - doy) >= 300, t := as.Date(sprintf("%d-%03d", year+1, DayOfYear), "%Y-%j")] # last scene
df[abs(DayOfYear - doy) <  300, t := as.Date(sprintf("%d-%03d", year  , DayOfYear), "%Y-%j")]

df <- df[!duplicated(df[, .(site, t)]), ]
# # MCD12Q1.006 land cover 1-17, IGBP scheme
# IGBPnames_006 <- c("ENF", "EBF", "DNF", "DBF", "MF" , "CSH", 
#               "OSH", "WSA", "SAV", "GRA", "WET", "CRO", 
#               "URB", "CNV", "SNOW", "BSV", "water", "UNC")
# Initial weights
df[, c("QC_flag", "w") := qc_summary(SummaryQA)]
df <- df[, .(site, y = EVI/1e4, t, date, w, QC_flag)]

## ----load_data----------------------------------------------------------------
sites        <- unique(df$site)
sitename     <- sites[3]
d            <- df[site == sitename] # get the first site data
sp           <- st[site == sitename]

south      <- sp$lat < 0
print      <- FALSE # whether print progress
IsPlot     <- TRUE  # for brks

prefix_fig <- "phenofit"
titlestr   <- with(sp, sprintf('[%03d,%s] %s, lat = %5.2f, lon = %6.2f',
                                     ID, site, IGBPname, lat, lon))
file_pdf   <- sprintf('Figure/%s_[%03d]_%s.pdf', prefix_fig, sp$ID[1], sp$site[1])

## ----interp Tn, eval=F--------------------------------------------------------
# d$Tn %<>% zoo::na.approx(maxgap = 4)
# plot(d$Tn, type = "l"); abline(a = 5, b = 0, col = "red")

## ----check_input--------------------------------------------------------------
dnew  <- add_HeadTail(d, south, nptperyear = 23) # add additional one year in head and tail
INPUT <- check_input(dnew$t, dnew$y, dnew$w, dnew$QC_flag,
                     nptperyear, south, 
                     maxgap = nptperyear/4, alpha = 0.02, wmin = 0.2)

## ----divide growing season----------------------------------------------------
par(mar = c(3, 2, 2, 1), mgp = c(3, 0.6, 0))
lambda <- init_lambda(INPUT$y)
# The detailed information of those parameters can be seen in `season`.
# brks   <- season(INPUT, nptperyear,
#                FUN = smooth_wWHIT, wFUN = wFUN, iters = 2,
#                lambda = lambda,
#                IsPlot = IsPlot, plotdat = d,
#                south = d$lat[1] < 0,
#                rymin_less = 0.6, ymax_min = ymax_min,
#                max_MaxPeaksperyear =2.5, max_MinPeaksperyear = 3.5) #, ...
# get growing season breaks in a 3-year moving window
brks2 <- season_mov(INPUT, 
    list(rFUN = "smooth_wWHIT", wFUN = wFUN, maxExtendMonth = 6, r_min = 0.1))
plot_season(INPUT, brks2)

## ----curve fitting, fig.height=7, fig.align="center"--------------------------
fit  <- curvefits(INPUT, brks2,
    options = list(
        methods = c("AG", "Zhang", "Beck", "Elmore"), #,"klos",, 'Gu'
        wFUN = wFUN,
        nextend = 2, maxExtendMonth = 3, minExtendMonth = 1, minPercValid = 0.2
    ))

## check the curve fitting parameters
l_param <- get_param(fit)
print(str(l_param, 1))
print(l_param$AG)

d_fit <- get_fitting(fit)
## Get GOF information
d_gof <- get_GOF(fit)
# fit$stat <- stat
print(head(d_gof))

# print(fit$fits$AG$`2002_1`$ws)
print(fit$`2002_1`$fFIT$AG$ws)
## visualization
g <- plot_curvefits(d_fit, brks2, NULL, ylab = "NDVI", "Time",
                   theme = coord_cartesian(xlim = c(ymd("2000-04-01"), ymd("2017-07-31"))))
grid::grid.newpage(); grid::grid.draw(g)# plot to check the curve fitting
# write_fig(g, "Figure1_phenofit_curve_fitting.pdf", 10, 6)

## ----Extract phenology, fig.height=5, fig.width=8, fig.align="center"---------
# pheno: list(p_date, p_doy)
l_pheno <- get_pheno(fit, IsPlot = F) #%>% map(~melt_list(., "meth"))

# ratio = 1.15
# file <- "Figure5_Phenology_Extraction_temp.pdf"
# cairo_pdf(file, 8*ratio, 6*ratio)
# temp <- get_pheno(fit$fits$ELMORE[2:6], IsPlot = T)
# dev.off()
# file.show(file)

## check the extracted phenology
pheno <- get_pheno(fit[1:6], "Elmore", IsPlot = T)
# print(str(pheno, 1))
head(l_pheno$doy$AG)

