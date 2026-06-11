## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = FALSE
)

## ---- eval = FALSE------------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("martakarass/adept")

## -----------------------------------------------------------------------------
library(adept)
library(magrittr)
library(ggplot2)

## ---- fig.width=2.5, fig.height=2.3-------------------------------------------
true.pattern <- cos(seq(0, 2 * pi, length.out = 100))
x <- c(true.pattern[1], replicate(10, true.pattern[-1]))

data.frame(x = seq(0, 1, length.out = 100), y = true.pattern) %>%
  ggplot() + geom_line(aes(x = x, y = y), color = "red") + 
  theme_bw(base_size = 9) + labs(x = "Phase", y = "Value", title = "Pattern")

## ---- fig.width=7, fig.height=2.3---------------------------------------------
data.frame(x = seq(0, by = 0.01, length.out = length(x)), y = x) %>%
  ggplot() + geom_line(aes(x = x, y = y)) +  theme_bw(base_size = 9) + 
  labs(x = "Time [s]", y = "Value", title = "Time-series x")

## -----------------------------------------------------------------------------
out <- segmentPattern(
  x = x,
  x.fs = 100,
  template = true.pattern,
  pattern.dur.seq = c(0.9, 0.95, 1.03, 1.1),
  similarity.measure = "cor",
  compute.template.idx = TRUE)
out

## -----------------------------------------------------------------------------
out <- segmentPattern(
  x = x,
  x.fs = 100,
  template = true.pattern,
  pattern.dur.seq = c(0.9, 0.95, 1, 1.03, 1.1),
  similarity.measure = "cor",
  compute.template.idx = TRUE)
out

## -----------------------------------------------------------------------------
out <- segmentPattern(
  x = x,
  x.fs = 10,   ## Assumed data frequency of 10 observations per second
  template = true.pattern,
  pattern.dur.seq = c(0.9, 0.95, 1, 1.03, 1.1) * 10, ## Adjusted accordingly
  similarity.measure = "cor",
  compute.template.idx = TRUE)
out

## ---- fig.width=7, fig.height=2.3---------------------------------------------
set.seed(1)
true.pattern <- cos(seq(0, 2 * pi, length.out = 200))
x <- numeric()
for (vl in seq(70, 130, by = 10)){
  true.pattern.s <- approx(
    seq(0, 1, length.out = 200), 
    true.pattern, xout = seq(0, 1, length.out = vl))$y
  x <- c(x, true.pattern.s[-1])
  if (vl == 70) x <- c(true.pattern.s[1], x)
}

data.frame(x = seq(0, by = 0.01, length.out = length(x)), y = x) %>%
  ggplot() + geom_line(aes(x = x, y = y)) +  theme_bw(base_size = 9) + 
  labs(x = "Time [s]", y = "Value", title = "Time-series x")

## Function to plot segmentation results with ggplot2
library(ggplot2)
out.plot1 <- function(val, out, fs = 100){
  yrange <- c(-1, 1) * max(abs(val))
  y.h <- 0
  plt <- ggplot()
  for (i in 1:nrow(out)){
    tau1_i <- out[i, "tau_i"]
    tau2_i <- tau1_i + out[i, "T_i"] - 1
    tau1_i <- tau1_i/fs
    tau2_i <- tau2_i/fs
    plt <- 
      plt + 
      geom_vline(xintercept = tau1_i, color = "red") + 
      geom_vline(xintercept = tau2_i, color = "red") + 
      annotate(
        "rect",
        fill = "pink", 
        alpha = 0.3,
        xmin = tau1_i, 
        xmax = tau2_i, 
        ymin = yrange[1],
        ymax = yrange[2]
    )
  }
  geom_line.df <- data.frame(x = seq(0, by = 1/fs, length.out = length(val)), y = val)
  plt <- 
    plt + 
    geom_line(data = geom_line.df, 
              aes(x = x, y = y), 
              color = "black", 
              size = 0.3) + 
    theme_bw(base_size = 9) + 
    labs(x = "Time [s]", y = "Black line: x",
         title = "Black line: signal x\nRed vertical lines: start and end points of identified pattern occurrence\nRed shaded area: area corresponding to identified pattern occurrence")
  plot(plt)
}

## ---- fig.width=7, fig.height=2.3---------------------------------------------
out <- segmentPattern(
  x = x,
  x.fs = 100,
  template = true.pattern,
  pattern.dur.seq = 60:130 * 0.01,
  similarity.measure = "cor",
  compute.template.idx = TRUE)
out

out.plot1(x, out)

## ---- fig.width=7, fig.height=2.3---------------------------------------------
out <- segmentPattern(
  x = x,
  x.fs = 100,
  template = true.pattern,
  pattern.dur.seq = c(0.6, 0.9, 1.2),
  similarity.measure = "cor",
  compute.template.idx = TRUE)
out

out.plot1(x, out)

## ---- fig.width=7, fig.height=2.3---------------------------------------------
out <- segmentPattern(
  x = x,
  x.fs = 100,
  template = true.pattern,
  pattern.dur.seq = c(0.6, 0.9, 1.2),
  similarity.measure = "cov",  ## Use covariance as a similarity statistic
  compute.template.idx = TRUE)
out

out.plot1(x, out)

## ---- fig.width=7, fig.height=2.3---------------------------------------------
true.pattern.1 <- cos(seq(0, 2 * pi, length.out = 200))
true.pattern.2 <- true.pattern.1
true.pattern.2[70:130] <- 2 * true.pattern.2[min(70:130)] + abs(true.pattern.2[70:130])
x <- numeric()
for (vl in seq(70, 130, by = 10)){
  true.pattern.1.s <- approx(
    seq(0, 1, length.out = 200), 
    true.pattern.1, xout = seq(0, 1, length.out = vl))$y
  true.pattern.2.s <- approx(
    seq(0, 1, length.out = 200), 
    true.pattern.2, xout = seq(0, 1, length.out = vl))$y
  x <- c(x, true.pattern.1.s[-1], true.pattern.2.s[-1])
  if (vl == 70) x <- c(true.pattern.1.s[1], x)
}

data.frame(x = seq(0, by = 0.01, length.out = length(x)), y = x) %>%
  ggplot() + geom_line(aes(x = x, y = y)) +  theme_bw(base_size = 9) + 
  labs(x = "Time [s]", y = "Value", title = "Time-series x")

## ---- fig.width=2.5, fig.height=2.3-------------------------------------------
plt1 <- 
  data.frame(x = seq(0, 1, length.out = length(true.pattern.1)), y = true.pattern.1) %>%
  ggplot() + geom_line(aes(x = x, y = y), color = "red") + 
  theme_bw(base_size = 9) + labs(x = "Phase", y = "Value", title = "Pattern 1") + 
  scale_y_continuous(limits = c(-1,1))
plt2 <- 
  data.frame(x = seq(0, 1, length.out = length(true.pattern.2)), y = true.pattern.2) %>%
  ggplot() + geom_line(aes(x = x, y = y), color = "red") + 
  theme_bw(base_size = 9) + labs(x = "Phase", y = "Value", title = "Pattern 2") + 
  scale_y_continuous(limits = c(-1,1))
plt1;plt2

## ---- fig.width=7, fig.height=2.3---------------------------------------------
out <- segmentPattern(
  x = x,
  x.fs = 100,
  template = true.pattern.1, ## Template consisting of one out of two true patterns
  pattern.dur.seq = 60:130 * 0.01,
  similarity.measure = "cor",
  compute.template.idx = TRUE)
out

out.plot1(x, out)

## ---- fig.width=7, fig.height=2.3---------------------------------------------
out <- segmentPattern(
  x = x,
  x.fs = 100,
  template = true.pattern.1,
  pattern.dur.seq = 60:130 * 0.01,
  similarity.measure = "cor",
  similarity.measure.thresh = 0.95, 
  compute.template.idx = TRUE)
out

out.plot1(x, out)

## ---- fig.width=7, fig.height=2.3---------------------------------------------
out <- segmentPattern(
  x = x,
  x.fs = 100,
  template = list(true.pattern.1, true.pattern.2),
  pattern.dur.seq = 60:130 * 0.01,
  similarity.measure = "cor",
  compute.template.idx = TRUE)
out

out.plot1(x, out)

## ---- fig.width=7, fig.height=2.3---------------------------------------------
## Generate `x` as a noisy version of a time-series generated in *Examples 3*.  
set.seed(1)
x <- x + rnorm(length(x), sd = 0.5)

data.frame(x = seq(0, by = 0.01, length.out = length(x)), y = x) %>%
  ggplot() + geom_line(aes(x = x, y = y), size = 0.3) +  theme_bw(base_size = 9) + 
  labs(x = "Time [s]", y = "Value", title = "Time-series x")

## ---- fig.width=7, fig.height=2.3---------------------------------------------
out <- segmentPattern(
  x = x,
  x.fs = 100,
  template = list(true.pattern.1, true.pattern.2),
  pattern.dur.seq =  60:130 * 0.01,
  similarity.measure = "cor",
  compute.template.idx = TRUE)
out

out.plot1(x, out)

## ---- fig.width=7, fig.height=2.3---------------------------------------------
x.smoothed <- windowSmooth(x = x, x.fs = 100, W = 0.1)
  
data.frame(x = seq(0, by = 0.01, length.out = length(x.smoothed)), y = x.smoothed) %>%
  ggplot() + geom_line(aes(x = x, y = y)) +  theme_bw(base_size = 9) + 
  labs(x = "Time [s]", y = "Value", title = "Time-series x smoothed")

## ---- fig.width=7, fig.height=2.3---------------------------------------------
out <- segmentPattern(
  x = x,
  x.fs = 100,
  template = list(true.pattern.1, true.pattern.2),
  pattern.dur.seq =  60:130 * 0.01,
  similarity.measure = "cor",
  x.adept.ma.W = 0.1,
  compute.template.idx = TRUE)
out

out.plot1(x, out)

## ---- fig.width=7, fig.height=2.3---------------------------------------------
out <- segmentPattern(
  x = x,
  x.fs = 100,
  template = list(true.pattern.1, true.pattern.2),
  pattern.dur.seq =  60:130 * 0.01,
  x.adept.ma.W = 0.1,
  finetune = "maxima",
  finetune.maxima.nbh.W = 0.3,
  compute.template.idx = TRUE)
out

out.plot1(x, out)

## ---- fig.width=7, fig.height=2.3---------------------------------------------
x.smoothed.2 <- windowSmooth(x = x, x.fs = 100, W = 0.5)

data.frame(x = seq(0, by = 0.01, length.out = length(x.smoothed.2)), y = x.smoothed.2) %>%
  ggplot() + geom_line(aes(x = x, y = y)) +  theme_bw(base_size = 9) + 
  labs(x = "Time [s]", y = "Value", title = "Time-series x smoothed aggresively")

## ---- fig.width=7, fig.height=2.3---------------------------------------------
out <- segmentPattern(
  x = x,
  x.fs = 100,
  template = list(true.pattern.1, true.pattern.2),
  pattern.dur.seq =  60:130 * 0.01,
  similarity.measure = "cor",
  x.adept.ma.W = 0.1,  ## smoothing parameter for similarity matrix computation
  finetune = "maxima",  ## use fine-tuning
  finetune.maxima.ma.W = 0.5, ## smoothing parameter for peak detection in fine-tuning
  finetune.maxima.nbh.W = 0.3,  ## neighborhoods length in fine-tuning
  compute.template.idx = TRUE)
out

## -----------------------------------------------------------------------------
## Function to plot nice results visualization 
out.plot2 <- function(val, val.sm, out, fs = 100){
  yrange <- c(-1, 1) * max(abs(val))
  y.h <- 0
  geom_line.df1 <- data.frame(
    x = seq(0, by = 1/fs, length.out = length(val)), y = val)
  plt <- 
    ggplot() + 
    geom_line(data = geom_line.df1,
              aes(x = x, y = y), 
              color = "grey") 
  for (i in 1:nrow(out)){
    tau1_i <- out[i, "tau_i"]
    tau2_i <- tau1_i + out[i, "T_i"] - 1
    tau1_i <- tau1_i/fs
    tau2_i <- tau2_i/fs
    plt <- 
      plt + 
      geom_vline(xintercept = tau1_i, color = "red") + 
      geom_vline(xintercept = tau2_i, color = "red") + 
      annotate(
        "rect",
        fill = "pink", 
        alpha = 0.3,
        xmin = tau1_i, 
        xmax = tau2_i, 
        ymin = yrange[1],
        ymax = yrange[2]
    )
  }
  geom_line.df2 <- data.frame(
    x = seq(0, by = 1/fs, length.out = length(val.sm)), y = val.sm)
  plt <- 
    plt + 
    geom_line(data = geom_line.df2, 
              aes(x = x, y = y), 
              color = "black", size = 0.6, alpha = 0.8) + 
    theme_bw(base_size = 9) + 
    labs(x = "Time [s]", 
         y = "Black line: smoothed x", 
         title ="Light gray line: signal x\nBlack line: smoothed signal x\nRed vertical lines: start and end points of identified pattern occurrence\nRed shaded area: area corresponding to identified pattern occurrence")
  plot(plt)
}

## ---- fig.width=7, fig.height=3-----------------------------------------------
out.plot2(x, windowSmooth(x = x, x.fs = 100, W = 0.5), out)

