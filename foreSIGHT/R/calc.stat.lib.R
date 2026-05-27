F0calc <- function(x) {
  temp <- get.below(data = x, threshold = 0)
  return(temp)
}

# CSL CALCULATION
CSLcalc <- function(x) {
  n <- length(x)
  m <- n * 1
  half <- floor((m / 2.0) - 1.0)
  len <- n - 5.0
  sum <- sum2 <- 0

  for (i in 6:half) {
    if ((length(which(x[(i - 5):i] < 17))) == 6) {
      sum <- half - i
      next
    }
  }

  for (i in (half + 1):len) {
    if ((length(which(x[(i):(i + 5)] > 17))) == 6) {
      sum2 <- i - (half + 1)
      next
    }
  }
  y <- sum + sum2
  return(y)
}


# dat=c(rep(20,10),rep(0,10),rep(20,10))
# CSLcalc(x=dat,n=length(dat))

GSLcalc <- function(x) {
  n <- length(x)
  m <- n * 1
  half <- floor((m / 2.0) - 1.0)
  len <- n - 5.0
  sum <- sum2 <- 0

  for (i in 6:half) {
    if ((length(which(x[(i - 5):i] > 5))) == 6) {
      sum <- half - i
      next
    }
  }

  for (i in (half + 1):len) {
    if ((length(which(x[(i):(i + 5)] < 5))) == 6) {
      sum2 <- i - (half + 1)
      next
    }
  }
  y <- sum + sum2
  return(y)
}

# dat=c(rep(20,10),rep(0,10),rep(20,10))
# GSLcalc(x=dat,n=length(dat))



# Pad string in front
str <- function(x, n, pad = " ") {
  temp <- as.character(x)
  nlen <- nchar(temp)
  if (nlen < n) {
    for (i in 1:(n - nlen)) {
      temp <- paste(pad, temp, sep = "")
    }
  }
  temp
}
################

insert_NAs_breaks_V2 <- function(data, indx) {
  data <- data[indx$val]
  if (length(indx$breaks) > 1) {
    # N.new = length(data)+length(indx$breaks)
    # isNA = indx$breaks + seq(1,length(indx$breaks))
    # a = 1:N.new
    # notNA = a[!a%in%isNA]
    # data.new = rep(NA,N.new)
    # data.new[notNA] = data
    data.new <- rep(NA, indx$N)
    data.new[indx$notNA] <- data
    # browser()
  } else {
    data.new <- data
  }
  return(data.new)
}

###########################################################################################################################
# CONTROLLER FUNCT - MATCHES LISTED ATT'S WITH CALCULATOR


# INPUTS - TS, INDEXES,LIST OF REQUESTED STATS
# GENERIC EXTRACTOR FUNCTION
extractor <- function(func = NULL, data = NULL, indx = NULL, attArgs = NULL, ...) { # returns a number

  # we introduce NAs in between periods where times are not continuous
  if (is.list(data)) {
    data.1 <- data[[1]]
    data.2 <- data[[2]]
    data <- NULL
    # data.1.new = insert_NAs_breaks(data.1,indx)
    data.1.new <- insert_NAs_breaks_V2(data.1, indx)
    # if(!(all((data.1.new == data.1.new.tmp | is.na(data.1.new)&is.na(data.1.new.tmp))))){browser()}
    # data.2.new = insert_NAs_breaks(data.2,indx)
    data.2.new <- insert_NAs_breaks_V2(data.2, indx)
    # if(!(all((data.2.new == data.2.new.tmp | is.na(data.2.new)&is.na(data.2.new.tmp))))){browser()}
  } else if (is.vector(data)) {
    data.1 <- data.2 <- NULL
    # data.new = insert_NAs_breaks(data,indx)
    data.new <- insert_NAs_breaks_V2(data, indx)
    # if(!(all((data.new == data.new.tmp | is.na(data.new)&is.na(data.new.tmp))))){browser()}
  }

  # if(any(is.na(data))){browser()}

  if (!is.null(data)) {
    if (is.null(attArgs)) {
      extractor.out <- func(data = data.new, ...)
    } else {
      extractor.out <- func(data = data.new, attArgs = attArgs, ...)
    }
  } else {
    if (is.null(attArgs)) {
      extractor.out <- func(data.1 = data.1.new, data.2 = data.2.new, ...)
    } else {
      extractor.out <- func(data.1 = data.1.new, data.2 = data.2.new, attArgs = attArgs, ...)
    }
  }

  return(extractor.out)
}

# EXTRACTOR FOR MULTIPLE PERIODS (TEMPORARY FUNCTION here)
extractor.summaryMean <- function(func = NULL,
                                  data = NULL,
                                  indx = NULL, ...) {
  nperiod <- length(indx)
  sim.series <- rep(NA, nperiod)
  for (p in 1:nperiod) {
    sim.series[p] <- extractor(func = func, data = data, indx = indx[[p]], ...)
  }
  m.series <- mean(x = sim.series, na.rm = TRUE)
  return(m.series)
}

####### NOTE: calling the following separately is inefficient (annual totals calculated for each)

### followup note: this is resolved using aggregation periods of 1 year ion attributes

# EXTRACTOR FOR MULTIPLE PERIODS (TEMPORARY FUNCTION here)
extractor.summarySD <- function(func = NULL,
                                data = NULL,
                                indx = NULL, ...) {
  nperiod <- length(indx)
  sim.series <- rep(NA, nperiod)
  for (p in 1:nperiod) {
    sim.series[p] <- extractor(func = func, data = data, indx = indx[[p]], ...)
  }
  m.series <- stats::sd(x = sim.series, na.rm = TRUE)
  return(m.series)
}

extractor.summaryCV <- function(func = NULL,
                                data = NULL,
                                indx = NULL, ...) {
  nperiod <- length(indx)
  sim.series <- rep(NA, nperiod)
  for (p in 1:nperiod) {
    sim.series[p] <- extractor(func = func, data = data, indx = indx[[p]], ...)
  }
  m.series <- stats::sd(x = sim.series, na.rm = TRUE) / mean(x = sim.series, na.rm = TRUE)
  return(m.series)
}

extractor.summaryCor <- function(func = NULL,
                                 data = NULL,
                                 indx = NULL, ...) {
  nperiod <- length(indx)
  sim.series <- rep(NA, nperiod)
  for (p in 1:nperiod) {
    sim.series[p] <- extractor(func = func, data = data, indx = indx[[p]], ...)
  }
  m.series <- stats::cor(x = sim.series[1:(nperiod - 1)], sim.series[2:nperiod])
  if (is.na(m.series)) {
    m.series <- -999
  }
  return(m.series)
}

extractor.summaryDwellTime <- function(func = NULL,
                                       data = NULL,
                                       indx = NULL, ...) {
  nperiod <- length(indx)
  sim.series <- rep(NA, nperiod)
  for (p in 1:nperiod) {
    sim.series[p] <- extractor(func = func, data = data, indx = indx[[p]], ...)
  }
  spell.lengths <- get.spell.lengths(
    data = sim.series,
    thresh = stats::median(sim.series),
    type = "dry"
  )
  m.series <- mean(spell.lengths)
  return(m.series)
}

extractor.summaryMin <- function(func = NULL,
                                 data = NULL,
                                 indx = NULL, ...) {
  nperiod <- length(indx)
  sim.series <- rep(NA, nperiod)
  for (p in 1:nperiod) {
    sim.series[p] <- extractor(func = func, data = data, indx = indx[[p]], ...)
  }
  m.series <- min(x = sim.series, na.rm = TRUE)
  return(m.series)
}

extractor.summaryMax <- function(func = NULL,
                                 data = NULL,
                                 indx = NULL, ...) {
  nperiod <- length(indx)
  sim.series <- rep(NA, nperiod)
  for (p in 1:nperiod) {
    sim.series[p] <- extractor(func = func, data = data, indx = indx[[p]], ...)
  }
  m.series <- max(x = sim.series, na.rm = TRUE)
  return(m.series)
}

# FUNCTION TO DETERMINE NUMBER OF INSTANCES ABOVE A THRESHOLD - nwet
get.nwet <- function(data = NULL, threshold = NULL) {
  data <- data[!is.na(data)]
  temp <- length(which(data > threshold))
  if (identical(temp, integer(0))) {
    temp <- 0
  }
  return(temp)
}

# FUNCTION TO DETERMINE NUMBER OF INSTANCES Below A THRESHOLD - nwet
get.below <- function(data = NULL, threshold = NULL) {
  temp <- length(which(data < threshold))
  if (identical(temp, integer(0))) {
    temp <- 0
  }
  return(temp)
}

# #FUNCTION TO EXTRACT ALL AMOUNTS ABOVE A THRESHOLD
# get.wet.amounts=function(data=NULL,threshold=NULL){
#   temp=data[which(data>threshold)]
#   return(temp)
# }

# FUNCTION TO GET AVERAGE ABOVE A THRESHOLD
get.wet.average <- function(data = NULL, threshold = NULL) {
  ind <- which(data > threshold)
  if (identical(ind, integer(0))) {
    temp <- 0 # if no wet days
  } else {
    temp <- mean(data[ind], na.rm = T)
  }
  return(temp)
}

get.quantile <- function(data = NULL, # vector
                         quant = NULL # quantile (between 0.001-0.999)
) {
  temp <- stats::quantile(x = data, probs = quant, na.rm = TRUE, names = FALSE)
  return(temp)
}

get.quantile.rng <- function(data = NULL, # vector
                             lim = 0.9 # limits of range (e.g. 0.9 for 5-95%)
) {
  p1 <- (1. - lim) / 2.
  p2 <- (1. + lim) / 2.
  temp <- stats::quantile(x = data, probs = p2, na.rm = TRUE, names = FALSE)[1] - stats::quantile(x = data, probs = p1, na.rm = TRUE, names = FALSE)[1]
  return(temp)
}
# note this function doesn't properly deal with missing data - ideally any spells with missing data should be omitted
get.spell.lengths <- function(data = NULL, # vector of rain
                              thresh = NULL, # wetness threshold, all values below or equal to deemed dry
                              type = "wet" # get wet or dry spell length
) {
  data <- data[!is.na(data)]
  above <- rep(0, length(data))
  ind <- which(data > thresh)
  above[ind] <- 1 # record entries above threshold as 1
  tmp <- rle(above)
  switch(type,
    "wet" = {
      ind.wet <- which(tmp$values == 1)
      spell.len <- tmp$lengths[ind.wet]
    },
    "dry" = {
      ind.dry <- which(tmp$values == 0)
      spell.len <- tmp$lengths[ind.dry]
    },
    -999.00
  )
  if (length(spell.len) == 0) {
    spell.len <- 0
  }
  return(spell.len)
}

get.spell.lengths.max <- function(data = NULL, # vector of rain
                                  thresh = NULL, # wetness threshold, all values below or equal to deemed dry
                                  type = "wet" # get wet or dry spell length
) {
  temp <- max(get.spell.lengths(data = data, thresh = thresh, type = type), na.rm = TRUE)
  temp
}

# series=c(0.5,0.5,0.5,0.01,0.01,0.01,0.8,0.5,0.5,0.5,0,0,0,2,2,0,2)
# get.spell.lengths(data=series,thresh=0.01,type="dry")
# mean(get.spell.lengths(data=series,thresh=0.01,type="wet"),na.rm=TRUE)

p <- function(...) {
  paste(..., sep = "")
} # PASTE FUNCTION

# categorise func
categ.fun <- function(perf.lim = c(5, 10), # performance limits (<=5% good, <=10& fair, >10% poor)
                      rel.diff = NULL # relative difference to classify
) {
  perf <- "poor" # start off at "poor"
  if (abs(rel.diff) <= perf.lim[1]) {
    perf <- "good"
  } else {
    if (abs(rel.diff) <= perf.lim[2]) {
      perf <- "fair"
    } else {
      perf <- "poor"
    }
  }

  return(perf)
}


# plot classifer chart element - one of many in grid of classifiers

plot_attrib_perf_solo <- function(rel.diff, # relative difference - scalar
                                  perf.lim = c(5, 10), # performance limits - good, fair, poor beyond
                                  targetType = NULL,
                                  att.name = NULL, # string that will label plot
                                  prim.lab = NULL, # primary label
                                  cex.mult = 3,
                                  cex.mult.sub = 1.1,
                                  y.text = 0.05,
                                  mtext.line = 0.35) {
  # COLOR RAMP
  # traffic.col=c("chartreuse3","gold1","red1")

  # MAKE VECTOR X
  att.cat <- categ.fun(perf.lim, rel.diff)
  if (att.cat == "poor") {
    ind <- 3
  }
  if (att.cat == "fair") {
    ind <- 2
  }
  if (att.cat == "good") {
    ind <- 1
  }
  x <- rep(0, 3) # make blank x vector
  x[ind] <- 100 # update to reflect att.cat


  # PLOT BARPLOT
  graphics::barplot(
    height = cbind(x = x / 100), horiz = T, xaxt = "n", yaxt = "n",
    beside = FALSE, width = c(0.1), col = traffic.col,
    args.legend = list(x = "topleft")
  )

  # ADD TEXT ANNOTATIONS
  if (ind == 3) {
    bg.col <- "black"
    front.col <- "white"
  } else {
    bg.col <- "white"
    front.col <- "black"
  } # text background colour updater
  if ((targetType == "pc") | (targetType == "frac")) {
    graphics::text(labels = paste(format(rel.diff, digits = 2), "%", sep = ""), x = 0.5, y = y.text, cex = cex.mult, pos = 3, col = front.col, bg = bg.col)
  } else {
    graphics::text(labels = paste(format(rel.diff, digits = 2), " delta", sep = ""), x = 0.5, y = y.text, cex = cex.mult, pos = 3, col = front.col, bg = bg.col)
  }

  # ADD SUBTITLE
  graphics::mtext(text = att.name, side = 1, at = 0.5, cex = cex.mult.sub, line = mtext.line)

  # ADD TITLE
  if (!is.null(prim.lab)) {
    graphics::mtext(text = prim.lab, side = 3, at = 0.5, cex = cex.mult.sub, line = mtext.line)
  }
}

# calculation of percentage change
pc.calc <- function(sim = NULL, # simulate point
                    target = NULL # target point
) {
  pc.diff <- (sim - target) / target * 100 # calc percen diff from target
}
# pc.calc(sim,target)

# calculation of percentage change
absDiff.calc <- function(sim = NULL, # simulate point
                         target = NULL # target point
) {
  abs_diff <- (sim - target) # calc abs diff from target
}

########################################
# x: the vector
# n: the number of samples
# centered: if FALSE, then average current sample and previous (n-1) samples
#           if TRUE, then average symmetrically in past and future. (If n is even, use one more sample from future.)
movingAverage <- function(x, n = 1, centered = FALSE) {
  if (centered) {
    before <- floor((n - 1) / 2)
    after <- ceiling((n - 1) / 2)
  } else {
    before <- n - 1
    after <- 0
  }

  # Track the sum and count of number of non-NA items
  s <- rep(0, length(x))
  count <- rep(0, length(x))

  # Add the centered data
  new <- x
  # Add to count list wherever there isn't a
  count <- count + !is.na(new)
  # Now replace NA_s with 0_s and add to total
  new[is.na(new)] <- 0
  s <- s + new

  # Add the data from before
  i <- 1
  while (i <= before) {
    # This is the vector with offset values to add
    new <- c(rep(NA, i), x[1:(length(x) - i)])

    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new

    i <- i + 1
  }

  # Add the data from after
  i <- 1
  while (i <= after) {
    # This is the vector with offset values to add
    new <- c(x[(i + 1):length(x)], rep(NA, i))

    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new

    i <- i + 1
  }

  # return sum divided by count
  s / count
}
