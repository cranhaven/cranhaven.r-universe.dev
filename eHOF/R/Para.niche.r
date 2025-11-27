#' @export
"Para_niche" <- function (
		resp,
		model,
		top,
		optima,
		pess,
		central = exp(-0.5),
		outer = exp(-2),
		newdata = NULL,
		...) {
  x <-  if(is.null(newdata)) seq(min(resp$x), max(resp$x), length.out = 10000) else newdata
  Range <- range(scale01(x))
  M <- resp$M
 if (missing(model))
    model <- pick.model(resp, gam=FALSE, ...)
 HOFfun <- function(resp, x, y, M) abs(y - predict(resp, new = x, M = M, model = model))

    if (model == "I") {
        outer.low <- Range[1]
        outer.high <- Range[2]
        central.low <- Range[1]
        central.high <- Range[2]
        orient <- NA
        }
    if (model == "II") {
        outer <- c(optima, optimize(HOFfun, y = top*eval(outer), Range, resp = resp, maximum = FALSE)$minimum)
        outer.low  <- min(outer)
        outer.high <- max(outer)
        central <- c(optima, optimize(HOFfun, y = top*eval(central), Range, resp = resp, maximum = FALSE)$minimum)
        central.low  <- min(central)
        central.high <- max(central)
        if(resp$models[[model]]$par[2] < 0) orient = 'increase' else orient = 'decrease'
    }
    if (model == "III") {
        top1 <- resp$models$III$fitted[which.min(resp$x)][1]
        top2 <- resp$models$III$fitted[which.max(resp$x)][1]
        if (top1 > top2)
            outer.low <- central.low <- Range[1] else
            outer.high <- central.high <- Range[2]

        tmp <- optimize(HOFfun, y = top*eval(outer), Range, resp = resp, maximum = FALSE)
        if (top1 > top2)
            outer.high <- tmp$minimum else
            outer.low <- tmp$minimum
        tmp <- optimize(HOFfun, y = top*eval(central), Range, resp = resp, maximum = FALSE)
        if (top1 > top2)
            central.high <- tmp$minimum else
            central.low <- tmp$minimum
        if(resp$models[[model]]$par[2] < 0) orient = 'increase' else orient = 'decrease'
    }

    if (model == "IV") {
        tmp <- optimize(HOFfun, y = top*eval(outer), Range, resp = resp, maximum = FALSE)
    	  tmp2 <-  optima - diff(c(optima,tmp$minimum))
    	  outer.high <- max(tmp$minimum, tmp2)
    	  outer.low <- min(tmp$minimum, tmp2)
              tmp <- optimize(HOFfun, y = top*eval(central), Range, resp = resp, maximum = FALSE)
    	  tmp2 <-  optima - diff(c(optima,tmp$minimum))
    	  central.high <- max(tmp$minimum, tmp2)
    	  print(central.high)
    	  central.low <- min(tmp$minimum, tmp2)
        orient = NA
    }

    if (model == "V") {
      tmp <- optimize(HOFfun, interval = Range, y = top*eval(outer), resp = resp, maximum = FALSE)
  	  outer.low <- tmp$minimum
  	   x <- resp$x[resp$x > optima]
      tmp <- optimize(HOFfun, y = top*eval(outer), c(optima, Range[2]), resp = resp, maximum = FALSE)
  	  outer.high <- tmp$minimum
  	  tmp <- optimize(HOFfun, y = top*eval(central), interval = c(Range[1], optima), resp = resp, maximum = FALSE)
  	  central.low <- tmp$minimum
  	  x <- resp$x[resp$x > optima]
      tmp <- optimize(HOFfun, interval = c(optima, Range[2]), y = top*eval(central), resp = resp, maximum = FALSE)
      central.high <- tmp$minimum
      orient <- if((optima - outer.low) < (outer.high - optima)) 'skewed.left' else 'skewed.right'
    }

    if(model %in% c('VI','VII')) {
        x <- seq(Range[1] - diff(Range), Range[2] + diff(Range),length.out=10000)
        pred <- predict.HOF(resp, newdata = x, model = model)
        infl <- c(FALSE, diff(diff(pred)>0)!=0)

      central.low <- optimize(HOFfun, y = top[1]*eval(central), c(min(x), optima['opt1']), resp = resp, maximum = FALSE)$minimum
      central.high <- optimize(HOFfun, y = top[1]*eval(central), c(optima['opt1'], pess), resp = resp, maximum = FALSE)$minimum
      central2.low <- optimize(HOFfun, y = top[2]*eval(central), c(pess, optima['opt2']), resp = resp, maximum = FALSE)['minimum']
      central2.high <- optimize(HOFfun, y = top[2]*eval(central), c(optima['opt2'], max(x)), resp = resp, maximum = FALSE)['minimum']

	  centralBorder <- c(central.low=central.low[[1]], central.high=central.high[[1]], central2.low=central2.low[[1]], central2.high=central2.high[[1]])

      outer.low <- optimize(HOFfun, y = max(top)*eval(outer), c(min(x)-diff(Range)/100, optima['opt1']), resp = resp, maximum = FALSE)$minimum
      outer.high <- optimize(HOFfun, y = max(top)*eval(outer), c(optima['opt1'], pess), resp = resp, maximum = FALSE)$minimum
      outer2.low <- optimize(HOFfun, y = max(top)*eval(outer), c(pess, optima['opt2']), resp = resp, maximum = FALSE)['minimum']
      outer2.high <- optimize(HOFfun, y = max(top)*eval(outer), c(optima['opt2'], max(x)+diff(Range)/100), resp = resp, maximum = FALSE)['minimum']

	  outerBorder <- c(outer.low=outer.low[[1]], outer.high=outer.high[[1]], outer2.low=outer2.low[[1]], outer2.high=outer2.high[[1]])

          orient = NA
	  relfreq.outer <- NA
    } else {
      indx <- outer.low <= resp$x & resp$x <= outer.high
      relfreq.outer <- sum(resp$y[indx] > 0) / sum(indx, na.rm=TRUE)
      outerBorder <- c(outer.low=outer.low, outer.high=outer.high)
      centralBorder <- c(central.low=central.low, central.high=central.high)
    }
  # centralBorder[which(centralBorder < min(Range))] <- min(Range)
  # centralBorder[which(centralBorder > max(Range))] <- max(Range)
  # outerBorder[which(outerBorder < min(Range))] <- min(Range)
  # outerBorder[which(outerBorder > max(Range))] <- max(Range)
  border <- list(centralBorder=centralBorder, outerBorder=outerBorder, orient=orient, relfreq.outer=relfreq.outer)
  return(border)
}

