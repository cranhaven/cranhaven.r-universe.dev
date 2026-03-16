#' Cover code translation
#'
#' @name tv.coverperc
#' @description Translate cover code into percentage cover values for Turboveg database observations.
#'
#' @export
#' @param db the name of the Turboveg database
#' @param obs dataframe of observations, containing Cover Codes, coded in tvscale.dbf of Turboveg installation
#' @param RelScale Dataframe of CoverScale codes per releve, if empty it is read from the database
#' @param tv_home Path to Turboveg installation
#' @param tvscale Cover scale
#' @param \dots Further options
#'
#' @return data.frame of observations with additional column COVER_PERC
#'
#' @keywords Turboveg

tv.coverperc <- function (db, obs, RelScale, tv_home, tvscale, ...) {
  if(missing(tv_home)) {
    tv_home <- tv.home()
  }

  if(missing(tvscale)) {
    #########################################
    # dat <- read.dbf(file.path(tv_home, "Popup", tv.dict('taxatest'), "tvscale.dbf"), as.is = TRUE)
    # dat_longer <- dat %>%
    #   tidyr::pivot_longer(
    #     cols = starts_with("SCH") | starts_with("DEC"),
    #     names_to = c(".value", "index"),
    #     names_pattern = "(SCH|DEC)(\\d+)",
    #     values_drop_na = TRUE
    #   )
    # tvscale <- dat_longer[!is.na(dat_longer$SCH), ]
    #########################################
    tvscale <- read.dbf(file.path(tv_home, "Popup", tv.dict(db), "tvscale.dbf") )
  }
  tvscale <- tvscale[!is.na(tvscale$SCALE_NR),]
  tvscale <- tvscale[,names(tvscale) != 'PUB']
  rownames(tvscale) <- tvscale[, 1]
  if (missing(RelScale)) {
      ow <- options('warn')
      options(warn = -1)
      suppressMessages(
        RelScale <- tv.site(db=db, tv_home=tv_home)[, c("PlotObservationID", "COVERSCALE")]
      )
      options(ow)
      }
  if (missing(obs))
      obs <- tv.obs(db, tv_home, as.is=TRUE)
  obs$COVERSCALE <- RelScale$COVERSCALE[match(obs$PlotObservationID, RelScale$PlotObservationID)]
  if(any(is.na(obs$COVERSCALE)))  {
    print(unique(obs[is.na(obs$COVERSCALE),'COVER_CODE']))
    print(unique(obs[is.na(obs$COVERSCALE),'PlotObservationID']))
    stop('The above releve numbers have no cover scale value in the header data or cover code is missing in proposed scale.')
    }
  ## Split ####
  g <- obs$COVERSCALE
  obs <- split(obs, g, drop = FALSE)
  for (i in names(obs)) {
    if (i %in% c("00","98","99")) {
    	if("COVER_CODE" %in% names(obs[[1]])) {
    	  obs[[i]]$COVER_CODE[obs[[i]]$COVER_CODE == '9X'] <- 100
    	  binrel <- unique(stats::na.omit(obs[[i]]$PlotObservationID[as.numeric(obs[[i]]$COVER_CODE) > 100]))
    	  # cn <- unique(obs$COVER_CODE)
    	  # cn[cn > as.raw(255)]
    	  obs[[i]]$COVER_CODE[obs[[i]]$COVER_CODE > 100]
    	  binrel <- unique(obs[[i]]$PlotObservationID[obs[[i]]$COVER_CODE > as.raw(255)])
    	  if(length(binrel) > 0) obs[[i]]$COVER_CODE[obs[[i]]$PlotObservationID == binrel] <- bin2word(obs[[i]]$COVER_CODE[obs[[i]]$PlotObservationID == binrel])
    	}

    	if(any(is.na(as.numeric(obs[[i]]$COVER_CODE))))
    	  warning('Not all percentage cover values in your database are numeric, please check in Turboveg.')
      obs[[i]] <- data.frame(obs[[i]], COVER_PERC = as.numeric(as.character(obs[[i]][, "COVER_CODE"])))
    }  else {
      p <- which(is.na(tvscale[i,]))[1]
      if(is.na(p)) p <- ncol(tvscale)
      scala <- tvscale[i,]
      if(is.na(scala[1])) stop('Can not find cover scale "', i, '" in ', file.path('Turbowin','Popup', tv.dict(db),'tvscale.dbf'))
      code <- iconv(t(scala[seq(4,(p-1),2)]), from=getOption('tv.iconv'), to='')
      perc <- scala[seq(5,p,2)][1,]
      d.f <- data.frame(code=code[,1], perc = as.numeric(perc))
      message('Cover code used: ',i , as.character(tvscale[i, 2]))
      #  write.table(t(d.f), col.names = FALSE, sep = "\t", quote = FALSE)
      #  print(table(t(d.f), col.names = FALSE, sep = "\t", quote = FALSE))
        print(as.table(t(d.f)), col.names = FALSE, sep = "\t", quote = FALSE)
      obs[[i]]["COVER_PERC"] <- d.f$perc[match(obs[[i]][,"COVER_CODE"], d.f$code)]
  }
  }
  obs <- unsplit(obs, g)
  if(any(is.na(obs$COVER_PERC))) {
      print(obs[is.na(obs$COVER_PERC),'COVER_CODE'])
      stop("Invalid cover codes, please check tvabund.dbf and tvscale.dbf!")
  }
  obs
}

