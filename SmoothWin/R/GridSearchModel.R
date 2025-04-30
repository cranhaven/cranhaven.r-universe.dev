gridSearchModel = function(object                                     ,
                           data                                       ,
                           t                                          ,
                           m = mean(t)                                ,
                           l = 1                                      ,
                           k = 1                                      ,
                           weightFUN = function(x, ...) {
                             x
                           }                                          ,
                           threshold = sqrt(.Machine$double.eps) * 10 ,
                           check = 1                                  ,
                           messages = TRUE                            ,
                           onlyOne  = FALSE                           ,
                           cdf                                        ,
                           zeroCompensation = 0                       ,
                           weightOrthreshold = 'weight'               ,
                           direction         = c(1, 1)                ,
                           ...) {
  lk     = length(k)
  ll     = length(l)
  m      = unique(m)
  ## outputs
  lmodel = lweight = orgWeight = list(NULL)
  rmat   = matrix(0, ncol = 4       , nrow = ll * lk)
  colnames(rmat)          = c('Index'        ,
                              'ObsInInterval',
                              'k'            ,
                              'l')
  counter = countProgress = 1
  pb      = txtProgressBar(
    min   = 0       ,
    max   = lk * ll ,
    style = 3     ,
    width = 50
  )
  for (kp in k) {
    for (lp in l) {
      weight = expWeight(
        t = t                               ,
        k = kp                              ,
        l = lp                              ,
        m = m                               ,
        plot = 0                            ,
        cdf  = cdf                          ,
        zeroCompensation = zeroCompensation ,
        direction = direction               ,
        progress = FALSE                    
      )
      wi      = checkWeightsN(
        w         = weight    ,
        threshold = threshold ,
        t         = t         ,
        check     =  check
      )
      if(weightOrthreshold == 'weight')
        inn                 = sum(wi$w)
      else
        inn = wi$NoZeWe
      
      newdata             = data[wi$wInd,]
      newdata$ModelWeight = wi$w
      slm = tryCatch(
        expr = {
          do.call('update',
                  list(
                    object                                     ,
                    weights  = weightFUN (newdata$ModelWeight) ,
                    data     = droplevels(newdata)             ,
                    ...
                  ))
        },
        error = function(err, messsages = messages) {
          if (messsages)
            message0(' ~> Error: '  , err)
          slm = NULL
        } ,
        warning = function(war, messsages = messages) {
          if (messsages)
            message0(' ~> Warning: ', war)
          slm = NULL
        }
      )
      setTxtProgressBar(pb = pb, value = countProgress)
      countProgress = countProgress + 1
      # pass or skip
      if (is.null(slm)) {
        rmat = rmat[-nrow(rmat), , drop = FALSE]
        next
      }
      
      lmodel[[counter]]      = slm
      lweight[[counter]]     = wi$w
      orgWeight[[counter]]   = weight
      rmat  [counter, ]      = c(counter  ,
                                 inn      ,
                                 kp       ,
                                 lp)
      counter                = counter  + 1
    }
  }
  close(pb)

  return(
    list(
      output = as.data.frame(rmat),
      data   = data               ,
      models = if (onlyOne     && length(lmodel) < 2) {
        lmodel[[1]]
      } else{
        lmodel
      },
      weights = if (onlyOne    && length(lweight) < 2) {
        lweight[[1]]
      } else{
        lweight
      },
      FullWeight = if (onlyOne && length(orgWeight) < 2) {
        orgWeight[[1]]
      } else{
        orgWeight
      }
    )
  )
}
