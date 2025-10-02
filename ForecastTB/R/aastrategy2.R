strategy2 <- function(data, n, h, d1, strats = 'recursive', Method) #h is nval & d is number of data values
{

  data = data

  #fit model as per strategy
  data=tail(data,d1)
  data <- as.numeric(unlist(data))

  if(tolower(strats)=='recursive')
    {
    nval = h
    #prediction methods are assumed to be recursive by default
    #recursive: predict one value, append it to the data set and fit the same model to predict next value
    if(toupper(Method)=="ARIMA")
      {
        d <- forecast(auto.arima(data), h)$mean
      }
    else if(toupper(Method)=="PSF")
      {
        d <- psf(data = data, cycle = 12)
        d <- predict(object = d, n.ahead = h)
      }
  else
    {
      d <- parse(text = Method)
      d <- eval(d)
      # d <- d$value(data, h)
    }
  }


else if(tolower(strats)=='dirrec')
{
  nval=1
  #dirrec: predict one value, append it to data and fit a new model to predict next val
  d = c()
  j=h
  if(toupper(Method)=="ARIMA")
  {
    while(h)
    {
      #data=tail(data,d1)
      v <- forecast(auto.arima(data), 1)$mean
      d = append(d, v, length(d))
      data= append(data, v, length(data))
      h=h-1
    }
  }
  else if(toupper(Method)=="PSF")
  {
    while(h)
    {
      #data=data[(j+1-h):length(data)]
      v1 <- psf(data = data, cycle = 12)
      v <- predict(object = v1, n.ahead = 1)
      #v <- predict(object = v)
      d=append(d, v, length(d))
      data= append(data, v, length(data))
      h=h-1
    }
  }
  else
  {
    while(h)
    {
      #data=data[(j+1-h):length(data)]
      v <- parse(text = Method)
      v <- eval(v)
      #v <- d$value(data, 1)
      d=append(d, v, length(d))
      data=append(data, v, length(data))
      h=h-1
    }
  }
}


# else if(tolower(strats)=='dirrec')
# {
#
#   d = vector(mode="numeric",length=0)
#   if(toupper(Method)=="arima")
#   {
#     while(h)
#     {
#       v <- forecast(auto.arima(data), 1)$mean
#       d=append(d, v, length(d))
#       data=append(data, v, length(data))
#       h=h-1
#     }
#   }
#   else if(toupper(Method)=="psf")
#   {
#     while(h)
#     {
#       v <- psf(data = data, cycle = 12)
#       v <- predict(object = d, n.ahead = 1)
#       d=append(d, v, length(d))
#       data=append(data, v, length(data))
#       h=h-1
#     }
#
#   }
#   else
#   {
#     while(h)
#     {
#       v <- parse(text = Method)
#       v <- eval(d)
#       v <- d$value(data, 1)
#       append(d, v, length(d))
#       append(data, v, length(data))
#       h=h-1
#     }
#   }
# }

else if(tolower(strats)=='direct')
{

  if(toupper(Method)=="ARIMA")
  {
    d = vector(mode="numeric",length=0)
    while(h)
    {
      v <- forecast(auto.arima(data), 1)$mean
      append(d, v, length(d))
      h=h-1
    }
  }
  else if(toupper(Method)=="PSF")
  {
    while(h)
    {
      d <- psf(data = data, cycle = 12)
      v <- predict(object = d, n.ahead = 1)
      append(d, v, length(d))
      h=h-1
    }
  }
  else
  {
    while(h)
    {
      v <- parse(text = Method)
      v <- eval(d)
      v <- d$value(data, 1)
      append(d, v, length(d))
      h=h-1
    }
  }
}


else if(tolower(strats)=='dirmo')
{
  d = vector(mode="numeric",length=0)

  if(toupper(Method)=="ARIMA")
  {
    s=h/n
    while(n)
    {
      data=data[h + 1 - (s*n) :length(data)]
      v <- forecast(auto.arima(data), s)$mean
      append(d, v, length(d))
      append(data, v, length(data))
      n=n-1
    }
  }
  else if(toupper(Method)=="PSF")
  {
    while(h)
    {
      data=data[h+1-(s*n):length(data)]
      d <- psf(data = data, cycle = 12)
      v <- predict(object = d, n.ahead = s)
      append(d, v, length(d))
      append(data, v, length(data))
      n=n-1
    }
  }
  else
  {
    while(h)
    {
      data=data[h+1-(s*n):length(data)]
      v <- parse(text = Method)
      v <- eval(d)
      v <- d$value(data, 1)
      append(d, v, length(d))
      append(data, v, length(data))
      n=n-1
    }
  }
}

return(d)
}
