
get_cum_returns <- function(x, period = "1D"){

  if(period == "1D"){
    
    dt <- x[nrow(x),]
    return(data.frame(symbol = colnames(x), Return = t(coredata(dt))))
    
  }else if(period == "7D"){
    x0 <- Sys.Date() %m-% weeks(1)
  }else if(period == "MTD"){
    x0 <- floor_date(Sys.Date(), unit = "months")
  }else if(period == "YTD"){
    x0 <- floor_date(Sys.Date(), unit = "years")
  }else if(period == "1M"){
    x0 <- Sys.Date() %m-% months(1)
  }else if(period == "3M"){
    x0 <- Sys.Date() %m-% months(3)
  }else if(period == "12M"){
    x0 <- Sys.Date() %m-% months(12)
  }else if(period == "3Y"){
    x0 <- Sys.Date() %m-% years(3)
  }
  
  x <- x[paste0(x0,"/"),]
  dt <- apply(x, 2, function(r) cumprod(1+r))
  ytd_xts <- xts::xts(dt,order.by = index(x))-1
  
  retVals <- apply(ytd_xts, 2, tail, 1)
  
  
  return(data.frame(symbol = names(retVals), Return = retVals))
}

