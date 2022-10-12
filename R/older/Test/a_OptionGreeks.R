library(quantmod)
library(fOptions)
library(dplyr)
library(forecast)
#User Created functions ####
#gets the historical option price
o.func = function(ticker,date,strike){
  optionchain = getOptionChain(ticker,Exp = as.Date(date))$calls
  x.in = optionchain %>% filter(Strike == strike) %>% row.names()
  x.out = suppressWarnings(getSymbols(x.in, auto.assign = F)) %>% na.omit
  colnames(x.out) = c("Open","High","Low","Close","Volume","Adjusted")
  x.out
}
my.df = function(ticker,date,strike) {
  stock.price = getSymbols(ticker,auto.assign = F,warnings = F) %>% OHLCV()
  option.price = o.func(ticker,date,strike)
  opt.tick = getOptionChain(ticker,as.Date(date))$calls %>% 
    filter(Strike == strike) %>%row.names()
  close.price = as.data.frame(
    merge(stock.price[,4],option.price[,4],join= "inner")
  )
  df.option = data.frame(
    date = as.Date(row.names(close.price)),
    time.value = as.numeric(date - as.Date(row.names(close.price)))/360,
    option = as.numeric(close.price[,2]),
    stock = as.numeric(close.price[,1])) %>% 
    rbind(data.frame(date = as.character.Date(Sys.Date(),format = "%Y-%m-%d"),
                     time.value = as.numeric(date-Sys.Date())/360,
                     option =getQuote(opt.tick)$Last,
                     stock = getQuote(ticker)$Last))
  df.option
  
}
OptionGreeks <- function(df) {
  # get Estimated implied volatility ####
  for (i in 1:nrow(df.option)){
    df.option$IV[i] = GBSVolatility(
      price = df.option$option[i],
      TypeFlag = "c",
      S = df.option$stock[i],
      X = strike,
      Time = df.option$time.value[i],
      r = .01,
      b = .10)}
  # get Estimated Price ####
  for (i in 1:nrow(df.option)){
    df.option$o.price[i] = GBSOption(
      TypeFlag = "c",
      S = df.option$stock[i],
      X = strike,
      Time = df.option$time.value[i],
      r = .01,
      b = .10,
      sigma = df.option$IV[i]
    )@price
  }
  # Get Delta ####
  for (i in 1:nrow(df.option)){
    df.option$delta[i] = GBSGreeks(
      Selection = "Delta",
      TypeFlag = "c",
      S = df.option$stock[i],
      X = strike,
      Time = df.option$time.value[i],
      r = .01,
      b = .10,
      sigma = df.option$IV[i])}
  # Get Theta ####
  for (i in 1:nrow(df.option)){
    df.option$theta[i] = GBSGreeks(
      Selection = "Theta",
      TypeFlag = "c",
      S = df.option$stock[i],
      X = strike,
      Time = df.option$time.value[i],
      r = .01,
      b = .10,
      sigma = df.option$IV[i])}
  # Get Vega ####
  for (i in 1:nrow(df.option)){
    df.option$vega[i] = GBSGreeks(
      Selection = "vega",
      TypeFlag = "c",
      S = df.option$stock[i],
      X = strike,
      Time = df.option$time.value[i],
      r = .01,
      b = .10,
      sigma = df.option$IV[i])}
  # Get rho ####
  for (i in 1:nrow(df.option)){
    df.option$rho[i] = GBSGreeks(
      Selection = "Rho",
      TypeFlag = "c",
      S = df.option$stock[i],
      X = strike,
      Time = df.option$time.value[i],
      r = .01,
      b = .10,
      sigma = df.option$IV[i])}
  # Get lambda ####
  for (i in 1:nrow(df.option)){
    df.option$lambda[i] = GBSGreeks(
      Selection = "Lambda",
      TypeFlag = "c",
      S = df.option$stock[i],
      X = strike,
      Time = df.option$time.value[i],
      r = .01,
      b = .10,
      sigma = df.option$IV[i])}
  df.option
}
# Price Checking option ####
priceCheck = function(tick,strike,r,t,v){
  price = matrix(0,20,3)
  price[,1] = seq(from=getQuote(tick)$Last,by=r,length.out=nrow(price))
  for (j in 1:nrow(price)){price[j,2] = BlackScholes(price[j],k,0.00,t/360,v,"C")}
  for (j in 1:nrow(price)){price[j,3] = BlackScholes(price[j],k,0.00,(t-1)/360,v,"C")}
  colnames(price) = c("Stock" , " Today"," Tommorrow")
  round(as.data.frame(price),2)
} 
priceCheck = function(ticker,strike,r){
  price = matrix(0,10,3)
  price[,1] = seq(from = getQuote(ticker)$Last, by = r, length.out = nrow(price))

  price
}
