library(quantmod)
library(fOptions)
library(dplyr)
library(forecast)
#iClean Global Clean Energy ETF
icln = c("PLUG","ENPH","DQ","OEZVY","GCTAY","XNYIF","MDDNF","DNNGY","ORA","VWDRY")
myOptions = c("ICLN210416C00029000",
                "ICLN210416C00045000",
                "ICLN210416C00043000",
                "BLNK210401C00060000")

#User Created functions ####
#gets the historical option price
o.func = function(ticker,date,strike){
  #Get Option Chain for selected stock 
  optionchain = getOptionChain(ticker,Exp = as.Date(date))$calls
  #Get the option Symbol for said stock 
  x.in = optionchain %>% filter(Strike == strike) %>% row.names()
  x.out = suppressWarnings(getSymbols(x.in, auto.assign = F)) %>% na.omit
  colnames(x.out) = c("Open","High","Low","Close","Volume","Adjusted")
  x.out
}
my.df = function(ticker,date,strike) {
  #Stock Price 
  stock.price = getSymbols(ticker,auto.assign = F,warnings = F) %>% OHLCV() 
  #Historical Option Price
  option.price = o.func(ticker,date,strike) 
  opt.tick = getOptionChain(ticker,as.Date(date))$calls %>% 
    filter(Strike == strike) %>%row.names()
  close.price = as.data.frame(
    merge(stock.price[,4],option.price[,4],join= "inner")
  )
  df.option = data.frame(
    date = as.Date(row.names(close.price)),
    #Calculate Time Value Until Expiration 
    time.value = as.numeric(date - as.Date(row.names(close.price)))/360,
    #Merge Historical Stock and Option Prices 
    option = as.numeric(close.price[,2]),
    stock = as.numeric(close.price[,1])) %>% 
    rbind(data.frame(date = as.character.Date(Sys.Date(),format = "%Y-%m-%d"),
                     time.value = as.numeric(date-Sys.Date())/360,
                     option =getQuote(opt.tick)$Last,
                     stock = getQuote(ticker)$Last))
  df.option
  
}
OptionGreeks <- function(df.option) {
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
greek.current = function(ticker,Expiry){df.option = getOptionChain(ticker,Exp = Expiry)$calls[,-8] %>% filter(ITM == FALSE)
# Get Delta ####
for (i in 1:nrow(df.option)){
  df.option$delta[i] = GBSGreeks(
    Selection = "Delta",
    TypeFlag = "c",
    S = getQuote(ticker)$Last,
    X = df.option$Strike[i],
    Time =as.numeric(Expiry-Sys.Date())/365,
    r = .01,
    b = .10,
    sigma = df.option$IV[i])}
# Get Gamma ####
for (i in 1:nrow(df.option)){
  df.option$gamma[i] = GBSGreeks(
    Selection = "Gamma",
    TypeFlag = "c",
    S = getQuote(ticker)$Last,
    X = df.option$Strike[i],
    Time =as.numeric(Expiry-Sys.Date())/365,
    r = .01,
    b = .10,
    sigma = df.option$IV[i])}
df.option}


ticker = "SP"
Expiry = as.Date("2021-04-16")
#Call Greek Current ####
greek.current("SP",as.Date("2021-04-16"))
getQuote(myOptions[1])

# Next Lets look at a 10 day moving average on the price of the option, as well as the option greeks. 

#Compare Solar Energy Stocks to oil, and other means of energy, ie. the DOW JONES
