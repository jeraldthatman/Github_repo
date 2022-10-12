library(quantmod)
library(fOptions)
library(dplyr)
library(forecast)

# we need to get the full option chain, for a said date or all dates. 

#getting the historical option price. 
tst_func = function(ticker,date,strike){
  optionchain = getOptionChain(ticker,Exp = as.Date(date))$calls
  #get the option symbol.
  x.in = optionchain %>% filter(Strike == strike) %>% row.names()
  #feed the option symol to get histroical prices. 
  x.out = suppressWarnings(getSymbols(x.in, auto.assign = F)) %>% na.omit
  #gets the Open, high, low, close, and volume of the contract. 
  colnames(x.out) = c("Open","High","Low","Close","Volume","Adjusted")
  x.out
}

#gets the full option chain, stores it in a list 1= calls , 2= puts. 
chains = getOptionChain('F',as.Date('2021-09-10'))
calls = chains[[1]]
puts = chains[[2]]

calls %>% filter(Strike ==15.5)
calls %>% filter(Strike ==17.5)

leg1 = OptionGreeks("SPWR",as.Date('2022-01-21'),70)
leg2 = OptionGreeks("SPWR",as.Date('2022-01-21'),75)

library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

leg1$date = as.Date(leg1$date)
leg2$date = as.Date(leg2$date)


#get each variable data type
#str(leg1)

ggplot(leg1, aes(x=date,y=IV))+geom_line()
ggplot(leg2, aes(x=date,y=IV))+geom_line()

leg1
