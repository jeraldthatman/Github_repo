setwd("/Users/jeraldachaibar/Documents/Dir/Market_R")
library(quantmod)
library(TTR)
library(dplyr)
source('a_OptionGreeks.R')
#SPY - s&p500 
#DIA - Dow Jones 
#IWM - Russel 2000 
#QQQ - Nasdaq 

symb= c('SPY', 'DIA', 'IWM', 'QQQ')
getSymbols(symb)



library(ggplot2)