# Create function that imports all option contracts. 
library(quantmod)
library(fOptions)
library(dplyr)
library(forecast)


df = getOptionChain('SPY')

df$calls$type = rep('Call', nrow(df$calls))
df$puts$type = rep('Put', nrow(df$puts))
df = rbind(df$calls, df$puts)


df
