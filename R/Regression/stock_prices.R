rm(list = ls())
######## Load in option data----------------------------------------------------
library(dplyr)
library(broom)
library(tidyr)

spy = read.csv('data/SPY.csv', header = TRUE)%>% mutate_all(~replace(., is.na(.), 0)) 
gdate = as.Date(spy$gatherDate,format = '%Y-%m-%d')

calls_n_puts  = spy %>% group_by(gatherDate, type) %>% 
  summarise(x = sum(volume)) %>% 
  pivot_wider(names_from = type, values_from = x, names_prefix = 'Volume_')

oi_call_put  = spy %>% group_by(gatherDate, type) %>% 
  summarise(x = sum(openInterest)) %>% 
  pivot_wider(names_from = type, values_from = x, names_prefix = 'OpenInterest') 


head(calls_n_puts)

xts(oi_call_put[,2:3], order.by = gatherDate)

time_period = as.Date.character(calls_n_puts$gatherDate,format = '%Y-%m-%d')
min(time_period)

head(calls_n_puts)

######## Load in the Price data----------------------------------------------------
tickers <- c("TLT", "IWM", "GLD", "SPY", "QQQ", "AAPL", "TSLA", "TBT", "VXX", "BNO", "WEAT", "UNG")
# CREATE AN ENVIORMENT TO STORE THE DATA IN. 
data <- new.env()

# USE THE ENV = DATA TO SPECIFY THE ENV, THEN CALL A DF FROM IT. 
library(quantmod)
getSymbols(tickers, 
           src = 'yahoo', 
           from  = min(spy$gatherDate), 
           env = data)

tail(data$VXX)

# RENAME THE COLUMNS TO TO EXCLUDE THE TICKER NAME 
for (i  in tickers){
  colnames(data[[i]]) = c("Open", "High", "Low", "Close", "Volume", "Adjusted")
}

######## Merge dataframe with Stock--------------
df = data$SPY
df$CallVolume = calls_n_puts$Call
df$PutVolume = calls_n_puts$Put
df$Call_oi = oi_call_put$Call
df$Put_oi = oi_call_put$Put
df$CallVolume_lag = lag(calls_n_puts$Call, lag_var)
df$Adjusted = lag(as.numeric(df$Adjusted), lag_var)
df$PutVolume_lag = lag(as.numeric(calls_n_puts$Put), lag_var)
df$Range_lag = lag(as.numeric((df$High - df$Low)), lag_var)

keeps = c("Close", "Volume", "CallVolume","PutVolume", 
          "Range_lag","CallVolume_lag", "PutVolume_lag",
          "Call_oi", "Put_oi")
model_df = na.omit(df[,keeps])
######## Exploration and Summary statistics --------------


######## Linear Model--------------
model = lm("Close ~.", data = model_df)
summary(model)
library(car)
library(MASS)
par(mfcol = c(1,2))
qqPlot(stdres(model))
hist(stdres(model))

####### Comments------------------
####### Looks like the model does a pretty good job at fitting the data, the amount of variance explained by the model (R^2) is .9347, The residuals vs. fitted plot looks to have a normal amount of variance between each point. QQ plot suggest that residuals follow a normal distribution 

