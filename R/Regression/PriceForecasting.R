library(corrplot)
library(dplyr)
library(tidyquant)
library(car)
library(forecast)
library(TTR)
library(lubridate)


tickers = "AAPL" # Market and a risk free asset 
start_date = as.Date('2018-01-01')
end_date = as.Date('2022-07-28')

prices <- tickers %>% 
  tq_get(get = "stock.price", from = start_date, to = end_date) %>% 
  select(date, close) %>%
  mutate(date = as.Date(date))


price_ts = xts(prices$close, order.by= prices$date)
ses(price_ts)
autoplot(ses(price_ts))

ind6 = lag(prices$date, 2) %>% na.omit()
sma6 = SMA(prices$close, n = 3) %>% na.omit()

ses_model = ses(xts(sma6, ind6),
                alpha = .05,
                lambda = 'auto', 
                level = c(90,95))

plot(ses_model$fitted, col = 'grey')
par(new = TRUE)
plot(sma6, col = 'blue', type = 'l')
par(new = TRUE)
plot(SMA(prices$close, n = 28), col = 'red', type = 'l')


ses_model$upper
