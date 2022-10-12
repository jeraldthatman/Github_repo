library(corrplot)
library(dplyr)
library(tidyquant)
library(car)
" Is there a relationship with UNG returns and CPI ? "

######## Load in the data
cpi_data = read.csv('data/CPI.csv') # Monthly Data Starting in 2018
names(cpi_data)[names(cpi_data) == 'Date'] = 'date'
names(cpi_data)[names(cpi_data) == 'US_All_items'] = 'CPI'

ppi_data = read.csv('data/PPI.csv') # Monthly Data starting in 2018 
names(ppi_data)[names(ppi_data) == 'Date'] = 'date'
names(ppi_data)[names(ppi_data) == 'Total.manufacturing.industries'] = 'PPI'


tickers = c("UNG", "TLT") # Market and a risk free asset 
start_date = as.Date('2018-01-01')
end_date = as.Date('2022-07-07')

prices <- tickers %>% 
  tq_get(get = "stock.price", from = start_date, to = end_date) %>% 
  select(date,ticker = symbol, close) %>%
  mutate(date = as.Date(date))

price_monthly_ret = prices %>% group_by(ticker) %>%
  tq_transmute( select = close, mutate_fun= periodReturn, period = 'monthly', colname = 'monthly_return')

UNG_ret = price_monthly_ret %>% filter(ticker == 'UNG') %>% ungroup() %>% select(date, monthly.returns) %>% data.frame()
tlt_ret = price_monthly_ret %>% filter(ticker == 'TLT') %>% ungroup() %>%  select(date, monthly.returns) %>% data.frame()

####### Data Cleaning and merging 

dte_seq = seq(as.Date("2018-01-01"), as.Date("2022-07-01"), 'months')
UNG_ret$date = dte_seq
tlt_ret$date = dte_seq
cpi_data$date = as.Date(cpi_data$date)
ppi_data$date = as.Date(ppi_data$date)
#cpi_data = cpi_data %>% select(date, CPI)
#ppi_data = ppi_data %>% select(date, PPI)

model_df = merge(cpi_data, UNG_ret, by= 'date')
model_df = merge(model_df, tlt_ret, by = 'date')
model_df = merge(model_df, ppi_data, by = 'date')
names(model_df)[names(model_df)=='monthly.returns.x'] = 'Market_return'
names(model_df)[names(model_df)=='monthly.returns.y'] = 'RF_return'

####### Data Exploration 
cor(model_df[,-1], use = 'pairwise')

####### Modeling 
linear_model = lm('Market_return -RF_return ~.', data = model_df[,-1])
summary(linear_model)
summary(linear_model)$coef %>% data.frame() %>% filter(Pr...t.. < 0.05)

####### Residual Analysis 
par(mfcol = c(2,2))

# Assumption 1: Linearity / Zero Mean: E[e_i] = 0
#   Check with: Plot the residuals (y-axis) and Predictor(x-axis)
plot(model_df$Market_return, resid(linear_model), main = 'Residuals vs. Predictor')
abline(0, 0, col = 'red')
# Assumption 2: Constant Variance: Var[e_i] = sigma^2
#   Check with: Plot residuals(y-axis) and fitted values(x-axis)
plot(fitted.values(linear_model), resid(linear_model), main = 'Residuals vs. Fitted Values')
abline(0, 0, col = 'red')

# Assumption 3: Independence: e_i are all IID
#   Check with: Constant variance plot, look for clustered data points.  

# Assumption 4: Normality: all e_i follow Normal(0, sigma^2)
#   Check with:QQPlot and histogram of the residuals. 

qqPlot(linear_model)
hist(resid(linear_model))

vif(linear_model)

####### Conclusion and Recommendation for further analysis 

'CPI Alone has no siginificant effect on market returns according to linear regression.' 

'Adding in The PPI data, caused the variance inflation factor to skyrocket. Suggesting multi-collinearity exist between CPI and PPI (we can expect that.)'

'It should be noted that the first assumption of linearity was violated as the plot of Residuals, vs. The market return shows a profound linear relationship. '

