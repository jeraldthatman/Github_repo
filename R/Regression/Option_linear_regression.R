library(dplyr)
library(car)
library(broom)
library(tidyr)
library(GGally)
library(corrplot)

##### Finding the most important variables fromt the option chain---
df = read.csv('data/SPY.csv', header = TRUE)
df = df %>% mutate_all(~replace(., is.na(.), 0))
colnames(df)

### Look at some correlations 
call_put_vol  = df %>% group_by(gatherDate, type) %>% 
  summarise(lastprice = mean(lastPrice), stockPrice = mean(stk_price), vol = mean(volume), oi = mean(openInterest))

head(call_put_vol)

correlation_matrix = cor(call_put_vol[,-1])
correlation_matrix
ggpairs(call_put_vol[,-1])


#### Call and Put linear regression ----
model1 = lm('stockPrice ~.', data = call_put_vol[,-1])
summary(model1)
bc = boxCox(model1)
lambda = bc$x[which.max(bc$y)]
round(lambda,1)

# lambda of .5 recommend sqrt(y) transformation 
model1.1 = lm(sqrt(stockPrice) ~ Call + Put, data = call_put_vol)
summary(model1.1)

# GROUPBY contractSymbol, filter contracts with 20 days of data. 
tmp= df %>% mutate_all(~replace(., is.na(.), 0)) %>%
  group_by(contractSymbol) %>% filter(n() > 20)

lm_model0 = tmp %>% 
  mutate(LM_model = 
    list(lm(impliedVolatility ~ openInterest+ volume+timeValue+ lastPrice, 
         data = cur_group()))) 

summary(lm_model0$LM_model[1][[1]])
c("gatherDate", "index", "stock", "contractSymbol", "strike", 
  "lastPrice", "bid", "ask", "percentChange", "volume", "openInterest", 
  "impliedVolatility", "type", "expiry", "stk_price", "timeValue", 
  "volume_pctChange",  "oi_pctChange", 
  "historicalVolatility", "priceVolatility", "theta", "delta", 
  "gamma", "vega", "theoPrice", "strike_stock_dif", "break_even", 
  "stk_price_breakeven_diff")

# First do the model
models = tmp %>% ungroup() %>% nest_by(contractSymbol) %>% 
  mutate(mod = list(lm(
  lastPrice ~ volume+ openInterest+impliedVolatility + stk_price, 
                data = data)))

# Then grab metrics like r^2 
r2 = models %>% summarise(rsq = summary(mod)$r.squared) %>% 
  filter(rsq >.80) %>%
  filter(rsq < .989)


head(data.frame(r2), 20)
# Look at the stats
reg_stats = models %>% summarise(broom::tidy(mod)) %>% na.omit()
#View(reg_stats %>% arrange(p.value))


best_vars  = reg_stats %>% group_by(contractSymbol) %>% filter(p.value < .01)
df_counts= as.data.frame(table(best_vars$term)[1:5]/ 
                           sum(table(best_vars$term)[1:5]))
df_counts %>% arrange(desc(Freq))
# Seems like open Interest was found as the most important predictor in price. 


