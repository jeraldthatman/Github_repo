library(dplyr)
library(plotly)
library(broom)

#df = read.csv('data/QQQ.csv', header = TRUE)
df = read.csv('data/SPY.csv', header = TRUE)
df$cash = abs(df$stk_price - df$strike) * df$volume
str(df)


#OBSERVE THE GROUPBY SPEED COMPARED TO PANDAS GROUPBY 
tmp = df %>% group_by(contractSymbol)  
tmp %>% group_keys() #View the grouped keys 
tmp %>% group_indices() %>% head() # prints the indices of which group each row belongs too 
tmp %>% group_rows() %>% head() #which rows each group contains 
tmp %>% group_vars() # Grouping Variable. 

# Aggregation functions and methods using groupby 
tmp %>% summarise(iv= mean(impliedVolatility)) #Average Implied Volatility of each contract
tmp %>% summarise(n = n(), .groups = 'keep') # counts the occurance of each contract

#Multiple grouping, dropping and keeping keys. 
multi_temp = df %>% group_by(gatherDate, strike) %>% summarise(n = n(), .groups = 'drop')

# Selecting, sorting, renaming, and relocating groups 
tmp %>% arrange(desc(volume_pctChange)) %>% select(gatherDate, volume_pctChange)
tmp %>% mutate(rank = min_rank(volume)) %>% select(volume)
tmp %>% slice(1)

# Performing Statistical calculations for each group such as lm 
# The following fits a linear regression with Implied Volatility as Y and openInterest as X
# for all contracts that have more than one entry 
lm_model0 = tmp %>% filter(n() > 5) %>% 
  mutate(LM_model = list(lm(impliedVolatility ~ openInterest+ volume+ timeValue, data = cur_group()))) %>% 
  select(LM_model)
lm_model0
# Above the models are stored with each key as a list, then to accsess the coeff you would need to loop through all
# keys and do something like this 
lm_model0$LM_model[1]

# Similarly to fit a linear regression for each group, and get the regression statistics you can do this. 
lm_model1 = tmp %>% filter(n() > 5) %>% do(ModelDf = lm(lastPrice ~ impliedVolatility + volume, data = .)) %>% 
  summarize(tidy(ModelDf))
# Return a table with the coefficient, standard error, t-stat, and P.value
lm_model1

lm_model2 = df %>% nest_by(contractSymbol) %>% 
  mutate(lm_model = list(lm(lastPrice ~ volume+openInterest, data = data)))%>% 
  summarize(augment(lm_model))
# return a table the coefficient, fitted value, residual, estimated sigma, cooks distance, and standard residual 
lm_model2

