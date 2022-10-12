library(dplyr)
library(scatterplot3d)
library(plotly)
library(broom)
library(tidyr)

QQQ = read.csv('data/QQQ.csv', header = TRUE)
SPY = read.csv('data/SPY.csv', header = TRUE)
date_columns = c("gatherDate", "expiry")

#df$gatherDate = as.POSIXct(df$gatherDate, format = '%Y-%m-%d')
#df$expiry = as.POSIXct(df$expiry, format = '%Y-%m-%d')
str(df)


df = SPY
df$cash = abs(df$stk_price - df$strike) * df$volume

# GROUP BY DATE, STRIKE, TO GET IMPLIED VOLATILITY 
iv_surface = df %>% group_by(expiry, strike, type) %>%
  summarise(Premium = sum(cash), OpenInterest = sum(openInterest))
head(iv_surface)

#PLOTLYSCATTER
plot_ly(x = iv_surface$expiry, 
        y = iv_surface$strike,
        z = iv_surface$Premium,  
        type = "scatter3d",
        color = ~iv_surface$type, 
        colors = c('darkgreen', 'red'), 
        mode = 'markers', 
        size = .1) %>% 
  layout(scene = list(xaxis = list(title = 'Expiration Date'),
                      yaxis = list(title = 'Strike'),                         
                      zaxis = list(title = 'Premium')))

#### Add a regression plane  ####
plane_plot0  = df %>% group_by(gatherDate, type) %>% 
  summarise(x = sum(openInterest), stockPrice = mean(stk_price)) %>% 
  pivot_wider(names_from = type, values_from = x, ) 
plane_plot = plane_plot0[1:4]
plane_plot$pcr = plane_plot$Put /plane_plot$Call
head(plane_plot)

# ADD IN A REGRESSION PLANE 
x_vals = log(plane_plot$Put)
y_vals = log(plane_plot$Call)
z_vals = plane_plot$

model = lm(z_vals ~ x_vals + y_vals)
summary(model)

# Define the regression plane 
grid_len = 50
x_grid = seq(from = min(x_vals), to= max(x_vals), length = grid_len )
y_grid = seq(from = min(y_vals), to= max(y_vals), length = grid_len )

# Compute the following 
# 1. Fitted beta coefficient 
# 2. fitted values of outer product of xgrid, and y grid 
# 3. extracting z_grid 

beta_hat = model %>% coef()
head(beta_hat)
fitted_vals = crossing(y_grid, x_grid) %>% 
  mutate( z_grid = beta_hat[1]+beta_hat[2]*x_grid+ beta_hat[3]*y_grid)
z_grid = fitted_vals %>% pull(z_grid) %>% matrix(nrow = length(x_grid)) %>% t()

# Define text element for each point in plane
text_grid <- fitted_vals %>% 
  pull(z_grid) %>%
  round(3) %>% 
  as.character() %>% 
  paste("Stock Price:  ", ., sep = "") %>% 
  matrix(nrow = length(x_grid)) %>%
  t()

plot_ly() %>% add_markers(
  x = x_vals, 
  y = y_vals, 
  z = z_vals, 
  marker = list(size = 2), 
  hoverinfo = 'text',
  colors = c('darkgreen', 'red'), 
  text = ~paste(
    "Average Price: ", z_vals, "<br>",
    "Strike: ", y_vals, "<br>",
    "TimeValue: ", x_vals, "<br>")
) %>% 
  # add in the plane
  add_surface(x = x_grid, 
              y = y_grid, 
              z = z_grid, 
              hoverinfo = 'text', 
              text = text_grid, 
              colorscale = list(c(0, 1), c("red", "green")), 
              opacity = .4) %>%
  layout(scene = list(xaxis = list(title = 'Put Volume(log scale)'),
                      yaxis = list(title = 'Call Volume (log scale)'),                         
                      zaxis = list(title = 'Stock Price')))

aov(model)


### Analyze the means 
data.frame(plane_plot0)
