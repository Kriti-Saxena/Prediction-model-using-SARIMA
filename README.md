# Prediction model using Seasonal Autoregressive Moving Average model(SARIMA) 

The project aims at exploring the walmart sales data and identify trends. Then, develop a SARIMA model to predict future sales. The dataset was extracted from Kaggle and then cleaned before modeling.

# importing relevent libraries 
```
library(forecast)
library(tidyverse)
library(lubridate)
library(plyr)
library(scales)
library(dplyr)
library(here)
```


# change date to date format 
```
sales <- sales %>%
  mutate(Date = dmy(Date),
         Store = as.factor(Store))
```

# Exploring the data 
```
head(sales)
sales %>% summary()
str(sales)
head(sales)
dim(sales)
length(table(sales$Store))
colSums(is.na(sales))
all(duplicated(sales))
```

# looking at weekly sales for each store 
```
sales %>%
  distinct(Store, Weekly_Sales) %>%
  group_by(Store) %>%
  summarize(mean_weekly_sales = mean(Weekly_Sales))
```
  

# Which store has the highest weekly sales 
```
mean_sales <- aggregate(Weekly_Sales ~ Store, data = sales, mean)
mean_sales %>% 
  slice_max(order_by = Weekly_Sales, n=1)
```

Since store 20 has had most number of sales, we will predict if it will continue to keep up with the sales in upcoming year

# Keeping the relevent data only 
```
sales_1 <- sales %>% 
  filter(Store == 20)
```

# Graphing the data 
```
sales_1 %>%
  ggplot(aes(x=Date, y = Weekly_Sales)) + geom_line()
```

Since the graph is very cluttered, we will look at smaller time frames 

# graphing monthly for clear picture 
```
sales_1 %>%
  filter(Date <= '2011-02-05') %>%
  ggplot(aes(x=Date, y = Weekly_Sales)) + geom_line()

sales_1 %>%
  filter(Date > '2011-02-05' & Date < '2012-02-10') %>%
  ggplot(aes(x=Date, y = Weekly_Sales)) + geom_line()

sales_1 %>%
  filter(Date > '2012-02-10' & Date < '2012-10-26') %>%
  ggplot(aes(x=Date, y = Weekly_Sales)) + geom_line()
````
After looking at the data in shorter time period, a similar period can be noticed every year. 
- The sales tends to increase in the month of December and between the months of April and May.

# structuring the data
```
conv_month <- sales_1 %>%
  mutate(monthly = floor_date(Date,"month"))
```

# Aggregating the sales 
```
sum_monthly_sales <- aggregate(Weekly_Sales ~ monthly, data = conv_month, sum)
class(sum_monthly_sales)

sum_monthly_sales<- sum_monthly_sales %>%
  mutate(sales = Weekly_Sales/1000000)
```

# graphing the sales 
```
sum_monthly_sales %>%
  ggplot(aes(x=monthly, y = sales)) + geom_line()
```
# Graphing montly sales 
```
# sales for months of 2010
sum_monthly_sales %>%
  filter(monthly < "2011-01-01") %>%
  ggplot(aes(x=monthly, y=sales)) + geom_line() 
```
```
# sales for months of 2011
sum_monthly_sales %>%
  filter(monthly < "2012-01-01" & monthly > '2011-01-01') %>%
  ggplot(aes(x=monthly, y=sales)) + geom_line()
```

```
# sales for month of 2012
sum_monthly_sales %>%
  filter(monthly > '2012-01-01') %>%
  ggplot(aes(x=monthly, y=sales)) + geom_line()
```
```
sum_monthly_sales <- sum_monthly_sales %>%
  add_row(monthly = ymd(20100101), sales = 5.45) %>%
  arrange(monthly)
```
# Summinh montthly sales 
```
sum_monthly_sales <- sum_monthly_sales %>%
  mutate(month = month(monthly))
```

# structuring variables
```
sum_monthly_sales$sales <- ts(sum_monthly_sales$sales)
sum_monthly_sales$month <- as.factor(sum_monthly_sales$month)
str(sum_monthly_sales)
```

# Creating a linear model 
```
Model <- lm(sales ~ month, data = sum_monthly_sales)
summary(Model)
```

# Decomposing the data
```
ts_data <- ts(sum_monthly_sales$sales, frequency = 12)
data <- decompose(ts_data, "multiplicative")
plot(data)
```
# Building the SARIMA model 
```
for_sarima <- ts(sum_monthly_sales$sales, start = 2010, frequency = 12)

# SARIMA
model <- auto.arima(for_sarima, D = 1)
summary(model)
```
# Predictions 
```
sales_p <- forecast(model, h = 14)
plot(sales_p)
p <- data.frame(summary(sales_p)$mean)
p
```

# Takeaways 

1. Through visual representation, it was seen that a increase in sales was seen in the month of december and then between April and May. The reason behind it can be multiple
but this project and model will only look at predicting sales. 

2. The predicted sales have high accuracy and a similar seasonl pattern can be seen. 

