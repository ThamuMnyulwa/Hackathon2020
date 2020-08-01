# Final Script to create the project

# Improve handling NA's
#install.packages('forecast')

library(readr)
library(curl)
library(tidyr)
library(dplyr)
library(stringr)
library(naniar)
library(UpSetR)
library(forecast)
library(lubridate)
library(tseries)
library(ggplot2)
library(rio)

raw_data <- read_tsv("https://raw.githubusercontent.com/SUHackathon/data-science-challenge/master/data/business_data.txt", na=c("","NA","NULL",NULL))

# Count Na's column wise --------------------------------------------------------
na_count <-sapply(raw_data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count


# https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html

# Visualize columns with missing values -----------------------------------------

gg_miss_which(raw_data)+   
  labs(title = "Raw data from Capitec",
       subtitle = "All features have missing values. We had 16 features and 80983 observations.",
       y = "",
       x = "Features"
       )

# Visualize overall missing -----------------------------------------------------

# Visualize missing data
vis_miss(raw_data,warn_large_data=FALSE)+   
       labs(title = "Relationship of missingness (Raw data)",
       subtitle = "16 features and 80983 observations.",
       y = "Observation",
       x = ""
  )



# Exploring patterns with UpSetR -----------------------------------------------------

gg_miss_upset(data1) 


n_var_miss(raw_data)

################################################################################

# Drop the single missing value in Capitec clients data

data1 = raw_data %>% drop_na("Town")

# Visualize columns with missing values -----------------------------------------
gg_miss_which(data1)+
  labs(title = "data1",
       subtitle = "7 features have missing values. We had 16 features and 80983 observations.",
       y = "",
       x = "Features")

  

# Count Na's column wise --------------------------------------------------------
na_count <-sapply(data1, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

# Explote pattern of missingness again ---------------------------------------
gg_miss_upset(data1)

n_var_miss(data1) # Count columns with no missing values

# Exploring Missingness Mechanisms --------------------


# General plot 2 ---------------------------------------
ggplot(data1,
       aes(x = factor(data1$Town),
           y = data1$Card_Amount_Paid)) +
  geom_point()


# Facets!
ggplot(data1,
       aes(x = factor(data1$Merchant_Id),
           y = data1$Card_Amount_Paid)) +
  geom_miss_point() + 
  facet_wrap(~factor(Town))

# Guys
ggplot(data1,
       aes(x = factor(data1$Merchant_Id),
           y = data1$Avg_Income_3M)) +
  geom_miss_point() + 
  facet_wrap(~factor(Town))

##################################################################

# Remove certain features
data1 = data1 %>% select(-c(Military_Time, Province))

# Explote pattern of missingness again --------------------------------
vis_miss(data1,warn_large_data=FALSE)+   
  labs(title = "data1",
       subtitle = "All features have missing values. We had 16 features and 80983 observations.",
       y = "",
       x = "Features"
  )

# Count Na's column wise ----------------------------------------------
na_count <-sapply(data1, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

# Create a feature Capitec Clients
data1 <- data1 %>% mutate(capitec_client = if_else(is.na(Client_ID),"No", "Yes"))

# Create feature showing month of transaction in order to use in visualization
# https://statistics.berkeley.edu/computing/r-dates-times

#dtparts = data1$Tran_Date
#dtparts = format(dtparts,'%A, %B %d, %Y %H:%M:%S')
#data1$Month = format(dtparts,'%B')

# Create a feature for Transaction Industry
data1 <- data1 %>% mutate(date = lubridate::date(Tran_Date),
                          day_of_month = lubridate::day(Tran_Date),
                          time_of_day = lubridate::hour(Tran_Date),
                          weekday = wday(Tran_Date, label = TRUE),
                          month = lubridate::month(Tran_Date),
                          capitec_client = if_else(is.na(Client_ID),"No", "Yes"),
                          industry = case_when(str_detect(Merchant_Name, "Liquor") ~ "Liquor",
                                               str_detect(Merchant_Name, "Market") ~ "MiniMarket",
                                               str_detect(Merchant_Name, "Rest") ~ "Restaurant",
                                               str_detect(Merchant_Name, "Coffee") ~ "CoffeeShop",
                                               str_detect(Merchant_Name, "Fast") ~ "FastFood",
                                               str_detect(Merchant_Name, "Beauty") ~ "Beauty",
                                               TRUE ~ "Unknown"))

# Create Industry Specific datasets --------------------

data_MiniMarket <- data1[data1$industry == "MiniMarket",]
data_Liquor <- data1[data1$industry == "Liquor",]
data_Restaurant <- data1[data1$industry == "Restaurant",]
data_CoffeeShop <- data1[data1$industry == "CoffeeShop",]
data_FastFood <- data1[data1$industry == "FastFood",]
data_Beauty <- data1[data1$industry == "Beauty",]  


# Card_Value_Spending and Card_Number_Spending are not the same
all.equal(data1$Card_Value_Spending, data1$Card_Number_Spending)

# Amount and Card_Amount_Paid are not the same
all.equal(data1$Amount,data1$Card_Amount_Paid)
identical(data1$Amount,data1$Card_Amount_Paid)


# Subscript liquor daily sales
data_Liquor = data_Liquor[order(as.Date(data_Liquor$Tran_Date)) ,]
liquor_data = data_Liquor

#All merchants for liquor
liquor.cum.spend = c()
for(i in 1:length(table(liquor_data$date))){
  liquor.cum.spend[i] = sum(liquor_data[liquor_data$date == names(table(liquor_data$date))[i],]$Amount)
}
liquor.ts = data.frame("Date" = as.Date(names(table(liquor_data$date))), "daily_spend" = liquor.cum.spend)
p =  ggplot(liquor.ts, aes(x = Date , y = daily_spend)) +
  geom_line() + scale_x_date(date_breaks = "1 week", date_labels = "%a" , date_minor_breaks = "1 day") + ggtitle("All liquor store sales") + xlab("Date") + ylab("Total expenditure")
p = ggplotly(p)
p

liquor.ts = xts(x=liquor.ts$daily_spend,order.by = liquor.ts$Date)

train.liquor = liquor.ts[1:50,]
test.liquor = liquor.ts[51:76,]

# Dickey Fuller test
adf.test(train.liquor)

# ACF and PACF
acf(train.liquor)
pacf(train.liquor)

# auto.arima
arima.liq = auto.arima(train.liquor,seasonal = TRUE)

# Ljung-Box test
Box.test(residuals(arima.liq))

# ggplot
forecast(arima.liq,h=26)
autoplot(forecast(arima.liq,h=26))


#Merchant 1
liquor1_data = liquor_data[liquor_data$Merchant_Name == "Liquor Store 1" ,]
liquor1_data = liquor1_data[order(as.Date(liquor1_data$Tran_Date)), ]
liquor1.cum.spend = c()
for(i in 1:length(table(liquor1_data$date))){
  liquor1.cum.spend[i] = sum(liquor1_data[liquor1_data$date == names(table(liquor1_data$date))[i],]$Amount)
}
liquor1.ts = data.frame("Date" = as.Date(names(table(liquor1_data$date))), "daily_spend" = liquor1.cum.spend)
p =  ggplot(liquor1.ts, aes(x = Date , y = daily_spend)) +
  geom_line() + scale_x_date(date_breaks = "1 week", date_labels = "%a" , date_minor_breaks = "1 day") + ggtitle("Liquor store 1 sales") + xlab("Date") + ylab("Total expenditure")
p = ggplotly(p)
p


liquor1.ts = xts(x=liquor1.ts$daily_spend,order.by = liquor1.ts$Date)

train.liquor1 = liquor1.ts[1:50,]
test.liquor1 = liquor1.ts[51:76,]

# Dickey Fuller test
adf.test(train.liquor1)

# ACF and PACF
acf(train.liquor1)
pacf(train.liquor1)

# auto.arima
arima.liq1 = auto.arima(train.liquor1,seasonal = TRUE)

# Ljung-Box test
Box.test(residuals(arima.liq1))

# ggplot
forecast(arima.liq1,h=26)
autoplot(forecast(arima.liq1,h=26))



#Restuarants
data_Restaurant = data_Restaurant[order(as.Date(data_Restaurant$Tran_Date)),]
restuarant_data =data_Restaurant

restuarant.cum.spend = c()
for(i in 1:length(table(restuarant_data$date))){
  restuarant.cum.spend[i] = sum(restuarant_data[restuarant_data$date == names(table(restuarant_data$date))[i],]$Amount)
}
restuarants.ts = data.frame("Date" = as.Date(names(table(restuarant_data$date))), "daily_spend" = restuarant.cum.spend)
p =  ggplot(restuarants.ts, aes(x = Date , y = daily_spend)) +
  geom_line() + scale_x_date(date_breaks = "1 week", date_labels = "%a" , date_minor_breaks = "1 day") + ggtitle("Restuarant sales") + xlab("Date") + ylab("Total expenditure")
p = ggplotly(p)
p

restuarants.ts = xts(x=restuarants.ts$daily_spend,order.by = restuarants.ts$Date)

train.restuarants = restuarants.ts[1:59,]
test.restuarants = restuarants.ts[60:90,]

# Dickey Fuller test
adf.test(train.restuarants)

# ACF and PACF
acf(train.restuarants)
pacf(train.restuarants)

# auto.arima
arima.restuarants = auto.arima(train.restuarants,seasonal = TRUE)

# Ljung-Box test
Box.test(residuals(arima.restuarants))

# ggplot
forecast(arima.restuarants,h=31)
autoplot(forecast(arima.restuarants,h=31))


# Merchant 1
restuarant1_data = restuarant_data[restuarant_data$Merchant_Name == "Restuarant 1" ,]
restuarant1_data = restuarant1_data[order(as.Date(restuarant1_data$Tran_Date)), ]
restuarant1.cum.spend = c()
for(i in 1:length(table(restuarant1_data$date))){
  restuarant1.cum.spend[i] = sum(restuarant1_data[restuarant1_data$date == names(table(restuarant1_data$date))[i],]$Amount)
}
restuarant1.ts = data.frame("Date" = as.Date(names(table(restuarant1_data$date))), "daily_spend" = restuarant1.cum.spend)
p =  ggplot(restuarant1.ts, aes(x = Date , y = daily_spend)) +
  geom_line() + scale_x_date(date_breaks = "1 week", date_labels = "%a" , date_minor_breaks = "1 day") + ggtitle("Restuarant 1 sales") + xlab("Date") + ylab("Total expenditure")
p = ggplotly(p)
p

restuarant1.ts = xts(x=restuarant1.ts$daily_spend,order.by = restuarant1.ts$Date)

train.restuarant1 = restuarant1.ts[1:59,]
test.restuarant1 = restuarant1.ts[60:90,]

# Dickey Fuller test
adf.test(train.restuarant1)

# ACF and PACF
acf(train.restuarant1)
pacf(train.restuarant1)

# auto.arima
arima.restuarant1 = auto.arima(train.restuarant1,seasonal = TRUE)

# Ljung-Box test
Box.test(residuals(arima.restuarant1))

# ggplot
forecast(arima.restuarant1,h=31)
autoplot(forecast(arima.restuarant1,h=31))



#Beauty
data_Beauty = data_Beauty[order(as.Date(data_Beauty$Tran_Date)),]
beauty_data = data_Beauty 

beauty.cum.spend = c()
for(i in 1:length(table(beauty_data$date))){
  beauty.cum.spend[i] = sum(beauty_data[beauty_data$date == names(table(beauty_data$date))[i],]$Amount)
}
beauty.ts = data.frame("Date" = as.Date(names(table(beauty_data$date))), "daily_spend" = beauty.cum.spend)
p =  ggplot(beauty.ts, aes(x = Date , y = daily_spend)) +
  geom_line() + scale_x_date(date_breaks = "1 week", date_labels = "%a" , date_minor_breaks = "1 day") + ggtitle("All beauty sales") + xlab("Date") + ylab("Total expenditure")
p = ggplotly(p)
p


beauty.ts = xts(x=beauty.ts$daily_spend,order.by = beauty.ts$Date)

train.beauty = beauty.ts[1:58,]
test.beauty = beauty.ts[59:89,]

# Dickey Fuller test
adf.test(train.beauty)

# ACF and PACF
acf(train.beauty)
pacf(train.beauty)

# auto.arima
arima.beauty = auto.arima(train.beauty,seasonal = TRUE)

# Ljung-Box test
Box.test(residuals(arima.beauty))

# ggplot
forecast(arima.beauty,h=31)
autoplot(forecast(arima.beauty,h=31))




#Merchant 1
beauty1_data = beauty_data[beauty_data$Merchant_Name == "Beauty 1" ,]
beauty1_data = beauty1_data[order(as.Date(beauty1_data$Tran_Date)), ]
beauty1.cum.spend = c()
for(i in 1:length(table(beauty1_data$date))){
  beauty1.cum.spend[i] = sum(beauty1_data[beauty1_data$date == names(table(beauty1_data$date))[i],]$Amount)
}
beauty1.ts = data.frame("Date" = as.Date(names(table(beauty1_data$date))), "daily_spend" = beauty1.cum.spend)
p =  ggplot(beauty1.ts, aes(x = Date , y = daily_spend)) +
  geom_line() + scale_x_date(date_breaks = "1 week", date_labels = "%a" , date_minor_breaks = "1 day") + ggtitle("Beauty store 1 sales") + xlab("Date") + ylab("Total expenditure")
p = ggplotly(p)
p

beauty1.ts = xts(x=beauty1.ts$daily_spend,order.by = beauty1.ts$Date)

train.beauty1 = beauty1.ts[1:58,]
test.beauty1 = beauty1.ts[59:89,]

# Dickey Fuller test
adf.test(train.beauty1)

# ACF and PACF
acf(train.beauty1)
pacf(train.beauty1)

# auto.arima
arima.beauty1 = auto.arima(train.beauty1,seasonal = TRUE)

# Ljung-Box test
Box.test(residuals(arima.beauty1))

# ggplot
forecast(arima.beauty1,h=31)
autoplot(forecast(arima.beauty1,h=31))



#Fast food
data_FastFood = data_FastFood[order(as.Date(data_FastFood$Tran_Date)),]
fast.food_data = data_FastFood

fast.food.cum.spend = c()
for(i in 1:length(table(fast.food_data$date))){
  fast.food.cum.spend[i] = sum(fast.food_data[fast.food_data$date == names(table(fast.food_data$date))[i],]$Amount)
}
fast.food.ts = data.frame("Date" = as.Date(names(table(fast.food_data$date))), "daily_spend" = fast.food.cum.spend)
p =  ggplot(fast.food.ts, aes(x = Date , y = daily_spend)) +
  geom_line() + scale_x_date(date_breaks = "1 week", date_labels = "%a" , date_minor_breaks = "1 day") + ggtitle("fast.food sales") + xlab("Date") + ylab("Total expenditure")
p = ggplotly(p)
p


fast.food.ts = xts(x=fast.food.ts$daily_spend,order.by = fast.food.ts$Date)

train.food = fast.food.ts[1:59,]
test.food = fast.food.ts[60:90,]

# Dickey Fuller test
adf.test(train.food)

# ACF and PACF
acf(train.food)
pacf(train.food)

# auto.arima
arima.food = auto.arima(train.food,seasonal = TRUE)

# Ljung-Box test
Box.test(residuals(arima.food))

# ggplot
forecast(arima.food,h=31)
autoplot(forecast(arima.food,h=31))



#Merchant 1
fast.food1_data = fast.food_data[fast.food_data$Merchant_Name == "Fast Food 1" ,]
fast.food1_data = fast.food1_data[order(as.Date(fast.food1_data$Tran_Date)), ]
fast.food1.cum.spend = c()
for(i in 1:length(table(fast.food1_data$date))){
  fast.food1.cum.spend[i] = sum(fast.food1_data[fast.food1_data$date == names(table(fast.food1_data$date))[i],]$Amount)
}
fast.food1.ts = data.frame("Date" = as.Date(names(table(fast.food1_data$date))), "daily_spend" = fast.food1.cum.spend)
p =  ggplot(fast.food1.ts, aes(x = Date , y = daily_spend)) +
  geom_line() + scale_x_date(date_breaks = "1 week", date_labels = "%a" , date_minor_breaks = "1 day") + ggtitle("Fast Food store 1 sales") + xlab("Date") + ylab("Total expenditure")
p = ggplotly(p)
p


fast.food1.ts = xts(x=fast.food1.ts$daily_spend,order.by = fast.food1.ts$Date)

train.food1 = fast.food1.ts[1:59,]
test.food1 = fast.food1.ts[60:90,]

# Dickey Fuller test
adf.test(train.food1)

# ACF and PACF
acf(train.food1)
pacf(train.food1)

# auto.arima
arima.food1 = auto.arima(train.food1,seasonal = TRUE)

# Ljung-Box test
Box.test(residuals(arima.food1))

# ggplot
forecast(arima.food1,h=31)
autoplot(forecast(arima.food1,h=31))




#Mini market
data_MiniMarket = data_MiniMarket[order(as.Date(data_MiniMarket$Tran_Date)),]
mini.market_data = data_MiniMarket
mini.market.cum.spend = c()
for(i in 1:length(table(mini.market_data$date))){
  mini.market.cum.spend[i] = sum(mini.market_data[mini.market_data$date == names(table(mini.market_data$date))[i],]$Amount)
}
mini.market.ts = data.frame("Date" = as.Date(names(table(mini.market_data$date))), "daily_spend" = mini.market.cum.spend)
p =  ggplot(mini.market.ts, aes(x = Date , y = daily_spend)) +
  geom_line() + scale_x_date(date_breaks = "1 week", date_labels = "%a" , date_minor_breaks = "1 day") + ggtitle("mini.market sales") + xlab("Date") + ylab("Total expenditure")
p = ggplotly(p)
p


mini.market.ts = xts(x=mini.market.ts$daily_spend,order.by = mini.market.ts$Date)

train.market = mini.market.ts[1:59,]
test.market = mini.market.ts[60:90,]

# Dickey Fuller test
adf.test(train.market)

# ACF and PACF
acf(train.market)
pacf(train.market)

# auto.arima
arima.market = auto.arima(train.market,seasonal = TRUE)

# Ljung-Box test
Box.test(residuals(arima.market))

# ggplot
forecast(arima.market,h=31)
autoplot(forecast(arima.market,h=31))



#Merchant 1
mini.market1_data = mini.market_data[mini.market_data$Merchant_Name == "Mini Market 1" ,]
mini.market1_data = mini.market1_data[order(as.Date(mini.market1_data$Tran_Date)), ]
mini.market1.cum.spend = c()
for(i in 1:length(table(mini.market1_data$date))){
  mini.market1.cum.spend[i] = sum(mini.market1_data[mini.market1_data$date == names(table(mini.market1_data$date))[i],]$Amount)
}
mini.market1.ts = data.frame("Date" = as.Date(names(table(mini.market1_data$date))), "daily_spend" = mini.market1.cum.spend)
p =  ggplot(mini.market1.ts, aes(x = Date , y = daily_spend)) +
  geom_line() + scale_x_date(date_breaks = "1 week", date_labels = "%a" , date_minor_breaks = "1 day") + ggtitle("Mini Market store 1 sales") + xlab("Date") + ylab("Total expenditure")
p = ggplotly(p)
p

mini.market1.ts = xts(x=mini.market1.ts$daily_spend,order.by = mini.market1.ts$Date)

train.market1 = mini.market1.ts[1:59,]
test.market1 = mini.market1.ts[60:90,]

# Dickey Fuller test
adf.test(train.market1)

# ACF and PACF
acf(train.market1)
pacf(train.market1)

# auto.arima
arima.market1 = auto.arima(train.market1,seasonal = TRUE)

# Ljung-Box test
Box.test(residuals(arima.market1))

# ggplot
forecast(arima.market1,h=31)
autoplot(forecast(arima.market1,h=31))


