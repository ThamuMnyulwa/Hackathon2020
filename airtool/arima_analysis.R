library(forecast)
library(xts)

data_Restaurant = data_Restaurant[order(as.Date(data_Restaurant$Tran_Date)),]
restuarant_data =data_Restaurant



restuarant.cum.spend = c()
for(i in 1:length(table(restuarant_data$date))){
  restuarant.cum.spend[i] = sum(restuarant_data[restuarant_data$date == names(table(restuarant_data$date))[i],]$Amount)
}
restuarants.ts = data.frame("Date" = as.Date(names(table(restuarant_data$date))), "daily_spend" = restuarant.cum.spend)


restuarants.ts = xts(x=restuarants.ts$daily_spend,order.by = restuarants.ts$Date)



train.restuarants = restuarants.ts[1:59,]
test.restuarants = restuarants.ts[60:90,]



# auto.arima
arima.restuarants = auto.arima(train.restuarants,seasonal = TRUE)



# forecast and plot
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



restuarant1.ts = xts(x=restuarant1.ts$daily_spend,order.by = restuarant1.ts$Date)



train.restuarant1 = restuarant1.ts[1:59,]
test.restuarant1 = restuarant1.ts[60:90,]



# auto.arima
arima.restuarant1 = auto.arima(train.restuarant1,seasonal = TRUE)



# forecast and plot
forecast(arima.restuarant1,h=31)
autoplot(forecast(arima.restuarant1,h=31))



png(filename="./www/first_arima_plot.png", width=1600, height=700)
autoplot(forecast(arima.restuarants,h=31))
dev.off()

png(filename="./www/second_arima_plot.png", width=1600, height=700)
autoplot(forecast(arima.restuarant1,h=31))
dev.off()



