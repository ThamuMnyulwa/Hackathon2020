
install.packages('viridis')
library(ggridges)
library(viridis)

# These are the images that must be placed in the APP

# Plot 2 ----------------------------------------- Put in App
ggplot(processed_data) +
  aes(x = time_of_day, fill = industry, colour = industry) +
  geom_bar() +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_color_brewer(palette = "RdYlBu") +
  labs(x = "Time of day (Hour)", y = "Transactions per hour", title = "Spending habits", subtitle = "Plot counting transactions in data set aganist the time of day") +
  theme_minimal()

############################################3\

# Plot 3 in app

ggplot(data_Restaurant) +
  aes(x = Tran_Date) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Time", y = "Transactions", title = "Time plot of daily transactions frequency", subtitle = "Frequency of transactions") +
  theme_minimal()

# Plot 4 in app
ggplot(data_Restaurant) +
  aes(x = Tran_Date, y = Amount) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  labs(x = "Time (Daily)", y = "Amount (ZAR)", title = "Time plot of Daily ammount", subtitle = "Daily profit made over time") +
  theme_minimal()

# Avg_Income

ggplot(data_Restaurant) +
  aes(x = Avg_Income_3M, group = month) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Time (Daily)", y = "Amount (ZAR)", title = "Time plot of Daily ammount", subtitle = "Daily profit made over time") +
  theme_minimal()



# Gender 
ggplot(data_CoffeeShop) +
  aes(x = Gender_code, weight = Amount) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Gender", y = "Ammount (ZAR)", title = "Client Profit vs Gender", subtitle = "Aggregate Profit per gender over time period") +
  theme_minimal()
##################################################################################################33
#############3
# Finished put in app

### Put in Presentation
ggplot(processed_data) +
  aes(x = time_of_day, fill = industry, colour = industry) +
  geom_bar() +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_color_brewer(palette = "RdYlBu") +
  labs(x = "Time of day (Hour)", y = "Number of Transactions", title = "Industry Spending ", subtitle = "Plot counting transactions in data set aganist the time of day") +
  theme_minimal()


  
# Plot 2 -----------------------------------------

esquisser()

ggplot(processed_data) +
  aes(x = ordered(processed_data$weekday,
                  levels=c("Sun","Mon", "Tue", "Wed", "Thu","Fri", "Sat"))
                  , fill = industry, colour = industry, weight = Amount) +
  geom_bar() +
  scale_fill_hue() +
  scale_color_hue() +
  labs(x = "Day", y = "Number of Transactions", title = "Industry Spending ", subtitle = "Plot counting transactions in data set aganist the time of day") +
  theme_minimal() +
  facet_wrap(vars(industry))

# Plot 3 -----------------------------------
library('ggpubr')

ggplot(data2) +
  aes(x = Avg_Income_3M, fill = industry, colour = industry, group = industry) +
  geom_bar() +
  scale_fill_viridis_d(option = "inferno") +
  scale_color_viridis_d(option = "inferno") +
  labs(x = "Average Income in 3 month period", y = "Frequency", title = "Frequency of transactions within each industry ", subtitle = "Complete data over the 3 month") +
  theme_minimal() +
  facet_wrap(vars(industry)) + ggpubr::rotate_x_text()

#  Plot 4 ----------------------------------


colnames(processed_data)


datatemp = processed_data
datatemp = datatemp %>% filter(Amount <= 1000)

density.chicks <- ggplot(datatemp) +
  geom_density(aes(x=Amount, color = industry,fill=industry), alpha=0.8, show.legend = TRUE)+
  labs(x="Amount (ZAR)", y = "Density" ,subtitle = "Plot shows that each industrys density has different distribution ") +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_color_brewer(palette = "RdYlBu") +
  ggtitle("Density of amount between industry") +
  theme_minimal()
density.chicks 

#  Plot 5

ggplot(data2) +
  aes(x = capitec_client, fill = industry, colour = industry, group = industry) +
  geom_bar() +
  scale_fill_brewer(palette = "RdBu") +
  scale_color_brewer(palette = "RdBu") +
  labs(x = "Capitec client transactions", y = "Number of transactions", title = "Capitec clients at Capitec clients", subtitle = "By location how many capitec clients go to capitec merchants over 3 month period") +
  theme_minimal() +
  facet_wrap(vars(industry))


  
  
